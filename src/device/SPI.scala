package ysyx

import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.apb._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class SPIIO(val ssWidth: Int = 8) extends Bundle {
  val sck = Output(Bool())
  val ss = Output(UInt(ssWidth.W))
  val mosi = Output(Bool())
  val miso = Input(Bool())
}

class spi_top_apb extends BlackBox {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Reset())
    val in = Flipped(new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32)))
    val spi = new SPIIO
    val spi_irq_out = Output(Bool())
  })
}

class flash extends BlackBox {
  val io = IO(Flipped(new SPIIO(1)))
}

class APBSPI(address: Seq[AddressSet])(implicit p: Parameters) extends LazyModule {
  val node = APBSlaveNode(Seq(APBSlavePortParameters(
    Seq(APBSlaveParameters(
      address       = address,
      executable    = true,
      supportsRead  = true,
      supportsWrite = true)),
    beatBytes  = 4)))

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val (in, _) = node.in(0)
    val spi_bundle = IO(new SPIIO)

    val mspi = Module(new spi_top_apb)
    mspi.io.clock := clock
    mspi.io.reset := reset
    mspi.io.in <> in
    spi_bundle <> mspi.io.spi

    val normal :: xip_tx :: xip_div :: xip_ss :: xip_ctrl :: xip_wait :: xip_read :: xip_hold :: Nil = Enum(8)
    val is_xip = Wire(Bool())

    val idle :: setup :: enable :: get_data :: Nil = Enum(4)
    val state = RegInit(normal)
    val apb_state = RegInit(idle)
    val apb_addr = RegInit(0.U(32.W))
    val apb_rdata = RegInit(0.U(32.W))
    val apb_wdata = RegInit(0.U(32.W))
    val apb_write = RegInit(false.B)
    val apb_run   = Wire(Bool())
    val apb_bus   = Wire(new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32)))
    

    is_xip := (in.paddr(29, 28) === "h3".U) && in.psel
    assert(!(is_xip && in.pwrite && in.penable), "Can't write to flash")

    apb_run := false.B
    //input
    mspi.io.in.paddr    := apb_bus.paddr
    mspi.io.in.psel     := apb_bus.psel
    mspi.io.in.penable  := apb_bus.penable
    mspi.io.in.pprot    := apb_bus.pprot
    mspi.io.in.pwrite   := apb_bus.pwrite
    mspi.io.in.pwdata   := apb_bus.pwdata
    mspi.io.in.pstrb    := apb_bus.pstrb
    apb_bus.pready      := mspi.io.in.pready
    apb_bus.prdata      := mspi.io.in.prdata
    apb_bus.pslverr     := mspi.io.in.pslverr
    //output
    in.pready   := false.B
    in.prdata   := 0.U(32.W)
    in.pslverr  := false.B
    switch(state) {
      is(normal) {
        when(is_xip) {
          state := xip_tx
          apb_run := true.B
          apb_addr := 0x10001004.U(32.W)// tx1
          apb_wdata := Cat(0x3.U(4.W),in.paddr(23, 0))
          apb_write := true.B
        }.otherwise {
          mspi.io.in.paddr := in.paddr
          mspi.io.in.psel := in.psel
          mspi.io.in.penable := in.penable
          mspi.io.in.pprot := in.pprot
          mspi.io.in.pwrite := in.pwrite
          mspi.io.in.pwdata := in.pwdata
          mspi.io.in.pstrb := in.pstrb
          in.pready := mspi.io.in.pready
          in.prdata := mspi.io.in.prdata
          in.pslverr := mspi.io.in.pslverr
        }
      }
      is(xip_tx) {
        when(apb_state === idle){
          state := xip_div
          apb_run := true.B
          apb_addr := 0x10001014.U(32.W)// div
          apb_wdata := 2.U(32.W)
          apb_write := true.B
        }
      }
      is(xip_div) {
        when(apb_state === idle){
          state := xip_ss
          apb_run := true.B
          apb_addr := 0x10001018.U(32.W)// ss
          apb_wdata := 1.U(32.W)
          apb_write := true.B
        }
      }
      is(xip_ss) {
        when(apb_state === idle){
          state := xip_ctrl
          apb_run := true.B
          apb_addr := 0x10001010.U(32.W)// ctrl
          apb_wdata := 0x2140.U(32.W)
          apb_write := true.B
        }
      }
      is(xip_ctrl) {
        when(apb_state === idle){
          state := xip_wait
          apb_run := true.B
          apb_addr := 0x10001010.U(32.W)// ctrl
          apb_wdata := 0.U(32.W)
          apb_write := false.B
        }
      }
      is(xip_wait) {
        when((apb_state === idle) && (apb_rdata(8) === 1.U)){
          state := xip_wait
          apb_run := true.B
        }.elsewhen((apb_state === idle) && (apb_rdata(8) === 0.U)){
          state := xip_read
          apb_run := true.B
          apb_addr := 0x10001000.U(32.W)// rx0
          apb_wdata := 0.U(32.W)
          apb_write := false.B
        }
      }
      is(xip_read) {
        when(apb_state === idle){
          state := normal
          in.pready := true.B
          in.prdata := Cat(apb_bus.prdata(7,0),apb_bus.prdata(15,8),apb_bus.prdata(23,16),apb_bus.prdata(31,24))
        }
      }
      is(xip_hold) {
        state := normal
        in.prdata := apb_rdata
      }
    }

    apb_bus.paddr   := apb_addr
    apb_bus.psel    := false.B
    apb_bus.penable := false.B
    apb_bus.pprot   := 1.U(3.W)
    apb_bus.pwrite  := apb_write
    apb_bus.pwdata  := apb_wdata
    apb_bus.pstrb   := 0xf.U(4.W)
    switch(apb_state) {
      is(idle) {
        when(apb_run) {
          apb_state := setup
        }
      }
      is(setup) {
        apb_state := enable
        apb_bus.psel := true.B
      }
      is(enable) {
        when(apb_bus.pready) {
          apb_state := get_data
        }
        apb_bus.psel := true.B
        apb_bus.penable := true.B
      }
      is(get_data) {
        apb_rdata := apb_bus.prdata
        apb_state := idle
      }
    }

    
  }
}
