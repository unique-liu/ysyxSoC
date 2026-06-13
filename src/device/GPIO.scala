package ysyx

import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.apb._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class GPIOIO extends Bundle {
  val out = Output(UInt(16.W))
  val in = Input(UInt(16.W))
  val seg = Output(Vec(8, UInt(8.W)))
}

class GPIOCtrlIO extends Bundle {
  val clock = Input(Clock())
  val reset = Input(Reset())
  val in = Flipped(new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32)))
  val gpio = new GPIOIO
}

class gpio_top_apb extends BlackBox {
  val io = IO(new GPIOCtrlIO)
}

class gpioChisel extends Module {
  def hex2seg(hex: UInt): UInt = {
    // 0-9, A-F
    //assume from high to low: A, B, C, D, E, F, G, DP,  low active
    val segs = VecInit(
      "b00000011".U, // 0
      "b10011111".U, // 1
      "b00100101".U, // 2
      "b00001101".U, // 3
      "b10011001".U, // 4
      "b01001001".U, // 5
      "b01000001".U, // 6
      "b00011111".U, // 7
      "b00000001".U, // 8
      "b00001001".U, // 9
      "b00010001".U, // A
      "b11000001".U, // B
      "b01100011".U, // C
      "b10000101".U, // D
      "b01100001".U, // E
      "b01110001".U  // F

    )
    segs(hex)
  }
  val io = IO(new GPIOCtrlIO)
  val led_out = RegInit(0.U(16.W))
  val seg_hex = RegInit("h01234567".U(32.W))
  val seg_out = Wire(Vec(8, UInt(8.W)))
  val rdata   = Wire(UInt(32.W))

  for (i <- 0 until 8) {
    seg_out(i) := hex2seg(seg_hex(4*i+3, 4*i))
  }

  io.gpio.out := led_out
  io.gpio.seg := seg_out
  

  val idle :: read :: write :: Nil = Enum(3)
  val state = RegInit(idle)
  io.in.pready := false.B
  io.in.prdata := 0.U(32.W)
  io.in.pslverr := false.B
  switch(state) {
    is(idle) {
      when(io.in.psel && io.in.penable) {
        state := Mux(io.in.pwrite, write, read)
      }
    }
    is(read) {
      state := idle
      io.in.pready := true.B
      io.in.prdata := rdata
    }
    is(write) {
      state := idle
      io.in.pready := true.B
    }
  }

  rdata := 0.U(32.W)
  switch(io.in.paddr(3,0)){
    is(0.U) { // LED
      when(state === write) {
        led_out := io.in.pwdata(15, 0)
      }
      rdata := Cat(0.U(16.W), led_out)
    }
    is(4.U) { // SEG
      when(state === write) {
        seg_hex := io.in.pwdata
      }
      rdata := seg_hex
    }
    is(8.U) { // IN
      rdata := Cat(0.U(16.W), io.gpio.in)
    }
  }

}

class APBGPIO(address: Seq[AddressSet])(implicit p: Parameters) extends LazyModule {
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
    val gpio_bundle = IO(new GPIOIO)

    val mgpio = Module(new gpioChisel)
    mgpio.io.clock := clock
    mgpio.io.reset := reset
    mgpio.io.in <> in
    gpio_bundle <> mgpio.io.gpio
  }
}
