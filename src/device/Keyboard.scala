package ysyx

import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.apb._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class PS2IO extends Bundle {
  val clk = Input(Bool())
  val data = Input(Bool())
}

class PS2CtrlIO extends Bundle {
  val clock = Input(Clock())
  val reset = Input(Bool())
  val in = Flipped(new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32)))
  val ps2 = new PS2IO
}

class ps2_top_apb extends BlackBox {
  val io = IO(new PS2CtrlIO)
}

class translator extends Bundle {
  val clock = Input(Clock())
  val keycode = Input(UInt(8.W))
  val keycode_valid = Input(Bool())
  val keycode_extend = Input(Bool())
  val keycode_release = Input(Bool())
  val amcode = Output(UInt(8.W))
}

class ps2_translator extends BlackBox {
  val io = IO(new translator)
}

class ps2Chisel extends Module {
  val io = IO(new PS2CtrlIO)
  val translator = Module(new ps2_translator)
  val ps2_idel :: ps2_read :: ps2_save :: Nil = Enum(3)
  val ps2_state = RegInit(ps2_idel)
  val ps2_reg = RegInit(0.U(8.W))
  val ps2_cnt = RegInit(0.U(4.W))
  val ps2_ext = RegInit(false.B)
  val ps2_release = RegInit(false.B)
  val ps2_amcode_fifo = RegInit(VecInit(Seq.fill(8)(0.U(8.W))))
  val ps2_amcode_num = RegInit(0.U(3.W))
  val ps2_amcode_out_idx = RegInit(0.U(3.W))
  val ps2_amcode_in_idx = RegInit(0.U(3.W))
  val out_data = Wire(UInt(8.W))
  val ps2_oldclk = RegInit(false.B)
  val ps2_have_data = RegInit(false.B)

  val idle :: read :: write :: Nil = Enum(3)
  val state = RegInit(idle)
  io.in.pready := false.B
  io.in.prdata := 0.U(32.W)
  io.in.pslverr := false.B
  switch(state) {
    is(idle) {
      when(io.in.psel && io.in.penable) {
        state := read
      }
    }
    is(read) {
      state := idle
      io.in.pready := true.B
      io.in.prdata := Cat(0.U(24.W),out_data)
    }
  }

  ps2_oldclk := io.ps2.clk

  translator.io.clock := clock
  translator.io.keycode := ps2_reg
  translator.io.keycode_valid := false.B
  translator.io.keycode_extend := ps2_ext
  translator.io.keycode_release := ps2_release
  when(ps2_oldclk === true.B && io.ps2.clk === false.B) { // falling edge of PS2 clock
    switch(ps2_state) {
      is(ps2_idel) {
        when(io.ps2.data === 0.U) { // start bit
          ps2_state := (ps2_read)
          ps2_cnt := 0.U(4.W)
        }
      }
      is(ps2_read) {
        ps2_cnt := ps2_cnt + 1.U
        when(ps2_cnt < 8.U(8.W)) { // data bits
          ps2_reg := Cat(io.ps2.data, ps2_reg(7,1)) // low bit first
        }.elsewhen(ps2_cnt === 9.U(4.W)) { //parity and stop bit are ignored
          when(ps2_reg === 0xE0.U(8.W)) { // extended key prefix
            ps2_state := ps2_idel
            ps2_ext := true.B
          }.elsewhen(ps2_reg === 0xF0.U(8.W)) { // break code prefix
            ps2_state := ps2_idel
            ps2_release := true.B
          }.otherwise {
            ps2_state := ps2_idel
            translator.io.keycode_valid := true.B
            ps2_release := false.B
            ps2_ext := false.B
          }
        }
      }
    }
  }

  when(translator.io.keycode_valid){//just to delay one cycle
    ps2_have_data := true.B
  }.otherwise {
    ps2_have_data := false.B
  }

  out_data := 0.U(8.W)
  when(ps2_have_data && ps2_amcode_num < 8.U) {
    ps2_amcode_fifo(ps2_amcode_in_idx) := translator.io.amcode
    ps2_amcode_in_idx := ps2_amcode_in_idx + 1.U
    ps2_amcode_num := ps2_amcode_num + 1.U
  }.elsewhen(state === read && ps2_amcode_num > 0.U) {
    out_data := ps2_amcode_fifo(ps2_amcode_out_idx)
    ps2_amcode_out_idx := ps2_amcode_out_idx + 1.U
    ps2_amcode_num := ps2_amcode_num - 1.U
  }

}

class APBKeyboard(address: Seq[AddressSet])(implicit p: Parameters) extends LazyModule {
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
    val ps2_bundle = IO(new PS2IO)

    val mps2 = Module(new ps2Chisel)
    mps2.io.clock := clock
    mps2.io.reset := reset
    mps2.io.in <> in
    ps2_bundle <> mps2.io.ps2
  }
}
