package ysyx

import chisel3._
import chisel3.util._

class bitrev extends BlackBox {
  val io = IO(Flipped(new SPIIO(1)))
}

class bitrevChisel extends RawModule { // we do not need clock and reset
  val io = IO(Flipped(new SPIIO(1)))
  val reg =withClockAndReset((~io.sck).asClock,io.ss.asBool.asAsyncReset) { RegInit(0.U(8.W)) }
  val counter =withClockAndReset((~io.sck).asClock,io.ss.asBool.asAsyncReset) { RegInit(0.U(4.W)) }
  counter := Mux(~counter(3), counter + 1.U, counter)
  reg := Mux(~counter(3),Cat(reg(6,0), io.mosi), Cat(0.U(1.W),reg(7,1))) // shift in the bits on the rising edge of sck
  io.miso := Mux(io.ss.asBool ,1.U, reg(0) & counter(3))
}
