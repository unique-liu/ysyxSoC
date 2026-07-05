package ysyx

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.amba._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class AXI4DelayerIO extends Bundle {
  val clock = Input(Clock())
  val reset = Input(Reset())
  val in = Flipped(new AXI4Bundle(AXI4BundleParameters(addrBits = 32, dataBits = 32, idBits = 4)))
  val out = new AXI4Bundle(AXI4BundleParameters(addrBits = 32, dataBits = 32, idBits = 4))
}

class axi4_delayer extends BlackBox {
  val io = IO(new AXI4DelayerIO)
}

class AXI4DelayerChisel extends Module {
  val r = 5 //delay times  npc is 500MHz soc is 100MHz
  val s = 8 //delay multipler & divider
  val s_power = 3 // power of s, use to fast divide
  val io = IO(new AXI4DelayerIO)

  if(r == 0 || !Config.useDelayer){
    io.out <> io.in
  }else{
    // read fsm
    val idle :: wait_finish :: wait_ret :: Nil = Enum(3)
    val rstate = RegInit(idle)
    val cnt_rdata = Reg(Vec(8, UInt(32.W)))//assume max burst length is 8
    val reg_rid   = RegInit(0.U(4.W))//it can not be a vector because the order of rdata is same as arvalid
    val reg_rdata = Reg(Vec(8, UInt(32.W)))//assume max burst length is 8
    val reg_rresp = Reg(Vec(8, UInt(2.W)))//assume max burst length is 8
    val ridx = RegInit(0.U(3.W))
    val ridx_max = RegInit(0.U(3.W))
    switch(rstate){
      is(idle) {
        when(io.in.ar.valid) {
          rstate := wait_finish
          ridx := 0.U(3.W)
          cnt_rdata := VecInit(Seq.fill(8)(0.U(32.W)))
        }
      }
      is(wait_finish) {
        when(io.out.r.valid && io.out.r.bits.last) {
          rstate := wait_ret
          reg_rid  := io.out.r.bits.id
          reg_rdata(ridx) := io.out.r.bits.data
          reg_rresp(ridx) := io.out.r.bits.resp
          ridx_max := ridx
          cnt_rdata(ridx) := (cnt_rdata(ridx) + (r*s).U(32.W)) >> s_power.U(32.W)
          ridx := 0.U(3.W)
        }.elsewhen(io.out.r.valid){
          reg_rdata(ridx) := io.out.r.bits.data
          reg_rresp(ridx) := io.out.r.bits.resp
          cnt_rdata(ridx) := (cnt_rdata(ridx) + (r*s).U(32.W)) >> s_power.U(32.W)
          ridx := ridx + 1.U(3.W)
        }.otherwise{
          cnt_rdata(ridx) := cnt_rdata(ridx) + (r*s).U(32.W)
        }
      }
      is(wait_ret) {
        when(cnt_rdata(ridx) === 0.U(32.W) && ridx === ridx_max) {
          rstate := idle
        }.elsewhen(cnt_rdata(ridx) === 0.U(32.W)){
          ridx := ridx + 1.U(3.W)
        }.otherwise{
          cnt_rdata(ridx) := cnt_rdata(ridx) - 1.U(32.W)
        }
      }
    }
    // ar
    io.out.ar <> io.in.ar
    // r
    io.in.r.valid   := (rstate === wait_ret) && (cnt_rdata(ridx) === 0.U(32.W))
    io.in.r.bits.id      := reg_rid
    io.in.r.bits.data    := reg_rdata(ridx)
    io.in.r.bits.resp    := reg_rresp(ridx)
    io.in.r.bits.last    := (rstate === wait_ret) && (cnt_rdata(ridx) === 0.U(32.W)) && (ridx === ridx_max)

    io.out.r.ready  := rstate === wait_finish

    // write fsm
    val wstate = RegInit(idle)
    val cnt_wdata = RegInit(0.U(32.W))
    val reg_wid   = RegInit(0.U(4.W))
    val reg_wresp = RegInit(0.U(2.W))
    switch(wstate){
      is(idle) {
        when(io.in.aw.valid || io.in.w.valid) {
          wstate := wait_finish
          cnt_wdata := 0.U(32.W)
        }
      }
      is(wait_finish) {
        when(io.out.b.valid) {
          wstate := wait_ret
          reg_wid  := io.out.b.bits.id
          reg_wresp := io.out.b.bits.resp
          cnt_wdata := (cnt_wdata + (r*s).U(32.W)) >> s_power.U(32.W)
        }.otherwise{
          cnt_wdata := cnt_wdata + (r*s).U(32.W)
        }
      }
      is(wait_ret) {
        when(cnt_wdata === 0.U(32.W)) {
          wstate := idle
        }.otherwise{
          cnt_wdata := cnt_wdata - 1.U(32.W)
        }
      }
    }
    // aw
    io.out.aw <> io.in.aw
    // w
    io.out.w <> io.in.w
    // b
    io.out.b.ready  := wstate === wait_finish

    io.in.b.valid   := (wstate === wait_ret) && (cnt_wdata === 0.U(32.W))
    io.in.b.bits.id      := reg_wid
    io.in.b.bits.resp    := reg_wresp
  }
}

class AXI4DelayerWrapper(implicit p: Parameters) extends LazyModule {
  val node = AXI4IdentityNode()

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      val delayer = Module(new AXI4DelayerChisel)
      delayer.io.clock := clock
      delayer.io.reset := reset
      delayer.io.in <> in
      out <> delayer.io.out
    }
  }
}

object AXI4Delayer {
  def apply()(implicit p: Parameters): AXI4Node = {
    val axi4delay = LazyModule(new AXI4DelayerWrapper)
    axi4delay.node
  }
}
