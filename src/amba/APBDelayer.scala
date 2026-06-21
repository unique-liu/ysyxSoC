package ysyx

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.amba._
import freechips.rocketchip.amba.apb._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class APBDelayerIO extends Bundle {
  val clock = Input(Clock())
  val reset = Input(Reset())
  val in = Flipped(new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32)))
  val out = new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32))
}

class apb_delayer extends BlackBox {
  val io = IO(new APBDelayerIO)
}

class APBDelayerChisel extends Module {
  val r = 5 //delay times  npc is 500MHz soc is 100MHz
  val s = 8 //delay multipler & divider
  val s_power = 3 // power of s, use to fast divide
  val io = IO(new APBDelayerIO)

  if(r == 0){
    io.out <> io.in
  }else{
    val idle :: wait_finish :: wait_ret :: Nil = Enum(3)
    val state = RegInit(idle)
    val counter = RegInit(0.U(32.W))
    val reg_prdata = RegInit(0.U(32.W))
    val reg_pslverr = RegInit(0.U(1.W))
    switch(state){
      is(idle) {
        when(io.in.penable) {
          state := wait_finish
          counter := 0.U(32.W)
        }
      }
      is(wait_finish) {
        when(io.out.pready) {
          state := wait_ret
          reg_prdata := io.out.prdata
          reg_pslverr := io.out.pslverr
          counter := (counter + (r*s).U(32.W)) >> s_power.U(32.W)
        }.otherwise{
          counter := counter + (r*s).U(32.W)
        }
      }
      is(wait_ret) {
        when(counter === 0.U(32.W)) {
          state := idle
        }.otherwise{
          counter := counter - 1.U(32.W)
        }
      }
    }
    io.out.paddr    := io.in.paddr
    io.out.psel     := io.in.penable && (state =/= wait_ret)
    io.out.penable  := (state === wait_finish)
    io.out.pprot    := io.in.pprot
    io.out.pwrite   := io.in.pwrite
    io.out.pwdata   := io.in.pwdata
    io.out.pstrb    := io.in.pstrb
    io.in.prdata    := reg_prdata
    io.in.pslverr   := reg_pslverr
    io.in.pready    := (state === wait_ret) && (counter === 0.U(32.W))

  }
}

class APBDelayerWrapper(implicit p: Parameters) extends LazyModule {
  val node = APBIdentityNode()

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      val delayer = Module(new APBDelayerChisel)
      delayer.io.clock := clock
      delayer.io.reset := reset
      delayer.io.in <> in
      out <> delayer.io.out
    }
  }
}

object APBDelayer {
  def apply()(implicit p: Parameters): APBNode = {
    val apbdelay = LazyModule(new APBDelayerWrapper)
    apbdelay.node
  }
}
