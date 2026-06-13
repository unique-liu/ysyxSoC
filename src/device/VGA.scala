package ysyx

import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.apb._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class VGAIO extends Bundle {
  val r = Output(UInt(8.W))
  val g = Output(UInt(8.W))
  val b = Output(UInt(8.W))
  val hsync = Output(Bool())
  val vsync = Output(Bool())
  val valid = Output(Bool())
}

class VGACtrlIO extends Bundle {
  val clock = Input(Clock())
  val reset = Input(Bool())
  val in = Flipped(new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32)))
  val vga = new VGAIO
}

class vga_top_apb extends BlackBox {
  val io = IO(new VGACtrlIO)
}

class vgaChisel extends Module {
  val io = IO(new VGACtrlIO)
  val vga_buf = SyncReadMem(640*480, UInt(32.W))// 640*480 resolution, each pixel is 32 bits (ARGB) assume ARGB: [7:0]=B [15:8]=G [23:16]=R [31:24]=A(not used)
  val read_addr = RegInit(1.U(19.W))// C0:read pix1 output invalid value of pix0  C1:read pix2 and output pix1 
  val read_data = Reg(UInt(32.W))



  val idle :: read :: write :: Nil = Enum(3)
  val state = RegInit(idle)
  io.in.pready := false.B
  io.in.prdata := 0.U(32.W)
  io.in.pslverr := false.B
  switch(state) {
    is(idle) {
      when(io.in.psel && io.in.penable && io.in.pwrite) {
        state := write
      }
    }
    is(write) {
      state := idle
      io.in.pready := true.B
      vga_buf.write(io.in.paddr(20,2), io.in.pwdata) // write pixel data to buffer, address is word aligned
    }
  }

  // VGA signal generation
  io.vga.valid := true.B
  io.vga.hsync := false.B
  io.vga.vsync := false.B
  when(state =/= write || io.in.paddr(20,2) =/= read_addr) { // During read or when not writing to the current pixel, update the read address and data
    read_data := vga_buf.read(read_addr)
    read_addr := Mux(read_addr === (640*480-1).U, 0.U, read_addr + 1.U)
  }.otherwise{
    // During write, keep outputting the current pixel to avoid glitches
    read_addr := Mux(read_addr === (640*480-1).U, 0.U, read_addr + 1.U)
  }
  io.vga.r := read_data(23,16)
  io.vga.g := read_data(15,8)
  io.vga.b := read_data(7,0)
}

class APBVGA(address: Seq[AddressSet])(implicit p: Parameters) extends LazyModule {
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
    val vga_bundle = IO(new VGAIO)

    val mvga = Module(new vgaChisel)
    mvga.io.clock := clock
    mvga.io.reset := reset
    mvga.io.in <> in
    vga_bundle <> mvga.io.vga
  }
}
