package xs.utils.sram

import chisel3._
import chisel3.util._
import xs.utils.GlobalData

class SpRamRwIO(dw: Int, be: Int, set: Int) extends Bundle {
  val clk = Input(Clock())
  val addr = Input(UInt(log2Ceil(set).W))
  val en = Input(Bool())
  val wmode = Input(Bool())
  val wmask = if (be > 1) Some(Input(UInt(be.W))) else None
  val wdata = Input(UInt(dw.W))
  val rdata = Output(UInt(dw.W))
}

class DpRamRIO(dw: Int, set: Int) extends Bundle {
  val clk = Input(Clock())
  val addr = Input(UInt(log2Ceil(set).W))
  val en = Input(Bool())
  val data = Output(UInt(dw.W))
}

class DpRamWIO(dw: Int, be: Int, set: Int) extends Bundle {
  val clk = Input(Clock())
  val addr = Input(UInt(log2Ceil(set).W))
  val en = Input(Bool())
  val data = Input(UInt(dw.W))
  val mask = if (be > 1) Some(Input(UInt(be.W))) else None
}

/**
  * FIRRTL-visible generic SRAM implementation.
  *
  * The previous implementation emitted an inline SystemVerilog blackbox. gsim
  * cannot generate a C++ model for that blackbox, whereas SyncReadMem lowers to
  * a FIRRTL memory that it can model directly. `delay` remains part of the
  * module signature and generated name for compatibility; gsim's former RTL
  * path did not enable the DELAY_READ preprocessor branch.
  */
class SramInstGen(sp: Boolean, dw: Int, be: Int, set: Int, delay: Boolean) extends RawModule {
  val io = IO(new Bundle {
    val RW0 = if (sp) Some(new SpRamRwIO(dw, be, set)) else None
    val R0 = if (!sp) Some(new DpRamRIO(dw, set)) else None
    val W0 = if (!sp) Some(new DpRamWIO(dw, be, set)) else None
  })

  private val moduleName =
    s"${GlobalData.prefix}GENERIC_RAM_${if (sp) 1 else 2}P${set}D${dw}W${be}M${if (delay) "D" else ""}"
  override val desiredName = moduleName

  if (sp) {
    val rw = io.RW0.get
    if (be > 1) {
      val dataType = Vec(be, UInt((dw / be).W))
      val mem = SyncReadMem(set, dataType)
      rw.rdata := mem.readWrite(rw.addr, rw.wdata.asTypeOf(dataType), rw.wmask.get.asBools, rw.en, rw.wmode, rw.clk).asUInt
    } else {
      val mem = SyncReadMem(set, UInt(dw.W))
      rw.rdata := mem.readWrite(rw.addr, rw.wdata, rw.en, rw.wmode, rw.clk)
    }
  } else {
    val r = io.R0.get
    val w = io.W0.get
    if (be > 1) {
      val dataType = Vec(be, UInt((dw / be).W))
      val mem = SyncReadMem(set, dataType)
      when(w.en) {
        mem.write(w.addr, w.data.asTypeOf(dataType), w.mask.get.asBools, w.clk)
      }
      r.data := mem.read(r.addr, r.en, r.clk).asUInt
    } else {
      val mem = SyncReadMem(set, UInt(dw.W))
      when(w.en) {
        mem.write(w.addr, w.data, w.clk)
      }
      r.data := mem.read(r.addr, r.en, r.clk)
    }
  }
}
