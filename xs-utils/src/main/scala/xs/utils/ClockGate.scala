package xs.utils

import chisel3._
import chisel3.experimental.BaseModule
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import scala.collection.mutable

class ClockGate extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val TE = Input(Bool())
    val E = Input(Bool())
    val CK = Input(Clock())
    val Q = Output(Clock())
  })
  // Do not collide with utility.ClockGate, which has different synthesis behavior.
  private val modName = s"${GlobalData.prefix}XSUtilsClockGate"
  override val desiredName = modName
  setInline(s"$modName.sv",
    s"""
      |// VCS coverage exclude_file
      |module $modName (
      |  input  wire TE,
      |  input  wire E,
      |  input  wire CK,
      |  output wire Q
      |);
      |`ifdef SYNTHESIS
      |  assign Q = CK;
      |`elsif BYPASS_CLOCKGATE
      |  assign Q = CK;
      |`else
      |  reg EN;
      |  always_latch begin
      |    if(!CK) EN = TE | E;
      |  end
      |  assign Q = CK & EN;
      |`endif
      |endmodule""".stripMargin)
}

class CgteBundle extends Bundle {
  val te = Input(Bool())
}
object ClockGate {
  private val teBoringQueue = new mutable.Queue[CgteBundle]
  private val hashModulesHasCgen = new mutable.Queue[BaseModule]
  def apply(TE:Bool, E:Bool, CK:Clock):Clock = {
    val module = Module.currentModule.get
    val cgbd = if(hashModulesHasCgen.contains(module)) {
      teBoringQueue.last
    } else {
      val cg = Wire(new CgteBundle)
      cg.te := TE
      dontTouch(cg)
      teBoringQueue.append(cg)
      hashModulesHasCgen.append(module)
      cg
    }
    val clockGate = Module(new ClockGate)
    clockGate.io.E := E || withClock(CK){ RegNext(E, true.B) }
    clockGate.io.TE := cgbd.te
    clockGate.io.CK := CK
    clockGate.io.Q
  }
  def getTop:CgteBundle = {
    val cgen = Wire(new CgteBundle)
    teBoringQueue.toSeq.foreach(BoringUtils.bore(_) := cgen)
    teBoringQueue.clear()
    cgen
  }
}
