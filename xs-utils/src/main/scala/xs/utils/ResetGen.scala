/***************************************************************************************
* Copyright (c) 2020-2022 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2022 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package xs.utils

import chisel3._
import chisel3.util._
class DFTResetSignals extends Bundle{
  val lgc_rst_n = AsyncReset()
  val mode = Bool()
  val scan_mode = Bool()
}

class ResetGenIO extends Bundle {
  val i_clock = Input(Clock())
  val i_reset = Input(AsyncReset())
  val i_dft_lgc_rst_n = Input(AsyncReset())
  val i_dft_mode = Input(Bool())
  val i_dft_scan_mode = Input(Bool())
  val o_reset = Output(AsyncReset())
  val o_raw_reset = Output(AsyncReset())
}

// ResetGenInner remains as a BlackBox with inline SystemVerilog.
// Pure Chisel Module approach was attempted twice but fails with gsim:
// 1. RawModule + withClockAndReset: Chisel generates _T signals that gsim
//    cannot resolve when inlining across ZhuJiang's deep module hierarchy
//    (error: use of undeclared identifier '_io_o_raw_reset_T').
// 2. Module (implicit clock): same _T signal issue; plus ResetGen callers
//    sometimes override the implicit clock (resetGen.clock := ...), which
//    creates additional complications for Chisel's clock domain tracking.
//
// The utility.ResetGen in mainline XiangShan works as pure Chisel because
// it's instantiated at the TOP level where gsim handles the inlining
// differently. ZhuJiang's ResetGen is deep in the module hierarchy where
// gsim's cross-module optimization creates these undeclared references.
//
// A C++ implementation is provided in difftest/src/test/csrc/common/zhujiang-extmodule.cpp
// as a passthrough (reset output = reset input). Synchronization is a timing
// construct; passthrough is functionally correct for simulation.
class ResetGenInner(SYNC_NUM: Int = 2) extends BlackBox with HasBlackBoxInline {
  require(SYNC_NUM > 1)
  val io = IO(new ResetGenIO)
  private val modName = s"${GlobalData.prefix}ResetGenInnerS${SYNC_NUM}"
  override val desiredName = modName

  // Register C++ extension module for gsim compilation.
  // Per-instance state cannot be maintained (static variables are shared);
  // passthrough is functionally correct since synchronization is timing-only.
  // NOTE: gsim strips the clock parameter from extmodule function signatures.
  private val cppExtModule =
    s"""
       |void $modName (
       |  unsigned char i_reset,
       |  unsigned char i_dft_lgc_rst_n,
       |  unsigned char i_dft_mode,
       |  unsigned char i_dft_scan_mode,
       |  unsigned char& o_reset,
       |  unsigned char& o_raw_reset
       |) {
       |  unsigned char lgc_rst = (i_dft_lgc_rst_n == 0) ? 1 : 0;
       |  unsigned char real_reset = i_dft_mode ? lgc_rst : i_reset;
       |  o_raw_reset = real_reset;
       |  o_reset = i_dft_scan_mode ? lgc_rst : real_reset;
       |}
       |""".stripMargin
  difftest.DifftestModule.createCppExtModule(modName, cppExtModule)

  setInline(s"$modName.sv",
    s"""// VCS coverage exclude_file
       |module $modName (
       |  input  i_clock,
       |  input  i_reset,
       |  input  i_dft_lgc_rst_n,
       |  input  i_dft_mode,
       |  input  i_dft_scan_mode,
       |  output o_reset,
       |  output o_raw_reset
       |);
       |  wire reset = i_dft_mode ? ~i_dft_lgc_rst_n : i_reset;
       |  reg [${SYNC_NUM - 1}:0] shifter;
       |
       |`ifndef SYNTHESIS
       |  initial shifter = ${SYNC_NUM}'d${(1 << SYNC_NUM) - 1};
       |`endif
       |
       |  always @(posedge i_clock or posedge reset) begin
       |    if (reset) begin
       |      shifter <= ${SYNC_NUM}'d${(1 << SYNC_NUM) - 1};
       |    end else begin
       |      shifter <= {1'b0, shifter[${SYNC_NUM - 1}:1]};
       |    end
       |  end
       |  assign o_raw_reset = shifter[0];
       |  assign o_reset = i_dft_scan_mode ? ~i_dft_lgc_rst_n : shifter[0];
       |endmodule""".stripMargin)
}

class ResetGen(SYNC_NUM: Int = 2) extends Module {
  override val desiredName = s"ResetGenS${SYNC_NUM}"
  val o_reset = IO(Output(AsyncReset()))
  val dft = IO(Input(new DFTResetSignals()))
  val raw_reset = IO(Output(AsyncReset()))

  private val inner = Module(new ResetGenInner(SYNC_NUM))
  inner.io.i_reset := reset
  inner.io.i_clock := clock
  inner.io.i_dft_lgc_rst_n := dft.lgc_rst_n
  inner.io.i_dft_scan_mode := dft.scan_mode
  inner.io.i_dft_mode := dft.mode
  raw_reset := inner.io.o_raw_reset
  o_reset := inner.io.o_reset
}

trait ResetNode

case class ModuleNode(mod: Module) extends ResetNode
case class CellNode(reset: Reset) extends ResetNode
case class ResetGenNode(children: Seq[ResetNode]) extends ResetNode

object ResetGen {
  def apply(SYNC_NUM: Int = 2, dft:Option[DFTResetSignals]): AsyncReset = {
    val resetSync = Module(new ResetGen(SYNC_NUM))
    if(dft.isDefined) {
      resetSync.dft := dft.get
    } else {
      resetSync.dft := 0.U.asTypeOf(new DFTResetSignals)
    }
    resetSync.o_reset
  }

  def apply(resetTree: ResetNode, reset: Reset, dft:Option[DFTResetSignals], sim: Boolean): Unit = {
    if(!sim) {
      resetTree match {
        case ModuleNode(mod) =>
          mod.reset := reset
        case CellNode(r) =>
          r := reset
        case ResetGenNode(children) =>
          val next_rst = Wire(Reset())
          withReset(reset){
            val resetGen = Module(new ResetGen)
            next_rst := resetGen.o_reset
            if(dft.isDefined) {
              resetGen.dft := dft.get
            } else {
              resetGen.dft := 0.U.asTypeOf(new DFTResetSignals)
            }
          }
          children.foreach(child => apply(child, next_rst, dft, sim))
      }
    }
  }

  def apply(resetChain: Seq[Seq[Module]], reset: Reset,  dft:Option[DFTResetSignals], sim: Boolean): Seq[Reset] = {
    val resetReg = Wire(Vec(resetChain.length + 1, Reset()))
    resetReg.foreach(_ := reset)
    for ((resetLevel, i) <- resetChain.zipWithIndex) {
      if (!sim) {
        withReset(resetReg(i)) {
          val resetGen = Module(new ResetGen)
          resetReg(i + 1) := resetGen.o_reset
          if(dft.isDefined) {
            resetGen.dft := dft.get
          } else {
            resetGen.dft := 0.U.asTypeOf(new DFTResetSignals)
          }
        }
      }
      resetLevel.foreach(_.reset := resetReg(i + 1))
    }
    resetReg.tail
  }

  def applyOneLevel(resetSigs: Seq[Reset], reset: Reset, sim: Boolean): DFTResetSignals = {
    val resetReg = Wire(Reset())
    val dft = Wire(new DFTResetSignals())
    resetReg := reset
    if (!sim) {
      withReset(reset) {
        val resetGen = Module(new ResetGen)
        resetReg := resetGen.o_reset
        resetGen.dft := dft
      }
    }
    resetSigs.foreach(_ := resetReg)
    dft
  }
}
