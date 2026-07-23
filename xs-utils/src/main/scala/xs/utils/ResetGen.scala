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

class ResetGen(SYNC_NUM: Int = 2) extends Module {
  require(SYNC_NUM > 1)
  override val desiredName = s"ResetGenS${SYNC_NUM}"
  val o_reset = IO(Output(Bool()))
  val dft = IO(Input(new DFTResetSignals()))

  private val selectedReset = Mux(dft.mode, !dft.lgc_rst_n.asBool, reset.asBool).asAsyncReset
  private val value = withClockAndReset(clock, selectedReset) {
    val value = RegInit(((1L << SYNC_NUM) - 1).U(SYNC_NUM.W))
    value := Cat(false.B, value(SYNC_NUM - 1, 1))
    value
  }
  o_reset := Mux(dft.scan_mode, !dft.lgc_rst_n.asBool, value(0))
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
    resetSync.o_reset.asAsyncReset
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
            next_rst := resetGen.o_reset.asAsyncReset
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
          resetReg(i + 1) := resetGen.o_reset.asAsyncReset
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
        resetReg := resetGen.o_reset.asAsyncReset
        resetGen.dft := dft
      }
    }
    resetSigs.foreach(_ := resetReg)
    dft
  }
}
