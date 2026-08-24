package dongjiang.backend

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import zhujiang.chi._
import zhujiang.chi.RspOpcode._
import zhujiang.chi.DatOpcode._
import dongjiang._
import dongjiang.utils._
import dongjiang.bundle._
import xs.utils.debug._
import dongjiang.directory.{DirEntry, DirMsg, HasPackDirMsg}
import dongjiang.data._
import zhujiang.chi.ReqOpcode._
import dongjiang.frontend._
import dongjiang.frontend.decode._

class CommitTask(implicit p: Parameters) extends DJBundle with HasPackChi with HasPackDirMsg with HasAlready with HasDsIdx with HasDecList with HasPackTaskCode with HasPackCmtCode with HasQoS {
    def isReplLLC = cmt.wriLLC & !dir.llc.hit
}

class CMTask(implicit p: Parameters) extends DJBundle with HasHnTxnID with HasPackChi with HasPackDataOp with HasDsIdx with HasQoS {
    val fromRepl = Bool()
    val snpVec   = Vec(nrSfMetas, Bool())
    val cbResp   = UInt(ChiResp.width.W)
    val doDMT    = Bool()
}

trait HasPackCMTask { this: DJBundle =>
    val task = new CMTask
}

class CMResp(implicit p: Parameters) extends DJBundle with HasHnTxnID with HasPackTaskInst with HasQoS with HasRespErr {
    val toRepl = Bool()
}

class ReplTask(implicit p: Parameters) extends DJBundle with HasHnTxnID with HasPackDirMsg with HasQoS {
    val wriSF  = Bool()
    val wriLLC = Bool()
    val directAllocSF = Bool()
    def isDirectAllocSF = wriSF & !dir.sf.hit & directAllocSF
    def isReplSF = wriSF & !dir.sf.hit & !directAllocSF
    def isReplLLC = wriLLC & !dir.llc.hit
    def isReplDIR = isReplSF | isReplLLC
}

class UpdHnTxnID(implicit p: Parameters) extends DJBundle {
    val before = UInt(hnTxnIDBits.W)
    val next   = UInt(hnTxnIDBits.W)
}
