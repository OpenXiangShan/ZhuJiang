package dongjiang.frontend

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import org.chipsalliance.cde.config._
import zhujiang.chi._
import zhujiang.chi.ReqOpcode._
import zhujiang.chi.DatOpcode._
import dongjiang._
import dongjiang.backend.{CMTask, CommitTask}
import dongjiang.utils._
import dongjiang.bundle._
import dongjiang.directory._
import xs.utils.debug._
import zhujiang.perf.ZJPerf
import dongjiang.frontend.decode._
import dongjiang.data._
import xs.utils.ParallelLookUp
import dongjiang.backend.GetDecRes
import dongjiang.frontend.decode.Decode._
import xs.utils.GTimer

class FstDec(implicit p: Parameters) extends Module {
    val io = IO(new Bundle {
        val inst = Input(new ChiInst)
        val out  = Output(MixedVec(UInt(log2Ceil(Decode.l_ci).W), UInt(log2Ceil(Decode.l_si).W), UInt(log2Ceil(Decode.l_ti).W), UInt(log2Ceil(Decode.l_sti).W)))
    })
    io.out := Decode.firstDec(io.inst)
}

class SecDec(implicit p: Parameters) extends DJModule {
    val io = IO(new Bundle {
        val inst = Input(new StateInst)
        val in   = Input(MixedVec(UInt(log2Ceil(Decode.l_ci).W), UInt(log2Ceil(Decode.l_si).W), UInt(log2Ceil(Decode.l_ti).W), UInt(log2Ceil(Decode.l_sti).W)))
        val out  = Output(MixedVec(UInt(log2Ceil(Decode.l_ci).W), UInt(log2Ceil(Decode.l_si).W), UInt(log2Ceil(Decode.l_ti).W), UInt(log2Ceil(Decode.l_sti).W)))
    })
    io.out := Decode.secondDec(io.in, io.inst)
}

class Decode(implicit p: Parameters) extends DJModule {

    val io = IO(new Bundle {

        val config = new DJConfigIO()

        val task_s2 = Flipped(Valid(new PackChi with HasAddr with HasPackHnIdx with HasAlready with HasQoS))

        val respDir_s3 = Flipped(Valid(new DirMsg))

        val cmtTask_s3 = Valid(new CommitTask with HasHnTxnID)

        val reqDB_s3    = Decoupled(new HnTxnID with HasDataVec)
        val fastData_s3 = Decoupled(new DataTask)
        val cleanDB_s3  = Valid(new HnTxnID with HasDataVec)

    })
    dontTouch(io)

    val fstDec       = Module(new FstDec)
    val secDec       = Module(new SecDec)
    val getDecRes    = Module(new GetDecRes)
    val validReg_s3  = RegNext(io.task_s2.valid, false.B)
    val taskReg_s3   = RegEnable(io.task_s2.bits, io.task_s2.valid)
    val stateInst_s3 = Wire(new StateInst())

    val chiInst_s2 = WireInit(io.task_s2.bits.chi.getChiInst)
    val respDir_s3 = Mux(io.respDir_s3.valid, io.respDir_s3.bits, 0.U.asTypeOf(new DirMsg()))
    stateInst_s3 := Mux(io.respDir_s3.valid, io.respDir_s3.bits.getStateInst(taskReg_s3.chi.metaIdOH), (new StateInst).Lit(_.valid -> true.B))

    chiInst_s2.valid   := io.task_s2.valid
    stateInst_s3.valid := validReg_s3

    dontTouch(chiInst_s2)
    dontTouch(stateInst_s3)

    HAssert.withEn(validReg_s3 & taskReg_s3.chi.memAttr.cacheable, io.respDir_s3.valid)
    HAssert.withEn(io.respDir_s3.valid, validReg_s3 & taskReg_s3.chi.memAttr.cacheable)
    HAssert.withEn(!io.respDir_s3.valid, validReg_s3 & !taskReg_s3.chi.memAttr.cacheable)

    fstDec.io.inst := chiInst_s2
    val decList_s2 = fstDec.io.out

    secDec.io.inst := stateInst_s3
    secDec.io.in   := RegEnable(decList_s2, io.task_s2.valid)
    val decList_s3 = secDec.io.out

    getDecRes.io.list.valid := validReg_s3
    getDecRes.io.list.bits  := decList_s3
    val taskCode_s3 = getDecRes.io.taskCode
    val cmtCode_s3  = getDecRes.io.commitCode
    dontTouch(decList_s2)
    dontTouch(decList_s3)

    val respCompData_s3 = validReg_s3 & !taskCode_s3.isValid & cmtCode_s3.sendResp &
        cmtCode_s3.channel === ChiChannel.DAT & cmtCode_s3.opcode === CompData

    io.cmtTask_s3.valid            := validReg_s3
    io.cmtTask_s3.bits.hnTxnID     := taskReg_s3.hnIdx.getTxnID
    io.cmtTask_s3.bits.qos         := taskReg_s3.qos
    io.cmtTask_s3.bits.chi         := taskReg_s3.chi
    io.cmtTask_s3.bits.chi.dataVec := taskReg_s3.chi.dataVec
    io.cmtTask_s3.bits.dir         := respDir_s3
    io.cmtTask_s3.bits.alr.reqDB   := io.reqDB_s3.fire | taskReg_s3.alr.reqDB
    io.cmtTask_s3.bits.alr.sData   := io.fastData_s3.fire
    io.cmtTask_s3.bits.alr.sDBID   := taskReg_s3.alr.sDBID
    io.cmtTask_s3.bits.decList     := decList_s3
    io.cmtTask_s3.bits.task        := taskCode_s3
    io.cmtTask_s3.bits.cmt         := Mux(taskCode_s3.isValid, 0.U.asTypeOf(new CommitCode), cmtCode_s3)
    io.cmtTask_s3.bits.ds.set(taskReg_s3.addr, respDir_s3.llc.way)
    HardwareAssertion.withEn(taskCode_s3.isValid | cmtCode_s3.isValid, validReg_s3)
    HardwareAssertion.withEn(respDir_s3.sf.metaIsVal, validReg_s3 & taskCode_s3.snoop)

    io.reqDB_s3.valid        := respCompData_s3
    io.reqDB_s3.bits.hnTxnID := taskReg_s3.hnIdx.getTxnID
    io.reqDB_s3.bits.dataVec := taskReg_s3.chi.dataVec
    HAssert.withEn(!taskReg_s3.alr.reqDB, io.reqDB_s3.valid)

    io.fastData_s3.valid        := respCompData_s3 & io.reqDB_s3.ready
    io.fastData_s3.bits         := DontCare
    io.fastData_s3.bits.hnTxnID := taskReg_s3.hnIdx.getTxnID

    io.fastData_s3.bits.txDat.DBID    := taskReg_s3.hnIdx.getTxnID
    io.fastData_s3.bits.txDat.Resp    := cmtCode_s3.resp
    io.fastData_s3.bits.txDat.Opcode  := CompData
    io.fastData_s3.bits.txDat.HomeNID := DontCare
    io.fastData_s3.bits.txDat.TxnID   := taskReg_s3.chi.txnID
    io.fastData_s3.bits.txDat.SrcID   := taskReg_s3.chi.getNoC
    io.fastData_s3.bits.txDat.TgtID   := taskReg_s3.chi.nodeId

    io.fastData_s3.bits.dataOp      := 0.U.asTypeOf(new DataOp)
    io.fastData_s3.bits.dataOp.read := true.B
    io.fastData_s3.bits.dataOp.send := true.B
    io.fastData_s3.bits.dataVec     := taskReg_s3.chi.dataVec
    io.fastData_s3.bits.ds.set(taskReg_s3.addr, respDir_s3.llc.way)
    HardwareAssertion.withEn(cmtCode_s3.dataOp.isValid, io.fastData_s3.valid)

    ZJPerf.whenEnabled {
        val isCacheableReq = validReg_s3 &&
            taskReg_s3.chi.channel === ChiChannel.REQ && taskReg_s3.chi.memAttr.cacheable
        val isReadNsd    = isCacheableReq && taskReg_s3.chi.opcode === ReadNotSharedDirty
        val isReadUnique = isCacheableReq && taskReg_s3.chi.opcode === ReadUnique
        val isReadOnce   = isCacheableReq && taskReg_s3.chi.opcode === ReadOnce
        val demandRead   = isReadNsd || isReadUnique
        val localHit     = isReadNsd && respDir_s3.llc.hit && respCompData_s3
        val perfTimer    = GTimer()
        val ingressCycle = taskReg_s3.chi.perfIngressCycle.get
        val cmtPerf      = io.cmtTask_s3.bits.perf.get

        cmtPerf.valid                := localHit
        cmtPerf.demandRead           := demandRead
        cmtPerf.llcHit               := respDir_s3.llc.hit
        cmtPerf.ingressCycle         := ingressCycle
        cmtPerf.decodeCycle          := perfTimer
        io.fastData_s3.bits.perf.get := cmtPerf

        ZJPerf.accumulate(
            Seq(
                ("zj_hn_readnsd", isReadNsd),
                ("zj_hn_readnsd_llc_hit", isReadNsd && respDir_s3.llc.hit),
                ("zj_hn_readnsd_llc_miss", isReadNsd && !respDir_s3.llc.hit),
                ("zj_hn_readunique", isReadUnique),
                ("zj_hn_readunique_llc_hit", isReadUnique && respDir_s3.llc.hit),
                ("zj_hn_readonce", isReadOnce),
                ("zj_hn_readonce_llc_hit", isReadOnce && respDir_s3.llc.hit)
            )
        )

        val localHitFastData         = localHit && io.fastData_s3.fire
        val demandLlcHitFastData     = demandRead && respDir_s3.llc.hit && io.fastData_s3.fire
        val localHitDeferredReqDB    = localHit && !io.reqDB_s3.ready
        val localHitDeferredDataTask = localHit && io.reqDB_s3.ready && !io.fastData_s3.ready
        HAssert.withEn(
            PopCount(Seq(localHitFastData, localHitDeferredReqDB, localHitDeferredDataTask)) === 1.U,
            localHit
        )
        ZJPerf.accumulate(
            Seq(
                ("zj_local_hit_fast_data_fire", localHitFastData),
                ("zj_local_hit_deferred_reqdb", localHitDeferredReqDB),
                ("zj_local_hit_deferred_datatask", localHitDeferredDataTask)
            )
        )
        ZJPerf.distribution("zj_local_hit_ingress_to_decode", perfTimer - ingressCycle, localHit)
        ZJPerf.distribution("zj_hn_llc_hit_fast_return", perfTimer - ingressCycle, demandLlcHitFastData)
    }

    val cleanUnuseDB_s3 = validReg_s3 & taskReg_s3.alr.reqDB & !taskReg_s3.chi.isFullSize & !(respDir_s3.sf.hit | respDir_s3.llc.hit)
    io.cleanDB_s3.valid        := cleanUnuseDB_s3
    io.cleanDB_s3.bits.hnTxnID := taskReg_s3.hnIdx.getTxnID
    io.cleanDB_s3.bits.dataVec := VecInit(taskReg_s3.chi.dataVec.map(!_))

    HAssert(!(respCompData_s3 & cleanUnuseDB_s3))
    HAssert.withEn(io.cleanDB_s3.bits.isHalfSize, io.cleanDB_s3.valid)

    ZJPerf.accumulate(
        Seq(
            ("zj_hn_stash_req", validReg_s3 & taskReg_s3.chi.reqIs(StashOnceShared)),
            ("zj_hn_stash_hit", validReg_s3 & taskReg_s3.chi.reqIs(StashOnceShared) & respDir_s3.llc.hit),
            ("zj_hn_stash_miss", validReg_s3 & taskReg_s3.chi.reqIs(StashOnceShared) & !respDir_s3.llc.hit)
        )
    )

    HardwareAssertion.placePipe(1)
}
