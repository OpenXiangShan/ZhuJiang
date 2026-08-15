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
import dongjiang.frontend.decode._
import dongjiang.frontend.PosClean
import dongjiang.backend.CmtState._
import chisel3.experimental.BundleLiterals._
import dongjiang.frontend.decode.Decode.{w_ci, w_si, w_sti, w_ti}
import xs.utils.perf.XSPerfAccumulate

object CmtState {
    val width   = 3
    val FREE    = 0.U
    val FSTTASK = 1.U
    val SECTASK = 2.U
    val COMMIT  = 3.U
    val CLEAN   = 4.U
}

class Flag(implicit p: Parameters) extends DJBundle {
    val intl = new DJBundle {

        val s = new DJBundle {
            val decode   = Bool()
            val reqDB    = Bool()
            val cmTask   = Bool()
            val dataTask = Bool()
            val wriDir   = Bool()

        }

        val w = new DJBundle {
            val cmResp   = Bool()
            val replResp = Bool()
            val dataResp = Bool()
        }
    }

    val chi = new DJBundle {
        val s = new DJBundle {
            val dbid = Bool()
            val resp = Bool()
        }
        val w = new DJBundle {
            val xCBWrData0 = Bool()
            val xCBWrData1 = Bool()
            val compAck    = Bool()
            require(djparam.nrBeat == 2)
        }
    }
}

class State(implicit p: Parameters) extends DJBundle {

    val value = UInt(CmtState.width.W)
    def isFree = value === FREE
    def isFstTask = value === FSTTASK
    def isSecTask = value === SECTASK
    def isCommit = value === COMMIT
    def isClean = value === CLEAN
}

class CommitEntry(implicit p: Parameters) extends DJModule {

    val io = IO(new DJBundle {
        val config  = new DJConfigIO
        val hnTxnID = Input(UInt(hnTxnIDBits.W))
        val hnIdx   = Input(new HnIndex)

        val alloc = Flipped(Valid(new CommitTask with HasHnTxnID))

        val trdDecOut = Decoupled(new DJBundle with HasPackTaskInst with HasDecList with HasHnTxnID)
        val fthDecOut = Decoupled(new DJBundle with HasPackTaskInst with HasDecList with HasHnTxnID)

        val decListIn  = Flipped(Valid(MixedVec(UInt(w_ci.W), UInt(w_si.W), UInt(w_ti.W), UInt(w_sti.W))))
        val taskCodeIn = Input(new TaskCode)
        val cmtCodeIn  = Input(new CommitCode)

        val txRsp = Decoupled(new RespFlit())
        val rxRsp = Flipped(Valid(new RespFlit()))
        val rxDat = Flipped(Valid(new DataFlit()))

        val cmTaskVec = Vec(nrTaskCM, Decoupled(new CMTask))

        val cmResp = Flipped(Valid(new CMResp))

        val replTask = Decoupled(new ReplTask)
        val replResp = Flipped(Valid(new HnTxnID))

        val reqDB    = Decoupled(new HnTxnID with HasDataVec with HasQoS)
        val dataTask = Decoupled(new DataTask)
        val dataResp = Flipped(Valid(new HnTxnID))

        val cleanPoS = Decoupled(new PosClean)

        val state = Output(new State)
    })
    HAssert.withEn(io.alloc.bits.dirBank === io.hnIdx.dirBank, io.alloc.valid)

    val taskReg   = Reg(new CommitTask)
    val taskNext  = WireInit(taskReg)
    val taskAlloc = Wire(new CommitTask)
    taskAlloc := io.alloc.bits

    val flagReg  = Reg(new Flag)
    val flagNext = WireInit(flagReg)

    val stateReg  = RegInit(new State().Lit(_.value -> FREE))
    val stateNext = WireInit(stateReg)
    val valid     = WireInit(!stateReg.isFree)
    io.state := stateReg

    val instReg  = RegInit(new TaskInst().Lit(_.valid -> false.B, _.getXCBResp -> false.B, _.xCBResp -> ChiResp.I))
    val instNext = WireInit(instReg)

    val cmTask = WireInit(0.U.asTypeOf(new CMTask))
    val alrGetReg = RegInit(0.U.asTypeOf(new Bundle {
        val compAck = Bool()
        val NCBWrD0 = Bool()
        val NCBWrD1 = Bool()
    }))
    val respErrReg = RegInit(0.U(2.W))

    val rxRspHit = io.rxRsp.valid & io.rxRsp.bits.TxnID === io.hnTxnID
    val rxDatHit = io.rxDat.valid & io.rxDat.bits.TxnID === io.hnTxnID

    val rspAckHit = rxRspHit & io.rxRsp.bits.Opcode === CompAck
    val datAckHit = rxDatHit & io.rxDat.bits.Opcode === NCBWrDataCompAck

    val NCBWrDataHit0 = rxDatHit & io.rxDat.bits.DataID === "b00".U & (io.rxDat.bits.Opcode === NonCopyBackWriteData | io.rxDat.bits.Opcode === NCBWrDataCompAck)
    val NCBWrDataHit1 = rxDatHit & io.rxDat.bits.DataID === "b10".U & (io.rxDat.bits.Opcode === NonCopyBackWriteData | io.rxDat.bits.Opcode === NCBWrDataCompAck)

    val CBWrDataHit0 = rxDatHit & io.rxDat.bits.DataID === "b00".U & io.rxDat.bits.Opcode === CopyBackWriteData
    val CBWrDataHit1 = rxDatHit & io.rxDat.bits.DataID === "b10".U & io.rxDat.bits.Opcode === CopyBackWriteData

    val compAckHit    = rspAckHit | datAckHit
    val XCBWrDataHit0 = NCBWrDataHit0 | CBWrDataHit0
    val XCBWrDataHit1 = NCBWrDataHit1 | CBWrDataHit1
    val XCBWrDataHit  = XCBWrDataHit0 | XCBWrDataHit1

    when(io.cleanPoS.fire) {
        alrGetReg := 0.U.asTypeOf(alrGetReg)

    }.otherwise {
        alrGetReg.compAck := compAckHit | alrGetReg.compAck
        alrGetReg.NCBWrD0 := NCBWrDataHit0 | alrGetReg.NCBWrD0
        alrGetReg.NCBWrD1 := NCBWrDataHit1 | alrGetReg.NCBWrD1
        HAssert.withEn(!compAckHit, alrGetReg.compAck)
        HAssert.withEn(!XCBWrDataHit0, alrGetReg.NCBWrD0)
        HAssert.withEn(!XCBWrDataHit1, alrGetReg.NCBWrD1)
        HAssert.withEn(!CBWrDataHit0, stateReg.isFree)
        HAssert.withEn(!CBWrDataHit1, stateReg.isFree)
    }

    val cmRespHit = valid & io.cmResp.fire & io.cmResp.bits.hnTxnID === io.hnTxnID
    when(io.cleanPoS.fire) {
        respErrReg := RespErr.NormalOkay
    }.elsewhen(!(respErrReg === RespErr.DataError | respErrReg === RespErr.NonDataError)) {
        when(cmRespHit & io.cmResp.bits.isERR) {
            respErrReg := io.cmResp.bits.respErr
        }.elsewhen(XCBWrDataHit) {
            respErrReg := io.rxDat.bits.RespErr
        }
    }

    io.reqDB.valid        := valid & flagReg.intl.s.reqDB
    io.reqDB.bits.hnTxnID := io.hnTxnID
    io.reqDB.bits.dataVec := Mux(flagReg.intl.s.cmTask & taskReg.task.snoop, DataVec.Full, taskReg.chi.dataVec)
    io.reqDB.bits.qos     := taskReg.qos

    io.dataTask.valid := valid & flagReg.intl.s.dataTask & taskReg.alr.reqDB
    io.dataTask.bits  := DontCare

    val replLLC = flagReg.intl.s.wriDir & taskReg.cmt.wriLLC & !taskReg.dir.llc.hit
    io.dataTask.bits.dataOp      := taskReg.cmt.dataOp
    io.dataTask.bits.dataOp.save := taskReg.cmt.dataOp.save & !replLLC
    HAssert.withEn(PopCount(io.dataTask.bits.dataOp.asUInt) =/= 0.U, io.dataTask.valid)

    io.dataTask.bits.txDat.DBID    := io.hnTxnID
    io.dataTask.bits.txDat.Resp    := taskReg.cmt.resp
    io.dataTask.bits.txDat.Opcode  := taskReg.cmt.opcode
    io.dataTask.bits.txDat.HomeNID := DontCare
    io.dataTask.bits.txDat.TxnID   := taskReg.chi.txnID
    io.dataTask.bits.txDat.SrcID   := taskReg.chi.getNoC
    io.dataTask.bits.txDat.TgtID   := taskReg.chi.nodeId
    io.dataTask.bits.txDat.RespErr := respErrReg

    io.dataTask.bits.ds      := taskReg.ds
    io.dataTask.bits.hnTxnID := io.hnTxnID
    io.dataTask.bits.dataVec := Mux(taskReg.cmt.fullSize, DataVec.Full, taskReg.chi.dataVec)
    io.dataTask.bits.qos     := taskReg.qos

    io.replTask.valid        := valid & flagReg.intl.s.wriDir & !flagReg.intl.w.dataResp
    io.replTask.bits.hnTxnID := io.hnTxnID
    io.replTask.bits.qos     := taskReg.qos

    io.replTask.bits.wriSF        := taskReg.cmt.isWriSF
    io.replTask.bits.dir.sf.hit   := taskReg.dir.sf.hit
    io.replTask.bits.dir.sf.wayOH := taskReg.dir.sf.wayOH
    io.replTask.bits.dir.sf.metaVec.map(_.state).zipWithIndex.foreach { case (s, i) =>
        val metaIdOH = taskReg.chi.metaIdOH
        val srcVec   = VecInit(metaIdOH.asBools)
        val snpVec   = taskReg.dir.getSnpVec(taskReg.task.snpTgt, metaIdOH)
        s := PriorityMux(
            Seq(
                (srcVec(i) & taskReg.cmt.wriSRC) -> taskReg.cmt.srcValid,
                (snpVec(i) & taskReg.cmt.wriSNP) -> taskReg.cmt.snpValid,
                true.B                           -> (taskReg.dir.sf.hit & taskReg.dir.sf.metaVec(i).state)
            )
        )
    }

    io.replTask.bits.wriLLC                     := taskReg.cmt.wriLLC
    io.replTask.bits.dir.llc.hit                := taskReg.dir.llc.hit
    io.replTask.bits.dir.llc.wayOH              := taskReg.dir.llc.wayOH
    io.replTask.bits.dir.llc.metaVec.head.state := taskReg.cmt.llcState

    cmTask.chi            := taskReg.chi
    cmTask.chi.channel    := Mux(taskReg.task.snoop, ChiChannel.SNP, ChiChannel.REQ)
    cmTask.chi.dataVec    := Mux(taskReg.task.snoop | taskReg.task.fullSize, DataVec.Full, taskReg.chi.dataVec)
    cmTask.chi.opcode     := taskReg.task.opcode
    cmTask.chi.expCompAck := taskReg.task.expCompAck
    cmTask.chi.retToSrc   := taskReg.task.retToSrc
    cmTask.chi.size       := Mux(taskReg.task.snoop | taskReg.task.fullSize, "b110".U, taskReg.chi.size)

    cmTask.hnTxnID  := io.hnTxnID
    cmTask.dataOp   := taskReg.task.dataOp
    cmTask.ds       := taskReg.ds
    cmTask.snpVec   := taskReg.dir.getSnpVec(taskReg.task.snpTgt, taskReg.chi.metaIdOH)
    cmTask.fromRepl := false.B
    cmTask.cbResp   := taskReg.dir.llc.metaVec.head.cbResp
    cmTask.doDMT    := taskReg.task.doDMT
    cmTask.qos      := taskReg.qos

    io.cmTaskVec(CMID.SNP).valid  := valid & flagReg.intl.s.cmTask & !flagReg.intl.s.reqDB & taskReg.task.snoop
    io.cmTaskVec(CMID.READ).valid := valid & flagReg.intl.s.cmTask & !flagReg.intl.s.reqDB & taskReg.task.read
    io.cmTaskVec(CMID.WRI).valid  := valid & flagReg.intl.s.cmTask & !flagReg.intl.s.reqDB & taskReg.task.write
    io.cmTaskVec.foreach(_.bits := cmTask)
    io.cmTaskVec.foreach(_.bits.qos := taskReg.qos)
    HAssert(PopCount(io.cmTaskVec.map(_.fire)) <= 1.U)

    io.txRsp.valid := valid & flagReg.chi.s.asUInt.orR & !flagReg.intl.s.reqDB

    io.txRsp.bits          := DontCare
    io.txRsp.bits.SrcID    := taskReg.chi.getNoC
    io.txRsp.bits.TgtID    := taskReg.chi.nodeId
    io.txRsp.bits.TxnID    := taskReg.chi.txnID
    io.txRsp.bits.DBID     := io.hnTxnID
    io.txRsp.bits.RespErr  := RespErr.NormalOkay
    io.txRsp.bits.Opcode   := Mux(flagReg.chi.s.resp, taskReg.cmt.opcode, Mux(taskReg.chi.isCopyBackWrite, CompDBIDResp, DBIDResp))
    io.txRsp.bits.FwdState := taskReg.cmt.fwdResp
    io.txRsp.bits.Resp     := taskReg.cmt.resp
    io.txRsp.bits.QoS      := taskReg.qos
    io.txRsp.bits.RespErr  := respErrReg
    HAssert.withEn(PopCount(flagReg.chi.s.asUInt) <= 1.U, valid)

    io.cleanPoS.valid := valid & stateReg.isClean

    io.cleanPoS.bits.hnIdx   := io.hnIdx
    io.cleanPoS.bits.channel := taskReg.chi.channel
    io.cleanPoS.bits.qos     := taskReg.qos

    val decValid = flagReg.intl.s.decode & !(flagReg.intl.w.cmResp | flagReg.chi.w.xCBWrData0 | flagReg.chi.w.xCBWrData1)

    io.trdDecOut.valid         := decValid & stateReg.isFstTask
    io.trdDecOut.bits.taskInst := instReg
    io.trdDecOut.bits.decList  := taskReg.decList
    io.trdDecOut.bits.hnTxnID  := io.hnTxnID

    io.fthDecOut.valid         := decValid & stateReg.isSecTask
    io.fthDecOut.bits.taskInst := instReg
    io.fthDecOut.bits.decList  := taskReg.decList
    io.fthDecOut.bits.hnTxnID  := io.hnTxnID
    HAssert(!(io.trdDecOut.fire & io.fthDecOut.fire))

    when(io.decListIn.valid) {
        taskNext.decList     := io.decListIn.bits
        taskNext.task        := io.taskCodeIn
        taskNext.task.snpTgt := taskReg.task.snpTgt
        taskNext.cmt         := Mux(stateReg.isFstTask & io.cmtCodeIn.waitSecDone, 0.U.asTypeOf(new CommitCode), io.cmtCodeIn)

        HAssert(instReg.valid)
        HAssert(stateReg.isFstTask | stateReg.isSecTask)

        HAssert(taskReg.decList(0) === io.decListIn.bits(0))
        HAssert(taskReg.decList(1) === io.decListIn.bits(1))
        HAssert.withEn(taskReg.decList(2) === io.decListIn.bits(2), !stateReg.isFstTask)
        HAssert.withEn(taskReg.decList(3) === io.decListIn.bits(3), !stateReg.isSecTask)

        HAssert(taskReg.task.isValid)
        HAssert(!taskReg.cmt.isValid)
        HAssert.withEn(!io.taskCodeIn.snoop, stateReg.isSecTask)
        HAssert.withEn(!io.taskCodeIn.returnDBID, stateReg.isSecTask)
    }

    val allocHit   = io.alloc.fire & io.alloc.bits.hnTxnID === io.hnTxnID
    val cmTaskHit  = Cat(io.cmTaskVec.map(_.fire)).orR
    val decodeFire = io.trdDecOut.fire | io.fthDecOut.fire

    when(allocHit | io.decListIn.valid) {
        val alloc            = io.alloc.bits
        val task             = Mux(io.decListIn.valid, taskNext.task, alloc.task)
        val cmt              = Mux(io.decListIn.valid, taskNext.cmt, alloc.cmt)
        val alrReqDB         = Mux(io.decListIn.valid, taskReg.alr.reqDB, alloc.alr.reqDB)
        val alrSendData      = Mux(io.decListIn.valid, taskReg.alr.sData, alloc.alr.sData)
        val needWaitAck      = Mux(io.decListIn.valid, flagReg.chi.w.compAck, alloc.chi.expCompAck)
        val needWaitData     = allocHit & (alloc.task.returnDBID | alloc.alr.sDBID)
        val copyBackNeedData = alloc.chi.isCopyBackWrite & needWaitData
        val replLLC          = cmt.wriLLC & !taskReg.dir.llc.hit

        flagNext.intl.s.decode   := stateNext.isFstTask | stateNext.isSecTask
        flagNext.intl.s.reqDB    := (task.needDB | cmt.dataOp.isValid) & !alrReqDB
        flagNext.intl.s.cmTask   := task.opsIsValid
        flagNext.intl.s.dataTask := Mux(cmt.dataOp.onlySave, !replLLC, cmt.dataOp.isValid) & !alrSendData
        flagNext.intl.s.wriDir   := cmt.isWriDir

        flagNext.intl.w.cmResp   := flagNext.intl.s.cmTask
        flagNext.intl.w.replResp := flagNext.intl.s.wriDir
        flagNext.intl.w.dataResp := flagNext.intl.s.dataTask | alrSendData

        flagNext.chi.s.dbid := allocHit & alloc.task.returnDBID & !alloc.alr.sDBID
        flagNext.chi.s.resp := cmt.sendResp & cmt.channel === ChiChannel.RSP

        flagNext.chi.w.xCBWrData0 := needWaitData & alloc.chi.dataVec(0) & !XCBWrDataHit0 & !alrGetReg.NCBWrD0
        flagNext.chi.w.xCBWrData1 := needWaitData & alloc.chi.dataVec(1) & !XCBWrDataHit1 & !alrGetReg.NCBWrD1
        flagNext.chi.w.compAck    := needWaitAck & !copyBackNeedData & !compAckHit & !alrGetReg.compAck

        HAssert(allocHit ^ io.decListIn.valid)
        HAssert.withEn(!(task.isValid & cmt.isValid), allocHit)
        HAssert.withEn(flagReg.intl.asUInt === 0.U & PopCount(flagReg.chi.asUInt) === flagReg.chi.w.compAck.asUInt, io.decListIn.valid)
        HAssert.withEn(stateNext.isFstTask, flagNext.chi.s.dbid)
        HAssert.withEn(stateNext.isFstTask, flagNext.chi.w.xCBWrData0)
        HAssert.withEn(stateNext.isFstTask, flagNext.chi.w.xCBWrData1)
        HAssert.withEn(!alloc.chi.isZero, needWaitData)

    }.otherwise {

        when(decodeFire) { flagNext.intl.s.decode := false.B; HAssert(flagReg.intl.s.decode) }
        when(io.reqDB.fire) { flagNext.intl.s.reqDB := false.B; HAssert(flagReg.intl.s.reqDB) }
        when(cmTaskHit) { flagNext.intl.s.cmTask := false.B; HAssert(flagReg.intl.s.cmTask & flagReg.intl.w.cmResp) }
        when(io.dataTask.fire) { flagNext.intl.s.dataTask := false.B; HAssert(flagReg.intl.s.dataTask & flagReg.intl.w.dataResp) }
        when(io.replTask.fire) { flagNext.intl.s.wriDir := false.B; HAssert(flagReg.intl.s.wriDir & flagReg.intl.w.replResp) }

        when(cmRespHit) { flagNext.intl.w.cmResp := false.B; HAssert.withEn(flagReg.intl.w.cmResp, valid) }
        when(io.replResp.fire & io.replResp.bits.hnTxnID === io.hnTxnID) { flagNext.intl.w.replResp := false.B; HAssert(flagReg.intl.w.replResp & valid) }
        when(io.dataResp.fire & io.dataResp.bits.hnTxnID === io.hnTxnID) { flagNext.intl.w.dataResp := false.B; HAssert.withEn(flagReg.intl.w.dataResp | flagReg.intl.w.replResp | flagReg.intl.w.cmResp, valid) }

        when(io.txRsp.fire) { flagNext.chi.s.dbid := false.B; HAssert.withEn(flagReg.chi.s.dbid, !stateReg.isCommit) }
        when(io.txRsp.fire) { flagNext.chi.s.resp := false.B; HAssert.withEn(flagReg.chi.s.resp, stateReg.isCommit) }

        when(compAckHit) { flagNext.chi.w.compAck := false.B; HAssert.withEn(taskReg.chi.expCompAck, valid) }
        when(XCBWrDataHit0) { flagNext.chi.w.xCBWrData0 := false.B; HAssert.withEn(flagReg.chi.w.xCBWrData0, valid) }
        when(XCBWrDataHit1) { flagNext.chi.w.xCBWrData1 := false.B; HAssert.withEn(flagReg.chi.w.xCBWrData1, valid) }
    }

    val allFlagDone = flagReg.intl.asUInt === 0.U & flagReg.chi.asUInt === 0.U
    switch(stateReg.value) {
        is(FREE) {
            when(allocHit) { stateNext.value := Mux(io.alloc.bits.task.isValid, FSTTASK, COMMIT) }
        }
        is(FSTTASK) {
            when(io.decListIn.valid) { stateNext.value := Mux(io.taskCodeIn.isValid & io.cmtCodeIn.waitSecDone, SECTASK, COMMIT) }
        }
        is(SECTASK) {
            when(io.decListIn.valid) { stateNext.value := COMMIT }
        }
        is(COMMIT) {
            when(allFlagDone) { stateNext.value := CLEAN }
        }
        is(CLEAN) {
            when(io.cleanPoS.fire) { stateNext.value := FREE }
        }
    }
    HAssert.withEn(stateReg.isFree, allocHit)
    HAssert.withEn(stateReg.isFstTask | stateReg.isSecTask, io.decListIn.valid)
    HAssert.withEn(stateReg.isClean, io.cleanPoS.valid)

    val cleanInst = (stateReg.isFstTask & stateNext.isSecTask) | stateNext.isCommit

    when(allocHit | cleanInst) {
        instNext := 0.U.asTypeOf(new TaskInst)
    }.elsewhen(cmRespHit) {
        instNext := PriorityMux(
            Seq(
                stateReg.isFstTask -> (instReg.asUInt | io.cmResp.bits.taskInst.asUInt).asTypeOf(new TaskInst),
                stateReg.isSecTask -> io.cmResp.bits.taskInst
            )
        )
        HAssert(stateReg.isFstTask | stateReg.isSecTask | stateReg.isCommit)
    }

    when(cleanInst) {
        instNext.getXCBResp := false.B
        instNext.xCBResp    := ChiResp.I
    }.elsewhen(XCBWrDataHit) {
        instNext.getXCBResp := true.B
        instNext.xCBResp    := io.rxDat.bits.Resp
        HAssert(stateReg.isFree | stateReg.isFstTask)
        HAssert.withEn(instNext.xCBResp === instReg.xCBResp, instReg.getXCBResp)
    }.otherwise {
        instNext.getXCBResp := instReg.getXCBResp
        instNext.xCBResp    := instReg.xCBResp
    }

    when(cleanInst) {
        instNext.valid := false.B
    }.elsewhen(cmRespHit | XCBWrDataHit) {
        instNext.valid := true.B
    }.otherwise {
        instNext.valid := instReg.valid
    }

    when(io.reqDB.fire) {
        taskNext.alr.reqDB := true.B
        HAssert(!taskReg.alr.reqDB)
    }

    val set = allocHit | valid | XCBWrDataHit; dontTouch(set)
    when(set) {
        taskReg  := Mux(allocHit, taskAlloc, taskNext)
        flagReg  := flagNext
        instReg  := instNext
        stateReg := stateNext
    }

    HAssert.checkTimeout(
        !valid,
        TIMEOUT_COMMIT,
        cf"\n\nTIMEOUT[${io.hnTxnID}]: " +
            cf"\nInternal Send: ${flagReg.intl.s}\nInternal Wait: ${flagReg.intl.w}" +
            cf"\nCHI Send: ${flagReg.chi.s}\nCHI Wait: ${flagReg.chi.w}" +
            cf"\nState: ${stateReg.value}\n${taskReg.chi.getChiInst}\n${taskReg.dir.getStateInst(taskReg.chi.metaIdOH)}\n\n"
    )

    XSPerfAccumulate(Seq(("zj_hn_stash_refill", io.replTask.fire && taskReg.chi.reqIs(StashOnceShared))))
}

class Commit(implicit p: Parameters) extends DJModule {

    val io = IO(new DJBundle {
        val config = new DJConfigIO

        val cmtTaskVec = Vec(djparam.nrDirBank, Flipped(Valid(new CommitTask with HasHnTxnID)))

        val txRsp = Decoupled(new RespFlit())
        val rxRsp = Flipped(Valid(new RespFlit()))
        val rxDat = Flipped(Valid(new DataFlit()))

        val cmTaskVec = Vec(nrTaskCM, Decoupled(new CMTask))

        val cmResp = Flipped(Valid(new CMResp))

        val reqDB    = Decoupled(new HnTxnID with HasDataVec)
        val replTask = Decoupled(new ReplTask)
        val replResp = Flipped(Valid(new HnTxnID))

        val dataTask = Decoupled(new DataTask)
        val dataResp = Flipped(Valid(new HnTxnID))

        val cleanPoS = Decoupled(new PosClean)
    })

    val entries = Seq.fill(djparam.nrCommit) { Module(new CommitEntry()) }
    val trdDec  = Module(new Decode("Third"))
    val fthDec  = Module(new Decode("Fourth"))

    entries.grouped(nrCommit).zipWithIndex.foreach { case (e0, i) =>
        e0.grouped(posWays - 2).zipWithIndex.foreach { case (e1, j) =>
            e1.zipWithIndex.foreach { case (e2, k) =>
                val id = i * nrPoS + j * posWays + k
                e2.io.hnTxnID       := id.U
                e2.io.hnIdx.dirBank := i.U
                e2.io.hnIdx.pos.set := j.U
                e2.io.hnIdx.pos.way := k.U
                e2.suggestName(s"entries_${id}_bank${i}_set${j}_way${k}")
            }
        }
    }

    entries.map(_.io.alloc).grouped(nrCommit).zip(io.cmtTaskVec).foreach { case (alloc, task) => alloc.foreach(_ := task) }

    trdDec.io.decMesIn := fastRRArb.validOut(entries.map(_.io.trdDecOut))
    fthDec.io.decMesIn := fastRRArb.validOut(entries.map(_.io.fthDecOut))

    entries.foreach { case e =>
        val trdHit = trdDec.io.hnTxnIdOut.valid & trdDec.io.hnTxnIdOut.bits === e.io.hnTxnID
        val fthHit = fthDec.io.hnTxnIdOut.valid & fthDec.io.hnTxnIdOut.bits === e.io.hnTxnID
        e.io.decListIn.valid := trdHit | fthHit
        e.io.decListIn.bits  := Mux(trdHit, trdDec.io.decListOut, fthDec.io.decListOut)
        e.io.taskCodeIn      := Mux(trdHit, trdDec.io.taskCodeOut, fthDec.io.taskCodeOut)
        e.io.cmtCodeIn       := Mux(trdHit, trdDec.io.cmtCodeOut, fthDec.io.cmtCodeOut)
        HAssert(!(trdHit & fthHit))
    }

    entries.foreach { case e =>
        e.io.config   := io.config
        e.io.rxRsp    := io.rxRsp
        e.io.rxDat    := io.rxDat
        e.io.cmResp   := io.cmResp
        e.io.replResp := io.replResp
        e.io.dataResp := io.dataResp
    }

    fastQosRRArb(entries.map(_.io.reqDB), io.reqDB)
    io.txRsp    <> fastQosRRArb(entries.map(_.io.txRsp))
    io.replTask <> fastQosRRArb(entries.map(_.io.replTask))
    io.dataTask <> fastQosRRArb(entries.map(_.io.dataTask))
    io.cleanPoS <> fastQosRRArb(entries.map(_.io.cleanPoS))
    io.cmTaskVec.zip(entries.map(_.io.cmTaskVec).transpose).foreach { case (a, b) => a <> fastQosRRArb(b) }

    HAssert.placePipe(1)
}
