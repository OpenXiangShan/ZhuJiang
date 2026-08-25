package dongjiang.data

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import zhujiang.chi._
import dongjiang._
import dongjiang.backend.UpdHnTxnID
import dongjiang.utils._
import dongjiang.bundle._
import xs.utils.debug._
import zhujiang.perf.HomeWrapperPerf
import xs.utils.queue.FastQueue
import dongjiang.data.CTRLSTATE._
import chisel3.experimental.BundleLiterals._
import utility.GTimer

object CTRLSTATE {
    val width = 3
    val FREE  = 0x0.U
    val ALLOC = 0x1.U
    val REPL  = 0x2.U
    val READ  = 0x3.U
    val SEND  = 0x4.U
    val SAVE  = 0x5.U
    val RESP  = 0x6.U
    val CLEAN = 0x7.U
}

trait HasCtrlMes { this: DJBundle =>

    val state    = UInt(CTRLSTATE.width.W)
    val critical = Bool()

    val sReadVec = UInt(djparam.nrBeat.W)
    val sSendVec = UInt(djparam.nrBeat.W)
    val sSaveVec = UInt(djparam.nrBeat.W)

    val wReadVec = UInt(djparam.nrBeat.W)
    val wSendVec = UInt(djparam.nrBeat.W)
    val wSaveVec = UInt(djparam.nrBeat.W)

    def readBeat = PriorityEncoder(sReadVec)
    def saveBeat = PriorityEncoder(sSaveVec)
    def sendBeat = PriorityEncoder(sSendVec)

    def isReadAll = wReadVec.asUInt === 0.U
    def isSendAll = wSendVec.asUInt === 0.U
    def isSaveAll = wSaveVec.asUInt === 0.U

    def isFree = state === FREE
    def isValid = !isFree
    def isAlloc = state === ALLOC
    def isRepl = state === REPL & sReadVec.asUInt =/= 0.U & sSaveVec.asUInt =/= 0.U
    def isRead = state === READ & sReadVec.asUInt =/= 0.U
    def isSend = state === SEND & sSendVec.asUInt =/= 0.U
    def isSave = state === SAVE & sSaveVec.asUInt =/= 0.U
    def isResp = state === RESP
    def isClean = state === CLEAN
}

class DataCtrlEntry(implicit p: Parameters) extends DJModule {

    val io = IO(new Bundle {
        val dcid = Input(UInt(dcIdBits.W))

        val updHnTxnID = Flipped(Valid(new UpdHnTxnID))
        val alloc      = Flipped(Decoupled(new HnTxnID with HasDataVec with HasDBIDVec))
        val task       = Flipped(Valid(new DataTask))
        val resp       = Decoupled(new HnTxnID)
        val clean      = Flipped(Valid(new HnTxnID with HasDataVec))

        val readForRepl = Output(Bool())
        val readToDB    = Decoupled(new ReadDS)
        val readToDS    = Decoupled(new ReadDB)
        val readToCHI   = Decoupled(new ReadDB)

        val release           = Decoupled(new DBIDVec with HasDataVec)
        val dsWriDB           = Flipped(Valid(new DCID with HasBeatNum))
        val txDatFire         = Flipped(Valid(new DCID with HasBeatNum))
        val dbWriDS           = Flipped(Valid(new DCID with HasBeatNum))
        val dsWriteCommit     = Output(Bool())
        val dsSaveWriteCommit = Output(Bool())
        val dsReplWriteCommit = Output(Bool())
        val txDatBits         = Output(new DataFlit)
        val localHitPerf      = Output(new LocalHitLatencyPerf)
        val llcReturnPerf     = Output(new LlcReturnPerf)

        val state = Valid(new HnTxnID with HasDataVec with HasDBIDVec)
    })

    val reg  = RegInit((new PackDataTask with HasCtrlMes with HasDataVec with HasDBIDVec).Lit(_.state -> FREE))
    val next = WireInit(reg)
    val perfTimer = GTimer()
    val perfLocalHitReg = RegInit(false.B)
    val perfDemandReadReg = RegInit(false.B)
    val perfLlcHitReg = RegInit(false.B)
    val perfDsReqSeenReg = RegInit(false.B)
    val perfDsRespSeenReg = RegInit(false.B)
    val perfTxDatSeenReg = RegInit(false.B)
    val perfDemandTxDatSeenReg = RegInit(false.B)
    val perfIngressCycleReg = Reg(UInt(64.W))
    val perfDecodeCycleReg = Reg(UInt(64.W))
    val perfDataTaskCycleReg = Reg(UInt(64.W))
    val perfDsReqCycleReg = Reg(UInt(64.W))
    val perfDsRespCycleReg = Reg(UInt(64.W))
    require(djparam.nrBeat == 2)
    HAssert.withEn(reg.dbidVec(0) =/= reg.dbidVec(1), reg.isValid & reg.isFullSize)

    io.txDatBits          := reg.task.txDat
    io.state.valid        := reg.isValid
    io.state.bits.hnTxnID := reg.task.hnTxnID
    io.state.bits.dataVec := reg.dataVec
    io.state.bits.dbidVec := reg.dbidVec

    io.alloc.ready := reg.isFree

    io.readForRepl := reg.isRepl

    io.readToDB.valid         := reg.isRepl | reg.isRead
    io.readToDB.bits.ds       := reg.task.ds
    io.readToDB.bits.dcid     := io.dcid
    io.readToDB.bits.dbid     := reg.dbidVec(reg.readBeat)
    io.readToDB.bits.beatNum  := reg.readBeat
    io.readToDB.bits.critical := reg.critical
    io.readToDB.bits.qos      := reg.task.qos
    io.readToDB.bits.toCHI    := (reg.task.dataOp.repl | reg.task.dataOp.send) & !reg.task.dataOp.merge
    HAssert.withEn(!reg.isReadAll, io.readToDB.valid)

    io.readToCHI.valid         := reg.isSend
    io.readToCHI.bits.ds       := DontCare
    io.readToCHI.bits.dcid     := io.dcid
    io.readToCHI.bits.dbid     := reg.dbidVec(reg.sendBeat)
    io.readToCHI.bits.beatNum  := reg.sendBeat
    io.readToCHI.bits.critical := reg.critical
    io.readToCHI.bits.repl     := false.B
    io.readToCHI.bits.qos      := reg.task.qos
    HAssert.withEn(!reg.isSendAll, io.readToCHI.valid)

    io.readToDS.valid         := reg.isRepl | reg.isSave
    io.readToDS.bits.ds       := reg.task.ds
    io.readToDS.bits.dcid     := io.dcid
    io.readToDS.bits.dbid     := reg.dbidVec(reg.saveBeat)
    io.readToDS.bits.beatNum  := reg.saveBeat
    io.readToDS.bits.critical := reg.critical
    io.readToDS.bits.repl     := reg.isRepl
    io.readToDS.bits.qos      := reg.task.qos
    HAssert.withEn(!reg.isSaveAll, io.readToDS.valid)

    HAssert.withEn(!(io.readToDB.valid ^ io.readToDS.valid), reg.isRepl)
    HAssert.withEn(!(io.readToDB.fire ^ io.readToDS.fire), reg.isRepl)
    HAssert.withEn(!(reg.sReadVec ^ reg.sSaveVec).orR, reg.isRepl)

    io.release.valid        := reg.isClean
    io.release.bits.dataVec := (reg.dataVec.asUInt & reg.task.dataVec.asUInt).asBools
    io.release.bits.dbidVec := reg.dbidVec
    HAssert.withEn(!io.release.bits.isZero, io.release.valid)

    io.resp.valid        := reg.isResp
    io.resp.bits.hnTxnID := reg.task.hnTxnID
    HAssert.withEn(reg.isReadAll & reg.isSendAll & reg.isSaveAll, io.resp.valid)

    val taskHit = reg.isValid & io.task.valid & io.task.bits.hnTxnID === reg.task.hnTxnID
    val localHitTask = taskHit && io.task.bits.perf.valid
    when(taskHit) {
        next.task := io.task.bits
        HAssert(reg.isAlloc)
        HAssert(io.task.bits.dataOp.isValid)
        reg.dataVec.zip(io.task.bits.dataVec).map { case (v, t) => HAssert.withEn(v, t) }
    }

    val cleanHit = reg.isValid & io.clean.valid & io.clean.bits.hnTxnID === reg.task.hnTxnID
    when(io.alloc.fire) {
        next.dataVec := io.alloc.bits.dataVec
        next.dbidVec := io.alloc.bits.dbidVec
    }.elsewhen(cleanHit) {
        next.task.dataVec := io.clean.bits.dataVec
        HAssert(!io.clean.bits.isZero)
        HAssert(reg.isAlloc)
        HAssert(!taskHit)
    }.elsewhen(io.release.fire) {
        next.dataVec := VecInit((reg.dataVec.asUInt & ~io.release.bits.dataVec.asUInt).asBools)
    }

    def setNextXXV(name: String, nsxv: UInt, nwxv: UInt, sxv: UInt, wxv: UInt, set: Bool, sFire: Bool, sBeat: UInt, wFire: Bool, wBeat: UInt) = {

        when(io.alloc.fire) {
            nsxv := 0.U
            nwxv := 0.U

        }.elsewhen(taskHit & set) {
            nsxv := io.task.bits.dataVec.asUInt
            nwxv := io.task.bits.dataVec.asUInt
            HAssert(sxv === 0.U, name)
            HAssert(wxv === 0.U, name)

        }.otherwise {

            nsxv := PriorityMux(
                Seq(
                    (sFire & wFire) -> (sxv & ~(UIntToOH(sBeat) | UIntToOH(wBeat))),
                    sFire           -> (sxv & ~UIntToOH(sBeat)),
                    wFire           -> (sxv & ~UIntToOH(wBeat)),
                    true.B          -> sxv
                )
            )
            HAssert.withEn(sxv(sBeat), sFire, name)
            HAssert.withEn(sBeat =/= wBeat, sFire & wFire, name)

            when(wFire) {
                nwxv := wxv & ~UIntToOH(wBeat)
                HAssert(wxv(wBeat), name)
            }
        }
    }

    val dsWriDBHit = reg.isValid & io.dsWriDB.valid & io.dsWriDB.bits.dcid === io.dcid
    setNextXXV(
        "read",
        next.sReadVec,
        next.wReadVec,
        reg.sReadVec,
        reg.wReadVec,
        io.task.bits.dataOp.readToDB,
        io.readToDB.fire,
        io.readToDB.bits.beatNum,
        dsWriDBHit,
        io.dsWriDB.bits.beatNum
    )

    val txDatHit = reg.isValid & io.txDatFire.valid & io.txDatFire.bits.dcid === io.dcid
    setNextXXV(
        "send",
        next.sSendVec,
        next.wSendVec,
        reg.sSendVec,
        reg.wSendVec,
        io.task.bits.dataOp.readToCHI,
        io.readToCHI.fire,
        io.readToCHI.bits.beatNum,
        txDatHit,
        io.txDatFire.bits.beatNum
    )

    val dbWriDSHit = reg.isValid & io.dbWriDS.valid & io.dbWriDS.bits.dcid === io.dcid
    io.dsWriteCommit     := dbWriDSHit
    io.dsSaveWriteCommit := dbWriDSHit && reg.isSave
    io.dsReplWriteCommit := dbWriDSHit && reg.isRepl
    setNextXXV(
        "save",
        next.sSaveVec,
        next.wSaveVec,
        reg.sSaveVec,
        reg.wSaveVec,
        io.task.bits.dataOp.readToDS,
        io.readToDS.fire,
        io.readToDS.bits.beatNum,
        dbWriDSHit,
        io.dbWriDS.bits.beatNum
    )

    when(io.alloc.fire | taskHit) {
        next.critical := false.B
        HAssert.withEn(!reg.critical, taskHit)
    }.otherwise {
        next.critical := PriorityMux(
            Seq(
                io.readToDB.fire  -> (PopCount(reg.sReadVec) > 1.U),
                io.readToCHI.fire -> (PopCount(reg.sSendVec) > 1.U),
                io.readToDS.fire  -> (PopCount(reg.sSaveVec) > 1.U),
                true.B            -> reg.critical
            )
        )
    }

    val firstLocalHitDsReq = perfLocalHitReg && !perfDsReqSeenReg && io.readToDB.fire
    val firstLocalHitDsResp = perfLocalHitReg && perfDsReqSeenReg && !perfDsRespSeenReg && dsWriDBHit
    val firstLocalHitTxDat = perfLocalHitReg && !perfTxDatSeenReg && txDatHit
    val firstDemandReturnTxDat = perfDemandReadReg && !perfDemandTxDatSeenReg && txDatHit

    when(io.alloc.fire) {
        perfLocalHitReg  := false.B
        perfDemandReadReg := false.B
        perfLlcHitReg := false.B
        perfDsReqSeenReg := false.B
        perfDsRespSeenReg := false.B
        perfTxDatSeenReg := false.B
        perfDemandTxDatSeenReg := false.B
    }
    when(localHitTask) {
        perfLocalHitReg      := true.B
        perfIngressCycleReg  := io.task.bits.perf.ingressCycle
        perfDecodeCycleReg   := io.task.bits.perf.decodeCycle
        perfDataTaskCycleReg := perfTimer
    }
    when(taskHit && io.task.bits.perf.demandRead) {
        perfDemandReadReg := true.B
        perfLlcHitReg := io.task.bits.perf.llcHit
        perfIngressCycleReg := io.task.bits.perf.ingressCycle
    }
    when(firstLocalHitDsReq) {
        perfDsReqSeenReg  := true.B
        perfDsReqCycleReg := perfTimer
    }
    when(firstLocalHitDsResp) {
        perfDsRespSeenReg  := true.B
        perfDsRespCycleReg := perfTimer
    }
    when(firstLocalHitTxDat) {
        perfTxDatSeenReg := true.B
    }
    when(firstDemandReturnTxDat) {
        perfDemandTxDatSeenReg := true.B
    }

    io.localHitPerf.decodeToDataTask.valid := localHitTask
    io.localHitPerf.decodeToDataTask.bits  := perfTimer - io.task.bits.perf.decodeCycle
    io.localHitPerf.dataTaskToDsReq.valid  := firstLocalHitDsReq
    io.localHitPerf.dataTaskToDsReq.bits   := perfTimer - perfDataTaskCycleReg
    io.localHitPerf.dsReqToDsResp.valid    := firstLocalHitDsResp
    io.localHitPerf.dsReqToDsResp.bits     := perfTimer - perfDsReqCycleReg
    io.localHitPerf.dsRespToTxDat.valid    := firstLocalHitTxDat
    io.localHitPerf.dsRespToTxDat.bits     := perfTimer - Mux(firstLocalHitDsResp, perfTimer, perfDsRespCycleReg)
    io.localHitPerf.decodeToTxDat.valid    := firstLocalHitTxDat
    io.localHitPerf.decodeToTxDat.bits     := perfTimer - perfDecodeCycleReg
    io.localHitPerf.ingressToTxDat.valid   := firstLocalHitTxDat
    io.localHitPerf.ingressToTxDat.bits    := perfTimer - perfIngressCycleReg
    io.llcReturnPerf.valid   := firstDemandReturnTxDat
    io.llcReturnPerf.llcHit  := perfLlcHitReg
    io.llcReturnPerf.latency := perfTimer - perfIngressCycleReg

    val updHnTxnIDHit = reg.isValid & io.updHnTxnID.valid & io.updHnTxnID.bits.before === reg.task.hnTxnID
    when(io.alloc.fire) {
        next.task.hnTxnID := io.alloc.bits.hnTxnID
    }.elsewhen(updHnTxnIDHit) {
        next.task.hnTxnID := io.updHnTxnID.bits.next
        HAssert(reg.isAlloc)
    }

    switch(reg.state) {
        is(FREE) {
            when(io.alloc.fire) {
                next.state := ALLOC
            }
        }
        is(ALLOC) {
            when(taskHit) {
                next.state := PriorityMux(
                    Seq(
                        io.task.bits.dataOp.repl -> REPL,
                        io.task.bits.dataOp.read -> READ,
                        io.task.bits.dataOp.send -> SEND,
                        io.task.bits.dataOp.save -> SAVE
                    )
                )
            }.elsewhen(cleanHit) {
                next.state := CLEAN
            }
        }
        is(REPL) {
            when(next.isReadAll & next.isSaveAll) {
                next.state := Mux(next.isSendAll, RESP, SEND)
            }
        }
        is(READ) {
            when(next.isReadAll) {
                next.state := PriorityMux(
                    Seq(
                        (reg.task.dataOp.send & !next.isSendAll) -> SEND,
                        (reg.task.dataOp.save & !next.isSaveAll) -> SAVE,
                        true.B                                   -> RESP
                    )
                )
            }
        }
        is(SEND) {
            when(next.isSendAll) {
                next.state := Mux(reg.task.dataOp.save & !next.isSaveAll, SAVE, RESP)
            }
        }
        is(SAVE) {
            when(next.isSaveAll) {
                next.state := RESP
            }
        }
        is(RESP) {
            when(io.resp.fire) {
                next.state := ALLOC
            }
        }
        is(CLEAN) {
            when(io.release.fire) {
                next.state := Mux(next.isZero, FREE, ALLOC)
            }
        }
    }
    HAssert.withEn(reg.isResp | reg.isClean, next.state < reg.state)
    HAssert.withEn(next.isFree | next.isAlloc, next.state < reg.state)
    HAssert.withEn(reg.isFree | reg.isClean, next.isFree)
    HAssert.withEn(reg.isFree | reg.isAlloc | reg.isResp | reg.isClean, next.isAlloc)

    val set = io.alloc.fire | reg.isValid; dontTouch(set)
    when(set) { reg := next }

    HomeWrapperPerf.accumulate(
        Seq(
            ("zj_datactrl_alloc_fire", io.alloc.fire),
            ("zj_datactrl_task_hit", taskHit),
            ("zj_datactrl_clean_hit", cleanHit),
            ("zj_datactrl_valid_cycle", reg.isValid),
            ("zj_datactrl_wait_alloc_cycle", reg.isAlloc),
            ("zj_datactrl_wait_repl_cycle", reg.isRepl),
            ("zj_datactrl_wait_read_cycle", reg.isRead),
            ("zj_datactrl_wait_send_cycle", reg.isSend),
            ("zj_datactrl_wait_save_cycle", reg.isSave),
            ("zj_datactrl_wait_resp_cycle", reg.isResp),
            ("zj_datactrl_wait_clean_cycle", reg.isClean),
            ("zj_datactrl_resp_fire", io.resp.fire),
            ("zj_datactrl_release_fire", io.release.fire)
        )
    )

    HAssert.checkTimeout(reg.isFree | updHnTxnIDHit, TIMEOUT_DATACM, cf"TIMEOUT: DataCM State[${reg.state}]")
}

class DataCM(implicit p: Parameters) extends DJModule {

    val io = IO(new Bundle {

        val updHnTxnID = Flipped(Valid(new UpdHnTxnID))
        val reqDBIn    = Flipped(Decoupled(new HnTxnID with HasDataVec))
        val task       = Flipped(Valid(new DataTask))
        val resp       = Valid(new HnTxnID)
        val clean      = Flipped(Valid(new HnTxnID with HasDataVec))

        val readToDB  = Decoupled(new ReadDS)
        val readToDS  = Decoupled(new ReadDB)
        val readToCHI = Decoupled(new ReadDB)

        val reqDBOut = Decoupled(Vec(djparam.nrBeat, Bool()))
        val dbidResp = Vec(djparam.nrBeat, Flipped(UInt(dbIdBits.W)))

        val getChiDat = new DJBundle {
            val valid = Input(Bool())
            val dcid  = Input(UInt(dcIdBits.W))
            val bits  = Output(new DataFlit)
        }
        val getDBID = new DJBundle {
            val valid  = Input(Bool())
            val TxnID  = Input(UInt(ChiTxnIdBits.W))
            val DataID = Input(UInt(2.W))
            val dbid   = Output(UInt(dbIdBits.W))
        }

        val release   = Valid(new DBIDVec with HasDataVec)
        val dsWriDB   = Flipped(Valid(new DCID with HasBeatNum))
        val txDatFire = Flipped(Valid(new DCID with HasBeatNum))
        val dbWriDS   = Flipped(Valid(new DCID with HasBeatNum))
    })

    HAssert.withEn(!io.reqDBIn.bits.isZero, io.reqDBIn.valid)
    HAssert.withEn(!io.task.bits.isZero, io.task.valid)

    val entries     = Seq.fill(nrDataCM) { Module(new DataCtrlEntry()) }
    val taskFireReg = RegNext(io.task.fire)
    val taskReg     = RegEnable(io.task.bits, io.task.fire)
    val dbgVec      = VecInit(entries.map(_.io.state))
    dontTouch(dbgVec)
    dbgVec.zipWithIndex.foreach { case (src, i) =>
        dbgVec.zipWithIndex.foreach { case (sink, j) =>
            HAssert.withEn(!(src.bits.hnTxnID === sink.bits.hnTxnID), src.valid & sink.valid & i.U =/= j.U)
            HAssert.withEn(!(src.bits.dbidVec(0) === sink.bits.dbidVec(0) & src.bits.dataVec(0) & sink.bits.dataVec(0)), src.valid & sink.valid & i.U =/= j.U)
            HAssert.withEn(!(src.bits.dbidVec(1) === sink.bits.dbidVec(1) & src.bits.dataVec(1) & sink.bits.dataVec(1)), src.valid & sink.valid & i.U =/= j.U)
        }
    }

    val freeDCVec = VecInit(entries.map(_.io.alloc.ready))
    val hasFreeDC = freeDCVec.asUInt.orR
    val freeDCID  = WireInit(PriorityEncoder(freeDCVec)); dontTouch(freeDCID)

    io.reqDBOut.valid := io.reqDBIn.valid & hasFreeDC
    io.reqDBOut.bits  := io.reqDBIn.bits.dataVec

    io.reqDBIn.ready := io.reqDBOut.ready & hasFreeDC

    entries.zipWithIndex.foreach { case (e, i) =>
        e.io.dcid := i.U

        e.io.alloc.valid        := io.reqDBIn.fire & freeDCID === i.U
        e.io.alloc.bits.hnTxnID := io.reqDBIn.bits.hnTxnID
        e.io.alloc.bits.dataVec := io.reqDBIn.bits.dataVec
        e.io.alloc.bits.dbidVec := io.dbidResp
        HAssert.withEn(io.reqDBOut.fire, e.io.alloc.fire, cf"DCID[${i.U}]")

        e.io.task.valid := taskFireReg
        e.io.task.bits  := taskReg
    }
    HAssert.withEn(io.reqDBOut.fire & PopCount(entries.map(_.io.alloc.fire)) === 1.U, io.reqDBIn.fire)
    HAssert.withEn(PopCount(entries.map(e => e.io.state.valid & e.io.state.bits.hnTxnID === taskReg.hnTxnID)) === 1.U, taskFireReg)

    entries.foreach(_.io.updHnTxnID := io.updHnTxnID)
    entries.foreach(_.io.clean := io.clean)
    entries.foreach(_.io.dsWriDB := io.dsWriDB)
    entries.foreach(_.io.txDatFire := io.txDatFire)
    entries.foreach(_.io.dbWriDS := io.dbWriDS)
    HAssert.withEn(entries.map(e => e.io.state.valid & e.io.state.bits.hnTxnID === io.updHnTxnID.bits.before).reduce(_ | _), io.updHnTxnID.valid)

    io.resp           := fastRRArb.validOut(entries.map(_.io.resp))
    io.release        := fastRRArb.validOut(entries.map(_.io.release))
    io.getChiDat.bits := VecInit(entries.map(_.io.txDatBits))(io.getChiDat.dcid)
    HAssert.withEn(dbgVec(io.getChiDat.dcid).valid, io.getChiDat.valid)

    val getDBIDHitVec = VecInit(entries.map(e => e.io.state.valid & io.getDBID.TxnID === e.io.state.bits.hnTxnID))
    val getDCID       = PriorityEncoder(getDBIDHitVec)
    val getBeatNum    = Mux(io.getDBID.DataID === "b10".U, 1.U, 0.U)
    io.getDBID.dbid := VecInit(entries.map(_.io.state.bits.dbidVec))(getDCID)(getBeatNum)
    HAssert.withEn(PopCount(getDBIDHitVec) === 1.U, io.getDBID.valid)
    HAssert.withEn(VecInit(entries.map(_.io.state.bits.dataVec))(getDCID)(getBeatNum), io.getDBID.valid)

    def connectReadToX[T <: Bundle with HasCritical](inVec: Vec[DecoupledIO[T]], out: DecoupledIO[T]): Unit = {
        val criticalVec = VecInit(inVec.map(e => e.valid & e.bits.critical))
        val hasCritical = criticalVec.asUInt.orR
        val criticalId  = PriorityEncoder(criticalVec)
        when(hasCritical) {
            inVec.foreach(_.ready := false.B)
            out <> inVec(criticalId)
        }.otherwise {
            out <> fastQosRRArb(inVec)
        }
        HAssert(PopCount(criticalVec) <= 1.U)
    }

    val replVec     = VecInit(entries.map(e => e.io.readForRepl))
    val replCVec    = VecInit(entries.map(e => e.io.readForRepl & e.io.readToDS.bits.critical))
    val hasRepl     = replVec.asUInt.orR
    val hasReplC    = replCVec.asUInt.orR
    val replDCID    = WireInit(Mux(hasReplC, PriorityEncoder(replCVec), PriorityEncoder(replVec))); dontTouch(replDCID)
    val readToDBVec = VecInit(entries.map(_.io.readToDB))
    val readToDSVec = VecInit(entries.map(_.io.readToDS))

    when(hasRepl) {
        readToDBVec.foreach(_.ready := false.B)
        readToDSVec.foreach(_.ready := false.B)
        io.readToDB.valid           := io.readToDS.ready
        io.readToDS.valid           := io.readToDB.ready
        io.readToDB.bits            := readToDBVec(replDCID).bits
        io.readToDS.bits            := readToDSVec(replDCID).bits
        readToDBVec(replDCID).ready := io.readToDS.ready & io.readToDB.ready
        readToDSVec(replDCID).ready := io.readToDS.ready & io.readToDB.ready
        HAssert(readToDBVec(replDCID).valid)
        HAssert(readToDSVec(replDCID).valid)

    }.otherwise {
        connectReadToX(readToDBVec, io.readToDB)
        connectReadToX(readToDSVec, io.readToDS)
    }

    connectReadToX(VecInit(entries.map(_.io.readToCHI)), io.readToCHI)

    private def collectLocalHitLatency(name: String, events: Seq[ValidIO[UInt]]): Unit = {
        val valid = VecInit(events.map(_.valid))
        HAssert(PopCount(valid) <= 1.U)
        HomeWrapperPerf.latency(name, Mux1H(valid, events.map(_.bits)), valid.asUInt.orR)
    }

    collectLocalHitLatency("zj_local_hit_decode_to_data_task", entries.map(_.io.localHitPerf.decodeToDataTask))
    collectLocalHitLatency("zj_local_hit_data_task_to_ds_req", entries.map(_.io.localHitPerf.dataTaskToDsReq))
    collectLocalHitLatency("zj_local_hit_ds_req_to_ds_resp", entries.map(_.io.localHitPerf.dsReqToDsResp))
    collectLocalHitLatency("zj_local_hit_ds_resp_to_txdat", entries.map(_.io.localHitPerf.dsRespToTxDat))
    collectLocalHitLatency("zj_local_hit_decode_to_txdat", entries.map(_.io.localHitPerf.decodeToTxDat))
    collectLocalHitLatency("zj_local_hit_ingress_to_txdat", entries.map(_.io.localHitPerf.ingressToTxDat))

    private def collectLlcReturnLatency(name: String, llcHit: Bool): Unit = {
        val valid = VecInit(entries.map(e => e.io.llcReturnPerf.valid && e.io.llcReturnPerf.llcHit === llcHit))
        HAssert(PopCount(valid) <= 1.U)
        HomeWrapperPerf.latency(name, Mux1H(valid, entries.map(_.io.llcReturnPerf.latency)), valid.asUInt.orR)
    }

    collectLlcReturnLatency("zj_hn_llc_hit_deferred_return", true.B)
    collectLlcReturnLatency("zj_hn_llc_miss_return", false.B)

    private val validDcidCount    = PopCount(entries.map(_.io.state.valid))
    private val noFreeDcid        = io.reqDBIn.valid && !hasFreeDC
    private val reqDBOutStall     = io.reqDBIn.valid && hasFreeDC && !io.reqDBOut.ready
    private val dsWriteCommit     = entries.map(_.io.dsWriteCommit).reduce(_ | _)
    private val dsSaveWriteCommit = entries.map(_.io.dsSaveWriteCommit).reduce(_ | _)
    private val dsReplWriteCommit = entries.map(_.io.dsReplWriteCommit).reduce(_ | _)
    HomeWrapperPerf.accumulate(
        Seq(
            ("zj_datacm_req_db_in_valid", io.reqDBIn.valid),
            ("zj_datacm_req_db_in_fire", io.reqDBIn.fire),
            ("zj_datacm_no_free_dcid", noFreeDcid),
            ("zj_datacm_req_db_out_stall", reqDBOutStall),
            ("zj_datacm_valid_dcid_sum", validDcidCount),
            ("zj_datacm_task_valid", io.task.valid),
            ("zj_datacm_resp_valid", io.resp.valid),
            ("zj_datacm_release_valid", io.release.valid),
            ("zj_datacm_read_to_db_fire", io.readToDB.fire),
            ("zj_datacm_read_to_db_stall", io.readToDB.valid && !io.readToDB.ready),
            ("zj_datacm_read_to_ds_fire", io.readToDS.fire),
            ("zj_datacm_read_to_ds_stall", io.readToDS.valid && !io.readToDS.ready),
            ("zj_datacm_read_to_chi_fire", io.readToCHI.fire),
            ("zj_datacm_read_to_chi_stall", io.readToCHI.valid && !io.readToCHI.ready),
            ("zj_datacm_has_repl_cycle", hasRepl),
            ("zj_ds_write_commit", dsWriteCommit),
            ("zj_ds_save_write_commit", dsSaveWriteCommit),
            ("zj_ds_repl_write_commit", dsReplWriteCommit)
        )
    )
    HomeWrapperPerf.max("zj_datacm_valid_dcid_max", validDcidCount, true.B)

    HAssert(!(RegNext(io.readToDB.fire) ^ (PopCount(entries.map(e => RegNext(e.io.readToDB.fire))) === 1.U)))
    HAssert(!(RegNext(io.readToDS.fire) ^ (PopCount(entries.map(e => RegNext(e.io.readToDS.fire))) === 1.U)))
    HAssert(!(RegNext(io.readToCHI.fire) ^ (PopCount(entries.map(e => RegNext(e.io.readToCHI.fire))) === 1.U)))

    HardwareAssertion.placePipe(1)
}
