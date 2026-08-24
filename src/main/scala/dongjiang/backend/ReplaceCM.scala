package dongjiang.backend

import chisel3._
import chisel3.experimental.BundleLiterals._
import chisel3.util._
import dongjiang._
import dongjiang.backend.ReplaceState._
import dongjiang.bundle._
import dongjiang.data._
import dongjiang.directory.{DirEntry, HasDirectAlloc}
import dongjiang.frontend._
import dongjiang.frontend.decode._
import dongjiang.utils._
import org.chipsalliance.cde.config._
import xs.utils.arb.VipArbiter
import xs.utils.debug._
import zhujiang.perf.HomeWrapperPerf
import zhujiang.chi.ReqOpcode._
import zhujiang.chi._

object ReplaceState {
    val width     = 5
    val FREE      = 0x0.U
    val REQPOS    = 0x1.U
    val WAITPOS   = 0x2.U
    val WRIDIR    = 0x3.U
    val WAITDIR   = 0x4.U
    val UPDATEID  = 0x5.U
    val RESPCMT   = 0x6.U
    val REQDB     = 0x7.U
    val WRITE     = 0x8.U
    val SNOOP     = 0x9.U
    val WAITRWRI  = 0xa.U
    val WAITRSNP  = 0xb.U
    val COPYID    = 0xc.U
    val SAVEDATA  = 0xd.U
    val WAITRESP  = 0xe.U
    val CLEANPOST = 0xf.U
    val CLEANPOSR = 0x10.U
    val WAITWRIDIR = 0x11.U
}

object ReplacementWritePolicy {
    def issueWrite(toLan: Bool, dirty: Bool): Bool = !toLan || dirty

    def saveLocalCleanVictim(toLan: Bool, dirty: Bool): Bool = toLan && !dirty
}

trait HasReplMes { this: DJBundle =>

    val state = UInt(ReplaceState.width.W)
    val ds    = new DsIdx()

    val repl = new DJBundle with HasHnTxnID { val toLan = Bool() }

    val needSnp   = Bool()
    val alrReplSF = Bool()

    def isFree = state === FREE
    def isValid = !isFree
    def isReqPoS = state === REQPOS
    def isWaitPoS = state === WAITPOS
    def isWriDir = state === WRIDIR
    def isWaitDir = state === WAITDIR
    def isUpdHnTxnID = state === UPDATEID
    def isRespCmt = state === RESPCMT
    def isReqDB = state === REQDB
    def isWrite = state === WRITE
    def isSnoop = state === SNOOP
    def isWaitWrite = state === WAITRWRI
    def isWaitSnp = state === WAITRSNP
    def isWaitRepl = isWaitWrite | isWaitSnp
    def isCopyID = state === COPYID
    def isSaveData = state === SAVEDATA
    def isWaitResp = state === WAITRESP
    def isCleanPosT = state === CLEANPOST
    def isCleanPosR = state === CLEANPOSR
    def isCleanPoS = isCleanPosT | isCleanPosR
    def isWaitWriDir = state === WAITWRIDIR
}

class ReplaceEntry(implicit p: Parameters) extends DJModule {

    val io = IO(new Bundle {
        val config = new DJConfigIO()

        val alloc = Flipped(Decoupled(new ReplTask))
        val resp  = Decoupled(new HnTxnID)

        val cmTaskVec = Vec(2, Decoupled(new CMTask))
        val cmResp    = Flipped(Valid(new CMResp))

        val reqPoS      = Decoupled(new HnIndex with HasChiChannel)
        val posRespVec2 = Vec(djparam.nrDirBank, Vec(posSets, Flipped(Valid(UInt(posWayBits.W)))))
        val updPosTag   = Valid(new Addr with HasAddrValid with HasPackHnIdx)
        val cleanPoS    = Decoupled(new PosClean)

        val writeDir = Decoupled(new DJBundle {
            val llc = Valid(new DirEntry("llc") with HasPackHnIdx with HasDirectAlloc)
            val sf  = Valid(new DirEntry("sf") with HasPackHnIdx with HasDirectAlloc)
        })
        val writeDirDone = Flipped(Valid(new HnTxnID))

        val respDir = new DJBundle {
            val llc = Flipped(Valid(new DirEntry("llc") with HasHnTxnID))
            val sf  = Flipped(Valid(new DirEntry("sf") with HasHnTxnID))
        }

        val reqDB               = Decoupled(new HnTxnID with HasDataVec with HasQoS)
        val updHnTxnID          = Decoupled(new UpdHnTxnID)
        val dataTask            = Decoupled(new DataTask)
        val dataResp            = Flipped(Valid(new HnTxnID))
        val sfReplWritePerf     = Output(new SFReplacementPerf)
        val sfReplSnpUniquePerf = Output(new SFReplacementPerf)

        val dbg = Valid(new Bundle {
            val task = new PackHnIdx with HasHnTxnID
            val repl = new PackHnIdx with HasHnTxnID
        })
    })
    dontTouch(io.dbg)

    val reg   = RegInit((new ReplTask with HasReplMes).Lit(_.state -> FREE, _.alrReplSF -> false.B))
    val alloc = WireInit(0.U.asTypeOf(reg))
    val next  = WireInit(reg)

    io.dbg.valid := reg.isValid

    io.dbg.bits.task.hnTxnID := reg.hnTxnID
    io.dbg.bits.task.hnIdx   := reg.getHnIdx

    io.dbg.bits.repl.hnTxnID := reg.repl.hnTxnID
    io.dbg.bits.repl.hnIdx   := reg.repl.getHnIdx

    io.alloc.ready := reg.isFree
    when(io.alloc.fire) {
        alloc.dir               := io.alloc.bits.dir
        alloc.wriSF             := io.alloc.bits.wriSF
        alloc.wriLLC            := io.alloc.bits.wriLLC
        alloc.sfWriSRC          := io.alloc.bits.sfWriSRC
        alloc.sfWriSNP          := io.alloc.bits.sfWriSNP
        alloc.reqOpcode         := io.alloc.bits.reqOpcode
        alloc.reqAllocate       := io.alloc.bits.reqAllocate
        alloc.sfSrcHit          := io.alloc.bits.sfSrcHit
        alloc.sfOthHit          := io.alloc.bits.sfOthHit
        alloc.effectiveLLCState := io.alloc.bits.effectiveLLCState
        alloc.directAllocSF     := io.alloc.bits.directAllocSF
        alloc.hnTxnID           := io.alloc.bits.hnTxnID
        alloc.repl.hnTxnID      := io.alloc.bits.hnTxnID
        alloc.qos               := io.alloc.bits.qos
    }

    io.reqPoS.valid        := reg.isReqPoS
    io.reqPoS.bits.dirBank := reg.dirBank
    io.reqPoS.bits.pos.set := reg.posSet
    io.reqPoS.bits.pos.way := DontCare
    io.reqPoS.bits.channel := Mux(reg.isReplSF, ChiChannel.SNP, ChiChannel.REQ)
    HAssert.withEn(reg.isReplDIR, io.reqPoS.valid)

    val posRespHit = reg.isWaitPoS & io.posRespVec2(reg.dirBank)(reg.posSet).valid

    when(reg.isCopyID) {
        next.hnTxnID := reg.repl.hnTxnID

    }.elsewhen(posRespHit) {
        val replHnIdx = Wire(new HnIndex)
        replHnIdx.dirBank := reg.dirBank
        replHnIdx.pos.set := reg.posSet
        replHnIdx.pos.way := io.posRespVec2(reg.dirBank)(reg.posSet).bits
        next.repl.hnTxnID := replHnIdx.getTxnID
    }

    io.writeDir.valid := reg.isWriDir

    io.writeDir.bits.llc.valid            := reg.wriLLC
    io.writeDir.bits.llc.bits.addr        := reg.hnTxnID
    io.writeDir.bits.llc.bits.wayOH       := reg.dir.llc.wayOH
    io.writeDir.bits.llc.bits.hit         := reg.dir.llc.hit
    io.writeDir.bits.llc.bits.metaVec     := reg.dir.llc.metaVec
    io.writeDir.bits.llc.bits.hnIdx       := reg.getHnIdx
    io.writeDir.bits.llc.bits.directAlloc := false.B

    io.writeDir.bits.sf.valid            := reg.wriSF
    io.writeDir.bits.sf.bits.addr        := reg.hnTxnID
    io.writeDir.bits.sf.bits.wayOH       := reg.dir.sf.wayOH
    io.writeDir.bits.sf.bits.hit         := reg.dir.sf.hit
    io.writeDir.bits.sf.bits.metaVec     := reg.dir.sf.metaVec
    io.writeDir.bits.sf.bits.hnIdx       := reg.getHnIdx
    io.writeDir.bits.sf.bits.directAlloc := reg.isDirectAllocSF

    HAssert.withEn(io.writeDir.bits.llc.bits.hit, io.writeDir.valid & io.writeDir.bits.llc.valid & io.writeDir.bits.llc.bits.meta.isInvalid)
    HAssert.withEn(io.writeDir.bits.sf.bits.hit, io.writeDir.valid & io.writeDir.bits.sf.valid & io.writeDir.bits.sf.bits.metaIsInv)

    HAssert.withEn(io.writeDir.bits.llc.bits.meta.isValid, io.writeDir.valid & io.writeDir.bits.llc.valid & !io.writeDir.bits.llc.bits.hit)
    HAssert.withEn(io.writeDir.bits.sf.bits.metaIsVal, io.writeDir.valid & io.writeDir.bits.sf.valid & !io.writeDir.bits.sf.bits.hit)
    HAssert.withEn(!reg.isReplSF, reg.isDirectAllocSF)

    val sfRespHit   = reg.isWaitDir & io.respDir.sf.valid & io.respDir.sf.bits.hnTxnID === reg.hnTxnID
    val llcRespHit  = reg.isWaitDir & io.respDir.llc.valid & io.respDir.llc.bits.hnTxnID === reg.hnTxnID
    val dirRespHit  = sfRespHit | llcRespHit
    val needReplSF  = io.respDir.sf.valid & io.respDir.sf.bits.metaVec.map(_.isValid).reduce(_ | _)
    val needReplLLC = io.respDir.llc.valid & io.respDir.llc.bits.metaVec.head.isValid
    val respAddr    = Mux(io.respDir.sf.valid, io.respDir.sf.bits.addr, io.respDir.llc.bits.addr)
    val localCleanLLCVictim = llcRespHit && ReplacementWritePolicy.saveLocalCleanVictim(
        io.respDir.llc.bits.Addr.isToLAN(io.config.ci),
        io.respDir.llc.bits.meta.isDirty
    )
    HAssert(!(io.respDir.sf.valid & io.respDir.llc.valid))
    HAssert.withEn(reg.isReplSF, sfRespHit)
    HAssert.withEn(reg.isReplLLC, llcRespHit)

    io.updPosTag.valid        := dirRespHit
    io.updPosTag.bits.addrVal := Mux(io.respDir.sf.valid, io.respDir.sf.bits.metaIsVal, io.respDir.llc.bits.metaIsVal)
    io.updPosTag.bits.addr    := respAddr
    io.updPosTag.bits.hnIdx   := reg.repl.getHnIdx

    when(llcRespHit) {
        next.repl.toLan := io.respDir.llc.bits.Addr.isToLAN(io.config.ci)
        next.ds.set(io.respDir.llc.bits.addr, io.respDir.llc.bits.way)
    }

    when(sfRespHit) {
        next.needSnp   := needReplSF
        next.alrReplSF := true.B
        HAssert(!reg.alrReplSF)
    }.elsewhen(next.isFree) {
        next.needSnp   := false.B
        next.alrReplSF := false.B
    }

    io.cmTaskVec(CMID.SNP).valid             := reg.isSnoop
    io.cmTaskVec(CMID.SNP).bits              := DontCare
    io.cmTaskVec(CMID.SNP).bits.chi.nodeId   := DontCare
    io.cmTaskVec(CMID.SNP).bits.chi.channel  := ChiChannel.SNP
    io.cmTaskVec(CMID.SNP).bits.chi.opcode   := SnpOpcode.SnpUnique
    io.cmTaskVec(CMID.SNP).bits.chi.dataVec  := Seq(true.B, true.B)
    io.cmTaskVec(CMID.SNP).bits.chi.retToSrc := true.B
    io.cmTaskVec(CMID.SNP).bits.chi.toLAN    := DontCare
    io.cmTaskVec(CMID.SNP).bits.chi.size     := "b110".U
    io.cmTaskVec(CMID.SNP).bits.hnTxnID      := reg.repl.hnTxnID
    io.cmTaskVec(CMID.SNP).bits.snpVec       := reg.dir.sf.metaVec.map(_.state.asBool)
    io.cmTaskVec(CMID.SNP).bits.fromRepl     := true.B

    io.cmTaskVec(CMID.WRI).valid                      := reg.isWrite
    io.cmTaskVec(CMID.WRI).bits                       := DontCare
    io.cmTaskVec(CMID.WRI).bits.chi.nodeId            := DontCare
    io.cmTaskVec(CMID.WRI).bits.chi.channel           := ChiChannel.REQ
    io.cmTaskVec(CMID.WRI).bits.chi.opcode            := Mux(reg.repl.toLan, WriteNoSnpFull, Mux(reg.dir.llc.meta.isDirty, WriteBackFull, WriteEvictOrEvict))
    io.cmTaskVec(CMID.WRI).bits.chi.dataVec           := Seq(true.B, true.B)
    io.cmTaskVec(CMID.WRI).bits.chi.memAttr.allocate  := false.B
    io.cmTaskVec(CMID.WRI).bits.chi.memAttr.device    := false.B
    io.cmTaskVec(CMID.WRI).bits.chi.memAttr.cacheable := true.B
    io.cmTaskVec(CMID.WRI).bits.chi.memAttr.ewa       := true.B
    io.cmTaskVec(CMID.WRI).bits.chi.toLAN             := reg.repl.toLan
    io.cmTaskVec(CMID.WRI).bits.chi.size              := "b110".U
    io.cmTaskVec(CMID.WRI).bits.hnTxnID               := reg.repl.hnTxnID
    io.cmTaskVec(CMID.WRI).bits.fromRepl              := true.B
    io.cmTaskVec(CMID.WRI).bits.ds                    := reg.ds
    io.cmTaskVec(CMID.WRI).bits.cbResp                := Mux(reg.repl.toLan, ChiResp.I, reg.dir.llc.meta.cbResp)
    io.cmTaskVec(CMID.WRI).bits.dataOp.repl           := true.B

    io.cmTaskVec.foreach(_.bits.qos := reg.qos)

    io.reqDB.valid        := reg.isReqDB
    io.reqDB.bits.hnTxnID := reg.repl.hnTxnID
    io.reqDB.bits.dataVec := DataVec.Full
    io.reqDB.bits.qos     := reg.qos

    io.updHnTxnID.valid       := reg.isUpdHnTxnID
    io.updHnTxnID.bits.before := reg.hnTxnID
    io.updHnTxnID.bits.next   := reg.repl.hnTxnID

    io.dataTask.valid            := reg.isSaveData
    io.dataTask.bits             := DontCare
    io.dataTask.bits.hnTxnID     := reg.hnTxnID
    io.dataTask.bits.dataOp.save := true.B
    io.dataTask.bits.dataVec     := DataVec.Full
    io.dataTask.bits.ds          := reg.ds
    io.dataTask.bits.qos         := reg.qos
    io.dataTask.bits.perf         := 0.U.asTypeOf(new LocalHitPerfTrace)

    io.cleanPoS.valid        := reg.isCleanPoS
    io.cleanPoS.bits.hnIdx   := Mux(reg.isCleanPosT, reg.getHnIdx, reg.repl.getHnIdx)
    io.cleanPoS.bits.channel := Mux(reg.isCleanPosT, ChiChannel.SNP, Mux(reg.isReplSF, ChiChannel.SNP, ChiChannel.REQ))
    io.cleanPoS.bits.qos     := reg.qos

    io.resp.valid        := reg.isRespCmt
    io.resp.bits.hnTxnID := reg.hnTxnID

    val cmRespData  = io.cmResp.bits.taskInst.valid & io.cmResp.bits.taskInst.channel === ChiChannel.DAT
    val cmRespHit   = reg.isValid & io.cmResp.valid & io.cmResp.bits.hnTxnID === reg.repl.hnTxnID
    val dataRespHit = reg.isValid & io.dataResp.valid & io.dataResp.bits.hnTxnID === reg.hnTxnID
    val writeDirDoneHit = reg.isWaitWriDir & io.writeDirDone.valid & io.writeDirDone.bits.hnTxnID === reg.hnTxnID
    switch(reg.state) {

        is(FREE) {
            when(io.alloc.fire) { next.state := Mux(io.alloc.bits.isReplDIR, REQPOS, WRIDIR) }
        }

        is(REQPOS) {
            when(io.reqPoS.fire) { next.state := WAITPOS }
        }

        is(WAITPOS) {
            when(true.B) { next.state := Mux(posRespHit, WRIDIR, REQPOS) }
        }

        is(WRIDIR) {
            when(io.writeDir.fire) { next.state := Mux(reg.isReplDIR, WAITDIR, Mux(reg.isDirectAllocSF, WAITWRIDIR, RESPCMT)) }
        }

        is(WAITWRIDIR) {
            when(writeDirDoneHit) { next.state := RESPCMT }
        }

        is(WAITDIR) {
            when(dirRespHit) { next.state := Mux(sfRespHit, RESPCMT, Mux(needReplLLC, Mux(localCleanLLCVictim, SAVEDATA, UPDATEID), SAVEDATA)) }
        }

        is(UPDATEID) {
            when(io.updHnTxnID.fire) { next.state := WRITE }
        }

        is(RESPCMT) {
            when(io.resp.fire) { next.state := Mux(reg.isReplSF, Mux(reg.needSnp, REQDB, CLEANPOSR), Mux(reg.isReplLLC, CLEANPOSR, FREE)) }
        }

        is(REQDB) {
            when(io.reqDB.fire) { next.state := SNOOP }
        }

        is(WRITE) {
            when(io.cmTaskVec(CMID.WRI).fire) { next.state := WAITRWRI }
        }

        is(SNOOP) {
            when(io.cmTaskVec(CMID.SNP).fire) { next.state := WAITRSNP }
        }

        is(WAITRWRI) {
            when(cmRespHit) { next.state := Mux(reg.alrReplSF, CLEANPOST, RESPCMT) }
        }

        is(WAITRSNP) {
            when(cmRespHit) { next.state := Mux(cmRespData, COPYID, CLEANPOSR) }
        }

        is(COPYID) {
            when(true.B) { next.state := REQPOS }
        }

        is(SAVEDATA) {
            when(io.dataTask.fire) { next.state := WAITRESP }
        }

        is(WAITRESP) {
            when(dataRespHit) { next.state := Mux(reg.alrReplSF, CLEANPOST, RESPCMT) }
        }

        is(CLEANPOST) {
            when(io.cleanPoS.fire) { next.state := CLEANPOSR }
        }

        is(CLEANPOSR) {
            when(io.cleanPoS.fire) { next.state := FREE }
        }
    }

    HAssert.withEn(!(reg.isReplSF & reg.isReplLLC), reg.isValid)
    HAssert.withEn(reg.isFree, io.alloc.fire)
    HAssert.withEn(reg.isReqPoS, reg.isValid & io.reqPoS.fire)
    HAssert.withEn(reg.isWaitPoS, reg.isValid & posRespHit)
    HAssert.withEn(reg.isWriDir, reg.isValid & io.writeDir.fire)
    HAssert.withEn(reg.isWaitWriDir, reg.isValid & writeDirDoneHit)
    HAssert.withEn(reg.isWaitDir, reg.isValid & dirRespHit)
    HAssert.withEn(reg.isUpdHnTxnID, reg.isValid & io.updHnTxnID.fire)
    HAssert.withEn(reg.isReqDB, reg.isValid & io.reqDB.fire)
    HAssert.withEn(reg.isReplSF, reg.isValid & io.cmTaskVec(CMID.SNP).fire)
    HAssert.withEn(reg.isReplLLC, reg.isValid & io.cmTaskVec(CMID.WRI).fire)
    HAssert.withEn(reg.isWaitRepl, reg.isValid & cmRespHit)
    HAssert.withEn(reg.isSaveData, reg.isValid & io.dataTask.fire)
    HAssert.withEn(reg.isCleanPoS, reg.isValid & io.cleanPoS.fire)

    val cmRespDirty = io.cmResp.bits.taskInst.resp.asTypeOf(new ChiResp).passDirty
    when(cmRespHit & cmRespData) {
        next.wriSF              := false.B
        next.wriLLC             := true.B
        next.dir.llc.hit        := false.B
        next.dir.llc.meta.state := Mux(cmRespDirty, ChiState.UD, ChiState.SC)
        HAssert(reg.isReplSF)
    }

    when(sfRespHit) {
        next.dir.sf := io.respDir.sf.bits
    }

    val set = io.alloc.fire | reg.isValid; dontTouch(set)
    when(set) {
        reg       := Mux(io.alloc.fire, alloc, next)
        reg.state := next.state
    }

    private def setSFReplacementPerf(perf: SFReplacementPerf, event: Bool): Unit = {
        perf.event             := event
        perf.fromSrc           := event && reg.sfWriSRC
        perf.fromSnp           := event && reg.sfWriSNP
        perf.allocate          := event && reg.reqAllocate
        perf.noAllocate        := event && !reg.reqAllocate
        perf.srcHit            := event && reg.sfSrcHit
        perf.othHit            := event && reg.sfOthHit
        perf.noSrcOrOthHit     := event && !reg.sfSrcHit && !reg.sfOthHit
        perf.readNsd           := event && reg.reqOpcode === ReadNotSharedDirty
        perf.readUnique        := event && reg.reqOpcode === ReadUnique
        perf.writeBackFull     := event && reg.reqOpcode === WriteBackFull
        perf.writeEvictOrEvict := event && reg.reqOpcode === WriteEvictOrEvict
        perf.otherOpcode       := event && reg.reqOpcode =/= ReadNotSharedDirty && reg.reqOpcode =/= ReadUnique && reg.reqOpcode =/= WriteBackFull && reg.reqOpcode =/= WriteEvictOrEvict
        perf.llcI              := event && reg.effectiveLLCState === ChiState.I
        perf.llcSC             := event && reg.effectiveLLCState === ChiState.SC
        perf.llcUC             := event && reg.effectiveLLCState === ChiState.UC
        perf.llcUD             := event && reg.effectiveLLCState === ChiState.UD
    }
    setSFReplacementPerf(io.sfReplWritePerf, io.writeDir.fire && reg.isReplSF)
    setSFReplacementPerf(io.sfReplSnpUniquePerf, io.cmTaskVec(CMID.SNP).fire && reg.isReplSF)
    HomeWrapperPerf.accumulate(
        Seq(
            ("zj_repl_entry_valid", reg.isValid),
            ("zj_repl_state_req_pos", reg.isReqPoS),
            ("zj_repl_state_wait_pos", reg.isWaitPoS),
            ("zj_repl_state_wri_dir", reg.isWriDir),
            ("zj_repl_state_wait_dir", reg.isWaitDir),
            ("zj_repl_state_update_id", reg.isUpdHnTxnID),
            ("zj_repl_state_resp_cmt", reg.isRespCmt),
            ("zj_repl_state_req_db", reg.isReqDB),
            ("zj_repl_state_write", reg.isWrite),
            ("zj_repl_state_snoop", reg.isSnoop),
            ("zj_repl_state_wait_wri", reg.isWaitWrite),
            ("zj_repl_state_wait_snp", reg.isWaitSnp),
            ("zj_repl_state_copy_id", reg.isCopyID),
            ("zj_repl_state_save_data", reg.isSaveData),
            ("zj_repl_state_wait_resp", reg.isWaitResp),
            ("zj_repl_state_clean_pos_t", reg.isCleanPosT),
            ("zj_repl_state_clean_pos_r", reg.isCleanPosR),
            ("zj_repl_alloc_fire", io.alloc.fire),
            ("zj_repl_req_pos_valid", io.reqPoS.valid),
            ("zj_repl_req_pos_fire", io.reqPoS.fire),
            ("zj_repl_req_pos_stall", io.reqPoS.valid && !io.reqPoS.ready),
            ("zj_repl_pos_resp_hit", posRespHit),
            ("zj_repl_write_dir_valid", io.writeDir.valid),
            ("zj_repl_write_dir_fire", io.writeDir.fire),
            ("zj_repl_write_dir_stall", io.writeDir.valid && !io.writeDir.ready),
            ("zj_repl_dir_resp_hit", dirRespHit),
            ("zj_repl_dir_resp_sf_hit", sfRespHit),
            ("zj_repl_dir_resp_llc_hit", llcRespHit),
            ("zj_repl_upd_hn_txnid_valid", io.updHnTxnID.valid),
            ("zj_repl_upd_hn_txnid_fire", io.updHnTxnID.fire),
            ("zj_repl_resp_valid", io.resp.valid),
            ("zj_repl_resp_fire", io.resp.fire),
            ("zj_repl_resp_stall", io.resp.valid && !io.resp.ready),
            ("zj_repl_req_db_valid", io.reqDB.valid),
            ("zj_repl_req_db_fire", io.reqDB.fire),
            ("zj_repl_req_db_stall", io.reqDB.valid && !io.reqDB.ready),
            ("zj_repl_data_task_valid", io.dataTask.valid),
            ("zj_repl_data_task_fire", io.dataTask.fire),
            ("zj_repl_data_task_stall", io.dataTask.valid && !io.dataTask.ready),
            ("zj_repl_clean_pos_valid", io.cleanPoS.valid),
            ("zj_repl_clean_pos_fire", io.cleanPoS.fire),
            ("zj_repl_clean_pos_stall", io.cleanPoS.valid && !io.cleanPoS.ready),
            ("zj_repl_cm_wri_fire", io.cmTaskVec(CMID.WRI).fire),
            ("zj_repl_cm_snp_fire", io.cmTaskVec(CMID.SNP).fire),
            ("zj_repl_cm_resp_hit", cmRespHit),
            ("zj_repl_data_resp_hit", dataRespHit)
        )
    )

    HAssert.checkTimeout(reg.isFree, TIMEOUT_REPLACE, cf"TIMEOUT: Replace State[${reg.state}]")
}

class ReplaceCM(implicit p: Parameters) extends DJModule {

    val io = IO(new Bundle {
        val config = new DJConfigIO()

        val task = Flipped(Decoupled(new ReplTask))
        val resp = Valid(new HnTxnID)

        val cmTaskVec = Vec(2, Decoupled(new CMTask))
        val cmResp    = Flipped(Valid(new CMResp))

        val reqPosVec2  = Vec(djparam.nrDirBank, Vec(posSets, Valid(new ChiChnlBundle)))
        val posRespVec2 = Vec(djparam.nrDirBank, Vec(posSets, Flipped(Valid(UInt(posWayBits.W)))))
        val updPosTag   = Valid(new Addr with HasAddrValid with HasPackHnIdx)
        val cleanPoS    = Decoupled(new PosClean)

        val writeDir = Decoupled(new DJBundle {
            val llc = Valid(new DirEntry("llc") with HasPackHnIdx with HasDirectAlloc)
            val sf  = Valid(new DirEntry("sf") with HasPackHnIdx with HasDirectAlloc)
        })
        val writeDirDone = Flipped(Valid(new HnTxnID))

        val respDir = new DJBundle {
            val llc = Flipped(Valid(new DirEntry("llc") with HasHnTxnID))
            val sf  = Flipped(Valid(new DirEntry("sf") with HasHnTxnID))
        }

        val reqDB      = Decoupled(new HnTxnID with HasDataVec)
        val updHnTxnID = Valid(new UpdHnTxnID)
        val dataTask   = Decoupled(new DataTask)
        val dataResp   = Flipped(Valid(new HnTxnID))
    })

    val entries = Seq.fill(nrReplaceCM) { Module(new ReplaceEntry()) }

    private def anySFReplacement(perfs: Seq[SFReplacementPerf])(select: SFReplacementPerf => Bool): Bool =
        perfs.map(select).reduce(_ | _)

    private def sfReplacementEvents(prefix: String, perfs: Seq[SFReplacementPerf]): Seq[(String, Bool)] =
        Seq(
            (prefix, anySFReplacement(perfs)(_.event)),
            (s"${prefix}_from_src", anySFReplacement(perfs)(_.fromSrc)),
            (s"${prefix}_from_snp", anySFReplacement(perfs)(_.fromSnp)),
            (s"${prefix}_allocate", anySFReplacement(perfs)(_.allocate)),
            (s"${prefix}_no_allocate", anySFReplacement(perfs)(_.noAllocate)),
            (s"${prefix}_src_hit", anySFReplacement(perfs)(_.srcHit)),
            (s"${prefix}_oth_hit", anySFReplacement(perfs)(_.othHit)),
            (s"${prefix}_no_src_or_oth_hit", anySFReplacement(perfs)(_.noSrcOrOthHit)),
            (s"${prefix}_readnsd", anySFReplacement(perfs)(_.readNsd)),
            (s"${prefix}_readunique", anySFReplacement(perfs)(_.readUnique)),
            (s"${prefix}_writebackfull", anySFReplacement(perfs)(_.writeBackFull)),
            (s"${prefix}_writeevictorevict", anySFReplacement(perfs)(_.writeEvictOrEvict)),
            (s"${prefix}_other_opcode", anySFReplacement(perfs)(_.otherOpcode)),
            (s"${prefix}_llc_i", anySFReplacement(perfs)(_.llcI)),
            (s"${prefix}_llc_sc", anySFReplacement(perfs)(_.llcSC)),
            (s"${prefix}_llc_uc", anySFReplacement(perfs)(_.llcUC)),
            (s"${prefix}_llc_ud", anySFReplacement(perfs)(_.llcUD))
        )

    private val sfReplWritePerfs     = entries.map(_.io.sfReplWritePerf)
    private val sfReplSnpUniquePerfs = entries.map(_.io.sfReplSnpUniquePerf)
    HAssert(PopCount(VecInit(sfReplWritePerfs.map(_.event))) <= 1.U)
    HAssert(PopCount(VecInit(sfReplSnpUniquePerfs.map(_.event))) <= 1.U)
    HomeWrapperPerf.accumulate(
        sfReplacementEvents("zj_sf_repl_write_commit", sfReplWritePerfs) ++
            sfReplacementEvents("zj_sf_repl_snpunique_fire", sfReplSnpUniquePerfs) ++
            Seq(
                (
                    "zj_sf_direct_alloc_write_fire",
                    io.writeDir.fire && io.writeDir.bits.sf.valid && io.writeDir.bits.sf.bits.directAlloc
                )
            )
    )

    Alloc(entries.map(_.io.alloc), io.task)

    val reqPosReadyVec3 = Wire(Vec(nrReplaceCM, Vec(djparam.nrDirBank, Vec(posSets, Bool()))))
    io.reqPosVec2.zipWithIndex.foreach { case (reqVec, i) =>
        reqVec.zipWithIndex.foreach { case (req, j) =>
            val vipArb   = Module(new VipArbiter(new ChiChnlBundle, nrReplaceCM))
            val hitVec   = VecInit(entries.map(e => e.io.reqPoS.bits.dirBank === i.U & e.io.reqPoS.bits.pos.set === j.U))
            val validVec = VecInit(entries.map(_.io.reqPoS.valid).zip(hitVec).map { case (a, b) => a & b })

            vipArb.io.in.zip(validVec).foreach { case (a, v) => a.valid := v }

            vipArb.io.in.zip(entries).foreach { case (a, e) => a.bits.channel := e.io.reqPoS.bits.channel }

            reqPosReadyVec3.zip(vipArb.io.in.zip(hitVec)).foreach { case (r, (a, h)) => r(i)(j) := a.ready & h }

            req.valid           := vipArb.io.out.valid
            req.bits            := vipArb.io.out.bits
            vipArb.io.out.ready := true.B
        }
    }

    entries.map(_.io.reqPoS.ready).zip(reqPosReadyVec3).foreach { case (a, b) =>
        a := VecInit(b.flatten).asUInt.orR
        HAssert(PopCount(b.flatten) <= 1.U)
    }

    entries.foreach { e =>
        e.io.config      := io.config
        e.io.cmResp      := io.cmResp
        e.io.respDir     := io.respDir
        e.io.dataResp    := io.dataResp
        e.io.posRespVec2 := io.posRespVec2
        e.io.writeDirDone := io.writeDirDone
    }

    fastQosRRArb(entries.map(_.io.reqDB), io.reqDB)
    io.resp       := fastRRArb.validOut(entries.map(_.io.resp))
    io.updHnTxnID := fastRRArb.validOut(entries.map(_.io.updHnTxnID))
    io.updPosTag  := fastArb(entries.map(_.io.updPosTag))
    io.cleanPoS   <> fastQosRRArb(entries.map(_.io.cleanPoS))
    io.dataTask   <> fastQosRRArb(entries.map(_.io.dataTask))
    io.writeDir   <> fastRRArb(entries.map(_.io.writeDir))
    HAssert(PopCount(entries.map(_.io.updPosTag.valid)) <= 1.U)

    io.cmTaskVec.zip(entries.map(_.io.cmTaskVec).transpose).foreach { case (a, b) => a <> fastQosRRArb(b) }

    HAssert.placePipe(1)
}
