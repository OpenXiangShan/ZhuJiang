package dongjiang.backend

import chisel3._
import chisel3.experimental.BundleLiterals._
import chisel3.util._
import dongjiang._
import dongjiang.backend.ReadState._
import dongjiang.bundle._
import dongjiang.frontend._
import dongjiang.frontend.decode._
import dongjiang.utils._
import org.chipsalliance.cde.config._
import xs.utils.debug._
import zhujiang.chi.DatOpcode._
import zhujiang.chi.RspOpcode._
import zhujiang.chi._
import zhujiang.perf.{HistogramRange, ZJPerf}

object ReadState {
    val width     = 3
    val FREE      = 0x0.U
    val CANNEST   = 0x1.U
    val SENDREQ   = 0x2.U
    val WAITDATA0 = 0x3.U
    val WAITDATA1 = 0x4.U
    val CANTNEST  = 0x5.U
    val SENDACK   = 0x6.U
    val RESPCMT   = 0x7.U
}

class ReadState(implicit p: Parameters) extends DJBundle {

    val state = UInt(ReadState.width.W)

    def isFree = state === FREE
    def isValid = !isFree
    def isCanNest = state === CANNEST
    def isSendReq = state === SENDREQ
    def isWaitData0 = state === WAITDATA0
    def isWaitData1 = state === WAITDATA1
    def isWaitData = isWaitData0 | isWaitData1
    def isCantNest = state === CANTNEST
    def isUpdNest = isCanNest | isCantNest
    def isSendCompAck = state === SENDACK
    def isRespCmt = state === RESPCMT
}

class ReadEntry(implicit p: Parameters) extends DJModule {

    val io = IO(new Bundle {
        val config = new DJConfigIO()

        val alloc = Flipped(Decoupled(new CMTask))
        val resp  = Decoupled(new CMResp)

        val txReq = Decoupled(new ReqFlit(true))
        val txRsp = if (hasBBN) Some(Decoupled(new RespFlit())) else None
        val rxDat = Flipped(Valid(new DataFlit()))

        val updPosNest = if (hasBBN) Some(Decoupled(new PosCanNest)) else None

        val dbg = Valid(new ReadState with HasHnTxnID)
    })

    val reg  = RegInit((new ReadState with HasPackCMTask with HasPackTaskInst with HasRespErr).Lit(_.state -> FREE))
    val next = WireInit(reg)

    io.dbg.valid        := reg.isValid
    io.dbg.bits.state   := reg.state
    io.dbg.bits.hnTxnID := reg.task.hnTxnID

    io.alloc.ready := reg.isFree
    HAssert.withEn(!io.alloc.bits.doDMT, io.alloc.valid & io.alloc.bits.chi.toBBN)

    io.txReq.valid := reg.isSendReq

    io.txReq.bits                 := DontCare
    io.txReq.bits.ExpCompAck      := reg.task.chi.expCompAck
    io.txReq.bits.MemAttr         := reg.task.chi.memAttr.asUInt
    io.txReq.bits.Order           := Order.None
    io.txReq.bits.Addr            := DontCare
    io.txReq.bits.Size            := reg.task.chi.size
    io.txReq.bits.Opcode          := reg.task.chi.opcode
    io.txReq.bits.ReturnTxnID.get := Mux(reg.task.doDMT, reg.task.chi.txnID, reg.task.hnTxnID)
    io.txReq.bits.ReturnNID.get   := Mux(reg.task.doDMT, reg.task.chi.nodeId, Fill(io.txReq.bits.ReturnNID.get.getWidth, 1.U))
    io.txReq.bits.TxnID           := reg.task.hnTxnID
    io.txReq.bits.SrcID           := reg.task.chi.getNoC
    io.txReq.bits.QoS             := reg.task.qos

    HAssert.withEn(PopCount(reg.task.chi.dataVec) === 1.U, io.txReq.valid && io.txReq.bits.Size <= 5.U)
    HAssert.withEn(PopCount(reg.task.chi.dataVec) === 2.U, io.txReq.valid && io.txReq.bits.Size === 6.U)

    if (hasBBN) {

        io.txRsp.get.valid := reg.isSendCompAck

        io.txRsp.get.bits        := DontCare
        io.txRsp.get.bits.Opcode := CompAck
        io.txRsp.get.bits.TxnID  := reg.task.chi.txnID
        io.txRsp.get.bits.SrcID  := reg.task.chi.getNoC
        io.txRsp.get.bits.TgtID  := reg.task.chi.nodeId
        io.txRsp.get.bits.QoS    := reg.task.qos
    }

    io.resp.valid := reg.isRespCmt

    io.resp.bits.hnTxnID := reg.task.hnTxnID
    io.resp.bits.toRepl  := false.B
    io.resp.bits.qos     := reg.task.qos
    when(!reg.task.doDMT) {
        io.resp.bits.taskInst          := 0.U.asTypeOf(new TaskInst)
        io.resp.bits.taskInst.valid    := true.B
        io.resp.bits.taskInst.fwdValid := false.B
        io.resp.bits.taskInst.channel  := ChiChannel.DAT
        io.resp.bits.taskInst.opcode   := CompData
        io.resp.bits.taskInst.resp     := reg.taskInst.resp
        io.resp.bits.taskInst.fwdResp  := ChiResp.I
        io.resp.bits.respErr           := reg.respErr
    }.otherwise {
        io.resp.bits.taskInst       := 0.U.asTypeOf(new TaskInst)
        io.resp.bits.taskInst.valid := true.B
        io.resp.bits.respErr        := RespErr.NormalOkay
    }

    if (hasBBN) {
        io.updPosNest.get.valid      := reg.isUpdNest
        io.updPosNest.get.bits.hnIdx := reg.task.getHnIdx
        io.updPosNest.get.bits.nest  := reg.isCanNest
        io.updPosNest.get.bits.qos   := reg.task.qos
    }

    val recDataHit = io.rxDat.fire & reg.task.hnTxnID === io.rxDat.bits.TxnID & io.rxDat.bits.Opcode === CompData
    when(io.alloc.fire) {
        next.task     := io.alloc.bits
        next.taskInst := 0.U.asTypeOf(new TaskInst)
        next.respErr  := RespErr.NormalOkay

    }.elsewhen(recDataHit) {

        next.task.chi.nodeId := io.rxDat.bits.HomeNID
        next.task.chi.txnID  := io.rxDat.bits.DBID

        next.taskInst.resp := io.rxDat.bits.Resp

        when(!reg.isERR) {
            next.respErr := io.rxDat.bits.RespErr
        }
        HAssert.withEn(next.isNDERR, reg.isWaitData1 & reg.isNDERR)
        HAssert.withEn(!(reg.isOK & next.isEXOK), reg.isWaitData1)
        HAssert.withEn(!(reg.isEXOK & next.isNDERR), reg.isWaitData1)
    }

    val updNestFire = io.updPosNest.map(_.fire).getOrElse(false.B)
    switch(reg.state) {
        is(FREE) {
            when(io.alloc.fire) { next.state := Mux(io.alloc.bits.chi.toBBN, CANNEST, SENDREQ) }
        }
        is(CANNEST) {
            when(updNestFire) { next.state := SENDREQ }
        }
        is(SENDREQ) {
            when(io.txReq.fire) { next.state := Mux(reg.task.doDMT, RESPCMT, WAITDATA0) }
        }
        is(WAITDATA0) {
            when(recDataHit) { next.state := Mux(reg.task.chi.isHalfSize, Mux(reg.task.chi.toBBN, CANTNEST, RESPCMT), WAITDATA1) }
        }
        is(WAITDATA1) {
            when(recDataHit) { next.state := Mux(reg.task.chi.toBBN, CANTNEST, RESPCMT) }
        }
        is(CANTNEST) {
            when(updNestFire) { next.state := Mux(reg.task.chi.expCompAck, SENDACK, FREE) }
        }
        is(SENDACK) {
            when(io.txRsp.map(_.fire).getOrElse(false.B)) { next.state := RESPCMT }
        }
        is(RESPCMT) {
            when(io.resp.fire) { next.state := FREE }
        }
    }

    HAssert.withEn(reg.isFree, io.alloc.fire)
    HAssert.withEn(reg.isUpdNest, reg.isValid & updNestFire)
    HAssert.withEn(reg.isSendReq, reg.isValid & io.txReq.fire)
    HAssert.withEn(reg.isWaitData, reg.isValid & recDataHit)
    HAssert.withEn(reg.isRespCmt, reg.isValid & io.resp.fire)
    HAssert.withEn(reg.isSendCompAck, reg.isValid & io.txRsp.map(_.fire).getOrElse(false.B))

    val set = io.alloc.fire | reg.isValid; dontTouch(set)
    when(set) { reg := next }

    HAssert.checkTimeout(reg.isFree, TIMEOUT_READ, cf"TIMEOUT: Read State[${reg.state}]")
}

class ReadCM(implicit p: Parameters) extends DJModule {

    val io = IO(new Bundle {
        val config = new DJConfigIO()

        val alloc = Flipped(Decoupled(new CMTask))
        val resp  = Decoupled(new CMResp)

        val txReq = Decoupled(new ReqFlit(true))
        val txRsp = if (hasBBN) Some(Decoupled(new RespFlit())) else None
        val rxDat = Flipped(Valid(new DataFlit()))

        val updPosNest = if (hasBBN) Some(Decoupled(new PosCanNest)) else None
    })

    val entries = Seq.fill(nrReadCM) { Module(new ReadEntry()) }
    val dbgVec  = VecInit(entries.map(_.io.dbg))
    dontTouch(dbgVec)

    entries.foreach(_.io.config := io.config)
    Alloc(entries.map(_.io.alloc), io.alloc)
    entries.foreach(_.io.rxDat := io.rxDat)

    io.txReq <> fastQosRRArb(entries.map(_.io.txReq))
    io.resp  <> fastQosRRArb(entries.map(_.io.resp))
    if (hasBBN) {
        io.txRsp.get      <> fastQosRRArb(entries.map(_.io.txRsp.get))
        io.updPosNest.get <> fastQosRRArb(entries.map(_.io.updPosNest.get))
    }

    ZJPerf.whenEnabled {
        val occupancy = PopCount(entries.map(_.io.dbg.valid))
        val full      = occupancy === nrReadCM.U
        ZJPerf.accumulate(
            Seq(
                "zj_hn_readcm_alloc_fire"       -> io.alloc.fire,
                "zj_hn_readcm_alloc_full_stall" -> (io.alloc.valid && full)
            )
        )
        ZJPerf.distribution(
            "zj_hn_readcm_occupancy",
            occupancy,
            true.B,
            HistogramRange.occupancy(nrReadCM)
        )
    }

    HardwareAssertion.placePipe(1)
}
