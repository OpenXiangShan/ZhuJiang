package dongjiang.frontend

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import zhujiang.chi._
import dongjiang._
import dongjiang.backend.{CMTask, CommitTask}
import dongjiang.utils._
import dongjiang.bundle._
import dongjiang.data.DataTask
import xs.utils.debug._
import dongjiang.directory.{DirEntry, DirMsg, PackDirMsg}
import dongjiang.frontend.decode.{CommitCode, Operations}
import xs.utils.queue.FastQueue
import xs.utils.GTimer
import zhujiang.perf.ZJPerf

class Frontend(isTop: Boolean = false)(implicit p: Parameters) extends DJModule {
    override def isTopModule: Boolean = isTop

    private def bufferReqWithIngressCycle(rxReq: DecoupledIO[ReqFlit]): DecoupledIO[TimedReqFlit] = {
        val timedReq = Wire(Decoupled(new TimedReqFlit))
        timedReq.valid             := rxReq.valid
        timedReq.bits.req          := rxReq.bits
        timedReq.bits.ingressCycle := GTimer()
        rxReq.ready                := timedReq.ready
        FastQueue(timedReq)
    }

    private def connectTimedReq(timedReq: DecoupledIO[TimedReqFlit], reqToTask: ReqToChiTask): Unit = {
        reqToTask.io.rxReq.valid          := timedReq.valid
        reqToTask.io.rxReq.bits           := timedReq.bits.req
        timedReq.ready                    := reqToTask.io.rxReq.ready
        reqToTask.io.perfIngressCycle.get := timedReq.bits.ingressCycle
    }

    val io = IO(new Bundle {

        val config  = new DJConfigIO()
        val dirBank = Input(UInt(dirBankBits.W))

        val rxReq = Flipped(Decoupled(new ReqFlit(false)))
        val rxHpr = Flipped(Decoupled(new ReqFlit(false)))
        val rxSnp = if (hasBBN) Some(Flipped(Decoupled(new SnoopFlit))) else None

        val reqDB_s1 = Decoupled(new HnTxnID with HasDataVec)
        val reqDB_s3 = Decoupled(new HnTxnID with HasDataVec)
        val fastData = Decoupled(new DataTask)
        val cleanDB  = Decoupled(new HnTxnID with HasDataVec)

        val readDir = Decoupled(new Addr with HasPackHnIdx)
        val respDir = Flipped(Valid(new DirMsg))

        val cmtTask = Valid(new CommitTask with HasHnTxnID)

        val getAddrVec = Vec(3, Flipped(new GetAddr))

        val reqPosVec  = Vec(posSets, Flipped(Valid(new ChiChnlBundle)))
        val posRespVec = Vec(posSets, Valid(UInt(posWayBits.W)))
        val updPosTag  = Flipped(Valid(new Addr with HasAddrValid with HasPackHnIdx))
        val updPosNest = if (hasBBN) Some(Flipped(Valid(new PosCanNest))) else None
        val cleanPoS   = Flipped(Valid(new PosClean))

        val fastResp = Decoupled(new RespFlit)

        val alrUsePoS = Output(UInt(log2Ceil(nrPoS + 1).W))

        val working = Output(Bool())
    })

    val req2Task = Module(new ReqToChiTask())
    val hpr2Task = Module(new ReqToChiTask())
    val snp2Task = if (hasBBN) Some(Module(new SnpToChiTask())) else None

    val hprTaskBuf = Module(new TaskBuffer(nrHprTaskBuf, sort = true))
    val reqTaskBuf = Module(new TaskBuffer(nrReqTaskBuf, sort = true))
    val snpTaskBuf = if (hasBBN) Some(Module(new TaskBuffer(nrSnpTaskBuf, sort = false))) else None

    val posAlloc_s0 = Wire(Valid(new Addr with HasChiChannel))
    val posTable    = Module(new PosTable())
    val block       = Module(new Block())

    val pipe = Module(new Pipe(chiselTypeOf(block.io.task_s1.bits), readDirLatency - 1))

    val decode = Module(new Decode())

    req2Task.io.config := io.config
    hpr2Task.io.config := io.config
    posTable.io.config := io.config
    block.io.config    := io.config
    decode.io.config   := io.config

    posTable.io.dirBank := io.dirBank

    io.getAddrVec.zip(posTable.io.getAddrVec).foreach { case (a, b) => a <> b }
    io.reqDB_s1      <> block.io.reqDB_s1
    io.reqDB_s3      <> decode.io.reqDB_s3
    io.readDir       <> block.io.readDir_s1
    io.fastResp      <> FastQueue(block.io.fastResp_s1)
    io.fastData      <> decode.io.fastData_s3
    io.cleanDB.valid := decode.io.cleanDB_s3.valid
    io.cleanDB.bits  := decode.io.cleanDB_s3.bits
    io.cmtTask       := decode.io.cmtTask_s3
    io.alrUsePoS     := posTable.io.alrUsePoS
    io.posRespVec    := posTable.io.posRespVec
    io.working       := hprTaskBuf.io.working | reqTaskBuf.io.working | snpTaskBuf.map(_.io.working).getOrElse(false.B) | posTable.io.working
    HAssert.withEn(io.cleanDB.ready, io.cleanDB.valid)

    if (ZJPerf.enabled) {
        connectTimedReq(bufferReqWithIngressCycle(io.rxHpr), hpr2Task)
        connectTimedReq(bufferReqWithIngressCycle(io.rxReq), req2Task)
    } else {
        hpr2Task.io.rxReq <> FastQueue(io.rxHpr)
        req2Task.io.rxReq <> FastQueue(io.rxReq)
    }

    hprTaskBuf.io.chiTaskIn <> hpr2Task.io.chiTask
    hprTaskBuf.io.retry_s1  := block.io.retry_s1
    hprTaskBuf.io.sleep_s1  := posTable.io.sleep_s1
    hprTaskBuf.io.wakeup    := posTable.io.wakeup

    reqTaskBuf.io.chiTaskIn <> req2Task.io.chiTask
    reqTaskBuf.io.retry_s1  := block.io.retry_s1
    reqTaskBuf.io.sleep_s1  := posTable.io.sleep_s1
    reqTaskBuf.io.wakeup    := posTable.io.wakeup

    if (hasBBN) {

        snp2Task.get.io.config := io.config
        snp2Task.get.io.rxSnp  <> FastQueue(io.rxSnp.get)

        snpTaskBuf.get.io.chiTaskIn <> snp2Task.get.io.chiTask
        snpTaskBuf.get.io.retry_s1  := block.io.retry_s1
        snpTaskBuf.get.io.sleep_s1  := DontCare
        snpTaskBuf.get.io.wakeup    := DontCare
        assert(!snpTaskBuf.get.io.chiTask_s0.valid, " TODO")
    }

    if (hasBBN) {
        posAlloc_s0.bits.channel := block.io.chiTask_s0.bits.chi.channel
        posTable.io.updNest.get  := io.updPosNest.get
    } else {
        posAlloc_s0.bits.channel := ChiChannel.REQ
    }
    posAlloc_s0.valid     := block.io.chiTask_s0.valid
    posAlloc_s0.bits.addr := block.io.chiTask_s0.bits.addr
    posTable.io.alloc_s0  := posAlloc_s0
    posTable.io.retry_s1  := block.io.retry_s1
    posTable.io.clean     := io.cleanPoS
    posTable.io.updTag    := io.updPosTag
    posTable.io.reqPosVec := io.reqPosVec

    require(!hasBBN, "TODO: connect snpTaskBuf.io.chiTask_s0")
    val selectReq = !hprTaskBuf.io.chiTask_s0.valid & !hprTaskBuf.io.lockTask
    reqTaskBuf.io.chiTask_s0.ready := selectReq
    hprTaskBuf.io.chiTask_s0.ready := true.B
    block.io.chiTask_s0.valid      := Mux(selectReq, reqTaskBuf.io.chiTask_s0.valid, hprTaskBuf.io.chiTask_s0.valid)
    block.io.chiTask_s0.bits       := Mux(selectReq, reqTaskBuf.io.chiTask_s0.bits, hprTaskBuf.io.chiTask_s0.bits)
    block.io.posBlock_s1           := posTable.io.block_s1
    block.io.hnIdx_s1              := posTable.io.hnIdx_s1

    pipe.io.enq.valid := block.io.task_s1.valid
    pipe.io.enq.bits  := block.io.task_s1.bits

    decode.io.task_s2.valid := pipe.io.deq.valid
    decode.io.task_s2.bits  := pipe.io.deq.bits
    decode.io.respDir_s3    := io.respDir

    HardwareAssertion.placePipe(2)
}
