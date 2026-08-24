package dongjiang.backend

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import zhujiang.chi._
import zhujiang.chi.RspOpcode._
import dongjiang._
import dongjiang.utils._
import dongjiang.bundle._
import xijiang.Node
import xs.utils.debug._
import zhujiang.perf.HomeWrapperPerf
import xs.utils.queue._
import dongjiang.frontend._
import dongjiang.frontend.decode._
import dongjiang.data._
import dongjiang.directory._

class Backend(isTop: Boolean = false)(implicit p: Parameters) extends DJModule {
    override def isTopModule: Boolean = isTop

    val io = IO(new Bundle {

        val config = new DJConfigIO()

        val txReq = Decoupled(new ReqFlit(true))
        val txSnp = Decoupled(new SnoopFlit)
        val txRsp = Decoupled(new RespFlit)

        val rxRsp = Flipped(Decoupled(new RespFlit))
        val rxDat = Flipped(Valid(new DataFlit))

        val writeDir = Decoupled(new DJBundle {
            val llc = Valid(new DirEntry("llc") with HasPackHnIdx with HasDirectAlloc)
            val sf  = Valid(new DirEntry("sf") with HasPackHnIdx with HasDirectAlloc)
        })

        val respDir = new DJBundle {
            val llc = Flipped(Valid(new DirEntry("llc") with HasHnTxnID))
            val sf  = Flipped(Valid(new DirEntry("sf") with HasHnTxnID))
        }

        val reqPosVec2  = Vec(djparam.nrDirBank, Vec(posSets, Valid(new ChiChnlBundle)))
        val posRespVec2 = Vec(djparam.nrDirBank, Vec(posSets, Flipped(Valid(UInt(posWayBits.W)))))
        val updPosTag   = Valid(new Addr with HasAddrValid with HasPackHnIdx)

        val cmtTaskVec = Vec(djparam.nrDirBank, Flipped(Valid(new CommitTask with HasHnTxnID)))

        val updPosNest = if (hasBBN) Some(Valid(new PosCanNest)) else None
        val cleanPoS   = Valid(new PosClean)

        val unlock = Valid(new PackHnIdx)

        val fastResp = Flipped(Decoupled(new RespFlit))

        val updHnTxnID = Valid(new UpdHnTxnID)
        val reqDB      = Decoupled(new HnTxnID with HasDataVec)
        val dataTask   = Decoupled(new DataTask)
        val dataResp   = Flipped(Valid(new HnTxnID))
        val cleanDB    = Decoupled(new HnTxnID with HasDataVec)

        val getAddrVec = Vec(3, new GetAddr)
    })

    val commit  = Module(new Commit())
    val replCM  = Module(new ReplaceCM())
    val snoopCM = Module(new SnoopCM())
    val writeCM = Module(new WriteCM())
    val readCM  = Module(new ReadCM())

    commit.io.config  := io.config
    replCM.io.config  := io.config
    snoopCM.io.config := io.config
    readCM.io.config  := io.config
    writeCM.io.config := io.config

    val txRspVec = if (hasBBN) Seq(commit.io.txRsp, readCM.io.txRsp.get, FastQueue(io.fastResp)) else Seq(commit.io.txRsp, FastQueue(io.fastResp))
    io.txReq       <> FastQueue(fastQosRRArb(Seq(readCM.io.txReq, writeCM.io.txReq)))
    io.txSnp       <> snoopCM.io.txSnp
    io.txRsp       <> fastQosRRArb(txRspVec)
    io.rxRsp.ready := true.B

    io.reqPosVec2 <> replCM.io.reqPosVec2
    io.updPosTag  := replCM.io.updPosTag
    if (hasBBN) {
        io.updPosNest.get := fastQosRRArb.validOut(Seq(readCM.io.updPosNest.get, writeCM.io.updPosNest.get))
    }

    val cleanPoS = FastQueue(fastQosRRArb(Seq(commit.io.cleanPoS, replCM.io.cleanPoS)))
    cleanPoS.ready          := io.cleanDB.ready
    io.cleanPoS.valid       := cleanPoS.fire
    io.cleanPoS.bits        := cleanPoS.bits
    io.unlock.valid         := cleanPoS.fire
    io.unlock.bits.hnIdx    := cleanPoS.bits.hnIdx
    io.cleanDB.valid        := cleanPoS.valid
    io.cleanDB.bits.hnTxnID := cleanPoS.bits.hnIdx.getTxnID
    io.cleanDB.bits.dataVec := DataVec.Full

    val wDirQ = Module(new Queue(chiselTypeOf(io.writeDir.bits), entries = 1, pipe = true, flow = false))
    wDirQ.io.enq <> replCM.io.writeDir
    io.writeDir  <> wDirQ.io.deq
    replCM.io.writeDirDone.valid := io.writeDir.fire && io.writeDir.bits.sf.valid && io.writeDir.bits.sf.bits.directAlloc
    replCM.io.writeDirDone.bits.hnTxnID := io.writeDir.bits.sf.bits.hnIdx.getTxnID

    io.updHnTxnID := replCM.io.updHnTxnID
    io.reqDB      <> fastArb(Seq(replCM.io.reqDB, commit.io.reqDB))
    io.dataTask   <> fastQosRRArb(Seq(FastQueue(commit.io.dataTask), FastQueue(replCM.io.dataTask), FastQueue(writeCM.io.dataTask)))

    commit.io.cmtTaskVec <> io.cmtTaskVec.map(t => Pipe(t))
    commit.io.rxRsp      := io.rxRsp
    commit.io.rxDat      := io.rxDat
    commit.io.replResp   := replCM.io.resp
    commit.io.dataResp   := io.dataResp

    replCM.io.task        <> FastQueue(commit.io.replTask)
    replCM.io.respDir     := io.respDir
    replCM.io.dataResp    := io.dataResp
    replCM.io.posRespVec2 := io.posRespVec2

    val cmResp = Pipe(fastQosRRArb.validOut(Seq(snoopCM.io.resp, readCM.io.resp, writeCM.io.resp)))
    commit.io.cmResp.valid := cmResp.valid & !cmResp.bits.toRepl
    replCM.io.cmResp.valid := cmResp.valid & cmResp.bits.toRepl
    commit.io.cmResp.bits  := cmResp.bits
    replCM.io.cmResp.bits  := cmResp.bits

    snoopCM.io.alloc <> fastQosRRArb(Seq(FastQueue(commit.io.cmTaskVec(CMID.SNP)), FastQueue(replCM.io.cmTaskVec(CMID.SNP))))
    snoopCM.io.rxRsp := io.rxRsp
    snoopCM.io.rxDat := io.rxDat

    writeCM.io.alloc    <> fastQosRRArb(Seq(FastQueue(commit.io.cmTaskVec(CMID.WRI)), FastQueue(replCM.io.cmTaskVec(CMID.WRI))))
    writeCM.io.rxRsp    := io.rxRsp
    writeCM.io.dataResp := io.dataResp

    readCM.io.alloc <> fastQosRRArb(Seq(FastQueue(commit.io.cmTaskVec(CMID.READ))))
    readCM.io.rxDat := io.rxDat

    io.getAddrVec(0).hnIdx := io.txReq.bits.TxnID.asTypeOf(new HnTxnID).getHnIdx
    io.getAddrVec(1).hnIdx := io.txSnp.bits.TxnID.asTypeOf(new HnTxnID).getHnIdx
    io.getAddrVec(2).hnIdx := replCM.io.writeDir.bits.llc.bits.addr.asTypeOf(new HnTxnID).getHnIdx
    HAssert.withEn(replCM.io.writeDir.bits.llc.bits.hnIdx.asUInt === replCM.io.writeDir.bits.sf.bits.hnIdx.asUInt, replCM.io.writeDir.valid)
    HAssert.withEn(replCM.io.writeDir.bits.llc.bits.addr === replCM.io.writeDir.bits.sf.bits.addr, replCM.io.writeDir.valid)

    val txReqMemAttr = io.txReq.bits.MemAttr.asTypeOf(MemAttr())
    val txReqAddr    = io.getAddrVec(0).result.addr
    val txReqSize    = io.txReq.bits.Size
    io.txReq.bits.Addr := PriorityMux(
        Seq(
            !txReqMemAttr.cacheable  -> txReqAddr,
            (txReqSize === "b110".U) -> Cat(txReqAddr(addrBits - 1, offsetBits), 0.U(offsetBits.W)),
            true.B                   -> Cat(txReqAddr(addrBits - 1, offsetBits - 1), 0.U((offsetBits - 1).W))
        )
    )
    require(djparam.nrBeat == 2)

    io.txSnp.bits.Addr := (io.getAddrVec(1).result.addr >> 6.U) << 3.U

    wDirQ.io.enq.bits.llc.bits.addr := io.getAddrVec(2).result.addr
    wDirQ.io.enq.bits.sf.bits.addr  := io.getAddrVec(2).result.addr

    private val cmtTaskPerfEvents = io.cmtTaskVec.zipWithIndex.flatMap { case (task, i) =>
        Seq(
            (s"zj_backend_cmt_task_valid_$i", task.valid)
        )
    }
    HomeWrapperPerf.accumulate(
        cmtTaskPerfEvents ++ Seq(
            ("zj_backend_tx_req_fire", io.txReq.fire),
            ("zj_backend_tx_req_stall", io.txReq.valid && !io.txReq.ready),
            ("zj_backend_tx_snp_fire", io.txSnp.fire),
            ("zj_backend_tx_snp_stall", io.txSnp.valid && !io.txSnp.ready),
            ("zj_backend_tx_rsp_fire", io.txRsp.fire),
            ("zj_backend_tx_rsp_stall", io.txRsp.valid && !io.txRsp.ready),
            ("zj_backend_rx_rsp_fire", io.rxRsp.fire),
            ("zj_backend_rx_dat_valid", io.rxDat.valid),
            ("zj_backend_write_dir_fire", io.writeDir.fire),
            ("zj_backend_write_dir_stall", io.writeDir.valid && !io.writeDir.ready),
            ("zj_backend_req_db_fire", io.reqDB.fire),
            ("zj_backend_req_db_stall", io.reqDB.valid && !io.reqDB.ready),
            ("zj_backend_data_task_fire", io.dataTask.fire),
            ("zj_backend_data_task_stall", io.dataTask.valid && !io.dataTask.ready),
            ("zj_backend_clean_db_fire", io.cleanDB.fire),
            ("zj_backend_clean_db_stall", io.cleanDB.valid && !io.cleanDB.ready),
            ("zj_backend_fast_resp_fire", io.fastResp.fire),
            ("zj_backend_fast_resp_stall", io.fastResp.valid && !io.fastResp.ready)
        )
    )

    HardwareAssertion.placePipe(2)
}
