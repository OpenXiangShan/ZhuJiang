package dongjiang

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import zhujiang.chi._
import dongjiang.utils._
import dongjiang.bundle._
import dongjiang.frontend._
import dongjiang.backend._
import dongjiang.directory._
import dongjiang.data._
import xijiang._
import xijiang.router.base.DeviceIcnBundle
import zhujiang.chi.FlitHelper.connIcn
import dongjiang.frontend.decode.Decode._
import xs.utils.debug.HardwareAssertion
import xs.utils.sram.SramPowerCtl
import zhujiang.ZJParametersKey
import zhujiang.utils.SramPwrCtlBoring
import zhujiang.perf.HomeWrapperPerf
import xs.utils.FileRegisters

class DJConfigIO(implicit p: Parameters) extends DJBundle {
    val ci       = Input(UInt(ciBits.W))
    val closeLLC = Input(Bool())
    val bankId   = Input(UInt(bankBits.W))
}

class DongJiang(lanNode: Node, bbnNode: Option[Node] = None)(implicit p: Parameters) extends DJModule with ImplicitClock with ImplicitReset {
    override def isTopModule: Boolean = true

    val io = IO(new Bundle {

        val flushCache = new DJBundle {
            val req = Input(Valid(UInt(nrBank.W)))
            val ack = Output(UInt(nrBank.W))
        }
        val working = Output(Bool())

        val config = new DJConfigIO()

        val lan       = new DeviceIcnBundle(lanNode)
        val bbnOpt    = if (hasBBN) Some(new DeviceIcnBundle(bbnNode.get)) else None
        val ramPwrCtl = new SramPowerCtl
    })

    io.flushCache.ack := DontCare

    var hnNodeSeq = Seq(lanNode)
    if (hasBBN) {
        require(bbnNode.nonEmpty)
        require(bbnNode.get.nodeType == NodeType.HX)
        hnNodeSeq = hnNodeSeq ++ Seq(bbnNode.get)
    } else {
        require(bbnNode.isEmpty)
    }
    require(lanNode.nodeType == NodeType.HF)

    def setRx(flit: Flit, t: Int): Flit = { val temp = WireInit(flit); temp.tgt := t.U; temp }
    val icnVec = Wire(MixedVec(hnNodeSeq.map(n => new DeviceIcnBundle(n))))

    icnVec.head <> io.lan
    icnVec.foreach(_.tx.debug.foreach(_ := DontCare))
    icnVec.head.rx.req.get.bits  := setRx(io.lan.rx.req.get.bits.asTypeOf(new ReqFlit(false)), LAN)
    icnVec.head.rx.resp.get.bits := setRx(io.lan.rx.resp.get.bits.asTypeOf(new RespFlit()), LAN)
    icnVec.head.rx.data.get.bits := setRx(io.lan.rx.data.get.bits.asTypeOf(new DataFlit()), LAN)
    if (hasHPR) {
        icnVec.head.rx.hpr.get.bits := setRx(io.lan.rx.hpr.get.bits.asTypeOf(new ReqFlit(false)), LAN)
    }

    if (hasBBN) {
        icnVec.last                   <> io.bbnOpt.get
        icnVec.last.rx.req.get.bits   := setRx(io.bbnOpt.get.rx.req.get.bits.asTypeOf(new ReqFlit(false)), BBN)
        icnVec.last.rx.snoop.get.bits := setRx(io.bbnOpt.get.rx.snoop.get.bits.asTypeOf(new SnoopFlit()), BBN)
        icnVec.last.rx.resp.get.bits  := setRx(io.bbnOpt.get.rx.resp.get.bits.asTypeOf(new RespFlit()), BBN)
        icnVec.last.rx.data.get.bits  := setRx(io.bbnOpt.get.rx.data.get.bits.asTypeOf(new DataFlit()), BBN)
        if (hasHPR) {
            icnVec.last.rx.hpr.get.bits := setRx(io.bbnOpt.get.rx.hpr.get.bits.asTypeOf(new ReqFlit(false)), BBN)
        }
    }

    val frontends = Seq.fill(djparam.nrDirBank)(Module(new Frontend()))
    val backend   = Module(new Backend())
    val directory = Module(new Directory())
    val dataBlock = Module(new DataBlock())
    val chiXbar   = Module(new ChiXbar())

    val workSftReg = RegInit(0.U((readDirLatency.max(readDsLatency) * 2).W))
    workSftReg := Cat(frontends.map(_.io.working).reduce(_ | _), workSftReg(workSftReg.getWidth - 1, 1))
    io.working := workSftReg.orR

    frontends.zipWithIndex.foreach { case (a, b) => a.io.dirBank := b.U }
    frontends.foreach(_.io.config := io.config)
    directory.io.config := io.config
    backend.io.config   := io.config

    chiXbar.io.rxReq.inVec.zip(icnVec.map(_.rx.req.get)).foreach { case (a, b) => connIcn(a, b) }
    chiXbar.io.rxReq.outVec.zip(frontends.map(_.io.rxReq)).foreach { case (a, b) => a <> b }

    if (hasHPR) {
        chiXbar.io.rxHpr.inVec.get.zip(icnVec.map(_.rx.hpr.get)).foreach { case (a, b) => connIcn(a, b) }
    }
    chiXbar.io.rxHpr.outVec.zip(frontends.map(_.io.rxHpr)).foreach { case (a, b) => a <> b }

    if (hasBBN) {
        connIcn(chiXbar.io.rxSnp.in.get, icnVec.last.rx.snoop.get)
        chiXbar.io.rxSnp.outVec.get.zip(frontends.map(_.io.rxSnp.get)).foreach { case (a, b) => a <> b }
    }

    val rxRspArb = Module(new Arbiter(new RespFlit, nrIcn))
    rxRspArb.io.in.zip(icnVec.map(_.rx.resp)).foreach { case (a, b) => a <> b.get }
    connIcn(backend.io.rxRsp, rxRspArb.io.out)

    val rxDatArb = Module(new Arbiter(new DataFlit, nrIcn))
    rxDatArb.io.in.zip(icnVec.map(_.rx.data)).foreach { case (a, b) => a <> b.get }
    connIcn(dataBlock.io.rxDat, rxDatArb.io.out)
    backend.io.rxDat.valid := rxDatArb.io.out.fire
    backend.io.rxDat.bits  := rxDatArb.io.out.bits

    chiXbar.io.txReq.in <> backend.io.txReq
    chiXbar.io.txReq.outVec.zip(icnVec.map(_.tx.req.get)).foreach { case (a, b) => connIcn(b, a) }

    chiXbar.io.txSnp.in <> backend.io.txSnp
    chiXbar.io.txSnp.outVec.zip(icnVec.map(_.tx.snoop.get)).foreach { case (a, b) => connIcn(b, a) }

    chiXbar.io.txRsp.in <> backend.io.txRsp
    chiXbar.io.txRsp.outVec.zip(icnVec.map(_.tx.resp.get)).foreach { case (a, b) => connIcn(b, a) }

    chiXbar.io.txDat.in <> dataBlock.io.txDat
    chiXbar.io.txDat.outVec.zip(icnVec.map(_.tx.data.get)).foreach { case (a, b) => connIcn(b, a) }

    val alrUsePos = frontends.map(_.io.alrUsePoS).reduce(_ +& _); dontTouch(alrUsePos)
    val posBusy = PriorityMux(
        Seq(
            (alrUsePos < (djparam.nrPoS * 0.5).toInt.U)  -> "b00".U,
            (alrUsePos < (djparam.nrPoS * 0.75).toInt.U) -> "b01".U,
            (alrUsePos < (djparam.nrPoS * 0.9).toInt.U)  -> "b10".U,
            true.B                                       -> "b11".U
        )
    )
    chiXbar.io.cBusy := RegNext(Cat(0.U(1.W), posBusy))

    frontends.zipWithIndex.foreach { case (f, i) =>
        f.io.respDir   := directory.io.rRespVec(i)
        f.io.reqPosVec := backend.io.reqPosVec2(i)
        f.io.updPosTag := backend.io.updPosTag
        f.io.cleanPoS  := backend.io.cleanPoS
        f.io.getAddrVec.zip(backend.io.getAddrVec).foreach { case (a, b) => a.hnIdx := b.hnIdx }
        if (hasBBN) {
            f.io.updPosNest.get := backend.io.updPosNest.get
        }
    }

    directory.io.readVec.zip(frontends.map(_.io.readDir)).foreach { case (a, b) => a <> b }
    directory.io.write  <> backend.io.writeDir
    directory.io.unlock := backend.io.unlock

    backend.io.respDir  := directory.io.wResp
    backend.io.fastResp <> fastRRArb(frontends.map(_.io.fastResp))
    backend.io.dataResp := dataBlock.io.resp
    backend.io.cmtTaskVec.zip(frontends.map(_.io.cmtTask)).foreach { case (a, b) => a <> b }
    backend.io.getAddrVec.zip(frontends.map(_.io.getAddrVec).transpose).foreach { case (a, b) =>
        a.result := VecInit(b.map(_.result))(a.hnIdx.dirBank)
    }
    backend.io.posRespVec2.zip(frontends.map(_.io.posRespVec)).foreach { case (a, b) => a <> b }

    dataBlock.io.updHnTxnID := backend.io.updHnTxnID
    dataBlock.io.cleanDB    <> fastArb.validOut(Seq(fastRRArb(frontends.map(_.io.cleanDB)), backend.io.cleanDB))
    dataBlock.io.reqDB      <> fastArb(Seq(backend.io.reqDB, fastRRArb(frontends.map(_.io.reqDB_s3)), fastRRArb(frontends.map(_.io.reqDB_s1))))
    dataBlock.io.task       := fastArb.validOut(Seq(backend.io.dataTask, fastRRArb(frontends.map(_.io.fastData))))

    SramPwrCtlBoring.getSrc() := io.ramPwrCtl
    HardwareAssertion.placePipe(3)

    val lanRxReq = io.lan.rx.req.get
    val lanRxReqBits = lanRxReq.bits.asTypeOf(new ReqFlit(false))
    val hnIngressStall = io.lan.rx.bundleMap.values.map(ch => ch.valid && !ch.ready).reduce(_ || _)
    val hnEgressStall = io.lan.tx.bundleMap.values.map(ch => ch.valid && !ch.ready).reduce(_ || _)
    val hnTopDownPressure = hnIngressStall || hnEgressStall
    val hnTopDownInternal = hnIngressStall
    val hnTopDownNoCExit = !hnTopDownInternal && hnEgressStall
    val hnCapacityPressure = hnIngressStall && alrUsePos >= djparam.nrPoS.U
    assert(PopCount(Seq(hnTopDownInternal, hnTopDownNoCExit)) ===
        hnTopDownPressure.asUInt, "HN TopDown owner is not closed")
    var xsPerfSeq = Seq(
        ("hn_lan_rx_readnsd", lanRxReq.fire && lanRxReqBits.Opcode === ReqOpcode.ReadNotSharedDirty),
        ("hn_lan_rx_readunique", lanRxReq.fire && lanRxReqBits.Opcode === ReqOpcode.ReadUnique),
        ("hn_lan_rx_readonce", lanRxReq.fire && lanRxReqBits.Opcode === ReqOpcode.ReadOnce),
        ("hn_lan_rx_readnosnp", lanRxReq.fire && lanRxReqBits.Opcode === ReqOpcode.ReadNoSnp),
        ("zj_hn_lan_rx_req_fire", io.lan.rx.req.get.fire),
        ("zj_hn_lan_rx_req_stall", io.lan.rx.req.get.valid && !io.lan.rx.req.get.ready),
        ("zj_hn_lan_rx_dat_fire", io.lan.rx.data.get.fire),
        ("zj_hn_lan_rx_dat_stall", io.lan.rx.data.get.valid && !io.lan.rx.data.get.ready),
        ("zj_hn_lan_rx_rsp_fire", io.lan.rx.resp.get.fire),
        ("zj_hn_lan_rx_rsp_stall", io.lan.rx.resp.get.valid && !io.lan.rx.resp.get.ready),
        ("zj_hn_lan_tx_req_fire", io.lan.tx.req.get.fire),
        ("zj_hn_lan_tx_req_stall", io.lan.tx.req.get.valid && !io.lan.tx.req.get.ready),
        ("zj_hn_lan_tx_dat_fire", io.lan.tx.data.get.fire),
        ("zj_hn_lan_tx_dat_stall", io.lan.tx.data.get.valid && !io.lan.tx.data.get.ready),
        ("zj_hn_lan_tx_rsp_fire", io.lan.tx.resp.get.fire),
        ("zj_hn_lan_tx_rsp_stall", io.lan.tx.resp.get.valid && !io.lan.tx.resp.get.ready),
        ("zj_hn_lan_tx_snp_fire", io.lan.tx.snoop.get.fire),
        ("zj_hn_lan_tx_snp_stall", io.lan.tx.snoop.get.valid && !io.lan.tx.snoop.get.ready),
        ("zj_hn_topdown_pressure_cycle", hnTopDownPressure),
        ("zj_hn_topdown_internal_block", hnTopDownInternal),
        ("zj_hn_topdown_internal_capacity_cycle", hnCapacityPressure),
        ("zj_hn_topdown_noc_exit_stall", hnTopDownNoCExit),
        ("zj_hn_working_cycle", workSftReg.orR),
        ("zj_hn_pos_occupancy_sum", alrUsePos),
        ("zj_hn_pos_busy_ge50_cycle", alrUsePos >= (djparam.nrPoS * 0.5).toInt.U),
        ("zj_hn_pos_busy_ge75_cycle", alrUsePos >= (djparam.nrPoS * 0.75).toInt.U),
        ("zj_hn_pos_busy_ge90_cycle", alrUsePos >= (djparam.nrPoS * 0.9).toInt.U)
    )
    if (hasHPR) {
        xsPerfSeq = xsPerfSeq ++ Seq(
            ("zj_hn_lan_rx_hpr_fire", io.lan.rx.hpr.get.fire),
            ("zj_hn_lan_rx_hpr_stall", io.lan.rx.hpr.get.valid && !io.lan.rx.hpr.get.ready)
        )
    }
    HomeWrapperPerf.accumulate(xsPerfSeq)
    HomeWrapperPerf.max("zj_hn_pos_occupancy_max", alrUsePos, true.B)
}
