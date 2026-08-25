package zhujiang.device.bridge.axi

import chisel3._
import chisel3.util._
import freechips.rocketchip.util.MaskGen
import org.chipsalliance.cde.config.Parameters
import xijiang.{Node, NodeType}
import xijiang.router.base.DeviceIcnBundle
import xs.utils.arb.ConditionVipArbiter
import xs.utils.mbist.MbistPipeline
import zhujiang.perf.ZhuJiangPerf
import xs.utils.PickOneLow
import zhujiang.ZJModule
import zhujiang.axi._
import zhujiang.chi.FlitHelper.connIcn
import zhujiang.chi.{DatOpcode, DataFlit, ReqFlit, ReqOpcode, RespFlit}

class AxiBridge(node: Node)(implicit p: Parameters) extends ZJModule {
    private val compareTagBits = 32
    private val tagOffset      = 6
    require(node.nodeType == NodeType.S)
    private val _axiP     = node.axiDevParams.get.extPortParams.getOrElse(AxiParams(idBits = log2Ceil(node.outstanding)))
    private val axiParams = _axiP.copy(dataBits = dw, addrBits = raw)

    val icn     = IO(new DeviceIcnBundle(node))
    val axi     = IO(new AxiBundle(axiParams))
    val working = IO(Output(Bool()))
    dontTouch(icn)
    dontTouch(axi)

    private def compareTag(addr0: UInt, addr1: UInt): Bool = {
        addr0(compareTagBits + tagOffset - 1, tagOffset) === addr1(compareTagBits + tagOffset - 1, tagOffset)
    }

    private val wakeups = Wire(Vec(node.outstanding, Valid(UInt(raw.W))))

    private def rspSelFunc(self: RespFlit, other: RespFlit): Bool = self.QoS >= other.QoS
    private val icnRspArb = Module(new ConditionVipArbiter(new RespFlit, node.outstanding, rspSelFunc))
    connIcn(icn.tx.resp.get, icnRspArb.io.out)

    private def axSelFunc(self: AXFlit, other: AXFlit): Bool = self.qos >= other.qos
    private val awArb = Module(new ConditionVipArbiter(new AXFlit(axiParams), node.outstanding, axSelFunc))
    axi.aw <> awArb.io.out

    private val arArb = Module(new ConditionVipArbiter(new AXFlit(axiParams), node.outstanding, axSelFunc))
    axi.ar <> arArb.io.out

    private val dataBuffer              = Module(new AxiDataBuffer(axiParams, node.outstanding, node.outstanding))
    private val dataBufferallocSelector = Module(new DataBufferAllocReqSelector(node.outstanding))
    dataBuffer.io.alloc     <> dataBufferallocSelector.io.out
    dataBuffer.io.icn.valid := icn.rx.data.get.valid
    dataBuffer.io.icn.bits  := icn.rx.data.get.bits.asTypeOf(dataBuffer.io.icn.bits)
    icn.rx.data.get.ready   := dataBuffer.io.icn.ready
    axi.w                   <> dataBuffer.io.axi

    private val cms = for (idx <- 0 until node.outstanding) yield {
        val cm = Module(new AxiBridgeCtrlMachine(node, axiParams, node.outstanding, compareTag))
        cm.suggestName(s"cm_$idx")
        cm.io.wakeupIns                    := wakeups.zipWithIndex.filterNot(_._2 == idx).map(_._1)
        wakeups(idx).valid                 := cm.io.wakeupOut.valid
        wakeups(idx).bits                  := cm.io.wakeupOut.bits
        cm.io.idx                          := idx.U
        icnRspArb.io.in(idx).valid         := cm.icn.tx.resp.valid
        icnRspArb.io.in(idx).bits          := cm.icn.tx.resp.bits.asTypeOf(icn.tx.resp.get.bits.cloneType)
        cm.icn.tx.resp.ready               := icnRspArb.io.in(idx).ready
        awArb.io.in(idx)                   <> cm.axi.aw
        arArb.io.in(idx)                   <> cm.axi.ar
        dataBufferallocSelector.io.in(idx) <> cm.dataBufferAlloc.req
        cm.dataBufferAlloc.resp            := dataBufferallocSelector.io.out.fire && dataBufferallocSelector.io.out.bits.idxOH(idx)
        cm
    }
    private val chiTxV = icn.tx.elements.values.map({
        case chn: DecoupledIO[Data] => chn.valid
        case _                      => false.B
    })
    working := RegNext(Cat(cms.map(_.io.info.valid) ++ chiTxV).orR)

    private val wSeq    = cms.map(_.axi.w)
    private val awQueue = Module(new Queue(UInt(node.outstanding.W), entries = node.outstanding))
    awQueue.io.enq.valid := awArb.io.out.fire
    awQueue.io.enq.bits  := UIntToOH(awArb.io.chosen)
    when(awArb.io.out.fire) {
        assert(awQueue.io.enq.ready)
    }
    private val wSelValid = Mux1H(awQueue.io.deq.bits, wSeq.map(_.valid))
    awQueue.io.deq.ready               := dataBuffer.io.fromCmDat.ready && wSelValid
    dataBuffer.io.fromCmDat.valid      := awQueue.io.deq.valid && wSelValid
    dataBuffer.io.fromCmDat.bits.flit  := Mux1H(awQueue.io.deq.bits, wSeq.map(_.bits))
    dataBuffer.io.fromCmDat.bits.idxOH := awQueue.io.deq.bits
    wSeq.zipWithIndex.foreach({ case (w, i) => w.ready := dataBuffer.io.fromCmDat.ready && awQueue.io.deq.valid && awQueue.io.deq.bits(i) })

    private val shouldBeWaited    = cms.map(cm => cm.io.info.valid && !cm.io.wakeupOut.valid && cm.io.info.bits.isSnooped)
    private val cmAddrSeq         = cms.map(cm => cm.io.info.bits.addr)
    private val req               = icn.rx.req.get.bits.asTypeOf(new ReqFlit(true))
    private val isWriteReq        = req.Opcode === ReqOpcode.WriteNoSnpPtl || req.Opcode === ReqOpcode.WriteNoSnpFull
    private val reqTagMatchVec    = VecInit(shouldBeWaited.zip(cmAddrSeq).map(elm => elm._1 && compareTag(elm._2, req.Addr)))
    private val pendingWriteTagMatchVec = VecInit(cms.map(cm =>
        cm.io.info.valid && cm.io.info.bits.isWrite && compareTag(cm.io.info.bits.addr, req.Addr)
    ))
    private val readNoSnpPendingWriteOverlap = icn.rx.req.get.fire && req.Opcode === ReqOpcode.ReadNoSnp && pendingWriteTagMatchVec.asUInt.orR
    private val reqTagMatchVecReg = RegEnable(reqTagMatchVec, icn.rx.req.get.fire)
    private val waitNum           = PopCount(reqTagMatchVecReg)

    private val busyEntries = cms.map(_.io.info.valid)
    private val enqCtrl     = PickOneLow(busyEntries)
    private val busyCmCount = PopCount(busyEntries)
    private val busyReadEntries = cms.map(cm => cm.io.info.valid && !cm.io.info.bits.isWrite)
    private val busyWriteEntries = cms.map(cm => cm.io.info.valid && cm.io.info.bits.isWrite)
    private val busyReadCount = PopCount(busyReadEntries)
    private val busyWriteCount = PopCount(busyWriteEntries)
    private val noFreeCm    = icn.rx.req.get.valid && !enqCtrl.valid

    // Keep the cross-protocol identity at the SN boundary.  For an SN node,
    // The HN-to-SN request carries the return transaction in ReturnTxnID,
    // HN source in SrcID, and HN request ID in TxnID. The original CPU source
    // is not present on this normal-read path.
    // The selected control-machine index is the AXI ID issued to memory.
    private val idMapActive = RegInit(VecInit(Seq.fill(node.outstanding)(false.B)))
    private val idMapReturnTxnId = Reg(Vec(node.outstanding, UInt(12.W)))
    private val idMapChiSource = Reg(Vec(node.outstanding, UInt(niw.W)))
    private val idMapHnTxnId = Reg(Vec(node.outstanding, UInt(12.W)))
    private val idMapReturnNid = Reg(Vec(node.outstanding, UInt(niw.W)))
    private val idMapArIssued = RegInit(VecInit(Seq.fill(node.outstanding)(false.B)))
    private val idMapIsWrite = RegInit(VecInit(Seq.fill(node.outstanding)(false.B)))
    private val idMapRemainingBeats = Reg(Vec(node.outstanding, UInt(4.W)))
    private val axiBusSize = log2Ceil(axiParams.dataBits / 8)

    for (idx <- 0 until node.outstanding) {
        when (!cms(idx).io.info.valid && idMapActive(idx) && idMapIsWrite(idx) &&
            !(icn.rx.req.get.fire && enqCtrl.bits(idx))) {
            idMapActive(idx) := false.B
            idMapArIssued(idx) := false.B
        }
        when (icn.rx.req.get.fire && enqCtrl.bits(idx)) {
            idMapActive(idx) := true.B
            idMapArIssued(idx) := false.B
            idMapReturnTxnId(idx) := req.ReturnTxnID.get
            idMapChiSource(idx) := req.SrcID
            idMapHnTxnId(idx) := req.TxnID
            idMapReturnNid(idx) := req.ReturnNID.get
            idMapIsWrite(idx) := isWriteReq
            idMapRemainingBeats(idx) := Mux(req.Size > axiBusSize.U,
                (1.U << (req.Size - axiBusSize.U)).asUInt, 1.U)
        }
        when (cms(idx).axi.ar.fire) {
            idMapArIssued(idx) := true.B
        }
    }

    dontTouch(idMapActive)
    dontTouch(idMapReturnTxnId)
    dontTouch(idMapChiSource)
    dontTouch(idMapHnTxnId)
    dontTouch(idMapReturnNid)
    dontTouch(idMapArIssued)

    icn.rx.req.get.ready := enqCtrl.valid
    axi.b.ready          := true.B

    for ((cm, idx) <- cms.zipWithIndex) {
        cm.icn.rx.req.valid  := icn.rx.req.get.valid && enqCtrl.bits(idx)
        cm.icn.rx.req.bits   := icn.rx.req.get.bits.asTypeOf(new ReqFlit(true))
        cm.icn.rx.data.valid := dataBuffer.io.toCmDat.valid && dataBuffer.io.toCmDat.bits.TxnID === idx.U
        cm.icn.rx.data.bits  := dataBuffer.io.toCmDat.bits

        cm.axi.b.valid     := axi.b.valid && axi.b.bits.id === idx.U
        cm.axi.b.bits      := axi.b.bits
        cm.io.readDataFire := axi.r.fire && axi.r.bits.id === idx.U
        cm.io.readDataLast := axi.r.bits.last
        cm.io.waitNum      := waitNum
    }

    private val readDataPipe = Module(new Queue(gen = new DataFlit, entries = 1, pipe = true))
    private val ctrlVec      = VecInit(cms.map(_.io.info.bits))
    private val ctrlSel      = ctrlVec(axi.r.bits.id(log2Ceil(node.outstanding) - 1, 0))

    private val cycleTick       = RegInit(0.U(32.W))
    private val arIssueTickVec  = Reg(Vec(node.outstanding, UInt(32.W)))
    private val readCompleteVec = cms.map(cm => cm.io.readDataFire && cm.io.readDataLast)
    private val readComplete    = readCompleteVec.reduce(_ || _)
    cycleTick := cycleTick + 1.U
    cms.zipWithIndex.foreach { case (cm, idx) =>
        when(cm.axi.ar.fire) {
            arIssueTickVec(idx) := cycleTick
        }
    }
    private val arToLastRLatencyVec = readCompleteVec.zipWithIndex.map { case (done, idx) =>
        Mux(done, cycleTick - arIssueTickVec(idx), 0.U)
    }
    private val arToLastRLatencySum = arToLastRLatencyVec.foldLeft(0.U(40.W)) { case (sum, latency) =>
        sum +& latency
    }
    private val readCompleteCount = PopCount(readCompleteVec)
    private val writeCompleteVec = cms.map(_.axi.b.fire)
    private val writeCompleteCount = PopCount(writeCompleteVec)

    // Strict buckets count each completed read independently.
    private def strictLatencyBucketCount(start: Int, stop: Int): UInt = {
        PopCount(arToLastRLatencyVec.zip(readCompleteVec).map { case (latency, done) =>
            done && latency >= start.U && latency < stop.U
        })
    }

    private def strictLatencyGeCount(start: Int): UInt = {
        PopCount(arToLastRLatencyVec.zip(readCompleteVec).map { case (latency, done) =>
            done && latency >= start.U
        })
    }

    private def strictLatencyBucketEvents(prefix: String, start: Int, stop: Int, step: Int): Seq[(String, UInt)] = {
        (start until stop by step).map { bucketStart =>
            val bucketStop = bucketStart + step
            (s"${prefix}_${bucketStart}_${bucketStop}", strictLatencyBucketCount(bucketStart, bucketStop))
        }
    }

    private val strictLatencyEvents =
        Seq(
            ("zj_axi_ar_to_last_r_strict_sampled", readCompleteCount),
            ("zj_axi_ar_to_last_r_strict_sum", arToLastRLatencySum),
            ("zj_axi_ar_to_last_r_strict_overflow_1000", strictLatencyGeCount(1000)),
            ("zj_axi_ar_to_last_r_strict_multi_complete_cycle", readCompleteCount > 1.U)
        ) ++
            strictLatencyBucketEvents("zj_axi_ar_to_last_r_strict_0_50", 0, 50, 1) ++
            strictLatencyBucketEvents("zj_axi_ar_to_last_r_strict_50_200", 50, 200, 10) ++
            strictLatencyBucketEvents("zj_axi_ar_to_last_r_strict_200_1000", 200, 1000, 100)

    readDataPipe.io.enq.valid := axi.r.valid
    axi.r.ready               := readDataPipe.io.enq.ready

    readDataPipe.io.enq.bits        := DontCare
    readDataPipe.io.enq.bits.Data   := axi.r.bits.data
    readDataPipe.io.enq.bits.Opcode := DatOpcode.CompData
    if (dw == 512) {
        readDataPipe.io.enq.bits.DataID := 0.U
    } else if (dw == 256) {
        readDataPipe.io.enq.bits.DataID := Cat(ctrlSel.addr(5), false.B) + (ctrlSel.readCnt << 1)
    } else if (dw == 128) {
        readDataPipe.io.enq.bits.DataID := ctrlSel.addr(5, 4) + ctrlSel.readCnt
    } else {
        require(requirement = false, s"illegal DW $dw")
    }
    readDataPipe.io.enq.bits.TxnID   := ctrlSel.returnTxnId.get
    readDataPipe.io.enq.bits.SrcID   := 0.U
    readDataPipe.io.enq.bits.TgtID   := ctrlSel.returnNid.get
    readDataPipe.io.enq.bits.HomeNID := ctrlSel.srcId
    readDataPipe.io.enq.bits.DBID    := ctrlSel.txnId
    readDataPipe.io.enq.bits.Resp    := "b010".U
    readDataPipe.io.enq.bits.RespErr := axi.r.bits.resp
    readDataPipe.io.enq.bits.QoS     := ctrlSel.qos
    readDataPipe.io.enq.bits.BE      := Mux(ctrlSel.size === 6.U, Fill(bew, true.B), MaskGen(ctrlSel.addr, ctrlSel.size, bew))

    connIcn(icn.tx.data.get, readDataPipe.io.deq)

    private val returnDat = icn.tx.data.get.bits.asTypeOf(new DataFlit)
    private val returnDataCandidateVec = VecInit((0 until node.outstanding).map { idx =>
        idMapActive(idx) && idMapArIssued(idx) && idMapReturnTxnId(idx) === returnDat.TxnID
    })
    private val returnDataCandidate = icn.tx.data.get.fire && returnDataCandidateVec.asUInt.orR
    private val returnDataMapMatchVec = VecInit((0 until node.outstanding).map { idx =>
        idMapActive(idx) && idMapArIssued(idx) &&
            idMapReturnTxnId(idx) === returnDat.TxnID &&
            idMapHnTxnId(idx) === returnDat.DBID(11, 0) &&
            idMapChiSource(idx) === returnDat.HomeNID &&
            idMapReturnNid(idx) === returnDat.TgtID
    })
    private val returnDataMapMatch = icn.tx.data.get.fire && returnDataMapMatchVec.asUInt.orR
    private val returnDataMapUnknown = icn.tx.data.get.fire && !returnDataMapMatch
    private val returnDataSourceMismatch = returnDataCandidate && !VecInit((0 until node.outstanding).map { idx =>
        returnDataCandidateVec(idx) && idMapChiSource(idx) === returnDat.HomeNID
    }).asUInt.orR
    private val returnDataDbidMismatch = returnDataCandidate && !VecInit((0 until node.outstanding).map { idx =>
        returnDataCandidateVec(idx) && idMapHnTxnId(idx) === returnDat.DBID(11, 0)
    }).asUInt.orR
    private val returnDataTargetMismatch = returnDataCandidate && !VecInit((0 until node.outstanding).map { idx =>
        returnDataCandidateVec(idx) && idMapReturnNid(idx) === returnDat.TgtID
    }).asUInt.orR
    private val returnDataMapLast = returnDataMapMatch && VecInit((0 until node.outstanding).map { idx =>
        returnDataMapMatchVec(idx) && idMapRemainingBeats(idx) === 1.U
    }).asUInt.orR
    private val readCompleteMapMatch = readComplete && VecInit((0 until node.outstanding).map { idx =>
        idMapActive(idx) && idMapArIssued(idx) && readCompleteVec(idx)
    }).asUInt.orR
    private val readCompleteMapUnknown = readComplete && !readCompleteMapMatch
    private val idMapActiveCount = PopCount(idMapActive)

    assert(!(icn.tx.data.get.fire && PopCount(returnDataMapMatchVec) > 1.U),
        "SN return DAT identity map is ambiguous")

    for (idx <- 0 until node.outstanding) {
        when (returnDataMapMatch && returnDataMapMatchVec(idx)) {
            when (idMapRemainingBeats(idx) === 1.U) {
                idMapActive(idx) := false.B
                idMapArIssued(idx) := false.B
            }.otherwise {
                idMapRemainingBeats(idx) := idMapRemainingBeats(idx) - 1.U
            }
        }
    }

    ZhuJiangPerf.accumulate(
        Seq(
            ("read_req_cnt", icn.rx.req.get.fire && req.Opcode === ReqOpcode.ReadNoSnp),
            ("zj_axi_readnosnp_overlap_pending_write", readNoSnpPendingWriteOverlap),
            ("write_req_cnt", icn.rx.req.get.fire && isWriteReq),
            ("total_mem_req_cnt", icn.rx.req.get.fire),
            ("total_req_retention_cnt", cms.map(_.io.info.valid).reduce(_ || _)),
            ("zj_axi_rx_req_stall", icn.rx.req.get.valid && !icn.rx.req.get.ready),
            ("zj_axi_rx_dat_fire", icn.rx.data.get.fire),
            ("zj_axi_rx_dat_stall", icn.rx.data.get.valid && !icn.rx.data.get.ready),
            ("zj_axi_tx_rsp_fire", icn.tx.resp.get.fire),
            ("zj_axi_tx_rsp_stall", icn.tx.resp.get.valid && !icn.tx.resp.get.ready),
            ("zj_axi_tx_dat_fire", icn.tx.data.get.fire),
            ("zj_axi_tx_dat_stall", icn.tx.data.get.valid && !icn.tx.data.get.ready),
            ("zj_axi_busy_cm_sum", busyCmCount),
            ("zj_axi_busy_read_cm_sum", busyReadCount),
            ("zj_axi_busy_write_cm_sum", busyWriteCount),
            ("zj_axi_busy_cm_any_cycle", busyEntries.reduce(_ || _)),
            ("zj_axi_no_free_cm", noFreeCm),
            ("zj_axi_aw_fire", axi.aw.fire),
            ("zj_axi_aw_stall", axi.aw.valid && !axi.aw.ready),
            ("zj_axi_w_fire", axi.w.fire),
            ("zj_axi_w_stall", axi.w.valid && !axi.w.ready),
            ("zj_axi_ar_fire", axi.ar.fire),
            ("zj_axi_ar_stall", axi.ar.valid && !axi.ar.ready),
            ("zj_axi_r_fire", axi.r.fire),
            ("zj_axi_r_stall", axi.r.valid && !axi.r.ready),
            ("zj_axi_write_complete", writeCompleteCount),
            ("zj_axi_b_fire", axi.b.fire),
            ("zj_axi_b_stall", axi.b.valid && !axi.b.ready),
            ("zj_sn_id_map_read_complete_match", readCompleteMapMatch),
            ("zj_sn_id_map_read_complete_unknown", readCompleteMapUnknown),
            ("zj_sn_id_map_return_dat_match", returnDataMapMatch),
            ("zj_sn_id_map_return_dat_last", returnDataMapLast),
            ("zj_sn_id_map_return_dat_unknown", returnDataMapUnknown),
            ("zj_sn_id_map_return_dat_ambiguous", icn.tx.data.get.fire && PopCount(returnDataMapMatchVec) > 1.U),
            ("zj_sn_id_map_return_dat_source_mismatch", returnDataSourceMismatch),
            ("zj_sn_id_map_return_dat_dbid_mismatch", returnDataDbidMismatch),
            ("zj_sn_id_map_return_dat_target_mismatch", returnDataTargetMismatch),
            ("zj_sn_id_map_active_sum", idMapActiveCount),
            ("zj_sn_id_map_source_present", icn.rx.req.get.fire && req.SrcID.orR),
            ("zj_sn_id_map_return_txn_present", icn.rx.req.get.fire && req.ReturnTxnID.get.orR),
            ("zj_sn_id_map_hn_txn_present", icn.rx.req.get.fire && req.TxnID.orR)
        ) ++ strictLatencyEvents
    )
    ZhuJiangPerf.max("zj_axi_busy_cm_max", busyCmCount, true.B)
    MbistPipeline.PlaceMbistPipeline(1, "MbistPipelineSn", hasMbist)
}
