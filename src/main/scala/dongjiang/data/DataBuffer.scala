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
import xs.utils.mbist.MbistPipeline
import xs.utils.queue.FastQueue
import xs.utils.sram.DualPortSramTemplate
import zhujiang.utils.SramPwrCtlBoring
import zhujiang.perf.{HistogramRange, ZJPerf}
import zhujiang.chi.DatOpcode._
import zhujiang.chi.RspOpcode.Comp
import chisel3.experimental.BundleLiterals._

class DataBuffer(powerCtl: Boolean)(implicit p: Parameters) extends DJModule {

    val io = IO(new Bundle {

        val readToCHI = Flipped(Decoupled(new ReadDB))
        val readToDS  = Flipped(Decoupled(new ReadDB))
        val clean     = Flipped(Valid(new DBIDVec with HasDataVec))

        val dsResp  = Flipped(Valid(new DsResp))
        val fromCHI = Flipped(Decoupled(new PackDataFilt with HasDBID))

        val writeDS = Decoupled(new WriteDS with HasBeatNum)
        val toCHI   = Decoupled(new PackDataFilt with HasDCID with HasBeatNum)
    })
    val FullMask = Fill(djparam.BeatByte, 1.U)

    val datBuf = Module(
        new DualPortSramTemplate(
            gen = UInt(8.W),
            set = djparam.nrDataBuf,
            way = djparam.BeatByte,
            hasMbist = hasMbist,
            suffix = "_llc_db",
            powerCtl = powerCtl
        )
    )
    SramPwrCtlBoring.addSink(datBuf.io.pwctl)

    val maskRegVec = Reg(Vec(djparam.nrDataBuf, UInt(djparam.BeatByte.W)))
    val replRegVec = Reg(Vec(djparam.nrDataBuf, Bool()))

    val rToCHISftReg = RegInit(0.U(2.W))
    val rToDSSftReg  = RegInit(0.U(2.W))
    rToCHISftReg := Cat(io.readToCHI.fire, rToCHISftReg(1))
    rToDSSftReg  := Cat(io.readToDS.fire, rToDSSftReg(1))
    HAssert(!(io.readToCHI.fire & io.readToDS.fire))

    val toDSQ  = Module(new FastQueue(new WriteDS, 2, false))
    val toCHIQ = Module(new FastQueue(new PackDataFilt with HasDCID with HasBeatNum, 2, false))

    val hasFreetoCHI = toCHIQ.io.freeNum > PopCount(rToCHISftReg)
    val hasFreetoDS  = toDSQ.io.freeNum > PopCount(rToDSSftReg)
    io.readToCHI.ready := hasFreetoCHI & !io.readToDS.valid
    io.readToDS.ready  := hasFreetoDS

    datBuf.io.rreq.valid := RegNext(io.readToCHI.fire | io.readToDS.fire, false.B)
    datBuf.io.rreq.bits  := RegEnable(Mux(io.readToDS.valid, io.readToDS.bits.dbid, io.readToCHI.bits.dbid), io.readToCHI.fire | io.readToDS.fire)

    replRegVec.zipWithIndex.foreach { case (r, i) =>
        val replHit  = io.readToDS.fire & io.readToDS.bits.repl & io.readToDS.bits.dbid === i.U
        val cleanHit = io.clean.valid & (io.clean.bits.dataVec.asUInt & VecInit(io.clean.bits.dbidVec.map(_ === i.U)).asUInt) =/= 0.U
        when(replHit) {
            r := true.B
            HAssert(!r)
        }.elsewhen(cleanHit) {
            r := false.B
        }
        HAssert(!(replHit & cleanHit))
    }

    io.fromCHI.ready := !io.dsResp.valid

    val wriVal       = io.dsResp.valid | io.fromCHI.valid
    val wDataVec     = Mux(io.dsResp.valid, io.dsResp.bits.beat, io.fromCHI.bits.dat.Data).asTypeOf(Vec(djparam.BeatByte, UInt(8.W)))
    val dbid         = Mux(io.dsResp.valid, io.dsResp.bits.dbid, io.fromCHI.bits.dbid)
    val dsWriReg     = RegNext(io.dsResp.valid)
    val replReg      = RegNext(replRegVec(dbid))
    val maskReg      = RegEnable(maskRegVec(dbid), wriVal)
    val fromChiBEReg = RegEnable(io.fromCHI.bits.dat.BE, io.fromCHI.valid)
    val fromChiOp    = io.fromCHI.bits.dat.Opcode
    val readOrSnpReg = RegNext(fromChiOp === SnpRespDataFwded | fromChiOp === SnpRespData | fromChiOp === CompData)

    datBuf.io.wreq.valid         := RegNext(wriVal)
    datBuf.io.wreq.bits.addr     := RegEnable(dbid, wriVal)
    datBuf.io.wreq.bits.mask.get := Mux(dsWriReg, Mux(replReg, FullMask, ~maskReg), Mux(readOrSnpReg, ~maskReg, fromChiBEReg))
    datBuf.io.wreq.bits.data.zip(wDataVec).foreach { case (a, b) => a := RegEnable(b, wriVal) }

    HAssert.withEn(
        fromChiOp === CompData |
            fromChiOp === SnpRespData |
            fromChiOp === SnpRespDataFwded |
            fromChiOp === NonCopyBackWriteData |
            fromChiOp === CopyBackWriteData |
            fromChiOp === NCBWrDataCompAck,
        io.fromCHI.fire
    )
    HAssert.withEn(io.fromCHI.bits.dat.BE === FullMask, io.fromCHI.fire & fromChiOp === SnpRespData)
    HAssert.withEn(io.fromCHI.bits.dat.BE === FullMask, io.fromCHI.fire & fromChiOp === SnpRespDataFwded)

    maskRegVec.zipWithIndex.foreach { case (m, i) =>
        val cleanHit = io.clean.valid & (io.clean.bits.dataVec.asUInt & VecInit(io.clean.bits.dbidVec.map(_ === i.U)).asUInt) =/= 0.U
        val dsHit    = io.dsResp.fire & io.dsResp.bits.dbid === i.U
        val chiHit   = io.fromCHI.fire & io.fromCHI.bits.dbid === i.U
        when(cleanHit) {
            m := 0.U
        }.elsewhen(dsHit) {
            m := FullMask
        }.elsewhen(chiHit) {
            m := Mux(fromChiOp === CompData | fromChiOp === SnpRespData | fromChiOp === SnpRespDataFwded, FullMask, m | io.fromCHI.bits.dat.BE)
        }
        HAssert(PopCount(Seq(cleanHit, dsHit, chiHit)) <= 1.U)
    }

    toCHIQ.io.enq.valid := rToCHISftReg(0)
    val toChiDBID    = RegEnable(RegEnable(io.readToCHI.bits.dbid, io.readToCHI.fire), rToCHISftReg(1))
    val toChiBeatNum = RegEnable(RegEnable(io.readToCHI.bits.beatNum, io.readToCHI.fire), rToCHISftReg(1))
    toCHIQ.io.enq.bits.dcid       := RegEnable(RegEnable(io.readToCHI.bits.dcid, io.readToCHI.fire), rToCHISftReg(1))
    toCHIQ.io.enq.bits.beatNum    := toChiBeatNum
    toCHIQ.io.enq.bits.dat        := DontCare
    toCHIQ.io.enq.bits.dat.BE     := maskRegVec(toChiDBID)
    toCHIQ.io.enq.bits.dat.Data   := datBuf.io.rresp.bits.asUInt
    toCHIQ.io.enq.bits.dat.DataID := Cat(toChiBeatNum, 0.U(1.W)); require(djparam.nrBeat == 2)
    HAssert.withEn(toCHIQ.io.enq.ready, toCHIQ.io.enq.valid)
    HAssert.withEn(datBuf.io.rresp.valid, toCHIQ.io.enq.valid)

    toDSQ.io.enq.valid        := rToDSSftReg(0)
    toDSQ.io.enq.bits.dcid    := RegEnable(RegEnable(io.readToDS.bits.dcid, io.readToDS.fire), rToDSSftReg(1))
    toDSQ.io.enq.bits.ds      := RegEnable(RegEnable(io.readToDS.bits.ds, io.readToDS.fire), rToDSSftReg(1))
    toDSQ.io.enq.bits.beatNum := RegEnable(RegEnable(io.readToDS.bits.beatNum, io.readToDS.fire), rToDSSftReg(1))
    toDSQ.io.enq.bits.beat    := datBuf.io.rresp.bits.asUInt
    HAssert.withEn(toDSQ.io.enq.ready, toDSQ.io.enq.valid)
    HAssert.withEn(datBuf.io.rresp.valid, toDSQ.io.enq.valid)

    io.toCHI   <> toCHIQ.io.deq
    io.writeDS <> toDSQ.io.deq

    ZJPerf.whenEnabled {
        val toCHIOccupancy = 2.U - toCHIQ.io.freeNum
        val toDSOccupancy  = 2.U - toDSQ.io.freeNum
        val readToCHIQueueStall = io.readToCHI.valid && !hasFreetoCHI
        val readToCHIArbStall = io.readToCHI.valid && hasFreetoCHI && io.readToDS.valid
        val readToDSQueueStall = io.readToDS.valid && !hasFreetoDS
        val fromCHIStall = io.fromCHI.valid && !io.fromCHI.ready
        val toCHIStall = io.toCHI.valid && !io.toCHI.ready
        val writeDSStall = io.writeDS.valid && !io.writeDS.ready

        ZJPerf.accumulate(
            Seq(
                ("zj_databuffer_read_to_chi_queue_stall", readToCHIQueueStall),
                ("zj_databuffer_read_to_chi_arb_stall", readToCHIArbStall),
                ("zj_databuffer_read_to_ds_queue_stall", readToDSQueueStall),
                ("zj_databuffer_from_chi_stall", fromCHIStall),
                ("zj_databuffer_to_chi_stall", toCHIStall),
                ("zj_databuffer_write_ds_stall", writeDSStall),
                ("zj_databuffer_to_chi_full", toCHIQ.io.freeNum === 0.U),
                ("zj_databuffer_to_ds_full", toDSQ.io.freeNum === 0.U)
            )
        )
        ZJPerf.distribution(
            "zj_databuffer_to_chi_occupancy",
            toCHIOccupancy,
            true.B,
            HistogramRange.occupancy(2),
            includeMinMax = true
        )
        ZJPerf.distribution(
            "zj_databuffer_to_ds_occupancy",
            toDSOccupancy,
            true.B,
            HistogramRange.occupancy(2),
            includeMinMax = true
        )
    }

    HardwareAssertion.placePipe(1)
}
