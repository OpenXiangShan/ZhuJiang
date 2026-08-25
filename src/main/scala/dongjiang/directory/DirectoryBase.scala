package dongjiang.directory

import math._
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import zhujiang.chi._
import dongjiang._
import dongjiang.utils._
import dongjiang.bundle._
import xs.utils.debug.{HAssert, HardwareAssertion}
import zhujiang.perf.HomeWrapperPerf
import xs.utils.sram.{DualPortSramTemplate, SinglePortSramTemplate}
import freechips.rocketchip.util.ReplacementPolicy
import xs.utils.mbist.MbistPipeline
import chisel3.experimental.BundleLiterals._
import zhujiang.utils.SramPwrCtlBoring
import dongjiang.directory.DIR._

object DIR {

    val D0 = 4
    val D1 = 3
    val D2 = 2
    val D3 = 1
    val D4 = 0
}

class Shift(implicit p: Parameters) extends DJBundle {

    val read  = UInt(readDirLatency.W)
    val write = UInt(readDirLatency.W)
    val repl  = UInt(readDirLatency.W)

    def recRead_d0(fire: Bool) = this.read := Cat(fire, read >> 1)
    def recRepl_d0(fire: Bool) = this.repl := Cat(fire, repl >> 1)
    def recWri_d0(fire: Bool) = this.write := Cat(fire, write >> 1)

    def wriUpdRepl_d3 = write(D3).asBool & !repl(D3).asBool
    def updTagMeta_d3 = read(D3).asBool & repl(D3).asBool

    def outDirResp_d4 = read(D4).asBool
    def updTagMeta_d4 = read(D4).asBool & repl(D4).asBool
    def wriUpdRepl_d4 = write(D4).asBool & !repl(D4).asBool

    private val hi = readDirLatency - 1
    private val lo = readDirLatency - (dirMuticycle - 1)
    def req = read | write
    def tagMetaReady = if (dirMuticycle > 1) !req(hi, lo).orR else true.B
    def replWillWrite = (repl & read).orR

}

class DirectoryBase(dirType: String, powerCtl: Boolean)(implicit p: Parameters) extends DJModule {
    override val desiredName = s"Directory${dirType.toUpperCase}"
    val param                = new DirParam(dirType)
    val repl                 = ReplacementPolicy.fromString("plru", param.ways)

    val io = IO(new Bundle {
        val config  = Input(new DJConfigIO())
        val dirBank = Input(UInt(dirBankBits.W))
        val read    = Flipped(Decoupled(new Addr(dirType) with HasPackHnIdx))
        val write   = Flipped(Decoupled(new DirEntry(dirType) with HasPackHnIdx with HasDirectAlloc))
        val resp    = Valid(new DirEntry(dirType) with HasHnTxnID { val toRepl = Bool() })
        val unlock  = Flipped(Valid(new PackHnIdx))
    })

    require(readDirLatency == 4)
    require(dirMuticycle == 2)

    val metaArray = Module(
        new SinglePortSramTemplate(
            gen = Vec(param.nrMetas, new ChiState(dirType)),
            set = param.sets,
            way = param.ways,
            shouldReset = true,
            setup = djparam.dirRamSetup,
            latency = djparam.dirRamLatency,
            extraHold = djparam.dirRamExtraHold,
            outputReg = true,
            suffix = s"_${dirType}_meta",
            hasMbist = hasMbist,
            powerCtl = powerCtl
        )
    )
    SramPwrCtlBoring.addSink(metaArray.io.pwctl)

    val tagArray = Module(
        new SinglePortSramTemplate(
            gen = UInt(param.tagBits.W),
            set = param.sets,
            way = param.ways,
            shouldReset = false,
            setup = djparam.dirRamSetup,
            latency = djparam.dirRamLatency,
            extraHold = djparam.dirRamExtraHold,
            suffix = s"_${dirType}_tag",
            outputReg = true,
            hasMbist = hasMbist,
            powerCtl = powerCtl
        )
    )
    SramPwrCtlBoring.addSink(tagArray.io.pwctl)

    val replArray = Module(
        new DualPortSramTemplate(
            gen = UInt(repl.nBits.W),
            set = param.sets,
            way = 1,
            shouldReset = true,
            bypassWrite = true,
            suffix = s"_${dirType}_repl",
            outputReg = true,
            hasMbist = hasMbist,
            powerCtl = powerCtl
        )
    )
    SramPwrCtlBoring.addSink(replArray.io.pwctl)
    MbistPipeline.PlaceMbistPipeline(1, desiredName, hasMbist)

    dontTouch(metaArray.io)
    dontTouch(tagArray.io)
    dontTouch(replArray.io)

    class OwnerWayEntry extends DJBundle {
        val valid = Bool()
        val set   = UInt(param.setBits.W)
        val way   = UInt(param.wayBits.W)
    }

    val lockWays = if (dirType == "llc") posWays - 1 else posWays - 2
    val lockTable = RegInit(VecInit(Seq.fill(posSets) {
        VecInit(Seq.fill(lockWays) {
            (new OwnerWayEntry).Lit(_.valid -> false.B)
        })
    }))
    val lockNext = WireInit(lockTable)
    val reservationTable = if (dirType == "sf") {
        RegInit(VecInit(Seq.fill(posSets) {
            VecInit(Seq.fill(lockWays) {
                (new OwnerWayEntry).Lit(_.valid -> false.B)
            })
        }))
    } else {
        WireInit(VecInit(Seq.fill(posSets) {
            VecInit(Seq.fill(lockWays) {
                0.U.asTypeOf(new OwnerWayEntry)
            })
        }))
    }
    val reservationNext = WireInit(reservationTable)

    val shiftReg = RegInit(0.U.asTypeOf(new Shift))

    val resetDoneReg = RegEnable(true.B, false.B, metaArray.io.req.ready & replArray.io.rreq.ready & replArray.io.wreq.ready)
    HardwareAssertion.withEn(!(metaArray.io.req.ready ^ io.write.ready), resetDoneReg & !shiftReg.replWillWrite)

    val reqSftReg = Reg(
        Vec(
            readDirLatency,
            new DJBundle with HasAddr with HasPackHnIdx {
                override def addrType: String = dirType
                val metaVec  = Vec(param.nrMetas, new ChiState(dirType))
                val wriWayOH = UInt(param.ways.W)
            }
        )
    )
    when(io.write.fire | io.read.fire) {
        reqSftReg.last.addr     := Mux(io.write.valid, io.write.bits.addr, io.read.bits.addr)
        reqSftReg.last.hnIdx    := Mux(io.write.valid, io.write.bits.hnIdx, io.read.bits.hnIdx)
        reqSftReg.last.wriWayOH := Mux(io.write.valid, io.write.bits.wayOH, 0.U)
        reqSftReg.last.metaVec  := Mux(io.write.valid, io.write.bits.metaVec, 0.U.asTypeOf(reqSftReg.last.metaVec))
    }
    reqSftReg.zipWithIndex.foreach { case (sft, i) =>
        if (i > 0) { when(shiftReg.req.orR | io.read.fire | io.write.fire) { reqSftReg(i - 1) := sft } }
    }

    val replMes_d2   = Wire(UInt(repl.nBits.W))
    val useWayVec_d2 = Wire(UInt(param.ways.W))
    dontTouch(useWayVec_d2)

    val tagResp_d3  = tagArray.io.resp.bits.data
    val metaResp_d3 = metaArray.io.resp.bits.data

    val req_d3        = reqSftReg(D3)
    val replMesReg_d3 = RegEnable(replMes_d2, shiftReg.req(D2))
    val addrVec_d3 = WireInit(VecInit(Seq.fill(param.ways) {
        0.U.asTypeOf(new DJBundle with HasAddr {
            override def addrType: String = dirType
        })
    }))
    val reqTag_d3     = Wire(UInt(param.tagBits.W)); dontTouch(reqTag_d3)
    val reqSet_d3     = Wire(UInt(param.setBits.W)); dontTouch(reqSet_d3)
    val tagHitVec_d3  = Wire(Vec(param.ways, Bool())); dontTouch(tagHitVec_d3)
    val metaValVec_d3 = Wire(Vec(param.ways, Bool())); dontTouch(metaValVec_d3)
    val invalidVec_d3 = Wire(Vec(param.ways, Bool())); dontTouch(invalidVec_d3)

    val readHit_d3    = WireInit(false.B)
    val selWayOH_d3   = WireInit(0.U(param.ways.W))
    val newReplMes_d3 = WireInit(0.U(repl.nBits.W))
    val resp_d3       = Wire(chiselTypeOf(io.resp.bits))

    val pendingAllocValid_d3 = WireInit(false.B)

    val req_d4           = reqSftReg.head
    val readHitReg_d4    = RegEnable(readHit_d3, shiftReg.req(D3))
    val selWayOHReg_d4   = RegEnable(selWayOH_d3, shiftReg.req(D3))
    val newReplMesReg_d4 = RegEnable(newReplMes_d3, shiftReg.req(D3))
    val respReg_d4       = RegEnable(resp_d3, shiftReg.req(D3))

    val ownerWayInRange_d0 = io.write.bits.hnIdx.pos.way < lockWays.U
    val ownerWayIdx_d0     = io.write.bits.hnIdx.pos.way(log2Ceil(lockWays) - 1, 0)
    val ownerReservation_d0 = WireInit(0.U.asTypeOf(chiselTypeOf(reservationTable.head.head)))
    val directAllocOwnerMatch_d0 = WireInit(false.B)
    if (dirType == "sf") {
        when(ownerWayInRange_d0) {
            ownerReservation_d0 := reservationTable(io.write.bits.hnIdx.pos.set)(ownerWayIdx_d0)
        }
        directAllocOwnerMatch_d0 := ownerWayInRange_d0 &&
            io.write.bits.hnIdx.dirBank === io.dirBank &&
            ownerReservation_d0.valid &&
            ownerReservation_d0.set === io.write.bits.Addr.set &&
            UIntToOH(ownerReservation_d0.way) === io.write.bits.wayOH
    }

    val writeDirectAlloc_d0 = io.write.fire & io.write.bits.directAlloc
    val writeHit_d0         = io.write.fire & io.write.bits.hit
    val wriNoHit_d0         = io.write.fire & !io.write.bits.hit & !io.write.bits.directAlloc
    val writeAny_d0         = writeHit_d0 | writeDirectAlloc_d0 | wriNoHit_d0
    val read_d0             = io.read.fire
    val repl_d0             = shiftReg.updTagMeta_d4

    val reqSet_d0 = Mux(repl_d0, req_d4.Addr.set, Mux(io.write.valid, io.write.bits.Addr.set, io.read.bits.Addr.set))

    val wriMask_d0    = Mux(repl_d0, selWayOHReg_d4, io.write.bits.wayOH)
    val wriMetaVec_d0 = Mux(repl_d0, req_d4.metaVec, io.write.bits.metaVec)

    metaArray.io.req.valid         := (writeAny_d0 | read_d0 | repl_d0) & resetDoneReg
    metaArray.io.req.bits.addr     := reqSet_d0
    metaArray.io.req.bits.write    := writeHit_d0 | writeDirectAlloc_d0 | repl_d0
    metaArray.io.req.bits.mask.get := wriMask_d0
    metaArray.io.req.bits.data.foreach(_ := wriMetaVec_d0)
    HardwareAssertion.withEn(metaArray.io.req.ready, metaArray.io.req.valid)
    HardwareAssertion.withEn(metaArray.io.req.bits.mask.get =/= 0.U, metaArray.io.req.valid & metaArray.io.req.bits.write)

    tagArray.io.req.valid         := (writeDirectAlloc_d0 | wriNoHit_d0 | read_d0 | repl_d0) & resetDoneReg
    tagArray.io.req.bits.addr     := reqSet_d0
    tagArray.io.req.bits.write    := writeDirectAlloc_d0 | repl_d0
    tagArray.io.req.bits.mask.get := Mux(writeDirectAlloc_d0, io.write.bits.wayOH, selWayOHReg_d4)
    tagArray.io.req.bits.data.foreach(_ := Mux(writeDirectAlloc_d0, io.write.bits.Addr.tag, req_d4.Addr.tag))
    HardwareAssertion.withEn(tagArray.io.req.ready, tagArray.io.req.valid)
    HardwareAssertion.withEn(tagArray.io.req.bits.mask.get =/= 0.U, tagArray.io.req.valid & tagArray.io.req.bits.write)

    shiftReg.recRead_d0(metaArray.io.req.fire & !metaArray.io.req.bits.write)
    shiftReg.recWri_d0(metaArray.io.req.fire & metaArray.io.req.bits.write)
    shiftReg.recRepl_d0(metaArray.io.req.fire & (wriNoHit_d0 | repl_d0))
    HardwareAssertion(!(shiftReg.read & shiftReg.write).orR)
    HardwareAssertion.withEn((shiftReg.repl & shiftReg.req).orR, shiftReg.repl.orR)

    io.read.ready  := resetDoneReg & shiftReg.tagMetaReady & !shiftReg.replWillWrite & !io.write.valid
    io.write.ready := resetDoneReg & shiftReg.tagMetaReady & !shiftReg.replWillWrite
    HardwareAssertion.withEn(metaArray.io.req.ready, shiftReg.updTagMeta_d4)
    HardwareAssertion.withEn(tagArray.io.req.ready, shiftReg.updTagMeta_d4)

    replArray.io.rreq.valid := (writeAny_d0 | read_d0) & resetDoneReg
    replArray.io.rreq.bits  := Mux(io.write.valid, io.write.bits.Addr.set, io.read.bits.Addr.set)

    replArray.io.wreq.valid        := shiftReg.wriUpdRepl_d4 | shiftReg.updTagMeta_d4 | (shiftReg.outDirResp_d4 & readHitReg_d4)
    replArray.io.wreq.bits.addr    := req_d4.Addr.set
    replArray.io.wreq.bits.data(0) := newReplMesReg_d4
    HardwareAssertion.withEn(replArray.io.rreq.ready, replArray.io.rreq.valid)
    HardwareAssertion.withEn(replArray.io.wreq.ready, replArray.io.wreq.valid)

    val replSetMatch_d1_d4 = req_d4.Addr.set === reqSftReg(D1).Addr.set & replArray.io.wreq.fire
    val replSetMatch_d2_d4 = req_d4.Addr.set === reqSftReg(D2).Addr.set & replArray.io.wreq.fire
    replMes_d2 := PriorityMux(
        Seq(
            replSetMatch_d2_d4          -> newReplMesReg_d4,
            RegNext(replSetMatch_d1_d4) -> RegEnable(newReplMesReg_d4, replSetMatch_d1_d4),
            true.B                      -> replArray.io.rresp.bits(0)
        )
    )
    val replRespNeedVal = shiftReg.read(D2) | (shiftReg.write(D2) & !shiftReg.repl(D2))
    HAssert(!(replRespNeedVal ^ replArray.io.rresp.valid))

    val registeredLockWayVec_d2 = lockTable.flatten
        .map(lock => Mux(lock.valid & lock.set === reqSftReg(D2).Addr.set, UIntToOH(lock.way), 0.U))
        .reduce(_ | _)
    val registeredReservationWayVec_d2 = reservationTable.flatten
        .map(reservation => Mux(reservation.valid & reservation.set === reqSftReg(D2).Addr.set, UIntToOH(reservation.way), 0.U))
        .reduce(_ | _)
    val pendingAllocSetMatch_d2 = pendingAllocValid_d3 && reqSet_d3 === reqSftReg(D2).Addr.set
    val pendingAllocWayOH_d2    = Mux(pendingAllocSetMatch_d2, selWayOH_d3, 0.U)
    useWayVec_d2 := registeredLockWayVec_d2 | registeredReservationWayVec_d2 | pendingAllocWayOH_d2
    val replWay_d2     = repl.get_replace_way(replMes_d2)
    val unuseWay_d2    = PriorityEncoder(~useWayVec_d2.asUInt)
    val selIsUsing_d2  = useWayVec_d2(replWay_d2)
    val hasUnuseWay_d2 = PopCount(useWayVec_d2) < param.ways.U

    addrVec_d3.zip(tagResp_d3).foreach { case (addr, tag) => addr.Addr.cat(io.config.bankId, tag, reqSet_d3, io.dirBank) }
    reqTag_d3 := req_d3.Addr.tag
    reqSet_d3 := req_d3.Addr.set
    HAssert(!(shiftReg.read(D3) ^ tagArray.io.resp.valid))
    HAssert(!(shiftReg.read(D3) ^ metaArray.io.resp.valid))

    val useWayVecReg_d3 = RegEnable(useWayVec_d2, shiftReg.req(D2))
    tagHitVec_d3  := addrVec_d3.map(_.Addr.tag === reqTag_d3)
    metaValVec_d3 := metaResp_d3.map(meta => Cat(meta.map(_.isValid)).orR)
    invalidVec_d3 := metaValVec_d3.zip(useWayVecReg_d3.asBools).map { case (m, u) => !m & !u }
    val hasInvalid_d3 = invalidVec_d3.reduce(_ | _)
    val hitVec_d3     = tagHitVec_d3.zip(metaValVec_d3).map { case (a, b) => a & b }
    val hit_d3        = hitVec_d3.reduce(_ | _)
    readHit_d3 := shiftReg.read(D3) & hit_d3
    HardwareAssertion.withEn(!hit_d3, shiftReg.updTagMeta_d3)
    HardwareAssertion.withEn(PopCount(hitVec_d3) <= 1.U, shiftReg.read(D3))

    val selIsUsingReg_d3 = RegEnable(selIsUsing_d2, shiftReg.req(D2))
    val unuseWayReg_d3   = RegEnable(unuseWay_d2, shiftReg.req(D2))
    val replWayReg_d3    = RegEnable(replWay_d2, shiftReg.req(D2))
    val hitWay_d3        = PriorityEncoder(hitVec_d3)
    val invWay_d3        = PriorityEncoder(invalidVec_d3)
    val selWay_d3 = PriorityMux(
        Seq(
            hit_d3           -> hitWay_d3,
            hasInvalid_d3    -> invWay_d3,
            selIsUsingReg_d3 -> unuseWayReg_d3,
            true.B           -> replWayReg_d3
        )
    )
    dontTouch(selIsUsingReg_d3)
    dontTouch(unuseWayReg_d3)
    dontTouch(replWayReg_d3)
    dontTouch(hit_d3)
    dontTouch(hasInvalid_d3)
    dontTouch(hitWay_d3)
    dontTouch(invWay_d3)
    HardwareAssertion.withEn(RegNext(hasUnuseWay_d2), !hit_d3 & shiftReg.read(D3))

    selWayOH_d3     := UIntToOH(selWay_d3)
    resp_d3.addr    := addrVec_d3(selWay_d3).addr
    resp_d3.wayOH   := selWayOH_d3
    resp_d3.hit     := hit_d3
    resp_d3.metaVec := metaResp_d3(selWay_d3)
    resp_d3.hnTxnID := req_d3.hnIdx.getTxnID
    resp_d3.toRepl  := shiftReg.repl(D3)

    newReplMes_d3 := repl.get_next_state(replMesReg_d3, OHToUInt(Mux(shiftReg.wriUpdRepl_d3, req_d3.wriWayOH, selWayOH_d3)))

    val read_d3     = shiftReg.read(D3) & !shiftReg.write(D3) & !shiftReg.repl(D3)
    val write_d3    = !shiftReg.read(D3) & shiftReg.write(D3) & !shiftReg.repl(D3)
    val readRepl_d3 = shiftReg.read(D3) & !shiftReg.write(D3) & shiftReg.repl(D3)
    val wriRepl_d3  = !shiftReg.read(D3) & shiftReg.write(D3) & shiftReg.repl(D3)
    if (dirType == "sf") {
        pendingAllocValid_d3 := read_d3 & !hit_d3 & hasInvalid_d3
    }
    val unLockHitVec2 = Wire(Vec(posSets, Vec(lockWays, Bool())))
    val reqHitVec2    = Wire(Vec(posSets, Vec(lockWays, Bool())))
    lockTable.zipWithIndex.foreach { case (lockSet, i) =>
        lockSet.zipWithIndex.foreach { case (lock, j) =>
            val hnIdx = Wire(new HnIndex)
            hnIdx.dirBank := io.dirBank
            hnIdx.pos.set := i.U
            hnIdx.pos.way := j.U

            val unLockHit = io.unlock.valid & io.unlock.bits.hnIdx.asUInt === hnIdx.asUInt
            val reqHit    = shiftReg.req(D3) & req_d3.hnIdx.asUInt === hnIdx.asUInt
            val readMiss  = read_d3 & reqHit & !hit_d3
            val readHit   = read_d3 & reqHit & hit_d3
            val write     = write_d3 & reqHit
            val readRepl  = readRepl_d3 & reqHit
            val wriRepl   = wriRepl_d3 & reqHit

            unLockHitVec2(i)(j) := unLockHit
            reqHitVec2(i)(j)    := reqHit

            reqHit.suggestName(f"reqHit_${i}_${j}")
            readMiss.suggestName(f"write_${i}_${j}")
            readHit.suggestName(f"readRepl_${i}_${j}")
            write.suggestName(f"read_${i}_${j}")
            readRepl.suggestName(f"wriRepl_${i}_${j}")
            wriRepl.suggestName(f"unLockHit_${i}_${j}")

            val dontCare = Cat(lock.valid, lock.valid)
            val state = PriorityMux(
                Seq(
                    readMiss -> "b00".U,
                    readHit  -> "b01".U,
                    write    -> dontCare,
                    readRepl -> "b01".U,
                    wriRepl  -> dontCare,
                    true.B   -> dontCare
                )
            )

            val (oldLock, newLock) = (state(1), state(0))
            oldLock.suggestName(f"oldLock_${i}_${j}")
            newLock.suggestName(f"newLock_${i}_${j}")
            when(unLockHit) {
                lockNext(i)(j).valid := false.B
            }.elsewhen(reqHit & (!oldLock & newLock)) {
                lockNext(i)(j).valid := true.B
                lockNext(i)(j).set   := reqSet_d3
                lockNext(i)(j).way   := selWay_d3
            }

            HAssert(!(readMiss & readHit & readRepl & unLockHit), cf"Lock Table Index[${i.U}][${j.U}]")
            HAssert(lock.valid === oldLock, cf"Lock Table Index[${i.U}][${j.U}]")
            HAssert.checkTimeout(!lock.valid, TIMEOUT_LOCK, cf"TIMEOUT: Directory Lock Index[${i.U}][${j.U}]")
        }
    }
    HAssert.withEn(PopCount(unLockHitVec2.flatten) === 1.U, io.unlock.valid & io.unlock.bits.hnIdx.dirBank === io.dirBank & (io.unlock.bits.hnIdx.pos.way < lockWays.U))
    HAssert.withEn(PopCount(reqHitVec2.flatten) === 1.U, shiftReg.req(D3))

    if (dirType == "sf") {
        reservationTable.zipWithIndex.foreach { case (reservationSet, i) =>
            reservationSet.zipWithIndex.foreach { case (_, j) =>
                val hnIdx = Wire(new HnIndex)
                hnIdx.dirBank := io.dirBank
                hnIdx.pos.set := i.U
                hnIdx.pos.way := j.U

                val clearReservationByWrite = writeDirectAlloc_d0 && io.write.bits.hnIdx.asUInt === hnIdx.asUInt
                val clearReservationByUnlock = io.unlock.valid && io.unlock.bits.hnIdx.asUInt === hnIdx.asUInt
                val reserveInvalidSfMiss = pendingAllocValid_d3 && req_d3.hnIdx.asUInt === hnIdx.asUInt

                when(clearReservationByWrite || clearReservationByUnlock) {
                    reservationNext(i)(j).valid := false.B
                }.elsewhen(reserveInvalidSfMiss) {
                    reservationNext(i)(j).valid := true.B
                    reservationNext(i)(j).set   := reqSet_d3
                    reservationNext(i)(j).way   := selWay_d3
                }
            }
        }

        val newReservationCount_d3 = PopCount(reservationNext.flatten.map(reservation =>
            reservation.valid &
                (reservation.set === reqSet_d3) &
                (reservation.way === selWay_d3)
        ))
        HAssert.withEn(newReservationCount_d3 === 1.U, pendingAllocValid_d3, "Duplicate SF reservation")

        when(pendingAllocValid_d3 || writeDirectAlloc_d0 || io.unlock.valid) {
            reservationTable := reservationNext
        }
    }

    when(shiftReg.req(D3) | io.unlock.valid) {
        lockTable := lockNext
    }

    io.resp.valid := shiftReg.outDirResp_d4
    io.resp.bits  := respReg_d4

    val respToRepl               = io.resp.valid && io.resp.bits.toRepl
    val respToReplVictimValid    = respToRepl && io.resp.bits.metaVec.map(_.isValid).reduce(_ | _)
    val respToReplVictimInvalid  = respToRepl && !io.resp.bits.metaVec.map(_.isValid).reduce(_ | _)
    val readReplVictimValid_d3   = readRepl_d3 && resp_d3.metaVec.map(_.isValid).reduce(_ | _)
    val readReplVictimInvalid_d3 = readRepl_d3 && !resp_d3.metaVec.map(_.isValid).reduce(_ | _)

    HAssert.withEn(!io.write.bits.hit, io.write.valid && io.write.bits.directAlloc)
    HAssert.withEn(io.write.bits.metaIsVal, io.write.valid && io.write.bits.directAlloc)
    if (dirType == "sf") {
        HAssert.withEn(directAllocOwnerMatch_d0, io.write.valid && io.write.bits.directAlloc)
    }

    HomeWrapperPerf.accumulate(
        Seq(
            ("zj_dirbase_read_valid", io.read.valid),
            ("zj_dirbase_read_fire", io.read.fire),
            ("zj_dirbase_read_stall", io.read.valid && !io.read.ready),
            ("zj_dirbase_read_stall_reset", io.read.valid && !io.read.ready && !resetDoneReg),
            ("zj_dirbase_read_stall_tag_meta", io.read.valid && !io.read.ready && !shiftReg.tagMetaReady),
            ("zj_dirbase_read_stall_repl_will_write", io.read.valid && !io.read.ready && shiftReg.replWillWrite),
            ("zj_dirbase_read_stall_write_valid", io.read.valid && !io.read.ready && io.write.valid),
            ("zj_dirbase_write_valid", io.write.valid),
            ("zj_dirbase_write_fire", io.write.fire),
            ("zj_dirbase_write_stall", io.write.valid && !io.write.ready),
            ("zj_dirbase_write_stall_reset", io.write.valid && !io.write.ready && !resetDoneReg),
            ("zj_dirbase_write_stall_tag_meta", io.write.valid && !io.write.ready && !shiftReg.tagMetaReady),
            ("zj_dirbase_write_stall_repl_will_write", io.write.valid && !io.write.ready && shiftReg.replWillWrite),
            ("zj_dirbase_tag_meta_not_ready_cycle", !shiftReg.tagMetaReady),
            ("zj_dirbase_repl_will_write_cycle", shiftReg.replWillWrite),
            ("zj_dirbase_shift_read_cycle", shiftReg.read.orR),
            ("zj_dirbase_shift_write_cycle", shiftReg.write.orR),
            ("zj_dirbase_shift_repl_cycle", shiftReg.repl.orR),
            ("zj_dirbase_resp_valid", io.resp.valid),
            (s"zj_dirbase_${dirType}_resp_to_repl", respToRepl),
            (s"zj_dirbase_${dirType}_resp_to_repl_victim_valid", respToReplVictimValid),
            (s"zj_dirbase_${dirType}_resp_to_repl_victim_invalid", respToReplVictimInvalid),
            ("zj_dirbase_unlock_valid", io.unlock.valid),
            ("zj_dirbase_d3_read_miss", read_d3 && !hit_d3),
            ("zj_dirbase_d3_read_hit", read_d3 && hit_d3),
            (s"zj_dirbase_${dirType}_d3_read_repl", readRepl_d3),
            (s"zj_dirbase_${dirType}_d3_read_repl_victim_valid", readReplVictimValid_d3),
            (s"zj_dirbase_${dirType}_d3_read_repl_victim_invalid", readReplVictimInvalid_d3),
            (s"zj_dirbase_${dirType}_d2_sel_is_using", shiftReg.req(D2) && selIsUsing_d2),
            (s"zj_dirbase_${dirType}_pending_d3_conflict", shiftReg.req(D2) && pendingAllocSetMatch_d2),
            (s"zj_dirbase_${dirType}_invalid_miss_reserve", pendingAllocValid_d3),
            (s"zj_dirbase_${dirType}_d3_read_repl_has_invalid", readRepl_d3 && hasInvalid_d3),
            (s"zj_dirbase_${dirType}_d3_read_repl_no_invalid", readRepl_d3 && !hasInvalid_d3),
            (s"zj_dirbase_${dirType}_direct_alloc_fire", writeDirectAlloc_d0),
            (s"zj_dirbase_${dirType}_direct_alloc_owner_match", writeDirectAlloc_d0 && directAllocOwnerMatch_d0),
            ("zj_dirbase_d3_write", write_d3),
            ("zj_dirbase_d3_wri_repl", wriRepl_d3)
        )
    )

    HardwareAssertion.placePipe(1)
}
