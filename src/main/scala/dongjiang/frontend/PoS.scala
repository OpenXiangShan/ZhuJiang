package dongjiang.frontend

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import zhujiang.chi._
import dongjiang._
import dongjiang.utils._
import dongjiang.bundle._
import xs.utils.debug._
import zhujiang.perf.HomeWrapperPerf
import chisel3.experimental.BundleLiterals._

class PosState(implicit p: Parameters) extends DJBundle {
    val req     = Bool()
    val snp     = Bool()
    val canNest = if (hasBBN) Some(Bool()) else None
    val tagVal  = Bool()
    val tag     = UInt(posTagBits.W)
    val offset  = UInt(offsetBits.W)

    def one: Bool = req ^ snp
    def two: Bool = req & snp
    def valid: Bool = req | snp
}

class PosCanNest(implicit p: Parameters) extends DJBundle with HasPackHnIdx with HasQoS { val nest = Bool() }

class PosClean(implicit p: Parameters) extends DJBundle with HasPackHnIdx with HasChiChannel with HasQoS

class PosEntry(implicit p: Parameters) extends DJModule {

    val io = IO(new Bundle {
        val config = new DJConfigIO()
        val hnIdx  = Input(new HnIndex())

        val alloc   = Flipped(Valid(new Addr with HasAddrValid with HasChiChannel))
        val updTag  = Flipped(Valid(new Addr with HasAddrValid with HasPackHnIdx))
        val updNest = if (hasBBN) Some(Flipped(Valid(new PosCanNest))) else None
        val clean   = Flipped(Valid(new PosClean))

        val wakeup = Valid(new Addr)

        val state = Output(new PosState with HasAddr)
    })
    dontTouch(io.state)

    val stateReg  = RegInit(new PosState().Lit(_.req -> false.B, _.snp -> false.B))
    val stateNext = WireInit(stateReg)
    dontTouch(stateNext)

    val updTagHit  = io.updTag.valid & io.updTag.bits.hnIdx.asUInt === io.hnIdx.asUInt
    val cleanHit   = io.clean.valid & io.clean.bits.hnIdx.asUInt === io.hnIdx.asUInt
    val canNestHit = if (hasBBN) io.updNest.get.valid & io.updNest.get.bits.hnIdx.asUInt === io.hnIdx.asUInt else false.B
    dontTouch(cleanHit)

    when(io.alloc.valid | updTagHit | cleanHit | canNestHit) {
        stateReg := stateNext
    }

    io.state.req                     := stateReg.req
    io.state.snp                     := stateReg.snp
    io.state.tagVal                  := stateReg.tagVal
    io.state.tag                     := stateReg.tag
    io.state.offset                  := stateReg.offset
    if (hasBBN) io.state.canNest.get := stateReg.canNest.get
    io.state.Addr.catPoS(io.config.bankId, stateReg.tag, io.hnIdx.pos.set, io.hnIdx.dirBank, stateReg.offset)
    HAssert.withEn(stateReg.offset === 0.U, io.state.snp)

    when(io.alloc.valid) {
        stateNext.tagVal := io.alloc.bits.addrVal
        stateNext.tag    := io.alloc.bits.Addr.posTag
        stateNext.offset := io.alloc.bits.Addr.offset
    }.elsewhen(updTagHit) {
        stateNext.tagVal := io.updTag.bits.addrVal
        stateNext.tag    := io.updTag.bits.Addr.posTag
        stateNext.offset := io.updTag.bits.Addr.offset
        HAssert(!stateReg.tagVal)
        HAssert(io.updTag.bits.Addr.offset === 0.U)
    }

    when(cleanHit & io.clean.bits.isReq) {
        stateNext.req := false.B
        HAssert(stateReg.req)

    }.elsewhen(io.alloc.valid & io.alloc.bits.isReq) {
        stateNext.req := true.B
        HAssert(!stateReg.req)
    }

    when(cleanHit & io.clean.bits.isSnp) {
        stateNext.snp := false.B
        HAssert(stateReg.snp)

    }.elsewhen(io.alloc.valid & io.alloc.bits.isSnp) {
        stateNext.snp := true.B
        HAssert(!stateReg.snp)
    }

    if (hasBBN) {

        when(!stateNext.req) {
            stateNext.canNest.get := false.B
            HAssert(!canNestHit)

        }.elsewhen(canNestHit) {
            stateNext.canNest.get := io.updNest.get.bits.nest
            HAssert(stateReg.canNest.get ^ io.updNest.get.bits.nest)
        }
        HAssert.withEn(!stateReg.canNest.get, canNestHit & io.updNest.get.bits.nest)
        HAssert.withEn(stateReg.canNest.get, canNestHit & !io.updNest.get.bits.nest)
        HAssert.withEn(!stateReg.canNest.get, io.alloc.valid & io.alloc.bits.isReq)
        HAssert.withEn(stateReg.canNest.get, io.alloc.valid & io.alloc.bits.isSnp & stateReg.req)
    }

    HAssert(!(io.alloc.valid & cleanHit & canNestHit))
    HAssert.checkTimeout(!stateReg.valid, TIMEOUT_POS, cf"PoS TIMEOUT")

    io.wakeup.valid := RegNext(cleanHit & stateReg.one & stateReg.tagVal)
    io.wakeup.bits.Addr.catPoS(io.config.bankId, stateReg.tag, io.hnIdx.pos.set, io.hnIdx.dirBank)
}

class PosSet(implicit p: Parameters) extends DJModule {

    val io = IO(new Bundle {
        val config  = new DJConfigIO()
        val dirBank = Input(UInt(dirBankBits.W))
        val posSet  = Input(UInt(posSetBits.W))

        val alloc_s0 = Flipped(Valid(new Addr with HasChiChannel))
        val sleep_s1 = Output(Bool())
        val block_s1 = Output(Bool())
        val hnIdx_s1 = Valid(new HnIndex)
        val retry_s1 = Input(Bool())

        val reqPoS  = Flipped(Valid(new ChiChnlBundle))
        val posResp = Valid(UInt(posWayBits.W))
        val updTag  = Flipped(Valid(new Addr with HasAddrValid with HasPackHnIdx))

        val updNest = if (hasBBN) Some(Flipped(Valid(new PosCanNest))) else None
        val clean   = Flipped(Valid(new PosClean))

        val wakeup = Valid(new Addr)

        val stateVec = Output(Vec(posWays, new PosState with HasAddr))
    })
    dontTouch(io.stateVec)

    val entries = Seq.fill(posWays) { Module(new PosEntry()) }
    val lockReg = RegInit(false.B)

    val esVec = VecInit(entries.map(_.io.state))

    val allocReg_s1    = RegInit(Valid(new Addr with HasChiChannel).Lit(_.valid -> false.B))
    val allocWayReg_s1 = Reg(UInt(posWayBits.W))

    val allocTag_s0 = io.alloc_s0.bits.Addr.posTag; dontTouch(allocTag_s0)

    val matTagVec_s0 = Cat(esVec.map(s => s.valid & s.tagVal & s.tag === allocTag_s0).reverse)
    val matTagWay_s0 = PriorityEncoder(matTagVec_s0)
    val hasMatTag    = matTagVec_s0.orR
    dontTouch(matTagVec_s0)
    HardwareAssertion.withEn(PopCount(matTagVec_s0) <= 1.U, io.alloc_s0.valid)

    val useWay_s1  = Mux(allocReg_s1.valid, ~UIntToOH(allocWayReg_s1), Fill(posWays, 1.U))
    val freeVec_s0 = Cat(esVec.map(!_.valid).reverse) & useWay_s1
    val hasFree_s0 = freeVec_s0(djparam.posWays - 3, 0).orR
    val freeWay_s0 = PriorityEncoder(freeVec_s0)

    val blockReq_s0 = Mux(hasMatTag, true.B, !hasFree_s0)

    val canNest_s0  = if (hasBBN) hasMatTag & esVec(matTagWay_s0).canNest.get else false.B
    val blockSnp_s0 = Mux(hasMatTag, !canNest_s0, !hasFree_s0)

    val matchReqS1_s0 = allocReg_s1.valid & allocTag_s0 === allocReg_s1.bits.Addr.posTag

    val block_s0 = Mux(io.alloc_s0.bits.isSnp, blockSnp_s0, blockReq_s0) | matchReqS1_s0 | lockReg | io.reqPoS.valid

    allocReg_s1.valid := io.alloc_s0.valid & !block_s0
    when(io.alloc_s0.valid) {
        allocReg_s1.bits := io.alloc_s0.bits
        allocWayReg_s1   := Mux(canNest_s0, matTagWay_s0, freeWay_s0)
    }

    io.sleep_s1              := RegNext(io.alloc_s0.valid & hasMatTag)
    io.block_s1              := RegNext(io.alloc_s0.valid & block_s0) | io.reqPoS.valid
    io.hnIdx_s1.valid        := RegNext(io.alloc_s0.valid) & !io.reqPoS.valid
    io.hnIdx_s1.bits.dirBank := io.dirBank
    io.hnIdx_s1.bits.pos.set := io.posSet
    io.hnIdx_s1.bits.pos.way := allocWayReg_s1

    val freeVec = Cat(esVec.map(!_.valid).reverse)
    val replSelWay = PriorityMux(
        Seq(
            (io.reqPoS.bits.isReq & freeVec(posWays - 1)) -> (posWays - 1).U,
            (io.reqPoS.bits.isSnp & freeVec(posWays - 2)) -> (posWays - 2).U,
            true.B                                        -> PriorityEncoder(freeVec.asBools.dropRight(2))
        )
    )
    val reqPosFire = io.reqPoS.valid & freeVec(replSelWay) & !lockReg

    io.posResp.valid := RegNext(reqPosFire, false.B)
    io.posResp.bits  := RegEnable(replSelWay, reqPosFire)

    when(reqPosFire) {
        lockReg := true.B
        HAssert(!lockReg)
    }.elsewhen(io.updTag.valid & io.updTag.bits.hnIdx.dirBank === io.dirBank & io.updTag.bits.hnIdx.pos.set === io.posSet) {
        lockReg := false.B
        HAssert(lockReg)
    }

    HAssert.checkTimeout(!lockReg, 128, cf"PoS Lock Set TIMEOUT")

    entries.zipWithIndex.foreach { case (e, i) =>
        val replReqHit = reqPosFire & replSelWay === i.U
        val allocHit   = allocReg_s1.valid & !io.retry_s1 & allocWayReg_s1 === i.U
        replReqHit.suggestName(f"replReqHit_${i}")
        allocHit.suggestName(f"allocHit${i}")

        e.io.config             := io.config
        e.io.hnIdx.dirBank      := io.dirBank
        e.io.hnIdx.pos.set      := io.posSet
        e.io.hnIdx.pos.way      := i.U
        e.io.alloc.valid        := Mux(io.reqPoS.valid, replReqHit, allocHit)
        e.io.alloc.bits.addrVal := !io.reqPoS.valid
        e.io.alloc.bits.addr    := Mux(io.reqPoS.valid, 0.U, allocReg_s1.bits.addr)
        e.io.alloc.bits.channel := Mux(io.reqPoS.valid, io.reqPoS.bits.channel, allocReg_s1.bits.channel)
        e.io.updTag             := io.updTag
        e.io.clean              := io.clean
        io.stateVec(i)          := e.io.state
        if (hasBBN) {
            e.io.updNest.get := io.updNest.get
        }
    }
    io.wakeup := Mux1H(entries.map(_.io.wakeup.valid), entries.map(_.io.wakeup))

    HAssert(PopCount(entries.map(_.io.wakeup.valid)) <= 1.U)
    HAssert.withEn(!VecInit(esVec.map(es => es.valid & es.tagVal & es.addr === allocReg_s1.bits.addr)).asUInt.orR, allocReg_s1.valid)
    HAssert.withEn(!VecInit(esVec.map(es => es.valid & es.tagVal & es.addr === io.updTag.bits.addr)).asUInt.orR, io.updTag.valid & io.updTag.bits.addrVal)

    private val usedWayCount = PopCount(esVec.map(_.valid))
    private val blockNoFree  = io.alloc_s0.valid && !hasMatTag && !hasFree_s0
    private val blockMatTag = io.alloc_s0.valid && hasMatTag && (
        io.alloc_s0.bits.isReq || (io.alloc_s0.bits.isSnp && !canNest_s0)
    )
    private val blockMatchS1 = io.alloc_s0.valid && matchReqS1_s0
    private val blockLock    = io.alloc_s0.valid && lockReg
    private val blockReqPoS  = io.alloc_s0.valid && io.reqPoS.valid
    private val reqPoSBlock  = io.reqPoS.valid && (!freeVec(replSelWay) || lockReg)
    HomeWrapperPerf.accumulate(
        Seq(
            ("zj_pos_set_alloc_valid", io.alloc_s0.valid),
            ("zj_pos_set_alloc_fire", allocReg_s1.valid && !io.retry_s1),
            ("zj_pos_set_alloc_retry", allocReg_s1.valid && io.retry_s1),
            ("zj_pos_set_block", io.alloc_s0.valid && block_s0),
            ("zj_pos_set_block_no_free", blockNoFree),
            ("zj_pos_set_block_mat_tag", blockMatTag),
            ("zj_pos_set_block_match_s1", blockMatchS1),
            ("zj_pos_set_block_lock", blockLock),
            ("zj_pos_set_block_req_pos", blockReqPoS),
            ("zj_pos_set_sleep", io.sleep_s1),
            ("zj_pos_set_req_pos_fire", reqPosFire),
            ("zj_pos_set_req_pos_block", reqPoSBlock),
            ("zj_pos_set_lock_cycle", lockReg),
            ("zj_pos_set_used_way_sum", usedWayCount)
        )
    )
    HomeWrapperPerf.max("zj_pos_set_used_way_max", usedWayCount, true.B)
}

class PosTable(isTop: Boolean = false)(implicit p: Parameters) extends DJModule {
    override def isTopModule: Boolean = isTop

    val io = IO(new Bundle {
        val config  = new DJConfigIO()
        val dirBank = Input(UInt(dirBankBits.W))

        val alloc_s0 = Flipped(Valid(new Addr with HasChiChannel))
        val sleep_s1 = Output(Bool())
        val block_s1 = Output(Bool())
        val hnIdx_s1 = Output(new HnIndex)
        val retry_s1 = Input(Bool())

        val reqPosVec  = Vec(posSets, Flipped(Valid(new ChiChnlBundle)))
        val posRespVec = Vec(posSets, Valid(UInt(posWayBits.W)))
        val updTag     = Flipped(Valid(new Addr with HasAddrValid with HasPackHnIdx))

        val updNest = if (hasBBN) Some(Flipped(Valid(new PosCanNest))) else None
        val clean   = Flipped(Valid(new PosClean))

        val wakeup = Valid(new Addr)

        val getAddrVec = Vec(3, Flipped(new GetAddr()))

        val alrUsePoS = Output(UInt(log2Ceil(nrPoS + 1).W))

        val working = Output(Bool())
    })

    val sets     = Seq.fill(posSets) { Module(new PosSet()) }
    val debugVec = WireInit(VecInit(sets.map(_.io.stateVec)))
    dontTouch(debugVec)

    sets.zipWithIndex.foreach { case (set, i) =>
        set.io.config  := io.config
        set.io.dirBank := io.dirBank
        set.io.posSet  := i.U

        set.io.alloc_s0.valid := io.alloc_s0.valid & io.alloc_s0.bits.Addr.posSet === i.U
        set.io.alloc_s0.bits  := io.alloc_s0.bits

        set.io.reqPoS.valid := io.reqPosVec(i).valid
        set.io.reqPoS.bits  := io.reqPosVec(i).bits

        set.io.retry_s1 := io.retry_s1

        set.io.clean  := io.clean
        set.io.updTag := io.updTag
        if (hasBBN) {
            set.io.updNest.get := io.updNest.get
        }
    }

    io.sleep_s1 := Cat(sets.map(_.io.sleep_s1)).orR
    io.block_s1 := Cat(sets.map(_.io.block_s1)).orR
    io.hnIdx_s1 := Mux1H(sets.map(_.io.hnIdx_s1.valid), sets.map(_.io.hnIdx_s1.bits))
    io.wakeup   := Mux1H(sets.map(_.io.wakeup.valid), sets.map(_.io.wakeup))
    io.posRespVec.zip(sets.map(_.io.posResp)).foreach { case (a, b) => a := b }

    HAssert(PopCount(sets.map(_.io.hnIdx_s1.valid)) <= 1.U)
    HAssert(PopCount(sets.map(_.io.wakeup.valid)) <= 1.U)

    io.alrUsePoS := PopCount(Cat(sets.flatMap(_.io.stateVec.map(_.valid))))

    val addrVec2 = Wire(Vec(posSets, Vec(posWays, UInt(addrBits.W))))
    addrVec2.zip(sets.map(_.io.stateVec.map(_.addr))).foreach { case (a, b) => a := b }
    io.getAddrVec.foreach { get => get.result.addr := addrVec2(get.hnIdx.pos.set)(get.hnIdx.pos.way) }
    dontTouch(addrVec2)

    io.working := Cat(sets.flatMap(_.io.stateVec.map(_.valid))).orR

    HomeWrapperPerf.accumulate(
        Seq(
            ("zj_pos_table_alloc_valid", io.alloc_s0.valid),
            ("zj_pos_table_block", io.block_s1),
            ("zj_pos_table_sleep", io.sleep_s1),
            ("zj_pos_table_wakeup", io.wakeup.valid),
            ("zj_pos_table_clean", io.clean.valid),
            ("zj_pos_table_upd_tag", io.updTag.valid),
            ("zj_pos_table_occupancy_sum", io.alrUsePoS),
            ("zj_pos_table_working_cycle", io.alrUsePoS.orR)
        )
    )
    HomeWrapperPerf.max("zj_pos_table_occupancy_max", io.alrUsePoS, true.B)

    HAssert.placePipe(1)
}
