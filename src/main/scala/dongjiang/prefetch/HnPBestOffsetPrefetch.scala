package dongjiang.prefetch

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import dongjiang._
import dongjiang.utils._
import xs.utils.Constantin
import xs.utils.sram.SRAMTemplate
import zhujiang.chi._
import zhujiang.perf.ZJPerf

case class HnBOPParameters(
    rrTableEntries: Int = 256,
    rrTagBits: Int = 12,
    scoreBits: Int = 5,
    roundMax: Int = 50,
    badScore: Int = 2,
    dQEntries: Int = 16,
    dQLatency: Int = 300,
    dQMaxLatency: Int = 1024,
    offsetList: Seq[Int] = Seq(
        -256, -250, -243, -240, -225, -216, -200, -192, -180, -162, -160, -150, -144, -135, -128, -125, -120, -108,
        -100, -96, -90, -81, -80, -75, -72, -64, -60, -54, -50, -48, -45, -40, -36, -32, -30, -27, -25, -24, -20, -18,
        -16, -15, -12, -10, -9, -8, -6, -5, -4, -3, -2, -1, 1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, 24, 25, 27,
        30, 32, 36, 40, 45, 48, 50, 54, 60, 64, 72, 75, 80, 81, 90, 96, 100, 108, 120, 125, 128, 135, 144, 150, 160, 162,
        180, 192, 200, 216, 225, 240, 243, 250
    )
)

trait HasHnBOPParams extends HasDJParam {
    def bopParams: HnBOPParameters = HnBOPParameters()
    def pageOffsetBits: Int = 12
    def fullAddrBits: Int = addrBits
    def noOffsetAddrBits: Int = fullAddrBits - offsetBits
    def rrTableEntries: Int = bopParams.rrTableEntries
    def rrIdxBits: Int = log2Up(rrTableEntries)
    def rrTagBits: Int = bopParams.rrTagBits
    def scoreBits: Int = bopParams.scoreBits
    def roundMax: Int = bopParams.roundMax
    def badScore: Int = bopParams.badScore
    def offsetList: Seq[Int] = bopParams.offsetList
    def dQEntries: Int = bopParams.dQEntries
    def dQLatency: Int = bopParams.dQLatency
    def dQMaxLatency: Int = bopParams.dQMaxLatency
    def scores: Int = offsetList.length
    def offsetWidth: Int = log2Up(offsetList.max) + 2
    def roundBits: Int = log2Up(roundMax)
    def scoreMax: Int = (1 << scoreBits) - 1
    def scoreTableIdxBits: Int = log2Up(scores)

    def signedExtend(x: UInt, width: Int): UInt = {
        if (x.getWidth >= width) x else Cat(Fill(width - x.getWidth, x.head(1)), x)
    }

    def getPPN(x: UInt): UInt = x(x.getWidth - 1, pageOffsetBits)
}

abstract class HnBOPModule(implicit val p: Parameters) extends Module with HasHnBOPParams {
    protected def emitOffsetDistCounters(prefix: String, offset: SInt, enable: Bool): Unit = {
        for (off <- offsetList) {
            val counterName =
                if (off < 0) prefix + "_neg_" + (-off).toString
                else prefix + "_pos_" + off.toString
            ZJPerf.accumulate(counterName, enable && offset === off.S(offsetWidth.W))
        }
    }
}

class HnTestOffsetReq(implicit p: Parameters) extends DJBundle with HasHnBOPParams {
    val addr = UInt(fullAddrBits.W)
    val testOffset = UInt(offsetWidth.W)
    val ptr = UInt(scoreTableIdxBits.W)
}

class HnTestOffsetResp(implicit p: Parameters) extends DJBundle with HasHnBOPParams {
    val ptr = UInt(scoreTableIdxBits.W)
    val hit = Bool()
}

class HnTestOffsetBundle(implicit p: Parameters) extends DJBundle with HasHnBOPParams {
    val req = DecoupledIO(new HnTestOffsetReq)
    val resp = Flipped(ValidIO(new HnTestOffsetResp))
}

class HnRecentRequestTable(name: String)(implicit p: Parameters) extends HnBOPModule {
    val io = IO(new Bundle {
        val w = Flipped(DecoupledIO(UInt(fullAddrBits.W)))
        val r = Flipped(new HnTestOffsetBundle)
    })

    def lineAddr(addr: UInt) = addr(fullAddrBits - 1, offsetBits)
    def hash1(addr: UInt) = lineAddr(addr)(rrIdxBits - 1, 0)
    def hash2(addr: UInt) = lineAddr(addr)(2 * rrIdxBits - 1, rrIdxBits)
    def idx(addr: UInt) = hash1(addr) ^ hash2(addr)
    def tag(addr: UInt) = lineAddr(addr)(rrTagBits + rrIdxBits - 1, rrIdxBits)
    def rrTableEntry() = new Bundle {
        val valid = Bool()
        val tag = UInt(rrTagBits.W)
    }

    val rrTable = Module(
        new SRAMTemplate(
            rrTableEntry(),
            set = rrTableEntries,
            way = 1,
            shouldReset = true,
            singlePort = true,
            hasMbist = hasMbist
        )
    )

    val wAddr = io.w.bits
    rrTable.io.w.req.valid := io.w.valid && !io.r.req.valid
    rrTable.io.w.req.bits.setIdx := idx(wAddr)
    rrTable.io.w.req.bits.data(0).valid := true.B
    rrTable.io.w.req.bits.data(0).tag := tag(wAddr)

    val rAddr = io.r.req.bits.addr - signedExtend((io.r.req.bits.testOffset << offsetBits), fullAddrBits)
    val rData = Wire(rrTableEntry())
    rrTable.io.r.req.valid := io.r.req.fire
    rrTable.io.r.req.bits.setIdx := idx(rAddr)
    rData := rrTable.io.r.resp.data(0)

    assert(!RegNext(io.w.fire && io.r.req.fire), "single port SRAM should not read and write at the same time")

    val s0_valid = rrTable.io.r.req.fire
    val s1_valid = RegNext(s0_valid, false.B)
    val s1_ptr = RegNext(io.r.req.bits.ptr)
    val s1_hit = rData.valid && rData.tag === RegNext(tag(rAddr))
    val s2_valid = RegNext(s1_valid, false.B)

    io.w.ready := rrTable.io.w.req.ready && !io.r.req.valid
    io.r.req.ready := true.B
    io.r.resp.valid := s2_valid
    io.r.resp.bits.ptr := RegEnable(s1_ptr, s1_valid)
    io.r.resp.bits.hit := RegEnable(s1_hit, s1_valid)
}

class HnScoreTableEntry(implicit p: Parameters) extends DJBundle with HasHnBOPParams {
    val score = UInt(scoreBits.W)
    def apply(score: UInt) = {
        val entry = Wire(this)
        entry.score := score
        entry
    }
}

class HnOffsetScoreTable(name: String = "")(implicit p: Parameters) extends HnBOPModule {
    val io = IO(new Bundle {
        val req = Flipped(DecoupledIO(UInt(fullAddrBits.W)))
        val prefetchOffset = Output(UInt(offsetWidth.W))
        val prefetchDisable = Output(Bool())
        val phaseEndPulse = Output(Bool())
        val test = new HnTestOffsetBundle
    })

    private val badscoreConstant = Constantin.createRecord(name + "_badScore", bopParams.badScore)
    private val roundMaxConstant = Constantin.createRecord(name + "_roundMax", roundMax)

    val prefetchOffset = RegInit(2.U(offsetWidth.W))
    val prefetchDisable = RegInit(false.B)
    val st = RegInit(VecInit(Seq.fill(scores)((new HnScoreTableEntry).apply(0.U))))
    val offList = WireInit(VecInit(offsetList.map(off => off.S(offsetWidth.W).asUInt)))
    val ptr = RegInit(0.U(scoreTableIdxBits.W))
    val round = RegInit(0.U(roundBits.W))
    val bestOffset = RegInit(2.U(offsetWidth.W))
    val bestScore = RegInit(0.U(scoreBits.W))
    val testOffset = offList(ptr)
    val respHit = io.test.resp.valid && io.test.resp.bits.hit
    val respPtr = io.test.resp.bits.ptr
    val oldScore = st(respPtr).score
    val newScore = Mux(oldScore === scoreMax.U, oldScore, oldScore + 1.U)
    val respOffset = offList(respPtr)
    val renewOffset = respHit && newScore > bestScore
    val committedBestOffset = Mux(renewOffset, respOffset, bestOffset)

    val s_idle :: s_learn :: Nil = Enum(2)
    val state = RegInit(s_idle)
    val isBad = bestScore < badscoreConstant

    when(state === s_idle) {
        st.foreach(_.score := 0.U)
        ptr := 0.U
        round := 0.U
        bestScore := 0.U
        prefetchOffset := bestOffset
        prefetchDisable := isBad
        state := s_learn
    }

    when(state === s_learn) {
        when(io.test.req.fire) {
            val roundFinish = ptr === (scores - 1).U
            ptr := Mux(roundFinish, 0.U, ptr + 1.U)
            round := Mux(roundFinish, round + 1.U, round)
        }
        when(round >= roundMaxConstant) {
            state := s_idle
        }
        when(respHit) {
            st(respPtr).score := newScore
            bestOffset := committedBestOffset
            bestScore := Mux(renewOffset, newScore, bestScore)
            when(newScore >= scoreMax.U) {
                state := s_idle
            }
        }
    }

    io.req.ready := state === s_learn
    io.prefetchOffset := prefetchOffset
    io.prefetchDisable := prefetchDisable
    io.phaseEndPulse := RegNext(state === s_idle, false.B)
    io.test.req.valid := state === s_learn && io.req.valid
    io.test.req.bits.addr := io.req.bits
    io.test.req.bits.testOffset := testOffset
    io.test.req.bits.ptr := ptr

    ZJPerf.accumulate("total_learn_phase", state === s_idle)
    ZJPerf.accumulate("total_bop_disable", state === s_idle && isBad)
    emitOffsetDistCounters("teacher_best_offset", io.prefetchOffset.asSInt, io.phaseEndPulse)
}

class HnDelayQueue(name: String = "")(implicit p: Parameters) extends HnBOPModule {
    val io = IO(new Bundle() {
        val pfCtrlOfDelayLatency = Input(UInt(10.W))
        val in = Flipped(DecoupledIO(UInt(noOffsetAddrBits.W)))
        val out = DecoupledIO(UInt(fullAddrBits.W))
    })

    val IdxWidth = log2Up(dQEntries)
    val LatencyWidth = log2Up(dQMaxLatency)
    class Entry extends Bundle {
        val addrNoOffset = UInt(noOffsetAddrBits.W)
        val cnt = UInt(LatencyWidth.W)
    }
    val queue = RegInit(VecInit(Seq.fill(dQEntries)(0.U.asTypeOf(new Entry))))
    val valids = RegInit(VecInit(Seq.fill(dQEntries)(false.B)))
    val head = RegInit(0.U(IdxWidth.W))
    val tail = RegInit(0.U(IdxWidth.W))
    val empty = head === tail && !valids.last
    val full = head === tail && valids.last
    val outValid = !empty && !queue(head).cnt.orR && valids(head)

    val cstDqLatency = Constantin.createRecord(name + "_delayQueueLatency", dQLatency)
    val setDqLatency = Mux(io.pfCtrlOfDelayLatency =/= 0.U, io.pfCtrlOfDelayLatency, cstDqLatency)
    val doEnq = io.in.valid
    val doDeq = outValid && io.out.ready
    val overwrite = full && doEnq && !doDeq
    val replaceFull = full && doEnq
    when(doEnq) {
        queue(tail).addrNoOffset := io.in.bits
        queue(tail).cnt := setDqLatency
        valids(tail) := true.B
        tail := tail + 1.U
    }
    when(doDeq && !replaceFull) {
        valids(head) := false.B
    }
    when(doDeq || overwrite) {
        head := head + 1.U
    }
    io.in.ready := true.B
    io.out.valid := outValid
    io.out.bits := Cat(queue(head).addrNoOffset, 0.U(offsetBits.W))

    for (i <- 0 until dQEntries) {
        when(queue(i).cnt.orR) {
            queue(i).cnt := queue(i).cnt - 1.U
        }
    }

    ZJPerf.accumulate("full", full)
    ZJPerf.accumulate("empty", empty)
    ZJPerf.accumulate("overwrite", overwrite)
    ZJPerf.accumulate("entryNumber", PopCount(valids.asUInt))
    ZJPerf.accumulate("inNumber", io.in.valid)
    ZJPerf.accumulate("outNumber", io.out.valid)
}

class HnPBestOffsetPrefetch(implicit p: Parameters) extends HnBOPModule {
    val io = IO(new Bundle() {
        val enable = Input(Bool())
        val bankId = Input(UInt(bankBits.W))
        val train = Flipped(DecoupledIO(UInt(fullAddrBits.W)))
        val req = DecoupledIO(new ReqFlit(false))
    })

    private val cstEnable = Constantin.createRecord("hn_pbop_enable", initValue = 0)
    val enable = io.enable && cstEnable.orR

    val delayQueue = Module(new HnDelayQueue("hnpbop"))
    val rrTable = Module(new HnRecentRequestTable("hnpbop"))
    val scoreTable = Module(new HnOffsetScoreTable("hnpbop"))

    val s1_req_valid = RegInit(false.B)
    val s0_ready = WireInit(false.B)
    val s1_ready = WireInit(false.B)
    val s0_fire = s0_ready && io.train.valid
    val s1_fire = s1_ready && s1_req_valid

    val teacherOffset = scoreTable.io.prefetchOffset
    val teacherDisable = scoreTable.io.prefetchDisable
    val issueEnable = !teacherDisable
    val s0_oldAddr = io.train.bits
    val s0_oldAddrNoOff = s0_oldAddr(s0_oldAddr.getWidth - 1, offsetBits)
    val s0_newAddr = s0_oldAddr + signedExtend((teacherOffset << offsetBits), fullAddrBits)
    val s0_crossPage = getPPN(s0_newAddr) =/= getPPN(s0_oldAddr)
    val s0_sameBank = getBankId(s0_newAddr) === io.bankId

    rrTable.io.r <> scoreTable.io.test
    rrTable.io.w <> delayQueue.io.out
    delayQueue.io.pfCtrlOfDelayLatency := 0.U
    delayQueue.io.in.valid := io.train.valid && scoreTable.io.req.ready
    delayQueue.io.in.bits := s0_oldAddrNoOff
    scoreTable.io.req.valid := io.train.valid && delayQueue.io.in.ready
    scoreTable.io.req.bits := s0_oldAddr

    class HnPBestOffsetReq extends Bundle {
        val addr = UInt(fullAddrBits.W)
        val issueOffset = SInt(offsetWidth.W)
    }
    val s1_addr = Reg(UInt(fullAddrBits.W))
    val s1_issueOffset = RegInit(0.S(offsetWidth.W))
    val reqSkidValid = RegInit(false.B)
    val reqSkidBits = Reg(new HnPBestOffsetReq)

    s0_ready := delayQueue.io.in.ready && scoreTable.io.req.ready
    s1_ready := true.B

    when(s0_fire) {
        s1_addr := s0_newAddr
        s1_issueOffset := teacherOffset.asSInt
        s1_req_valid := !s0_crossPage && s0_sameBank && issueEnable
    }.elsewhen(s1_fire) {
        s1_req_valid := false.B
    }

    val skidEnq = s1_req_valid
    val skidDeq = enable && reqSkidValid && io.req.ready
    when(skidEnq) {
        reqSkidBits.addr := s1_addr
        reqSkidBits.issueOffset := s1_issueOffset
        reqSkidValid := true.B
    }.elsewhen(skidDeq) {
        reqSkidValid := false.B
    }
    io.req.valid := enable && reqSkidValid
    io.req.bits := 0.U.asTypeOf(new ReqFlit(false))
    io.req.bits.Addr := reqSkidBits.addr
    io.req.bits.Size := log2Ceil(djparam.CacheLine).U
    io.req.bits.Opcode := ReqOpcode.StashOnceShared
    io.req.bits.Order := Order.None
    io.req.bits.SnpAttr := true.B
    io.req.bits.MemAttr := MemAttr(allocate = true.B, cacheable = true.B, device = false.B, ewa = true.B).asUInt
    io.req.bits.ExpCompAck := false.B
    io.req.bits.Excl := false.B
    io.req.bits.TxnID := 0.U
    io.req.bits.SrcID := 0.U
    io.req.bits.TgtID := LAN.U
    io.req.bits.QoS := 0.U
    io.train.ready := s0_ready

    ZJPerf.accumulate("bop_req", io.req.fire)
    emitOffsetDistCounters("prefetch_sent_issue_offset", reqSkidBits.issueOffset, io.req.fire)
    ZJPerf.accumulate("bop_train", io.train.fire)
    ZJPerf.accumulate("bop_train_stall_for_st_not_ready", io.train.valid && !scoreTable.io.req.ready)
    ZJPerf.accumulate("bop_drop_for_cross_page", scoreTable.io.req.fire && s0_crossPage)
    ZJPerf.accumulate("bop_drop_for_cross_bank", scoreTable.io.req.fire && !s0_sameBank)
    ZJPerf.accumulate("bop_drop_for_external_disable", scoreTable.io.req.fire && !enable)
    ZJPerf.accumulate("bop_drop_for_auto_disable", scoreTable.io.req.fire && enable && !issueEnable)
    ZJPerf.accumulate("bop_issue_hold", reqSkidValid && !enable)
    ZJPerf.accumulate("bop_issue_overwrite", reqSkidValid && skidEnq && !skidDeq)
}
