package dongjiang.frontend

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import zhujiang.chi._
import zhujiang.chi.ReqOpcode._
import dongjiang._
import dongjiang.utils._
import dongjiang.bundle._
import xs.utils.debug._
import xs.utils.GTimer
import zhujiang.perf.{HistogramRange, ZJPerf}
import utility.{ChiselDB, Constantin, ParallelMux}
import dongjiang.frontend.TaskState._
import chisel3.experimental.BundleLiterals._

object TaskState {
    val width = 4
    val FREE  = "b0001".U
    val SEND  = "b0010".U
    val WAIT  = "b0100".U
    val SLEEP = "b1000".U
}

class TaskState(implicit p: Parameters) extends DJBundle {
    val state = UInt(TaskState.width.W)

    def isFree = state(0)
    def isValid = !isFree
    def isSend = state(1)
    def isWait = state(2)
    def isSleep = state(3)
}

class TaskEntry(nidBits: Int, sort: Boolean, timeout: Int)(implicit p: Parameters) extends DJModule {

    val io = IO(new Bundle {

        val chiTaskIn  = Flipped(Decoupled(new PackChi with HasAddr with HasQoS))
        val chiTask_s0 = Decoupled(new PackChi with HasAddr with HasQoS)

        val retry_s1 = Input(Bool())
        val sleep_s1 = Input(Bool())
        val wakeup   = Flipped(Valid(new Addr))

        val initNid = if (sort) Some(Input(UInt(nidBits.W))) else None
        val othRel  = if (sort) Some(Input(Bool())) else None
        val waitMeasure = Output(Valid(UInt(64.W)))
        val state = Output(new DJBundle with HasAddr {
            val valid   = Bool()
            val value   = UInt(TaskState.width.W)
            val release = Bool()
            val nid     = UInt(nidBits.W)
            val lock    = Bool()
            val isStash = Bool()
        })
    })

    val taskReg     = RegInit((new TaskState with HasPackChi with HasAddr with HasQoS).Lit(_.state -> FREE))
    val nidReg      = if (sort) Some(Reg(UInt(nidBits.W))) else None
    val retryNumReg = RegInit(0.U(log2Ceil(timeout).W))
    val timeoutReg  = RegNext(retryNumReg === (timeout - 1).U, false.B)
    require(isPow2(timeout))

    when(io.chiTaskIn.fire) {
        retryNumReg := 0.U
    }.elsewhen((retryNumReg < (timeout - 1).U) & taskReg.isWait & io.retry_s1) {
        retryNumReg := retryNumReg + 1.U
    }

    when(io.chiTaskIn.fire) {
        taskReg.chi  := io.chiTaskIn.bits.chi
        taskReg.addr := io.chiTaskIn.bits.addr
        taskReg.qos  := io.chiTaskIn.bits.qos
    }
    io.chiTaskIn.ready := taskReg.isFree

    // 排队时长:入队到首次被发射(重试不再计入,由 retry 路径另行观测)
    val queueTsReg = RegEnable(GTimer(), io.chiTaskIn.fire)
    val issuedReg  = RegInit(false.B)
    when (io.chiTaskIn.fire) { issuedReg := false.B }
    when (io.chiTask_s0.fire) { issuedReg := true.B }
    io.waitMeasure.valid := io.chiTask_s0.fire && !issuedReg
    io.waitMeasure.bits  := GTimer() - queueTsReg

    io.chiTask_s0.valid := taskReg.isSend & nidReg.getOrElse(0.U) === 0.U
    io.chiTask_s0.bits  := taskReg

    if (sort) {

        when(io.chiTaskIn.fire) {
            nidReg.get := io.initNid.get
        }.elsewhen(taskReg.isValid) {
            nidReg.get := nidReg.get - io.othRel.get
            HAssert.withEn(nidReg.get > 0.U, io.othRel.get)
        }
    }

    io.state.valid   := taskReg.isValid
    io.state.release := RegNext(taskReg.isValid) & taskReg.isFree
    io.state.addr    := taskReg.addr
    io.state.value   := taskReg.state
    io.state.nid     := nidReg.getOrElse(0.U)
    io.state.lock := (taskReg.isSend | taskReg.isWait) & timeoutReg
    io.state.isStash := taskReg.chi.reqIs(StashOnceShared)
    HAssert(!(io.state.valid & io.state.release))

    val wakeUpHit = io.wakeup.valid && taskReg.Addr.useAddr === io.wakeup.bits.Addr.useAddr
    switch(taskReg.state) {
        is(FREE) {
            when(io.chiTaskIn.fire) { taskReg.state := SEND }
        }
        is(SEND) {
            when(io.chiTask_s0.fire) { taskReg.state := WAIT }
        }
        is(WAIT) {
            when(wakeUpHit) { taskReg.state := SEND }
                .elsewhen(io.sleep_s1) { taskReg.state := SLEEP }
                .elsewhen(io.retry_s1) { taskReg.state := SEND }
                .otherwise { taskReg.state := FREE }
        }
        is(SLEEP) {
            when(wakeUpHit) { taskReg.state := SEND }
        }
    }

    HAssert.withEn(nidReg.getOrElse(0.U) === 0.U, taskReg.isSleep & wakeUpHit)

    HAssert.checkTimeout(taskReg.isFree, TIMEOUT_TASKBUF, desc = cf"State[${taskReg.state}]")
}

class StashCollideDbEntry(implicit p: Parameters) extends DJBundle {
    val cycle       = UInt(64.W)
    val useAddr     = UInt(useAddrBits.W)
    val collideType = UInt(2.W)
    val nrInflight  = UInt(5.W)
}

class TaskBuffer(nrEntries: Int, sort: Boolean, timeout: Int = 8, perfName: String = "req")(implicit p: Parameters) extends DJModule {

    val io = IO(new Bundle {

        val chiTaskIn  = Flipped(Decoupled(new PackChi with HasAddr with HasQoS))
        val chiTask_s0 = Decoupled(new PackChi with HasAddr with HasQoS)
        val lockTask   = Output(Bool())

        val retry_s1 = Input(Bool())
        val sleep_s1 = Input(Bool())
        val wakeup   = Flipped(Valid(new Addr))

        val working = Output(Bool())
    })

    val entries  = Seq.fill(nrEntries) { Module(new TaskEntry(log2Ceil(nrEntries), sort, timeout)) }
    val debugVec = WireInit(VecInit(entries.map(_.io.state)))
    dontTouch(debugVec)

    Alloc(entries.map(_.io.chiTaskIn), io.chiTaskIn)

    val taskVec_s0 = VecInit(entries.map(_.io.chiTask_s0))
    val lockVec    = VecInit(entries.map(_.io.state.lock))
    val lockIdx    = PriorityEncoder(lockVec)
    val hasLockReg = RegNext(lockVec.asUInt.orR, false.B)
    when(hasLockReg) {
        taskVec_s0.foreach(_.ready := false.B)
        io.chiTask_s0 <> taskVec_s0(lockIdx)
    }.otherwise {
        io.chiTask_s0 <> fastRRArb(taskVec_s0)
    }
    io.lockTask := hasLockReg

    entries.foreach { case e =>
        e.io.retry_s1 := io.retry_s1
        e.io.sleep_s1 := io.sleep_s1
        e.io.wakeup   := io.wakeup
    }

    if (sort) {
        entries.foreach { self =>
            self.io.initNid.get := PopCount(Cat(entries.map(other => other.io.state.valid & other.io.state.Addr.useAddr === io.chiTaskIn.bits.Addr.useAddr)))
            self.io.othRel.get  := Cat(entries.map(other => other.io.state.release & other.io.state.Addr.useAddr === self.io.state.Addr.useAddr)).orR
        }
        HAssert(PopCount(Cat(entries.map(e => e.io.state.release))) <= 1.U)
    }

    io.working := Cat(entries.map(_.io.state.valid)).orR

    ZJPerf.whenEnabled {
        // 排队时长聚合:本次发射 entry 的 (入队时刻 -> 首次发射)
        val waitMeasure = ParallelMux(entries.map(e => e.io.waitMeasure.valid -> e.io.waitMeasure.bits))
        val waitEn = entries.map(_.io.waitMeasure.valid).reduce(_ || _)
        ZJPerf.accumulate(
            Seq(
                (s"zj_taskbuf_${perfName}_queue_wait_sum", Mux(waitEn, waitMeasure, 0.U)),
                (s"zj_taskbuf_${perfName}_queue_cnt", waitEn)
            )
        )
        val validEntryCount = PopCount(entries.map(_.io.state.valid))
        val waitEntryCount  = PopCount(entries.map(_.io.state.value === WAIT))
        val newIsStash      = io.chiTaskIn.bits.chi.reqIs(StashOnceShared)
        val collideReqVec   = entries.map(e => e.io.state.valid & e.io.state.Addr.useAddr === io.chiTaskIn.bits.Addr.useAddr & !e.io.state.isStash)
        val collideStashVec = entries.map(e => e.io.state.valid & e.io.state.Addr.useAddr === io.chiTaskIn.bits.Addr.useAddr & e.io.state.isStash)
        val collideReqAny   = collideReqVec.reduce(_ | _)
        val collideStashAny = collideStashVec.reduce(_ | _)
        val collideAny      = io.chiTaskIn.fire & (collideReqAny | collideStashAny)
        ZJPerf.accumulate(
            Seq(
                ("zj_taskbuf_input_fire", io.chiTaskIn.fire),
                ("zj_taskbuf_input_stall", io.chiTaskIn.valid && !io.chiTaskIn.ready),
                ("zj_taskbuf_input_stash", io.chiTaskIn.fire && newIsStash),
                ("zj_hn_stash_collide_req", io.chiTaskIn.fire && newIsStash && collideReqAny),
                ("zj_hn_stash_collide_stash", io.chiTaskIn.fire && newIsStash && collideStashAny && !collideReqAny),
                ("zj_hn_req_collide_stash", io.chiTaskIn.fire && !newIsStash && collideStashAny),
                ("zj_taskbuf_output_fire", io.chiTask_s0.fire),
                ("zj_taskbuf_output_stall", io.chiTask_s0.valid && !io.chiTask_s0.ready),
                ("zj_taskbuf_lock_cycle", io.lockTask)
            )
        )
        if (sort) {
            ZJPerf.histogram(
                s"zj_taskbuf_collide_set_$perfName",
                Cat(io.chiTaskIn.bits.Addr.dirBank, io.chiTaskIn.bits.Addr.llcSet),
                collideAny,
                0,
                djparam.nrDirBank * llcSets,
                4
            )

            val collideTrace = Constantin.createRecord("zj_llc_trace_enable", initValue = false)
            val collideTable = ChiselDB.createRuntimeGatedTable(s"ZJ_StashCollide_$perfName", new StashCollideDbEntry)
            val collideEntry = Wire(new StashCollideDbEntry)
            collideEntry.cycle       := GTimer()
            collideEntry.useAddr     := io.chiTaskIn.bits.Addr.useAddr
            collideEntry.collideType := Cat(collideStashAny, collideReqAny)
            collideEntry.nrInflight  := PopCount(collideReqVec ++ collideStashVec)
            collideTable.log(collideEntry, collideTrace && collideAny, s"ZJ_StashCollide_$perfName", clock, reset)
        }
        ZJPerf.distribution(
            "zj_taskbuf_valid_entries",
            validEntryCount,
            true.B,
            HistogramRange.occupancy(entries.size)
        )
        ZJPerf.max("zj_taskbuf_wait_entries", waitEntryCount, true.B)
    }

    HAssert.placePipe(1)
}
