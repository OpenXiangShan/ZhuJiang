package xijiang.router.base

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xijiang.Node
import xs.utils.ResetRRArbiter
import zhujiang.perf.ZhuJiangPerf
import zhujiang.chi.{NodeIdBundle, RingFlit}
import zhujiang.{ZJModule, ZJParametersKey}

class SingleChannelTap[T <: RingFlit](gen: T, channel: String)(implicit p: Parameters) extends ZJModule {
    private val ringSize  = p(ZJParametersKey).island.size
    private val timerBits = log2Ceil(ringSize + 1)
    val io = IO(new Bundle {
        val in       = Input(new ChannelBundle(gen))
        val out      = Output(new ChannelBundle(gen))
        val inject   = Flipped(Decoupled(gen))
        val eject    = Decoupled(gen)
        val matchTag = Input(UInt(niw.W))
        val tapIdx   = Input(UInt(nodeAidBits.W))
    })

    private val modName         = s"SingleChannelTap$channel"
    override val desiredName    = modName
    private val normalShift     = 0
    private val injectRsvdShift = 1
    private val waitSlotShift   = 2

    private val s_normal          = (1 << normalShift).U(3.W)
    private val s_inject_reserved = (1 << injectRsvdShift).U(3.W)
    private val s_wait_slot       = (1 << waitSlotShift).U(3.W)
    private val state             = RegInit(s_normal)
    private val counter           = RegInit(0.U(timerBits.W))
    private val flitNext          = Wire(Valid(gen))
    private val rsvdNext          = Wire(Valid(UInt(niw.W)))
    private val injectFire        = io.inject.fire
    private val ejectFire         = io.eject.fire
    private val rsvdMarkVal       = Cat(io.matchTag(niw - 1, nodeAidBits), io.tapIdx)
    private val meetRsvdSlot      = io.in.rsvd.bits === rsvdMarkVal

    when(injectFire) {
        counter := 0.U
    }.elsewhen(io.inject.valid && !io.inject.ready && state(normalShift) && !counter(timerBits - 1)) {
        counter := counter + 1.U
    }

    switch(state) {
        is(s_normal) {
            state := Mux(counter(timerBits - 1), s_inject_reserved, s_normal)
        }
        is(s_inject_reserved) {
            state := Mux(injectFire, s_normal, Mux(io.in.rsvd.valid, s_inject_reserved, s_wait_slot))
        }
        is(s_wait_slot) {
            state := Mux(injectFire, s_normal, s_wait_slot)
        }
    }
    when(io.in.rsvd.valid && meetRsvdSlot) {
        assert(state(waitSlotShift), "Unexpected reserved slot!")
    }

    private val emptySlot     = Mux(io.in.flit.valid, ejectFire, true.B)
    private val availableSlot = Mux(io.in.rsvd.valid, meetRsvdSlot, !state(waitSlotShift))
    dontTouch(emptySlot)
    dontTouch(availableSlot)
    io.inject.ready := emptySlot && availableSlot

    flitNext.valid := injectFire || io.in.flit.valid && !ejectFire
    flitNext.bits  := Mux(injectFire, io.inject.bits, io.in.flit.bits)

    rsvdNext.valid := (state(injectRsvdShift) || io.in.rsvd.valid) && !injectFire
    rsvdNext.bits  := Mux(state(injectRsvdShift) && !io.in.rsvd.valid, rsvdMarkVal, io.in.rsvd.bits)

    private val flitVReg = RegNext(flitNext.valid, false.B)
    private val flitBReg = RegEnable(flitNext.bits, io.inject.valid || io.in.flit.valid)
    private val rsvdVReg = RegNext(rsvdNext.valid, false.B)
    private val rsvdBReg = RegEnable(rsvdNext.bits, state(injectRsvdShift) || io.in.rsvd.valid)
    io.out.flit.valid := flitVReg
    io.out.flit.bits  := flitBReg
    io.out.rsvd.valid := rsvdVReg
    io.out.rsvd.bits  := rsvdBReg
    flitVReg.suggestName("flit_pipe_v")
    flitBReg.suggestName("flit_pipe_b")
    rsvdVReg.suggestName("rsvd_pipe_v")
    rsvdBReg.suggestName("rsvd_pipe_b")

    private val matcher = io.matchTag.asTypeOf(new NodeIdBundle)
    io.eject.valid := io.in.flit.bits.tgt.asTypeOf(new NodeIdBundle).router === matcher.router && io.in.flit.valid
    io.eject.bits  := io.in.flit.bits

    private val chn               = channel.toLowerCase
    private val injectStall       = io.inject.valid && !io.inject.ready
    private val injectBlockFlit   = io.inject.valid && !io.inject.ready && !emptySlot
    private val injectBlockSlot   = io.inject.valid && !io.inject.ready && emptySlot && !availableSlot
    private val ejectStall        = io.eject.valid && !io.eject.ready
    private val rsvdMatch         = io.in.rsvd.valid && meetRsvdSlot
    private val waitSlotCycle     = state(waitSlotShift)

    // Local Ring TopDown only.  The parent population is a cycle with either
    // injection or ejection back-pressure at this channel tap.  Categories
    // are deliberately one-hot and use only signals available in this module:
    // queue/full, endpoint back-pressure, reserved/link wait, and unknown.
    // This is not a CPU-loss or end-to-end transaction attribution counter.
    private val topdownPressure = injectStall || ejectStall
    private val topdownQueueFull = injectBlockFlit
    private val topdownReservedLink = !topdownQueueFull && injectBlockSlot
    private val topdownEndpoint = !topdownQueueFull && !topdownReservedLink && ejectStall
    private val topdownClasses = VecInit(Seq(
        topdownQueueFull,
        topdownEndpoint,
        topdownReservedLink
    ))
    assert(PopCount(topdownClasses) <= 1.U, "Ring TopDown owner must be one-hot")
    assert(PopCount(topdownClasses) === topdownPressure.asUInt,
        "Ring TopDown owner must cover every pressure cycle")
    private val injectWaitCounter = RegInit(0.U(16.W))
    private val injectWaitNext = Mux(
        io.inject.fire || !io.inject.valid,
        0.U,
        Mux(injectStall && !injectWaitCounter.andR, injectWaitCounter + 1.U, injectWaitCounter)
    )
    injectWaitCounter := injectWaitNext

    ZhuJiangPerf.accumulate(
        Seq(
            (s"zj_ring_${chn}_rx_flit_valid", io.in.flit.valid),
            (s"zj_ring_${chn}_tx_flit_valid", io.out.flit.valid),
            (s"zj_ring_${chn}_inject_valid", io.inject.valid),
            (s"zj_ring_${chn}_inject_fire", injectFire),
            (s"zj_ring_${chn}_inject_stall", injectStall),
            (s"zj_ring_${chn}_inject_block_flit", injectBlockFlit),
            (s"zj_ring_${chn}_inject_block_slot", injectBlockSlot),
            (s"zj_ring_${chn}_eject_valid", io.eject.valid),
            (s"zj_ring_${chn}_eject_fire", ejectFire),
            (s"zj_ring_${chn}_eject_stall", ejectStall),
            (s"zj_ring_${chn}_rsvd_valid", io.in.rsvd.valid),
            (s"zj_ring_${chn}_rsvd_match", rsvdMatch),
            (s"zj_ring_${chn}_inject_reserved_cycle", state(injectRsvdShift)),
            (s"zj_ring_${chn}_wait_slot_cycle", waitSlotCycle),
            (s"zj_ring_${chn}_topdown_pressure_cycle", topdownPressure)
        )
    )
    ZhuJiangPerf.max(s"zj_ring_${chn}_inject_wait", injectWaitNext, injectStall)
}

class ChannelTap[T <: RingFlit](
    gen: T,
    channel: String,
    ejectBuf: Int,
    node: Node
)(implicit p: Parameters)
    extends ZJModule {
    val io = IO(new Bundle {
        val rx             = Input(Vec(2, new ChannelBundle(gen)))
        val tx             = Output(Vec(2, new ChannelBundle(gen)))
        val inject         = Flipped(Decoupled(gen))
        val eject          = Decoupled(gen)
        val matchTag       = Input(UInt(niw.W))
        val injectTapSelOH = Input(Vec(2, Bool()))
    })
    override val desiredName = s"ChannelTapLocal$channel"
    private val taps         = Seq.fill(2)(Module(new SingleChannelTap(gen, channel)))
    private val ejectArb     = Module(new ResetRRArbiter(gen, 2))
    for (idx <- taps.indices) {
        taps(idx).io.in           := io.rx(idx)
        io.tx(idx)                := taps(idx).io.out
        taps(idx).io.inject.valid := io.inject.valid && io.injectTapSelOH(idx)
        taps(idx).io.inject.bits  := io.inject.bits
        taps(idx).io.matchTag     := io.matchTag
        taps(idx).io.tapIdx       := idx.U
        if (channel == "DBG") {
            val eb = Module(new Queue(gen, 2))
            eb.io.enq           <> taps(idx).io.eject
            ejectArb.io.in(idx) <> eb.io.deq
            eb.suggestName(s"eject_buf_$idx")
        } else {
            val eb = Module(new EjectBuffer(gen, ejectBuf, channel))
            eb.io.enq           <> taps(idx).io.eject
            ejectArb.io.in(idx) <> eb.io.deq
            eb.suggestName(s"eject_buf_$idx")
        }
    }
    io.eject        <> ejectArb.io.out
    io.inject.ready := Mux1H(io.injectTapSelOH, taps.map(_.io.inject.ready))
}
