package zhujiang.utils

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xs.utils.perf.LogPerfHelper
import zhujiang.perf.ZJPerf

class DoubleCounterClockGate(ckenWindow: Int = 7, idleWindow: Int = 7, testctl: Boolean = false)(implicit p: Parameters) extends Module {
    override val desiredName = "DoubleCounterICG"
    val io = IO(new Bundle {
        val te       = Input(Bool())
        val test_on  = Option.when(testctl)(Input(Bool()))
        val test_off = Option.when(testctl)(Input(Bool()))
        val inbound  = Input(Bool())
        val working  = Input(Bool())
        val ock      = Output(Clock())
        val active   = Output(Bool())
    })
    private val ckenWindowCnt = RegInit(0.U(log2Ceil(ckenWindow).W))
    private val idleWindowCnt = RegInit(0.U(log2Ceil(idleWindow).W))
    private val cken          = RegInit(true.B)
    private val active        = RegInit(false.B)
    private val cg            = Module(new xs.utils.ClockGate)
    private val perfWake = if (ZJPerf.enabled) {
        val ctrl = Module(new LogPerfHelper).io
        ctrl.clean | ctrl.dump
    } else {
        false.B
    }
    cg.io.TE  := io.te | io.test_on.getOrElse(false.B) | perfWake
    cg.io.CK  := clock
    io.ock    := cg.io.Q
    cg.io.E   := Mux(io.test_off.getOrElse(false.B), false.B, cken | io.inbound)
    io.active := active
    cken      := Mux(io.inbound, true.B, Mux(cken, ckenWindowCnt.orR || idleWindowCnt.orR, false.B))
    active    := Mux(io.inbound, true.B, Mux(active, ckenWindowCnt.orR || idleWindowCnt.orR, false.B))
    when(io.inbound) {
        ckenWindowCnt := Fill(ckenWindowCnt.getWidth, true.B)
    }.elsewhen(ckenWindowCnt.orR) {
        ckenWindowCnt := ckenWindowCnt - 1.U
    }
    when(io.working) {
        idleWindowCnt := Fill(idleWindowCnt.getWidth, true.B)
    }.elsewhen(idleWindowCnt.orR) {
        idleWindowCnt := idleWindowCnt - 1.U
    }
}
