package zhujiang.perf

import chisel3._
import org.chipsalliance.cde.config.Parameters
import utility.{LogPerfIO, XSPerfCounterScope, XSPerfLevel}
import utility.XSPerfLevel.XSPerfLevel
import xs.utils.perf.LogPerfHelper

private object ScopedPerfControl {
  def apply(): LogPerfIO = {
    val legacy = Module(new LogPerfHelper).io
    val ctrl = Wire(new LogPerfIO)
    ctrl.timer := legacy.timer
    ctrl.logEnable := legacy.logEnable
    ctrl.clean := legacy.clean
    ctrl.dump := legacy.dump
    ctrl
  }
}

abstract class ScopedPerf {
  private val scope = new XSPerfCounterScope

  def accumulate(perfName: String, perfCnt: UInt, perfLevel: XSPerfLevel = XSPerfLevel.VERBOSE)
                (implicit p: Parameters): Unit =
    scope.accumulate(perfName, perfCnt, perfLevel)

  def accumulate(events: Seq[(String, UInt)])(implicit p: Parameters): Unit =
    scope.accumulate(events)

  def max(perfName: String, perfCnt: UInt, enable: Bool, perfLevel: XSPerfLevel = XSPerfLevel.VERBOSE)
         (implicit p: Parameters): Unit =
    scope.max(perfName, perfCnt, enable, perfLevel)

  def histogram(
    perfName: String,
    perfCnt: UInt,
    enable: Bool,
    start: Int,
    stop: Int,
    step: Int = 1,
    leftStrict: Boolean = false,
    rightStrict: Boolean = false,
    perfLevel: XSPerfLevel = XSPerfLevel.VERBOSE
  )(implicit p: Parameters): Unit =
    scope.histogram(perfName, perfCnt, enable, start, stop, step, leftStrict, rightStrict, perfLevel)

  def latency(perfName: String, perfCnt: UInt, enable: Bool)(implicit p: Parameters): Unit = {
    accumulate(s"${perfName}_sum", Mux(enable, perfCnt, 0.U))
    accumulate(s"${perfName}_sampled", enable)
    histogram(s"${perfName}_0_50", perfCnt, enable, 0, 50, 1,
      leftStrict = true, rightStrict = true)
    histogram(s"${perfName}_50_200", perfCnt, enable, 50, 200, 10,
      leftStrict = true, rightStrict = true)
    histogram(s"${perfName}_200_1000", perfCnt, enable, 200, 1000, 100,
      leftStrict = true, rightStrict = true)
  }

  def collect()(implicit p: Parameters): Unit = scope.collect(ScopedPerfControl())
}

object ZhuJiangPerf extends ScopedPerf

object HomeWrapperPerf extends ScopedPerf
