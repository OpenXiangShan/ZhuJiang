package zhujiang.perf

import chisel3._
import org.chipsalliance.cde.config.{Field, Parameters}
import xs.utils.perf.{PerfCounterOptionsKey => LocalPerfCounterOptionsKey}
import xs.utils.perf.{XSPerfAccumulate => LocalXSPerfAccumulate}
import xs.utils.perf.{XSPerfHistogram => LocalXSPerfHistogram}
import xs.utils.perf.{XSPerfMax => LocalXSPerfMax}
import xs.utils.perf.{XSPerfMin => LocalXSPerfMin}

object ZJPerfLevel extends Enumeration {
    type ZJPerfLevel = Value

    val VERBOSE  = Value(0, "VERBOSE")
    val NORMAL   = Value("NORMAL")
    val CRITICAL = Value("CRITICAL")
}

object ZJPerfScopeKind extends Enumeration {
    type ZJPerfScopeKind = Value

    val Normal     = Value("Normal")
    val Definition = Value("Definition")
}

case class HistogramRange(start: Int, stop: Int, step: Int) {
    require(start >= 0, "histogram range start must be non-negative")
    require(stop > start, "histogram range stop must be greater than start")
    require(step > 0, "histogram range step must be positive")
    require((stop - start) % step == 0, "histogram range must contain complete bins")

    private[perf] val bins: Seq[(Int, Int)] =
        (start until stop by step).map(binStart => (binStart, binStart + step))
}

object HistogramRange {
    private[perf] def validate(ranges: Seq[HistogramRange]): Unit = {
        require(ranges.nonEmpty, "histogram ranges must be non-empty")
        ranges.sliding(2).foreach {
            case Seq(left, right) =>
                require(left.stop == right.start, "histogram ranges must be ordered and contiguous")
            case _ =>
        }
    }
}

private[perf] object ZJPerfDistribution {
    def counters(name: String, value: UInt, enable: Bool, ranges: Seq[HistogramRange]): Seq[(String, UInt)] = {
        HistogramRange.validate(ranges)
        val summary = Seq(
            s"${name}_sum"       -> Mux(enable, value, 0.U),
            s"${name}_sampled"   -> enable,
            s"${name}_underflow" -> (enable && value < ranges.head.start.U),
            s"${name}_overflow"  -> (enable && value >= ranges.last.stop.U)
        )
        val buckets = ranges.flatMap(_.bins).map { case (start, stop) =>
            s"${name}_${start}_${stop}" -> (enable && value >= start.U && value < stop.U)
        }
        summary ++ buckets
    }
}

sealed trait ZJPerfEventSpec {
    def name: String
    def level: ZJPerfLevel.Value
    def ownerId: Int
}

case class ZJPerfAccumulateSpec(name: String, level: ZJPerfLevel.Value, ownerId: Int) extends ZJPerfEventSpec

case class ZJPerfMaxSpec(name: String, level: ZJPerfLevel.Value, ownerId: Int) extends ZJPerfEventSpec

case class ZJPerfDistributionSpec(
    name: String,
    ranges: Seq[HistogramRange],
    includeMinMax: Boolean,
    level: ZJPerfLevel.Value,
    ownerId: Int
) extends ZJPerfEventSpec

case class ZJPerfHistogramSpec(
    name: String,
    start: Int,
    stop: Int,
    step: Int,
    leftStrict: Boolean,
    rightStrict: Boolean,
    level: ZJPerfLevel.Value,
    ownerId: Int
) extends ZJPerfEventSpec

class ZJPerfEvent extends Bundle {
    val value  = UInt(64.W)
    val enable = Bool()
}

class ZJPerfExport(val specs: Seq[ZJPerfEventSpec]) extends Bundle {
    val events = Vec(specs.size, new ZJPerfEvent)
}

// Backend-independent operations used by one performance instrumentation scope.
trait ZJPerfBackendScope {
    def accumulate(name: String, value: UInt, level: ZJPerfLevel.Value)(implicit p: Parameters): Unit

    def max(name: String, value: UInt, enable: Bool, level: ZJPerfLevel.Value)(implicit p: Parameters): Unit

    def distribution(
        name: String,
        value: UInt,
        enable: Bool,
        ranges: Seq[HistogramRange],
        includeMinMax: Boolean,
        level: ZJPerfLevel.Value
    )(implicit p: Parameters): Unit

    def histogram(
        name: String,
        value: UInt,
        enable: Bool,
        start: Int,
        stop: Int,
        step: Int,
        leftStrict: Boolean,
        rightStrict: Boolean,
        level: ZJPerfLevel.Value
    )(implicit p: Parameters): Unit

    def collect()(implicit p: Parameters): ZJPerfExport
}

// A backend creates scopes and consumes exports produced by Definition scopes.
// Implementations decide whether events are registered locally or exported.
trait ZJPerfBackend {
    def enabled(implicit p: Parameters): Boolean

    def newScope(kind: ZJPerfScopeKind.Value): ZJPerfBackendScope

    def consume(exports: Seq[ZJPerfExport])(implicit p: Parameters): Unit
}

// Parent designs override this key to select a different backend.
case object ZJPerfBackendKey extends Field[ZJPerfBackend](ZhuJiangLocalPerfBackend)

// The standalone ZhuJiang backend registers events directly with xs.utils.perf.
// It does not need to export or consume hardware event bundles.
object ZhuJiangLocalPerfBackend extends ZJPerfBackend {
    override def enabled(implicit p: Parameters): Boolean =
        p(LocalPerfCounterOptionsKey).enablePerfPrint

    override def newScope(kind: ZJPerfScopeKind.Value): ZJPerfBackendScope = new ZJPerfBackendScope {
        private def levelEnabled(level: ZJPerfLevel.Value)(implicit p: Parameters): Boolean =
            level.id >= p(LocalPerfCounterOptionsKey).perfLevel.id

        override def accumulate(name: String, value: UInt, level: ZJPerfLevel.Value)(implicit p: Parameters): Unit =
            if (levelEnabled(level)) LocalXSPerfAccumulate(name, value)

        override def max(
            name: String,
            value: UInt,
            enable: Bool,
            level: ZJPerfLevel.Value
        )(implicit p: Parameters): Unit =
            if (levelEnabled(level)) LocalXSPerfMax(name, value, enable)

        override def distribution(
            name: String,
            value: UInt,
            enable: Bool,
            ranges: Seq[HistogramRange],
            includeMinMax: Boolean,
            level: ZJPerfLevel.Value
        )(implicit p: Parameters): Unit = {
            if (levelEnabled(level)) {
                LocalXSPerfAccumulate(ZJPerfDistribution.counters(name, value, enable, ranges))
                if (includeMinMax) {
                    LocalXSPerfMin(name, value, enable)
                    LocalXSPerfMax(name, value, enable)
                }
            }
        }

        override def histogram(
            name: String,
            value: UInt,
            enable: Bool,
            start: Int,
            stop: Int,
            step: Int,
            leftStrict: Boolean,
            rightStrict: Boolean,
            level: ZJPerfLevel.Value
        )(implicit p: Parameters): Unit = {
            if (levelEnabled(level)) {
                LocalXSPerfHistogram(name, value, enable, start, stop, step, leftStrict, rightStrict)
            }
        }

        override def collect()(implicit p: Parameters): ZJPerfExport = {
            val output = Wire(new ZJPerfExport(Seq.empty))
            output := DontCare
            output
        }
    }

    override def consume(exports: Seq[ZJPerfExport])(implicit p: Parameters): Unit = ()
}

import zhujiang.perf.ZJPerfLevel.ZJPerfLevel
import zhujiang.perf.ZJPerfScopeKind.ZJPerfScopeKind

// Internal scope implementation. The public ZJPerf facade selects which
// scope receives instrumentation calls at a module boundary.
private class ScopedPerf(kind: ZJPerfScopeKind) {
    private var active: Option[(ZJPerfBackend, ZJPerfBackendScope)] = None

    private def scope(implicit p: Parameters): ZJPerfBackendScope = active match {
        case Some((backend, value)) =>
            require(backend eq p(ZJPerfBackendKey), "performance backend changed inside one scope")
            value
        case None =>
            val backend = p(ZJPerfBackendKey)
            val value   = backend.newScope(kind)
            active = Some((backend, value))
            value
    }

    def accumulate(
        perfName: String,
        perfCnt: UInt,
        perfLevel: ZJPerfLevel = ZJPerfLevel.VERBOSE
    )(implicit p: Parameters): Unit =
        scope.accumulate(perfName, perfCnt, perfLevel)

    def accumulate(events: Seq[(String, UInt)])(implicit p: Parameters): Unit =
        events.foreach { case (perfName, perfCnt) => accumulate(perfName, perfCnt) }

    def max(
        perfName: String,
        perfCnt: UInt,
        enable: Bool,
        perfLevel: ZJPerfLevel = ZJPerfLevel.VERBOSE
    )(implicit p: Parameters): Unit = {
        require(!perfName.endsWith("_max"), s"maximum performance counter '$perfName' must use a base name")
        scope.max(perfName, perfCnt, enable, perfLevel)
    }

    def distribution(
        perfName: String,
        perfCnt: UInt,
        enable: Bool,
        ranges: Seq[HistogramRange],
        includeMinMax: Boolean,
        perfLevel: ZJPerfLevel = ZJPerfLevel.VERBOSE
    )(implicit p: Parameters): Unit = {
        HistogramRange.validate(ranges)
        scope.distribution(perfName, perfCnt, enable, ranges, includeMinMax, perfLevel)
    }

    def histogram(
        perfName: String,
        perfCnt: UInt,
        enable: Bool,
        start: Int,
        stop: Int,
        step: Int = 1,
        leftStrict: Boolean = false,
        rightStrict: Boolean = false,
        perfLevel: ZJPerfLevel = ZJPerfLevel.VERBOSE
    )(implicit p: Parameters): Unit =
        scope.histogram(perfName, perfCnt, enable, start, stop, step, leftStrict, rightStrict, perfLevel)

    // End the current scope and let the backend produce its optional export.
    def collect()(implicit p: Parameters): ZJPerfExport = {
        val backend = p(ZJPerfBackendKey)
        val current = active.getOrElse((backend, backend.newScope(kind)))
        active = None
        current._2.collect()
    }

    // Forward Definition-scope exports to the configured backend.
    def consume(exports: Seq[ZJPerfExport])(implicit p: Parameters): Unit =
        p(ZJPerfBackendKey).consume(exports)
}

// Unified performance counter API for ZhuJiang and HomeNode/DongJiang logic.
// The module boundary, rather than each call site, selects the active scope.
object ZJPerf {
    val DefaultHistogramRanges: Seq[HistogramRange] = Seq(
        HistogramRange(0, 50, 1),
        HistogramRange(50, 200, 10),
        HistogramRange(200, 1000, 100)
    )

    private val normalScope = new ScopedPerf(ZJPerfScopeKind.Normal)
    private var scopeStack = List(normalScope)

    private def currentScope: ScopedPerf = scopeStack.head

    def enabled(implicit p: Parameters): Boolean = p(ZJPerfBackendKey).enabled

    def whenEnabled(instrumentation: => Unit)(implicit p: Parameters): Unit =
        if (enabled) instrumentation

    // Reusable Definition modules call this before elaborating their children.
    def beginDefinition(): Unit = {
        scopeStack = new ScopedPerf(ZJPerfScopeKind.Definition) :: scopeStack
    }

    def accumulate(
        perfName: String,
        perfCnt: => UInt,
        perfLevel: ZJPerfLevel = ZJPerfLevel.VERBOSE
    )(implicit p: Parameters): Unit =
        if (enabled) currentScope.accumulate(perfName, perfCnt, perfLevel)

    def accumulate(events: => Seq[(String, UInt)])(implicit p: Parameters): Unit =
        if (enabled) currentScope.accumulate(events)

    def max(
        perfName: String,
        perfCnt: => UInt,
        enable:  => Bool,
        perfLevel: ZJPerfLevel = ZJPerfLevel.VERBOSE
    )(implicit p: Parameters): Unit =
        if (enabled) currentScope.max(perfName, perfCnt, enable, perfLevel)

    def distribution(
        perfName: String,
        perfCnt: => UInt,
        enable:  => Bool,
        ranges: Seq[HistogramRange] = DefaultHistogramRanges,
        includeMinMax: Boolean = true
    )(implicit p: Parameters): Unit =
        if (enabled) currentScope.distribution(perfName, perfCnt, enable, ranges, includeMinMax)

    def histogram(
        perfName: String,
        perfCnt: => UInt,
        enable:  => Bool,
        start: Int,
        stop: Int,
        step: Int = 1,
        leftStrict: Boolean = false,
        rightStrict: Boolean = false,
        perfLevel: ZJPerfLevel = ZJPerfLevel.VERBOSE
    )(implicit p: Parameters): Unit =
        if (enabled) {
            currentScope.histogram(perfName, perfCnt, enable, start, stop, step, leftStrict, rightStrict, perfLevel)
        }

    // End the active scope. Definition scopes are removed so the parent
    // module resumes using its previous scope.
    def collect()(implicit p: Parameters): ZJPerfExport = {
        val output = currentScope.collect()
        if (scopeStack.tail.nonEmpty) scopeStack = scopeStack.tail
        output
    }

    // The parent calls this after instantiating a Definition and wiring its export.
    def consume(exports: => Seq[ZJPerfExport])(implicit p: Parameters): Unit =
        if (enabled) currentScope.consume(exports)
}
