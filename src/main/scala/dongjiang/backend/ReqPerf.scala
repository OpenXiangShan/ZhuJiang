package dongjiang.backend

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import zhujiang.chi.ReqOpcode
import zhujiang.perf.{HistogramRange, ZJPerf}
import xs.utils.GTimer

object ReqPerfFamily {
    val width = 2

    val NONE     = 0.U(width.W)
    val READ     = 1.U(width.W)
    val WRITE    = 2.U(width.W)
    val PREFETCH = 3.U(width.W)
}

object ReqPerfPath {
    val width = 2

    val HIT     = 0.U(width.W)
    val DCT     = 1.U(width.W)
    val DMT     = 2.U(width.W)
    val NON_DMT = 3.U(width.W)
}

class ReqPerfTrace extends Bundle {
    val valid        = Bool()
    val family       = UInt(ReqPerfFamily.width.W)
    val opcode       = UInt(ReqOpcode.width.W)
    val llcHit       = Bool()
    val sawDMT       = Bool()
    val sawDCT       = Bool()
    val ingressCycle = UInt(64.W)
    val frontLatency = UInt(64.W)
}

class ReqPerfSample extends Bundle {
    val trace        = new ReqPerfTrace
    val totalLatency = UInt(64.W)
}

class ServiceLatencyTracker extends Module {
    val io = IO(new Bundle {
        val start  = Input(Bool())
        val finish = Input(Bool())
        val sample = Output(Valid(UInt(64.W)))
    })

    val timer      = GTimer()
    val active     = RegInit(false.B)
    val startCycle = Reg(UInt(64.W))

    io.sample.valid := active && io.finish
    io.sample.bits  := timer - startCycle

    when(io.start) {
        assert(!active, "service latency start while active")
        active     := true.B
        startCycle := timer
    }.elsewhen(io.finish) {
        assert(active, "service latency finish while inactive")
        active := false.B
    }
}

object ReqPerf {
    val LocalLatencyRanges: Seq[HistogramRange] = Seq(
        HistogramRange(0, 4, 4),
        HistogramRange(4, 8, 4),
        HistogramRange(8, 16, 8),
        HistogramRange(16, 32, 16),
        HistogramRange(32, 64, 32),
        HistogramRange(64, 128, 64),
        HistogramRange(128, 256, 128),
        HistogramRange(256, 512, 256),
        HistogramRange(512, 1024, 512),
        HistogramRange(1024, 2048, 1024),
        HistogramRange(2048, 4096, 2048)
    )

    val TotalLatencyRanges: Seq[HistogramRange] = LocalLatencyRanges ++ Seq(
        HistogramRange(4096, 8192, 4096),
        HistogramRange(8192, 16384, 8192),
        HistogramRange(16384, 32768, 16384),
        HistogramRange(32768, 65536, 32768),
        HistogramRange(65536, 131072, 65536)
    )

    def family(opcode: UInt, isRead: Bool, isWrite: Bool): UInt = {
        Mux(
            opcode === ReqOpcode.StashOnceShared,
            ReqPerfFamily.PREFETCH,
            Mux(isRead, ReqPerfFamily.READ, Mux(isWrite, ReqPerfFamily.WRITE, ReqPerfFamily.NONE))
        )
    }

    def path(llcHit: Bool, sawDCT: Bool, sawDMT: Bool): UInt = {
        Mux(
            llcHit,
            ReqPerfPath.HIT,
            Mux(sawDCT, ReqPerfPath.DCT, Mux(sawDMT, ReqPerfPath.DMT, ReqPerfPath.NON_DMT))
        )
    }

    def nextSawDMT(oldSawDMT: Bool, readTaskFire: Bool, doDMT: Bool): Bool =
        oldSawDMT || (readTaskFire && doDMT)

    def nextSawDCT(oldSawDCT: Bool, cmRespHit: Bool, forwarded: Bool): Bool =
        oldSawDCT || (cmRespHit && forwarded)

    // One CM can retire more than one entry in a cycle through independent ports.
    // Aggregate every completed sample into one histogram instead of dropping one.
    def aggregateDistribution(name: String, samples: Seq[ValidIO[UInt]], ranges: Seq[HistogramRange])(implicit p: Parameters): Unit = {
        require(samples.nonEmpty)
        require(ranges.nonEmpty)
        ranges.sliding(2).foreach {
            case Seq(left, right) => require(left.stop == right.start, "histogram ranges must be ordered and contiguous")
            case _                =>
        }

        val sum       = samples.map(s => Mux(s.valid, s.bits, 0.U)).reduce(_ +& _)
        val sampled   = PopCount(samples.map(_.valid))
        val underflow = PopCount(samples.map(s => s.valid && s.bits < ranges.head.start.U))
        val overflow  = PopCount(samples.map(s => s.valid && s.bits >= ranges.last.stop.U))
        val buckets = ranges.flatMap(range => (range.start until range.stop by range.step).map(start => (start, start + range.step))).map { case (start, stop) =>
            s"${name}_${start}_${stop}" -> PopCount(samples.map(s => s.valid && s.bits >= start.U && s.bits < stop.U))
        }

        ZJPerf.accumulate(
            Seq(
                s"${name}_sum"       -> sum,
                s"${name}_sampled"   -> sampled,
                s"${name}_underflow" -> underflow,
                s"${name}_overflow"  -> overflow
            ) ++ buckets
        )
    }
}
