package dongjiang.data

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import zhujiang.chi._
import dongjiang._
import dongjiang.utils._
import dongjiang.bundle._
import xs.utils.debug._
import zhujiang.perf.HomeWrapperPerf
import xs.utils.queue.FastQueue
import chisel3.experimental.BundleLiterals._
import dongjiang.backend.UpdHnTxnID

class DBIDPool(implicit p: Parameters) extends DJModule {
    val io = IO(new Bundle {
        val enq0 = Flipped(Valid(UInt(dbIdBits.W)))
        val enq1 = Flipped(Valid(UInt(dbIdBits.W)))
        val deq0 = Decoupled(UInt(dbIdBits.W))
        val deq1 = Decoupled(UInt(dbIdBits.W))
    })
    require(djparam.nrBeat == 2)

    val q0 = Module(new FastQueue(UInt(dbIdBits.W), djparam.nrDataBuf / 2, false))
    val q1 = Module(new FastQueue(UInt(dbIdBits.W), djparam.nrDataBuf / 2, false))
    dontTouch(q0.io)
    dontTouch(q1.io)

    val rstCounter = Counter(djparam.nrDataBuf / 2)
    val rstDoneReg = RegEnable(true.B, false.B, rstCounter.inc())

    val enqOne   = io.enq0.valid ^ io.enq1.valid
    val enqTwo   = io.enq0.valid & io.enq1.valid
    val enqSel0  = io.enq0.valid
    val enqSelQ0 = q0.io.count <= q1.io.count
    dontTouch(enqOne)
    dontTouch(enqTwo)
    dontTouch(enqSel0)
    dontTouch(enqSelQ0)

    when(!rstDoneReg) {
        q0.io.enq.valid := true.B
        q1.io.enq.valid := true.B
        q0.io.enq.bits  := Cat(0.U, rstCounter.value)
        q1.io.enq.bits  := Cat(1.U, rstCounter.value)

    }.elsewhen(enqOne) {
        q0.io.enq.valid := enqSelQ0
        q1.io.enq.valid := !enqSelQ0
        q0.io.enq.bits  := Mux(enqSel0, io.enq0.bits, io.enq1.bits)
        q1.io.enq.bits  := Mux(enqSel0, io.enq0.bits, io.enq1.bits)

    }.otherwise {
        q0.io.enq.valid := enqTwo
        q1.io.enq.valid := enqTwo
        q0.io.enq.bits  := io.enq0.bits
        q1.io.enq.bits  := io.enq1.bits
    }
    HAssert.withEn(q0.io.enq.ready, q0.io.enq.valid)
    HAssert.withEn(q1.io.enq.ready, q1.io.enq.valid)

    val deqOne   = io.deq0.ready ^ io.deq1.ready
    val deqTwo   = io.deq0.ready & io.deq1.ready
    val deqSel0  = io.deq0.ready
    val deqSelQ0 = q0.io.count >= q1.io.count
    dontTouch(deqOne)
    dontTouch(deqTwo)
    dontTouch(deqSel0)
    dontTouch(deqSelQ0)

    when(deqOne) {
        q0.io.deq.ready := deqSelQ0
        q1.io.deq.ready := !deqSelQ0
        io.deq0.bits    := Mux(deqSelQ0, q0.io.deq.bits, q1.io.deq.bits)
        io.deq1.bits    := Mux(deqSelQ0, q0.io.deq.bits, q1.io.deq.bits)

    }.otherwise {
        q0.io.deq.ready := deqTwo
        q1.io.deq.ready := deqTwo
        io.deq0.bits    := q0.io.deq.bits
        io.deq1.bits    := q1.io.deq.bits
    }
    io.deq0.valid := q0.io.deq.valid & rstDoneReg
    io.deq1.valid := q1.io.deq.valid & rstDoneReg
    HAssert.withEn(q0.io.deq.bits =/= q1.io.deq.bits, q0.io.deq.valid & q1.io.deq.valid)
}

class DBIDCtrl(implicit p: Parameters) extends DJModule {
    val io = IO(new Bundle {
        val req     = Flipped(Decoupled(Vec(djparam.nrBeat, Bool())))
        val resp    = Vec(djparam.nrBeat, UInt(dbIdBits.W))
        val release = Flipped(Valid(new DBIDVec with HasDataVec))
    })

    val pool = Module(new DBIDPool)

    val hasTwo = pool.io.deq0.valid & pool.io.deq1.valid
    io.req.ready := hasTwo

    pool.io.deq0.ready := io.req.valid & io.req.bits(0) & hasTwo
    pool.io.deq1.ready := io.req.valid & io.req.bits(1) & hasTwo

    io.resp(0) := pool.io.deq0.bits
    io.resp(1) := pool.io.deq1.bits

    HAssert.withEn(pool.io.deq0.fire, io.req.fire & io.req.bits(0))
    HAssert.withEn(pool.io.deq1.fire, io.req.fire & io.req.bits(1))
    HAssert.withEn(io.req.bits.asUInt =/= 0.U, io.req.valid)

    pool.io.enq0.valid := io.release.valid & io.release.bits.dataVec(0)
    pool.io.enq1.valid := io.release.valid & io.release.bits.dataVec(1)

    pool.io.enq0.bits := io.release.bits.dbidVec(0)
    pool.io.enq1.bits := io.release.bits.dbidVec(1)
    HAssert.withEn(io.release.bits.dbidVec(0) =/= io.release.bits.dbidVec(1), pool.io.enq0.valid & pool.io.enq1.valid)

    private val dbidAvailableCount = pool.io.deq0.valid.asUInt +& pool.io.deq1.valid.asUInt
    HomeWrapperPerf.accumulate(
        Seq(
            ("zj_dbid_req_valid", io.req.valid),
            ("zj_dbid_req_fire", io.req.fire),
            ("zj_dbid_req_stall_empty", io.req.valid && !hasTwo),
            ("zj_dbid_release_valid", io.release.valid),
            ("zj_dbid_release_beat0", pool.io.enq0.valid),
            ("zj_dbid_release_beat1", pool.io.enq1.valid),
            ("zj_dbid_available_sum", dbidAvailableCount)
        )
    )
    HomeWrapperPerf.max("zj_dbid_available_max", dbidAvailableCount, true.B)

    HardwareAssertion.placePipe(1)
}
