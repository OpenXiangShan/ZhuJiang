package dongjiang.frontend

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import zhujiang.chi._
import dongjiang._
import dongjiang.utils._
import dongjiang.bundle.ChiChannel._
import xs.utils.debug._
import zhujiang.chi.ReqOpcode._
import dongjiang.bundle._
import utility.GTimer

class ReqToChiTask(implicit p: Parameters) extends DJModule {

    val io = IO(new Bundle {

        val config = new DJConfigIO()

        val rxReq = Flipped(Decoupled(new ReqFlit(false)))

        val chiTask = Decoupled(new PackChi with HasAddr with HasQoS)
    })

    io.chiTask.valid := io.rxReq.valid
    io.rxReq.ready   := io.chiTask.ready

    val req  = io.rxReq.bits
    val task = io.chiTask.bits
    task.addr        := req.Addr
    task.qos         := req.QoS
    task.chi.toLAN   := task.Addr.isToLAN(io.config.ci)
    task.chi.fromLAN := NocType.rxIs(req, LAN)
    task.chi.nodeId  := req.SrcID
    task.chi.channel := REQ
    task.chi.opcode  := req.Opcode
    task.chi.txnID   := req.TxnID
    task.chi.perfIngressCycle := GTimer()

    task.chi.order      := req.Order
    task.chi.snpAttr    := req.SnpAttr
    task.chi.snoopMe    := req.SnoopMe
    task.chi.memAttr    := req.MemAttr.asTypeOf(task.chi.memAttr)
    task.chi.expCompAck := req.ExpCompAck
    task.chi.size       := req.Size

    task.chi.fwdNID   := DontCare
    task.chi.fwdTxnID := DontCare
    task.chi.retToSrc := DontCare

    task.chi.dataVec(1) := req.Size === 6.U | req.Addr(5)
    task.chi.dataVec(0) := req.Size === 6.U | !req.Addr(5)
    require(djparam.nrBeat == 2)

    when(io.rxReq.valid) {
        HAssert(!task.chi.memAttr.device, s"MemAttr = ${task.chi.memAttr}")
        HAssert.withEn(task.chi.isFullSize, task.chi.isAllocatingRead | task.chi.isDataless | task.chi.isWriteFull)
    }

    HardwareAssertion.placePipe(1)
}

class SnpToChiTask(implicit p: Parameters) extends DJModule {

    val io = IO(new Bundle {

        val config = new DJConfigIO()

        val rxSnp = Flipped(Decoupled(new SnoopFlit()))

        val chiTask = Decoupled(new PackChi with HasAddr with HasQoS)
    })

    HardwareAssertion(!io.rxSnp.valid)

    io.chiTask.valid := io.rxSnp.valid
    io.rxSnp.ready   := io.chiTask.ready

    val snp  = io.rxSnp.bits
    val task = io.chiTask.bits
    task.addr        := Cat(snp.Addr, 0.U(3.W))
    task.qos         := snp.QoS
    task.chi.toLAN   := true.B
    task.chi.fromLAN := false.B
    task.chi.nodeId  := snp.SrcID
    task.chi.channel := SNP
    task.chi.opcode  := snp.Opcode
    task.chi.txnID   := snp.TxnID
    task.chi.perfIngressCycle := 0.U

    task.chi.order      := DontCare
    task.chi.snpAttr    := DontCare
    task.chi.snoopMe    := DontCare
    task.chi.memAttr    := DontCare
    task.chi.expCompAck := DontCare
    task.chi.size       := "b110".U

    task.chi.fwdNID   := snp.FwdNID
    task.chi.fwdTxnID := snp.FwdTxnID
    task.chi.retToSrc := snp.RetToSrc

    task.chi.dataVec(1) := true.B
    task.chi.dataVec(0) := true.B

    when(io.rxSnp.valid) {

        HardwareAssertion(task.chi.fromBBN)
        HardwareAssertion(task.chi.bbnCI =/= io.config.ci)
        HardwareAssertion(task.chi.bbnBId === io.config.bankId)

        HardwareAssertion(task.chi.snpIsLegal)

        HardwareAssertion(task.Addr.ci === io.config.ci)

    }

    HardwareAssertion.placePipe(1)
}
