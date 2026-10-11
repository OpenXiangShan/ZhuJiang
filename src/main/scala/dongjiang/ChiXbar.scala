package dongjiang

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import zhujiang.chi._
import dongjiang.bundle._
import dongjiang.utils._
import xs.utils.debug.HAssert

class ChiXbar(implicit p: Parameters) extends DJModule {

    val io = IO(new Bundle {

        val rxReq = new Bundle {
            val inVec  = Vec(nrIcn, Flipped(Decoupled(new ReqFlit(false))))
            val outVec = Vec(djparam.nrDirBank, Decoupled(new ReqFlit(false)))
        }

        val rxHpr = new Bundle {
            val inVec  = if (hasHPR) Some(Vec(nrIcn, Flipped(Decoupled(new ReqFlit(false))))) else None
            val outVec = Vec(djparam.nrDirBank, Decoupled(new ReqFlit(false)))
        }

        val rxSnp = new Bundle {
            val in     = if (hasBBN) Some(Flipped(Decoupled(new SnoopFlit()))) else None
            val outVec = if (hasBBN) Some(Vec(djparam.nrDirBank, Decoupled(new SnoopFlit()))) else None
        }

        val txReq = new Bundle {
            val in     = Flipped(Decoupled(new ReqFlit(true)))
            val outVec = Vec(nrIcn, Decoupled(new ReqFlit(true)))
        }

        val txSnp = new Bundle {
            val in     = Flipped(Decoupled(new SnoopFlit()))
            val outVec = Vec(nrIcn, Decoupled(new SnoopFlit()))
        }

        val txRsp = new Bundle {
            val in     = Flipped(Decoupled(new RespFlit()))
            val outVec = Vec(nrIcn, Decoupled(new RespFlit()))
        }

        val txDat = new Bundle {
            val in     = Flipped(Decoupled(new DataFlit()))
            val outVec = Vec(nrIcn, Decoupled(new DataFlit()))
        }

        val cBusy = Input(UInt(3.W))
    })
    require(nrIcn <= 3)
    require(nrLanIcn <= 2)

    def rxRedir[T <: Bundle](inVec: Seq[DecoupledIO[T]], outVec: Seq[DecoupledIO[T]], inDirIdVec: Seq[UInt]): Unit = {
        val redirects = Seq.fill(inVec.size) { Seq.fill(outVec.size) { WireInit(0.U.asTypeOf(inVec(0))) } }

        redirects.zip(inVec).zipWithIndex.foreach { case ((redirs, in), i) =>
            val readyVec = Wire(Vec(outVec.size, Bool()))
            redirs.zipWithIndex.foreach { case (redir, j) =>
                redir.valid := in.valid & inDirIdVec(i) === j.U
                redir.bits  := in.bits
                readyVec(j) := redir.ready
            }
            in.ready := readyVec(inDirIdVec(i))
        }

        outVec.zip(redirects.transpose).foreach { case (out, redirs) =>
            out <> fastQosRRArb(redirs)
        }
    }

    val reqRedirVec = Wire(chiselTypeOf(io.rxReq.outVec))
    rxRedir(io.rxReq.inVec, reqRedirVec, io.rxReq.inVec.map(in => getDirBank(in.bits.Addr)))

    val hprRedirVec = WireInit(0.U.asTypeOf(io.rxHpr.outVec))
    if (hasHPR) { rxRedir(io.rxHpr.inVec.get, hprRedirVec, io.rxHpr.inVec.get.map(in => getDirBank(in.bits.Addr))) }

    if (hasBBN) { rxRedir(Seq(io.rxSnp.in.get), io.rxSnp.outVec.get, Seq(getDirBank(Cat(io.rxSnp.in.map(_.bits.Addr).get, 0.U(3.W))))) }

    io.rxReq.outVec.zip(io.rxHpr.outVec).zipWithIndex.foreach { case ((req, hpr), i) =>
        when(reqRedirVec(i).bits.QoS === 0xf.U) {
            req.valid := false.B
            req.bits  := DontCare
            if (hasHPR) {
                hpr <> fastArb(Seq(hprRedirVec(i), reqRedirVec(i)))
            } else {
                hpr <> reqRedirVec(i)
            }

        }.otherwise {
            req <> reqRedirVec(i)
            if (hasHPR) {
                hpr <> hprRedirVec(i)
            } else {
                hpr.valid := false.B
                hpr.bits  := DontCare
            }
        }

        HAssert.withEn(hpr.bits.QoS === 0xf.U, hpr.valid)
    }

    if (nrIcn == 1) {
        io.txReq.outVec.head <> io.txReq.in
        io.txSnp.outVec.head <> io.txSnp.in
        io.txRsp.outVec.head <> io.txRsp.in
        io.txDat.outVec.head <> io.txDat.in
    } else {

        io.txReq.outVec.head.valid := io.txReq.in.valid & NocType.txIs(io.txReq.in.bits, LAN)
        io.txSnp.outVec.head.valid := io.txReq.in.valid & NocType.txIs(io.txSnp.in.bits, LAN)
        io.txRsp.outVec.head.valid := io.txReq.in.valid & NocType.txIs(io.txRsp.in.bits, LAN)
        io.txDat.outVec.head.valid := io.txReq.in.valid & NocType.txIs(io.txDat.in.bits, LAN)

        io.txReq.outVec.last.valid := io.txReq.in.valid & NocType.txIs(io.txReq.in.bits, BBN)
        io.txSnp.outVec.last.valid := io.txReq.in.valid & NocType.txIs(io.txSnp.in.bits, BBN)
        io.txRsp.outVec.last.valid := io.txReq.in.valid & NocType.txIs(io.txRsp.in.bits, BBN)
        io.txDat.outVec.last.valid := io.txReq.in.valid & NocType.txIs(io.txDat.in.bits, BBN)

        io.txReq.in.ready := Mux(NocType.txIs(io.txReq.in.bits, LAN), io.txReq.outVec.head.ready, io.txReq.outVec.last.ready)
        io.txSnp.in.ready := Mux(NocType.txIs(io.txSnp.in.bits, LAN), io.txSnp.outVec.head.ready, io.txSnp.outVec.last.ready)
        io.txRsp.in.ready := Mux(NocType.txIs(io.txRsp.in.bits, LAN), io.txRsp.outVec.head.ready, io.txRsp.outVec.last.ready)
        io.txDat.in.ready := Mux(NocType.txIs(io.txDat.in.bits, LAN), io.txDat.outVec.head.ready, io.txDat.outVec.last.ready)

        io.txReq.outVec.foreach(_.bits := io.txReq.in.bits)
        io.txSnp.outVec.foreach(_.bits := io.txSnp.in.bits)
        io.txRsp.outVec.foreach(_.bits := io.txRsp.in.bits)
        io.txDat.outVec.foreach(_.bits := io.txDat.in.bits)
    }

    io.txReq.outVec.foreach(_.bits.SrcID := 0.U)
    io.txSnp.outVec.foreach(_.bits.SrcID := 0.U)
    io.txRsp.outVec.foreach(_.bits.SrcID := 0.U)
    io.txDat.outVec.foreach(_.bits.SrcID := 0.U)

    io.txRsp.outVec.foreach(_.bits.CBusy := io.cBusy)
    io.txDat.outVec.foreach(_.bits.CBusy := io.cBusy)
}
