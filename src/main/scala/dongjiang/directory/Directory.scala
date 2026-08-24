package dongjiang.directory

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import zhujiang.chi._
import dongjiang._
import dongjiang.utils._
import dongjiang.bundle._
import xs.utils.debug._
import zhujiang.perf.HomeWrapperPerf
import xs.utils.mbist.MbistPipeline

class Directory(isTop: Boolean = false)(implicit p: Parameters) extends DJModule {
    override def isTopModule: Boolean = isTop

    val io = IO(new Bundle {
        val config = Input(new DJConfigIO())

        val readVec = Vec(djparam.nrDirBank, Flipped(Decoupled(new Addr with HasPackHnIdx)))

        val rRespVec = Vec(djparam.nrDirBank, Valid(new DirMsg))

        val write = Flipped(Decoupled(new DJBundle {
            val llc = Valid(new DirEntry("llc") with HasPackHnIdx with HasDirectAlloc)
            val sf  = Valid(new DirEntry("sf") with HasPackHnIdx with HasDirectAlloc)
        }))

        val wResp = new DJBundle {
            val llc = Valid(new DirEntry("llc") with HasHnTxnID)
            val sf  = Valid(new DirEntry("sf") with HasHnTxnID)
        }

        val unlock = Flipped(Valid(new PackHnIdx))
    })

    val llcs = Seq.fill(djparam.nrDirBank)(Module(new DirectoryBase("llc", !isTop)))
    val sfs  = Seq.fill(djparam.nrDirBank)(Module(new DirectoryBase("sf", !isTop)))

    llcs.zip(sfs).zipWithIndex.foreach { case ((llc, sf), i) =>
        llc.io.config  := io.config
        sf.io.config   := io.config
        llc.io.dirBank := i.U
        sf.io.dirBank  := i.U

        llc.io.read.valid := io.readVec(i).valid & sf.io.read.ready
        sf.io.read.valid  := io.readVec(i).valid & llc.io.read.ready

        llc.io.read.bits := io.readVec(i).bits
        sf.io.read.bits  := io.readVec(i).bits

        io.readVec(i).ready := llc.io.read.ready & sf.io.read.ready

        llc.io.write.valid := io.write.fire & io.write.bits.llc.valid & io.write.bits.llc.bits.Addr.dirBank === i.U
        sf.io.write.valid  := io.write.fire & io.write.bits.sf.valid & io.write.bits.sf.bits.Addr.dirBank === i.U

        llc.io.write.bits := io.write.bits.llc.bits
        sf.io.write.bits  := io.write.bits.sf.bits

        llc.io.unlock := io.unlock
        sf.io.unlock  := io.unlock
    }
    val llcWReady = VecInit(llcs.map(_.io.write.ready))(io.write.bits.llc.bits.Addr.dirBank)
    val sfWReady  = VecInit(sfs.map(_.io.write.ready))(io.write.bits.sf.bits.Addr.dirBank)
    io.write.ready := (llcWReady | !io.write.bits.llc.valid) & (sfWReady | !io.write.bits.sf.valid)
    HAssert.withEn(io.write.bits.llc.valid | io.write.bits.sf.valid, io.write.valid)

    private val readPerfEvents = io.readVec.zipWithIndex.flatMap { case (read, i) =>
        Seq(
            (s"zj_dir_read_valid_$i", read.valid),
            (s"zj_dir_read_fire_$i", read.fire),
            (s"zj_dir_read_stall_$i", read.valid && !read.ready)
        )
    }

    io.rRespVec.map(_.valid).zip(llcs.map(_.io.resp)).foreach { case (a, b) => a := b.valid & !b.bits.toRepl }
    io.rRespVec.map(_.bits.llc).zip(llcs.map(_.io.resp)).foreach { case (a, b) => a := b.bits }
    io.rRespVec.map(_.bits.sf).zip(sfs.map(_.io.resp)).foreach { case (a, b) => a := b.bits }

    val llcRespVec   = VecInit(llcs.map(_.io.resp))
    val llctoReplVec = VecInit(llcRespVec.map(r => r.valid & r.bits.toRepl))
    val llcToReplId  = PriorityEncoder(llctoReplVec)
    io.wResp.llc.valid := llctoReplVec.asUInt.orR
    io.wResp.llc.bits  := llcRespVec(llcToReplId).bits
    HAssert(PopCount(llctoReplVec) <= 1.U)

    val sfRespVec   = VecInit(sfs.map(_.io.resp))
    val sftoReplVec = VecInit(sfRespVec.map(r => r.valid & r.bits.toRepl))
    val sfToReplId  = PriorityEncoder(sftoReplVec)
    io.wResp.sf.valid := sftoReplVec.asUInt.orR
    io.wResp.sf.bits  := sfRespVec(sfToReplId).bits
    HAssert(PopCount(sftoReplVec) <= 1.U)

    HomeWrapperPerf.accumulate(
        readPerfEvents ++ Seq(
            ("zj_dir_write_valid", io.write.valid),
            ("zj_dir_write_fire", io.write.fire),
            ("zj_dir_write_stall", io.write.valid && !io.write.ready),
            ("zj_dir_write_llc_valid", io.write.valid && io.write.bits.llc.valid),
            ("zj_dir_write_sf_valid", io.write.valid && io.write.bits.sf.valid),
            ("zj_dir_llc_resp_to_repl", llctoReplVec.asUInt.orR),
            ("zj_dir_sf_resp_to_repl", sftoReplVec.asUInt.orR)
        )
    )

    HardwareAssertion.placePipe(2)
}
