package xs.utils.arb

import chisel3._
import chisel3.util._

class BaseVipArbiter[T <: Data](gen: T, size:Int) extends BaseArbiter(gen, size) {
  val valids = VecInit(io.in.map(_.valid))
  val validMask = valids.asUInt

  val vipPtrOH = RegInit(1.U(size.W))
  private val defaultPtrOH = validMask & ((~validMask).asUInt + 1.U)
  private val vipSelValid = (validMask & vipPtrOH).orR
  private val selPtrOH = Mux(vipSelValid, vipPtrOH, defaultPtrOH)

  io.out.valid := validMask.orR
  io.out.bits := Mux1H(selPtrOH, io.in.map(_.bits))
  io.chosen := OHToUInt(selPtrOH)
  for(i <- io.in.indices) {
    io.in(i).ready := selPtrOH(i) && io.out.ready
  }
  val vipPtrMove = (validMask & (~vipPtrOH).asUInt).orR && Mux(vipSelValid, io.out.fire, true.B)
  val vipPtrNext = Wire(UInt(size.W))
  when(vipPtrMove) {
    vipPtrOH := vipPtrNext
  }
  assert(PopCount(vipPtrOH) === 1.U)
}

class VipArbiter[T <: Data](gen: T, size:Int) extends BaseVipArbiter(gen, size) {
  // Derive the ranges around the one-hot pointer without expanding a prefix
  // reduction for every bit. This keeps wide arbiters tractable for synthesis.
  private val lowMask = vipPtrOH - 1.U
  private val highMask = ~(vipPtrOH | lowMask)
  assert((highMask | lowMask | vipPtrOH).andR)

  private val highValidMask = highMask & validMask
  private val lowValidMask = lowMask & validMask
  private val nextValidMask = Mux(highValidMask.orR, highValidMask, lowValidMask)

  vipPtrNext := nextValidMask & ((~nextValidMask).asUInt + 1.U)
  when(vipPtrMove) {
    assert((vipPtrNext & validMask).orR)
  }
}

class StepVipArbiter[T <: Data](gen: T, size:Int) extends BaseVipArbiter(gen, size) {
  vipPtrNext := Cat(vipPtrOH(size - 2, 0), vipPtrOH(size - 1))
}

object BaseVipArbiter {
  def apply[T <: Data](in:Seq[DecoupledIO[T]], name:String = "normal"): DecoupledIO[T] = {
    val arb = name match {
      case "step" => Module(new StepVipArbiter(in.head.bits.cloneType, in.size))
      case _ => Module(new VipArbiter(in.head.bits.cloneType, in.size))
    }
    arb.io.in.zip(in).foreach({case(a, b) => a <> b})
    arb.io.out
  }
}

object VipArbiter {
  def apply[T <: Data](in:Seq[DecoupledIO[T]]): DecoupledIO[T] = BaseVipArbiter(in, "normal")
}

object StepVipArbiter {
  def apply[T <: Data](in:Seq[DecoupledIO[T]]): DecoupledIO[T] = BaseVipArbiter(in, "step")
}
