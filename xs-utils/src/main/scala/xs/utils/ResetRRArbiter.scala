package xs.utils
import chisel3.{Data, Module}
import chisel3.util.RRArbiter
import sifive.enterprise.firrtl.FullAsyncResetAnnotation

object ResetRRArbiter {
  def apply[T <: Data](gen: T, n: Int): RRArbiter[T] = {
    Module(new ResetRRArbiter(gen, n))
  }
}

class ResetRRArbiter[T <: Data](gen: T, n: Int) extends RRArbiter(gen, n) {
  chisel3.experimental.annotate(this)(Seq(FullAsyncResetAnnotation(reset.toNamed)))
}
