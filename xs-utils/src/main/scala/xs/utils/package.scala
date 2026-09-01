package xs

import chisel3.Data

package object utils {
  type SRAMQueue[T <: Data] = _root_.xs.utils.sram.SramQueue[T]
  type CircularQueuePtr[T <: CircularQueuePtr[T]] = _root_.xs.utils.queue.CircularQueuePtr[T]
  type MimoQueue[T <: Data] = _root_.xs.utils.queue.MimoQueue[T]
  type OverrideableQueue[T <: Data] = _root_.xs.utils.queue.OverrideableQueue[T]
  type HasCircularQueuePtrHelper = _root_.xs.utils.queue.HasCircularQueuePtrHelper

  object DifftestCompat {
    def createCppExtModule(name: String, source: String): Unit = {
      try {
        val moduleClass = Class.forName("difftest.DifftestModule$")
        val module = moduleClass.getField("MODULE$").get(null)
        val method = moduleClass.getMethod("createCppExtModule", classOf[String], classOf[String])
        method.invoke(module, name, source)
      } catch {
        case _: ClassNotFoundException =>
        case error: ReflectiveOperationException =>
          throw new IllegalStateException("failed to register difftest C++ extension", error)
      }
    }
  }
}
