package dongjiang.backend

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class ReplacementWritePolicyHarness extends Module {
  val io = IO(new Bundle {
    val toLan = Input(Bool())
    val dirty = Input(Bool())
    val issueWrite = Output(Bool())
    val saveLocalCleanVictim = Output(Bool())
  })

  io.issueWrite := ReplacementWritePolicy.issueWrite(io.toLan, io.dirty)
  io.saveLocalCleanVictim := ReplacementWritePolicy.saveLocalCleanVictim(io.toLan, io.dirty)
}

class ReplacementWritePolicySpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "ReplacementWritePolicy"

  it should "write only dirty local victims and every remote victim" in {
    test(new ReplacementWritePolicyHarness) { dut =>
      dut.io.toLan.poke(true.B)
      dut.io.dirty.poke(false.B)
      dut.io.issueWrite.expect(false.B)
      dut.io.saveLocalCleanVictim.expect(true.B)

      dut.io.dirty.poke(true.B)
      dut.io.issueWrite.expect(true.B)
      dut.io.saveLocalCleanVictim.expect(false.B)

      dut.io.toLan.poke(false.B)
      dut.io.dirty.poke(false.B)
      dut.io.issueWrite.expect(true.B)
      dut.io.saveLocalCleanVictim.expect(false.B)
    }
  }
}
