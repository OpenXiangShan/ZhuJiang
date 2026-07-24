package xs.utils.perf

import chisel3._
import chisel3.util.HasBlackBoxInline

class LogPerfIO extends Bundle {
  val timer = UInt(64.W)
  val logEnable = Bool()
  val clean = Bool()
  val dump = Bool()
}

class LogPerfHelper extends BlackBox with HasBlackBoxInline {
  val io = IO(Output(new LogPerfIO))

  // Register C++ extension module for gsim compilation.
  // gsim compiles extmodule functions outside the generated SimTop methods,
  // so the SystemVerilog-only top-level control signals are not in scope.
  // Returning zeros disables performance logging, which is acceptable for
  // functional simulation.
  private val cppExtModule =
    """
      |void LogPerfHelper (
      |  uint64_t& timer,
      |  uint8_t&  logEnable,
      |  uint8_t&  clean,
      |  uint8_t&  dump
      |) {
      |  timer     = 0;
      |  logEnable = 0;
      |  clean     = 0;
      |  dump      = 0;
      |}
      |""".stripMargin
  difftest.DifftestModule.createCppExtModule("LogPerfHelper", cppExtModule)

  val sverilog =
    """`ifndef SIM_TOP_MODULE_NAME
      |  `define SIM_TOP_MODULE_NAME SimTop
      |`endif
      |
      |/*verilator tracing_off*/
      |
      |module LogPerfHelper (
      |  output [63:0] timer,
      |  output        logEnable,
      |  output        clean,
      |  output        dump
      |);
      |
      |  assign timer         = `SIM_TOP_MODULE_NAME.difftest_timer;
      |  assign logEnable     = `SIM_TOP_MODULE_NAME.difftest_log_enable;
      |  assign clean         = `SIM_TOP_MODULE_NAME.difftest_perfCtrl_clean;
      |  assign dump          = `SIM_TOP_MODULE_NAME.difftest_perfCtrl_dump;
      |
      |endmodule
      |
      |""".stripMargin
  setInline("LogPerfHelper.sv", sverilog)
}
