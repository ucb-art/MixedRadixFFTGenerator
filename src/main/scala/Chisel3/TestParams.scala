package barstools.tapeout
import dsptools.{DspTesterOptionsManager, DspTesterOptions}
import chisel3.iotesters.TesterOptions
import firrtl._
import logger.LogLevel

object TestParams {

  val testerOptionsGlobal = TesterOptions(
      isVerbose = false,
      displayBase = 16,
      backendName = "verilator",
      isGenVerilog = true)

  val options0Tol = new DspTesterOptionsManager {
    dspTesterOptions = DspTesterOptions(
        fixTolLSBs = 0,
        genVerilogTb = false,
        isVerbose = true)
    testerOptions = testerOptionsGlobal
  }

  val options1Tol = new DspTesterOptionsManager {
    dspTesterOptions = DspTesterOptions(
        fixTolLSBs = 1,
        genVerilogTb = false,
        isVerbose = true)
    testerOptions = testerOptionsGlobal
    commonOptions = CommonOptions(globalLogLevel = LogLevel.Debug)
  }
}