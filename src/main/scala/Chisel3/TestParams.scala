package barstools.tapeout
import dsptools.{DspTesterOptionsManager, DspTesterOptions}
import chisel3.iotesters.TesterOptions
import firrtl._
import logger.LogLevel
import chisel3._

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
  }

  val options1TolWaveform = new DspTesterOptionsManager {
    dspTesterOptions = DspTesterOptions(
        fixTolLSBs = 1,
        genVerilogTb = false,
        isVerbose = true)
    testerOptions = testerOptionsGlobal.copy(waveform = Some(new java.io.File("test_run_dir/waveform.vcd")))
  }

  val debugBuild = new ExecutionOptionsManager("DebugBuild") with HasChiselExecutionOptions with HasFirrtlOptions {
    commonOptions = CommonOptions(
      globalLogLevel = LogLevel.Info,
      targetDirName = s"test_run_dir/DebugBuild"
    )
    firrtlOptions = FirrtlExecutionOptions(compilerName = "verilog") 
  }
   
}