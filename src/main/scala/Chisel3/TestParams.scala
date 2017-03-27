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

  val options0TolQuiet = new DspTesterOptionsManager {
    dspTesterOptions = DspTesterOptions(
        fixTolLSBs = 0,
        genVerilogTb = false,
        isVerbose = false)
    testerOptions = testerOptionsGlobal
  }

  val options1Tol = new DspTesterOptionsManager {
    dspTesterOptions = DspTesterOptions(
        fixTolLSBs = 1,
        genVerilogTb = false,
        isVerbose = true)
    testerOptions = testerOptionsGlobal
  }

  val options1TolFir = new DspTesterOptionsManager {
    dspTesterOptions = DspTesterOptions(
        fixTolLSBs = 1,
        genVerilogTb = false,
        isVerbose = true)
    testerOptions = testerOptionsGlobal.copy(
      backendName = "firrtl"
    )
  }

  val options1TolWaveform = new DspTesterOptionsManager {
    dspTesterOptions = DspTesterOptions(
        fixTolLSBs = 1,
        genVerilogTb = false,
        isVerbose = true)
    testerOptions = testerOptionsGlobal.copy(waveform = Some(new java.io.File("test_run_dir/waveform.vcd")))
  }

  val options1TolWaveformTB = new DspTesterOptionsManager {
    dspTesterOptions = DspTesterOptions(
        fixTolLSBs = 1,
        genVerilogTb = true,
        isVerbose = true)
    testerOptions = testerOptionsGlobal.copy(waveform = Some(new java.io.File("test_run_dir/waveform.vcd")))
  }

  val options1TolWaveformTBVCS = new DspTesterOptionsManager {
    dspTesterOptions = DspTesterOptions(
        fixTolLSBs = 1,
        genVerilogTb = true,
        isVerbose = true)
    testerOptions = testerOptionsGlobal.copy(
      waveform = Some(new java.io.File("test_run_dir/waveform.vcd")),
      backendName = "vcs"
    )
  }

  val debugBuild = new ExecutionOptionsManager("DebugBuild") with HasChiselExecutionOptions with HasFirrtlOptions {
    commonOptions = CommonOptions(
      globalLogLevel = LogLevel.Info,
      targetDirName = s"test_run_dir/DebugBuild"
    )
    firrtlOptions = FirrtlExecutionOptions(compilerName = "verilog") 
  }

  val buildWithMemories = new ExecutionOptionsManager("BuildWithMemories") with HasChiselExecutionOptions with HasFirrtlOptions {
    commonOptions = CommonOptions(
      globalLogLevel = LogLevel.Info,
      targetDirName = s"test_run_dir/BuildWithMemories"
    )
    firrtlOptions = FirrtlExecutionOptions(
      compilerName = "verilog",
      // TODO: Switch to new style transforms
      // TODO: Don't hard code names
      customTransforms = Seq(
        new passes.memlib.InferReadWrite(),
        new passes.memlib.ReplSeqMem()),
      annotations = List(
        passes.memlib.InferReadWriteAnnotation("FFASTTopWrapper"),
        passes.memlib.ReplSeqMemAnnotation(s"-c:FFASTTopWrapper:-o:test_run_dir/BuildWithMemories/vlsi_mem_gen.conf"))
    ) 
  }
   
}