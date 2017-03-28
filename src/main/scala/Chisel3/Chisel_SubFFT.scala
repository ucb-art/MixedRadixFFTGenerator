package dspblocks.fft
import chisel3._
import barstools.tapeout.transforms._
import dsptools.numbers._
import dsptools.numbers.implicits._
import chisel3.experimental._

class SubFFTIO[T <: Data:RealBits](dspDataType: => T, fftParams: FactorizationParams, parallelPELabels: Seq[Int]) extends Bundle {

  val maxDepth = fftParams.mem.bankLengths.max
  val maxNumBanks = fftParams.mem.maxNumBanks

  val clk = Input(Clock())
  val stateInfo = new StateTransitionIO
  val dataToMemory = Flipped(new FFASTMemInputLanesInner(
    // # lanes = # of needed banks
    parallelPELabels.map(_ -> 
      Vec(maxNumBanks, new MemInputLane(DspComplex(dspDataType), maxNumBanks = maxNumBanks, maxDepth = maxDepth))
    ): _*))
  val dataFromMemory = Flipped(new FFASTMemOutputLanesInner(
    // # lanes = # of needed banks
    parallelPELabels.map(_ -> 
      // TODO: Generalize -- do I want it to be DspComplex?
      Vec(maxNumBanks, new MemOutputLane(DspComplex(dspDataType), maxNumBanks = maxNumBanks, maxDepth = maxDepth))
    ): _*))
  override def cloneType = (new SubFFTIO(dspDataType, fftParams, parallelPELabels)).asInstanceOf[this.type]
}

@chiselName
class SubFFT[T <: Data:RealBits](
    dspDataType: => T, 
    fftParams: FactorizationParams, 
    parallelPELabels: Seq[Int], 
    fftType: FFTType, 
    memOutDelay: Int) extends Module {

  val maxNumBanks = fftParams.mem.maxNumBanks
  
  val io = IO(new SubFFTIO(dspDataType, fftParams, parallelPELabels))
  // Parallel processing elements!
  val pe = parallelPELabels.map(label => 
    label -> Module (new ProcessingElement(
      twiddleType = fftParams.twiddle.getTwiddleType(dspDataType),
      dataType = dspDataType,
      fftParams,
      fftType
    ))
  ).toMap
  val pe0Ref = pe.toSeq.head._2

  val twiddleGen = Module(new TwiddleGen(
    dspDataType = dspDataType, 
    fftParams,
    fftType,
    wftaDelay = pe0Ref.wfta.moduleDelay
  ))
  val calcCtrl = Module(new CalcCtrl(
    fftParams, 
    fftType,
    memOutDelay = memOutDelay,
    // TODO: Name consistency!
    wftaDly = pe0Ref.wfta.moduleDelay,
    twDly = pe0Ref.twDelay
  ))

  require(twiddleGen.moduleDelay == (calcCtrl.moduleDelay + memOutDelay), 
    "Twiddle gen and (Calc + Mem) delays must match to reach BF @ the same time!")

  calcCtrl.io.clk := io.clk
  calcCtrl.io.stateInfo := io.stateInfo

  twiddleGen.io.clk := io.clk
  twiddleGen.io.startState := io.stateInfo.start
  twiddleGen.io.currentStageToTwiddle := calcCtrl.io.currentStageToTwiddle
  twiddleGen.io.twiddleCountEnable := calcCtrl.io.twiddleCountEnable

  pe foreach { case (label, mod) =>
    mod.io.clk := io.clk
    mod.io.twiddles := twiddleGen.io.twiddles
    // TODO: Constistant name!! ; also this bit is redundant across PE's
    mod.io.currRad := calcCtrl.io.currentRadToBF
  }
    
  parallelPELabels foreach { case fftLabel => 
    (0 until maxNumBanks) foreach { case lane =>
      // TODO: Less copy paste
      io.dataToMemory(fftLabel)(lane).din := pe(fftLabel).io.y(lane)
      io.dataToMemory(fftLabel)(lane).we := calcCtrl.io.we(lane)
      io.dataToMemory(fftLabel)(lane).loc := calcCtrl.io.wlocs(lane)
      pe(fftLabel).io.x(lane) := io.dataFromMemory(fftLabel)(lane)
      io.dataFromMemory(fftLabel)(lane).re := calcCtrl.io.re(lane)
      io.dataFromMemory(fftLabel)(lane).loc := calcCtrl.io.rlocs(lane)
    }
  }

}