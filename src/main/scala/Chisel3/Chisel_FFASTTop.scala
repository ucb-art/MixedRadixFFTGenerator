package dspblocks.fft

import chisel3._
import chisel3.experimental._
import dsptools.numbers._
import dsptools.numbers.implicits._
import barstools.tapeout.TestParams
import org.scalatest.{FlatSpec, Matchers}
import barstools.tapeout.transforms.pads._
import barstools.tapeout.transforms.clkgen._
import breeze.math.Complex
import dsptools.{DspTester, DspTesterOptionsManager}
import barstools.tapeout.transforms._

import chisel3.util.ShiftRegister

// TODO: Suggest better names???
// TODO: Use Array.range(), etc.

// TODO: Simplify imports

// TODO: SCR DEBUG should all be low @ start to let stuff flush out 
// TODO: ADC Calibration -- read/write (in ADC) -- write only in debug ; read in both -- CAN USE SINGLE-PORTED
// TODO: Timing Calibration -- read/write (in peeling?) -- write only in debug ; read in both -- CAN USE SINGLE-PORTED
// TODO: Output buffer -- write (in peeling) -- only need read in debug -- SINGLE-PORTED

// NOTE: On the off chance that you've asserted debug done and the states changed BEFORE your last address was written
// you should double check to see that your address was written correctly before proceeding

class FFASTTopIO[T <: Data:RealBits](
    dspDataType: => T,
    ffastParams: FFASTParams,
    numStates: Int,
    subFFTnsColMaxs: Map[Int, Seq[Int]]) extends Bundle {

  val resetClk = Input(Bool())
  val inClk = Input(Clock())
  val analogIn = Input(DspReal())

  // Top-level stuff
  val stateMachineReset = Input(Bool())
  val extSlowClk = Input(Clock())
  val extSlowClkSel = Input(Bool())

  val scr = new ControlStatusIO(DspComplex(dspDataType), ffastParams, numStates)

  // The following IO are for debug purposes only (removed for real tapeout)
  // val adc = new CollectADCSamplesIO(dspDataType, ffastParams, subFFTnsColMaxs)
  // val debug = new DebugIO(dspDataType, ffastParams, numStates, subFFTnsColMaxs)

  override def cloneType = 
    (new FFASTTopIO(dspDataType, ffastParams, numStates, subFFTnsColMaxs)).asInstanceOf[this.type]

  ///////////////////////////////////////
  //////////////////////////////// DEBUG
/*
  val asyncEnqValidMin = Output(Bool())
  val asyncEnqDataMin = Output(dspDataType)
  val asyncDeqValidMin = Output(Bool())
  val asyncDeqDataMin = Output(dspDataType)

  val asyncEnqValidMax = Output(Bool())
  val asyncEnqDataMax = Output(dspDataType)
  val asyncDeqValidMax = Output(Bool())
  val asyncDeqDataMax = Output(dspDataType)

  val subFFTMin = ffastParams.subFFTns.min 
  val subFFTMax = ffastParams.subFFTns.max
  val countMaxFFTMin = Output(UInt(range"[0, $subFFTMin]"))
  val countMaxFFTMax = Output(UInt(range"[0, $subFFTMin]"))
*/
}

@chiselName
class FFASTTop[T <: Data:RealBits](
  adcDataType: => T, 
  dspDataType: => T, 
  ffastParams: FFASTParams, 
  maxNumPeels: Int = 10,
  useBlackBox: Boolean = false) extends Module {

////////////////// STATE MACHINE

  val basicStateNames = Seq(
    "ADCCollect",
    "FFT" //,
    //"PopulateNonZerotons"
  ) ++ (if (maxNumPeels == 0) Seq.empty else Seq(0 until maxNumPeels).map(n => s"Peel$n"))
  val stateNames = basicStateNames.map(state => Seq(state, s"${state}Debug")).flatten ++ Seq("reset")
  require(stateNames.distinct.length == stateNames.length, "State names must be unique!")

  println("State machine states: " + stateNames.mkString(", "))
  
  // TODO: Unnecessary???
  val statesInt = stateNames.zipWithIndex.map { case (name, idx) => name -> idx }.toMap
  val states = stateNames.zipWithIndex.map { case (name, idx) => name -> idx.U }.toMap

  // Reset is not a state you enter -- only a state you leave
  // After last debug, return back to ADCCollect
  // val nextStateNames = stateNames.tail.dropRight(1) ++ Seq.fill(2)(stateNames.head)

  // TODO: Remove subFFTnsColMaxs dependence -- move to FFASTParams
  val inputSubFFTIdxToBankAddrLUT = Module(new SubFFTIdxToBankAddrLUTs(ffastParams, ffastParams.inputType))
  val outputSubFFTIdxToBankAddrLUT = Module(new SubFFTIdxToBankAddrLUTs(ffastParams, ffastParams.outputType))

  val numStates = stateNames.length

///////////////// END STATE MACHINE

val subFFTnsColMaxs = inputSubFFTIdxToBankAddrLUT.io.pack.subFFTnsColMaxs

  val io = IO(
    new FFASTTopIO(dspDataType, ffastParams, numStates, subFFTnsColMaxs))

  val collectADCSamplesBlock = Module(
    new CollectADCSamples(
      adcDataType = adcDataType,
      dspDataType = dspDataType, 
      ffastParams, 
      ffastParams.inputType, 
      subFFTnsColMaxs,
      useBlackBox = useBlackBox)
  )
  collectADCSamplesBlock.io.resetClk := io.resetClk
  collectADCSamplesBlock.io.inClk := io.inClk
  collectADCSamplesBlock.io.analogIn := io.analogIn
  collectADCSamplesBlock.io.idxToBankAddr.bankAddrs := inputSubFFTIdxToBankAddrLUT.io.pack.bankAddrs

  collectADCSamplesBlock.io.extSlowClk := io.extSlowClk 
  collectADCSamplesBlock.io.extSlowClkSel := io.extSlowClkSel

  val globalClk = collectADCSamplesBlock.io.globalClk

  // Start @ reset state
  val synchronizedStateMachineReset = withClock(globalClk) { ShiftRegister(io.stateMachineReset, 3) }
  val currentState = withClockAndReset(globalClk, synchronizedStateMachineReset) { RegInit(init = (numStates - 1).U) }

  // Clks for LUTs
  inputSubFFTIdxToBankAddrLUT.io.clk := globalClk
  outputSubFFTIdxToBankAddrLUT.io.clk := globalClk

  val debugBlock = Module(
    new Debug(
      dspDataType,
      ffastParams,
      statesInt,
      subFFTnsColMaxs)
  )

  debugBlock.io.scr <> io.scr
  debugBlock.io.currentState := currentState
  debugBlock.io.clk := globalClk
  debugBlock.io.adcIdxToBankAddr.bankAddrs := inputSubFFTIdxToBankAddrLUT.io.pack.bankAddrs 
  debugBlock.io.postFFTIdxToBankAddr.bankAddrs := outputSubFFTIdxToBankAddrLUT.io.pack.bankAddrs 

  val subFFTs = ffastParams.subFFTns.map { case fft =>
    val fftParams = PeelingScheduling.getFFTParams(fft)
    val mod = Module(new SubFFT(
      dspDataType = dspDataType,
      fftParams = fftParams,
      parallelPELabels = ffastParams.adcDelays,
      fftType = ffastParams.inputType,
      // TODO: DON'T HARD CODE!!!
      memOutDelay = 1
    ))
    mod.io.clk := globalClk
    mod.suggestName(s"subFFT$fft")
    fft -> mod
  }.toMap

  val dataMems = ffastParams.getSubFFTDelayKeys.map { case (n, ph) => 
    // TODO: Should dspDataType be complex?
    val memBankLengths = ffastParams.subFFTBankLengths(n)
    val mem = Module(new MemBankInterface(DspComplex(dspDataType), memBankLengths, name = s"ffastDataSRAM_${n}_${ph}"))
    mem.io.clk := globalClk
    mem.suggestName(s"ffastDataMemInterface_${n}_${ph}")
    (n, ph) -> mem
  }.toMap

  // TODO: This DspComplex[T] vs. T being DspComplex thing is driving me crazy -- make consistent!
  def connectToMem(
      mems: Map[(Int, Int), MemBankInterface[DspComplex[T]]], 
      dataToMemory: FFASTMemInputLanes[DspComplex[T]], 
      dataFromMemory: FFASTMemOutputLanes[DspComplex[T]]): Unit = {
    ffastParams.getSubFFTDelayKeys.map { case (n, ph) => 
      mems(n, ph).io.i := dataToMemory(n)(ph)
      dataFromMemory(n)(ph) <> mems(n, ph).io.o
    }
  }

  // TODO: Add more!
  when(currentState === states("ADCCollect")) {
    connectToMem(dataMems, collectADCSamplesBlock.io.dataToMemory, collectADCSamplesBlock.io.dataFromMemory)
  } .elsewhen(currentState === states("FFT")) {
    ffastParams.getSubFFTDelayKeys.map { case (n, ph) => 
      dataMems(n, ph).io.i := subFFTs(n).io.dataToMemory(ph)
      subFFTs(n).io.dataFromMemory(ph) <> dataMems(n, ph).io.o
    }
  } .otherwise {
    connectToMem(dataMems, debugBlock.io.dataToMemory, debugBlock.io.dataFromMemory)
  }

  def connectLUTIdxsToDefault(idxs: CustomIndexedBundle[UInt]): Unit = {
    idxs.elements foreach { case (idx, port) => 
      port := 0.U
    }
  }

  // TODO: Change when I write peel
  // Currently, only used for debug
  // FFT and before: use default
  outputSubFFTIdxToBankAddrLUT.io.pack.idxs := debugBlock.io.postFFTIdxToBankAddr.idxs

  when(currentState === states("ADCCollect")) {
    inputSubFFTIdxToBankAddrLUT.io.pack.idxs := collectADCSamplesBlock.io.idxToBankAddr.idxs  
  } .elsewhen(currentState === states("ADCCollectDebug")) {
    inputSubFFTIdxToBankAddrLUT.io.pack.idxs := debugBlock.io.adcIdxToBankAddr.idxs
  } .otherwise {
    // This LUT is only ever used for ADC Input + Debug right after
    connectLUTIdxsToDefault(inputSubFFTIdxToBankAddrLUT.io.pack.idxs)
  }

  // TODO: Any way to automate this more?
  /*
  stateNames.map { 
    case name: String if name == "ADCCollect" => name -> collectADCSamplesBlock
    case name: String if name == "FFT" => throw new Exception("Not valid state!")
    case name: String if name == "PopulateNonZerotons" => throw new Exception("Not valid state!")
    case name: String if name.endsWidth("Debug") => name -> debugBlock
    case name: String if name.startsWith("Peel") => throw new Exception("Not valid state!")
  }
  */

  val done = Wire(Bool())
  val currentStateBools = stateNames.zipWithIndex.map { case (name, idx) => name -> (currentState === idx.U) }.toMap
  val nextStateWithoutSkipToEnd = Wire(UInt(range"[0, $numStates)"))

  // TODO: Should be last debug
  val lastState = basicStateNames.last + "Debug"
  when(currentStateBools("reset") | currentStateBools(lastState)) {
    nextStateWithoutSkipToEnd := states("ADCCollect")
  } .otherwise {
    nextStateWithoutSkipToEnd := currentState +& 1.U
  }

  // TODO: When done + skip to end (peel only), nextState is different
  // TODO: Convert to enable?
  val nextState = nextStateWithoutSkipToEnd
  currentState := Mux(done, nextState, currentState)

  // TODO: Make state machine smarter...

  // TODO: Fix meaning
  // Debug: if 1, then stay in state
  // ADC: if 1, then leave state
  collectADCSamplesBlock.io.skipADC := io.scr.debugStates(statesInt("ADCCollect"))

  when (currentStateBools("reset")) {

    done := true.B
    collectADCSamplesBlock.io.stateInfo.start := done
    collectADCSamplesBlock.io.stateInfo.inState := false.B 
    debugBlock.io.stateInfo.start := false.B
    debugBlock.io.stateInfo.inState := false.B 
    // TODO: Make sub ADC wrapper?
    subFFTs foreach { case (name, mod) => 
      mod.io.stateInfo.start := false.B
      mod.io.stateInfo.inState := false.B 
    }

  } .elsewhen (currentStateBools("ADCCollect")) {

    done := collectADCSamplesBlock.io.stateInfo.done 
    collectADCSamplesBlock.io.stateInfo.start := false.B 
    collectADCSamplesBlock.io.stateInfo.inState := true.B 
    debugBlock.io.stateInfo.start := done
    debugBlock.io.stateInfo.inState := false.B 
    subFFTs foreach { case (name, mod) => 
      mod.io.stateInfo.start := false.B
      mod.io.stateInfo.inState := false.B 
    }

  } .elsewhen (currentStateBools("ADCCollectDebug")) {

    done := debugBlock.io.stateInfo.done 
    collectADCSamplesBlock.io.stateInfo.start := false.B
    collectADCSamplesBlock.io.stateInfo.inState := false.B 
    debugBlock.io.stateInfo.start := false.B
    debugBlock.io.stateInfo.inState := true.B 
    subFFTs foreach { case (name, mod) => 
      mod.io.stateInfo.start := done
      mod.io.stateInfo.inState := false.B 
    }

  } .elsewhen (currentStateBools("FFT")) {

    // Done when ALL sub-FFTs finish
    done := subFFTs.map { case (name, mod) => mod.io.stateInfo.done }.reduce(_ & _)
    collectADCSamplesBlock.io.stateInfo.start := false.B
    collectADCSamplesBlock.io.stateInfo.inState := false.B 
    debugBlock.io.stateInfo.start := done
    debugBlock.io.stateInfo.inState := false.B 
    subFFTs foreach { case (name, mod) => 
      mod.io.stateInfo.start := false.B
      mod.io.stateInfo.inState := true.B 
    }

  } .elsewhen (currentStateBools("FFTDebug")) {

    done := debugBlock.io.stateInfo.done 
    // TODO: Change -- here we assume this is the last state
    collectADCSamplesBlock.io.stateInfo.start := done
    collectADCSamplesBlock.io.stateInfo.inState := false.B 
    debugBlock.io.stateInfo.start := false.B
    debugBlock.io.stateInfo.inState := true.B 
    subFFTs foreach { case (name, mod) => 
      mod.io.stateInfo.start := false.B
      mod.io.stateInfo.inState := false.B 
    }

  } .otherwise { // SHOULD NEVER GET HERE

    done := false.B 
    collectADCSamplesBlock.io.stateInfo.start := false.B 
    collectADCSamplesBlock.io.stateInfo.inState := false.B
    debugBlock.io.stateInfo.start := false.B
    debugBlock.io.stateInfo.inState := false.B
    subFFTs foreach { case (name, mod) => 
      mod.io.stateInfo.start := false.B
      mod.io.stateInfo.inState := false.B 
    }

  }

  //////////////////////////////////
  /////////////////////////// DEBUG
/*
  io.asyncEnqValidMin := collectADCSamplesBlock.io.asyncEnqValidMin
  io.asyncEnqDataMin := collectADCSamplesBlock.io.asyncEnqDataMin
  io.asyncDeqValidMin := collectADCSamplesBlock.io.asyncDeqValidMin
  io.asyncDeqDataMin := collectADCSamplesBlock.io.asyncDeqDataMin

  io.asyncEnqValidMax := collectADCSamplesBlock.io.asyncEnqValidMax
  io.asyncEnqDataMax := collectADCSamplesBlock.io.asyncEnqDataMax
  io.asyncDeqValidMax := collectADCSamplesBlock.io.asyncDeqValidMax
  io.asyncDeqDataMax := collectADCSamplesBlock.io.asyncDeqDataMax

  io.countMaxFFTMin := collectADCSamplesBlock.io.countMaxFFTMin
  io.countMaxFFTMax := collectADCSamplesBlock.io.countMaxFFTMax
*/
}

//////////////

class FFASTTopWrapper[T <: Data:RealBits](
    val adcDataType: T, 
    val dspDataType: T, 
    val ffastParams: FFASTParams, 
    maxNumPeels: Int,
    useBlackBox: Boolean = true) extends TopModule(usePads = false) {

  (adcDataType, dspDataType) match {
    case (adc: FixedPoint, dsp: FixedPoint) => 
      println(s"ADC Width: ${adc.getWidth}. ADC Fractional Width: ${adc.binaryPoint.get}")
      println(s"DSP Width: ${dsp.getWidth}. DSP Fractional Width: ${dsp.binaryPoint.get}")
    case (_, _) =>
  }

  println(s"FFAST Params: ${ffastParams.toString}")
  println(s"Max # Peeling Iterations: $maxNumPeels")
  if (useBlackBox) println("Using analog black box!")

  // Need to annotate top-level clk when using clk div
  val mod = 
    Module(new FFASTTop(adcDataType = adcDataType, dspDataType = dspDataType, ffastParams, maxNumPeels, useBlackBox))
  
  val io = IO(new Bundle { 
    val analogIn = Input(DspReal())
    val scr = new ControlStatusIO(DspComplex(dspDataType), ffastParams, mod.numStates)
    // clock, reset used for Analog

    val stateMachineReset = Input(Bool())
    val extSlowClk = Input(Bool())
    val extSlowClkSel = Input(Bool())
  })
    
  SCRHelper(io.scr)

  // WARNING: SCARY: Fast clk + fast clk reset uses Chisel default clock, reset
  mod.io.resetClk := reset
  mod.io.inClk := clock
  mod.io.analogIn := io.analogIn
  mod.io.scr <> io.scr
  mod.io.stateMachineReset := io.stateMachineReset
  mod.io.extSlowClk := io.extSlowClk.asClock
  mod.io.extSlowClkSel := io.extSlowClkSel
  
  annotateClkPort(clock, 
    id = "clock", // not in io bundle
    sink = Sink(Some(ClkSrc(period = 5.0)))
  )

}

class FFASTTopSpec extends FlatSpec with Matchers {
  behavior of "FFASTTop"
  it should "read in ADC inputs" in {

    import dspblocks.fft.FFASTTopParams._

    val opt = TestParams.optionsBTolWaveformTB(lsbs = fpBP - 1, outDir = "test_run_dir/FFASTTopTB")

    dsptools.DspContext.alter(dspContext) {
      dsptools.Driver.execute(() => 
        new FFASTTopWrapper(
          //adcDataType = DspReal(),
          //dspDataType = DspReal(),
          adcDataType = adcDataType, 
          dspDataType = dspDataType,
          ffastParams = ffastParams.copy(delays = Seq(Seq(0, 1))),
          maxNumPeels = maxNumPeels,
          useBlackBox = false
        ), opt
      ) { c =>
        new FFASTTopTester(c)
      } should be (true)
    }
  }
}

class FFASTTopBuildSpec extends FlatSpec with Matchers {
  behavior of "FFASTTopBuild"
  it should "not fail to build" in {

    import dspblocks.fft.FFASTTopParams._

    dsptools.DspContext.alter(dspContext) {
      chisel3.Driver.execute(TestParams.buildWithMemories, () => 
        new FFASTTopWrapper(
          adcDataType = adcDataType, 
          dspDataType = dspDataType,
          ffastParams = ffastParams,
          maxNumPeels = maxNumPeels
        )
      ) 
    }

  }
}

//////////////

class FFASTTopTester[T <: Data:RealBits](c: FFASTTopWrapper[T]) extends DspTester(c) {

  val usedDebugStates = Seq("ADCCollectDebug")
  val numLoops = 2
  // TODO: Don't hard code! (fix by width)
  val adcInInc = 1.0 / (1 << 8) //1.0 / (1 << 8) + 1.0 / (1 << 5)
  // Fastest clk
  val subsamplingT = c.ffastParams.subSamplingFactors.map(_._2).min
  val adcInStart = -1.0 //-10000.0
  val checksPerformed = scala.collection.mutable.ArrayBuffer[String]()

  case class DebugNext(idx: Int, done: Boolean)

  // WARNING: Not thread-safe!!!
  val peekedResults = c.ffastParams.getSubFFTDelayKeys.map { case (n, ph) => 
    (n, ph) -> Array.fill(c.ffastParams.subFFTns.max)(Complex(0.0, 0.0))
  }.toMap


  // TODO: Generalize

  def getEnable(n: Int, ph: Int): Int = {
    val fftGroups = c.ffastParams.getSubFFTDelayKeys  
    val (groupTag, groupIdx) = fftGroups.zipWithIndex.filter { 
      case ((nG, phG), idx) => n == nG && ph == phG 
    }.head
    1 << groupIdx
  }
  def getAllEnable: Int = {
    val fftGroups = c.ffastParams.getSubFFTDelayKeys  
    fftGroups.map { case (n, ph) => getEnable(n, ph) }.sum
  }

  // TODO: Combine with dsptools' checkDecimal
  def compare(exp: Seq[Complex], out: Seq[Complex], tag: (Int, Int), fixTolOverride: Int = -1, test: String = "") = {

    val fft = tag._1
    val ph = tag._2

    val testRef = s"$test RESULTS: SubFFT $fft, Phase $ph"
    checksPerformed += testRef

    println(s" ***** $testRef ***** ")

    import breeze.linalg._
    import breeze.plot._

    val f = Figure()
    val p = f.subplot(0)

    p.legend_=(true)

    val xaxis = (0 until exp.length).map(e => e.toDouble).toSeq.toArray

    // Log 0 doesn't exist
    val plotMin = 0.0000000001
    val outPlot = out.map(c => 20 * math.log10(Seq(c.abs, plotMin).max)).toSeq
    val expPlot = exp.map(c => 20 * math.log10(Seq(c.abs, plotMin).max)).toSeq
    p += plot(xaxis, outPlot.toArray, name = "Result")
    p += plot(xaxis, expPlot.toArray, name = "Expected")
    
    p.ylim(Seq(-100.0, expPlot.min).max, expPlot.max)
    p.title_=(s"FFT: $fft Phase: $ph $test")

    p.xlabel = "Frequency Bin"
    p.ylabel = "20log10(||Vpeak||)"

    f.saveas(s"test_run_dir/FFASTTopTB/${test}Result_${fft}_${ph}.pdf") 

    // TODO: Remove some redundancy

    def toMax(w: Int): BigInt = (BigInt(1) << w) - 1

    val floTolDec = math.pow(10, -realTolDecPts.value)   
    val fixTolInt = toMax(if (fixTolOverride == -1) fixTolLSBs.value else fixTolOverride)
    val tol = c.dspDataType match {
      case r: DspReal => floTolDec
      case f: FixedPoint =>
        require(f.binaryPoint.known, "Binary point must be known!")
        FixedPoint.toDouble(fixTolInt, f.binaryPoint.get)
    }

    val (expectPasses, maxErrors) = exp.zip(out).zipWithIndex.map { case ((e, o), i) =>
      val realDelta = math.abs(e.real - o.real)
      val imagDelta = math.abs(e.imag - o.imag)
      val pass = updatableDspVerbose.withValue(false) {
        expect(realDelta < tol && imagDelta < tol, 
          s"Expected: $e \t Actual: $o")
      }
      (pass, Seq(realDelta, imagDelta).max)
    }.unzip
    expect(expectPasses.reduce(_ & _), s"$test should be right. Tolerance $tol. Worst error ${maxErrors.max}.")
  }

  def clearResults() = {
    c.ffastParams.getSubFFTDelayKeys foreach { case (n, ph) => 
      // TODO: There has to be a better way to reset!
      for (arrayIdx <- 0 until c.ffastParams.subFFTns.max)
        peekedResults(n, ph)(arrayIdx) = Complex(0.0, 0.0)
    }
  }

  def setupDebug(debugStates: Seq[String]) = {
    val debugStatesPoke = debugStates.foldLeft(0)((accum, next) => accum + (1 << c.mod.statesInt(next)))
    // val debugStatesPoke = 1 << c.mod.statesInt(debugStates)
    updatableDspVerbose.withValue(false) {
      poke(c.io.scr.debugStates, debugStatesPoke)
      poke(c.io.scr.cpuDone, false)
      // In debug, always read; maybe write -- overwrite later
      poke(c.io.scr.ctrlMemReadFromCPU.re, getAllEnable)
      poke(c.io.scr.ctrlMemWrite.we, 0)
    }
  }

  def debugCollection(debugNext: DebugNext, stopIdx: Int = -1, wantToEscape: Boolean = true): DebugNext = {
    val done = DebugNext(0, true)
    val incInput = DebugNext(debugNext.idx + 1, false)
    updatableDspVerbose.withValue(false) {
      poke(c.io.scr.ctrlMemReadFromCPU.rIdx, debugNext.idx)

      if (peek(c.io.scr.reToCPU) == 0) {
        // Don't increment if can't read
        debugNext
      }
      else {
        // All memories must be fully read
        val rIdxOutMin = c.ffastParams.getSubFFTDelayKeys.map { case (n, ph) => 

          val rIdxOut = peek(c.io.scr.ctrlMemReadToCPU(n)(ph).rIdx)

          if (rIdxOut < n)
            peekedResults(n, ph)(rIdxOut) = peek(c.io.scr.ctrlMemReadToCPU(n)(ph).dout)

          rIdxOut
        }.min
        val stopCond = (stopIdx != -1 && rIdxOutMin == stopIdx) || 
          (stopIdx == -1 && rIdxOutMin == c.ffastParams.subFFTns.max - 1)

        if (stopCond) {
          poke(c.io.scr.cpuDone, wantToEscape)
          done
        }
        else incInput
      }
    }
  }

  def quantize(ins: Seq[Double]): Seq[Double] = {
    import dspblocks.fft.FFASTTopParams._
    c.dspDataType match {
      case f: FixedPoint => 
        ins.map { case i => 
          // Quantization needs to match exactly
          // FixedPoint.toDouble(FixedPoint.toBigInt(i, adcBP), adcBP)
          // math.round(i * (1 << adcBP)).toDouble / (1 << adcBP)
          val temp1 = i * (1 << adcBP).toDouble
          // ADC uses round half up
          val temp2 = math.floor(temp1 + 0.5).toDouble
          temp2 / (1 << adcBP)
        }
      case r: DspReal => ins 
    }
  }

  def checkADCResults(test: String, skip: Boolean = false, customInput: Option[Seq[Double]] = None): Complex = {
    import dspblocks.fft.FFASTTopParams._
    val startingValue = customInput match {
      case None if skip =>
        // All test results can be derived once you know at what point ADC input was fed in (which time sample was 
        // gotten when clks were aligned)
        peekedResults(c.ffastParams.subFFTns.min, 0)(0)
      case Some(ins) if skip =>
        // Need to quantize the input to get a good match
        // Patterns repeat a lot so need very specific (inefficient) search
        val quantizedIns = quantize(ins)
        val n = c.ffastParams.subFFTns.min
        val outValsSeq = peekedResults(n, 0).toSeq.take(n)
        val subsamplingFactor = c.ffastParams.subSamplingFactors(n)
        val indicesThatMatchHead = quantizedIns.zipWithIndex.filter(_._1 == outValsSeq.head.real).map(_._2)
        val possibleIdx = indicesThatMatchHead.find { case i => 
          val possibleMatch = (0 until n).map(x => (i + x * subsamplingFactor) % c.ffastParams.fftn).map(x => Complex(quantizedIns(x), 0.0))
          possibleMatch == outValsSeq
        }
        require(possibleIdx != None, "Idx must exist!")
        // TODO: Don't fake this
        Complex(possibleIdx.get.toDouble, 0.0)
      case Some(ins) if !skip =>
        // TODO: Get rid of copy paste
        // Need to quantize the input to get a good match
        val quantizedIns = quantize(ins)
        var headAtPh0Idx: Int = 0
        // Guarantee you look at PH0 first
        peekedResults.toSeq.sortBy { case ((n, ph), outVals) => (n, ph) } foreach { case ((n, ph), outVals) =>
          val testRef = s"$test ADC RESULTS: SubFFT $n, Phase $ph"
          checksPerformed += testRef

          println(s" ***** $testRef ***** ")
          // TODO: Turn above into function?

          // No good reason for choosing the min
          val outValsSeq = outVals.toSeq.take(n)
          if (c.ffastParams.subFFTns.min == n && ph == 0) {
            val subsamplingFactor = c.ffastParams.subSamplingFactors(n)
            val indicesThatMatchHead = quantizedIns.zipWithIndex.filter(_._1 == outValsSeq.head.real).map(_._2)
            val possibleIdx = indicesThatMatchHead.find { case i => 
              val possibleMatch = (0 until n).map(x => (i + x * subsamplingFactor) % c.ffastParams.fftn).map(x => Complex(quantizedIns(x), 0.0))
              possibleMatch == outValsSeq
            }
            require(possibleIdx != None, "Idx must exist!")
            headAtPh0Idx = possibleIdx.get
          }
          else {
            // TODO: Copy pasta
            val headAtPhXIdx = (headAtPh0Idx + ph) % c.ffastParams.fftn
            require(expect(outVals.head == Complex(quantizedIns(headAtPhXIdx), 0.0), 
              s"Relative samples must be right across sub-ADCs. Head @ ${outValsSeq.head}"))
          }

          val subsamplingFactor = c.ffastParams.subSamplingFactors(n)
          val headAtPhXIdx = (headAtPh0Idx + ph) % c.ffastParams.fftn
          val expectedIdxAdd = Seq.fill(outValsSeq.length - 1)(subsamplingFactor)
          val expectedIdx = expectedIdxAdd.scanLeft(headAtPhXIdx) { (accum, next) => 
            (accum + next) % c.ffastParams.fftn
          }
          val expected = expectedIdx.map(i => Complex(quantizedIns(i), 0.0))
          val pass = expect(outValsSeq == expected, "Subsampled values should be spaced correctly!")
          if (!pass) {
            val firstFailed = outValsSeq.zip(expected).find { case (out, exp) => out != exp }.get
            println(
              "First failed @ index " + 
              expected.indexOf(firstFailed._2) + 
              s". Expected ${firstFailed._2}. Got ${firstFailed._1}.")
          }
          // Terminate if fail
          require(pass)
          // println(s"Expected : \t" + expected.mkString(","))
        }
        // TODO: Ew don't do this
        Complex(headAtPh0Idx.toDouble, 0.0)

      case None if !skip => 
        var headAtPh0 = Complex(0.0, 0.0)
        // Guarantee you look at PH0 first
        peekedResults.toSeq.sortBy { case ((n, ph), outVals) => (n, ph) } foreach { case ((n, ph), outVals) =>
          val testRef = s"$test ADC RESULTS: SubFFT $n, Phase $ph"
          checksPerformed += testRef

          println(s" ***** $testRef ***** ")

          // No good reason for choosing the min
          val outValsSeq = outVals.toSeq.take(n)
          if (c.ffastParams.subFFTns.min == n && ph == 0)
            headAtPh0 = outValsSeq.head
          else {
            val headAtPhXTemp = headAtPh0 + Complex(ph * adcInInc, 0.0)
            val headAtPhX = {
              if (headAtPhXTemp.real >= math.abs(adcInStart)) Complex(headAtPhXTemp.real + 2 * adcInStart, 0.0)
              else headAtPhXTemp
            }
            require(expect(outValsSeq.head == headAtPhX, 
              s"Relative samples must be right across sub-ADCs. Head @ ${outValsSeq.head}"))
          }
          val subsamplingFactor = c.ffastParams.subSamplingFactors(n)
          val expectedAdd = Seq.fill(outValsSeq.length - 1)(Complex(subsamplingFactor * adcInInc, 0.0))
          val expected = expectedAdd.scanLeft(outValsSeq.head) { (accum, next) => 
            val unmoddedOut = accum + next 
            if (unmoddedOut.real >= math.abs(adcInStart)) Complex(unmoddedOut.real + 2 * adcInStart, 0.0)
            else unmoddedOut
          }
          // println(s"Loop $loop, SubFFT $n, Phase $ph : \t" + outValsSeq.mkString(","))
          val pass = expect(outValsSeq == expected, "Subsampled values should be spaced correctly!")
          if (!pass) {
            val firstFailed = outValsSeq.zip(expected).find { case (out, exp) => out != exp }.get
            println(
              "First failed @ index " + 
              expected.indexOf(firstFailed._2) + 
              s". Expected ${firstFailed._2}. Got ${firstFailed._1}.")
          }
          // Terminate if fail
          require(pass)
          // println(s"Expected : \t" + expected.mkString(","))
        }
        headAtPh0
    }
    clearResults()
    if (customInput == None) println(s"------- $test ADC In Initial Value: $startingValue")
    else println(s"------- $test ADC In Initial Value: ${startingValue.real.toInt}")
    startingValue
  }

  def cycleThroughUntil(state: String) = {
    updatableDspVerbose.withValue(false) { 
      var currState = peek(c.io.scr.currentState)
      val debugStateIdxs = c.mod.statesInt.filter { 
        case (key, value) => key.endsWith("Debug") 
      }.map { case (key, value) => value }.toSeq
      var inDebugState = false
      while (currState != c.mod.statesInt(state)) {
        // Skip through debugs back to beginning
        // Note: My state machine requires a rising edge detect to change state
        // which helps w/ timing but is annoying if you forget about it
        if (debugStateIdxs.contains(currState) && !inDebugState) {
          poke(c.io.scr.cpuDone, false.B)
          inDebugState = true
        }
        else if (debugStateIdxs.contains(currState) && inDebugState) {
          poke(c.io.scr.cpuDone, true.B)
          inDebugState = false
        }    
        step(subsamplingT) 
        currState = peek(c.io.scr.currentState)
      }
      poke(c.io.scr.cpuDone, false.B)
    }
  }

  def runADC(customInput: Option[Seq[Double]] = None) = {
    // Cycle back
    updatableDspVerbose.withValue(false) { 
      cycleThroughUntil("ADCCollect")
      customInput match { 
        case Some(ins) =>
          var adcIdx = 0
          while (peek(c.io.scr.currentState) == c.mod.statesInt("ADCCollect")) {
            // Fast rate!
            poke(c.io.analogIn, ins(adcIdx)) 
            step(1)
            adcIdx = (adcIdx + 1) % c.ffastParams.fftn
          }  
        case None => 
          var adcIn = adcInStart
          while (peek(c.io.scr.currentState) == c.mod.statesInt("ADCCollect")) {
            // Fast rate!
            poke(c.io.analogIn, adcIn) 
            step(1)
            adcIn += adcInInc
            if (adcIn >= math.abs(adcInStart)) adcIn = adcIn + 2 * adcInStart
          }  
      }
    }
  }

  def runDebug(state: String, stopIdx: Int = -1) = {
    updatableDspVerbose.withValue(false) { 
      cycleThroughUntil(state)  
      var debugNext = DebugNext(idx = 0, done = false)
      while (!debugNext.done) {
        debugNext = debugCollection(debugNext, stopIdx)
        step(subsamplingT)
      }
    }
  }

  def writeInDebug(debugNext: DebugNext, flipInput: Boolean, customInput: Option[Seq[Complex]]): DebugNext = {
    val done = DebugNext(0, true)
    val incInput = DebugNext(debugNext.idx + 1, false)
    updatableDspVerbose.withValue(false) {

      c.ffastParams.subFFTns.foreach { case n =>
        // Once the index has exceeded the subFFT n, turn write off
        if (debugNext.idx == n) {
          val origWE = peek(c.io.scr.ctrlMemWrite.we)
          val turnOffNEn = ~(c.ffastParams.adcDelays.map(ph => getEnable(n, ph)).sum) & origWE
          poke(c.io.scr.ctrlMemWrite.we, turnOffNEn)
        }
      }

      if (BigInt(debugNext.idx).bitLength <= c.io.scr.ctrlMemWrite.wIdx.getWidth) 
        poke(c.io.scr.ctrlMemWrite.wIdx, debugNext.idx)
      val input = customInput match {
        case Some(seq) => 
          if (debugNext.idx >= seq.length) Complex(0.0, 0.0)
          else seq(debugNext.idx)
        case None => 
          if (flipInput) Complex(-debugNext.idx.toDouble * adcInInc, debugNext.idx.toDouble * adcInInc)
          else Complex(debugNext.idx.toDouble * adcInInc, -debugNext.idx.toDouble * adcInInc)
      }
      poke(c.io.scr.ctrlMemWrite.din, input)
      // Some margin to actually finish writing (theoretically, should read back the value...)
      if (debugNext.idx == c.ffastParams.subFFTns.max + 5) {
        done
      }
      else incInput
    }
  }

  def runWriteDebug(state: String, flipInput: Boolean = true, customInput: Option[Seq[Complex]] = None, wantToEscape: Boolean = false): Boolean = {
    updatableDspVerbose.withValue(false) { 
      cycleThroughUntil(state)  
      poke(c.io.scr.ctrlMemWrite.we, getAllEnable)
      var debugNext = DebugNext(idx = 0, done = false)
      while (!debugNext.done) {
        debugNext = writeInDebug(debugNext, flipInput, customInput)
        step(subsamplingT)
      }
      poke(c.io.scr.cpuDone, wantToEscape)
    }  
    // What the next one should do
    !flipInput
  }

  // TODO: Check write results assuming real sine
  def checkWriteResults(state: String, flipInput: Boolean): Unit = {
    peekedResults.toSeq.sortBy { case ((n, ph), outVals) => (n, ph) } foreach { case ((n, ph), outVals) =>
      val testRef = s"WRITE RESULTS: DebugState $state SubFFT $n, Phase $ph, R/I Flipped? $flipInput"
      checksPerformed += testRef

      println(s" ***** $testRef ***** ")

      val outValsSeq = outVals.toSeq.take(n)
      
      val expected = (0 until outValsSeq.length).map { case idx => 
        if (flipInput) Complex(-idx.toDouble * adcInInc, idx.toDouble * adcInInc)
        else Complex(idx.toDouble * adcInInc, -idx.toDouble * adcInInc)
      }
      // TODO: Remove copy-pasta
      val pass = expect(outValsSeq == expected, "Written values should be right")
      if (!pass) {
        val firstFailed = outValsSeq.zip(expected).find { case (out, exp) => out != exp }.get
        println(
          "First failed @ index " + 
          expected.indexOf(firstFailed._2) + 
          s". Expected ${firstFailed._2}. Got ${firstFailed._1}.")
      }
      // Terminate if fail
      require(pass)
    }
    clearResults()
  }

  import dspblocks.fft.FFASTTopParams._
  
  // Clk gen reset
  reset(10)

  updatableDspVerbose.withValue(false) { 

    // Use internal clk for simulation
    poke(c.io.extSlowClk, false.B)
    poke(c.io.extSlowClkSel, false.B)

    poke(c.io.stateMachineReset, true.B)
    step(subsamplingT * 2)
    poke(c.io.stateMachineReset, false.B)

    // Takes 3 slow clk cycles to synchronize
    step(subsamplingT * 4)

  }

/*
  setupDebug(usedDebugStates)

  for (loopNum <- 0 until numLoops) {
    runADC()
    // Always run ADCCollectDebug -- gives you a sense of how to calculate stuff afterwards
    val stopIdx = if (usedDebugStates.contains("ADCCollectDebug")) -1 else 0
    runDebug("ADCCollectDebug", stopIdx)
    checkADCResults(s"Loop $loopNum")
  }

  // Check that writing works
  runADC()
  runWriteDebug("ADCCollectDebug", flipInput = false)
  runDebug("ADCCollectDebug")
  checkWriteResults("ADCCollectDebug", flipInput = false)

  runADC()
  runWriteDebug("ADCCollectDebug", flipInput = true)
  runDebug("ADCCollectDebug")
  checkWriteResults("ADCCollectDebug", flipInput = true)

  // WARNING: I'm NOT first quantizing the input -- so comparing with floating point
  // calculation (will be worse)

  ////////////////////////////////////////////////////////////////////////////////////

  // Warning: To exit out of debug, need to detect RISING EDGE
  setupDebug(Seq("ADCCollectDebug", "FFTDebug"))
  runADC()
  runADC()

  // Smaller FFTs will use a subset
  val inFFT = FFTTestVectors.createInput(c.ffastParams.subFFTns.max, fracBits = adcBP)
  // Writes to all memories simultaneously
  runWriteDebug("ADCCollectDebug", customInput = Some(inFFT))
  // Collect data
  // runDebug("ADCCollectDebug")
  // clearResults()
  // Should auto-escape from ADCCollectDebug
  runDebug("FFTDebug")
  peekedResults.toSeq foreach { case ((n, ph), outVals) =>
    compare(exp = FFTTestVectors.createOutput(inFFT.take(n)), out = outVals.toSeq.take(n), tag = (n, ph), test = "Debug Write to FFT")
  }
  clearResults() 
*/

  setupDebug(Seq("ADCCollectDebug", "FFTDebug"))

  // WARNING: NO QUANTIZATION SO RESULTS WILL BE WORSE IN COMPARISON
  val inLarge = FFTTestVectors.createInput(c.ffastParams.fftn, fracBits = adcBP)
  val inLargeReal = inLarge.map(x => x.real)
  runADC(customInput = Some(inLargeReal))
  runDebug("ADCCollectDebug")
  // TODO: Don't use complex
  val adcInInitialIdx = checkADCResults(s"FFTN ${c.ffastParams.fftn} In", customInput = Some(inLargeReal), skip = true).real.toInt
  runDebug("FFTDebug")

  peekedResults.toSeq foreach { case ((n, ph), outVals) =>
    val subsamplingFactor = c.ffastParams.subSamplingFactors(n)
    // TODO: Convert everything to this (easy to understand)
    val rotatedIn = inLarge.drop(adcInInitialIdx) ++ inLarge.take(adcInInitialIdx)
    val subsampledIn = rotatedIn.drop(ph).grouped(subsamplingFactor).map(_.head).toSeq
    require(subsampledIn.length == n, s"# of subsampled inputs should be $n")
    compare(
      exp = FFTTestVectors.createOutput(subsampledIn), 
      out = outVals.toSeq.take(n), 
      tag = (n, ph), 
      test = "Subsampled and Delayed FFTs", 
      fixTolOverride = fpBP + 1)
  }
  clearResults()

  println("\n\n *************************************************** \n\n")
  checksPerformed.toSeq foreach { x => println(x) }

}