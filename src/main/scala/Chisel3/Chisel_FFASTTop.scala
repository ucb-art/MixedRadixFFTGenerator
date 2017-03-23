package dspblocks.fft

import chisel3._
import chisel3.experimental._
import dsptools.numbers._
import dsptools.numbers.implicits._
import barstools.tapeout.TestParams
import dsptools.DspTester
import org.scalatest.{FlatSpec, Matchers}
import barstools.tapeout.transforms.pads._
import barstools.tapeout.transforms.clkgen._
import breeze.math.Complex
import dsptools.{DspTester, DspTesterOptionsManager}
import barstools.tapeout.transforms._

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
  val scr = new ControlStatusIO(DspComplex(dspDataType), ffastParams, numStates)

  // The following IO are for debug purposes only (removed for real tapeout)
  val adc = new CollectADCSamplesIO(dspDataType, ffastParams, subFFTnsColMaxs)
  val debug = new DebugIO(dspDataType, ffastParams, numStates, subFFTnsColMaxs)

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

class FFASTTop[T <: Data:RealBits](
  adcDataType: T, 
  dspDataType: T, 
  ffastParams: FFASTParams, 
  maxNumPeels: Int = 10) extends Module {

////////////////// STATE MACHINE

  val basicStateNames = Seq(
    "ADCCollect" //,
    //"FFT",
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
      subFFTnsColMaxs)
  )
  collectADCSamplesBlock.io.resetClk := io.resetClk
  collectADCSamplesBlock.io.inClk := io.inClk
  collectADCSamplesBlock.io.analogIn := io.analogIn
  collectADCSamplesBlock.io.idxToBankAddr.bankAddrs := inputSubFFTIdxToBankAddrLUT.io.pack.bankAddrs

  val globalClk = collectADCSamplesBlock.io.globalClk

  // Start @ reset state
  val currentState = withClockAndReset(globalClk, io.resetClk) { RegInit(init = (numStates - 1).U) }

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

  val dataMems = ffastParams.getSubFFTDelayKeys.map { case (n, ph) => 
    // TODO: Should dspDataType be complex?
    val memBankLengths = ffastParams.subFFTBankLengths(n)
    val mem = Module(new MemBankInterface(DspComplex(dspDataType), memBankLengths))
    mem.io.clk := globalClk
    mem.suggestName(s"ffastDataMem_${n}_${ph}")
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
  when(currentStateBools("reset") | currentStateBools("ADCCollectDebug")) {
    nextStateWithoutSkipToEnd := states("ADCCollect")
  } .otherwise {
    nextStateWithoutSkipToEnd := currentState +& 1.U
  }

  // TODO: When done + skip to end (peel only), nextState is different
  val nextState = nextStateWithoutSkipToEnd
  currentState := Mux(done, nextState, currentState)

  // TODO: Make state machine smarter...

  when (currentStateBools("reset")) {

    done := true.B
    collectADCSamplesBlock.io.stateInfo.start := done
    collectADCSamplesBlock.io.stateInfo.inState := false.B 
    debugBlock.io.stateInfo.start := false.B
    debugBlock.io.stateInfo.inState := false.B 

  } .elsewhen (currentStateBools("ADCCollect")) {

    done := collectADCSamplesBlock.io.stateInfo.done 
    collectADCSamplesBlock.io.stateInfo.start := false.B 
    collectADCSamplesBlock.io.stateInfo.inState := true.B 
    debugBlock.io.stateInfo.start := done
    debugBlock.io.stateInfo.inState := false.B 

  } .elsewhen (currentStateBools("ADCCollectDebug")) {

    done := debugBlock.io.stateInfo.done 
    // TODO: Change -- here we assume this is the last state
    collectADCSamplesBlock.io.stateInfo.start := done 
    collectADCSamplesBlock.io.stateInfo.inState := false.B 
    debugBlock.io.stateInfo.start := false.B
    debugBlock.io.stateInfo.inState := true.B 

  } .otherwise { // SHOULD NEVER GET HERE

    done := false.B 
    collectADCSamplesBlock.io.stateInfo.start := false.B 
    collectADCSamplesBlock.io.stateInfo.inState := false.B
    debugBlock.io.stateInfo.start := false.B
    debugBlock.io.stateInfo.inState := false.B

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
    adcDataType: T, 
    dspDataType: T, 
    val ffastParams: FFASTParams, 
    maxNumPeels: Int = 10) extends TopModule(usePads = false) {
  // Need to annotate top-level clk when using clk div
  val mod = Module(new FFASTTop(adcDataType = adcDataType, dspDataType = dspDataType, ffastParams, maxNumPeels))
  
  val io = IO(new Bundle {
    val resetClk = Input(Bool())
    val analogIn = Input(DspReal())
    val scr = new ControlStatusIO(DspComplex(dspDataType), ffastParams, mod.numStates)

    // DEBUG
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
    // TODO: Option clk
    // The following IO are for debug purposes only (removed for real tapeout)
    // val adc = new CollectADCSamplesIO(dspDataType, ffastParams, mod.subFFTnsColMaxs)
    // val debug = new DebugIO(dspDataType, ffastParams, mod.numStates, mod.subFFTnsColMaxs)
  })

  mod.io.resetClk := reset
  mod.io.inClk := clock
  mod.io.analogIn := io.analogIn
  mod.io.scr <> io.scr

  // DEBUG
/*
  io.asyncEnqValidMin := mod.io.asyncEnqValidMin
  io.asyncEnqDataMin := mod.io.asyncEnqDataMin
  io.asyncDeqValidMin := mod.io.asyncDeqValidMin
  io.asyncDeqDataMin := mod.io.asyncDeqDataMin

  io.asyncEnqValidMax := mod.io.asyncEnqValidMax
  io.asyncEnqDataMax := mod.io.asyncEnqDataMax
  io.asyncDeqValidMax := mod.io.asyncDeqValidMax
  io.asyncDeqDataMax := mod.io.asyncDeqDataMax

  io.countMaxFFTMin := mod.io.countMaxFFTMin
  io.countMaxFFTMax := mod.io.countMaxFFTMax
*/
  // mod.io.adc <> io.adc
  // mod.io.debug <> io.debug

  annotateClkPort(clock, 
    id = "clock", // not in io bundle
    sink = Sink(Some(ClkSrc(period = 5.0)))
  )
}

class FFASTTopSpec extends FlatSpec with Matchers {
  behavior of "FFASTTop"
  it should "read in ADC inputs" in {

    val opt = new DspTesterOptionsManager {
      dspTesterOptions = TestParams.options1TolWaveformTBVCS.dspTesterOptions
      testerOptions = TestParams.options1TolWaveformTBVCS.testerOptions
      commonOptions = TestParams.options1TolWaveformTBVCS.commonOptions.copy(targetDirName = s"test_run_dir/FFASTTopTB")
    }

    dsptools.Driver.execute(() => 
      new FFASTTopWrapper(
        //adcDataType = DspReal(),
        //dspDataType = DspReal(),
        adcDataType = FixedPoint(20.W, 8.BP), 
        dspDataType = FixedPoint(24.W, 12.BP),
        ffastParams = FFASTParams(
          // fftn = 20,
          // subFFTns = Seq(4, 5),
          // delays = Seq(Seq(0, 1)),
          fftn = 21600,
          subFFTns = Seq(675),//, 800, 864),
          delays = Seq(Seq(0)),//, 1, 3, 23)),
          inputType = DIF
        ),
        maxNumPeels = 0
      ), opt
    ) { c =>
      new FFASTTopTester(c)
    } should be (true)
  }
}

class FFASTTopBuildSpec extends FlatSpec with Matchers {
  behavior of "FFASTTopBuild"
  it should "not fail to build" in {
    chisel3.Driver.execute(TestParams.buildWithMemories, () => 
      new FFASTTopWrapper(
        // adcDataType = DspReal(),
        // dspDataType = DspReal(),
        adcDataType = FixedPoint(20.W, 8.BP), 
        dspDataType = FixedPoint(24.W, 12.BP),
        ffastParams = FFASTParams(
          // fftn = 20,
          // subFFTns = Seq(4, 5),
          // delays = Seq(Seq(0, 1)),
          fftn = 21600,
          subFFTns = Seq(675, 800, 864),
          delays = Seq(Seq(0, 1, 3, 23)),
          inputType = DIF
        ),
        maxNumPeels = 0
        /*
        adcDataType = FixedPoint(16.W, 8.BP), 
        dspDataType = FixedPoint(22.W, 12.BP),
        ffastParams = FFASTParams(
          fftn = 20,
          subFFTns = Seq(4, 5),
          delays = Seq(Seq(0, 1)),
          inputType = DIF
        ),
        maxNumPeels = 0
        */
      )
    ) 
  }
}

//////////////

class FFASTTopTester[T <: Data:RealBits](c: FFASTTopWrapper[T]) extends DspTester(c) {

  val usedDebugStates = Seq("ADCCollectDebug")
  val numLoops = 3
  val adcInInc = 1.toDouble / (1 << 12) //(1 << 8)
  // Fastest clk
  val subsamplingT = c.ffastParams.subSamplingFactors.map(_._2).min
  val adcInStart = -500.0
  val checksPerformed = scala.collection.mutable.ArrayBuffer[String]()

  case class DebugNext(idx: Int, done: Boolean)

  // WARNING: Not thread-safe!!!
  val peekedResults = c.ffastParams.getSubFFTDelayKeys.map { case (n, ph) => 
    (n, ph) -> Array.fill(c.ffastParams.subFFTns.max)(Complex(0.0, 0.0))
  }.toMap

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
      c.ffastParams.getSubFFTDelayKeys.foreach { case (n, ph) => 
        poke(c.io.scr.ctrlMemReadFromCPU.re(n)(ph), true.B)
        poke(c.io.scr.ctrlMemWrite.we(n)(ph), false.B)
      }
    }
  }

  def debugCollection(debugNext: DebugNext, stopIdx: Int = -1, wantToEscape: Boolean = true): DebugNext = {
    val done = DebugNext(0, true)
    val incInput = DebugNext(debugNext.idx + 1, false)
    updatableDspVerbose.withValue(false) {
      poke(c.io.scr.ctrlMemReadFromCPU.rIdx, debugNext.idx)

      // All memories must be fully read
      val rIdxOutMin = c.ffastParams.getSubFFTDelayKeys.map { case (n, ph) => 

        val rIdxOut = peek(c.io.scr.ctrlMemReadToCPU(n)(ph).rIdx)

        if (rIdxOut < c.ffastParams.subFFTns.max)
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

  def checkADCResults(loop: Int, skip: Boolean = false): Complex = {
    val startingValue = 
      if (skip) 
        // All test results can be derived once you know at what point ADC input was fed in (which time sample was 
        // gotten when clks were aligned)
        peekedResults(c.ffastParams.subFFTns.min, 0)(0)
      else {
        var headAtPh0 = Complex(0.0, 0.0)
        // Guarantee you look at PH0 first
        peekedResults.toSeq.sortBy { case ((n, ph), outVals) => (n, ph) } foreach { case ((n, ph), outVals) =>
          val testRef = s"ADC RESULTS: Loop $loop, SubFFT $n, Phase $ph"
          checksPerformed += testRef

          println(s" ***** $testRef ***** ")

          // No good reason for choosing the min
          val outValsSeq = outVals.toSeq.take(n)
          if (c.ffastParams.subFFTns.min == n && ph == 0)
            headAtPh0 = outValsSeq.head
          else {
            require(expect(outValsSeq.head == headAtPh0 + Complex(ph * adcInInc, 0.0), 
              s"Relative samples must be right across sub-ADCs. Head @ ${outValsSeq.head}"))
          }
          val subsamplingFactor = c.ffastParams.subSamplingFactors(n)
          val expectedAdd = Seq.fill(outValsSeq.length - 1)(Complex(subsamplingFactor * adcInInc, 0.0))
          val expected = expectedAdd.scanLeft(outValsSeq.head)((accum, next) => accum + next)
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
    startingValue
  }

  def runADC() = {
    // Cycle back
    updatableDspVerbose.withValue(false) { 
      while (peek(c.io.scr.currentState) != c.mod.statesInt("ADCCollect")) {
        step(1) 
      }
      var adcIn = adcInStart
      while (peek(c.io.scr.currentState) == c.mod.statesInt("ADCCollect")) {
        // Fast rate!
        poke(c.io.analogIn, adcIn) 
        step(1)
        adcIn += adcInInc
      }  
    }
  }

  def runDebug(state: String, stopIdx: Int = -1) = {
    updatableDspVerbose.withValue(false) { 
      poke(c.io.scr.cpuDone, false.B)
      // Uses slow clock
      while (peek(c.io.scr.currentState) != c.mod.statesInt(state)) {
        step(subsamplingT)  
      }
      var debugNext = DebugNext(idx = 0, done = false)
      while (!debugNext.done) {
        debugNext = debugCollection(debugNext, stopIdx)
        step(subsamplingT)
      }
    }
  }

  def writeInDebug(debugNext: DebugNext, flipInput: Boolean): DebugNext = {
    val done = DebugNext(0, true)
    val incInput = DebugNext(debugNext.idx + 1, false)
    updatableDspVerbose.withValue(false) {
      c.ffastParams.getSubFFTDelayKeys.foreach { case (n, ph) => 
        // Once the index has exceeded the subFFT n, turn write off
        if (debugNext.idx == n)
          poke(c.io.scr.ctrlMemWrite.we(n)(ph), false.B)
      }
      poke(c.io.scr.ctrlMemWrite.wIdx, debugNext.idx)
      val input = 
        if (flipInput) Complex(-debugNext.idx.toDouble * adcInInc, debugNext.idx.toDouble * adcInInc)
        else Complex(debugNext.idx.toDouble * adcInInc, -debugNext.idx.toDouble * adcInInc)
      poke(c.io.scr.ctrlMemWrite.din, input)
      // Some margin to actually finish writing (theoretically, should read back the value...)
      if (debugNext.idx == c.ffastParams.subFFTns.max + 5) {
        done
      }
      else incInput
    }
  }

  def runWriteDebug(state: String, flipInput: Boolean): Boolean = {
    updatableDspVerbose.withValue(false) { 
      poke(c.io.scr.cpuDone, false.B)
      // Uses slow clock
      while (peek(c.io.scr.currentState) != c.mod.statesInt(state)) {
        step(subsamplingT)  
      }
      c.ffastParams.getSubFFTDelayKeys.foreach { case (n, ph) => 
        poke(c.io.scr.ctrlMemWrite.we(n)(ph), true.B)
      }
      var debugNext = DebugNext(idx = 0, done = false)
      while (!debugNext.done) {
        debugNext = writeInDebug(debugNext, flipInput)
        step(subsamplingT)
      }
    }  
    // What the next one should do
    !flipInput
  }

  def checkWriteResults(state: String, flipInput: Boolean): Unit = {
    peekedResults.toSeq.sortBy { case ((n, ph), outVals) => (n, ph) } foreach { case ((n, ph), outVals) =>
      val testRef = s"WRITE RESULTS: DebugState $state SubFFT $n, Phase $ph"
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
      // require(pass)
    }
    clearResults()
  }

  reset(10)
  setupDebug(usedDebugStates)

  /*for (loopNum <- 0 until numLoops) {
    runADC()
    // Always run ADCCollectDebug -- gives you a sense of how to calculate stuff afterwards
    val stopIdx = if (usedDebugStates.contains("ADCCollectDebug")) -1 else 0
    runDebug("ADCCollectDebug", stopIdx)
    val adcInInitialVal = checkADCResults(loopNum)
    println(s"------- Loop $loopNum ADC In Initial Value: $adcInInitialVal")
  }*/

  // Check that writing works
  runADC()
  runWriteDebug("ADCCollectDebug", flipInput = false)
  runDebug("ADCCollectDebug")
  checkWriteResults("ADCCollectDebug", flipInput = false)

  println("\n\n *************************************************** ")
  checksPerformed.toSeq foreach { x => println(x) }

}