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

//////////////
// NOT SYNTHESIZABLE
class FFASTTopWrapper[T <: Data:RealBits](
    val adcDataType: T, 
    val dspDataType: T, 
    val ffastParams: FFASTParams, 
    maxNumPeels: Int,
    useBlackBox: Boolean = true) extends TopModule(usePads = false) with AnalogAnnotator {

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

    val scr = new ControlStatusIO(DspComplex(dspDataType), ffastParams, mod.numStates)
    val adcCalScr = new ADCCalSCR(adcDataType, ffastParams)
    val peelScr = new PeelingSCR(dspDataType, ffastParams)

    // Connect to core reset
    // Connect to core clk
    val stateMachineReset = Input(Bool())

    val ADCINP = Input(DspReal())
    val ADCINM = Input(DspReal())
    val ADCCLKP = Input(Bool())
    val ADCCLKM = Input(Bool())
    val clkrst = Input(Bool())

    // Not critical to local tests
    val ADCBIAS = Input(Bool())
    val adcScr = new ADCSCR(ffastParams)

  })

  mod.io.peelScr <> io.peelScr
  mod.io.adcCalScr <> io.adcCalScr
  mod.io.adcScr <> io.adcScr

  // WARNING: SCARY: Fast clk + fast clk reset uses Chisel default clock, reset
  mod.io.clkrst := reset
  mod.io.ADCCLKP := clock.asUInt
  mod.io.ADCCLKM := ~clock.asUInt

  mod.io.ADCBIAS := io.ADCBIAS

  val adcinp = Wire(Analog(1.W))
  val adcinm = Wire(Analog(1.W))

  renameAnalog(adcinp, "wire\n`ifndef SYNTHESIS\n  real\n`endif\n       ")
  renameAnalog(adcinm, "wire\n`ifndef SYNTHESIS\n  real\n`endif\n       ")

  attach(adcinp, BitsToReal(io.ADCINP))
  attach(adcinm, BitsToReal(io.ADCINM))

  attach(mod.io.ADCINP, adcinp)
  attach(mod.io.ADCINM, adcinm)

  mod.io.scr <> io.scr
  mod.io.stateMachineReset := io.stateMachineReset

  // Fake subsampling clk derived from 10G
  val subsamplingT = ffastParams.subSamplingFactors.map(_._2).min - 1
  val clkDivFake = Module(new SEClkDivider(divBy = subsamplingT, phases = Seq(0, 4)))
  clkDivFake.io.reset := reset
  clkDivFake.io.inClk := clock
  mod.io.extSlowClk := clkDivFake.io.outClks(4)
  
  // TODO: Doesn't do anything...
  annotateClkPort(clock, 
    id = "clock", // not in io bundle
    sink = Sink(Some(ClkSrc(period = 5.0)))
  )

}

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
    adcDataType: => T,
    dspDataType: => T,
    ffastParams: FFASTParams,
    numStates: Int,
    subFFTnsColMaxs: Map[Int, Seq[Int]]) extends Bundle with PeripheryADCBundle {

  // Top-level stuff
  val stateMachineReset = Input(Bool())
  val extSlowClk = Input(Clock())

  val scr = new ControlStatusIO(DspComplex(dspDataType), ffastParams, numStates)
  val adcScr = new ADCSCR(ffastParams)
  val adcCalScr = new ADCCalSCR(adcDataType, ffastParams)
  val peelScr = new PeelingSCR(dspDataType, ffastParams)

  override def cloneType = 
    (new FFASTTopIO(adcDataType = adcDataType, dspDataType = dspDataType, ffastParams, numStates, subFFTnsColMaxs)).asInstanceOf[this.type]

}

@chiselName
class FFASTTop[T <: Data:RealBits](
  adcDataType: => T, 
  dspDataType: => T, 
  ffastParams: FFASTParams, 
  maxNumPeels: Int = 10,
  useBlackBox: Boolean, 
  override_clock: Option[Clock] = Some(false.B.asClock),
  override_reset: Option[Bool] = Some(true.B)
) extends chisel3.Module(override_clock = override_clock, override_reset = override_reset) with RealAnalogAnnotator {

////////////////// STATE MACHINE

  val peelStateNames = if (maxNumPeels == 0) Seq[String]() else (0 until maxNumPeels).map(nP => s"Peel${nP}")
  val basicStateNames = Seq(
    "ADCCollect",
    "FFT",
    "PopulateNonZerotons"
  ) ++ peelStateNames
  val stateNames = basicStateNames.map(state => Seq(state, s"${state}Debug")).flatten ++ Seq("reset")
  require(stateNames.distinct.length == stateNames.length, "State names must be unique!")

  println("State machine states: " + stateNames.mkString(", "))
  
  // TODO: Unnecessary???
  val statesInt = stateNames.zipWithIndex.map { case (name, idx) => name -> idx }.toMap

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
    new FFASTTopIO(adcDataType = adcDataType, dspDataType = dspDataType, ffastParams, numStates, subFFTnsColMaxs))

  val scrInfo = (SCRHelper(io.scr) ++ SCRHelper(io.adcScr) ++ SCRHelper(io.adcCalScr) ++ SCRHelper(io.peelScr)).map { case (el, str) => str }
  require(scrInfo.distinct.length == scrInfo.length, "All SCR entry names must be distinct!")

  annotateReal()

  val collectADCSamplesBlock = Module(
    new CollectADCSamples(
      adcDataType = adcDataType,
      dspDataType = dspDataType, 
      ffastParams, 
      ffastParams.inputType, 
      subFFTnsColMaxs,
      useBlackBox = useBlackBox)
  )

  collectADCSamplesBlock.io.adcCalScr <> io.adcCalScr
  collectADCSamplesBlock.io.adcScr := io.adcScr
  attach(io.ADCINP, collectADCSamplesBlock.io.ADCINP)
  attach(io.ADCINM, collectADCSamplesBlock.io.ADCINM)
  collectADCSamplesBlock.io.ADCCLKP := io.ADCCLKP
  collectADCSamplesBlock.io.ADCCLKM := io.ADCCLKM
  collectADCSamplesBlock.io.ADCBIAS := io.ADCBIAS
  collectADCSamplesBlock.io.clkrst := io.clkrst

  collectADCSamplesBlock.io.idxToBankAddr.bankAddrs := inputSubFFTIdxToBankAddrLUT.io.pack.bankAddrs

  collectADCSamplesBlock.io.extSlowClk := io.extSlowClk 

  val globalClk = collectADCSamplesBlock.io.globalClk

  // Start @ reset state
  val synchronizedStateMachineReset = withClock(globalClk) { ShiftRegister(io.stateMachineReset, 3) }
  val currentState = withClockAndReset(globalClk, synchronizedStateMachineReset) { RegInit(init = (numStates - 1).U) }

  // Clks for LUTs
  inputSubFFTIdxToBankAddrLUT.io.clk := globalClk
  outputSubFFTIdxToBankAddrLUT.io.clk := globalClk

  val (dataMemsTemp, memDlys) = ffastParams.getSubFFTDelayKeys.map { case (n, ph) => 
    // TODO: Should dspDataType be complex?
    val memBankLengths = ffastParams.subFFTBankLengths(n)
    val mem = Module(new MemBankInterface(DspComplex(dspDataType), memBankLengths, name = s"ffastDataSRAM_${n}_${ph}"))
    mem.io.clk := globalClk
    mem.suggestName(s"ffastDataMemInterface_${n}_${ph}")
    ((n, ph) -> mem, mem.moduleDelay)
  }.unzip

  val dataMems = dataMemsTemp.toMap
  require(memDlys.distinct.length == 1)
  val memOutDelay = memDlys.min

  val subFFTs = ffastParams.subFFTns.map { case fft =>
    val fftParams = PeelingScheduling.getFFTParams(fft)
    val mod = Module(new SubFFT(
      dspDataType = dspDataType,
      fftParams = fftParams,
      parallelPELabels = ffastParams.adcDelays,
      fftType = ffastParams.inputType,
      // TODO: DON'T HARD CODE!!!
      memOutDelay = memOutDelay
    ))
    mod.io.clk := globalClk
    mod.suggestName(s"subFFT$fft")
    fft -> mod
  }.toMap

  val debugBlock = Module(
    new Debug(
      dspDataType,
      ffastParams,
      statesInt,
      subFFTnsColMaxs,
      memOutDelay = memOutDelay,
      idxToBankAddrDelay = inputSubFFTIdxToBankAddrLUT.moduleDelay
    )
  )

  debugBlock.io.scr <> io.scr
  debugBlock.io.currentState := currentState
  debugBlock.io.clk := globalClk
  debugBlock.io.adcIdxToBankAddr.bankAddrs := inputSubFFTIdxToBankAddrLUT.io.pack.bankAddrs 
  debugBlock.io.postFFTIdxToBankAddr.bankAddrs := outputSubFFTIdxToBankAddrLUT.io.pack.bankAddrs 

  val peelingBlock = Module(
    new Peeling(
      dspDataType,
      ffastParams,
      subFFTnsColMaxs,
      memOutDelay
    )
  )

  peelingBlock.io.peelScr <> io.peelScr 
  peelingBlock.io.clk := globalClk
  peelingBlock.io.idxToBankAddr.bankAddrs := outputSubFFTIdxToBankAddrLUT.io.pack.bankAddrs 

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

  val currentStateBools = stateNames.zipWithIndex.map { case (name, idx) => name -> (currentState === idx.U) }.toMap

  val isAPeelState = (Seq("PopulateNonZerotons") ++ peelStateNames).map(name => currentStateBools(name)).reduce(_ | _)

  when(currentStateBools("ADCCollect")) {
    connectToMem(dataMems, collectADCSamplesBlock.io.dataToMemory, collectADCSamplesBlock.io.dataFromMemory)
  } .elsewhen(currentStateBools("FFT")) {
    ffastParams.getSubFFTDelayKeys foreach { case (n, ph) => 
      dataMems(n, ph).io.i := subFFTs(n).io.dataToMemory(ph)
      subFFTs(n).io.dataFromMemory(ph) <> dataMems(n, ph).io.o
    }
  } .elsewhen(isAPeelState) {
    connectToMem(dataMems, peelingBlock.io.dataToMemory, peelingBlock.io.dataFromMemory)
  } .otherwise {
    connectToMem(dataMems, debugBlock.io.dataToMemory, debugBlock.io.dataFromMemory)
  }

  // Currently, only used for debug
  // FFT and before: use default
  when(isAPeelState) {
    outputSubFFTIdxToBankAddrLUT.io.pack.idxs := peelingBlock.io.idxToBankAddr.idxs
  } .otherwise {
    outputSubFFTIdxToBankAddrLUT.io.pack.idxs := debugBlock.io.postFFTIdxToBankAddr.idxs
  }

  // This LUT is only ever used for ADC Input + Debug right after
  when(currentStateBools("ADCCollect")) {
    inputSubFFTIdxToBankAddrLUT.io.pack.idxs := collectADCSamplesBlock.io.idxToBankAddr.idxs  
  } .otherwise {
    inputSubFFTIdxToBankAddrLUT.io.pack.idxs := debugBlock.io.adcIdxToBankAddr.idxs
  } 
    
  val done = Wire(Bool())
  val nextStateWithoutSkipToEnd = Wire(UInt(range"[0, $numStates)"))

  // TODO: Should be last debug
  val lastState = basicStateNames.last + "Debug"

  require(lastState == s"Peel${maxNumPeels - 1}Debug")
  val stateResetCond = currentStateBools("reset") | currentStateBools(lastState)
  when(stateResetCond) {
    nextStateWithoutSkipToEnd := statesInt("ADCCollect").U
  } .otherwise {
    nextStateWithoutSkipToEnd := currentState +& 1.U
  }

  // TODO: When done + skip to end (peel only), nextState is different -- or just waste # peel cycles
  // with done always raised
  // TODO: Convert to enable?
  val nextState = nextStateWithoutSkipToEnd
  currentState := Mux(done, nextState, currentState)

  // TODO: Make state machine smarter...

  // TODO: Fix meaning
  // Debug: if 1, then stay in state
  // ADC: if 1, then leave state
  collectADCSamplesBlock.io.skipADC := io.scr.debugStates(statesInt("ADCCollect"))

  val isADebugState = basicStateNames.map(state => currentStateBools(s"${state}Debug")).reduce(_ | _)
  val isANormalState = basicStateNames.map(state => currentStateBools(state)).reduce(_ | _)

  when (currentStateBools("ADCCollect")) {
    done := collectADCSamplesBlock.io.stateInfo.done 
  } .elsewhen (isADebugState) {
    done := debugBlock.io.stateInfo.done 
  } .elsewhen (currentStateBools("FFT")) {
    // Done when ALL sub-FFTs finish
    done := subFFTs.map { case (name, mod) => mod.io.stateInfo.done }.reduce(_ & _)
  } .elsewhen (isAPeelState) {
    done := peelingBlock.io.stateInfo.done
  } .otherwise {
    // Reset or never
    done := true.B 
  }

  collectADCSamplesBlock.io.stateInfo.start := stateResetCond & done
  collectADCSamplesBlock.io.stateInfo.inState := currentStateBools("ADCCollect")

  debugBlock.io.stateInfo.start := isANormalState & done
  debugBlock.io.stateInfo.inState := isADebugState

  val startFFT = currentStateBools("ADCCollectDebug") & done
  subFFTs foreach { case (name, mod) => 
    mod.io.stateInfo.start := startFFT
    mod.io.stateInfo.inState := currentStateBools("FFT")
  }

  // TODO: Somewhat redundant
  val prePeel = (
    Seq("FFT", "PopulateNonZerotons") ++ peelStateNames.filter(_ != s"Peel${maxNumPeels - 1}")
  ).map(name => currentStateBools(s"${name}Debug")).reduce(_ | _)
  peelingBlock.io.stateInfo.start := prePeel & done
  peelingBlock.io.stateInfo.inState := isAPeelState

  // TODO: Somewhat redundant
  peelingBlock.io.resetPeel := currentStateBools("FFTDebug") & done

}