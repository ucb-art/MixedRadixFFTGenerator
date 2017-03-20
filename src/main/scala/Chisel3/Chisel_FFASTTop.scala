package dspblocks.fft

import chisel3._
import chisel3.experimental._
import dsptools.numbers._
import dsptools.numbers.implicits._

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
  ) ++ Seq(0 until maxNumPeels).map(n => s"Peel$n")
  val stateNames = basicStateNames.map(state => Seq(state, s"${state}Debug")).flatten ++ Seq("reset")
  require(stateNames.distinct.length == stateNames.length, "State names must be unique!")
  
  // TODO: Unnecessary???
  val statesInt = stateNames.zipWithIndex.map { case (name, idx) => name -> idx }.toMap
  val states = stateNames.zipWithIndex.map { case (name, idx) => name -> idx.U }.toMap

  // Reset is not a state you enter -- only a state you leave
  // After last debug, return back to ADCCollect
  val nextStateNames = stateNames.tail.dropRight(1) ++ Seq.fill(2)(stateNames.head)

  // TODO: Remove subFFTnsColMaxs dependence -- move to FFASTParams
  val inputSubFFTIdxToBankAddrLUT = Module(new SubFFTIdxToBankAddrLUTs(ffastParams, ffastParams.inputType))
  val outputSubFFTIdxToBankAddrLUT = Module(new SubFFTIdxToBankAddrLUTs(ffastParams, ffastParams.outputType))

  val numStates = stateNames.length
  val currentState = Reg(UInt(range"[0, $numStates)"))

///////////////// END STATE MACHINE

  val io = IO(
    new FFASTTopIO(dspDataType, ffastParams, stateNames.length, inputSubFFTIdxToBankAddrLUT.io.pack.subFFTnsColMaxs))

  val collectADCSamplesBlock = Module(
    new CollectADCSamples(
      adcDataType,
      dspDataType, 
      ffastParams, 
      ffastParams.inputType, 
      inputSubFFTIdxToBankAddrLUT.io.pack.subFFTnsColMaxs)
  )
  collectADCSamplesBlock.io.resetClk := io.resetClk
  collectADCSamplesBlock.io.inClk := io.inClk
  collectADCSamplesBlock.io.analogIn := io.analogIn
  collectADCSamplesBlock.io.idxToBankAddr.bankAddrs := inputSubFFTIdxToBankAddrLUT.io.pack.bankAddrs

  val globalClk = collectADCSamplesBlock.io.globalClk

  // Clks for LUTs
  inputSubFFTIdxToBankAddrLUT.io.clk := globalClk
  outputSubFFTIdxToBankAddrLUT.io.clk := globalClk

  val debugBlock = Module(
    new Debug(
      dspDataType,
      ffastParams,
      statesInt,
      inputSubFFTIdxToBankAddrLUT.io.pack.subFFTnsColMaxs)
  )
  debugBlock.io.scr := io.scr
  debugBlock.io.currentState := currentState
  debugBlock.io.clk := globalClk
  debugBlock.io.adcIdxToBankAddr.bankAddrs := inputSubFFTIdxToBankAddrLUT.io.pack.bankAddrs 
  debugBlock.io.postFFTIdxToBankAddr.bankAddrs := outputSubFFTIdxToBankAddrLUT.io.pack.bankAddrs 

  val dataMems = ffastParams.getSubFFTDelayKeys.map { case (n, ph) => 
    // TODO: Should dspDataType be complex?
    val memBankLengths = ffastParams.subFFTBankLengths(n)
    val mem = Module(new MemBankInterface(DspComplex(dspDataType), memBankLengths))
    mem.io.clk := globalClk
    mem.suggestName(s"dataMem_${n}_${ph}")
    (n, ph) -> mem
  }.toMap

  // TODO: This DspComplex[T] vs. T being DspComplex thing is driving me crazy -- make consistent!
  def connectToMem(
      mems: Map[(Int, Int), MemBankInterface[DspComplex[T]]], 
      dataToMemory: FFASTMemInputLanes[DspComplex[T]], 
      dataFromMemory: FFASTMemOutputLanes[DspComplex[T]]): Unit = {
    ffastParams.getSubFFTDelayKeys.map { case (n, ph) => 
      mems(n, ph).io.i := dataToMemory(n)(ph)
      dataFromMemory(n)(ph) := mems(n, ph).io.o
    }
  }

  when(currentState === states("ADCCollect")) {
    connectToMem(dataMems, collectADCSamplesBlock.io.dataToMemory, collectADCSamplesBlock.io.dataFromMemory)
  } .otherwise {
    connectToMem(dataMems, debugBlock.io.dataToMemory, debugBlock.io.dataFromMemory)
  }

  // TODO: Change when I write peel
  // Currently, only used for debug
  outputSubFFTIdxToBankAddrLUT.io.pack.idxs := debugBlock.io.postFFTIdxToBankAddr.idxs

  when(currentState === states("ADCCollect")) {
    inputSubFFTIdxToBankAddrLUT.io.pack.idxs := collectADCSamplesBlock.io.idxToBankAddr.idxs  
  } .otherwise {
    inputSubFFTIdxToBankAddrLUT.io.pack.idxs := debugBlock.io.adcIdxToBankAddr.idxs
  }










  val stateMods = 
  
start
instate
done
skiptoend

// ADC
//io.stateInfo
 

// Debug
//stateInfo

//currentState

// go to next state if current state's done is true
// go to end if done + end


// check complex


}