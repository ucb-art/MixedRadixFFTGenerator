/*

package dspblocks.fft

import chisel3._

// TODO: ADC Calibration -- read/write (in ADC) -- write only in debug ; read in both -- CAN USE SINGLE-PORTED
// TODO: Timing Calibration -- read/write (in peeling?) -- write only in debug ; read in both -- CAN USE SINGLE-PORTED
// TODO: Output buffer -- write (in peeling) -- only need read in debug -- SINGLE-PORTED

// NOTE: On the off chance that you've asserted debug done and the states changed BEFORE your last address was written
// you should double check to see that your address was written correctly before proceeding

class FFASTTop[T <: Data:RealBits](
  adcDataType: T, 
  dspDataType: T, 
  ffastParams: FFASTParams, 
  maxNumPeels: Int = 10) extends Module {

  val inputSubFFTIdxToBankAddrLUT = Module(new SubFFTIdxToBankAddrLUTs(ffastParams, ffastParams.inputType))
  val outputSubFFTIdxToBankAddrLUT = Module(new SubFFTIdxToBankAddrLUTs(ffastParams, ffastParams.outputType))
  val dataMems = ffastParams.getSubFFTDelayKeys.map { case (n, ph) => 
    // TODO: Should dspDataType be complex?
    val memBankLengths = ffastParams.subFFTBankLengths(n)
    val mem = Module(new MemBankInterface(DspComplex(dspDataType), memBankLengths))
    mem.desiredName = s"dataMem_$n_$ph"
    (n, ph) -> mem
  }.toMap

  val collectADCSamplesBlock = Module(
    new CollectADCSamples(
      adcDataType, 
      ffastParams, 
      ffastParams.inputType, 
      inputSubFFTIdxToBankAddrLUT.io.pack.subFFTnsColMaxs)
  )

  val basicStateNames = Seq(
    "ADCCollect",
    "FFT",
    "PopulateNonZerotons"
  ) ++ Seq(0 until maxNumPeels).map(n => s"Peel$n")
  val stateNames = basicStates.map(state => Seq(state, s"${state}Debug")).flatten ++ Seq("reset")

  require(states.distinct.length == states.length, "State names must be unique!")

  val states = stateNames.zipWithIndex.map { case (name, idx) => name -> idx.U }.toMap
  // Reset is not a state you enter -- only a state you leave
  // After last debug, return back to ADCCollect
  val nextStateNames = stateNames.tail.dropRight(1) ++ Seq.fill(2)(stateNames.head)






















// CONNECT UP MEMORIES, and we're good?
// go to next state if current state's done is true
// go to end if done + end






// state
// start
// done

// memory

 


 

} */