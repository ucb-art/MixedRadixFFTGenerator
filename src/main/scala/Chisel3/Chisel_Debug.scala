package dspblocks.fft

import chisel3._

// TODO: Hook up to SCR

// Big memory is dual ported -- can read/write simultaneously
// Nominally 32 + 10 + 18 bits
class ControlMemWritePack[T <: Data:Ring](memDataType: DspComplex[T], ffastParams: FFASTParams) extends Bundle {
  val maxIdx = ffastParams.subFFTns.max - 1
  val din = Input(memDataType)
  val wIdx = Input(UInt(range"[0, $maxIdx]"))
  val we = CustomIndexedBundle(CustomIndexedBundle(Input(Bool()), ffastParams.adcDelays), ffastParams.subFFTns)
  override def cloneType = (new ControlMemWritePack(memDataType, ffastParams)).asInstanceOf[this.type]
  // This enables you to write din simultaneously to all memories where we is high @ idx wIdx
}

class ControlMemReadPackFromCPU(ffastParams: FFASTParams) extends Bundle {
  val maxIdx = ffastParams.subFFTns.max - 1
  val rIdx = Input(UInt(range"[0, $maxIdx]"))
  val re = CustomIndexedBundle(CustomIndexedBundle(Input(Bool()), ffastParams.adcDelays), ffastParams.subFFTns)
  override def cloneType = (new ControlMemReadPackFromCPU(ffastParams)).asInstanceOf[this.type]
  // This enables you to read from rIdx of all memories where re is high
}

// TODO: Technically, maxIdx is different depending on sub FFT, but to make life easier...
class ControlMemReadPackToCPU[T <: Data:Ring](memDataType: DspComplex[T], ffastParams: FFASTParams) extends Bundle {
  val maxIdx = ffastParams.subFFTns.max - 1
  val rIdx = Output(UInt(range"[0, $maxIdx]"))
  val dout = Output(memDataType)
  override def cloneType = (new ControlMemReadPackToCPU(memDataType, ffastParams)).asInstanceOf[this.type]
}

// TODO: SCR!
// WARNING: Should set re/we low before exiting debug!
class ControlStatusIO[T <: Data:Ring](memDataType: DspComplex[T], ffastParams: FFASTParams, numStates: Int) extends Bundle {
  val ctrlMemWrite = new ControlMemWritePack(memDataType, ffastParams)
  val ctrlMemReadFromCPU = new ControlMemReadPackFromCPU(ffastParams)
  // # adc delays * # subFFT stages --> # of control registers
  // WARNING: Takes 2 cycles to propagate (one for LUT, one for SyncReadMem) -- how does that need to be reflected in SCR?
  val ctrlMemReadToCPU = CustomIndexedBundle(
    CustomIndexedBundle(
      new ControlMemReadPackToCPU(memDataType, ffastParams), 
      ffastParams.adcDelays),ffastParams.subFFTns)
  val currentState = Output(UInt(range"[0, $numStates)"))
  // If position is high, the corresponding debug state is active 
  // TODO: Make less noob -- technically needs to just have width = # of debug states
  // This ignores and 1's for non-debug states
  val debugStates = Input(UInt(numStates.W))
  override def cloneType = (new ControlStatusIO(memDataType, ffastParams, numStates)).asInstanceOf[this.type]
}





class DebugIO[T <: Data:Ring](memDataType: DspComplex[T], ffastParams: FFASTParams, numStates: Int) extends Bundle {
  val currentState = Input(UInt(range"[0, $numStates)"))
}



  FFASTMemOutputLanes.connectToDefault(io.dataFromMemory, ffastParams)
  FFASTMemInputLanes.connectToDefault(io.dataToMemory, ffastParams)


  // Only 1 input at a time for each memory
  val dataToMemory = Flipped(FFASTMemInputLanes(dspDataType, ffastParams))
  // Never used
  val dataFromMemory = Flipped(FFASTMemOutputLanes(dspDataType, ffastParams))

  val idxToBankAddr = Flipped(new SubFFTIdxToBankAddrLUTsIO(subFFTnsColMaxs))



  

// if not debug, insta done. 
// we == false

// done low on start. enable with done from CPU (synchronous). then hold. 

// current state also affects which LUT idx used
