package dspblocks.fft

import chisel3._
import dsptools.numbers._
import dsptools.numbers.implicits._
import chisel3.experimental._
import barstools.tapeout.transforms._
import chisel3.util._

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
class ControlStatusIO[T <: Data:Ring](
    memDataType: DspComplex[T], 
    ffastParams: FFASTParams, 
    numStates: Int) extends Bundle {
  val ctrlMemWrite = new ControlMemWritePack(memDataType, ffastParams)
  val ctrlMemReadFromCPU = new ControlMemReadPackFromCPU(ffastParams)
  // # adc delays * # subFFT stages --> # of control registers
  // WARNING: Takes 2 cycles to propagate (one for LUT, one for SyncReadMem) 
  // -- how does that need to be reflected in SCR?
  val ctrlMemReadToCPU = CustomIndexedBundle(
    CustomIndexedBundle(
      new ControlMemReadPackToCPU(memDataType, ffastParams), 
      ffastParams.adcDelays),ffastParams.subFFTns)
  val currentState = Output(UInt(range"[0, $numStates)"))
  // If position is high, the corresponding debug state is active 
  // TODO: Make less noob -- technically needs to just have width = # of debug states
  // This ignores and 1's for non-debug states
  val debugStates = Input(UInt(numStates.W))
  // Assumes it's low then goes high sometime during the state -- looks for rising edge
  // i.e. can be high @ beginning of state, but doesn't matter
  // Always check that last value is written before asserting
  val cpuDone = Input(Bool())
  override def cloneType = (new ControlStatusIO(memDataType, ffastParams, numStates)).asInstanceOf[this.type]
}

class DebugIO[T <: Data:RealBits](
    dspDataType: T, 
    ffastParams: FFASTParams, 
    numStates: Int, 
    subFFTnsColMaxs: Map[Int, Seq[Int]]) extends Bundle {
  val currentState = Input(UInt(range"[0, $numStates)"))
  val scr = new ControlStatusIO(DspComplex(dspDataType), ffastParams, numStates)
  val dataToMemory = Flipped(FFASTMemInputLanes(dspDataType, ffastParams))
  val dataFromMemory = Flipped(FFASTMemOutputLanes(dspDataType, ffastParams))
  val adcIdxToBankAddr = Flipped(new SubFFTIdxToBankAddrLUTsIO(subFFTnsColMaxs))
  val postFFTIdxToBankAddr = Flipped(new SubFFTIdxToBankAddrLUTsIO(subFFTnsColMaxs))
  val clk = Input(Clock())
  val stateInfo = new StateTransitionIO
  override def cloneType = (new DebugIO(dspDataType, ffastParams, numStates, subFFTnsColMaxs)).asInstanceOf[this.type]
}

class Debug[T <: Data:RealBits](
    dspDataType: T, 
    ffastParams: FFASTParams, 
    states: Map[String, UInt], 
    subFFTnsColMaxs: Map[Int, Seq[Int]]) extends Module {

  val io = IO(new DebugIO(dspDataType, ffastParams, states.toSeq.length, subFFTnsColMaxs))

  // Default connection
  FFASTMemOutputLanes.connectToDefault(io.dataFromMemory, ffastParams)
  FFASTMemInputLanes.connectToDefault(io.dataToMemory, ffastParams)

  withClockAndReset(io.clk, io.stateInfo.start) {

    io.scr.currentState := io.currentState
    io.stateInfo.skipToEnd := false.B

    // Assumes you'll never be reading the same time you write in this state -- writing has precedence
    val getAllCPUwes = ffastParams.getSubFFTDelayKeys.map { case (n, ph) => 
      io.scr.ctrlMemWrite.we(n)(ph) }
    val cpuWrite = getAllCPUwes.reduce(_ | _)
    val usedIdx = Mux(cpuWrite, io.scr.ctrlMemWrite.wIdx, io.scr.ctrlMemReadFromCPU.rIdx)
    val isADCCollectDebugState = io.currentState === states("ADCCollectDebug")
    val delayedIsADCCollectDebugState = RegNext(isADCCollectDebugState)


    val (addrTemp, bankTemp) = ffastParams.subFFTns.map { case n =>
      io.adcIdxToBankAddr.idxs(n) := usedIdx
      io.postFFTIdxToBankAddr.idxs(n) := usedIdx
      val addr = Mux(
        delayedIsADCCollectDebugState, 
        io.adcIdxToBankAddr.bankAddrs(n).addr, 
        io.postFFTIdxToBankAddr.bankAddrs(n).addr)
      val bank = Mux(
        delayedIsADCCollectDebugState, 
        io.adcIdxToBankAddr.bankAddrs(n).bank, 
        io.postFFTIdxToBankAddr.bankAddrs(n).bank)
      ((n -> addr), (n -> bank))
    }.unzip
    val addr = addrTemp.toMap
    val bank = bankTemp.toMap

    // Since bank, address delayed by 1 clk cycle (through LUT)
    val delayedCPUdin = RegNext(io.scr.ctrlMemWrite.din)
    // Bank, address delayed by 1 clk cycle; mem read takes another cycle
    // TODO: Don't hard code???
    val delayedCPUrIdx = ShiftRegister(io.scr.ctrlMemReadFromCPU, 2)

    val currStateIsDebug =
      Mux1H((0 until states.toSeq.length).map(x => (io.currentState === x.U) -> io.scr.debugStates(x)))
    
    ffastParams.getSubFFTDelayKeys.foreach { case (n, ph) => 
      // For each MemBankInterface, only using 1 lane at a time
      io.dataToMemory(n)(ph)(0).din := delayedCPUdin
      // To be safe, always reset WE @ CPU side!
      io.dataToMemory(n)(ph)(0).we := RegNext(io.scr.ctrlMemWrite.we(n)(ph), init = false.B) & currStateIsDebug
      io.dataFromMemory(n)(ph)(0).re := RegNext(io.scr.ctrlMemReadFromCPU.re(n)(ph), init = false.B)
      // 1 cycle delay for bank, addr
      io.dataToMemory(n)(ph)(0).loc.addr := addr(n)
      io.dataToMemory(n)(ph)(0).loc.bank := bank(n)
      io.dataFromMemory(n)(ph)(0).loc.addr := addr(n)
      io.dataFromMemory(n)(ph)(0).loc.bank := bank(n)
      io.scr.ctrlMemReadToCPU(n)(ph).rIdx := delayedCPUrIdx
      io.scr.ctrlMemReadToCPU(n)(ph).dout := io.dataFromMemory(n)(ph)(0).dout
    }

    val cpuDoneDly = RegNext(io.scr.cpuDone)
    val cpuDoneRising = ~cpuDoneDly & io.scr.cpuDone
    // Generally make sure what you've written/read is right before asserting done
    val done = RegEnable(true.B, init = false.B, enable = cpuDoneRising) | ~currStateIsDebug
    io.stateInfo.done := done

  }

}