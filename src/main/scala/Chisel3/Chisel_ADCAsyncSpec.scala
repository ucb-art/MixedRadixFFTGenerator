package dspblocks.fft
import rocketchiselutil._
import chisel3._
import dsptools.numbers._
import dsptools.numbers.implicits._
import barstools.tapeout.transforms._
import chisel3.experimental.{withClockAndReset, withClock, withReset}
import chisel3.experimental._
import chisel3.util._

// WARNING ******** Maybe it's better to have separate LUTs so I don't need to guarantee timing!? THINK MORE!!!

// TODO: Everything should be under with clock and reset

// WARNING: ToMemory WE delayed, stateInfo DONE delayed
// ToMemory DIN delayed (placeholder b/c ADC calibration output will be delayed 1 cycle)
// IdxToBankAddr LUT (external) delayed by default (therefore ToMemory WADDR delayed correspondingly)

// ADC: clk, analogIn, digitalOut, reset, valid (registered)
// AsyncQueue: enq_clock, enq_reset, enq (Decoupled), deq_clock, deq_reset, deq (Decoupled)
// Decoupled interface: ready, valid, bits
// Valid indicates producer has put valid data in bits
// Ready indicates consumer is ready to accept the data this cycle

// If sender starts up after receiver
// enq.valid, deq.ready high **always true
// check deq.valid; enq.ready always true

// If sender starts up before the receiver
// enq.valid, deq.ready high **always true
// deq.valid always true; check enq.ready

class StateTransitionIO extends Bundle {
  // Pulsed the cycle before state starts
  val start = Input(Bool())
  // Held until moved on
  val inState = Input(Bool())
  // Tells external state machine that this state should be over (held)
  val done = Output(Bool())
  // Tells the external state machine that when this state is done, it should go to the last
  // debug state (done w/ calc)
  // In general, should be high when done is high
  // val skipToEnd = Output(Bool())
}

// TODO: Find some better way to pass in params
class CollectADCSamplesIO[T <: Data:RealBits](
    adcDataType: => T,
    dspDataType: => T, 
    ffastParams: FFASTParams, 
    subFFTnsColMaxs: Map[Int, Seq[Int]]) extends Bundle with PeripheryADCBundle {

  val adcCalScr = new ADCCalSCR(adcDataType, ffastParams)

  val adcScr = new ADCSCR(ffastParams)

  // In case ADC doesn't work, be able to skip out of this state
  val skipADC = Input(Bool())

  // Unique to this block b/c clk generator lives here
  val resetClk = Input(Bool())
  val inClk = Input(Clock())
  val globalClk = Output(Clock())

  val stateInfo = new StateTransitionIO
  val analogIn = Input(DspReal())

  val extSlowClk = Input(Clock())

  // TODO: Maybe move dataToMemory, dataFromMemory, stateInfo to separate bundle

  // Only 1 input at a time for each memory
  val dataToMemory = Flipped(FFASTMemInputLanes(dspDataType, ffastParams))
  // Never used
  val dataFromMemory = Flipped(FFASTMemOutputLanes(dspDataType, ffastParams))

  val idxToBankAddr = Flipped(new SubFFTIdxToBankAddrLUTsIO(subFFTnsColMaxs))

  override def cloneType = (new CollectADCSamplesIO(adcDataType = adcDataType, dspDataType = dspDataType, ffastParams, subFFTnsColMaxs)).asInstanceOf[this.type]

  ////////////////////////////////////////
  ////////////////////////////////// DEBUG
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

final class FFASTMemInputLanes[T <: Data:Ring](elts: (Int, FFASTMemInputLanesInner[T])*) 
    extends CustomIndexedBundle(elts: _*) {
  override def cloneType = (new FFASTMemInputLanes(indexedElements.toList: _*)).asInstanceOf[this.type]
}

final class FFASTMemInputLanesInner[T <: Data:Ring](elts: (Int, Vec[MemInputLane[T]])*) 
    extends CustomIndexedBundle(elts: _*) {
  override def cloneType = (new FFASTMemInputLanesInner(indexedElements.toList: _*)).asInstanceOf[this.type]
}

// TODO: Figure out how to copy paste less
object FFASTMemInputLanes {
  // SubFFT groups of memories
  // Each SubFFT has adcDelays # of independent memories
  // Each banked "memory" takes in # of banks worth of parallel lanes
  def apply[T <: Data:Ring](gen: => T, ffastParams:FFASTParams) = {
    new FFASTMemInputLanes(
      ffastParams.subFFTBankLengths.map { case (fftn, banklengths) => 
        val bundle = new FFASTMemInputLanesInner(
          // # lanes = # of needed banks
          ffastParams.adcDelays.map(_ -> 
            Vec(banklengths.length, new MemInputLane(DspComplex(gen), maxNumBanks = banklengths.length, maxDepth = banklengths.max))
          ): _*
        )
        fftn -> bundle
      }.toSeq: _*
    )
  }
  // TODO: This T stuff got messy -- hard to figure out if it's DspComplex[T] or T
  def connectToDefault[T <: Data:RealBits](bundle: FFASTMemInputLanes[DspComplex[T]], ffastParams: FFASTParams) = {
    ffastParams.getSubFFTDelayKeys.foreach { case (n, ph) => 
      // # of lanes/banks
      for (idx <- 0 until ffastParams.subFFTBankLengths(n).length) {
        bundle(n)(ph)(idx).loc.addr := 0.U
        bundle(n)(ph)(idx).loc.bank := 0.U 
        bundle(n)(ph)(idx).we := false.B
        bundle(n)(ph)(idx).din.imag := Ring[T].zero 
        bundle(n)(ph)(idx).din.real := Ring[T].zero
      }
    }
  }
}

class FFASTMemOutputLanes[T <: Data:Ring](elts: (Int, FFASTMemOutputLanesInner[T])*) 
    extends CustomIndexedBundle(elts: _*) {
  override def cloneType = (new FFASTMemOutputLanes(indexedElements.toList: _*)).asInstanceOf[this.type]
}

class FFASTMemOutputLanesInner[T <: Data:Ring](elts: (Int, Vec[MemOutputLane[T]])*) 
    extends CustomIndexedBundle(elts: _*) {
  override def cloneType = (new FFASTMemOutputLanesInner(indexedElements.toList: _*)).asInstanceOf[this.type]
}

object FFASTMemOutputLanes {
  // SubFFT groups of memories
  // Each SubFFT has adcDelays # of independent memories
  // Each banked "memory" takes in # of banks worth of parallel lanes
  def apply[T <: Data:RealBits](gen: => T, ffastParams:FFASTParams) = {
    new FFASTMemOutputLanes(
      ffastParams.subFFTBankLengths.map { case (fftn, banklengths) => 
        val bundle = new FFASTMemOutputLanesInner(
          // # lanes = # of needed banks
          ffastParams.adcDelays.map(_ -> 
            // TODO: Generalize -- do I want it to be DspComplex?
            Vec(banklengths.length, new MemOutputLane(DspComplex(gen), maxNumBanks = banklengths.length, maxDepth = banklengths.max))
          ): _* 
        )
        fftn -> bundle
      }.toSeq: _*
    )
  }
  def connectToDefault[T <: Data:RealBits](bundle: FFASTMemOutputLanes[DspComplex[T]], ffastParams: FFASTParams) = {
    ffastParams.getSubFFTDelayKeys.foreach { case (n, ph) => 
      // # of lanes/banks
      for (idx <- 0 until ffastParams.subFFTBankLengths(n).length) {
        bundle(n)(ph)(idx).loc.addr := 0.U
        bundle(n)(ph)(idx).loc.bank := 0.U 
        bundle(n)(ph)(idx).re := false.B 
      }
    }
  }
}

// TODO: DON'T HARD CODE LATENCY FROM IDX TO BANKADDR
class CollectADCSamples[T <: Data:RealBits](
    adcDataType: => T, 
    dspDataType: => T,
    ffastParams: FFASTParams, 
    fftType: FFTType, 
    // TODO: Consider moving into FFASTParams?
    subFFTnsColMaxs: Map[Int, Seq[Int]],
    useBlackBox: Boolean) extends Module with RealAnalogAnnotator {

  val io = IO(new CollectADCSamplesIO(adcDataType = adcDataType, dspDataType = dspDataType, ffastParams, subFFTnsColMaxs))

  annotateReal()

  // Should not jump to the last state if done with this state
  // io.stateInfo.skipToEnd := false.B

  // Default memory access
  // Note: this state never requires reading from memory
  FFASTMemOutputLanes.connectToDefault(io.dataFromMemory, ffastParams)
  FFASTMemInputLanes.connectToDefault(io.dataToMemory, ffastParams)

  val analogBlock = Module(new AnalogModelWrapper(adcDataType, ffastParams, useBlackBox = useBlackBox))
   
  analogBlock.io.adcScr := io.adcScr
  attach(io.ADCINP, analogBlock.io.ADCINP)
  attach(io.ADCINM, analogBlock.io.ADCINM)
  analogBlock.io.ADCCLKP := io.ADCCLKP
  analogBlock.io.ADCCLKM := io.ADCCLKM
  analogBlock.io.ADCBIAS := io.ADCBIAS
  analogBlock.io.clkrst := io.clkrst

  // Global clock = fastest clk, phase 0
  val globalClk = io.extSlowClk 
  io.globalClk := globalClk

  val deqReady = io.stateInfo.start || io.stateInfo.inState

  // TODO: Switch over to this syntax everywhere
  val asyncs = ffastParams.getSubFFTDelayKeys.map { case (n, ph) => 
    // Only need depth 2 b/c clk to the right of async is always fastest clk -- add for margin
    // NOTE: Async queue has weird power of 2 requirement
    // Sync 3 is safe enough for metastability
    val async = chisel3.Module(new AsyncQueue(adcDataType, depth = 16, sync = 3, safe = true))
    val thisClk = analogBlock.io.adcClks(n)(ph)
    async.io.enq_clock := thisClk
    
    val synchronizedReset = withClock(thisClk) {
      ShiftRegister(~io.stateInfo.inState, 3)
    }
    // Synchronize to local clk domain
    async.io.enq_reset := synchronizedReset

    // Generally, RX always ready, but let's say it's not ready until it actually needs data
    // async.io.enq.ready
    async.io.enq.valid := analogBlock.io.adcDigitalOut(n)(ph).valid
    async.io.enq.bits := analogBlock.io.adcDigitalOut(n)(ph).bits

    // Uses ph 0 of fastest clk on other side!
    // TODO: Maybe external clk?
    async.io.deq_clock := globalClk
    async.io.deq_reset := ~io.stateInfo.inState
    async.io.deq.ready := deqReady

    (n, ph) -> async

  }.toMap

  // TODO: RESET BEFORE PREVIOUS STATE DONE TO MINIMIZE LATENCY
  // Queue takes many cycles to reset -- should not feed data in until all queues are reset
  val collectAsyncEnqReady = ffastParams.getSubFFTDelayKeys.map { case (n, ph) => asyncs(n, ph).io.enq.ready }
  val allAsyncEnqsReady = collectAsyncEnqReady.reduce(_ & _)
  val synchronizedAllAsyncEnqsReady = withClockAndReset(globalClk, io.stateInfo.start) {
    ShiftRegister(allAsyncEnqsReady, 3)
  }
  val asyncEnqsAllReady = withClockAndReset(globalClk, io.stateInfo.start) {
    RegEnable(true.B, enable = synchronizedAllAsyncEnqsReady, init = false.B)
  }

  // Don't synchronize enq_valids until the queues are all ready
  val actuallyCollectADCSamples = io.stateInfo.inState & asyncEnqsAllReady
  analogBlock.io.collectADCSamplesState := actuallyCollectADCSamples

  // Calibration
  val calibrationMod = Module(new ADCCal(adcDataType, ffastParams))
  calibrationMod.io.adcCalScr <> io.adcCalScr
  calibrationMod.io.clk := globalClk
  calibrationMod.io.isAdcCollect := actuallyCollectADCSamples
  val calibrationModDelay = calibrationMod.moduleDelay

  // Eventually, calibration blocks will be here
  // Also, the idx -> bank, addr LUT takes 1 cycle (outputs registered)
  ffastParams.getSubFFTDelayKeys.map { case (n, ph) => 
    withClock(globalClk) {
      // Only write to memory on valid
      // Only first lane is ever active in this block
      // NOTE: ADC output has much fewer bits than memory (due to growing during DSP ops)
      val async = asyncs(n, ph)
      calibrationMod.io.adcIn(n)(ph) := async.io.deq.bits
      val cmplx = Wire(DspComplex(dspDataType))
      cmplx.real := calibrationMod.io.calOut(n)(ph)
      cmplx.imag := Ring[T].zero
      io.dataToMemory(n)(ph)(0).din := RegNext(cmplx)
    } 
  }

  // TODO: Can I use only 1 counter per stage associated w/ last phase? b/c I assume that always comes last?
  // Or do I need multiple? Minor optimization...
  val (memIdxCountsTemp, isMaxCounts) = withClockAndReset(globalClk, io.stateInfo.start) {
    ffastParams.getSubFFTDelayKeys.map { case (n, ph) => 
      // Delay valid to match calibration
      val delayedValid = ShiftRegister(asyncs(n, ph).io.deq.valid, calibrationModDelay)
      // @ count n, hold
      val count = Wire(UInt(range"[0, $n]"))
      val isMaxCount = count === n.U
      val isNotMaxCount = ~isMaxCount
      val en = delayedValid & (~isMaxCount)
      // First time deq valid happens corresponds to data 0
      // which means that it goes to idx one the next time data is received (data 1)
      count := RegEnable(next = count + 1.U, init = 0.U, enable = en)
      // Delayed to match Idx -> Bank, Addr LUT + Calibration delay
      io.dataToMemory(n)(ph)(0).we := RegNext(en)
      // TODO: Is # of lanes always == # of banks?
      // Address associated with *current* count
      (((n, ph) -> count), isMaxCount)
    }
  }.unzip

  val memIdxCounts = memIdxCountsTemp.toMap
  // Done when all counts maxed out (or you're not supposed to be in ADC state)
  val done = isMaxCounts.reduce(_ & _)
  io.stateInfo.done := withClockAndReset(globalClk, io.stateInfo.start) { RegNext(done, init = false.B) } | io.skipADC

  // TODO: Are all ADC outputs @ the same relative data index? If they're not, I don't think they'll differ by too many

  // Get address associated with phase lane that is ready first; also keep track of previous addresses
  val countMaxPerSubFFT = ffastParams.subFFTns.map { case n => 
    val countsPerSubFFT = ffastParams.adcDelays.map { case ph => memIdxCounts(n, ph) }
    val max = countsPerSubFFT.tail.foldLeft(countsPerSubFFT.head)((accum, count) => Mux(count > accum, count, accum))
    // TODO: Don't hard-code
    val delayedMax = withClockAndReset(globalClk, io.stateInfo.start) {
      val maxDly = RegNext(next = max, init = 0.U)
      val maxDly2 = RegNext(next = maxDly, init = 0.U)
      val maxDly3 = RegNext(next = maxDly2, init = 0.U)
      val maxDly4 = RegNext(next = maxDly3, init = 0.U)
      Seq(maxDly, maxDly2, maxDly3, maxDly4)
    }
    n -> (Seq(max) ++ delayedMax)
  }.toMap

  // One cycle delayed relative to countMaxPerSubFFT
  val bankAddrs = ffastParams.subFFTns.map { case n => 
    // Delay 0
    io.idxToBankAddr.idxs(n) := countMaxPerSubFFT(n)(0)
    // Delay 1 (LUT outputs registered)
    val bankAddr = io.idxToBankAddr.bankAddrs(n)
    // TODO: Have withClockAndReset only once
    val delayedBankAddr = withClockAndReset(globalClk, io.stateInfo.start) {
      // TODO: Is RegInit not necessary? -- it's OK to write garbage and then write over it
      val bankAddrDly = RegNext(bankAddr)
      val bankAddrDly2 = RegNext(bankAddrDly)
      val bankAddrDly3 = RegNext(bankAddrDly2)
      val bankAddrDly4 = RegNext(bankAddrDly3)
      // TODO: Don't hard code!
      Seq(bankAddrDly, bankAddrDly2, bankAddrDly3, bankAddrDly4)
    }
    n -> (Seq(bankAddr) ++ delayedBankAddr)
  }.toMap

  // TODO: I'm being silly for the sake of time
  // Match LUT output delay
  val countMaxPerSubFFTDelayed = countMaxPerSubFFT.map { case (n, seq) =>
    n -> seq.map { case x => withClock(globalClk) { RegNext(x) } }
  }
 
  // Point of this code is to not have repeated LUTs for each Phase
  // Assume that the queue outputs occur roughly at the same time, so that the fastest
  // ADC lane would only be ahead by ~1 data address. You keep track of older addresses
  // and translate as necessary by matching to older results.
  ffastParams.getSubFFTDelayKeys.foreach { case (n, ph) => 
    // Need to delay idxCount, countMaxCurrentSubFFT to match bankAddrs latency
    val idxCount = withClock(globalClk) { RegNext(memIdxCounts(n, ph)) }
    val countMaxCurrentSubFFT = countMaxPerSubFFTDelayed(n)
    val bankAddrsCurrentSubFFT = bankAddrs(n)
    val correctBankAddr = Mux1H(
      countMaxCurrentSubFFT.zip(bankAddrsCurrentSubFFT).map { case (thisMax, thisBankAddr) => 
        (idxCount === thisMax) -> thisBankAddr 
      }
    )
    io.dataToMemory(n)(ph)(0).loc.addr := correctBankAddr.addr
    io.dataToMemory(n)(ph)(0).loc.bank := correctBankAddr.bank
  }

}
