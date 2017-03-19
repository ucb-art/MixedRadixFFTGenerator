package dspblocks.fft
import rocketchiselutil._
import chisel3._
import dsptools.numbers._
import dsptools.numbers.implicits._
import barstools.tapeout.transforms._
import chisel3.experimental.{withClockAndReset, withClock, withReset}
import chisel3.experimental._
import chisel3.util._
import barstools.modules._

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
}

// TODO: Find some better way to pass in params
class CollectADCSamplesIO[T <: Data:RealBits](
    adcDataType: => T, 
    ffastParams: FFASTParams, 
    subFFTnsColMaxs: Map[Int, Seq[Int]]) extends Bundle {

  // Unique to this block b/c clk generator lives here
  val resetClk = Input(Bool())
  val inClk = Input(Clock())
  val globalClk = Output(Clock())

  val stateInfo = new StateTransitionIO
  val analogIn = Input(DspReal())

  // TODO: Maybe move dataToMemory, dataFromMemory, stateInfo to separate bundle

  // Only 1 input at a time for each memory
  val dataToMemory = Flipped(FFASTMemInputLanes(adcDataType, ffastParams))
  // Never used
  val dataFromMemory = Flipped(FFASTMemOutputLanes(adcDataType, ffastParams))

  val idxToBankAddr = Flipped(new SubFFTIdxToBankAddrLUTsIO(subFFTnsColMaxs))

  override def cloneType = (new CollectADCSamplesIO(adcDataType, ffastParams, subFFTnsColMaxs)).asInstanceOf[this.type]
}

final class FFASTMemInputLanes[T <: Data:RealBits](elts: (Int, FFASTMemInputLanesInner[T])*) 
    extends CustomIndexedBundle(elts: _*) {
  override def cloneType = (new FFASTMemInputLanes(indexedElements.toList: _*)).asInstanceOf[this.type]
}

final class FFASTMemInputLanesInner[T <: Data:RealBits](elts: (Int, Vec[MemInputLane[T]])*) 
    extends CustomIndexedBundle(elts: _*) {
  override def cloneType = (new FFASTMemInputLanesInner(indexedElements.toList: _*)).asInstanceOf[this.type]
}

// TODO: Figure out how to copy paste less
object FFASTMemInputLanes {
  // SubFFT groups of memories
  // Each SubFFT has adcDelays # of independent memories
  // Each banked "memory" takes in # of banks worth of parallel lanes
  def apply[T <: Data:RealBits](gen: => T, ffastParams:FFASTParams) = {
    new FFASTMemInputLanes(
      ffastParams.subFFTBankLengths.map { case (fftn, banklengths) => 
        val bundle = new FFASTMemInputLanesInner(
          // # lanes = # of needed banks
          ffastParams.adcDelays.map(_ -> 
            Vec(banklengths.length, new MemInputLane(gen, maxNumBanks = banklengths.length, maxDepth = banklengths.max))
          ): _*
        )
        fftn -> bundle
      }.toSeq: _*
    )
  }
  def connectToDefault[T <: Data:RealBits](bundle: FFASTMemInputLanes[T], ffastParams: FFASTParams) = {
    ffastParams.getSubFFTDelayKeys.foreach { case (n, ph) => 
      // # of lanes/banks
      for (idx <- 0 until ffastParams.subFFTBankLengths(n).length) {
        bundle(n)(ph)(idx).loc.addr := 0.U
        bundle(n)(ph)(idx).loc.bank := 0.U 
        bundle(n)(ph)(idx).we := false.B 
        bundle(n)(ph)(idx).din := Ring[T].zero
      }
    }
  }
}

class FFASTMemOutputLanes[T <: Data:RealBits](elts: (Int, FFASTMemOutputLanesInner[T])*) 
    extends CustomIndexedBundle(elts: _*) {
  override def cloneType = (new FFASTMemOutputLanes(indexedElements.toList: _*)).asInstanceOf[this.type]
}

class FFASTMemOutputLanesInner[T <: Data:RealBits](elts: (Int, Vec[MemOutputLane[T]])*) 
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
            Vec(banklengths.length, new MemOutputLane(gen, maxNumBanks = banklengths.length, maxDepth = banklengths.max))
          ): _* 
        )
        fftn -> bundle
      }.toSeq: _*
    )
  }
  def connectToDefault[T <: Data:RealBits](bundle: FFASTMemOutputLanes[T], ffastParams: FFASTParams) = {
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

class CollectADCSamples[T <: Data:RealBits](
    adcDataType: => T, 
    ffastParams: FFASTParams, 
    fftType: FFTType, 
    // TODO: Consider moving into FFASTParams?
    subFFTnsColMaxs: Map[Int, Seq[Int]]) extends Module {

  val io = IO(new CollectADCSamplesIO(adcDataType, ffastParams, subFFTnsColMaxs))

  // Default memory access
  // Note: this state never requires reading from memory
  FFASTMemOutputLanes.connectToDefault(io.dataFromMemory, ffastParams)
  FFASTMemInputLanes.connectToDefault(io.dataToMemory, ffastParams)

  val analogBlock = Module(new AnalogModel(adcDataType, ffastParams))
  analogBlock.io.resetClk := io.resetClk
  analogBlock.io.inClk := io.inClk
  analogBlock.io.collectADCSamplesState := io.stateInfo.inState
  analogBlock.io.analogIn := io.analogIn
  io.globalClk := analogBlock.io.globalClk

  val deqReady = io.stateInfo.start || io.stateInfo.inState

  // TODO: Switch over to this syntax everywhere
  val asyncs = ffastParams.getSubFFTDelayKeys.map { case (n, ph) => 
    // Only need depth 2 b/c clk to the right of async is always fastest clk
    // Sync 3 is safe enough for metastability
    val async = Module(new AsyncQueue(adcDataType, depth = 3, sync = 3))
    async.io.enq_clock := analogBlock.io.adcClks(n)(ph)
    // 1 cycle before state starts
    async.io.enq_reset := io.stateInfo.start
    // Generally, RX always ready, but let's say it's not ready until it actually needs data
    // async.io.enq.ready
    async.io.enq.valid := analogBlock.io.adcDigitalOut(n)(ph).valid
    async.io.enq.bits := analogBlock.io.adcDigitalOut(n)(ph).bits

    // Uses ph 0 of fastest clk on other side!
    // TODO: Maybe external clk?
    async.io.deq_clock := analogBlock.io.globalClk
    async.io.deq_reset := io.stateInfo.start
    async.io.deq.ready := deqReady

    (n, ph) -> async

  }.toMap

  // Placeholder
  // Eventually, calibration blocks will be here
  // Also, the idx -> bank, addr LUT takes 1 cycle (outputs registered)
  ffastParams.getSubFFTDelayKeys.map { case (n, ph) => 
    withClock(analogBlock.io.globalClk) {
      // Only write to memory on valid
      // Only first lane is ever active in this block
      val async = asyncs(n, ph)
      io.dataToMemory(n)(ph)(0).din := RegNext(async.io.deq.bits)
    } 
  }

  // TODO: When passing through calibration, delay deq.valid one time b/c SeqReadMem has 1 cycle latency

  // TODO: Can I use only 1 counter per stage associated w/ last phase? b/c I assume that always comes last?
  // Or do I need multiple? Minor optimization...
  val (memIdxCountsTemp, isMaxCounts) = withClockAndReset(analogBlock.io.globalClk, io.stateInfo.start) {
    ffastParams.getSubFFTDelayKeys.map { case (n, ph) => 
      // @ count n, hold
      val count = Wire(UInt(range"[0, $n]"))
      val isMaxCount = count === n.U
      val countNext = Mux(isMaxCount, count, count +& 1.U)
      // First time deq valid happens corresponds to data 0
      // which means that it goes to idx one the next time data is received (data 1)
      count := RegEnable(next = countNext, init = 0.U, enable = asyncs(n, ph).io.deq.valid)
      // Delayed to match Idx -> Bank, Addr LUT + Calibration delay
      io.dataToMemory(n)(ph)(0).we := RegNext(asyncs(n, ph).io.deq.valid & (~isMaxCount))
      // TODO: Is # of lanes always == # of banks?
      // Address associated with *current* count
      (((n, ph) -> count), isMaxCount)
    }
  }.unzip

  val memIdxCounts = memIdxCountsTemp.toMap
  // Done when all counts maxed out
  val done = isMaxCounts.reduce(_ & _)
  // Reset enq valid
  analogBlock.io.stopCollectingADCSamples := done
  io.stateInfo.done := withClockAndReset(analogBlock.io.globalClk, io.stateInfo.start) { RegNext(done, init = false.B) }

  // TODO: Are all ADC outputs @ the same relative data index? If they're not, I don't think they'll differ by too many

  // Get address associated with phase lane that is ready first; also keep track of previous addresses
  val countMaxPerSubFFT = ffastParams.subFFTns.map { case n => 
    val countsPerSubFFT = ffastParams.adcDelays.map { case ph => memIdxCounts(n, ph) }
    val max = countsPerSubFFT.tail.foldLeft(countsPerSubFFT.head)((accum, count) => Mux(count > accum, count, accum))
    // TODO: Don't hard-code
    val delayedMax = withClockAndReset(analogBlock.io.globalClk, io.stateInfo.start) {
      val maxDly = RegNext(next = max, init = 0.U)
      val maxDly2 = RegNext(next = maxDly, init = 0.U)
      Seq(maxDly, maxDly2)
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
    val delayedBankAddr = withClockAndReset(analogBlock.io.globalClk, io.stateInfo.start) {
      // TODO: Is RegInit not necessary? -- it's OK to write garbage and then write over it
      val bankAddrDly = RegNext(bankAddr)
      val bankAddrDly2 = RegNext(bankAddrDly)
      Seq(bankAddrDly, bankAddrDly2)
    }
    Seq(bankAddr) ++ delayedBankAddr
  }

  // TODO: I'm being silly for the sake of time
  // Match LUT output delay
  val countMaxPerSubFFTDelayed = countMaxPerSubFFT.map { case (n, seq) =>
    n -> seq.map { case x => withClock(analogBlock.io.globalClk) { RegNext(x) } }
  }
 
  // Point of this code is to not have repeated LUTs for each Phase
  // Assume that the queue outputs occur roughly at the same time, so that the fastest
  // ADC lane would only be ahead by ~1 data address. You keep track of older addresses
  // and translate as necessary by matching to older results.
  ffastParams.getSubFFTDelayKeys.foreach { case (n, ph) => 
    // Need to delay idxCount, countMaxCurrentSubFFT to match bankAddrs latency
    val idxCount = withClock(analogBlock.io.globalClk) { RegNext(memIdxCounts(n, ph)) }
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
