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
  // Tells external state machine that this state should be over
  val done = Output(Bool())
}

class CollectADCSamplesIO[T <: Data:RealBits](adcDataType: => T, ffastParams: FFASTParams) extends Bundle {

  // Unique to this block b/c clk generator lives here
  val resetClk = Input(Bool())
  val inClk = Input(Clock())
  val globalClk = Output(Clock())

  val stateInfo = new StateTransitionIO
  val analogIn = Input(DspReal())

  // Only 1 input at a time for each memory
  val dataToMemory = Flipped(FFASTMemInputLanes(adcDataType, ffastParams))
  // Never used
  val dataFromMemory = Flipped(FFASTMemOutputLanes(adcDataType, ffastParams))

  override def cloneType = (new CollectADCSamplesIO(adcDataType, ffastParams)).asInstanceOf[this.type]
}

// TODO: Figure out how to copy paste less
object FFASTMemInputLanes {
  // SubFFT groups of memories
  // Each SubFFT has adcDelays # of independent memories
  // Each banked "memory" takes in # of banks worth of parallel lanes
  def apply[T <: Data:RealBits](gen: => T, ffastParams:FFASTParams) = {
    new CustomIndexedBundle(
      ffastParams.subFFTBankLengths.map { case (fftn, banklengths) => 
        val bundle = CustomIndexedBundle(
          // # lanes = # of needed banks
          Vec(banklengths.length, new MemInputLane(gen, maxNumBanks = banklengths.length, maxDepth = banklengths.max)), 
          ffastParams.adcDelays
        )
        fftn -> bundle
      }.toSeq: _*
    )
  }
}

object FFASTMemOutputLanes {
  // SubFFT groups of memories
  // Each SubFFT has adcDelays # of independent memories
  // Each banked "memory" takes in # of banks worth of parallel lanes
  def apply[T <: Data:RealBits](gen: => T, ffastParams:FFASTParams) = {
    new CustomIndexedBundle(
      ffastParams.subFFTBankLengths.map { case (fftn, banklengths) => 
        val bundle = CustomIndexedBundle(
          // # lanes = # of needed banks
          Vec(banklengths.length, new MemOutputLane(gen, maxNumBanks = banklengths.length, maxDepth = banklengths.max)), 
          ffastParams.adcDelays
        )
        fftn -> bundle
      }.toSeq: _*
    )
  }
}

class CollectADCSamples[T <: Data:RealBits](adcDataType: => T, ffastParams: FFASTParams, fftType: FFTType) extends Module {
  val io = IO(new CollectADCSamplesIO(adcDataType, ffastParams))
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

    // Only write to memory on valid
    io.dataToMemory(n)(ph)(0).din := async.io.deq.bits
    for (idx <- (1 until ffastParams.subFFTBankLengths(n).length)) {
      io.dataToMemory(n)(ph)(idx).din := Ring[T].zero
    }

    (n, ph) -> async

  }.toMap

  // TODO: When passing through calibration, delay deq.valid one time b/c SeqReadMem has 1 cycle latency

  // TODO: Can I use only 1 counter per stage associated w/ last phase? b/c I assume that always comes last?
  // Or do I need multiple? Minor optimization...
  val (memIdxCountsTemp, isMaxCounts) = withClockAndReset(analogBlock.io.globalClk, io.stateInfo.start) {
    ffastParams.getSubFFTDelayKeys.map { case (n, ph) => 
      // @ count n, hold
      val count = Wire(UInt(range"[0, n]"))
      val isMaxCount = count === n.U
      val countNext = Mux(isMaxCount, count, count +& 1.U)
      // First time deq valid happens corresponds to data 0
      // which means that it goes to idx one the next time data is received (data 1)
      count := RegEnable(next = countNext, init = 0.U, enable = asyncs(n, ph).io.deq.valid)
      io.dataToMemory(n)(ph)(0).we := asyncs(n, ph).io.deq.valid & (~isMaxCount)
      // Inactive lanes should not be writing
      // TODO: Is # of lanes always == # of banks?
      for (idx <- (1 until ffastParams.subFFTBankLengths(n).length)) {
        io.dataToMemory(n)(ph)(idx).we := false.B
      }
      // Address associated with *current* count
      (((n, ph) -> count), isMaxCount)
    }
  }.unzip
  val memIdxCounts = memIdxCountsTemp.toMap
  // Done when all counts maxed out
  val done = isMaxCounts.reduce(_ & _)
  // Reset enq valid
  analogBlock.io.stopCollectingADCSamples := done
  io.stateInfo.done := done

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

  val (idxToBankAddrLUTsTemp, bankAddrsTemp) = ffastParams.subFFTns.map { case n => 
    val lutName = s"ADC${fftType.serialize}IdxToBankAddr$n"
    val lutConsts = PeelingScheduling.getIOMemBankAddr(n, fftType).map(x => x.getBankAddr)
    val lutMod = Module(new UIntLUT2D(lutName, lutConsts, Seq("bank", "addr")))
    val lutModColMax = lutMod.colMax
    lutMod.io.clk := analogBlock.io.globalClk
    // zero corresponds to delay of 0
    lutMod.io.addr := countMaxPerSubFFT(n)(0)
    // TODO: Don't use Seq
    val bankAddr = Wire(new BankAddressBundle(lutModColMax))
    bankAddr.bank := lutMod.io.dout("bank")
    bankAddr.addr := lutMod.io.dout("addr")
    ((n -> lutMod), (n -> bankAddr))
  }.unzip
  val idxToBankAddrLuts = idxToBankAddrLUTsTemp.toMap
  val bankAddrs = bankAddrsTemp.map { case (n, dout) => 
    val delayedDout = withClockAndReset(analogBlock.io.globalClk, io.stateInfo.start) {
      val doutInit = Wire(dout.cloneType) 
      doutInit.addr := 0.U
      doutInit.bank := 0.U
      val doutDly = RegNext(next = dout, init = doutInit)
      val doutDly2 = RegNext(next = doutDly, init = doutInit)
      Seq(doutDly, doutDly2)
    }
    n -> (Seq(dout) ++ delayedDout)
  }.toMap

  // Point of this code is to not have repeated LUTs for each Phase
  // Assume that the queue outputs occur roughly at the same time, so that the fastest
  // ADC lane would only be ahead by ~1 data address. You keep track of older addresses
  // and translate as necessary by matching to older results.
  ffastParams.getSubFFTDelayKeys.foreach { case (n, ph) => 
    val idxCount = memIdxCounts(n, ph)
    val countMaxCurrentSubFFT = countMaxPerSubFFT(n)
    val bankAddrsCurrentSubFFT = bankAddrs(n)
    val correctBankAddr = Mux1H(
      countMaxCurrentSubFFT.zip(bankAddrsCurrentSubFFT).map { case (thisMax, thisBankAddr) => 
        (idxCount === thisMax) -> thisBankAddr 
      }
    )
    io.dataToMemory(n)(ph)(0).loc.addr := correctBankAddr.addr
    io.dataToMemory(n)(ph)(0).loc.bank := correctBankAddr.bank
    for (idx <- (1 until ffastParams.subFFTBankLengths(n).length)) {
      io.dataToMemory(n)(ph)(idx).loc.addr := 0.U
      io.dataToMemory(n)(ph)(idx).loc.bank := 0.U
    }
  }

  // TODO: Don't copy-paste
  // Never read in this state
  ffastParams.getSubFFTDelayKeys.foreach { case (n, ph) =>
    for (idx <- 0 until ffastParams.subFFTBankLengths(n).length) {
      io.dataFromMemory(n)(ph)(idx).loc.addr := 0.U
      io.dataFromMemory(n)(ph)(idx).loc.bank := 0.U 
      io.dataFromMemory(n)(ph)(idx).re := false.B 
    }
  }




  // twiddle factor gen
  // external done signal, state signal
  // debug: read from 1 at a time
  // escape from debug: done high
  // enter debug: lower done
 // read one at a time (out of 18 different -- different phase)

  // try multiple phases for reset
  // connect up to memory -- go straight to debug
  // interface with outside world packet
}
