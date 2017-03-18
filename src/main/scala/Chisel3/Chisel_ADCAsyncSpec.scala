package dspblocks.fft
import rocketchiselutil._
import chisel3._
import dsptools.numbers._
import dsptools.numbers.implicits._
import rocketchiselutil._

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

class ADCAsyncTestModule[T <: Data:RealBits](ffastParams: FFASTParams, dataType: => T) extends Module {

  // Use default clock
  val io = IO(new Bundle {
    val analogIn = Input(DspReal())
    // Synchronous with globalClk, should happen one clk cycle before you enter ADC state
    val restart = Input(Bool())
  })

  val clkGen = Module(new FFASTClkDiv(ffastParams))
  clkGen.io.reset := reset
  clkGen.io.inClk := clock
  // clkGen.io.outClks
  // clkGen.io.frameAligned
  // Uses fastest clk, ph 0 as global clk
  val globalClk = clkGen.io.outClks(ffastParams.subFFTns.max)(0)






// push in from mux of all addresses
// current address max -- reg -- prev address -- reg -- prev, prev address
// allows 1 LUT across all channels

// need valid from ADC
// goes high along w/ first data valid (after align); goes async low when i feed it something
// queue reset on restart; reset on allsync



reset -- synch??? == this & io.restart
  // Counters disabled @ start of ADC collection state -- re-enabled once clks are aligned
  // Assumes that after alignment, outputs come in proper order
  // For counter, reset has precedence over everything -- reset synchronous!
  val countersEn = Wire(Bool())
  countersEn := !AsyncResetReg(io.restart | countersEn, clk = globalClk, rst = clkGen.io.frameAligned) 









// hold count until each has gone through
// take fft out: see delay: store delay





  // Assuming you're in the right phase, the memory is always ready to receive data
  // If you're in a different phase, it's ok if the data is just discarded
  val deqReady = true.B 

  // TODO: Switch over to this syntax everywhere
  val (adcs, asyncs) = ffastParams.getSubFFTDelayKeys.map { case (n, ph) => 

    val adc = Module(new FakeADC(dataType))
    adc.io.clk := clkGen.io.outClks(n)(ph)
    adc.io.analogIn := io.analogIn
    // adc.io.digitalOut below
    adc.io.reset := io.restart
    // adc.io.valid below

    // Only need depth 2 b/c clk to the right of async is always fastest clk
    // Sync 3 is safe enough for metastability
    val async = Module(new AsyncQueue(dataType, depth = 2, sync = 3))
    async.io.enq_clock := clkGen.io.outClks(n)(ph)
    async.io.enq_reset := io.restart
    // async.io.enq.ready always true (RX always ready)
    async.io.enq.valid := adc.io.valid
    async.io.enq.bits := adc.io.digitalOut
    // Uses ph 0 of fastest clk on other side!
    // TODO: Maybe external clk?
    async.io.deq_clock := globalClk
    async.io.deq_reset := io.restart
    async.io.deq.ready := deqReady
    // Need to separately handle async.io.deq.valid, async.io.deq.bits
    // i.e. @ memory interface

    ((n, ph) -> adc, (n, ph) -> async)

  }.unzip
  val adcsMap = adcs.toMap
  val asyncsMap = asyncs.toMap

// restart each set when last ph high
// inc when last phase done? --> only need one counter????
// test: try multiple phases

// (18) counters: reset on restart; in on deqValid

// Need to go to index -> bank here

/*

  for (subFFT <- ffastParams.subFFTns; ph <- mod.delays) {
    io.outClks(subFFT)(ph) := mod.io.outClks(subFFT)(ph).asUInt
  }

  */

}