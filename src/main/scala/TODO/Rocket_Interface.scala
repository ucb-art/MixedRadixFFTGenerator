package FFT
import ChiselDSP._
import Chisel.{Pipe =>_,Complex => _,Mux => _, RegInit => _, RegNext => _, Counter => _, when => _, _}

class CalcInCounter(max:Int) extends Counter(CountParams(countMax = max))

// See SCRFile in Uncore for usage example

// Copy of IO needed for SMI interface
// TODO: Just use junctions one

// From Rocket
// io.smi.req.valid       --- Rocket wants to read or write
// io.smi.req.bits.rw     --- Rocket wants to read (false) or write (true)
// io.smi.req.bits.addr   --- Memory location Rocket wants to access
// io.smi.req.bits.data   --- Data Rocket wants to write (FFT should ignore on read)
// io.smi.resp.ready      --- Rocket ready to take back response

// From FFT
// io.smi.req.ready       --- FFT capable of taking requests (tied high; nothing to stall)
// io.smi.resp.valid      --- FFT did something (write to memory successful; data to Rocket valid)
// io.smi.resp.bits       --- FFT data to be sent to Rocket

class FFTSmiReq(val dataWidth: Int, val addrWidth: Int) extends Bundle {
  val rw = Bool()
  val addr = UInt(width = addrWidth)
  val data = Bits(width = dataWidth)

  override def cloneType =
    new FFTSmiReq(dataWidth, addrWidth).asInstanceOf[this.type]
}

class FFTSmiIO(val dataWidth: Int, val addrWidth: Int) extends Bundle {
  // Rocket -> FFT
  val req = Decoupled(new FFTSmiReq(dataWidth, addrWidth))
  // FFT -> Rocket
  val resp = Decoupled(Bits(width = dataWidth)).flip

  override def cloneType =
    new FFTSmiIO(dataWidth, addrWidth).asInstanceOf[this.type]
}


class RocketToFFT extends Module {

///////////////////////////////////////////// SETUP FFT

  // TODO: Print used params
  val fftParams = GeneratorParams(
    complex = ComplexParams(
      intBits       = 4,
      fracBits      = 19,
      use4Muls      = true,
      mulPipe       = 2,
      addPipe       = 0.333,
      trimType      = Truncate,
      overflowType  = Grow,
      mulFracGrowth = 1
    ),
    clock = ClockParams(
      periodx100ps  = Math.floor(162.76).toInt
    ),
    fft = FFTParams(
      sizes   = List( 12,24,48,96,192,384,768,36,72,144,288,576,1152,108,216,432,864,324,648,1296,972,
                      60,120,240,480,960,180,360,720,300,600,1200,540,1080,900).sorted ++
                List( 64,128,256,512,1024,2048,1536),
      normalized = true,
      generateOffset = true
    ),
    test = TestParams(
      frames  = 5
    )
  )

  // TODO: Get rid of this (esp. in GenInit)
  val args = Array("-params_true_true")
  val (isFixedParam,p) = Init({fftParams}, jsonName = "", args = args)

  val fft = DSPModule(new FFT(DSPFixed(p.complex.getFixedParams),p), "rocket_fft")

  // TODO: Write getter function
  val fixedWidth = p.complex.intBits + p.complex.fracBits + 1
  val complexWidth = 2*fixedWidth
  val inputMemLength = p.fft.sizes.max

///////////////////////////////////////////// ROCKET <--> FFT INTERFACE

  val ctrlRegNames = List("fftIdx","isFFT","setupDone","calcDone")
  // Note: write to setupDone --> enable setup; read from setupDone --> true if actually done
  // Same for calcDone

  val ctrlRegBaseAddr = 2*inputMemLength
  val numCtrlRegs = ctrlRegNames.length
  // Read + write mem + ctrl regs
  val totalInterfaceMem = 2*inputMemLength + numCtrlRegs
  val addrWidth = DSPUInt.toBitWidth(totalInterfaceMem-1)
  val dataWidth = {
    val dblWidth = 64
    if (complexWidth > dblWidth) Error("Rocket data interface can't support > 64 bits")
    dblWidth
  }

  Status("Rocket memory length: " + totalInterfaceMem)
  Status("Rocket memory address width: " + addrWidth)
  Status("Rocket data width needed (LSBs of 64): " + complexWidth)

  val ctrlRegAddrs = ctrlRegNames.zipWithIndex.map{ n => {
    val addr = ctrlRegBaseAddr + n._2
    Status(n._1 + " Address: " + addr)
    addr
  }}

  val io = new Bundle {
    val smi = new FFTSmiIO(dataWidth,addrWidth).flip
  }

  // Setup control registers
  val fftIdx = RegInit(DSPUInt(0,Params.getFFT.nCount-1))
  val isFFT = RegInit(DSPBool(true))
  val setupDone = RegInit(Bool(false))
  val calcDone = RegInit(Bool(false))
  fft.setup.fftIdx := fftIdx
  fft.setup.isFFT := isFFT

  // TODO: do this name thing for all 0 until nums
  val dataMem = List("in","out").map(i => {
    DSPModule(
      new Memory(
        Complex(DSPFixed(p.complex.getFixedParams)),
        depth = inputMemLength,
        seqRead = Params.getDelays.memSeqRead,
        outReg = Params.getDelays.memOutReg,
        conflictHandling = false
      ),
      nameExt = "mem_" + i
    )
  })
  val MemI = 0
  val MemO = 1

  // Accessing control register rather than data mem
  val addrIsCtrlReg = io.smi.req.bits.addr >= UInt(2*inputMemLength)

  // Accessing input mem
  val addrIsInMem = io.smi.req.bits.addr < UInt(inputMemLength)
  val inMemAddr = io.smi.req.bits.addr

  // Accessing output mem
  val addrIsOutMem = (!addrIsInMem) & (!addrIsCtrlReg)
  val outMemAddr = io.smi.req.bits.addr - UInt(inputMemLength)

  // Rocket to FFT input valid, FFT can accept input
  val globalWE = io.smi.req.fire() & io.smi.req.bits.rw

  // Convert from bits to complex
  val realIn = DSPFixed(io.smi.req.bits.data(2*fixedWidth-1,fixedWidth).toSInt,p.complex.getFixedParams)
  val imagIn = DSPFixed(io.smi.req.bits.data(fixedWidth-1,0).toSInt,p.complex.getFixedParams)
  val complexIn = Complex(realIn,imagIn)

  // Hook up in memory (only Rocket can write)
  dataMem(MemI).io.wAddr := DSPUInt(inMemAddr,inputMemLength-1)
  dataMem(MemI).io.WE := DSPBool(globalWE & addrIsInMem)
  dataMem(MemI).io.dIn := complexIn
  fft.io.din := dataMem(MemI).io.dOut

  // Hook up out memory (only FFT can write, only Rocket can read)
  dataMem(MemO).io.wAddr := fft.ctrl.k
  dataMem(MemO).io.dIn := fft.io.dout

  // Note: Need to hold read address + data out until Rocket receives it
  val outMemReadAddr = RegInit(DSPUInt(0,inputMemLength-1))
  // Store new read address every time Rocket to FFT addr, etc. is valid and FFT ready to take request
  outMemReadAddr := Mux(DSPBool(io.smi.req.fire()),DSPUInt(outMemAddr,inputMemLength-1),outMemReadAddr)
  dataMem(MemO).io.rAddr := outMemReadAddr

  // Write to fftIdx?
  val isFFTIdxAddr = io.smi.req.bits.addr === UInt(ctrlRegAddrs(ctrlRegNames.indexOf("fftIdx")))
  val fftIdxWE = DSPBool(globalWE & isFFTIdxAddr)
  val dataAsFFTIdx = DSPUInt(io.smi.req.bits.data,fftIdx.getRange.max)
  fftIdx := Mux(fftIdxWE,dataAsFFTIdx,fftIdx)

  // Write to isFFT?
  val isFFTAddr = io.smi.req.bits.addr === UInt(ctrlRegAddrs(ctrlRegNames.indexOf("isFFT")))
  val isFFTWE = DSPBool(globalWE & isFFTAddr)
  isFFT := Mux(isFFTWE,DSPBool(io.smi.req.bits.data(0)),isFFT)

  // *Done addresses
  val isSetupDoneAddr = io.smi.req.bits.addr === UInt(ctrlRegAddrs(ctrlRegNames.indexOf("setupDone")))
  val isCalcDoneAddr = io.smi.req.bits.addr === UInt(ctrlRegAddrs(ctrlRegNames.indexOf("calcDone")))

  // Read address needs to be held until Rocket acknowledges it's received data
  // Store new read address on Rocket request fire
  val isFFTIdxRAddr = RegInit(Bool(true))
  val isFFTRAddr = RegInit(Bool(true))
  val rAddrIsInMem = RegInit(Bool(true))
  val rAddrIsOutMem = RegInit(Bool(true))
  val isCalcDoneRAddr = RegInit(Bool(true))
  val isSetupDoneRAddr = RegInit(Bool(true))
  isFFTIdxRAddr := Mux(io.smi.req.fire(),isFFTIdxAddr,isFFTIdxRAddr)
  isFFTRAddr := Mux(io.smi.req.fire(),isFFTAddr,isFFTRAddr)
  rAddrIsInMem := Mux(io.smi.req.fire(),addrIsInMem,rAddrIsInMem)
  rAddrIsOutMem := Mux(io.smi.req.fire(),addrIsOutMem,rAddrIsOutMem)
  isCalcDoneRAddr := Mux(io.smi.req.fire(),isCalcDoneAddr,isCalcDoneRAddr)
  isSetupDoneRAddr := Mux(io.smi.req.fire(),isSetupDoneAddr,isSetupDoneRAddr)

  // Pass correct value to output
  when(isFFTIdxRAddr){
    io.smi.resp.bits := fftIdx.toBits
  }.elsewhen(isFFTRAddr){
    io.smi.resp.bits := isFFT.toBits
  }.elsewhen(rAddrIsInMem){
    io.smi.resp.bits := dataMem(MemI).io.dOut.toBits
  }.elsewhen(rAddrIsOutMem) {
    io.smi.resp.bits := dataMem(MemO).io.dOut.toBits
  }.elsewhen(isSetupDoneRAddr) {
    io.smi.resp.bits := setupDone.toBits
  }.elsewhen(isCalcDoneRAddr) {
    io.smi.resp.bits := calcDone.toBits
  }.otherwise{
    io.smi.resp.bits := UInt(0)
  }

  // 3 main FFT response states
  // Idle: req.ready & !resp.valid --> (on req.fire)
  // Processing: !req.ready & !resp.alid --> (on counter = delay-1)
  // DataOut: !req.ready & resp.valid --> (on resp.fire)
  // Idle
  // Actual state machine for read, but use it for write too

  // TODO: Write fancier state machine, make sure no state reuse
  val idle = UInt(2)
  val processing = UInt(1)
  val dataout = UInt(0)

  val state = RegInit(idle)
  val memDlyCounter = RegInit(DSPUInt(0,Params.getDelays.memReadAtoD))
  // Note: req.fire only occurs in Idle
  val memDlyCountNext = (memDlyCounter + DSPUInt(1)).shorten(memDlyCounter.getRange.max)
  memDlyCounter := Mux(DSPBool(io.smi.req.fire() & (state === idle)),DSPUInt(0),memDlyCountNext)
  val memDlyCount = memDlyCounter.toUInt

  when((state === idle) & io.smi.req.fire()){
    state := processing
  }.elsewhen((state === processing) & (memDlyCount === UInt(Params.getDelays.memReadAtoD-1))){
    state := dataout
  }.elsewhen((state === dataout) & io.smi.resp.fire()){
    state := idle
  }
  io.smi.resp.valid := (state === dataout)
  io.smi.req.ready := (state === idle)

///////////////////////////////////////////// ROCKET <--> FFT INTERFACE

  // setupDone? (write to to initiate setup); read = true means done
  val isSetupDoneWE = globalWE & isSetupDoneAddr

  when(isSetupDoneWE){
    // Once setup starts, it's not done until flag goes high
    setupDone := Bool(false)
  }.elsewhen(fft.setup.done.toBool){
    setupDone := Bool(true)
  }

  // Setup enable goes high when user writes to the setupDone reg
  // (expects that fftIdx, isFFT registers are already valid)
  // It stays high until we've made sure the FFT module has seen it (on IO clkEn)
  // and then goes low
  val setupEn = RegInit(Bool(false))
  when(isSetupDoneWE){
    setupEn := Bool(true)
  }.elsewhen(fft.ctrl.clkEn.toBool & setupEn){
    setupEn := Bool(false)
  }
  fft.setup.enable := DSPBool(setupEn)

  // Get max k for particular FFT
  val KmaxLUT = DSPModule(new IntLUT2D(List(Params.getFFT.sizes.map(_ - 1)).transpose), "fftn")
  KmaxLUT.io.addr := fftIdx
  val kmax = KmaxLUT.io.dout

  // Calculation states (don't reuse above)
  val notCalc = UInt(2)
  val calcSync = UInt(1)
  val calc1Frame = UInt(0)

  val calcState = RegInit(notCalc)

  // Not calculating --> req.fire & writing to calcDone register
  // Synchronizing to io clk --> slow clk enable
  // Calculating --> (k = max & valid & slowEn) = captured 1 frame
  // Not calculating
  val calcDone1Frame = fft.ctrl.outValid & fft.ctrl.clkEn & (fft.ctrl.k === kmax.head)

  // calcDone? (write to run calculation); read = true means done
  val isCalcDoneWE = globalWE & isCalcDoneAddr

  when(isCalcDoneWE){
    // Once calc starts, it's not done until flag goes high
    calcDone := Bool(false)
  }.elsewhen(calcDone1Frame.toBool){
    calcDone := Bool(true)
  }

  when((calcState === notCalc) & isCalcDoneWE){
    calcState := calcSync
  }.elsewhen((calcState === calcSync) & fft.ctrl.clkEn.toBool){
    calcState := calc1Frame
  }.elsewhen((calcState === calc1Frame) & calcDone1Frame.toBool){
    calcState := notCalc
  }

  // Keeps track of read address for inputting to FFT (wraps)
  // Note: reset has precendence
  val calcInCounter = DSPModule(new CalcInCounter(inputMemLength-1),"calcInAddr")
  calcInCounter.iCtrl.reset := DSPBool(calcState === calcSync) & fft.ctrl.clkEn
  calcInCounter.iCtrl.change.get := DSPBool(calcState === calc1Frame) & fft.ctrl.clkEn
  calcInCounter.io.max.get := kmax.head

  // Can read from input memory only when not calculating
  val inMemReadAddrSMI = RegInit(DSPUInt(0,inputMemLength-1))
  // Store new read address every time Rocket to FFT addr, etc. is valid and FFT ready to take request
  inMemReadAddrSMI := Mux(DSPBool(io.smi.req.fire()),DSPUInt(inMemAddr,inputMemLength-1),inMemReadAddrSMI)
  dataMem(MemI).io.rAddr := Mux(DSPBool(calcState === notCalc),inMemReadAddrSMI,calcInCounter.io.out)

  val fftIOEn = RegInit(Bool(false))
  when((calcState === calc1Frame) & fft.ctrl.clkEn.toBool){
    // Data associated with reset address is valid on the following clock cycle
    // TODO: Generalize; now assumes memory delay is 2
    fftIOEn := Bool(true)
  }.elsewhen((calcState === calc1Frame) & calcDone1Frame.toBool){
    // After you've gotten one full frame of output data, freeze
    fftIOEn := Bool(false)
  }.elsewhen(calcState =/= calc1Frame){
    // Redundant
    fftIOEn := Bool(false)
  }
  fft.ctrl.enable := DSPBool(fftIOEn)
  // Only write enable while calculating
  dataMem(MemO).io.WE := fft.ctrl.enable & fft.ctrl.outValid

  // One high reset pulse @ the start of calculation (of 1 frame); aligned with read data out
  val fftReset = RegInit(Bool(false))
  when(calcInCounter.iCtrl.reset.toBool){
    fftReset := Bool(true)
  }.elsewhen((calcState === calc1Frame) & fft.ctrl.clkEn.toBool & (calcInCounter.io.out === DSPUInt(1)).toBool){
    // TODO: Fix timing for more generalized versions
    // (reset = 1 when data0 out, corresponding to addr = 1 for memDly = 2)
    fftReset := Bool(false)
  }
  fft.ctrl.reset := DSPBool(fftReset)

}