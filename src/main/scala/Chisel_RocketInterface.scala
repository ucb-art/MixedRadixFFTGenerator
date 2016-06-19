package FFT
import ChiselDSP._
import Chisel.{Pipe =>_,Complex => _,Mux => _, RegInit => _, RegNext => _, Counter => _, when => _, _}

// Allows use of DSPTester
class RocketToFFTWrapper extends DSPModule {
  val rocketToFFT = Module(new RocketToFFT)
  override val io = new Bundle {
    val smi = new FFTSmiIO(rocketToFFT.dataWidth,rocketToFFT.addrWidth).flip
  }
  io <> rocketToFFT.io

  val memMap = rocketToFFT.memMap
}

// Module for Rocket support
class RocketToFFT extends Module {

///////////////////////////////////////////// SETUP FFT

  // TODO: Print used params
  val fftParams = RocketInterfaceParams()

  // TODO: Get rid of this (esp. in GenInit)
  val args = Array("-params_true_true")
  val (isFixedParam,p) = Init({fftParams}, jsonName = "", args = args)

  val fft = DSPModule(new FFT(DSPFixed(p.complex.getFixedParams),p), "rocket_fft")

  val fixedWidth = p.complex.getFixedWidth
  val inputMemLength = p.fft.sizes.max

///////////////////////////////////////////// SETUP MEMORIES

  val memSpecs:List[MemorySpecs[Data]] = List(
    MemorySpecs(
      key = "toFFT",
      depth = inputMemLength,
      dataType = Complex(DSPFixed(p.complex.getFixedParams))
    ),
    MemorySpecs(
      key = "fromFFT",
      depth = inputMemLength,
      isWritable = false,
      dataType = Complex(DSPFixed(p.complex.getFixedParams))
    ),
    MemorySpecs(
      key = "fftIdx",
      dataType = DSPUInt(0,Params.getFFT.nCount-1)
    ),
    MemorySpecs(
      key = "isFFT",
      // Data type for depth = 1 memories should be representative lit (with correct range)
      dataType = DSPBool(true)
    ),
    MemorySpecs(
      key = "k",
      isWritable = false,
      dataType = DSPUInt(0,inputMemLength-1)
    ),
    MemorySpecs(
      key = "setupDone",
      // Value not actually written to this, but will read true when done
      isCtrl = true,
      dataType = DSPBool(false)
    ),
    MemorySpecs(
      key = "calcDone",
      // Value not actually written to this, but will read true when done
      isCtrl = true,
      dataType = DSPBool(false)
    )
  )

  // Maps memories to appropriate addresses
  val memMap = memSpecs.tail.scanLeft(memSpecs.head.key -> memSpecs.head.copy(base = 0))((accum,e) => {
    // current base dependent on previous memory blocks
    val newBase = accum._2.base + accum._2.depth
    e.key -> e.copy(base = newBase)
  }).toMap
  // Print addressing
  memMap.foreach{ x => x._2.printMap}

  // Maximum address + corresponding width needed
  val addrMax = memMap.map(_._2.addrMax).max
  val addrWidth = DSPUInt.toBitWidth(addrMax)
  Status("Requires Rocket Interface Address Width:" + addrWidth + " for total memory length " + (addrMax + 1))
  if (memMap.map(_._2.base).exists(_ == -1)) Error("Some memories have unassigned base addresses")

  val neededDataWidth = memMap.map(_._2.dataType.getWidth).max
  val dataWidth = {
    val dblWidth = 64
    if (neededDataWidth > dblWidth) Error("Rocket data interface can't support > 64 bits")
    dblWidth
  }
  Status("FFT requires " + neededDataWidth + " data bits, 64 used")

  // Connections with Rocket
  val io = new Bundle {
    val smi = new FFTSmiIO(dataWidth,addrWidth).flip
  }

  // IO handling deals with Rocket Decoupled control signals
  val ioHandling = Module(new RocketIOHandling(dataWidth,addrWidth,dly = Params.getDelays.memReadAtoD))
  ioHandling.io.smi <> io.smi

  // Write addresses normalized for each memory element
  val rocketWAddrs = memMap.map(a => {
    if (a._2.depth == 1) a._1 -> DSPUInt(0)
    else {
      // TODO: Get rid of shorten
      val sig = (ioHandling.io.ctrl.wAddr - DSPUInt(a._2.base)).shorten(a._2.depth-1)
      sig.setName("rocketWAddr_" + a._1)
      a._1 -> sig
    }
  }).toMap

  // For memory blocks, write enable also affected by whether the original wAddr is contained within the memory blocks
  // Note: These WE's don't consider whether the blocks are inherently not meant to be written to
  val rocketEs = memMap.map(a => {
    val e = {
      if (a._2.depth == 1) ioHandling.io.ctrl.wAddr === DSPUInt(a._2.base)
      else {
        (ioHandling.io.ctrl.wAddr >= DSPUInt(a._2.base)) & (ioHandling.io.ctrl.wAddr <= DSPUInt(a._2.addrMax))
      }
    }
    e.setName("rocketE_" + a._1)
    a._1 -> e
  }).toMap
  val rocketWEs = rocketEs.map(a => {
    val out = a._2 & ioHandling.io.ctrl.we
    out.setName("rocketWE_" + a._1)
    a._1 -> out
  }).toMap

  // Read addresses should be held after they are retrieved from Rocket
  val rocketRAddrs = rocketWAddrs.map(a => {
    val out = RegInit(DSPUInt(0,a._2.getRange.max))
    out := Mux(ioHandling.io.ctrl.rocketFire,a._2,out)
    out.setName("rocketRAddr_" + a._1)
    a._1 -> out
  }).toMap

  // Used for routing Dout
  val rocketREs = rocketEs.map(a => {
    val out = RegInit(Bool(false))
    out := Mux(ioHandling.io.ctrl.rocketFire.toBool,a._2.toBool,out)
    out.setName("rocketRE_" + a._1)
    a._1 -> out
  }).toMap

  // TODO: Automate the rest i.e. if depth > 1, use memory, else use registers
  // Create actual memories for anything with >1 depth
  val dataMem = memSpecs.filter( x => x.depth > 1).map( x => {
    val mem = DSPModule(
      new Memory(
        x.dataType,
        depth = x.depth,
        seqRead = Params.getDelays.memSeqRead,
        outReg = Params.getDelays.memOutReg,
        conflictHandling = true
      ),
      nameExt = "RocketIOMem_" + x.key
    )
    x.key -> mem
  }).toMap

  // Create registers for everything else
  val regs = memSpecs.filter(x => x.depth == 1).map(x => {
    val out = RegInit(x.dataType)
    out.setName("RocketIOReg_" + x.key)
    x.key -> out
  }).toMap

  // TODO: Check should I be using memMap instead of memSpecs?

///////////////////////////////////////////// USE MEMORIES

  // TODO: Check if DSPBool has any issues with when? (in terms of synthesis)
  // TODO: Figure out how to name regs

  // Only write to register if it's supposed to be writable
  // If it is a go/done register, write 0 (not done @ start)
  regs.foreach{x => {
    val key = x._1
    if (memMap(key).isWritable) {
      val writeBits = {
        if (memMap(key).isCtrl) Bits(0,width=ioHandling.io.ctrl.din.getWidth)
        else ioHandling.io.ctrl.din
      }
      when(rocketWEs(key)) {
        x._2 match {
          case u: DSPUInt => x._2 := DSPUInt(writeBits,u.getRange.max)
          case b: DSPBool => x._2 := DSPBool(writeBits(0))
          case _ => Error("Interface type not supported yet.")
        }
      }
    }
  }}

  // TODO: Check interaction of when's with bools and dspbools (no elsewhen, just when(){}, when(){}

  // Flag indicating setup is done
  when(fft.setup.done){
    regs("setupDone") := DSPBool(true)
  }

  // Set control registers
  fft.setup.fftIdx := regs("fftIdx")
  fft.setup.isFFT := regs("isFFT")

  // TODO: Automate
  // Hook up toFFT memory (only Rocket can write to it)
  // Note: Read address is more complicated
  dataMem("toFFT").io.wAddr := rocketWAddrs("toFFT")
  dataMem("toFFT").io.WE := rocketWEs("toFFT")
  dataMem("toFFT").io.dIn := Complex.fromBits(DSPFixed(p.complex.getFixedParams),ioHandling.io.ctrl.din)
  fft.io.din := dataMem("toFFT").io.dOut

  // Hook up fromFFT memory (only FFT can write, only Rocket can read)
  // WE more complicated
  dataMem("fromFFT").io.wAddr := fft.ctrl.k
  dataMem("fromFFT").io.dIn := fft.io.dout
  dataMem("fromFFT").io.rAddr := rocketRAddrs("fromFFT")

  // Map to outputs
  ioHandling.io.ctrl.dout := Bits(0,width=ioHandling.io.ctrl.dout.getWidth)
  regs.foreach{x => {
    val key = x._1
    when(rocketREs(key)) {
      ioHandling.io.ctrl.dout := x._2.toBits
    }
  }}
  dataMem.foreach{x => {
    val key = x._1
    when(rocketREs(key)){
      ioHandling.io.ctrl.dout := x._2.io.dOut.asInstanceOf[Complex[DSPFixed]].toFixedBits(32)
    }
  }}

  // TODO: Parameterize 32,64

///////////////////////////////////////////// ROCKET <--> FFT INTERFACE

  // setupEn synchronized to FFT IO clock
  // Note that FFT only cares about the last IO cycle in which it saw
  // setupEn high (you should already have all the configuration registers set)
  val setupEn = RegInit(DSPBool(false))
  when(rocketWEs("setupDone")){
    setupEn := DSPBool(true)
  }.elsewhen(fft.ctrl.clkEn & setupEn){
    setupEn := DSPBool(false)
  }
  // TODO: setupEn condition extraneous ^
  fft.setup.enable := setupEn

  // TODO: Do I really need cloneType? Error was "Statically resolved component differs from dynamically resolved..."
  // Get max k for particular FFT
  val KmaxLUT = DSPModule(new IntLUT2D(List(Params.getFFT.sizes.map(_ - 1)).transpose), "fftks")
  KmaxLUT.io.addr := regs("fftIdx")
  val kmax = KmaxLUT.io.dout.cloneType
  kmax := KmaxLUT.io.dout

  // Idle --> rocketFire & Rocket writing to calcDone register
  // Synchronizing to io clk --> slow clk enable
  // Calculating --> (k = max & output valid & slowEn) = captured 1 frame
  // Idle
  val calcIdle :: calcSync :: calc :: Nil = Enum(UInt(),3)
  val calcState = RegInit(calcIdle)

  val isCalcIdle = calcState === calcIdle
  val isCalcSync = calcState === calcSync
  val isCalc = calcState === calc

  // One valid output frame has been collected
  val frameOutValid = fft.ctrl.outValid & fft.ctrl.clkEn & (fft.ctrl.k === kmax.head)
  when (frameOutValid){
    regs("calcDone") := DSPBool(true)
  }

  val startCalc = isCalcIdle & rocketWEs("calcDone").toBool
  val doneCalcSync = isCalcSync & fft.ctrl.clkEn.toBool
  val doneCalc = isCalc & frameOutValid.toBool
  val calcClkEn = isCalc & fft.ctrl.clkEn.toBool

  // Detected start of calculation
  when(startCalc){
    calcState := calcSync
  // Sync to IO clk
  }.elsewhen(doneCalcSync){
    calcState := calc
  }.elsewhen(doneCalc){
    calcState := calcIdle
  }

  // Keeps track of read address for inputting to FFT (wraps)
  // Note: reset has precendence
  val calcInCounter = DSPModule(new CalcInCounter(inputMemLength-1),"calcInAddr")
  calcInCounter.iCtrl.reset := DSPBool(doneCalcSync)
  calcInCounter.iCtrl.change.get := DSPBool(calcClkEn)
  calcInCounter.io.max.get := kmax.head

  if (Params.getDelays.memReadAtoD % Params.getIO.clkRatio != 0)
    Error("Interface memory read address -> data delay should be an integer multiple of the IO to calc clk ratio")

  // Counts cycles for memory read address to data valid delay
  val memReadDlyCount = RegInit(UInt(Params.getDelays.memReadAtoD))
  when(doneCalcSync){
    memReadDlyCount := UInt(0)
  }.elsewhen(isCalc & memReadDlyCount =/= UInt(Params.getDelays.memReadAtoD)){
    memReadDlyCount := memReadDlyCount + UInt(1)
  }

  val doneReadDly = (memReadDlyCount === UInt(Params.getDelays.memReadAtoD-1)) & isCalc
  // FFT reset should be on the same cycle that data out @ first address is valid
  // TODO: get rid of redundant cond. i.e. fftReset
  val fftReset = RegInit(DSPBool(false))
  when(doneReadDly){
    fftReset := DSPBool(true)
  }.elsewhen(fftReset.toBool & calcClkEn){
    fftReset := DSPBool(false)
  }
  fft.ctrl.reset := fftReset

  // TODO: Remove redundant conditions
  // Enable FFT (aligned with first data into FFT)
  val fftIOEn = RegInit(DSPBool(false))
  when(doneCalcSync | doneCalc | isCalcIdle){
    fftIOEn := DSPBool(false)
  }.elsewhen(doneReadDly){
    fftIOEn := DSPBool(true)
  }
  fft.ctrl.enable := fftIOEn

  // If calculating, the read address should be related to the FFT input; otherwise
  // you can read using Rocket read address
  dataMem("toFFT").io.rAddr := Mux(DSPBool(isCalcIdle),rocketRAddrs("toFFT"),calcInCounter.io.out)

  // Only write output while in calculation mode
  dataMem("fromFFT").io.WE := fftIOEn & fft.ctrl.outValid

  // Store k value for good measure
  when(isCalc){
    regs("k") := fft.ctrl.k
  }

  // TODO: need to save prev. read in address when start/stop enabling

}