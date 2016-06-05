// TODO: Swap foldLeft with reduceLeft (initial val automatic)

package FFT
import ChiselDSP._
import Chisel.{Pipe =>_,Complex => _,Mux => _, RegInit => _, RegNext => _, _}

class IOSetupO extends IOBundle {

  val numCoprimeCounters = Params.getIO.global.length
  val counterRads = Params.getIO.globalRads.tail
  val maxPrimeDigits = Params.getIO.globalMaxCoprimes.zip(counterRads).map {
    // Digits represented in base prime (not rad)
    case (coprime,rad) => {
      // global -> [prime,maxRadix,maxCoprime]
      val prime = Params.getIO.global.find(_._2 == rad).get._1
      BaseN.numDigits(coprime,prime)
  }}
  val maxStages = Params.getCalc.maxStages

  // Counter associated with particular coprime location (in potentially unique
  // decomposition order)
  val usedLoc = Vec(numCoprimeCounters,DSPUInt(OUTPUT,numCoprimeCounters-1))
  // Is counter used?
  val isUsed = Vec(numCoprimeCounters,DSPBool(OUTPUT))
  // Base-N # of digits (for counter modulus)
  val counterPrimeDigits = Vec(maxPrimeDigits.map(x => DSPUInt(OUTPUT,x)))
  // QDIFs associated with counters
  val counterQDIFs = Vec(Params.getIO.globalMaxCoprimes.zip(counterRads).map{
    case (coprime,rad) => BaseN(OUTPUT,rad,coprime-1)
  })
  // QDITs associated with counters
  val counterQDITs = Vec(counterQDIFs.map(x => x.cloneType))
  // Prime index associated with each stage
  val stagePrimeIdx = Vec(maxStages,DSPUInt(OUTPUT,numCoprimeCounters))
  // Is stage active?
  val stageIsActive = Vec(maxStages,DSPBool(OUTPUT))
  // Used for determining which digit of each coprime is used
  // (for DIF/DIT with bit reversal) for decomposition of coprimes via stages
  val digitIdxDIF = Vec(maxStages,DSPUInt(OUTPUT,maxStages))
  val digitIdxDIT = Vec(maxStages,DSPUInt(OUTPUT,maxStages))

}

class IOSetup(prevSetupDelay: Int) extends DSPModule{

  val setupTop = new SetupTopIO
  val generalSetup = (new GeneralSetupO).flip
  val o = new IOSetupO

  // Used for masking to get coprime mod (when operating in Base N and not binary)
  // i.e. mod 4 is equivalent to a 000...00011 bit mask, except this is a digit mask
  // coprimes -> [coprime, corresponding prime, digit mask]
  val primeDigitsLUT = DSPModule(new IntLUT2D(Params.getIO.numDigits), "primeDigits")
  primeDigitsLUT.io.addr := setupTop.fftIdx
  val primeDigits = primeDigitsLUT.io.dout.cloneType
  primeDigits := RegNext(Mux(setupTop.enable,primeDigitsLUT.io.dout,primeDigits))

  // Indices indicating order of prime decomposition i.e. (3,2,5) might have indices (2,1,3) if
  // Params.getIO.global has primes stored as (2,3,5)
  // global -> [prime used, prime base (max radix), max coprime]
  // globalPrimes(0) associated with unused (i.e. *1)
  // globalRads(0) associated with unused
  // TODO: Add to params
  val globalPrimes = Params.getIO.globalPrimes
  val globalRads = Params.getIO.globalRads
  val primeIndices = Params.getIO.primes.map(_.map( x=> globalPrimes.indexOf(x)))
  val primeIdxLUT = DSPModule(new IntLUT2D(primeIndices), "primeIdx")
  primeIdxLUT.io.addr := setupTop.fftIdx
  val primeIdx = primeIdxLUT.io.dout.cloneType
  primeIdx := RegNext(Mux(setupTop.enable,primeIdxLUT.io.dout,primeIdx))

  // Q for input DIF, DIT
  // Note: qDIF/qDIT nested list arranged as: location, base type, digit
  val qDIFLUT = Params.getIO.qDIF.transpose.zipWithIndex.map{
    case (x,i) => DSPModule(new MixedBaseLUT(x), "qDIF_" + i.toString)
  }
  val qDITLUT = Params.getIO.qDIT.transpose.zipWithIndex.map{
    case (x,i) => DSPModule(new MixedBaseLUT(x), "qDIT_" + i.toString)
  }
  val qDIF = Vec(qDIFLUT.map{ x => {
    x.io.addr := setupTop.fftIdx
    val out = x.io.dout.cloneType
    out := RegNext(Mux(setupTop.enable,x.io.dout,out))
    out
  }})
  val qDIT = Vec(qDITLUT.map{ x => {
    x.io.addr := setupTop.fftIdx
    val out = x.io.dout.cloneType
    out := RegNext(Mux(setupTop.enable,x.io.dout,out))
    out
  }})

  // IO Counter locations (counter radices are specified in a globally defined order;
  // counter ordering is potentially shuffled to match coprime decomposition ordering used in
  // actual FFT), when used
  val (isUsedX,usedLocX) = globalRads.tail.map{ case (rad) => {
    // Each counter operates on a fixed radix, previous = to the right (should increment first)
    // Note that counterRadIdx should never = 0
    val counterRadIdx = DSPUInt(globalRads.indexOf(rad))
    // Get location in primeIdx Vec where counterRadIdx matches (or location doesn't exist = counter unused)
    val primeIdxMatch = Vec(primeIdx.map(_ === counterRadIdx))
    // Is radix + associated counters used?
    val isUsed = primeIdxMatch.tail.foldLeft(primeIdxMatch.head)(_ | _)
    // Where counter is used (determines counting order) -- note that default is loc = 0 (but need to check isUsed to
    // see if it's actually used) -- expect extra OR to be optimized out
    // usedLoc dictates where the counter output goes to
    val usedLoc = primeIdxMatch.zipWithIndex.tail.foldLeft(DSPUInt(0))(
      (accum, x) => accum | (DSPUInt(x._2) ? x._1)
    )
    (isUsed.pipe(1),usedLoc.pipe(1))
  }}.unzip
  o.isUsed := Vec(isUsedX)
  o.usedLoc := Vec(usedLocX)

  // TODO: Enable DSPUInt to address Vec
  o.counterPrimeDigits := Vec(o.usedLoc.zipWithIndex.map{ case(x,i) => {
    val temp = primeDigits.zipWithIndex.foldLeft(DSPUInt(0))((accum, e) => (e._1 ? (x === DSPUInt(e._2))) | accum)
    temp.shorten(o.counterPrimeDigits(i).getRange).pipe(1)
  }})

  // Get QDIFs associated with counter (note that at the worst case, the # of QDIF columns is 1 less than the # of
  // counters allocated (i.e. for primes 2,3,5, the counter associated with 5 isn't used)
  // Same for DIT
  val (counterQDIFsX,counterQDITsX) = o.usedLoc.zip(o.isUsed).zip(globalRads.tail).map{case ((usedLocE,isUsedE),rad) =>{
    //qDIF multi-dimension iterable --> location, base type, digit
    val tempDIF = qDIF.zipWithIndex.foldLeft(BaseN(0,rad))((accum, e) => {
      // Find Q lut output with matching radix, or return 0 if no matching radix is found (remember that
      // there are more counters than Q's) -- tools should get rid of unused accumulator
      val baseNelem = e._1.find(_.rad == rad).getOrElse(BaseN(0,rad))
      (baseNelem ? ((usedLocE === DSPUInt(e._2)) & isUsedE)) | accum
    })
    val tempDIT = qDIT.zipWithIndex.foldLeft(BaseN(0,rad))((accum, e) => {
      // Note that qDIT is derived from coprimes.reverse, therefore indexing is flipped
      val baseNelem = e._1.find(_.rad == rad).getOrElse(BaseN(0,rad))
      (baseNelem ? ((usedLocE === DSPUInt(o.usedLoc.length-1-e._2)) & isUsedE)) | accum
    })
    (Pipe(tempDIF,1),Pipe(tempDIT,1))
  }}.unzip
  // TODO: Always pad to output length? ; Also, cannot mix directions error
  o.counterQDIFs.zip(counterQDIFsX).foreach{ case (out,sig) => out := sig.padTo(out.length).asOutput}
  o.counterQDITs.zip(counterQDITsX).foreach{ case (out,sig) => out := sig.padTo(out.length).asOutput}

  // Index associated with stage prime
  o.stagePrimeIdx := Vec(generalSetup.stageRad.map(e => {
    // TODO: Generalize?
    val stagePrime = {
      if (Params.getBF.rad.contains(4)) Mux(e === DSPUInt(4),DSPUInt(2),e)
      else e
    }
    globalPrimes.zipWithIndex.foldLeft(DSPUInt(0))(
      (accum,x) => accum | (DSPUInt(x._2) ? (DSPUInt(x._1) === stagePrime))
    ).pipe(1)
  }))

  // Stage is used for current FFT
  o.stageIsActive := Vec(o.stagePrimeIdx.map(x => (x =/= DSPUInt(0)).pipe(1)))

  // DIF digit index associated with stage (note: some digits invalid -- i.e. when stage is inactive)
  o.digitIdxDIF := Vec(generalSetup.primeStageSum.zipWithIndex.map{case (e,i) => {
    (e - DSPUInt(i+1)).shorten(o.digitIdxDIF(i).getRange).pipe(1)
  }})
  // DIT digit index associated with stage ("digit reverse")
  o.digitIdxDIT := Vec(generalSetup.prevPrimeStageSum.zipWithIndex.map{case (e,i) => {
    (DSPUInt(i)-e).shorten(o.digitIdxDIT(i).getRange).pipe(1)
  }})

  // Keep track of how long setup should take (this delay is added on top of general setup delay)
  val setupDelay = o.getMaxOutDelay()
  val totalSetupDelay = setupDelay + prevSetupDelay
  setupTop.done := setupTop.enable.pipe(totalSetupDelay)

  Status("General setup + IOSetup take " + totalSetupDelay + " clocks.")

  // TODO: Less conservative delay

}