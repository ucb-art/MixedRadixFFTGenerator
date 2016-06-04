package FFT
import ChiselDSP._
import Chisel.{Pipe =>_,Complex => _,Mux => _, RegInit => _, RegNext => _, _}

class IOCtrlIO extends IOBundle {
  // Accepting new input/output data (can pause/resume IO, also hold if IO is slower than calc. clock)
  val ioEnable = DSPBool(INPUT)
  // Reset IO counters & start inputting from n = 0 of frame
  val startFrameIn = DSPBool(INPUT)
  // Output valid (will go high sometime after startFrameIn, starting when corresponding k = 0 is valid)
  val validOut = DSPBool(OUTPUT)
  // Output k value
  val k = DSPUInt(OUTPUT,Params.getFFT.sizes.max-1)
}

class IOCtrl extends DSPModule {

  val setup = new SetupIO
  val ctrl = new IOCtrlIO
  val generalSetupIO = (new GeneralSetupIO).flip

  // Is IO in DIF mode?
  val ioDIF = RegInit(DSPBool(true))

  // Used for masking to get coprime mod (when operating in Base N and not binary)
  // i.e. mod 4 is equivalent to a 000...00011 bit mask, except this is a digit mask
  // coprimes -> [coprime, corresponding prime, digit mask]
  val primeDigitsLUT = DSPModule(new IntLUT2D(Params.getIO.coprimes.map(_.map(_._3))), "primeDigits")
  primeDigitsLUT.io.addr := setup.fftIdx
  val primeDigits = primeDigitsLUT.io.dout.cloneType
  primeDigits := RegNext(Mux(setup.enable,primeDigitsLUT.io.dout,primeDigits))

  // Indices indicating order of prime decomposition i.e. (3,2,5) might have indices (2,1,3) if
  // Params.getIO.global has primes stored as (2,3,5)
  // global -> [prime used, prime base (max radix), max coprime]
  // globalPrimes(0) associated with unused (i.e. *1)
  // globalRads(0) associated with unused
  // TODO: Add to params
  val globalPrimes = List(1) ++ Params.getIO.global.map(_._1)
  val globalRads = List(0) ++ Params.getIO.global.map(_._2)
  val primeIndices = Params.getIO.coprimes.map(_.map{ x =>
    val prime = x._2
    globalPrimes.indexOf(prime)
  })
  val primeIdxLUT = DSPModule(new IntLUT2D(primeIndices), "primeIdx")
  primeIdxLUT.io.addr := setup.fftIdx
  val primeIdx = primeIdxLUT.io.dout.cloneType
  primeIdx := RegNext(Mux(setup.enable,primeIdxLUT.io.dout,primeIdx))

  // Q for input DIF, DIT
  // Note: qDIF/qDIT nested list arranged as: location, base type, digit
  val qDIFLUT = Params.getIO.qDIF.transpose.zipWithIndex.map{
    case (x,i) => DSPModule(new MixedBaseLUT(x), "qDIF_" + i.toString)
  }
  val qDITLUT = Params.getIO.qDIT.transpose.zipWithIndex.map{
    case (x,i) => DSPModule(new MixedBaseLUT(x), "qDIT_" + i.toString)
  }
  val qDIF = Vec(qDIFLUT.map{ x => {
    x.io.addr := setup.fftIdx
    val out = x.io.dout.cloneType
    out := RegNext(Mux(setup.enable,x.io.dout,out))
    out
  }})
  val qDIT = Vec(qDITLUT.map{ x => {
    x.io.addr := setup.fftIdx
    val out = x.io.dout.cloneType
    out := RegNext(Mux(setup.enable,x.io.dout,out))
    out
  }})

  // IO Mod N counters (first set does (n+1)%coprime, second set does (r+Q)%coprime, with wrap
  // condition off of the first set; reused for DIT/DIF)
  val (ioIncCounters, ioQCounters) = Params.getIO.global.map{ case (prime,rad,maxCoprime) => {
    val c1 = BaseNIncCounter(rad, maxCoprime, Params.getIO.clkRatio, nameExt = "ioInc_rad_" + rad.toString)
    val c2 = BaseNAccWithWrap(rad, maxCoprime, Params.getIO.clkRatio, nameExt = "ioQ_rad_" + rad.toString)
    // TODO: Double check timing is ok
    c1.ctrl.reset := ctrl.startFrameIn
    c2.ctrl.reset := ctrl.startFrameIn
    c1.ctrl.en.get := ctrl.ioEnable
    c2.ctrl.en.get := ctrl.ioEnable
    (c1, c2)
  }}.unzip

  // IO Counter locations (counter radices are specified in a globally defined order;
  // counter ordering is potentially shuffled to match coprime decomposition ordering used in
  // actual FFT), when used
  val (isUsedX,usedLocX) = ioIncCounters.map { case e => {
    // Each counter operates on a fixed radix, previous = to the right (should increment first)
    // Note that counterRadIdx should never = 0
    val counterRadIdx = DSPUInt(globalRads.indexOf(e.rad))
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
  val isUsed = Vec(isUsedX)
  val usedLoc = Vec(usedLocX)

  // TODO: Enable DSPUInt to address Vec
  val counterPrimeDigits = Vec(usedLoc.map{ x => {
    val temp = primeDigits.zipWithIndex.foldLeft(DSPUInt(0))((accum, e) => (e._1 ? (x === DSPUInt(e._2))) | accum)
    temp.pipe(1)
  }})

  val isLastLoc = Vec(usedLoc.map(e => {
    // Is last used location? (not dependent on anything else)
    // Note: Misnomer: DIF = last; DIT = first (b/c counters should be flipped)
    val lastLoc = Mux(ioDIF,DSPUInt(primeIdx.length-1),DSPUInt(0))
    (e === lastLoc)
  }))

  // TODO: If order is consistent, get rid of extra logic
  // Change flag for IO Inc Counters
  val ioIncChange = Vec(usedLoc.zipWithIndex.map{case (usedLocE,i) => {
    // Get other counters not including this
    val otherCounters = ioIncCounters.zipWithIndex.filter(_._2 != i)
    // This counter should update when the counters to its right wrap
    val changeTemp = otherCounters.foldLeft(DSPBool(true))( (accum,counterIdx) => {
      // Note, if a particular counter is unused, corresponding counterLoc will = 0 --> will not affect
      // "true-ness" of changeTemp
      val counterLoc = usedLoc(counterIdx._2)
      // Note: Misnomer: DIF = right; DIT = left (b/c counters should be flipped)
      // Also, when a counter is unused, the location defaults to 0. For DIF, this is ok, because
      // 0 is never to the right of anything; for DIT, the location condition by itself is insufficient
      val isRight = Mux(ioDIF,counterLoc > usedLocE, counterLoc < usedLocE)
      val isRightUsed = isRight & isUsed(counterIdx._2)
      ((counterIdx._1.ctrl.isMax ? isRightUsed) | (!isRightUsed) ) & accum
    })
    // Only update counter if actually used; note that if the counter is the right-most counter, it should always change
    isUsed(i) & (changeTemp | isLastLoc(i))
  }})

  // Get QDIFs associated with counter (note that at the worst case, the # of QDIF columns is 1 less than the # of
  // counters allocated (i.e. for primes 2,3,5, the counter associated with 5 isn't used)
  val counterQDIFs = Vec(usedLoc.zip(isUsed).zip(ioQCounters.map(_.rad)).map{ case ((usedLocE,isUsedE),rad) => {
    //qDIF multi-dimension iterable --> location, base type, digit
    val temp = qDIF.zipWithIndex.foldLeft(BaseN(0,rad))((accum, e) => {
      // Find Q lut output with matching radix, or return 0 if no matching radix is found (remember that
      // there are more counters than Q's) -- tools should get rid of unused accumulator
      val baseNelem = e._1.find(_.rad == rad).getOrElse(BaseN(0,rad))
      (baseNelem ? ((usedLocE === DSPUInt(e._2)) & isUsedE)) | accum
    })
    temp
  }})
  // Same for DIT
  val counterQDITs = Vec(usedLoc.zip(isUsed).zip(ioQCounters.map(_.rad)).map{ case ((usedLocE,isUsedE),rad) => {
    val temp = qDIT.zipWithIndex.foldLeft(BaseN(0,rad))((accum, e) => {
      // Note that qDIT is derived from coprimes.reverse, therefore indexing is flipped
      val baseNelem = e._1.find(_.rad == rad).getOrElse(BaseN(0,rad))
      (baseNelem ? ((usedLocE === DSPUInt(usedLoc.length-1-e._2)) & isUsedE)) | accum
    })
    temp
  }})
  val counterQs = Pipe(Mux(ioDIF,counterQDIFs,counterQDITs),1)

  // First layer counter outputs (see counters above)
  val (ioIncCountsX,ioQCountsX) = ioIncCounters.zip(ioQCounters).zipWithIndex.map{case ((ioIncCounter,ioQCounter),i) =>{
    // Assign # of base-r digits for modding each counter
    ioIncCounter.io.primeDigits := counterPrimeDigits(i).shorten(ioIncCounter.io.primeDigits.getRange.max)
    ioQCounter.io.primeDigits := counterPrimeDigits(i).shorten(ioQCounter.io.primeDigits.getRange.max)
    // Assign change condition to each IO counter
    ioIncCounter.ctrl.change.get := ioIncChange(i)
    // TODO: Cannot mix directions on left hand side of := ??? --> .asOutput
    ioQCounter.io.inc.get := counterQs(i).padTo(ioQCounter.io.inc.get.length).asOutput
    ioQCounter.ctrl.wrap.get := ioIncChange(i)
    val ioIncCount = ioIncCounter.io.out.cloneType
    ioIncCount := ioIncCounter.io.out
    val ioQCount = ioQCounter.io.out.cloneType
    ioQCount := ioQCounter.io.out
    (ioIncCount,ioQCount)
  }}.unzip
  val ioIncCounts = Vec(ioIncCountsX)
  val ioQCounts = Vec(ioQCountsX)

  // IO indexing when broken into coprimes (before decomposition into relevant radices)
  val coprimeCountsX = ioIncCounts.zipWithIndex.map {case (e,i) => {
    val modSum = (e + ioQCounts(i)).maskWithMaxCheck(counterPrimeDigits(i))._1
    // If radix 2 is used in the FFT decomposition, convert Base-4 representation to mixed radix
    // [4,4,...4,2] representation (Note that doing x.rad still results in 4)
    // TODO: Generalize?
    if (modSum.rad == 4 && Params.getBF.rad.contains(2)) {
      val temp = Mux(generalSetupIO.use2 & ioDIF,modSum.toRad42(),modSum)
      Pipe(temp,1)
    }
    else Pipe(modSum,1)
  }}
  // Note: need to have same lengths for addressing
  val matchCoprimeCountLengths = coprimeCountsX.map(_.length).max
  val coprimeCounts = Vec(coprimeCountsX.map(e => BaseN(e, e.rad).padTo(matchCoprimeCountLengths)))

  // Index associated with stage prime
  val stagePrimeIdx = Vec(generalSetupIO.stageRad.map(e => {
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
  val stageIsActive = Vec(stagePrimeIdx.map(x => (x =/= DSPUInt(0)).pipe(1)))
  // DIF digit index associated with stage (note: some digits invalid -- i.e. when stage is inactive)
  val digitIdxDIF = Vec(generalSetupIO.primeStageSum.zipWithIndex.map{case (e,i) => {e - DSPUInt(i+1)}})
  // DIT digit index associated with stage ("digit reverse")
  val digitIdxDIT = Vec(generalSetupIO.prevPrimeStageSum.zipWithIndex.map{case (e,i) => {DSPUInt(i)-e}})
  val digitIdx = Mux(ioDIF,digitIdxDIF,digitIdxDIT)

  // TODO: Make foldLeft into DSPUInt lookup function, decide on MixedRad vs. Vec?
  val nIOX = (0 until Params.getCalc.maxStages).map{ i => {
    // Pick out used coprime @ stage; then pick out used digit of coprime @ stage
    // Note: coprimeCounts uses global; stagePrimeIdx is _ ++ global i.e. prime = 1 to keep track of unused primes
    val coprime = coprimeCounts.zipWithIndex.foldLeft(
      MixedRad(Vec(matchCoprimeCountLengths,DSPUInt(0,Params.getBF.rad.max-1)))
    )(
      (accum,e) => accum | (MixedRad(e._1) ? (DSPUInt(e._2+1) === stagePrimeIdx(i)))
    )
    val digit = coprime.zipWithIndex.foldLeft(DSPUInt(0,Params.getBF.rad.max-1))(
      (accum,e) => accum | (e._1 ? (digitIdx(i) === DSPUInt(e._2)))
    )
    Mux(stageIsActive(i),digit,DSPUInt(0))
  }}
  val nIO = Vec(nIOX)








  // scanin
  // en
  // scan on separate vdd: inmemaddress (up to max fftsize), 32 bit val -- enable: enable mem write
  // scan: fftsize, etc.
  // go: run twice
  // scan send address: en; output clk, start, data

  debug(nIO)
  // mem readen
  // 3 multiplies, 2x rad2
  // n to bank addr
  // separate out counter stuff from constants
  // worst case delay = this + setup delay
  // restart dif/dit mode, also reg?

}