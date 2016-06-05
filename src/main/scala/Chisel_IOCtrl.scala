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
  // IO in DIF mode?
  val ioDIF = DSPBool(OUTPUT)

  // **** TODO: Separate bundle!

  // Memory address
  val ioAddr = DSPUInt(OUTPUT,Params.getMem.lengths.max-1)
  // Memory bank
  val ioBank = DSPUInt(OUTPUT,Params.getMem.banks-1)
}

class IOCtrl extends DSPModule {

  val ctrl = new IOCtrlIO
  val ioSetup = (new IOSetupO).flip
  val generalSetup = (new GeneralSetupO).flip

  val usedLoc = ioSetup.usedLoc
  val isUsed = ioSetup.isUsed
  val counterPrimeDigits = ioSetup.counterPrimeDigits

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

  // Is IO in DIF mode?
  val ioDIF = RegInit(DSPBool(false))

  val isLastLoc = Vec(usedLoc.map(e => {
    // Is last used location? (not dependent on anything else)
    // Note: Misnomer: DIF = last; DIT = first (b/c counters should be flipped)
    val lastLoc = Mux(ioDIF,DSPUInt(usedLoc.length-1),DSPUInt(0))
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

  val counterQs = Mux(ioDIF,ioSetup.counterQDIFs,ioSetup.counterQDITs)

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
      val temp = Mux(generalSetup.use2 & ioDIF,modSum.toRad42(),modSum)
      Pipe(temp,1)
    }
    else Pipe(modSum,1)
  }}
  // Note: need to have same lengths for addressing
  val matchCoprimeCountLengths = coprimeCountsX.map(_.length).max
  val coprimeCounts = Vec(coprimeCountsX.map(e => BaseN(e, e.rad).padTo(matchCoprimeCountLengths)))

  val digitIdx = Mux(ioDIF,ioSetup.digitIdxDIF,ioSetup.digitIdxDIT)

  // TODO: Make foldLeft into DSPUInt lookup function, decide on MixedRad vs. Vec?
  val nIOX = (0 until Params.getCalc.maxStages).map{ i => {
    // Pick out used coprime @ stage; then pick out used digit of coprime @ stage
    // Note: coprimeCounts uses global; stagePrimeIdx is _ ++ global i.e. prime = 1 to keep track of unused primes
    val coprime = coprimeCounts.zipWithIndex.foldLeft(
      MixedRad(Vec(matchCoprimeCountLengths,DSPUInt(0,Params.getBF.rad.max-1)))
    )(
      (accum,e) => accum | (MixedRad(e._1) ? (DSPUInt(e._2+1) === ioSetup.stagePrimeIdx(i)))
    )
    val digit = coprime.zipWithIndex.foldLeft(DSPUInt(0,Params.getBF.rad.max-1))(
      (accum,e) => accum | (e._1 ? (digitIdx(i) === DSPUInt(e._2)))
    )
    Mux(ioSetup.stageIsActive(i),digit,DSPUInt(0))
  }}
  val nIO = Vec(nIOX)

  debug(nIO)
}



/*  // scanin
  // en
  // scan on separate vdd: inmemaddress (up to max fftsize), 32 bit val -- enable: enable mem write
  // scan: fftsize, etc.
  // go: run twice
  // scan send address: en; output clk, start, data
  // clk going through
  // read out scanned data (mem @ addr o)


  // mem readen
  // 3 multiplies
  // n to bank addr
  // restart dif/dit mode
  // test 100
  // fix frameout timing
  */// twiddle ctrl