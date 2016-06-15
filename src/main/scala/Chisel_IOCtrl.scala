// TODO: Go through pipes; check delays are accounted for for all modules

package FFT
import ChiselDSP._
import Chisel.{Pipe =>_,Complex => _,Mux => _, RegInit => _, RegNext => _, _}

class IOCtrlIO extends IOBundle {
  // Accepting new input/output data (can pause/resume IO, also hold if IO is slower than calc. clock)
  val enable = DSPBool(INPUT)
  // Reset IO counters & start inputting from n = 0 of frame
  val reset = DSPBool(INPUT)
  // Output valid (will go high sometime after startFrameIn, starting when corresponding k = 0 is valid)
  val outValid = DSPBool(OUTPUT)
  // Output k value
  val k = DSPUInt(OUTPUT,Params.getFFT.sizes.max-1)
}

class IOFlagsO extends IOBundle{
  // IO in DIF mode?
  val isDIF = DSPBool(OUTPUT)
  // IO using mem B?
  val isMemB = DSPBool(OUTPUT)
  // IO is about to wrap
  val wrapCond = DSPBool(OUTPUT)
  // Write enable
  val we = DSPBool(OUTPUT)
}

class IOCtrl extends DSPModule {

  // TODO: Get rid of debug
  val ioFlagsNoDelay = new IOFlagsO

  // TODO: Should pipe 1x or clkRatio?
  // Internal delay from expected with reset, enable to n
  val intDelay = Params.getDelays.ioTop

  val ctrl = new IOCtrlIO
  val ioSetup = (new IOSetupO).flip
  val generalSetup = (new GeneralSetupO).flip
  val o = new nToAddrBankIO

  val calcCtrlI = new CalcCtrlI

  val usedLoc = ioSetup.usedLoc
  val isUsed = ioSetup.isUsed
  val counterPrimeDigits = ioSetup.counterPrimeDigits

  // IO Mod N counters (first set does (n+1)%coprime, second set does (r+Q)%coprime, with wrap
  // condition off of the first set; reused for DIT/DIF)
  val (ioIncCounters, ioQCounters) = Params.getIO.global.map{ case (prime,rad,maxCoprime) => {
    val c1 = BaseNIncCounter(rad, maxCoprime, Params.getIO.clkRatio, nameExt = "ioInc_rad_" + rad.toString)
    val c2 = BaseNAccWithWrap(rad, maxCoprime, Params.getIO.clkRatio, nameExt = "ioQ_rad_" + rad.toString)
    // TODO: Double check timing is ok
    c1.ctrl.reset := ctrl.reset
    c2.ctrl.reset := ctrl.reset
    c1.ctrl.en.get := ctrl.enable
    c2.ctrl.en.get := ctrl.enable
    (c1, c2)
  }}.unzip
  ioFlagsNoDelay.we := ctrl.enable

  val ioIncCounterUsed = ioIncCounters.zip(isUsed)
  // TODO: Custom function, is this redundant logic somewhere? -- don't need to check used, just check max?
  // Wrap when used counters are maxed (held for IO clock)
  val frameWrapCond = ioIncCounterUsed.tail.foldLeft({
    val (counter, used) = ioIncCounterUsed.head
    val max = counter.ctrl.isMax
    (used & max) | (!used)
  })( (accum,e) => {
    val (counter,used) = e
    val max = counter.ctrl.isMax
    ((used & max) | (!used)) & accum
  }) & ctrl.enable
  // Match ioAddr/ioBank delay
  ioFlagsNoDelay.wrapCond := frameWrapCond

  // Is MemB the current memory used for IO?
  // Alternates every frame (starts true after reset)
  val ioMemB = RegInit(DSPBool(false))
  val ioMemBTemp = Mux(frameWrapCond,!ioMemB,ioMemB)
  ioMemB := ctrl.reset | (!ctrl.reset & ioMemBTemp)
  // Match ioAddr/ioBank delay
  ioFlagsNoDelay.isMemB := ioMemB

  // Is IO in DIF mode?
  // Note: switches every other frame. After startFrameIn, true for 1 frame,
  // then false for 2 frames, then true for 2 frames.
  val ioDIF = RegInit(DSPBool(false))

  // DIF DIT DIT DIF DIF
  // B   A   B   A   B
  // ioDIF transitions ever B -> A

  val ioDIFTemp = Mux(frameWrapCond & ioMemB,!ioDIF,ioDIF)
  ioDIF := ctrl.reset | (!ctrl.reset & ioDIFTemp)
  // Match ioAddr/ioBank delay
  ioFlagsNoDelay.isDIF := ioDIF

  // Out valid should go high at the start of the 3rd frame (takes 2 frames to input + calculate)
  // This is on first IO A-> B transition with DIT
  // This should stay high even though io.enable might go low
  val outValid = RegInit(DSPBool(false))
  val outValidTransitionCond = frameWrapCond & !ioMemB & !ioDIF
  val outValidNext = !ctrl.reset & (outValidTransitionCond | outValid)
  outValid := outValidNext

  // Module outValid will go low if IO disabled
  val realOutValid = RegInit(DSPBool(false))
  realOutValid := outValidNext ? calcCtrlI.enable

  // TODO: kReset conditions redundant
  // K starts counting output # (mod FFT size) when output is valid
  val kEnableStart = !outValid & outValidNext & ctrl.enable
  val kEnable = kEnableStart | (outValid & ctrl.enable)
  val kReset = kEnableStart | ctrl.reset | frameWrapCond

  val kCounter = IncReset(Params.getFFT.sizes.max-1,nameExt="kOffset")
  kCounter.iCtrl.reset := kReset
  kCounter.iCtrl.change.get := kEnable

  val isLastLoc = Vec(usedLoc.map(e => {
    // Is last used location? (not dependent on anything else)
    // Note: Misnomer: DIF = last; DIT = first (b/c counters should be flipped)
    val lastLoc = Mux(ioDIF,DSPUInt(usedLoc.length-1),DSPUInt(0))
    (e === lastLoc)
  }))

  // TODO: If order is consistent, get rid of extra logic
  // TODO: Can you leverage previous counter's wrap conditions?
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
    // TODO: Debug? There's a cross-module error for counterPrimeDigits if I directly use shorten. To get around that,
    // I did redundant << then >> operations that should be optimized out in hardware...
    val counterPrimeDigitShort = ((counterPrimeDigits(i) << 1) >> 1).shorten(ioIncCounter.io.primeDigits.getRange.max)
    ioIncCounter.io.primeDigits := counterPrimeDigitShort
    ioQCounter.io.primeDigits := counterPrimeDigitShort
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
      Pipe(temp,intDelay)
    }
    else Pipe(modSum,intDelay)
  }}
  // Note: need to have same lengths for addressing
  val matchCoprimeCountLengths = coprimeCountsX.map(_.length).max
  val coprimeCounts = Vec(coprimeCountsX.map(e => BaseN(e, e.rad).padTo(matchCoprimeCountLengths)))

  // Using delayed DIF flag
  val digitIdx = Mux(ioDIF.pipe(intDelay),ioSetup.digitIdxDIF,ioSetup.digitIdxDIT)

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

  val nToAddrBank = DSPModule(new nToAddrBank)
  nToAddrBank.io.n := nIO
  nToAddrBank.generalSetup <> generalSetup
  o <> nToAddrBank.io

  val delay = intDelay + nToAddrBank.delay
  Status("Total IO Ctrl delay: " + delay)

  // Delayed to be @ the right time @ FFT Top
  ctrl.k := kCounter.io.out.pipe(Params.getDelays.outFlagDelay)

  // At any point, reset should invalidate output
  ctrl.outValid := (0 until Params.getDelays.outFlagDelay).foldLeft(realOutValid)((accum,e) => {
    val temp = accum ? !ctrl.reset
    temp.pipe(1)
  })

}