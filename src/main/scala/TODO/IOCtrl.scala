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

  // TODO: If order is consistent, get rid of extra logic
  // Change flag for IO Inc Counters
  val ioIncChange = Vec(usedLoc.zip(isUsed).zipWithIndex.map{case ((usedLocE,isUsedE),i) => {
    // Is last used location? (not dependent on anything else)
    val isLastLoc = (usedLocE === DSPUInt(primeIdx.length-1))
    // Get other counters not including this
    val otherCounters = ioIncCounters.zipWithIndex.filter(_._2 != i)
    // This counter should update when the counters to its right wrap
    val changeTemp = otherCounters.foldLeft(DSPBool(true))( (accum,counterIdx) => {
      // Note, if a particular counter is unused, corresponding counterLoc will = 0 --> will not affect
      // "true-ness" of changeTemp
      val counterLoc = usedLoc(counterIdx._2)
      val isRight = counterLoc > usedLocE
      ((counterIdx._1.ctrl.isMax ? isRight) | (!isRight) ) & accum
    })
    // Only update counter if actually used; note that if the counter is the right-most counter, it should always change
    isUsedE & (changeTemp | isLastLoc)
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
    Pipe(temp,1)
  }})

  // First layer counter outputs (see counters above)
  val (ioIncCountsX,ioQCountsX) = ioIncCounters.zip(ioQCounters).zipWithIndex.map{case ((ioIncCounter,ioQCounter),i) =>{
    // Assign # of base-r digits for modding each counter
    ioIncCounter.io.primeDigits := counterPrimeDigits(i).shorten(ioIncCounter.io.primeDigits.getRange.max)
    ioQCounter.io.primeDigits := counterPrimeDigits(i).shorten(ioQCounter.io.primeDigits.getRange.max)
    // Assign change condition to each IO counter
    ioIncCounter.ctrl.change.get := ioIncChange(i)
    // TODO: Cannot mix directions on left hand side of := ??? --> .asOutput
    ioQCounter.io.inc.get := counterQDIFs(i).padTo(ioQCounter.io.inc.get.length).asOutput
    ioQCounter.ctrl.wrap.get := ioIncChange(i)
    val ioIncCount = ioIncCounter.io.out.cloneType
    ioIncCount := ioIncCounter.io.out
    val ioQCount = ioQCounter.io.out.cloneType
    ioQCount := ioQCounter.io.out
    (ioIncCount,ioQCount)
  }}.unzip
  val ioIncCounts = Vec(ioIncCountsX)
  val ioQCounts = Vec(ioQCountsX)







// pipe change

/*

  // Should try to minimize mem output length and pad where there is width mismatch
  // CAN PIPELINE DELAY ioFinalConuts
  val ioFinalCounts = Vec(Params.getIO.global.map(_._3).zipWithIndex.map { case (x, i) => {
    if (x == Params.getIO.global.last._3) ioIncCounts1(i)
    else (ioIncCounts1(i) + ioModCounts1(i)).maskWithMaxCheck(primeDigits(i))._1
  }
  })
  debug(ioFinalCounts)





  val iDIFtemp1 = Vec(ioFinalCounts.zipWithIndex.map { case (x, i) => {
    if (Params.getBF.rad.contains(2) && Params.getIO.global(i)._1 == 2 && Params.getBF.rad.contains(4)) {
      // switch to getBF.rad(i) == 2  ????
      // Convert to Mixed radix [4,...4,2] if current FFT size requires radix 2 stage & operating on 2^n coprime
      // FOR DIF 2 always follows 4 and it's the only one with list of length 2
      Mux(DSPBool(numPower(i + 1)(0)), x.toRad42(), x)
    }
    else x
  }
  })
  // registered here?
  // Need to be same length to address properly (really Chisel should error)
  val colLengths = iDIFtemp1.map(x => x.length).max
  val iDIFtemp = Vec(iDIFtemp1.map(x => {
    val set = x.map(y => {
      // delay 2 (calc clk)
      y.reg()
    })
    BaseN(set, x.rad).padTo(colLengths)
    //Vec(set) // basen gets misinterpretted?
  }))
  iDIFtemp.foreach {
    debug(_)
  }
  // when doing addr, warn when not vec col nto same length

*/

  /*
      // dit? -- need out-- how did i handle unused?
      // reg @ count
*/


/*  override val io = new IOBundle {
    val stageSum = stageSumI.cloneType.asOutput
    val stageRad = stageRadI.cloneType.asOutput
    val use2 = use2I.cloneType.asOutput
    val maxRad = maxRadI.cloneType.asOutput
  }*/


















  debug(primeIdx)
  debug(qDIF)
  debug(qDIT)
  debug(ioIncCounts)
  debug(primeDigits)
  debug(counterPrimeDigits)
  debug(usedLoc)
  debug(ioIncChange)
  debug(ioQCounts)
  //debug(counterQDIFs)

  // when doing dit, flip whole thing or only active primes?

}