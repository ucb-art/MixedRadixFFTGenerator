package FFT
import ChiselDSP._
import Chisel.{Pipe =>_,Complex => _,Mux => _, RegInit => _, RegNext => _, _}

class IOSetupIO extends IOBundle {
  // Index of current FFT N
  val fftIdx = DSPUInt(INPUT,Params.getFFT.nCount - 1)
  // Enable setup
  val enable = DSPBool(INPUT)
  // Done with IO setup
  val done = DSPBool(OUTPUT)
}

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

  val setup = new IOSetupIO
  val ctrl = new IOCtrlIO

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
    val usedLoc = primeIdxMatch.zipWithIndex.tail.foldLeft(DSPUInt(0, max = primeIdx.length - 1))(
      (accum, x) => accum | (DSPUInt(x._2, max = primeIdx.length - 1) ? x._1)
    )
    (isUsed,usedLoc)
  }}.unzip
  val isUsed = Vec(isUsedX)
  val usedLoc = Vec(usedLocX)








  // TODO: Enable DSPUInt to address Vec
  val counterPrimeDigits = Vec(usedLoc.map{x => {
    val outMax = primeDigits.map(_.getRange.max).max
    primeDigits.zipWithIndex.foldLeft(DSPUInt(0,outMax))((accum,e)=> (e._1 ? (x === DSPUInt(e._2)))| accum)
  }})













/*

  // TODO: If order is consistent, get rid of extra logic
  val ioIncCounts = Vec(
    // Current counter increments when the counter to its right wraps (don't care if counter unused)
    val rightLoc = usedLoc + DSPUInt(1)

    val rightCounterRadIdx = primeIdx(rightLoc.toUInt)
    // Is last used location? (not dependent on anything else)
    val isLastLoc = (usedLoc === DSPUInt(primeIdx.length-1)) | (rightCounterRadIdx === DSPUInt(0))
    // Get other counters not including this
    val otherCounters = ioIncCounters.filter(_ != e)
    // This counter should update when the counter to the right of it wraps (condition: about to change & is maxed out)
    val changeTemp = otherCounters.foldLeft(DSPBool(false))( (accum,x) => {
      val radIdx = DSPUInt(globalRads.indexOf(x.rad))
      val isRight = (radIdx === rightCounterRadIdx)
      (x.ctrl.change.get & x.ctrl.isMax & isRight) | accum
    })
    // Only update counter if actually used; note that if the counter is the right-most counter, it should always change
    val change = isUsed & (changeTemp | isLastLoc)









    //println("sos" + otherCounters.length)

    //e.ctrl.change.get := change

/*
    e.io.primeDigits := primeDigits(i).shorten(e.io.primeDigits.getRange.max)
    val temp = e.io.out.cloneType
    temp := e.io.out
    temp*/
    // might need to pull isUsed out
    // dit?
    // reg @ count
    // usedLoc to get primeDigits





    usedLoc
  }})*/





















  debug(primeIdx)
  debug(qDIF)
  debug(qDIT)
  //debug(ioIncCounts)
  debug(primeDigits)
  debug(counterPrimeDigits)
  debug(usedLoc)

}