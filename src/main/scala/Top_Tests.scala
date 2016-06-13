// TODO: Support IFFT (normalization too), check signal limit (in double test), build in CP/GI halt
// TODO: Update meaning of control signals

package FFT
import ChiselDSP._
import Chisel.{Complex => _, _}

class FFTTests[T <: FFT[_ <: DSPQnm[_]]](c: T, fftn: Option[Int] = None, in: Option[List[ScalaComplex]] = None,
                                         normalized:Boolean, genOffset:Boolean)
                                        extends DSPTester(c) {

  traceOn = false

  // Default is run all tests
  if (fftn == None) runAll()
  else run(fftn.get)

  Status("\nTested FFTs: [" + Tracker.testedFFTs.mkString(", ") + "]")

  /** Run all tests for all FFTNs */
  def runAll() : Unit = runTo(Params.getFFT.sizes.last)

  /** Run for all FFT sizes until (and including) N */
  def runTo(n: Int): Unit = {
    val sizes = Params.getFFT.sizes
    val idx = sizes.indexOf(n)
    for (i <- 0 to idx) run(sizes(i))
  }

  /** Run tests for desired FFTN */
  def run(n: Int) : Unit = {
    Tracker.testedFFTs = Tracker.testedFFTs :+ n
    Tracker.reset(n)
    Status("///////////////////////////////////////// FFTN = " + n)
    stepThrough(n)
  }

  /** Step through test vectors for FFTN */
  def stepThrough(n: Int): Unit = {
    val idxN = Params.getFFT.sizes.indexOf(n)
    if (idxN < 0) Error("FFTN is not included in this generated output")
    val inVec = in.getOrElse(TestVectors.getIn(idxN))
    val outVec = {
      if (in == None) Some(TestVectors.getOut(idxN))
      else {
        if (inVec.length % n != 0) Error("Test vector length must be an integer multiple of the FFT N")
        if (fftn == None) Error("Custom input vector requires custom FFT size!")
        None
      }
    }
    testFFTNio(idxN,fftTF = true,inVec,outVec)
  }

  /** Setup new FFTN (Runtime configuration) where fftTF true -> FFT else IFFT */
  def newSetup(fftIndex:Int, fftTF:Boolean){
    Status("///////////////////////////////////////// NEW SETUP")
    val initT = t
    poke(c.setup.enable,true)
    poke(c.setup.fftIdx,fftIndex)
    poke(c.setup.isFFT,fftTF)
    step(Params.getIO.clkRatio)
    poke(c.setup.enable,false)
    poke(c.setup.fftIdx,0)
    // Wait until done setting up before checking setup constants (note that step size is not always 1 -- depends on
    // user parameters)
    var setupDone = peek(c.setup.done)
    var whileCnt = 0
    while (!setupDone){
      whileCnt = whileCnt + 1
      if (whileCnt > 100) Error("Setup is not completing...")
      inSetupDebug()
      step(1)
      setupDone = peek(c.setup.done)
    }
    setupDebug()
    val modT = (t-initT)%Params.getIO.clkRatio
    if (modT != 0) Error("Setup done and other control signals should occur as expected from IO clock rate")
    // SETUP_DONE should be held for clkRatio calc clk cycles
    for (i <- 0 until Params.getIO.clkRatio-1) {step(1);expect(c.setup.done,true)}
    Status("///////////////////////////////////////// SETUP DONE")
  }

  /** Feed in FFT inputs and read FFT outputs */
  def testFFTNio(fftIndex:Int, fftTF:Boolean, in:List[ScalaComplex], out:Option[List[ScalaComplex]]){
    // Safety initialize control signals to false
    reset(Params.getIO.clkRatio)
    poke(c.ctrl.enable,true)
    poke(c.ctrl.reset,false)
    poke(c.setup.enable,false)
    newSetup(fftIndex,fftTF)
    step(Params.getIO.clkRatio + 1)
    // After setup, start sending data to process (start on the second IO clock period after SETUP_DONE)
    poke(c.ctrl.reset,true)
    stepTrack(Params.getIO.clkRatio,in,out)
    poke(c.ctrl.reset,false)
    val n = Params.getFFT.sizes(fftIndex)
    val frames = in.length/n
    // Output k = 0 starts 2 frames after n = 0
    for (i <- 0 until frames + 2; j <- 0 until n){
      stepTrack(Params.getIO.clkRatio,in,out)
    }
    // Go a little past to account for pipeline delay
    while (Tracker.outStep < in.length){
      stepTrack(Params.getIO.clkRatio,in,out)
    }
    if (!Tracker.outValid) Error("Output valid was never detected...")
  }

  /** Peek and then step, where num should be = clkRatio */
  def stepTrack(num:Int, in:List[ScalaComplex], out:Option[List[ScalaComplex]]){
    var firstOutValid = false
    for (i <- 0 until num) {
      calcDebug(i)
      val inVal = in(Tracker.inStep % in.length)
      // Checks when k = 0 is output (Detects transition to first symbol) & dumps input
      if (i == 0) {
        poke(c.io.din, inVal)
        val firstOut = peek(c.ctrl.outValid)
        if (firstOut && !Tracker.firstSymbol) {
          Status("///////////////////////////////////////// FRAME = %d, K = 0".format(Tracker.frameNum))
          // Streaming output valid after start of first output symbol detected
          Tracker.outValid = true
          Tracker.frameNum = Tracker.frameNum + 1
          firstOutValid = true
        }
        Tracker.firstSymbol = firstOut
      }
      else{
        // FRAME_FIRST_OUT should be held for clkRatio calc clk cycles if true
        if (firstOutValid) expect(c.ctrl.outValid,true)
        else expect(c.ctrl.outValid,false)
      }
      // Read output if valid & check for error
      if (Tracker.outValid){
        val errorString = " FFTN = " + Tracker.FFTN + ", FRAME = " + (Tracker.frameNum-1) +
                          ",  k = " + Tracker.outStep%Tracker.FFTN + "\n "
        if (out != None) {
          val outExpected = out.get(Tracker.outStep)
          // TODO: Support IFFT(!)
          // Normalize FFT appropriately
          val outExpectedNormalized = {
            if(normalized) outExpected**(1/math.sqrt(Tracker.FFTN),typ = Real)
            else outExpected
          }
          expect(c.io.dout, outExpectedNormalized, Tracker.FFTN.toString, errorString)
        }
        // Doesn't compare, just stores results for post-processing
        else if (i == 0) {
          Tracker.FFTOut = Tracker.FFTOut :+ peek(c.io.dout)
        }
        if(genOffset) expect(c.ctrl.k,Tracker.outStep%Tracker.FFTN, "Offset value unexpected")
      }
      else{
        if(genOffset) expect(c.ctrl.k,0, "Offset value should be 0 if output isn't valid initially")
      }
      step(1)
    }
    // Steps by IO clock rate (output only steps if output is known to be valid)
    Tracker.inStep =  Tracker.inStep + 1
    if (Tracker.outValid) {
      Tracker.outStep = Tracker.outStep + 1
    }
  }

  /** Placeholder for debugging signals */
  var calcDone = false
  def calcDebug(i:Int): Unit = {
    val temp = traceOn
    traceOn = true

    if (Tracker.FFTN == 12) {
      //peek(c.ioDIT)
      //peek(c.calcMemB)
      //peek(c.IOCtrl.ioIncCounters(0).ctrl.isMax)
      //peek(c.CalcCtrl.maxStageCountUsed)
      //peek(c.CalcCtrl.currentStage)
      //peek(c.calcControl.calcn)
      //peek(c.CalcCtrl.n)
      //val a = peek(c.calcControl.calcn).toList
      //val b = peek(c.CalcCtrl.n).toList
      //if (!a.sameElements(b) && Tracker.inStep >= 2) Status("------------cc")

    }

    /*if (Tracker.FFTN == 24){
      val a = peek(c.calcControl.calcn).toList
      val b = peek(c.CalcCtrl.n).toList
      if (!a.sameElements(b) && Tracker.inStep >= 2) Error("------------cc")
    }*/

    traceOn = temp

    //if (!peek(c.ons).toList.sameElements(peek(c.IOCtrl.nIO).toList) & Tracker.inStep >= 2) Error("nio")
    //if (peek(c.ioAddr) != peek(c.IOCtrl.o.addr) && Tracker.inStep >= 2) Error("addr")
    //if (peek(c.ioBank) != peek(c.IOCtrl.o.bank) && Tracker.inStep >= 2) Error("bank")
    //if (peek(c.calcControl.calcn) != peek(c.CalcCtrl.n) && Tracker.inStep >= 2) Error("cc")

    //val a = peek(c.calcControl.calcn).toList
    //val b = peek(c.CalcCtrl.n).toList
    //if (!a.sameElements(b) && Tracker.inStep >= 2) Error("------------cc")

    //if (!peek(c.calcBank).toList.sameElements(peek(c.CalcCtrl.o.banks).toList) && Tracker.inStep >= 2) Error("calcbanks")
    //if (!peek(c.calcAddr).toList.sameElements(peek(c.CalcCtrl.o.addrs).toList) && Tracker.inStep >= 2) Error("calcaddrs")

  }
  def setupDebug(): Unit = {
    val temp = traceOn

    traceOn = true
    //peek(c.IOSetup.o.stagePrimeIdx)
    //if (!peek(c.twiddleCount).toList.sameElements(peek(c.TwiddleSetup.o.twiddleCounts).toList)) Error("twiddlecnt")
    //if (!peek(c.twiddleSubCountMax).toList.sameElements(peek(c.TwiddleSetup.o.twiddleSubCounts).toList)) Error("twiddlesubcount")
    peek(c.TwiddleSetup.o.twiddleCounts)

    //if (!peek(c.twiddleMul).toList.sameElements(peek(c.TwiddleSetup.o.twiddleMuls).toList)) Error("twiddlerenorm")
    traceOn = temp
  }
  def inSetupDebug(): Unit = {
    val temp = traceOn
    traceOn = true
    traceOn = temp
  }

}

object Tracker {

  // Variables to track tester progress
  var firstSymbol = false
  var outValid = false
  var frameNum = 0
  var inStep = 0
  var outStep = 0
  var FFTN = 0
  var testedFFTs = List[Int]()

  // Store output data
  var FFTOut = List[ScalaComplex]()

  // Reset variables on new test
  def reset(n: Int) : Unit = {

    val idx = Params.getFFT.sizes.indexOf(n)
    val dblTol = TestVectors.outAbsMin(idx)
    // Set tolerance for comparing expected values
    // val fixedTol = (DSPFixed.toFixed(dblTol,Complex.getFrac).bitLength-2).max(1)

    // TODO: Don't use Complex.getFrac!

    val fixedTol = Complex.getFrac/2
    // val floTol = (0.00000001).max(dblTol/n/100000)
    val floTol = 0.0000000001
    DSPTester.setTol(floTol = floTol, fixedTol = fixedTol)

    firstSymbol = false
    outValid = false
    frameNum = 0
    inStep = 0
    outStep = 0
    FFTN = n
    // FFTOut = List[ScalaComplex]()

  }
}