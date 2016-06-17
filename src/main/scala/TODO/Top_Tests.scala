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
    // TODO: Support IFFT
    testFFTNio(idxN,fftTF = true,inVec,outVec)
  }

  /** Setup new FFTN (Runtime configuration) where fftTF true -> FFT else IFFT */
  def newSetup(fftIndex:Int, fftTF:Boolean){

    Status("///////////////////////////////////////// NEW SETUP @ t = " + t)
    Tracker.initT = t

    poke(c.setup.enable,true)
    poke(c.setup.fftIdx,fftIndex)
    poke(c.setup.isFFT,fftTF)

    step(Params.getIO.clkRatio)

    // Output should not be valid after setup mode detected
    expect(c.ctrl.outValid,false)
    expect(c.setup.done,false)

    // Invalidate setup parameters to make sure engine doesn't pick up the wrong ones
    poke(c.setup.enable,false)
    poke(c.setup.fftIdx,(fftIndex+1)%Params.getFFT.nCount)
    poke(c.setup.isFFT,!fftTF)

    // Wait until done setting up
    var setupDone = peek(c.setup.done)
    var whileCnt = 0
    while (!setupDone){
      whileCnt = whileCnt + 1
      if (whileCnt > 100) Error("Setup is not completing...")
      step(1)
      setupDone = peek(c.setup.done)

      // Output should not be valid after setup mode detected
      expect(c.ctrl.outValid,false)

    }

    Status("///////////////////////////////////////// N = " + Tracker.FFTN + " SETUP DONE @ t = " + t)
    setupDebug()

    val modT = (t-Tracker.initT)%Params.getIO.clkRatio
    if (modT != 0) Error("Setup done should occur @ right time corresponding to IO clk rate")

    // Make sure setup done held for at least the full IO clock period
    for (i <- 0 until Params.getIO.clkRatio-1) {step(1); expect(c.setup.done,true)}

    // After setup, should be @ first fast clk in IO clk (poke directly @ core time)
    step(1)

    expect(c.setup.done,true)
    expect(c.ctrl.outValid,false)

  }

  /** Feed in FFT inputs and read FFT outputs */
  def testFFTNio(fftIndex:Int, fftTF:Boolean, in:List[ScalaComplex], out:Option[List[ScalaComplex]]){

    // Initialize control signals to false
    reset(Params.getIO.clkRatio)

    poke(c.setup.enable,false)
    poke(c.setup.fftIdx,(fftIndex+1)%Params.getFFT.nCount)
    poke(c.setup.isFFT,!fftTF)

    poke(c.ctrl.enable,false)
    // Means not starting yet
    poke(c.ctrl.reset,false)

    step(Params.getIO.clkRatio)

    newSetup(fftIndex,fftTF)

    // Step a full IO clock cycle for good measure
    step(Params.getIO.clkRatio)

    // After setup, start sending data to process
    // When first data in is valid, reset is high (note stepTrack doesn't step until after pokes)
    poke(c.ctrl.enable,true)
    poke(c.ctrl.reset,true)
    stepTrack(2*Params.getIO.clkRatio,in,out, enable = true)

    // Reset only high 1 IO clock cycle
    poke(c.ctrl.reset,false)

    // Wait until as many outputs checked as there were inputs
    while (Tracker.outStep < in.length){

      val ioCycle = (t-Tracker.initT)/Params.getIO.clkRatio

      if (!Tracker.outPropagated && ioCycle > 4 * Tracker.FFTN)
        Error("Data out never seems valid")

      //val en = if (ioCycle == 3 * Tracker.FFTN + 1) false else true
      val en = true
      stepTrack(Params.getIO.clkRatio,in,out, enable = en)
    }

  }

  /** Peek+poke and then step */
  def stepTrack(cycles:Int, in:List[ScalaComplex], out:Option[List[ScalaComplex]], enable:Boolean): Unit ={

    expect(c.setup.done,true)

    // Enable/disable IO
    poke(c.ctrl.enable,enable)

    // Wraps input data (since it takes extra cycles to get all of corresponding out)
    val inVal = in(Tracker.inStep % in.length)
    poke(c.io.din, inVal)

    // Check @ fast clock cycle rate
    for (i <- 0 until cycles){

      calcDebug()

      // Check if output is valid
      val prevOutValid = Tracker.outValid
      Tracker.outValid = peek(c.ctrl.outValid)

      // Track when output is first valid after reset
      if (Tracker.outValid && !Tracker.outPropagated){
        // First frame out
        Status("///////////////////////////////////////// N = " + Tracker.FFTN + ", FRAME = 0, K = 0 @ t = " + t)
        // Next time reporting happens should be on the following frame
        Tracker.frameNum = Tracker.frameNum + 1
        Tracker.outPropagated = true
      }
      // Only report once per frame
      else if (i == 0 && Tracker.outValid && (Tracker.outStep%Tracker.FFTN == 0)){
        // Frame out
        Status("///////////////////////////////////////// N = " + Tracker.FFTN + ", FRAME = " + Tracker.frameNum
          + ", K = 0 @ t = " + t)
        // Next time reporting happens should be on the following frame
        Tracker.frameNum = Tracker.frameNum + 1
      }

      // Output should transition on the correct fast clock cycle (note that output valid can go low if io enable
      // ever goes low)
      if ((Tracker.outValid && !prevOutValid) || (!Tracker.outValid && prevOutValid)){
        // TODO: Make function?
        val modT = (t-Tracker.initT)%Params.getIO.clkRatio
        val k = Tracker.outStep%Tracker.FFTN
        if (modT != 0) Error("Out valid should occur @ right time corresponding to IO clk rate: K = " + k)
      }

      // Read output if valid & check for error
      if (Tracker.outValid){

        // Note that Tracker.frameNum is always one ahead
        val errorString = " FFTN = " + Tracker.FFTN + ", FRAME = " + (Tracker.frameNum-1) +
          ",  k = " + Tracker.outStep % Tracker.FFTN + " @ t = " + t + "\n "

        // Check that k matches as expected
        if(genOffset)
          expect(c.ctrl.k,Tracker.outStep % Tracker.FFTN, "Offset value unexpected" + errorString)

        // Compare with expected output
        if (out != None) {
          val outExpected = out.get(Tracker.outStep)
          // TODO: Support IFFT(!)
          // Normalize FFT appropriately
          val outExpectedNormalized = {
            if (normalized) outExpected**(1/math.sqrt(Tracker.FFTN),typ = Real)
            else outExpected
          }
          expect(c.io.dout, outExpectedNormalized, Tracker.FFTN.toString, errorString)
        }
        // Doesn't compare, just stores results for post-processing
        else{
          val outVal = peek(c.io.dout)
          if (i == 0) Tracker.FFTOut = Tracker.FFTOut :+ outVal
          else {
            if (outVal != Tracker.FFTOut.last) Error("Dout inconsistent across fast clk cycles within 1 IO cycle")
          }
        }

      }

      step(1)
    }

    // Steps by IO clock rate (output only steps if output is known to be valid)
    if (enable) Tracker.inStep =  Tracker.inStep + 1
    if (Tracker.outValid) Tracker.outStep = Tracker.outStep + 1

  }

  /** Placeholder for debugging signals */
  def calcDebug(): Unit = {
    val temp = traceOn
    traceOn = true
    // if (!a.toList.sameElements(b.toList)) Error(":(")
    traceOn = temp
  }

  def setupDebug(): Unit = {
    val temp = traceOn
    traceOn = true
    // if (!a.toList.sameElements(b.toList)) Error(":(")
    traceOn = temp
  }

}

object Tracker {

  // Variables to track tester progress
  // Output currently valid
  var outValid = false
  // Current frame + 1
  var frameNum = 0
  var inStep = 0
  var outStep = 0
  var FFTN = 0
  // FFTs tested up until now
  var testedFFTs = List[Int]()
  // Time when each FFTN starts
  var initT = 0
  // After reset, valid has gone high at least once (for each FFTN)
  var outPropagated = false

  // Store output data
  var FFTOut = List[ScalaComplex]()

  // Reset variables on new test
  def reset(n: Int) : Unit = {

    val idx = Params.getFFT.sizes.indexOf(n)
    val dblTol = TestVectors.outAbsMin(idx)
    // Set tolerance for comparing expected values
    // val fixedTol = (DSPFixed.toFixed(dblTol,Complex.getFrac).bitLength-2).max(1)

    // TODO: Don't use Complex.getFrac!
    val fixedTol = Params.getComplex.fracBits/3
    // val floTol = (0.00000001).max(dblTol/n/100000)
    val floTol = 0.0000000001
    DSPTester.setTol(floTol = floTol, fixedTol = fixedTol)

    outPropagated = false
    outValid = false
    frameNum = 0
    inStep = 0
    outStep = 0
    FFTN = n
    // FFTOut = List[ScalaComplex]()

  }
}