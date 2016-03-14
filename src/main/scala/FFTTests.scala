// TODO: Support IFFT, check signal limit (in double test), build in CP/GI halt

package FFT
import ChiselDSP._
import Chisel.{Complex => _, _}

class FFTTests[T <: FFT[_ <: DSPQnm[_]]](c: T) extends DSPTester(c) {

  traceOn = false
  runAll()
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
    val inVec = TestVectors.getIn(idxN)
    val outVec = TestVectors.getOut(idxN)
    testFFTNio(idxN,fftTF = true,inVec,outVec)
  }

  /** Setup new FFTN (Runtime configuration) where fftTF true -> FFT else IFFT */
  def newSetup(fftIndex:Int, fftTF:Boolean){
    Status("///////////////////////////////////////// NEW SETUP")
    val initT = t
    poke(c.setup.SETUP_INIT,true)
    poke(c.setup.FFT_INDEX,fftIndex)
    poke(c.setup.FFT,fftTF)
    step(Params.getIO.clkRatio)
    poke(c.setup.SETUP_INIT,false)
    poke(c.setup.FFT_INDEX,0)
    // Wait until done setting up before checking setup constants (note that step size is not always 1 -- depends on
    // user parameters)
    var setupDone = peek(c.setup.SETUP_DONE)
    var whileCnt = 0
    while (!setupDone){
      whileCnt = whileCnt + 1
      if (whileCnt > 100) Error("Setup is not completing...")
      step(1)
      setupDone = peek(c.setup.SETUP_DONE)
    }
    setupDebug()
    val modT = (t-initT)%Params.getIO.clkRatio
    if (modT != 0) Error("Setup done and other control signals should occur as expected from IO clock rate")
    // SETUP_DONE should be held for clkRatio calc clk cycles
    for (i <- 0 until Params.getIO.clkRatio-1) {step(1);expect(c.setup.SETUP_DONE,true)}
    Status("///////////////////////////////////////// SETUP DONE")
  }

  /** Feed in FFT inputs and read FFT outputs */
  def testFFTNio(fftIndex:Int, fftTF:Boolean, in:List[ScalaComplex], out:List[ScalaComplex]){
    // Safety initialize control signals to false
    poke(c.ctrl.ENABLE,true)
    poke(c.ctrl.START_FIRST_FRAME,false)
    poke(c.setup.SETUP_INIT,false)
    newSetup(fftIndex,fftTF)
    step(Params.getIO.clkRatio + 1)
    // After setup, start sending data to process (start on the second IO clock period after SETUP_DONE)
    poke(c.ctrl.START_FIRST_FRAME,true)
    stepTrack(Params.getIO.clkRatio,in,out)
    poke(c.ctrl.START_FIRST_FRAME,false)
    // Output k = 0 starts 2 frames after n = 0
    for (i <- 0 until Params.getTest.frames + 2; j <- 0 until Params.getFFT.sizes(fftIndex)){
      stepTrack(Params.getIO.clkRatio,in,out)
    }
    if (!Tracker.outValid) Error("Output valid was never detected...")
  }

  /** Peek and then step, where num should be = clkRatio */
  def stepTrack(num:Int, in:List[ScalaComplex], out:List[ScalaComplex]){
    var firstOutValid = false
    for (i <- 0 until num) {
      calcDebug()
      val inVal = in(Tracker.inStep % in.length)
      // Checks when k = 0 is output (Detects transition to first symbol) & dumps input
      if (i == 0) {
        poke(c.io.DATA_IN, inVal)
        val firstOut = peek(c.ctrl.FRAME_FIRST_OUT)
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
        if (firstOutValid) expect(c.ctrl.FRAME_FIRST_OUT,true)
        else expect(c.ctrl.FRAME_FIRST_OUT,false)
      }
      // Read output if valid & check for error
      if (Tracker.outValid){
        val errorString = " FFTN = " + Tracker.FFTN + ", FRAME = " + (Tracker.frameNum-1) +
                          ",  k = " + Tracker.outStep%Tracker.FFTN + "\n "
        val outExpected = out(Tracker.outStep)
        expect(c.io.DATA_OUT,outExpected,Tracker.FFTN.toString,errorString)
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
  def calcDebug(): Unit = {}
  def setupDebug(): Unit = {}

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

  // Reset variables on new test
  def reset(n: Int) : Unit = {

    val idx = Params.getFFT.sizes.indexOf(n)
    val dblTol = TestVectors.outAbsMin(idx)
    // Set tolerance for comparing expected values
    // val fixedTol = (DSPFixed.toFixed(dblTol,Complex.getFrac).bitLength-2).max(1)
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

  }
}