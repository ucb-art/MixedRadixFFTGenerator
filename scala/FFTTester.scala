// TODO: Support IFFT, check signal limit (in double test)

package FFT
import ChiselDSP._
import Chisel.{Complex => _, _}

class FFTTests[T <: FFT[_ <: DSPQnm[_]]](c: T) extends DSPTester(c) {

  // Don't display peeks, pokes, etc.
  traceOn = false
  // Set tolerance for comparing expected values
  DSPTester.setTol(floTol = 0.00000001,fixedTol = (Complex.getFrac/3).toInt)
  runAll()

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
    poke(c.io.SETUP_INIT,true)
    poke(c.io.FFT_INDEX,fftIndex)
    poke(c.io.FFT,fftTF)
    step(Params.getIO.clkRatio)
    poke(c.io.SETUP_INIT,false)
    poke(c.io.FFT_INDEX,0)
    // Wait until done setting up before checking setup constants (note that step size is not always 1 -- depends on
    // user parameters)
    var setupDone = peek(c.io.SETUP_DONE)
    var whileCnt = 0
    while (!setupDone){
      whileCnt = whileCnt + 1
      if (whileCnt > 100) Error("Setup is not completing...")
      step(Params.getIO.clkRatio)
      setupDone = peek(c.io.SETUP_DONE)
    }
    val modT = (t-initT)%Params.getIO.clkRatio
    if (modT != 0) Error("Setup done and other control signals should occur as expected from IO clock rate")
    // SETUP_DONE should be held for clkRatio calc clk cycles
    for (i <- 0 until Params.getIO.clkRatio-1) {step(1);expect(c.io.SETUP_DONE,true)}
    Status("///////////////////////////////////////// SETUP DONE")
  }

  /** Feed in FFT inputs and read FFT outputs */
  def testFFTNio(fftIndex:Int, fftTF:Boolean, in:List[ScalaComplex], out:List[ScalaComplex]){
    newSetup(fftIndex,fftTF)
    step(Params.getIO.clkRatio)
    // After setup, start sending data to process
    poke(c.io.START_FIRST_FRAME,true)
    stepTrack(Params.getIO.clkRatio,in,out)
    poke(c.io.START_FIRST_FRAME,false)
    for (i <- 0 until Params.getTest.frames; j <- 0 until Params.getFFT.sizes(fftIndex)){
      stepTrack(Params.getIO.clkRatio,in,out)
    }
    if (!Tracker.outValid) Error("Output valid was never detected...")
  }

  /** Peek and then step */
  def stepTrack(num:Int, in:List[ScalaComplex], out:List[ScalaComplex]){
    for (i <- 0 until num) {
      debug()
      val inVal = in(Tracker.inStep)
      poke(c.io.DATA_IN, inVal)
      // Checks when k = 0 is output
      val firstSymbol = peek(c.io.FIRST_OUT)
      // Detects transition to first symbol
      if (firstSymbol && !Tracker.firstSymbol) {
        if (num != 0)
          Error("First out and other control signals should occur as expected from IO clock rate")
        Status("///////////////////////////////////////// SYMBOL = %d, K = 0".format(Tracker.symbolNum))
        // Streaming output valid after start of first output symbol detected
        Tracker.outValid = true
        Tracker.symbolNum = Tracker.symbolNum + 1
      }
      Tracker.firstSymbol = firstSymbol
      step(1)
    }
    // Steps by IO clock rate
    Tracker.inStep =  Tracker.inStep + 1










// make sure first out continuously high, overflow?

   /* if (Tracker.outValid){
      val stepOutC:Int = (Tracker.outStep)
      val indexOut = math.floor(stepOutC/2).toInt
      val outExpected = out(indexOut)
      Tracker.outStep = Tracker.outStep + 1
      val errorString = "\n  FFTN = " + Tracker.FFTN + "\n  k = " + indexOut%Tracker.FFTN

      //val errorString = "Does not match " + outExpected.r + "  +  " + outExpected.i + " i \n  FFTN = " + FFTN + "\n  k = " + indexOut%FFTN
      expect(c.io.DATA_OUT,outExpected,Tracker.FFTN.toString,"")


    }*/









  }

  /** Placeholder for debugging signals */
  def debug(): Unit = {

  }

}

object Tracker {

  // Variables to track tester progress
  var firstSymbol = false
  var outValid = false
  var symbolNum = 0
  var inStep = 0
  var outStep = 0
  var FFTN = 0

  // Reset variables on new test
  def reset(n: Int) : Unit = {
    firstSymbol = false
    outValid = false
    symbolNum = 0
    inStep = 0
    outStep = 0
    FFTN = n
  }

}