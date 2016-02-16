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
    Status("FFTN = " + n)
    stepThrough(n)
  }








  /** Step through test vectors for FFTN */
  def stepThrough(n: Int): Unit = {

    val idxN = Params.getFFT.sizes.indexOf(n)
    if (idxN < 0) Error("FFTN is not included in this generated output")
    val inVec = TestVectors.getIn(idxN)
    val outVec = TestVectors.getOut(idxN)
    testFFTNio(idxN, fftTF = true, inVec,outVec)

  }











  def newSetup(fftIndex:Int, fftTF:Boolean){
    println("///////////////////////////////////////// NEW SETUP")
    poke(c.io.SETUP_INIT,true)
    poke(c.io.FFT_INDEX,fftIndex)
    poke(c.io.FFT,fftTF)
    step(1)
    poke(c.io.SETUP_INIT,true)
    poke(c.io.FFT_INDEX,fftIndex)
    poke(c.io.FFT,fftTF)
    step(1)
    poke(c.io.SETUP_INIT,false)
    var doneFlag = peek(c.io.SETUP_DONE)
    while (!doneFlag){
      step(1)
      doneFlag = peek(c.io.SETUP_DONE)
    }
    // Wait until done setting up
    readSetupConstants(fftIndex,1)
  }

  def readSetupConstants(fftIndex:Int, exitTF:Int){
    expect(c.fftIndex,fftIndex)
    expect(c.fftTF,true)
    //expect(c.qDIFi,ioAddressConstants.qDIFiArray(fftIndex))
    //expect(c.qDIFo,ioAddressConstants.qDIFoArray(fftIndex))
    expect(c.numPower,generalConstants.numPowerArray(fftIndex))
    expect(c.coprimes,generalConstants.coprimesArray(fftIndex))
    expect(c.coprimesFlipped,generalConstants.coprimesArray(fftIndex).reverse)
    expect(c.twiddleCount,twiddleConstants.twiddleCountMaxArray(fftIndex))
    peek(c.stageSum)
    peek(c.maxRadix)
    peek(c.twiddleSubCountMax)
    peek(c.radNTwiddleMul)
    peek(c.stageRadix)
    peek(c.maxStageCount)
    peek(c.addressConstant)
    peek (c.twiddleMul)
    peek(c.io.SETUP_DONE)
    println("\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ SETUP DONE")
  }



  def testFFTNio(fftIndex:Int, fftTF:Boolean, in1:List[ScalaComplex],out1:List[ScalaComplex]){
    println("\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ TEST FFT N" + Tracker.FFTN)
    newSetup(fftIndex,fftTF)


    step(1)
    poke(c.io.START_FIRST_FRAME,true)
    stepP(1, in1, out1)
    poke(c.io.START_FIRST_FRAME,true)
    stepP(1, in1, out1)													// io Count starts here
    poke(c.io.START_FIRST_FRAME,false)
    // Continue checking for a few counts after full 2x FFT size (slow clk takes twice as long as fast clock) -- both phases
    for (i <- 0 until 5){
      for (i <- 0 until 2*Params.getFFT.sizes(fftIndex)){
        stepP(1, in1, out1)
      }
    }
    for (i <- 0 until 7){										// Back to A
      stepP(1, in1, out1)
    }

  }









  // Always peek signals before stepping
    def stepP(num:Int, in1:List[ScalaComplex],out1:List[ScalaComplex]){

      // Every other time function is executed, change input data (IO "clock" slow)
      val stepInC:Int = (Tracker.inStep)
      val indexIn:Int = math.floor(stepInC/2).toInt
    val in = in1(indexIn)
    poke(c.io.DATA_IN, in)

    //** Starts counting cycle after START_FIRST_FRAME asserted


















    val start_symbol = peek(c.io.FIRST_OUT)					    // k,n = 0 IO

    if (start_symbol && !Tracker.startSymbol){
      println("/////////////////////////// k = 0 Symbol Start ///////////////////////////")
      Tracker.outValid = true							// Output valid
    }




    Tracker.startSymbol = start_symbol  // Save old value





    if (Tracker.outValid){
      val stepOutC:Int = (Tracker.outStep)
      val indexOut = math.floor(stepOutC/2).toInt
      val outExpected = out1(indexOut)
      Tracker.outStep = Tracker.outStep + 1
      val errorString = "\n  FFTN = " + Tracker.FFTN + "\n  k = " + indexOut%Tracker.FFTN

      //val errorString = "Does not match " + outExpected.r + "  +  " + outExpected.i + " i \n  FFTN = " + FFTN + "\n  k = " + indexOut%FFTN
      expect(c.io.DATA_OUT,outExpected,Tracker.FFTN.toString,"")


    }

    Tracker.inStep =  Tracker.inStep + 1

    step(num)

  }







}

object Tracker {

  // Variables to track tester progress
  var startSymbol = false
  var outValid = false
  var inStep = 0
  var outStep = 0
  var FFTN = 0

  // Reset variables on new test
  def reset(n: Int) : Unit = {
    startSymbol = false
    outValid = false
    inStep = 0
    outStep = 0
    FFTN = n
  }
}