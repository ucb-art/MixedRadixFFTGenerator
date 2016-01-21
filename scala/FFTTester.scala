// TODO: Support IFFT, check signal limit (in double test)

package FFT
import ChiselDSP._
import Chisel.{Complex => _, _}

/** Module tester that allows switching between fixed and floating point testing */
class FFTTests[T <: FFT[_ <: DSPQnm[_]]](c: T) extends DSPTester(c) {

  traceOn = false
  // Set tolerance for comparing expected values
  DSPTester.setTol(floTol = 0.00000001,fixedTol = (Complex.getFrac/3).toInt)
  runAll()
  //runTo(60)
  //run(243)
  //List(2,5).foreach{run(_)}
  ////////////////////////////////////////////////////////////////////////

  /** Run all tests for all FFTNs */
  def runAll() : Unit = runTo(Params.getFFT.sizes.last)

  /** Run for all FFT sizes until (and including) N */
  def runTo(n: Int): Unit = {
    reset()
    val sizes = Params.getFFT.sizes
    val idx = sizes.indexOf(n)
    for (i <- 0 to idx) run(sizes(i))
  }

  /** Run tests for desired FFTN */
  def run(n: Int) : Unit = {
    Tracker.reset(n)
    if (traceOn) Status("FFTN = " + n)
    stepThrough(n)
  }

  /** Step through test vectors for FFTN */
  def stepThrough(n: Int): Unit = {
    val idxN = Params.getFFT.sizes.indexOf(n)
    if (idxN < 0) Error("FFTN is not included in this generated output")
    val inVec = TestVectors.getIn(idxN)
    val outVec = TestVectors.getOut(idxN)

    //println(inVec)
    //println(outVec)
    // TODO: Clean
    testFFTNio(idxN, fftTF = true, inVec,outVec)
    /*
    butterflyTest(inVec,outVec,calcDIT=true)
    butterflyTest(inVec,outVec,calcDIT=false)
    testMem(inVec)
    */
  }

  /** Test full butterfly (DIT/DIF) */
  /*
  def butterflyTest(inVec: List[ScalaComplex],outVec: List[ScalaComplex], calcDIT: Boolean): Unit = {
    val peIO = c.io
    poke(peIO.calcDIT,calcDIT)
    if (Tracker.FFTN <= Params.getBF.rad.max){
      val radIdx = Params.getBF.rad.indexOf(Tracker.FFTN)
      // # of twiddles is 1 less than radix
      for ( i <- 0 until Tracker.FFTN-1) poke(peIO.twiddles(i),TestVectors.twiddles(i))
      // Don't need a current radix flag if only 1 radix is used
      if (Params.getBF.rad.length > 1) poke((peIO.currRad.get)(radIdx),true)
      // Butterfly input
      for ( i <- 0 until Tracker.FFTN) poke(peIO.x(i),inVec(i))
      // DIT: twiddle multiplication output (into WFTA module)
      val twiddleOutDIT = (0 until Tracker.FFTN).map(i => {
        if (i == 0) inVec(i)
        else inVec(i) * TestVectors.twiddles(i-1)
      })
      val outDIT = TestVectors.populateOut(twiddleOutDIT.toList,Tracker.FFTN)
      // Wait pipeline delay
      step(c.pe.delay)
      for ( i <- 0 until Tracker.FFTN) {
        val expectedOut = {
          // DIT
          if (calcDIT) outDIT(i)
          // DIF
          else if (i == 0) outVec(i)
          else outVec(i) * TestVectors.twiddles(i-1)
        }
        expect(peIO.y(i), expectedOut, test = Tracker.FFTN.toString, error = "FFT out wrong")
      }
      if (Params.getBF.rad.length > 1) poke((peIO.currRad.get)(radIdx),false)
    }
  }
  */

  /** Test memory interface is expected */
  /*
  def testMem(inVec: List[ScalaComplex]): Unit = {

    val memIO = c.memIO
    if (c.mem.conflictHandling) poke(memIO.passThrough.get,true)
    for ( i <- 0 until Tracker.FFTN) {
      poke(memIO.dIn,inVec(i))
      poke(memIO.wAddr,i)
      poke(memIO.rAddr,i)
      poke(memIO.WE,true)
      // Sequential write (takes 1 clock cycle)
      step()
      // Peek inside memory
      peekAt(c.mem.mem,i)
      // Check that read/write conflict resolves itself
      expect(memIO.dOut,inVec(i), test = Tracker.FFTN.toString, error = "Mem Test R/W Conflict Failed")
      val j = (i-1).max(0)
      poke(memIO.rAddr,j)
      // Sequential read (takes 1 clock cycle)
      step()
      expect(memIO.dOut,inVec(j), test = Tracker.FFTN.toString, error = "Mem Test Failed")
    }

  }
  */

  /** Peek butterfly internal signals */
  /*
  def peekWFTA(): Unit = {
    val wfta = c.pe.wfta
    for (i <- 0 until wfta.io.x.length) peek(wfta.io.x(i))
    for (i <- 0 until wfta.x.length) peek(wfta.x(i))
    for (i <- 0 until wfta.n0.length) peek(wfta.n0(i))
    for (i <- 0 until wfta.n1.length) peek(wfta.n1(i))
    for (i <- 0 until wfta.n2.length) peek(wfta.n2(i))
    for (i <- 0 until wfta.n3.length) peek(wfta.n3(i))
    for (i <- 0 until wfta.n4.length) peek(wfta.n4(i))
    for (i <- 0 until wfta.n5.length) peek(wfta.n5(i))
    for (i <- 0 until wfta.n6.length) peek(wfta.n6(i))
    for (i <- 0 until wfta.y.length) peek(wfta.y(i))
    for (i <- 0 until wfta.io.y.length) peek(wfta.io.y(i))
    step()
  }
  */

  // TODO: Uncomment + clear below



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
    expect(c.qDIFi,ioAddressConstants.qDIFiArray(fftIndex))
    expect(c.qDIFo,ioAddressConstants.qDIFoArray(fftIndex))
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
    for (i <- 0 until 4){
      for (i <- 0 until 2*fftSizes.fftSizeArray(fftIndex)){
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


    traceOn = true
    //println("ioIncCOunts")
    //c.ioIncCounts.foreach{peek(_)}
    //c.ioIncCounters.foreach{x => peek(x.io.primeDigits)}

    //println("iDIFCounts")
    //peek(c.iDIFCounts)
    //peek(c.qDIFi)
    //c.qDIFis.foreach{peek(_)}
    //peek(c.primeDigits)
    //peek(c.primeDigitsTemp)
    //println("iomodcounts")
    //c.ioModCounts.foreach{peek(_)}
    //c.ioFinalCounts.foreach{peek(_)}
    //println("idifmodcount")
    //peek(c.iDIFModCounts)
    c.iDIFtemp.foreach{peek(_)}
    peek(c.iDIFn)
    traceOn = false

    //peek(c.memBanks.io.calcBank)
    //peek(c.memBanks.io.calcAddr)
    /*peek(c.iDIFCounts)
    peek(c.iDIFModCounts)
    peek(c.iDIFNewCounts)
    peek(c.dec2xAryDIFi(1))
    peek(c.iDIFn)
    peek(c.memBanks.io.ioBank)
    peek(c.memBanks.io.ioAddr)*/
    //peek(c.memBanks.io.currRad)
    /*peek(c.memBanks.io.currRad)
    peek(c.twiddleLUT(0)(0).addr)*/
    /*peek(c.twiddleLUT(0)(0).dout)
    peek(c.twiddles(0)(0))
    peek(c.twiddleXReal(0))
    peek(c.twiddleXImag(0))
    peek(c.twiddleX(0))*/


    /*peek(c.memBanks.io.Din)
    peek(c.memBanks.io.Dout)*/
    /*peek(c.memBanks.io.x(0))
      peek(c.memBanks.io.x(1))
      peek(c.memBanks.io.x(2))
      peek(c.memBanks.io.x(3))
      peek(c.memBanks.io.x(4))
    peek(c.memBanks.io.y(0))
      peek(c.memBanks.io.y(1))
      peek(c.memBanks.io.y(2))
      peek(c.memBanks.io.y(3))
      peek(c.memBanks.io.y(4))
    peek(c.butterfly.io.twiddles(0))
    peek(c.butterfly.io.twiddles(1))
    peek(c.butterfly.io.twiddles(2))
    peek(c.butterfly.io.twiddles(3))*/

    /*peek(c.memBanks.mems(1)(0).dIn)
    peek(c.memBanks.mems(1)(0).dOut)
    peek(c.memBanks.mems(1)(0).WE)
    peek(c.memBanks.mems(1)(0).rAddr)
    peek(c.memBanks.mems(1)(0).wAddr)
    peek(c.memBanks.mems(1)(0).passThrough.get)
    peek(c.memBanks.mems(1)(1).dIn)
    peek(c.memBanks.mems(1)(1).dOut)
    peek(c.memBanks.mems(1)(1).WE)
    peek(c.memBanks.mems(1)(1).rAddr)
    peek(c.memBanks.mems(1)(1).wAddr)
    peek(c.memBanks.mems(1)(1).passThrough.get)
    peek(c.memBanks.mems(1)(2).dIn)
    peek(c.memBanks.mems(1)(2).dOut)
    peek(c.memBanks.mems(1)(2).WE)
    peek(c.memBanks.mems(1)(2).rAddr)
    peek(c.memBanks.mems(1)(2).wAddr)
    peek(c.memBanks.mems(1)(2).passThrough.get)
    peek(c.memBanks.mems(1)(3).dIn)
    peek(c.memBanks.mems(1)(3).dOut)
    peek(c.memBanks.mems(1)(3).WE)
    peek(c.memBanks.mems(1)(3).rAddr)
    peek(c.memBanks.mems(1)(3).wAddr)
    peek(c.memBanks.mems(1)(3).passThrough.get)*/


    /*peek(c.twiddleXReal(2))
    peek(c.twiddles(0)(0))
    peek(c.twiddles(0)(1))
    peek(c.twiddles(0)(2))

    peek(c.twiddles(1)(0))
    peek(c.twiddles(1)(1))

    peek(c.twiddles(2)(0))
    peek(c.twiddles(2)(1))
    peek(c.twiddles(2)(2))*/



    //c.twiddles.flatten.foreach{peek(_._2)}

    //peek(c.butterfly.io.currRad)

    /*
    peek(c.memBanks.io.x(0))
    peek(c.memBanks.io.y(0))
    peek(c.memBanks.io.Din)
    peek(c.memBanks.io.Dout)*/
    /*peek(c.memBanks.io.calcMemB)
    peek(c.memBanks.io.calcDoneFlag)
    peek(c.memBanks.io.ioWriteFlag)
    peek(c.memBanks.io.discardCalcWrite)*/
    /*peek(c.butterfly.io.twiddles(0))
    peek(c.butterfly.io.x(0))
    peek(c.butterfly.io.y(0))
    if (peek(c.twiddleLUT(0)(0).addr) != 0) Error("yay")
    peek(c.twiddleLUT(0)(0).dout)*/




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
