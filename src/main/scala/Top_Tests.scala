// TODO: Support IFFT (normalization too), check signal limit (in double test), build in CP/GI halt

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
  def testFFTNio(fftIndex:Int, fftTF:Boolean, in:List[ScalaComplex], out:Option[List[ScalaComplex]]){
    // Safety initialize control signals to false
    reset(Params.getIO.clkRatio)
    poke(c.ctrl.ENABLE,true)
    poke(c.ctrl.START_FIRST_FRAME,false)
    poke(c.setup.SETUP_INIT,false)
    newSetup(fftIndex,fftTF)
    step(Params.getIO.clkRatio + 1)
    // After setup, start sending data to process (start on the second IO clock period after SETUP_DONE)
    poke(c.ctrl.START_FIRST_FRAME,true)
    stepTrack(Params.getIO.clkRatio,in,out)
    poke(c.ctrl.START_FIRST_FRAME,false)
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
        if (out != None) {
          val outExpected = out.get(Tracker.outStep)
          // TODO: Support IFFT(!)
          // Normalize FFT appropriately
          val outExpectedNormalized = {
            if(normalized) outExpected**(1/math.sqrt(Tracker.FFTN),typ = Real)
            else outExpected
          }
          expect(c.io.DATA_OUT, outExpectedNormalized, Tracker.FFTN.toString, errorString)
        }
        // Doesn't compare, just stores results for post-processing
        else if (i == 0) {
          Tracker.FFTOut = Tracker.FFTOut :+ peek(c.io.DATA_OUT)
        }
        if(genOffset) expect(c.ctrl.OFFSET,Tracker.outStep%Tracker.FFTN, "Offset value unexpected")
      }
      else{
        if(genOffset) expect(c.ctrl.OFFSET,0, "Offset value should be 0 if output isn't valid initially")
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
    /*val calcDoneNew = peek(c.calcDoneFlag)
    if (!calcDone && calcDoneNew) Status("Calc finished @ t = " + t)
    if (calcDone && !calcDoneNew) Status("Calc started @ t = " + t)
    calcDone = calcDoneNew*/
    val temp = traceOn
    traceOn = true


    //if (!peek(c.ions).toList.sameElements(peek(c.IOCtrl.nIO).toList) & Tracker.inStep > 3) Error("nio")





    //peek(c.io.DATA_OUT.real)
    /*if (Tracker.FFTN == 12){
      peek(c.ctrl.FRAME_FIRST_OUT)
      peek(c.ctrl.OFFSET)
      peek(c.offsetCounter.iCtrl.change.get)
      peek(c.offsetCounter.iCtrl.reset)
      peek(c.offsetCountEnable)
      peek(c.frameFirstOutPreTemp)
    }
    peek(c.ctrl.FRAME_FIRST_OUT)
    peek(c.ctrl.OFFSET)
    peek(c.offsetCounter.iCtrl.reset)
    peek(c.offsetCounter.iCtrl.change.get)
    peek(c.offsetCounter.nextCount)*/
    /*if(i == 0) {
      peek(c.memBanks.io.ioAddr)
      peek(c.memBanks.io.ioBank)
    }*/
    /*peek(c.memBanks.io.calcAddr)
    peek(c.memBanks.io.calcBank)*/

    if (Tracker.FFTN == 24) {
      //c.iDIFtemp.foreach{x => peek(x)}
      //c.IOCtrl.coprimeCounts.foreach{x => peek(x)}
      //peek(c.IOCtrl.digitIdxDIF)
      //peek(c.IOCtrl.digitIdxDIT)
    }
    /*if (Tracker.FFTN == 72) {
    //c.IOCtrl.ioIncCounters.foreach{x => peek(x.ctrl.reset)}
    //c.IOCtrl.ioIncCounters.foreach{x => peek(x.ctrl.en.get)}
    (0 until 3).foreach{ x => {
      if (!peek(c.IOCtrl.ioIncCounts(x)).toList.sameElements(peek(c.ioIncCounts1(x)).toList) & Tracker.inStep != 0) Error("iocount")
    }}
    //if (peek(c.IOCtrl.ioIncCounts).toList.flatten != peek(c.ioIncCounts1).toList.flatten) Error("iocount")
    }*/

    //if (!peek(c.ions).toList.sameElements(peek(c.IOCtrl.nIO).toList) & Tracker.inStep >= 2) Error("nio")


  if (Tracker.FFTN == 12) {
    //peek(c.IOCtrl.isUsed)
    //peek(c.IOCtrl.ioIncChange)
    //peek(c.IOCtrl.test)
    //c.IOCtrl.ioIncCounters.foreach{x => peek(x.ctrl.isMax)}


    /*if (!peek(c.IOCtrl.ioIncCounts(0)).toList.sameElements(peek(c.oIncCounts(2)).toList)) Error("ocount0")
    if (!peek(c.IOCtrl.ioIncCounts(1)).toList.sameElements(peek(c.oIncCounts(1)).toList)) Error("ocount1")
    if (!peek(c.IOCtrl.ioIncCounts(2)).toList.sameElements(peek(c.oIncCounts(0)).toList)) Error("ocount2")*/
    //peek(c.IOCtrl.ioIncCounts(1))
    //peek(c.IOCtrl.ioIncCounts(2))

    //peek(c.oIncCounts(1))
    //peek(c.oIncCounts(0))
  }


    traceOn = temp





  /*(0 until 3).foreach { x => {
    if (!peek(c.IOCtrl.ioIncCounts(x)).toList.sameElements(peek(c.oIncCounts(2-x)).toList) & Tracker.inStep != 0) Error("iocount")
  }}

    (0 until 2).foreach{ x => {
      if (!peek(c.IOCtrl.ioQCounts(2-x)).toList.sameElements(peek(c.oModCounts(x)).toList) & Tracker.inStep != 0) Error("ioq")
    }}*/

    /*c.oDIFtemp.zip(c.IOCtrl.coprimeCounts).foreach{case (x,y) => {
      if (!peek(x).toList.sameElements(peek(y).toList) & Tracker.inStep >= 2) Error("coprimecounts")
    }}*/



    //if (Tracker.FFTN == 24) {
      //c.IOCtrl.ioIncCounters.foreach{x => peek(x.ctrl.reset)}
      //c.IOCtrl.ioIncCounters.foreach{x => peek(x.ctrl.en.get)}
/*
    (0 until 3).foreach{ x => {
        if (!peek(c.IOCtrl.ioIncCounts(x)).toList.sameElements(peek(c.ioIncCounts1(x)).toList) & Tracker.inStep != 0) Error("iocount")
      }}
    (0 until 2).foreach{ x => {
      if (!peek(c.IOCtrl.ioQCounts(x)).toList.sameElements(peek(c.ioModCounts1(x)).toList) & Tracker.inStep != 0) Error("ioq")
    }}


    c.iDIFtemp.zip(c.IOCtrl.coprimeCounts).foreach{case (x,y) => {
      if (!peek(x).toList.sameElements(peek(y).toList) & Tracker.inStep >= 2) Error("coprimecounts")
    }}*/

   /* c.ions.zip(c.IOCtrl.nIOX).foreach{case (x,y) => {
      if (!peek(x).toList.sameElements(peek(y).toList) & Tracker.inStep >= 2) Error("coprimecounts")
    }}*/

   if (!peek(c.ions).toList.sameElements(peek(c.IOCtrl.nIO).toList) & Tracker.inStep >= 2) Error("nio")



      //if (peek(c.IOCtrl.ioIncCounts).toList.flatten != peek(c.ioIncCounts1).toList.flatten) Error("iocount")
    //}


  }
  def setupDebug(): Unit = {
    val temp = traceOn
    traceOn = true
    //peek(c.IOCtrl.primeIdx)
    //peek(c.IOCtrl.ioIncCounts)
    //peek(c.ioIncCounts1)
    //peek(c.IOCtrl.primeDigits)
    //peek(c.IOCtrl.counterPrimeDigits)
    //peek(c.IOCtrl.usedLoc)
    //peek(c.IOCtrl.isUsed)
    //peek(c.IOCtrl.ioIncChange)
    //peek(c.IOCtrl.counterQDIFs)
    //if (!peek(c.IOCtrl.primeIdx).toList.sameElements(peek(c.primeIdx).toList)) Error("primidx")
    //if (!peek(c.IOCtrl.qDIF).toList.sameElements(peek(c.qDIFis).toList)) Error("qdif")
    //if (!peek(c.IOCtrl.qDIT).toList.sameElements(peek(c.qDIFos).toList)) Error ("qdit")
    //peek(c.GeneralSetup.stageRad)
    //peek(c.GeneralSetup.use2)
    //peek(c.GeneralSetup.maxRad)
    //peek(c.GeneralSetup.io.stageRadIdx)
    //peek(c.GeneralSetup.io.primeStageSum)
    //peek(c.IOCtrl.stagePrimeIdx)
    //peek(c.IOCtrl.nIO)
    //peek(c.GeneralSetup.io.stageRad)
    //peek(c.GeneralSetup.io.use2)
    //peek(c.GeneralSetup.io.maxRad)
    //peek(c.IOCtrl.test)
    //c.IOCtrl.counterQDITs.foreach{x => peek(x)}
    //c.qDIFos.foreach{x => peek(x)}
    //peek(c.GeneralSetup.prevPrimeStageSumShort)
    //peek(c.IOCtrl.generalSetupIO.prevPrimeStageSum)
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