package FFT
import ChiselDSP._
import Chisel.{Complex => _, _}

class RocketToFFTWrapperTests(c: RocketToFFTWrapper, fftn: Int = -1, frames: Int = 1) extends DSPTester(c) {

  //MemInit.create()

  // TODO: Support IFFT

  val calcOption = "debugUntil1FrameOut"

  traceOn = false

  val sizes = if (fftn == -1) Params.getFFT.sizes else List(fftn) //.slice(1,4)
  val (cins,couts,cinsshort) = test(sizes)

///////////////////////////////////////////// C HEADER

  val numTests = sizes.length
  val testMemSize = c.rocketToFFT.addrMax + 1
  val testBase = "0x48000000"
  val memLocs = c.rocketToFFT.memMap.map(x => x._1 -> x._2.base).toMap
  val numFrames = List.fill(numTests)(Params.getTest.frames)
  val fftIdxs = sizes.map(n => Params.getFFT.sizes.indexOf(n))
  val isFFTs = List.fill(numTests)(1)

  val ch = new java.io.BufferedWriter(new java.io.FileWriter("build/debug/RocketToFFTTester.h"))

  ch write "int setup(unsigned long currFFTIdx, unsigned long currIsFFT); \n"
  ch write "int load(int testNum, int idxStart, int currN); \n"
  ch write "int calculate(unsigned long option); \n"
  ch write "int checkOut(int testNum, int idxStart, int currN); \n\n"
  ch write "#define CALC_OPTION %dUL \n".format(c.calcOptions(calcOption))
  ch write "#define START 0UL \n"
  ch write "#define DONE 1UL \n\n"
  ch write "#define TEST_MEM_SIZE %d \n".format(testMemSize)
  ch write "#define TEST_BASE %sUL \n".format(testBase)
  memLocs.foreach{ x =>
    ch write "#define %s_OFFSET %d \n".format(x._1,x._2)
  }
  ch write "\n"
  ch write "#define NUM_TESTS %d \n".format(numTests)
  ch write "int fftn[NUM_TESTS] = {%s}; \n".format(sizes.mkString(","))
  ch write "int frames[NUM_TESTS] = {%s}; \n".format(numFrames.mkString(","))
  ch write "unsigned long fftIdx[NUM_TESTS] = {%s}; \n".format(fftIdxs.mkString(","))
  ch write "unsigned long isFFT[NUM_TESTS] = {%s}; \n\n".format(isFFTs.mkString(","))
  val inames = cins.zipWithIndex.map{ x => {
    val n = sizes(x._2)
    // WARNING: CURRENTLY INVALID!!! (24 bits instead of 32)
    val length = x._1.length
    ch write "unsigned long i%d[%d] = {%s}; \n".format(n,length,x._1.mkString(","))
    "i" + n
  }}

  cinsshort.zipWithIndex.foreach{ x => {
    val n = sizes(x._2)
    if (n == 324) {
      val insBottomTemp = if (x._1.length > 1024) x._1.dropRight(1024) else x._1
      val insBottom = insBottomTemp ++ List.fill(1024 - insBottomTemp.length)(BigInt(0))
      val insTopTemp = x._1.drop(1024)
      val insTop = insTopTemp ++ List.fill(1024 - insTopTemp.length)(BigInt(0))
      val insBottomLSB = insBottom.map(num => num & ((1 << 24) - 1)).map(num => num.toString(16)).mkString(" \n")
      val insBottomMSB = insBottom.map(num => (num >> 24) & ((1 << 24) - 1)).map(num => num.toString(16)).mkString(" \n")
      val insTopLSB = insTop.map(num => num & ((1 << 24) - 1)).map(num => num.toString(16)).mkString(" \n")
      val insTopMSB = insTop.map(num => (num >> 24) & ((1 << 24) - 1)).map(num => num.toString(16)).mkString(" \n")
      scala.tools.nsc.io.File("bottom_LSB.txt").writeAll(insBottomLSB)
      scala.tools.nsc.io.File("bottom_MSB.txt").writeAll(insBottomMSB)
      scala.tools.nsc.io.File("top_LSB.txt").writeAll(insTopLSB)
      scala.tools.nsc.io.File("top_MSB.txt").writeAll(insTopMSB)
    }
  }}

  ch write "\n"
  val onames = couts.zipWithIndex.map{ x => {
    val n = sizes(x._2)
    val length = x._1.length
    ch write "unsigned long o%d[%d] = {%s}; \n".format(n,length,x._1.mkString(","))
    "o" + n
  }}
  ch write "\n"
  ch write "unsigned long *inptr[NUM_TESTS] = {%s}; \n".format(inames.mkString(","))
  ch write "unsigned long *outptr[NUM_TESTS] = {%s}; \n".format(onames.mkString(","))

  ch.close()

///////////////////////////////////////////// MACRO FUNCTIONS

  def test(sizes:List[Int]): Tuple3[List[List[BigInt]],List[List[BigInt]],List[List[BigInt]]] = {

    TestVectors(sizes, frames)

    val fracWidth = Params.getComplex.fracBits
    val bigIntsIn = sizes.map{n => {
      val idx = Params.getFFT.sizes.indexOf(n)
      val in = if (sizes.length == 1) TestVectors.getIn(0) else TestVectors.getIn(idx)
      in.map(x => x.toBigInt(fracWidth,32))
    }}
    val bigIntsInShort = sizes.map{n => {
      val idx = Params.getFFT.sizes.indexOf(n)
      val in = if (sizes.length == 1) TestVectors.getIn(0) else TestVectors.getIn(idx)
      in.map(x => x.toBigInt(fracWidth,24))
    }}
    val bigIntsOut = sizes.map { n => {
      Tracker.reset(n)
      setup(n = n, isFFT = true)
      val idx = Params.getFFT.sizes.indexOf(n)
      val inT = if (sizes.length == 1) TestVectors.getIn(0) else TestVectors.getIn(idx)
      val outT = if (sizes.length == 1) TestVectors.getOut(0) else TestVectors.getOut(idx)
      val in = inT.grouped(n).toList
      val out = outT.grouped(n).toList

      in.zip(out).zipWithIndex.map{ x => {
        Status("///////////////////////////////////////////// FFT N = " + n)
        Status("Loading inputs for N = " + n + ", Frame " + x._2)
        load(x._1._1)
        calculate(c.calcOptions(calcOption))
        //calculate(0)
        if (read(c.memMap("k").base)!= n-1) Error("K not expected -- should be " + (n-1))
        Status("Verifying outputs for N = " + n + ", Frame " + x._2)
        check(x._1._2)
      }}.flatten
    }}
    (bigIntsIn,bigIntsOut,bigIntsInShort)
  }

  def check(x: List[ScalaComplex]): List[BigInt] = {
    val fracWidth = Params.getComplex.fracBits
    val n = x.length
    println(s"Test vector length: $n")
    val fromFFTAddr = c.memMap("fromFFT").base
    val oideal = (0 until x.length).map{ i => 
      // normalized
      x(i)**(1/math.sqrt(Tracker.FFTN),typ = Real)
    }

    // http://www.ti.com/lit/an/spra948/spra948.pdf
    val (o, osqnr) = oideal.zipWithIndex.map{ case (normalized, i) => {
      val outBigInt = read(fromFFTAddr + i)
      val (out,orb,oib) = Complex.toScalaComplex(outBigInt,fracWidth,32)
      // TODO: Add in IFFT
      checkError(normalized,out,orb,oib,"@ [Out] FFT = " + n + ", k = " + i)
      (outBigInt, out)
    }}.unzip

    val sig = oideal.map(x => math.pow(x.abs, 2)).sum
    val noise = oideal.zip(osqnr).map { case (ideal, nonideal) => 
      math.pow((ideal - nonideal).abs, 2) }.sum
    val sqnr = sig/noise
    val sqnrpwr = 10*math.log10(sqnr)
    println(sqnrpwr)
    scala.tools.nsc.io.File("sqnr.csv").appendAll(s"$fftn, $sqnrpwr \n")

    Status("Successfully verified outputs for N = " + n)
    o.toList
  }

  // DEBUG MODES (calcType)
  // OK debugUntil1FrameOut --> Reset and run until k = N-1
  // OK debugPow --> Reset and run until calcType register written to again
  // debugUntil1FrameInStart --> Reset and run until n = N-1 (stop when n -- input counter -- wraps)
  // debugUntil1FrameInContinue --> (No reset), run until n = N-1

  def calculate(option:BigInt): Unit = {
    val calcTypeAddr = c.memMap("calcType").base
    write(calcTypeAddr,option)
    if (read(calcTypeAddr) != option) Error("Calc. option doesn't match " + option)
    val calcAddr = c.memMap("calcStartDone").base
    write(calcAddr,0)
    var calcDone = read(calcAddr)
    //var testCount = 0
    while (calcDone != BigInt(1)){
      //testCount = testCount + 1
      //if (testCount == 1000)
        //write(calcTypeAddr,0) 
      calcDone = read(calcAddr)
    }
    Status("Done calculating!")
  }

  def load(x: List[ScalaComplex]): Unit = {
    val fracWidth = Params.getComplex.fracBits
    val n = x.length
    val toFFTAddr = c.memMap("toFFT").base
    for (i <- 0 until x.length){
      // real is MSB (out of 64)
      val in = x(i).toBigInt(fracWidth,32)
      write(toFFTAddr + i,in)
      val outBigInt = read(toFFTAddr + i)
      val (out,orb,oib) = Complex.toScalaComplex(outBigInt,fracWidth,32)
      checkError(x(i),out,orb,oib,"@ [In] FFT = " + n + ", n = " + i)
    }
    Status("Successfully loaded inputs for N = " + n)
  }

  def setup(n:Int, isFFT:Boolean): Unit = {
    val idx = Params.getFFT.sizes.indexOf(n)
    val fftIdxAddr = c.memMap("fftIdx").base
    val isFFTAddr = c.memMap("isFFT").base
    val setupAddr = c.memMap("setupStartDone").base
    val isFFTInt = if (isFFT) 1 else 0
    write(fftIdxAddr,idx)
    write(isFFTAddr,isFFTInt)
    write(setupAddr,0)
    var setupDone = read(setupAddr)
    while (setupDone != BigInt(1)){
      setupDone = read(setupAddr)
    }
    if (read(fftIdxAddr) != BigInt(idx)) Error("fftIdx doesn't match " + idx)
    if (read(isFFTAddr) != BigInt(isFFTInt)) Error("isFFT doesn't match " + isFFT)
    Status("Setup FFT = " + n + " in Mode = " + {if (isFFT) "FFT" else "IFFT"})
  }

  def write(a:Int, d:BigInt): Unit = {
    var fftReady = peek(c.io.smi.req.ready)
    while(!fftReady){
      // Not ready
      step(1)
      fftReady = peek(c.io.smi.req.ready)
    }
    poke(c.io.smi.req.valid,true)
    poke(c.io.smi.req.bits.rw,true)
    poke(c.io.smi.req.bits.addr,a)
    poke(c.io.smi.req.bits.data,d)
    poke(c.io.smi.resp.ready,false)
    step(1)
    poke(c.io.smi.req.valid,false)
    poke(c.io.smi.resp.ready,true)
    var fftValid = peek(c.io.smi.resp.valid)
    while(!fftValid){
      // Not valid
      step(1)
      fftValid = peek(c.io.smi.resp.valid)
    }
  }

  def read(a:Int): BigInt ={
    var fftReady = peek(c.io.smi.req.ready)
    while(!fftReady){
      // Not ready
      step(1)
      fftReady = peek(c.io.smi.req.ready)
    }
    poke(c.io.smi.req.valid,true)
    poke(c.io.smi.req.bits.rw,false)
    poke(c.io.smi.req.bits.addr,a)
    poke(c.io.smi.resp.ready,false)
    step(1)
    poke(c.io.smi.req.valid,false)
    poke(c.io.smi.resp.ready,true)
    var fftValid = peek(c.io.smi.resp.valid)
    while(!fftValid){
      // Not valid
      step(1)
      fftValid = peek(c.io.smi.resp.valid)
    }
    peek(c.io.smi.resp.bits)
  }

  def checkError(exp: ScalaComplex, out: ScalaComplex, outrBI: BigInt, outiBI: BigInt, errmsg: String = ""): Boolean = {
    val (goodR, toleranceR) = checkDecimal(c.rocketToFFT.fft.io.din.real, exp.real, out.real, outrBI)
    val (goodI, toleranceI) = checkDecimal(c.rocketToFFT.fft.io.dout.imag, exp.imag, out.imag, outiBI)
    val good = goodR && goodI
    if (!good) {
      Error("Output value " + out.toString + " doesn't match expected " + exp.toString + " " + errmsg)
    }
    good
  }

}