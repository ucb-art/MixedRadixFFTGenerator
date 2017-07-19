package dspblocks.fft

import chisel3._
import chisel3.experimental._
import dsptools.numbers._
import dsptools.numbers.implicits._
import barstools.tapeout.TestParams
import org.scalatest.{FlatSpec, Matchers}
import barstools.tapeout.transforms.pads._
import barstools.tapeout.transforms.clkgen._
import breeze.math.Complex
import dsptools.{DspTester, DspTesterOptionsManager}
import barstools.tapeout.transforms._

class FFASTTopSpec extends FlatSpec with Matchers {
  behavior of "FFASTTop"
  it should "work" in {

    import dspblocks.fft.FFASTTopParams._

    val opt = TestParams.optionsBTolWaveformTB(lsbs = fpBP - 1, outDir = "test_run_dir/FFASTTopTB")

    dsptools.DspContext.alter(dspContext) {
      dsptools.Driver.execute(() => 
        new FFASTTopWrapper(
          //adcDataType = DspReal(),
          //dspDataType = DspReal(),
          adcDataType = adcDataType, 
          dspDataType = dspDataType,
          ffastParams = ffastParams,
          maxNumPeels = maxNumPeels,
          useBlackBox = false
        ), opt
      ) { c =>
        new FFASTTopTester(c)
      } should be (true)
    }
  }
}

class FFASTTopSmallSpec extends FlatSpec with Matchers {
  behavior of "FFASTTop (small)"
  it should "work" in {

    import dspblocks.fft.FFASTTopParams._

    val opt = TestParams.optionsBTolWaveformTB(lsbs = fpBP - 1, outDir = "test_run_dir/FFASTTopSmallTB")

    dsptools.DspContext.alter(dspContext) {
      dsptools.Driver.execute(() => 
        new FFASTTopWrapper(
          //adcDataType = DspReal(),
          //dspDataType = DspReal(),
          adcDataType = adcDataType, 
          dspDataType = dspDataType,
          ffastParams = ffastParams.copy(delays = Seq(Seq(0, 1))),
          maxNumPeels = maxNumPeels,
          useBlackBox = false
        ), opt
      ) { c =>
        new FFASTTopTester(c)
      } should be (true)
    }
  }
}

class FFASTTopBuildSpec extends FlatSpec with Matchers {
  behavior of "FFASTTopBuild"
  it should "not fail to build" in {

    import dspblocks.fft.FFASTTopParams._

    dsptools.DspContext.alter(dspContext) {
      chisel3.Driver.execute(TestParams.buildWithMemories(tName = "FFASTTop"), () => 
        // WARNING UNUSED THINGS NOT CONNECTED IN WRAPPER!!!
        new FFASTTop(
          adcDataType = adcDataType, 
          dspDataType = dspDataType,
          ffastParams = ffastParams,
          maxNumPeels = maxNumPeels,
          useBlackBox = true,
          override_clock = None,
          override_reset = None
        )
      ) 
    }

  }
}

class FFASTTopBuildSmallMemTestSpec extends FlatSpec with Matchers {
  behavior of "FFASTTopBuild (small, without memories)"
  it should "not fail to build" in {

    import dspblocks.fft.FFASTTopParams._

    dsptools.DspContext.alter(dspContext) {
      chisel3.Driver.execute(TestParams.buildWithMemories(name = "BuildSmall"), () => 
        new FFASTTopWrapper(
          adcDataType = adcDataType, 
          dspDataType = dspDataType,
          ffastParams = ffastParams.copy(delays = Seq(Seq(0, 1))),
          maxNumPeels = maxNumPeels,
          useBlackBox = false
        )
      ) 
    }

  }
}

//////////////

class FFASTTopTester[T <: Data:RealBits](c: FFASTTopWrapper[T]) extends DspTester(c) {

  val usedDebugStates = Seq("ADCCollectDebug")
  val numLoops = 2
  // TODO: Don't hard code! (fix by width)
  val adcInInc = 1.0 / (1 << 8) //1.0 / (1 << 8) + 1.0 / (1 << 5)
  // Fastest clk
  val subsamplingT = c.subsamplingT
  val adcInStart = -1.0 //-10000.0
  val checksPerformed = scala.collection.mutable.ArrayBuffer[String]()

  case class DebugNext(idx: Int, done: Boolean)

  // WARNING: Not thread-safe!!!
  val peekedResults = c.ffastParams.getSubFFTDelayKeys.map { case (n, ph) => 
    (n, ph) -> Array.fill(c.ffastParams.subFFTns.max)(Complex(0.0, 0.0))
  }.toMap

  case class PeekedComplexBigInt(real: BigInt, imag: BigInt)
  val peekedResultsBigInts = c.ffastParams.getSubFFTDelayKeys.map { case (n, ph) => 
    (n, ph) -> Array.fill(c.ffastParams.subFFTns.max)(PeekedComplexBigInt(0, 0))
  }.toMap
  // TODO: Generalize

  def getEnable(n: Int, ph: Int): Int = {
    val fftGroups = c.ffastParams.getSubFFTDelayKeys  
    val (groupTag, groupIdx) = fftGroups.zipWithIndex.filter { 
      case ((nG, phG), idx) => n == nG && ph == phG 
    }.head
    1 << groupIdx
  }
  def getAllEnable: Int = {
    val fftGroups = c.ffastParams.getSubFFTDelayKeys  
    fftGroups.map { case (n, ph) => getEnable(n, ph) }.sum
  }

  // TODO: Combine with dsptools' checkDecimal
  def compare(
      exp: Seq[Complex], 
      out: Seq[Complex], 
      tag: (Int, Int), 
      fixTolOverride: Int = -1, 
      test: String = "", 
      print: Boolean = false, 
      zeroThresholdPwr: Double = 1,
      disp: Boolean = false) = {

    val fft = tag._1
    val ph = tag._2

    val testRef = s"$test RESULTS: SubFFT $fft, Phase $ph"
    checksPerformed += testRef

    println(s" ***** $testRef ***** ")

    import breeze.linalg._
    import breeze.plot._

    val f = Figure()
    val p = f.subplot(0)

    p.legend_=(true)

    val xaxis = (0 until exp.length).map(e => e.toDouble).toSeq.toArray

    // Log 0 doesn't exist
    val plotMin = 0.0000000001
    val outPlot = out.map(c => 20 * math.log10(Seq(c.abs, plotMin).max)).toSeq
    val expPlot = exp.map(c => 20 * math.log10(Seq(c.abs, plotMin).max)).toSeq
    p += plot(xaxis, outPlot.toArray, name = "Result")
    p += plot(xaxis, expPlot.toArray, name = "Expected")
    
    p.ylim(Seq(-100.0, expPlot.min).max, expPlot.max)
    p.title_=(s"FFT: $fft Phase: $ph $test")

    p.xlabel = "Frequency Bin"
    p.ylabel = "20log10(||Vpeak||)"

    if (print)
      f.saveas(s"test_run_dir/${test}Result_${fft}_${ph}.pdf") 

    // TODO: Remove some redundancy

    def toMax(w: Int): BigInt = (BigInt(1) << w) - 1

    val floTolDec = math.pow(10, -realTolDecPts.value)   
    val fixTolInt = toMax(if (fixTolOverride == -1) fixTolLSBs.value else fixTolOverride)
    val tol = c.dspDataType match {
      case r: DspReal => floTolDec
      case f: FixedPoint =>
        require(f.binaryPoint.known, "Binary point must be known!")
        FixedPoint.toDouble(fixTolInt, f.binaryPoint.get)
    }

    val (expectPasses, maxErrors) = exp.zip(out).zipWithIndex.map { case ((e, o), i) =>

      val magSq = o.real * o.real + o.imag * o.imag
      if (magSq > zeroThresholdPwr && disp)
        println(s"FFT: $fft Ph: $ph Index: $i MagSq: $magSq")  
        
      val realDelta = math.abs(e.real - o.real)
      val imagDelta = math.abs(e.imag - o.imag)
      val pass = updatableDspVerbose.withValue(false) {
        expect(realDelta < tol && imagDelta < tol, 
          s"Expected: $e \t Actual: $o")
      }
      (pass, Seq(realDelta, imagDelta).max)
    }.unzip
    expect(expectPasses.reduce(_ & _), s"$test should be right. Tolerance $tol. Worst error ${maxErrors.max}.")
  }

  def clearResults() = {
    c.ffastParams.getSubFFTDelayKeys foreach { case (n, ph) => 
      // TODO: There has to be a better way to reset!
      for (arrayIdx <- 0 until c.ffastParams.subFFTns.max) {
        peekedResults(n, ph)(arrayIdx) = Complex(0.0, 0.0)
        peekedResultsBigInts(n, ph)(arrayIdx) = PeekedComplexBigInt(0, 0)
      }
    }
  }

  def setupDebug(debugStates: Seq[String]) = {
    val debugStatesPoke = debugStates.foldLeft(0)((accum, next) => accum + (1 << c.mod.statesInt(next)))
    // val debugStatesPoke = 1 << c.mod.statesInt(debugStates)
    updatableDspVerbose.withValue(false) {
      poke(c.io.scr.debugStates, debugStatesPoke)
      poke(c.io.scr.cpuDone, false)
      // In debug, always read; maybe write -- overwrite later
      poke(c.io.scr.ctrlMemReadFromCPU.re, getAllEnable)
      poke(c.io.scr.ctrlMemWrite.we, 0)
    }
  }

  def debugCollection(debugNext: DebugNext, stopIdx: Int = -1, wantToEscape: Boolean = true): DebugNext = {
    val done = DebugNext(0, true)
    val incInput = DebugNext(debugNext.idx + 1, false)
    updatableDspVerbose.withValue(false) {
      
      poke(c.io.scr.ctrlMemReadFromCPU.rIdx, debugNext.idx % c.ffastParams.subFFTns.max)
      if (debugNext.idx >= c.ffastParams.subFFTns.max)
        poke(c.io.scr.ctrlMemReadFromCPU.re, 0)
      
      if (peek(c.io.scr.reToCPU) == 0) {
        // Don't increment if can't read
        debugNext
      }
      else {
        // All memories must be fully read
        val rIdxOutMin = c.ffastParams.getSubFFTDelayKeys.map { case (n, ph) => 

          val rIdxOut = peek(c.io.scr.ctrlMemReadToCPU(n)(ph).rIdx)

          if (rIdxOut < n) {
            peekedResults(n, ph)(rIdxOut) = peek(c.io.scr.ctrlMemReadToCPU(n)(ph).dout)

            // FOR ROCKET-CHIP TESTING
            val realPeek = dspPeekWithBigInt(c.io.scr.ctrlMemReadToCPU(n)(ph).dout.real)._2
            val imagPeek = dspPeekWithBigInt(c.io.scr.ctrlMemReadToCPU(n)(ph).dout.imag)._2
            peekedResultsBigInts(n, ph)(rIdxOut) = PeekedComplexBigInt(real = realPeek, imag = imagPeek)
          }

          rIdxOut
        }.min
        val stopCond = (stopIdx != -1 && rIdxOutMin == stopIdx) || 
          (stopIdx == -1 && rIdxOutMin == c.ffastParams.subFFTns.max - 1)

        if (stopCond) {
          poke(c.io.scr.cpuDone, wantToEscape)
          done
        }
        else incInput
      }
    }
  }

  def quantize(ins: Seq[Double]): Seq[Double] = {
    import dspblocks.fft.FFASTTopParams._
    c.dspDataType match {
      case f: FixedPoint => 
        ins.map { case i => 
          // Quantization needs to match exactly
          // FixedPoint.toDouble(FixedPoint.toBigInt(i, adcBP), adcBP)
          // math.round(i * (1 << adcBP)).toDouble / (1 << adcBP)
          val temp1 = i * (1 << adcBP).toDouble
          // ADC uses round half up
          val temp2 = math.floor(temp1 + 0.5).toDouble
          temp2 / (1 << adcBP)
        }
      case r: DspReal => ins 
    }
  }

  def checkADCResults(test: String, skip: Boolean = false, customInput: Option[Seq[Double]] = None): Complex = {
    import dspblocks.fft.FFASTTopParams._
    val startingValue = customInput match {
      case None if skip =>
        // All test results can be derived once you know at what point ADC input was fed in (which time sample was 
        // gotten when clks were aligned)
        peekedResults(c.ffastParams.subFFTns.min, 0)(0)
      case Some(ins) if skip =>
        // Need to quantize the input to get a good match
        // Patterns repeat a lot so need very specific (inefficient) search
        val quantizedIns = quantize(ins)
        val n = c.ffastParams.subFFTns.min
        val outValsSeq = peekedResults(n, 0).toSeq.take(n)
        val subsamplingFactor = c.ffastParams.subSamplingFactors(n)
        val indicesThatMatchHead = quantizedIns.zipWithIndex.filter(_._1 == outValsSeq.head.real).map(_._2)
        val possibleIdx = indicesThatMatchHead.find { case i => 
          val possibleMatch = (0 until n).map(x => (i + x * subsamplingFactor) % c.ffastParams.fftn).map(x => Complex(quantizedIns(x), 0.0))
          possibleMatch == outValsSeq
        }
        require(possibleIdx != None, "Idx must exist!")
        // TODO: Don't fake this
        Complex(possibleIdx.get.toDouble, 0.0)
      case Some(ins) if !skip =>
        // TODO: Get rid of copy paste
        // Need to quantize the input to get a good match
        val quantizedIns = quantize(ins)
        var headAtPh0Idx: Int = 0
        // Guarantee you look at PH0 first
        peekedResults.toSeq.sortBy { case ((n, ph), outVals) => (n, ph) } foreach { case ((n, ph), outVals) =>
          val testRef = s"$test ADC RESULTS: SubFFT $n, Phase $ph"
          checksPerformed += testRef

          println(s" ***** $testRef ***** ")
          // TODO: Turn above into function?

          // No good reason for choosing the min
          val outValsSeq = outVals.toSeq.take(n)
          if (c.ffastParams.subFFTns.min == n && ph == 0) {
            val subsamplingFactor = c.ffastParams.subSamplingFactors(n)
            val indicesThatMatchHead = quantizedIns.zipWithIndex.filter(_._1 == outValsSeq.head.real).map(_._2)
            val possibleIdx = indicesThatMatchHead.find { case i => 
              val possibleMatch = (0 until n).map(x => (i + x * subsamplingFactor) % c.ffastParams.fftn).map(x => Complex(quantizedIns(x), 0.0))
              possibleMatch == outValsSeq
            }
            require(possibleIdx != None, "Idx must exist!")
            headAtPh0Idx = possibleIdx.get
          }
          else {
            // TODO: Copy pasta
            val headAtPhXIdx = (headAtPh0Idx + ph) % c.ffastParams.fftn
            require(expect(outVals.head == Complex(quantizedIns(headAtPhXIdx), 0.0), 
              s"Relative samples must be right across sub-ADCs. Head @ ${outValsSeq.head}"))
          }

          val subsamplingFactor = c.ffastParams.subSamplingFactors(n)
          val headAtPhXIdx = (headAtPh0Idx + ph) % c.ffastParams.fftn
          val expectedIdxAdd = Seq.fill(outValsSeq.length - 1)(subsamplingFactor)
          val expectedIdx = expectedIdxAdd.scanLeft(headAtPhXIdx) { (accum, next) => 
            (accum + next) % c.ffastParams.fftn
          }
          val expected = expectedIdx.map(i => Complex(quantizedIns(i), 0.0))
          val pass = expect(outValsSeq == expected, "Subsampled values should be spaced correctly!")
          if (!pass) {
            val firstFailed = outValsSeq.zip(expected).find { case (out, exp) => out != exp }.get
            println(
              "First failed @ index " + 
              expected.indexOf(firstFailed._2) + 
              s". Expected ${firstFailed._2}. Got ${firstFailed._1}.")
          }
          // Terminate if fail
          require(pass)
          // println(s"Expected : \t" + expected.mkString(","))
        }
        // TODO: Ew don't do this
        Complex(headAtPh0Idx.toDouble, 0.0)

      case None if !skip => 
        var headAtPh0 = Complex(0.0, 0.0)
        // Guarantee you look at PH0 first
        peekedResults.toSeq.sortBy { case ((n, ph), outVals) => (n, ph) } foreach { case ((n, ph), outVals) =>
          val testRef = s"$test ADC RESULTS: SubFFT $n, Phase $ph"
          checksPerformed += testRef

          println(s" ***** $testRef ***** ")

          // No good reason for choosing the min
          val outValsSeq = outVals.toSeq.take(n)
          if (c.ffastParams.subFFTns.min == n && ph == 0)
            headAtPh0 = outValsSeq.head
          else {
            val headAtPhXTemp = headAtPh0 + Complex(ph * adcInInc, 0.0)
            val headAtPhX = {
              if (headAtPhXTemp.real >= math.abs(adcInStart)) Complex(headAtPhXTemp.real + 2 * adcInStart, 0.0)
              else headAtPhXTemp
            }
            require(expect(outValsSeq.head == headAtPhX, 
              s"Relative samples must be right across sub-ADCs. Head @ ${outValsSeq.head}"))
          }
          val subsamplingFactor = c.ffastParams.subSamplingFactors(n)
          val expectedAdd = Seq.fill(outValsSeq.length - 1)(Complex(subsamplingFactor * adcInInc, 0.0))
          val expected = expectedAdd.scanLeft(outValsSeq.head) { (accum, next) => 
            val unmoddedOut = accum + next 
            if (unmoddedOut.real >= math.abs(adcInStart)) Complex(unmoddedOut.real + 2 * adcInStart, 0.0)
            else unmoddedOut
          }
          // println(s"Loop $loop, SubFFT $n, Phase $ph : \t" + outValsSeq.mkString(","))
          val pass = expect(outValsSeq == expected, "Subsampled values should be spaced correctly!")
          if (!pass) {
            val firstFailed = outValsSeq.zip(expected).find { case (out, exp) => out != exp }.get
            println(
              "First failed @ index " + 
              expected.indexOf(firstFailed._2) + 
              s". Expected ${firstFailed._2}. Got ${firstFailed._1}.")
          }
          // Terminate if fail
          require(pass)
          // println(s"Expected : \t" + expected.mkString(","))
        }
        headAtPh0
    }
    clearResults()
    if (customInput == None) println(s"------- $test ADC In Initial Value: $startingValue")
    else println(s"------- $test ADC In Initial Value: ${startingValue.real.toInt}")
    startingValue
  }

  def cycleThroughUntil(state: String) = {
    updatableDspVerbose.withValue(false) { 
      var currState = peek(c.io.scr.currentState)
      val debugStateIdxs = c.mod.statesInt.filter { 
        case (key, value) => key.endsWith("Debug") 
      }.map { case (key, value) => value }.toSeq
      var inDebugState = false
      while (currState != c.mod.statesInt(state)) {
        // Skip through debugs back to beginning
        // Note: My state machine requires a rising edge detect to change state
        // which helps w/ timing but is annoying if you forget about it
        if (debugStateIdxs.contains(currState) && !inDebugState) {
          poke(c.io.scr.cpuDone, false.B)
          inDebugState = true
        }
        else if (debugStateIdxs.contains(currState) && inDebugState) {
          poke(c.io.scr.cpuDone, true.B)
          inDebugState = false
        }    
        step(subsamplingT) 
        currState = peek(c.io.scr.currentState)
      }
      poke(c.io.scr.cpuDone, false.B)
    }
  }

  def runADC(customInput: Option[Seq[Double]] = None) = {
    // Cycle back
    updatableDspVerbose.withValue(false) { 
      cycleThroughUntil("ADCCollect")
      customInput match { 
        case Some(ins) =>
          var adcIdx = 0
          while (peek(c.io.scr.currentState) == c.mod.statesInt("ADCCollect")) {
            // Fast rate!
            poke(c.io.ADCINP, ins(adcIdx)) 
            step(1)
            adcIdx = (adcIdx + 1) % c.ffastParams.fftn
          }  
        case None => 
          var adcIn = adcInStart
          while (peek(c.io.scr.currentState) == c.mod.statesInt("ADCCollect")) {
            // Fast rate!
            poke(c.io.ADCINP, adcIn) 
            step(1)
            adcIn += adcInInc
            if (adcIn >= math.abs(adcInStart)) adcIn = adcIn + 2 * adcInStart
          }  
      }
    }
  }

  def runDebug(state: String, stopIdx: Int = -1) = {
    updatableDspVerbose.withValue(false) { 
      cycleThroughUntil(state) 
      poke(c.io.scr.ctrlMemReadFromCPU.re, getAllEnable)
      var debugNext = DebugNext(idx = 0, done = false)
      while (!debugNext.done) {
        debugNext = debugCollection(debugNext, stopIdx)
        step(subsamplingT)
      }
    }
  }

  def writeInDebug(debugNext: DebugNext, flipInput: Boolean, customInput: Option[Seq[Complex]]): DebugNext = {
    val done = DebugNext(0, true)
    val incInput = DebugNext(debugNext.idx + 1, false)
    updatableDspVerbose.withValue(false) {

      c.ffastParams.subFFTns.foreach { case n =>
        // Once the index has exceeded the subFFT n, turn write off
        if (debugNext.idx == n) {
          val origWE = peek(c.io.scr.ctrlMemWrite.we)
          val turnOffNEn = ~(c.ffastParams.adcDelays.map(ph => getEnable(n, ph)).sum) & origWE
          poke(c.io.scr.ctrlMemWrite.we, turnOffNEn)
        }
      }

      if (BigInt(debugNext.idx).bitLength <= c.io.scr.ctrlMemWrite.wIdx.getWidth) 
        poke(c.io.scr.ctrlMemWrite.wIdx, debugNext.idx)
      val input = customInput match {
        case Some(seq) => 
          if (debugNext.idx >= seq.length) Complex(0.0, 0.0)
          else seq(debugNext.idx)
        case None => 
          if (flipInput) Complex(-debugNext.idx.toDouble * adcInInc, debugNext.idx.toDouble * adcInInc)
          else Complex(debugNext.idx.toDouble * adcInInc, -debugNext.idx.toDouble * adcInInc)
      }
      poke(c.io.scr.ctrlMemWrite.din, input)
      // Some margin to actually finish writing (theoretically, should read back the value...)
      if (debugNext.idx == c.ffastParams.subFFTns.max + 5) {
        done
      }
      else incInput
    }
  }

  def runWriteDebug(state: String, flipInput: Boolean = true, customInput: Option[Seq[Complex]] = None, wantToEscape: Boolean = false): Boolean = {
    updatableDspVerbose.withValue(false) { 
      cycleThroughUntil(state)  
      poke(c.io.scr.ctrlMemWrite.we, getAllEnable)
      var debugNext = DebugNext(idx = 0, done = false)
      while (!debugNext.done) {
        debugNext = writeInDebug(debugNext, flipInput, customInput)
        step(subsamplingT)
      }
      poke(c.io.scr.cpuDone, wantToEscape)
    }  
    // What the next one should do
    !flipInput
  }

  // TODO: Check write results assuming real sine
  def checkWriteResults(state: String, flipInput: Boolean): Unit = {
    peekedResults.toSeq.sortBy { case ((n, ph), outVals) => (n, ph) } foreach { case ((n, ph), outVals) =>
      val testRef = s"WRITE RESULTS: DebugState $state SubFFT $n, Phase $ph, R/I Flipped? $flipInput"
      checksPerformed += testRef

      println(s" ***** $testRef ***** ")

      val outValsSeq = outVals.toSeq.take(n)
      
      val expected = (0 until outValsSeq.length).map { case idx => 
        if (flipInput) Complex(-idx.toDouble * adcInInc, idx.toDouble * adcInInc)
        else Complex(idx.toDouble * adcInInc, -idx.toDouble * adcInInc)
      }
      // TODO: Remove copy-pasta
      val pass = expect(outValsSeq == expected, "Written values should be right")
      if (!pass) {
        val firstFailed = outValsSeq.zip(expected).find { case (out, exp) => out != exp }.get
        println(
          "First failed @ index " + 
          expected.indexOf(firstFailed._2) + 
          s". Expected ${firstFailed._2}. Got ${firstFailed._1}.")
      }
      // Terminate if fail
      require(pass)
    }
    clearResults()
  }

  def setupAdcCal(): Unit = {
    updatableDspVerbose.withValue(false) { 
      cycleThroughUntil("ADCCollectDebug")  
      poke(c.io.adcCalScr.calWE, getAllEnable)
      poke(c.io.adcCalScr.calAllRE, false)
      // Write to ADC Cal
      for (x <- 0 until (1 << c.adcDataType.getWidth)) {
        poke(c.io.adcCalScr.loadAddr, x)
        c.ffastParams.getSubFFTDelayKeys.map { case (n, ph) => 
          poke(c.io.adcCalScr.calCoeff(n)(ph), x)
        }
        step(subsamplingT)
      }
      poke(c.io.adcCalScr.calWE, 0)
      step(subsamplingT)
      poke(c.io.adcCalScr.calAllRE, true)
      // Read from ADC Cal
      for (x <- 0 until (1 << c.adcDataType.getWidth)) {
        poke(c.io.adcCalScr.loadAddr, x)
        // Mem delay
        step(2 * subsamplingT)
        c.ffastParams.getSubFFTDelayKeys.map { case (n, ph) => 
          expect(c.io.adcCalScr.calOut(n)(ph), x)
        }
        step(subsamplingT)
      }
      poke(c.io.adcCalScr.calAllRE, false)

    } 
  }

  // ADC Stuff should fail if not hooked up properly
  def checkConnection(): Unit = {
    // Non black box version only used during testing
    val checkedPorts = (SCRHelper(c.io.adcScr, print = false) ++ Seq((c.io.ADCBIAS -> "ADCBIAS"))).map { case (el, str) => str -> el }.toMap
    c.mod.collectADCSamplesBlock.analogBlock.analogModel.asInstanceOf[AnalogModel[T]].connectionCheckMap.foreach { case (str, (el, maxVal)) => 
      updatableDspVerbose.withValue(false) { 
        poke(checkedPorts(str), maxVal)
      } 
    }
  }

  def setupPeel(): Unit = {
    updatableDspVerbose.withValue(false) { 
      val nf = 20
      val numDelays = c.ffastParams.adcDelays.length
      c.ffastParams.subFFTns.zipWithIndex.foreach { case (n, idx) =>
        // TODO: Less arbitrary
        //val noiseThresholdPwr = nf * numDelays.toDouble / math.pow(n, 2).toDouble
        //val sigThresholdPwr = nf * 1.toDouble / math.pow(n, 2).toDouble

        // TODO: FIX -- DEPENDS ON TEST VECTORS
        // Above this number -> multiton
        val noiseThresholdPwr = numDelays * .01 *.01 *.7
        val sigThresholdPwr = .006 * .006

        poke(c.io.peelScr.zeroThresholdPwr(n), noiseThresholdPwr)
        poke(c.io.peelScr.sigThresholdPwr(n), sigThresholdPwr)
        poke(c.io.peelScr.sigThresholdPwrMulDlys(n), sigThresholdPwr * numDelays)
        c.ffastParams.delayConstants.zipWithIndex foreach { case (const, id) =>
          poke(c.io.peelScr.delayCalcConstants(n)(id), const)
        }
        c.ffastParams.adcDelays foreach { case d =>
          poke(c.io.peelScr.delayCalibration(n)(d), d.toDouble)
          // Senssiitivity to delay offset
          //poke(c.io.peelScr.delayCalibration(n)(d), d.toDouble + (-1 + idx.toDouble) / 250)
        }
      }
    }
  }
    
  import dspblocks.fft.FFASTTopParams._
  
  // Clk gen reset
  reset(10)

  updatableDspVerbose.withValue(false) { 

    poke(c.io.ADCINM, 0.0)

    poke(c.io.stateMachineReset, true.B)
    step(subsamplingT * 2)
    poke(c.io.stateMachineReset, false.B)

    // Takes 3 slow clk cycles to synchronize
    step(subsamplingT * 4)

  }

// -------------------------------- GENERAL READ AND WRITE TESTS

  setupDebug(usedDebugStates)
  setupAdcCal()
  checkConnection()

/*
  for (loopNum <- 0 until numLoops) {
    runADC()
    // Always run ADCCollectDebug -- gives you a sense of how to calculate stuff afterwards
    val stopIdx = if (usedDebugStates.contains("ADCCollectDebug")) -1 else 0
    runDebug("ADCCollectDebug", stopIdx)
    checkADCResults(s"Loop $loopNum")
  }

  // Check that writing works
  runADC()
  runWriteDebug("ADCCollectDebug", flipInput = false)
  runDebug("ADCCollectDebug")
  checkWriteResults("ADCCollectDebug", flipInput = false)

  runADC()
  runWriteDebug("ADCCollectDebug", flipInput = true)
  runDebug("ADCCollectDebug")
  checkWriteResults("ADCCollectDebug", flipInput = true)

  // WARNING: I'm NOT first quantizing the input -- so comparing with floating point
  // calculation (will be worse)

  ////////////////////////////////////////////////////////////////////////////////////

  // Warning: To exit out of debug, need to detect RISING EDGE
  setupDebug(Seq("ADCCollectDebug", "FFTDebug"))
  runADC()
  runADC()

  // Smaller FFTs will use a subset
  val inFFT = FFTTestVectors.createInput(c.ffastParams.subFFTns.max, fracBits = adcBP)
  // Writes to all memories simultaneously
  runWriteDebug("ADCCollectDebug", customInput = Some(inFFT))
  // Collect data
  // runDebug("ADCCollectDebug")
  // clearResults()
  // Should auto-escape from ADCCollectDebug
  runDebug("FFTDebug")
  peekedResults.toSeq foreach { case ((n, ph), outVals) =>
    compare(exp = FFTTestVectors.createOutput(inFFT.take(n)), out = outVals.toSeq.take(n), tag = (n, ph), test = "Debug Write to FFT")
  }
  clearResults() 

// -------------------------------- FOR ROCKET-CHIP TESTING

  // Skip ADC Collect state for basic debug, since ADC isn't hooked up
  setupDebug(Seq("ADCCollect", "ADCCollectDebug", "FFTDebug"))
  // Smaller FFTs will use a subset
  // val inFFT = FFTTestVectors.createInput(c.ffastParams.subFFTns.max, fracBits = adcBP)

  val cTestInputs = inFFT.map(x => FixedPoint.toBigInt(x.real, fpBP))
  
  // Writes to all memories simultaneously
  runWriteDebug("ADCCollectDebug", customInput = Some(inFFT))
  // Should auto-escape from ADCCollectDebug
  runDebug("FFTDebug")
  peekedResults.toSeq foreach { case ((n, ph), outVals) =>
    compare(exp = FFTTestVectors.createOutput(inFFT.take(n)), out = outVals.toSeq.take(n), tag = (n, ph), test = "Debug Write to FFT")
  }

  println("\n\n *************************************************** \n\n")
  checksPerformed.toSeq foreach { x => println(x) }
*/
// -------------------------------- ADC -> FFT OUTPUT TESTS

  val lastState = c.mod.basicStateNames.last + "Debug"

  setupDebug(Seq("ADCCollectDebug", "FFTDebug", "PopulateNonZerotonsDebug", lastState))
  setupPeel()

  // WARNING: NO QUANTIZATION SO RESULTS WILL BE WORSE IN COMPARISON
  val inLarge = FFTTestVectors.createInput(c.ffastParams.fftn, fracBits = adcBP)
  val inLargeReal = inLarge.map(x => x.real)
  // val fftLargeThreshold = 1.toDouble / math.pow(c.ffastParams.fftn, 2).toDouble
  val fftLargeThreshold = 0.004 * 0.004
  val fftLargeOutIdxs = FFTTestVectors.expectedSubSampleLocations(inLarge, zeroThresholdPwr = fftLargeThreshold, disp = true)

  runADC(customInput = Some(inLargeReal))
  /*
  runDebug("ADCCollectDebug")
  // TODO: Don't use complex
  val adcInInitialIdx = checkADCResults(s"FFTN ${c.ffastParams.fftn} In", customInput = Some(inLargeReal), skip = true).real.toInt
  runDebug("FFTDebug")

  peekedResults.toSeq.sortBy { case ((n, ph), outVals) => (n, ph) }. foreach { case ((n, ph), outVals) =>
    val subsamplingFactor = c.ffastParams.subSamplingFactors(n)
    // TODO: Convert everything to this (easy to understand)
    val rotatedIn = inLarge.drop(adcInInitialIdx) ++ inLarge.take(adcInInitialIdx)
    val subsampledIn = rotatedIn.drop(ph).grouped(subsamplingFactor).map(_.head).toSeq
    require(subsampledIn.length == n, s"# of subsampled inputs should be $n")
    println(s"FFT: $n Ph: $ph (Normalized)")
    val fftThreshold = 1.toDouble / math.pow(n, 2).toDouble
    val (expectedOut, expectedOutIdx) = FFTTestVectors.createOutputInt(subsampledIn, zeroThresholdPwr = fftThreshold, disp = true)
    compare(
      exp = expectedOut, 
      out = outVals.toSeq.take(n), 
      tag = (n, ph), 
      test = "Subsampled and Delayed FFTs", 
      fixTolOverride = fpBP + 1,
      zeroThresholdPwr = 1,
      disp = true)

  }

  val mapBinToSubFFTIdx = PeelingScheduling.getBinToSubFFTIdxMap(c.ffastParams)

  val binToSubFFTIdxExpected = for (fullFFTBin <- fftLargeOutIdxs ; subFFT <- c.ffastParams.subFFTns) yield {
    val o = mapBinToSubFFTIdx(fullFFTBin)(subFFT)
    ((fullFFTBin, subFFT), o)
  }

  binToSubFFTIdxExpected.sortBy { case ((fullFFTBin, subFFT), subFFTIdx) => (subFFT, subFFTIdx) }. foreach { case ((fullFFTBin, subFFT), subFFTIdx) =>
    println(s"Sub FFT: $subFFT Full FFT Bin: $fullFFTBin Sub FFT Index: $subFFTIdx")
  }
  */

  // TODO: Convert to expect tests

  cycleThroughUntil("PopulateNonZerotonsDebug")
  updatableDspVerbose.withValue(false) { 
    poke(c.io.peelScr.cbREFromCPU, true) 
    poke(c.io.scr.ctrlMemReadFromCPU.re, getAllEnable)
  }
/*
  c.ffastParams.subFFTns foreach { case n =>
    println(s"Non-zero bins for FFT $n")
    c.ffastParams.adcDelays foreach { case ph => 
      val cbLength = updatableDspVerbose.withValue(false) { peek(c.io.peelScr.cbLength(n)) }
      (0 until cbLength) foreach { case idx =>
        updatableDspVerbose.withValue(false) {
          poke(c.io.peelScr.cbRIdxFromCPU, idx)
          step(subsamplingT * 4)
        }
        val (bin, binSignal) = updatableDspVerbose.withValue(false) {
          val bin = peek(c.io.peelScr.cbSubBinToCPU(n))
          poke(c.io.scr.ctrlMemReadFromCPU.rIdx, bin)
          step(subsamplingT * 4)
          val bp = c.io.scr.ctrlMemReadToCPU(n)(ph).dout.real.asInstanceOf[FixedPoint].binaryPoint.get
          val newBP = c.io.scr.ctrlMemReadToCPU(n)(ph).dout.real.asInstanceOf[FixedPoint].getWidth - 1
          (bin, peek(c.io.scr.ctrlMemReadToCPU(n)(ph).dout) / math.pow(2, newBP - bp))
        }
        println(s"    FFT: $n PH: $ph Bin: $bin \t $binSignal")
      }   
    }
  }
*/
  // TODO: Expect, don't hard code
  val seTest = 
    SingletonEstimatorTest(c.ffastParams, subFFT = 675, subFFTIdx = 135, binLoc = 12960, isSingleton = true, 
      delayedIns = Seq(
        0 -> Complex(0.09999557432549745, 2.185843291722172E-5),
        1 -> Complex(-0.08087364851509277, -0.05876563484628506),
        6 -> Complex(-0.0809610863716996, -0.05877587352157287),
        9 -> Complex(-0.08085895723749413, 0.05880696639231166),
        12 -> Complex(0.030908872124478386, 0.09509897704946693),
        19 -> Complex(-0.08087959862445164, 0.05879325658840878)
      ).toMap
    )

  poke(c.io.peelScr.singletonEstCurrentFFTBools, 1)
  poke(c.io.peelScr.singletonEstSubFFTIdx, 135)
  c.ffastParams.adcDelays.foreach { d =>
    poke(c.io.peelScr.singletonIns(d), seTest.delayedIns(d))
  }
  step(subsamplingT * 70)
  peek(c.io.peelScr.seBinType("zero"))
  peek(c.io.peelScr.seBinType("single"))
  peek(c.io.peelScr.seBinType("multi"))
  peek(c.io.peelScr.seBinLoc)
  peek(c.io.peelScr.seBinSignal)

  val ffastBins = scala.collection.mutable.ArrayBuffer[Int]()

  cycleThroughUntil(lastState)
  val lastPointerFound = peek(c.io.peelScr.ffastFoundPointer)
  poke(c.io.peelScr.ffastREFromCPU, true)
  for (i <- 0 to lastPointerFound) {
    poke(c.io.peelScr.ffastOutRIdx, i)
    step(subsamplingT * 5)
    peek(c.io.peelScr.ffastOutVal)
    ffastBins += peek(c.io.peelScr.ffastOutBin)
  }

  val ffastOutBins = ffastBins.toSeq
  val falsePositives = ffastOutBins.diff(fftLargeOutIdxs)
  val falseNegatives = fftLargeOutIdxs.diff(ffastOutBins)
  val foundCorrect = fftLargeOutIdxs.intersect(ffastOutBins)
  if (falsePositives.length != 0)
    println(s"FAILED: False positive locations (${falsePositives.length}): " + falsePositives.mkString(", "))
  else
    println("No false positives! :)")
  if (falseNegatives.length != 0)
    println(s"FAILED: False negative locations (${falseNegatives.length}): " + falseNegatives.mkString(", "))
  else
    println("No false negatives! :)")

  println(s"Correctly found (${foundCorrect.length}): " + foundCorrect.mkString(", "))

  cycleThroughUntil("ADCCollectDebug")
  clearResults()

  val out = FFTTestVectors.createOutput(inLarge).map(x => x / c.ffastParams.fftn)
  PlotFFT(out)

}