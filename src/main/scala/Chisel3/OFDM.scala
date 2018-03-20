package dspblocks.fft

import breeze.math._
import breeze.linalg._
import breeze.signal._
import breeze.numerics._
import org.scalatest.{FlatSpec, Matchers}

import dsptools.numbers._
import dsptools.numbers.implicits._
import chisel3._
import chisel3.experimental._
import dsptools.{DspTester, DspTesterOptionsManager}
import chisel3.util._
import barstools.tapeout.TestParams

import dsptools.{DspTesterOptions}
import chisel3.iotesters.TesterOptions

object QAM16 {
  val map = Seq(
    Complex(-3, -3),
    Complex(-3, -1),
    Complex(-3, 3),
    Complex(-3, 1),
    Complex(-1, -3),
    Complex(-1, -1),
    Complex(-1, 3),
    Complex(-1, 1),
    Complex(3, -3),
    Complex(3, -1),
    Complex(3, 3),
    Complex(3, 1),
    Complex(1, -3),
    Complex(1, -1),
    Complex(1, 3),
    Complex(1, 1)
  )
  val reverseMap = map.zipWithIndex.toMap
  def mod(data: Int): Complex = {
    map(data)
  }
  def pull(data: Double): Double = {
    data match {
      case x if x <= -2 => -3
      case x if x > -2 && x <= 0 => -1
      case x if x > 0 && x <= 2 => 1
      case _ => 3
    } 
  }
  def demod(data: Complex): Int = {
    val pulledData = Complex(pull(data.real), pull(data.imag))
    reverseMap(pulledData)
  }
}

object Tx {
  def apply(numFrames: Int, fftn: Int, snrsdB: Seq[Int]): Seq[(Seq[Complex], Seq[Int], Double)] = {
    val r = scala.util.Random
    snrsdB map { case snrdB =>
      // 16-QAM
      val bitsSeq = Seq.fill(numFrames)(Seq.fill(fftn)(r.nextInt(16)))
      val symbols = bitsSeq.map { case bits =>
        val symbolsFreq = bits.map(QAM16.mod(_))
        iFourierTr(DenseVector(symbolsFreq.toArray)).toArray.toSeq
      }.flatten
      val snr = math.pow(10, snrdB.toDouble / 10)
      val sigPow = (symbols.map { case x => math.pow(x.abs, 2) }.sum).toDouble / symbols.length
      val sigma2 = sigPow.toDouble / snr
      val sigmaRe = math.sqrt(sigma2.toDouble / 2)
      val noise = Seq.fill(symbols.length)(Complex(sigmaRe * r.nextGaussian(), sigmaRe * r.nextGaussian()))
      // val esn0 = snr / 2
      val esn0 = snr
      val txdata = symbols.zip(noise).map { case (s, n) => s + n }
      (txdata, bitsSeq.flatten, esn0)
    }
  }
}

object Rx {
  def apply(txdata: Seq[Complex], fftn: Int): Seq[Int] = {
    val txdataGrouped = txdata.grouped(fftn)
    val freqData = txdataGrouped.map { case group =>
      fourierTr(DenseVector(group.toArray)).toArray.toSeq
    }.flatten.toSeq
    freqData.map(QAM16.demod(_))
  }
}

object SER {
  def apply(numFrames: Int, fftn: Int, snrsdB: Seq[Int]): Seq[Seq[Double]] = {
    val txOut = Tx(numFrames, fftn, snrsdB)
    val idealResult = txOut.zipWithIndex.map { case ((txdata, txBits, esn0), idx) => 
      val qarg = math.sqrt(3 * esn0.toDouble / (16 - 1))
      val q = 0.5 * erfc(qarg.toDouble / math.sqrt(2))
      val psc = 2 * (1 - 1.toDouble / math.sqrt(16)) * q
      val theorySer = 1 - math.pow(1 - psc, 2)
      val rxBits = Rx(txdata, fftn)
      val numErrors = rxBits.zip(txBits).map { case (rx, tx) => 
        if (rx == tx) 0
        else 1
      }.sum
      val realSer = numErrors.toDouble / txdata.length
      (snrsdB(idx), theorySer, realSer)
    }

    val insLong = txOut.map(_._1).flatten

    val realADCBits = BigInt(math.round(insLong.map { case x => math.abs(x.real) }.max)).bitLength
    val imagADCBits = BigInt(math.round(insLong.map { case x => math.abs(x.imag) }.max)).bitLength
    println(s"Real ADC Bits: $realADCBits")
    println(s"Imag ADC Bits: $imagADCBits")
    val worstADCBits = Seq(realADCBits, imagADCBits).max

    val realFFTBits = realADCBits + BigInt(fftn - 1).bitLength
    val imagFFTBits = imagADCBits + BigInt(fftn - 1).bitLength
    val worstFFTBits = Seq(realFFTBits, imagFFTBits).max

    def runChisel[T <: Data:RealBits](genADC: => T, genFFT: => T): Seq[Double] = {
      RXResults.storedDemod.clear()
      dsptools.Driver.execute(() => new Receiver(genADC = genADC, genFFT = genFFT, 16, fftn), TestParams2.options1Tol) { c =>
        new RXTester(c, insLong)
      } 

      val chiselRxBits = RXResults.storedDemod.toSeq.grouped(fftn).toSeq.grouped(numFrames).toSeq.map(_.flatten)

      val chiselResult = txOut.zip(chiselRxBits).zipWithIndex.map { case (((_, txBits, _), rxBits), idx) => 
        val numErrors = rxBits.zip(txBits).map { case (rx, tx) => 
          if (rx == tx) 0
          else 1
        }.sum
        val chiselSer = numErrors.toDouble / txBits.length
        chiselSer
      }
      chiselResult
    }  

    val adcNumBits = Seq(8, 12, 16)
    val fftNumBits = Seq(12, 14, 16)

    // Initial: Horizontal: SNR, Vertical: Bits
    // Want column to be Bits + Vertical to be SNR changes
    val fpChiselResults = adcNumBits.map { case adcBits =>
      fftNumBits.map { case fftBits =>
        println(s"ADC Bits: $adcBits , FFT Bits: $fftBits")
        val genADC = FixedPoint(adcBits.W, (adcBits - (worstADCBits + 1)).BP)
        val genFFT = FixedPoint(fftBits.W, (fftBits - (worstFFTBits + 1)).BP)
        runChisel(genADC, genFFT).toList
      }
    }.flatten.transpose

    val chiselResult = runChisel(DspReal(), DspReal())

    // Going down should be SNR changes
    val out = idealResult.zip(chiselResult).zip(fpChiselResults).map { case (((a, b, c), d), e) =>
      Seq(a.toDouble, b, c, d) ++ e
    }
    out foreach {
      println(_)
    }
    out
  }  
}

class SerSpec extends FlatSpec with Matchers {
  behavior of "Ser"
  it should "work" in {
    val snrsdB = 0 until 19 by 2
    val out = SER(numFrames = 200, fftn = 128, snrsdB.toSeq)
  }
}

// 12-16 bits, 8 bit adc: min, max for best allocation

class ADC[T <: Data:RealBits](gen: => T) extends chisel3.Module {
  val io = IO(new Bundle {
    val analogIn = Input(DspReal())
    val digitalOut = Output(gen)
  })
  gen match {
    case _: UInt => throw new Exception("ADC gen should be signed!")
    case _: SInt => io.digitalOut := ShiftRegister(io.analogIn.intPart, 1)
    case f: FixedPoint => io.digitalOut := ShiftRegister(io.analogIn.asFixed(f), 1)
    case _: DspReal => io.digitalOut := ShiftRegister(io.analogIn, 1)
    case _ => throw new Exception("Invalid gen for ADC!")
  }
}

class Demod[T <: Data:RealBits](gen: => T, M: Int) extends chisel3.Module {
  val io = IO(new Bundle {
    val complexIn = Input(DspComplex(gen))
    val bitsOut = Output(UInt(BigInt(M).bitLength.W))
  })
  require(M == 16, "Only 16-QAM supported...")

  val negThree = gen.fromDouble(-3)
  val negOne = gen.fromDouble(-1)
  val one = gen.fromDouble(1)
  val three = gen.fromDouble(3)

  def pull(data: T): T = {
    val res = Wire(gen.cloneType)
    val negTwo = gen.fromDouble(-2)
    val zero = gen.fromDouble(0)
    val two = gen.fromDouble(2)
    when(data <= negTwo) {
      res := negThree
    }.elsewhen(data > negTwo && data <= zero) {
      res := negOne
    }.elsewhen(data > zero && data <= two) {
      res := one
    }.otherwise {
      res := three
    }
    res
  } 
  val pulledIn = Wire(DspComplex(gen.cloneType))
  pulledIn.real := pull(io.complexIn.real)
  pulledIn.imag := pull(io.complexIn.imag)

  when(pulledIn.real === negThree && pulledIn.imag === negThree) {
    io.bitsOut := 0.U
  }.elsewhen(pulledIn.real === negThree && pulledIn.imag === negOne) {
    io.bitsOut := 1.U
  }.elsewhen(pulledIn.real === negThree && pulledIn.imag === three) {
    io.bitsOut := 2.U
  }.elsewhen(pulledIn.real === negThree && pulledIn.imag === one) {
    io.bitsOut := 3.U
  }.elsewhen(pulledIn.real === negOne && pulledIn.imag === negThree) {
    io.bitsOut := 4.U
  }.elsewhen(pulledIn.real === negOne && pulledIn.imag === negOne) {
    io.bitsOut := 5.U
  }.elsewhen(pulledIn.real === negOne && pulledIn.imag === three) {
    io.bitsOut := 6.U
  }.elsewhen(pulledIn.real === negOne && pulledIn.imag === one) {
    io.bitsOut := 7.U
  }.elsewhen(pulledIn.real === three && pulledIn.imag === negThree) {
    io.bitsOut := 8.U
  }.elsewhen(pulledIn.real === three && pulledIn.imag === negOne) {
    io.bitsOut := 9.U
  }.elsewhen(pulledIn.real === three && pulledIn.imag === three) {
    io.bitsOut := 10.U
  }.elsewhen(pulledIn.real === three && pulledIn.imag === one) {
    io.bitsOut := 11.U
  }.elsewhen(pulledIn.real === one && pulledIn.imag === negThree) {
    io.bitsOut := 12.U
  }.elsewhen(pulledIn.real === one && pulledIn.imag === negOne) {
    io.bitsOut := 13.U
  }.elsewhen(pulledIn.real === one && pulledIn.imag === three) {
    io.bitsOut := 14.U
  }.otherwise {
    io.bitsOut := 15.U
  }

}

class FFTIdxToBankAddrLUT(n: Int, fftType: FFTType) extends chisel3.Module {
  val lutConsts = PeelingScheduling.getIOMemBankAddr(n, fftType).map(x => x.getBankAddr)
  val lutName = s"${fftType.serialize}IdxToBankAddr$n"
  val lutMod = Module(new UIntLUT2D(lutName, lutConsts, Seq("bank", "addr"), outputReg = false))
  lutMod.io.clk := clock 
  val io = IO(new Bundle {
    val bank = Output(lutMod.io.dout("bank").cloneType)
    val addr = Output(lutMod.io.dout("addr").cloneType)
    val idx = Input(UInt(BigInt(n - 1).bitLength.W))
  })
  lutMod.io.addr := io.idx
  io.bank := lutMod.io.dout("bank")
  io.addr := lutMod.io.dout("addr")
}

// Reset
// ADCIn0   ADCIn1    ADCIn2
//          ADCOut0   ADCOut1
//          0         1

class Receiver[T <: Data:RealBits, R <: Data:RealBits](genADC: => T, genFFT: => R, val M: Int, val fftn: Int) extends chisel3.Module {
  val io = IO(new Bundle {
    val rst = Input(Bool())
    val analogIn = Input(DspComplex(DspReal()))
    val bitsOut = Output(UInt(BigInt(M).bitLength.W))
    val fftOut = Output(DspComplex(genFFT))
    val adcOut = Output(DspComplex(genADC))
    val outIdx = Output(UInt(BigInt(fftn - 1).bitLength.W))
  })

  val adcR = Module(new ADC(genADC))
  val adcI = Module(new ADC(genADC))
  adcR.io.analogIn := io.analogIn.real
  adcI.io.analogIn := io.analogIn.imag
  io.adcOut.real := adcR.io.digitalOut
  io.adcOut.imag := adcI.io.digitalOut

  // Dummy location
  val inputIncCount = Wire(UInt(BigInt(fftn).bitLength.W))
  withReset(io.rst) {
    inputIncCount := RegEnable(next = inputIncCount + 1.U, init = 0.U, enable = inputIncCount =/= fftn.U) 
  }   
  val difMap = Module(new FFTIdxToBankAddrLUT(fftn, DIF))
  difMap.io.idx := inputIncCount
  val inputBank = difMap.io.bank
  val inputAddr = difMap.io.addr

  val fftParams = PeelingScheduling.getFFTParams(fftn)
  val fftBankLengths = fftParams.mem.bankLengths
  val numBanks = fftBankLengths.length
  val mem = Module(new MemBankInterface(DspComplex(genFFT), fftBankLengths, name = s"dataSRAM_${fftn}"))
  mem.io.clk := clock
  val memOutDelay = mem.moduleDelay
  
  val mod = Module(new SubFFT(
    dspDataType = genFFT,
    fftParams = fftParams,
    parallelPELabels = Seq(0),
    fftType = DIF,
    memOutDelay = memOutDelay
  ))
  mod.io.clk := clock

  val states = Seq("Input", "FFT", "Output").zipWithIndex.map { case (name, idx) => name -> idx.U }.toMap

  val state = withReset(io.rst) { RegInit(init = states("Input")) }

  val startFFT = ShiftRegister(in = inputIncCount === (fftn - 1).U, n = memOutDelay, resetData = false.B, en = true.B)

  when(startFFT) {
    state := states("FFT")
  }.elsewhen(mod.io.stateInfo.done && state === states("FFT")) {
    state := states("Output")
  }

  mod.io.stateInfo.start := startFFT
  mod.io.stateInfo.inState := state === states("FFT")

  val outputIncCount = Wire(UInt(BigInt(fftn).bitLength.W))
  withReset(mod.io.stateInfo.done && mod.io.stateInfo.inState) {
    outputIncCount := RegEnable(next = outputIncCount + 1.U, init = 0.U, enable = outputIncCount =/= fftn.U)  
  }
  io.outIdx := ShiftRegister(outputIncCount, memOutDelay)
  val ditMap = Module(new FFTIdxToBankAddrLUT(fftn, DIT))
  ditMap.io.idx := outputIncCount
  val outputBank = ditMap.io.bank
  val outputAddr = ditMap.io.addr

  for (x <- 0 until numBanks) { 
    mem.io.i(x).din := io.adcOut
    if (x == 0) mem.io.i(0).we := (inputIncCount <= (fftn - 1).U) & (state === states("Input"))
    else mem.io.i(x).we := false.B
    mem.io.i(x).loc.bank := inputBank
    mem.io.i(x).loc.addr := inputAddr

    mem.io.o(x).re := true.B
    mem.io.o(x).loc.bank := outputBank
    mem.io.o(x).loc.addr := outputAddr
  } 

  when(state === states("FFT")) {
    mem.io.i := mod.io.dataToMemory(0)
    mod.io.dataFromMemory(0) <> mem.io.o  
  }

  io.fftOut := mem.io.o(0).dout

  val demod = Module(new Demod(genFFT, M))
  demod.io.complexIn := io.fftOut
  io.bitsOut := demod.io.bitsOut

}

object TestParams2 {
  val testerOptionsGlobal = TesterOptions(
    isVerbose = false,
    displayBase = 16,
    backendName = "verilator"
  )

  val options1Tol = new DspTesterOptionsManager {
    dspTesterOptions = DspTesterOptions(
      fixTolLSBs = 1,
      genVerilogTb = false,
      isVerbose = false)
    testerOptions = testerOptionsGlobal.copy(waveform = None)
    //testerOptions = testerOptionsGlobal.copy(waveform = Some(new java.io.File("test_run_dir/waveform.vcd")))
  }
}  

////////////////////////////////////////////////////////

class RXSpec extends FlatSpec with Matchers {
  behavior of "RX"
  it should "demodulate data" in {
    val insLong = Seq.fill(2)(Seq(Complex(0, 0), Complex(1, -1), Complex(2, -2), Complex(3, -3))).flatten
    dsptools.Driver.execute(() => new Receiver(genADC = DspReal(), genFFT = DspReal(), 16, 128 / 32), TestParams2.options1Tol) { c =>
      new RXTester(c, insLong)
    } should be (true)
  }
}

object RXResults {
  val storedFFT = scala.collection.mutable.ArrayBuffer[Complex]()
  val storedDemod = scala.collection.mutable.ArrayBuffer[Int]()  
}

class RXTester[T <: Data:RealBits, R <: Data:RealBits](c: Receiver[T, R], insLong: Seq[Complex]) extends DspTester(c) {
  val fftn = c.fftn
  val ins = insLong.grouped(fftn)
  ins foreach { case set =>
    set.zipWithIndex foreach { case (in, idx) =>
      poke(c.io.analogIn, in)
      if (idx == 0) poke(c.io.rst, true)
      else if (idx == 1) poke(c.io.rst, false)
      // peek(c.io.adcOut)
      step(1)
    }
    var prevFFTVal = Complex(0, 0)
    var prevBitVal = 0
    while (peek(c.io.outIdx) == 0) {
      prevFFTVal = peek(c.io.fftOut)
      prevBitVal = peek(c.io.bitsOut)
      step(1)
    }

    RXResults.storedDemod += prevBitVal
    RXResults.storedFFT += prevFFTVal

    while(peek(c.io.outIdx) < fftn - 1) {
      RXResults.storedFFT += peek(c.io.fftOut)
      RXResults.storedDemod += peek(c.io.bitsOut)
      step(1)
    }

    RXResults.storedFFT += peek(c.io.fftOut)
    RXResults.storedDemod += peek(c.io.bitsOut)
  }
}