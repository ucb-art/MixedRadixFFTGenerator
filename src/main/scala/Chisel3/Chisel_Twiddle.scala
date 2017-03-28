package dspblocks.fft
import chisel3._
import chisel3.experimental._
import dsptools.numbers._
import dsptools.numbers.implicits._
import barstools.tapeout.transforms._
import chisel3.util._
import barstools.modules._

// TODO: Do all twiddle outputs need to be the same length?
class TwiddleGenIO[T <: Data:RealBits](dspDataType: => T, fftParams: FactorizationParams) extends Bundle {
  
  // TODO: Rename to maxNumStages
  val maxNumStages = fftParams.calc.maxStages
  val maxNumBanks = fftParams.mem.maxNumBanks

  // Not delayed
  val currentStageToTwiddle = Vec(maxNumStages, Input(Bool()))
  val twiddleCountEnable = Input(Bool())
  // stateInfo.start
  val startState = Input(Bool())
  val clk = Input(Clock())

  val twiddles = Output(CustomIndexedBundle(DspComplex(dspDataType), (1 until maxNumBanks)))

  override def cloneType = (new TwiddleGenIO(dspDataType, fftParams)).asInstanceOf[this.type]

}

class TwiddleGen[T <: Data:RealBits](
    dspDataType: => T, 
    fftParams: FactorizationParams, 
    fftType: FFTType, 
    wftaDelay: Int) extends Module with DelayTracking {

  require(fftParams.calc.getStages.length == 1, "Only 1 FFT supported at a time!")
  
  val stagePrimes = fftParams.calc.getStages.head.stagePrimes
  val globalPrime = fftParams.io.globalPrime
  val maxNumBanks = fftParams.mem.maxNumBanks

  // TODO: Don't hard code
  // CalcCtrl (2) + Mem Read Delay (1)
  // Delayed @ TwiddleAddr + final out (x2) -- Look for shift register
  val moduleDelay = 3

  val twiddleTypeTemp = dspDataType match {
    case r: DspReal => r
    case f: FixedPoint =>
      require(f.binaryPoint.known, "Binary point must be known!")
      val fixedWidth = fftParams.twiddle.getTwiddleWidth(f.binaryPoint.get)
      FixedPoint(fixedWidth.W, f.binaryPoint)
  }
  val twiddleType = twiddleTypeTemp.asInstanceOf[T]

  val io = IO(new TwiddleGenIO(twiddleType, fftParams))

  // TODO: Make programmable, rename?
  require(fftParams.twiddle.twiddleCountMax.length == 1, "Only 1 FFT supported at a time")
  val twiddleCountsInt = fftParams.twiddle.twiddleCountMax.head
  val twiddleCounts = twiddleCountsInt.map(c => c.U)
  val twiddleSubCountsInt = fftParams.twiddle.twiddleSubcountMaxPerStage.head
  val twiddleSubCounts = twiddleSubCountsInt.map(c => c.U)
  val twiddleCountMulsInt = fftParams.twiddle.twiddleCountMulPerStage.head
  val twiddleCountMuls = twiddleCountMulsInt.map(c => c.U)

  // Get counter maxes associated with each stage
  val twiddleCountMaxUsed = Mux1H(io.currentStageToTwiddle.zip(twiddleCounts))
  val twiddleSubCountMaxUsed = Mux1H(io.currentStageToTwiddle.zip(twiddleSubCounts))
  val twiddleCountMulsUsed = Mux1H(io.currentStageToTwiddle.zip(twiddleCountMuls))

  withClockAndReset(io.clk, io.startState) {
    val twiddleCountMax = twiddleCountsInt.max
    val twiddleCount = Wire(UInt(range"[0, $twiddleCountMax]"))
    val twiddleCountNext = Mux(twiddleCount === twiddleCountMaxUsed, 0.U, twiddleCount +& 1.U)
    val twiddleCountEnableInt = Wire(Bool())
    twiddleCount := RegEnable(twiddleCountNext, init = 0.U, enable = twiddleCountEnableInt)

    val twiddleSubCountMax = twiddleSubCountsInt.max

    if (twiddleSubCountMax == 0) {
      // Only 1 coprime -> don't need sub counter
      twiddleCountEnableInt := io.twiddleCountEnable
    }
    else {
      val twiddleSubCount = Wire(UInt(range"[0, $twiddleSubCountMax]"))
      // TODO: Maybe make counters again? (Although registers are more descriptive in the code...)
      val twiddleSubCountIsMax = twiddleSubCount === twiddleSubCountMaxUsed
      val twiddleSubCountNext = Mux(twiddleSubCountIsMax, 0.U, twiddleSubCount +& 1.U)
      twiddleSubCount := RegEnable(twiddleSubCountNext, init = 0.U, enable = io.twiddleCountEnable)
      twiddleCountEnableInt := io.twiddleCountEnable & twiddleSubCountIsMax
    }

    val maxTwiddleROMDepth = fftParams.twiddle.maxTwiddleROMDepth
    val twiddleAddr = Wire(UInt(range"[0, $maxTwiddleROMDepth)"))
    twiddleAddr := ShiftRegister(twiddleCount * twiddleCountMulsUsed, 1)

    val primeIdx = stagePrimes.map { case p => 
      val idx = globalPrime.indexOf(p) 
      if (idx == -1) globalPrime.length
      else idx
    }.map(p => p.U)
    val currentPrimeIdx = ShiftRegister(Mux1H(io.currentStageToTwiddle.zip(primeIdx)), 1)

    // Parameters: columns associated with twiddles for 1 until radix, but want to address column first
    val twiddleList = fftParams.twiddle.twiddles.map { case (associatedPrime, twiddles) => 
      associatedPrime -> twiddles.transpose 
    } 
    // TODO: Transposing is confusing

    val twiddleLUTs = twiddleList.map { case (associatedPrime, twiddles) => 
      val laneLUTs = twiddles.zip((1 to twiddles.length)).map { case (laneLUT, laneIdx) => 
        laneIdx -> Module(new ComplexLUT(twiddleType, s"twiddles_${associatedPrime}_${laneIdx}", laneLUT))
      }.toMap
      associatedPrime -> laneLUTs
    }.toMap

    // TODO: Don't hard code
    val assignedTwiddleAddr = fftType match {
      // Multiplication occurs before WFTA
      case DIT => twiddleAddr
      // Multiplication occurs after WFTA
      // TODO: Be consistent on spelling
      case DIF => ShiftRegister(twiddleAddr, wftaDelay)
    }

    val currentPrimeIdxAlignedWithTwiddleAddr = fftType match {
      case DIT => currentPrimeIdx
      case DIF => ShiftRegister(currentPrimeIdx, wftaDelay)
    }

    // All twiddle LUTs associated with a particular coprime share the same address
    // (Already delayed version)
    val primeIdxBools = twiddleLUTs.zipWithIndex.map { case (_, primeIdx) =>
      currentPrimeIdxAlignedWithTwiddleAddr === primeIdx.U
    }

    // TODO: Better way to do this?
    twiddleLUTs.zipWithIndex foreach { case ((associatedPrime, twiddles), primeIdx) => 
      twiddles foreach { case (laneIdx, laneLUT) =>
        laneLUT.io.addr := Mux(primeIdxBools(primeIdx), assignedTwiddleAddr, 0.U)
      }
    }

    // TODO: Do you have to do this?
    val zero = Wire(DspComplex(twiddleType))
    zero.real := Ring[T].zero 
    zero.imag := Ring[T].zero

    val twiddleOutsColsLanes = twiddleLUTs.map { case (associatedPrime, twiddles) => 
      val numColsLanes = twiddles.toSeq.length
      val pad = maxNumBanks - numColsLanes
      twiddles.map { case (laneIdx, laneLUT) =>
        laneLUT.io.dout
      } ++ Seq.fill(pad)(zero)
    }.transpose

    // Lanes indexed first
    val outInternal = twiddleOutsColsLanes.map { case laneCols => 
      Mux1H(
        // laneCols correspond to prime
        laneCols.zipWithIndex.map { case (primeTwOption, primeIdx) => 
          primeIdxBools(primeIdx) -> primeTwOption 
        }
      )
    }

    // Should already be appropriately delayed
    // Note that non-trivial twiddles start at lane 1
    outInternal.zipWithIndex foreach { case (outLane, idx) =>
      val laneIdx = idx + 1
      io.twiddles(laneIdx) := ShiftRegister(outLane, 2) 
    }

  }

}

// TODO: UNIT TEST!!!