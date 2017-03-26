package dspblocks.fft
import chisel3._
import chisel3.experimental._
import barstools.tapeout.transforms._
import chisel3.util._
import dsptools.{DspTester, DspTesterOptionsManager}
import org.scalatest.{FlatSpec, Matchers}
import barstools.tapeout.TestParams

// TODO: Make runtime reconfigurable

class CalcCtrlIO(fftParams: FactorizationParams) extends Bundle {
  val maxDepth = fftParams.mem.bankLengths.max
  val maxNumBanks = fftParams.mem.maxNumBanks
  // TODO: Rename to maxNumStages
  val maxNumStages = fftParams.calc.maxStages
  val usedRads = fftParams.butterfly.rad
  val stages = fftParams.calc.getStages.head.stages 

  val clk = Input(Clock())
  val stateInfo = new StateTransitionIO
  // Not read on stall
  val re = Output(Bool())
  val we = Output(Bool())
  // TODO: Better way to input! Differentiate between # of banks + # of FFT lanes
  // Banks/addresses for each lane
  val locs = CustomIndexedBundle(
    Seq.fill(maxNumBanks)(new BankAddressBundle(Seq(maxNumBanks - 1, maxDepth - 1)))
  )
  // Not delayed
  val currentStageToTwiddle = Output(UInt(range"[0, $maxNumStages)"))
  val currentRadToBF = new CustomIndexedBundle(usedRads.map(r => r -> Output(Bool())): _*)

  override def cloneType = (new CalcCtrlIO(fftParams)).asInstanceOf[this.type]

  // DEBUG ONLY
  val nNoDelay = CustomIndexedBundle(stages.map(r => Output(UInt(range"[0, $r)"))))

}

class CalcCtrlWrapper(val fftParams: FactorizationParams, val fftType: FFTType, memOutDelay: Int, peDelay: Int) extends Module {
  val mod = Module(new CalcCtrl(fftParams, fftType, memOutDelay, peDelay))
  val io = IO(mod.io.cloneType)
  mod.io.clk := clock
  mod.io.stateInfo <> io.stateInfo
  io.re := mod.io.re 
  io.we := mod.io.we 
  io.locs := mod.io.locs 
  io.currentStageToTwiddle := mod.io.currentStageToTwiddle
  io.currentRadToBF := mod.io.currentRadToBF
  io.nNoDelay := mod.io.nNoDelay
}

@chiselName
class CalcCtrl(fftParams: FactorizationParams, fftType: FFTType, memOutDelay: Int, peDelay: Int) extends Module with DelayTracking {

  val peAndMemOutDly = peDelay + memOutDelay

  val maxDepth = fftParams.mem.bankLengths.max
  val maxNumBanks = fftParams.mem.maxNumBanks
  val usedRads = fftParams.butterfly.rad 
  val maxNumStages = fftParams.calc.maxStages
  val maxStageCount = maxNumStages - 1
  require(fftParams.calc.getStages.length == 1, "Only support 1 FFT at a time now (non reconfigurable)")
  val stages = fftParams.calc.getStages.head.stages
  require(fftParams.mem.addressConstants.length == 1, "Only support 1 FFT at a time now (non reconfigurable)")
  val addressConstants = fftParams.mem.addressConstants.head

  val maxStageCountsOriginal = stages.map(s => (s - 1).U)

  val io = IO(new CalcCtrlIO(fftParams))
  val nToBankAddr = Module(new NToBankAddr(fftParams))

  // TODO: Don't hard code
  // Local to this current module
  val localDelay = 1
  def moduleDelay = nToBankAddr.moduleDelay + localDelay

  withClockAndReset(io.clk, io.stateInfo.start) {

    // Note: When you use in reconfigurable mode (yet to be added back)
    // When you switch calculation-related memories, you switch from DIT/DIF and vice versa.
    // Instead of doing wrapping, you hold the value. DIF 0 to right most -> DIT right-most to 0

    val stageCountEnable = Wire(Bool())
    val stageCount = Wire(UInt(range"[0, $maxNumStages)"))
    // DIF counts (moves from) left to right
    // DIT counts (moves from) right to left
    val stageCountInit = fftType match {
      case DIF => 0.U
      case DIT => maxStageCount.U
    }
    val stageCountIsDone = fftType match {
      case DIF => stageCount === maxStageCount.U
      case DIT => stageCount === 0.U
    }
    // Note: Assumes reset has priority -- will reset regardless of what stage comes next
    // Otherwise, hold when done (enable low)
    val stageCountNext = fftType match {
      case DIF => stageCount + 1.U
      case DIT => stageCount - 1.U
    }
    stageCount := RegEnable(stageCountNext, init = stageCountInit, enable = stageCountEnable)
    val isStageBools = (0 until stages.length).map(idx => idx.U === stageCount)
    val currentRad = Mux1H(isStageBools.zip(stages.map(s => s.U)))
    val currentRadBools = Wire(new CustomIndexedBundle(usedRads.map(r => r -> Bool()): _*))
    currentRadBools.elements.foreach { case (rad, node) => node := currentRad === rad.toInt.U }

    // Get address constant associated with current stage (for parallel inputs)
    val currentAddressConstant = Mux1H(isStageBools.zip(addressConstants.map(ac => ac.U)))
    
    // TODO: Generalize scheduling hack for 2x rad 2
    val (maxStageCountsWithScheduling, currentRadWithScheduling, rad2x2Scheduling) = {
      if (usedRads.contains(2) && usedRads.contains(4) && !usedRads.contains(5)) {
        val currRadIs2 = currentRadBools(2)
        //currRadIs2AndUse4 is always true in the non-reconfigurable case
        // TODO: Bad assumption that 4 is always first (not sufficiently general)
        // TODO: Why do you need noUse5?
        // For FFTNs with both radix 4 and radix 2, when operating in thte radix-2 stage,
        // do 2 radix-2 butterflies in parallel --> half radix 4 counts
        // Count goes to r - 1
        val leftMost4MaxStageCount = Mux(currRadIs2, 1.U, 3.U)
        val maxStageCountsWithScheduling = Seq(leftMost4MaxStageCount) ++ maxStageCountsOriginal.tail
        // When radix-2, have 4 active lanes instead of just 2
        val currentRadWithScheduling = Mux(currRadIs2, 4.U, currentRad)
        (maxStageCountsWithScheduling, currentRadWithScheduling, currRadIs2)
      }
      else 
        (maxStageCountsOriginal, currentRad, false.B)
    }

    // Note that the counts for the current stage are zeroed out, because rather than being used sequentially,
    // they're used in parallel i.e. radix-4 -> 4 inputs/outputs so that the number of butterflies per stage * radix # = N
    // Count = associated radix - 1
    val maxStageCountsUsed = maxStageCountsWithScheduling.zip(isStageBools).map { case (maxStageCount, isStage) => 
      Mux(isStage, 0.U, maxStageCount)
    }

    // TODO: Make programmable (in that case, you need to check the last *used* stage for a particular FFTN)
    val isEndStage = fftType match {
      case DIF => isStageBools.last 
      case DIT => isStageBools.head
    }

    // When pipelining butterfly (pipeline cycles in PE = 2); Data *,** are stale -> don't save results with them
    // ARBF0,stg1       x*             x**      ARBF0,stg2 (earliest possible with guaranteed right data)
    //              DRBF0,stg1        x*          x**             DRBF0,stg2
    //                              Dly1          x*                  x**
    //                                         Dly2 (PE out ok)       x*                   x**
    //                                         AWBF0,stg1,WE          x*,~WE               x**,~WE
    //                                                            DataBF0,stg1_valid in mem
    // When stallCount = # of pipeline delays, n counting is enabled (*)
    // i.e. for N= 3*3:       StallCnt  n1  n2
    // Stage 1:                   2*     0   0 --> n enable
    //                            2*     0   1 --> n enable
    //                            2*     0   2 --> stageChangeCond, n enable
    // Stage 2:                   0      0   0 --> stall (!we)
    //                            1      0   0 --> stall (!we)
    //                            2*     0   0 --> n enable
    //                            2*     1   0 --> n enable
    //                            2*     2   0 --> n enable, stageChangeCond
    // @ the beginning of each stage (not including the first stage), stall address count to flush out previous stage
    // results (don't write stale values). Subsequent stages expect that the current stage has finished calculating,
    // which pipelining messes up. Therefore, you need to stall. When stalling, you hold n counters @ 0.

    val stallCount = Wire(UInt(range"[0, $peAndMemOutDly]"))
    val stallMaxed = stallCount === peAndMemOutDly.U
    val stallCountNext = Mux(stallMaxed, Mux(stageCountEnable, 0.U, stallCount), stallCount + 1.U)
    stallCount := RegEnable(stallCountNext, init = peAndMemOutDly.U, enable = io.stateInfo.inState)

    // First stage, don't stall (= always maxed out counter)
    // @ the start of subsequent stages, 'reset' + enable stall counter (it'll wrap to 0 initially)
    // i.e. keep running if not maxed out. If finishing last stage, hold @ max = don't write to memory.

    val calcCounts = Wire(CustomIndexedBundle(stages.map(r => UInt(range"[0, $r)"))))
    val reachedMaxCounts = maxStageCountsUsed.zipWithIndex.map { case (max, idx) => 
      calcCounts(idx) === max
    }
    // This is ok b/c counts change once per cycle
    // Change if counters to the right have maxed out
    // TODO: State info shouldn't be necessary here?
    val calcCountsEnableLong = reachedMaxCounts.scanRight(stallMaxed & io.stateInfo.inState)((next, accum) => accum & next)
    val calcCountsEnable = calcCountsEnableLong.drop(1)
    // Hold counts if reached max (until stage ends)
    val calcCountsNext = reachedMaxCounts.zipWithIndex.map { case (reachedMax, idx) => 
      Mux(reachedMax, calcCounts(idx), calcCounts(idx) + 1.U)
    }
    // Reset has precedence
    val calcReset = stageCountEnable | io.stateInfo.start

    calcCounts.elements foreach { case (idxS, count) =>
      val idx = idxS.toInt
      count := withClockAndReset(io.clk, calcReset) { 
        RegEnable(calcCountsNext(idx), init = 0.U, enable = calcCountsEnable(idx)) 
      }
    }

    // Change stage when counts all maxed out (hold state when done)
    stageCountEnable := calcCountsEnableLong.head & ~stageCountIsDone 
    // Doesn't go high until the cycle after all counts are maxed out
    val calcDone = RegEnable(true.B, init = false.B, enable = calcCountsEnableLong.head & stageCountIsDone)
   
////////////////////////////////////////////////////// -- No pipeline delay up to this point

    nToBankAddr.io.n := calcCounts
    nToBankAddr.io.clk := io.clk

    // Use address constant associated with the current stage (where the counter is zeroed out -> parallel)
    // to generate the remaining butterfly input addresses
    // Note that the second input has the value of the first input + the coefficient associated with the current stage

    // TODO: Anywhere that there's Wire, remove!
    val currentAddressConstantDelayed = ShiftRegister(currentAddressConstant, nToBankAddr.moduleDelay)
    val calcAddresses = (1 until maxNumBanks).scanLeft(nToBankAddr.io.loc.addr) { case (accum, _) =>
      val out = Wire(UInt(range"[0, $maxDepth)"))
      out := accum +& currentAddressConstantDelayed
      out
    }
    // TODO: NEEDED FOR SCHEDULING HACK. GENERALIZE AND REPLACE!
    val rad2x2SchedulingDelayed = ShiftRegister(rad2x2Scheduling, nToBankAddr.moduleDelay)
    val reScheduledCalcAddresses = calcAddresses.zipWithIndex.map { case (addr, idx) =>
      // To support 2x radix 2 lanes 2, 3 must be active
      // Use same addresses; different banks
      if (idx == 2 | idx == 3) 
        ShiftRegister(Mux(rad2x2SchedulingDelayed, calcAddresses(idx - 2), addr), localDelay)
      else 
        ShiftRegister(addr, localDelay)
    }
    reScheduledCalcAddresses.zipWithIndex foreach { case (addr, idx) =>
      io.locs(idx).addr := addr
    }

    // Bank is just (bank0 + i) % maxRadix for the ith input to the butterfly
    // TODO: Switch to using max radix instead of max # of banks (?)
    // TODO: Make programmable
    val bank0 = nToBankAddr.io.loc.bank
    val calcBanks = Seq(ShiftRegister(bank0, localDelay)) ++ (1 until maxNumBanks).map(i => 
      ShiftRegister(ShortConstantMod(bank0 +& i.U, maxNumBanks, 2 * maxNumBanks - 2), localDelay)
    )
    // TODO: Combine with the above foreach
    calcBanks.zipWithIndex foreach { case (bank, idx) =>
      io.locs(idx).bank := bank
    }

/////////////////////////////
    
    io.stateInfo.skipToEnd := false.B
    // Done after last write: delay through this module + mem read out + butterfly delay
    io.stateInfo.done := ShiftRegister(
      in = calcDone, 
      n = moduleDelay + peAndMemOutDly, 
      resetData = false.B, 
      en = io.stateInfo.inState
    )
    // Align with bank + address
    val re = ShiftRegister(
      in = stallMaxed & ~calcDone, 
      n = moduleDelay,
      resetData = false.B,
      en = io.stateInfo.inState
    )
    io.re := re
    // Not delayed
    io.currentStageToTwiddle := stageCount
    io.currentRadToBF.elements foreach { case (rad, port) =>
      port := ShiftRegister(
        in = currentRadBools(rad.toInt),
        // Match timing to butterfly input
        n = moduleDelay + memOutDelay,
        resetData = false.B,
        en = io.stateInfo.inState
      )
    }
    io.we := ShiftRegister(
      in = re,
      // Match timing to butterfly output
      n = peAndMemOutDly,
      resetData = false.B,
      en = io.stateInfo.inState
    )

    // DEBUG
    io.nNoDelay.elements foreach { case (idxS, port) =>  
      val idx = idxS.toInt
      port := calcCounts(idx)
    }

  }
}

class CalcCtrlSpec extends FlatSpec with Matchers {
  behavior of "CalcCtrl"
  it should "generate correct calculation banks + addresses" in {
    // TODO: Change name of PeelingScheduling
    dsptools.Driver.execute(() =>
      new CalcCtrlWrapper(
        fftParams = PeelingScheduling.getFFTParams(24),
        fftType = DIF, 
        memOutDelay = 1, 
        peDelay = 3                 // in + 1 constant multiply + 1 twiddle multiply
      ), TestParams.options0Tol
    ) { c =>
      new CalcCtrlTester(c)
    } should be (true)
  }
}











class CalcCtrlTester(c: CalcCtrlWrapper) extends DspTester(c) {
  require(c.fftParams.calc.getStages.length == 1)
  val stages = c.fftParams.calc.getStages.head.stages
  val addressConstants = fftParams.mem.addressConstants.head

  val nExpected = for ((s, idx) <- stages.zipWithIndex) yield {
    // Max stage count associated with current stage is zeroed
    val stagesNew = stages.updated(idx, 1)
    // TODO: Generalize! -- now assumes 4 is left-most
    val stagesHack = 
      if (s == 2 && stages.contains(4) && !stages.contains(5)) stagesNew.updated(0, 4)
      else stagesNew
    val stagesReversed = stagesHack.reverse
    val stageCountFlipped = stagesReversed.zipWithIndex.map { case (rad, stageIdx) => 
      val origRightProduct = if (stageIdx == 0) 1 else stagesReversed.take(stageIdx).product 
      val origLeftProduct = stagesReversed.drop(stageIdx + 1).product
      Seq.fill(origLeftProduct)(
        (0 until rad).map(Seq.fill(origRightProduct)(_)).flatten
      ).flatten
    }.transpose
    stageCountFlipped.map(row => row.reverse)
  }

  // TODO: Minimize copy-pasta from FW_BinToBankAddrMap

  val (bank0, addr0) = nExpected.zipWithIndex.map { case (row, idx) =>
    val addr = row.zip(addressConstants).map { case (n, ac) => n * ac }.sum
    val bank = row.sum % maxNumBanks
    (bank, addr)
  }.unzip

  val banks = bank0.map { case bank0Row => 
    Seq(bank0Row) ++ (1 until maxNumBanks).map(idx => (bank0Row + idx) % maxNumBanks)
  }



  






  nExpected foreach { x => 
    println("tttt")
    x foreach { y => println(y.mkString(",")) } }

// how many low





  
  // no we immediately before/after are same

}







// TODO: Try DIT





// global: wait until all counts end for all FFTs
