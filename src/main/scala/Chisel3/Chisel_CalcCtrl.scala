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
  val re = Vec(maxNumBanks, Output(Bool()))
  val we = Vec(maxNumBanks, Output(Bool()))
  // TODO: Better way to input! Differentiate between # of banks + # of FFT lanes
  // Banks/addresses for each lane
  val rlocs = CustomIndexedBundle(
    Seq.fill(maxNumBanks)(new BankAddressBundle(Seq(maxNumBanks - 1, maxDepth - 1)))
  )
  val wlocs = Output(rlocs.cloneType)
  // Not delayed
  val currentStageToTwiddle = Vec(maxNumStages, Output(Bool()))
  val twiddleCountEnable = Output(Bool())

  // Delayed
  val currentRadToBF = new CustomIndexedBundle(usedRads.map(r => r -> Output(Bool())): _*)

  override def cloneType = (new CalcCtrlIO(fftParams)).asInstanceOf[this.type]

  // DEBUG ONLY
  val nNoDelay = CustomIndexedBundle(stages.map(r => Output(UInt(range"[0, $r)"))))

}

class CalcCtrlWrapper(val fftParams: FactorizationParams, val fftType: FFTType, memOutDelay: Int, wftaDly: Int, twDly: Int) extends Module {
  val mod = Module(new CalcCtrl(fftParams, fftType, memOutDelay, wftaDly, twDly))
  val io = IO(mod.io.cloneType)
  mod.io.clk := clock
  mod.io.stateInfo <> io.stateInfo
  io.re := mod.io.re 
  io.we := mod.io.we 
  io.rlocs := mod.io.rlocs
  io.wlocs := mod.io.wlocs 
  io.currentStageToTwiddle := mod.io.currentStageToTwiddle
  io.currentRadToBF := mod.io.currentRadToBF
  io.nNoDelay := mod.io.nNoDelay
}

@chiselName
class CalcCtrl(fftParams: FactorizationParams, fftType: FFTType, memOutDelay: Int, wftaDly: Int, twDly: Int) extends Module with DelayTracking {

  val peDelay = wftaDly + twDly
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
  val moduleDelay = nToBankAddr.moduleDelay + localDelay

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
    // Hold counts if all counts have reached max (until stage ends) ; otherwise, wrap
    val calcCountsNext = reachedMaxCounts.zipWithIndex.map { case (reachedMax, idx) => 
      // TODO: Probably redundant logic (with reset) -- fix
      Mux(reachedMax, Mux(calcCountsEnableLong.head, calcCounts(idx), 0.U), calcCounts(idx) + 1.U)
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
      io.rlocs(idx).addr := addr
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
      io.rlocs(idx).bank := bank
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

    val reNoDelay = stallMaxed & ~calcDone
    val reInternal = Wire(Vec(maxNumBanks, Bool()))
    reInternal.zipWithIndex foreach { case (lane, idx) => lane := (idx.U < currentRadWithScheduling) & reNoDelay }
    
    // Align with bank + address
    val re = ShiftRegister(
      in = reInternal, 
      n = moduleDelay,
      resetData = Vec(Seq.fill(maxNumBanks)(false.B)),
      en = io.stateInfo.inState
    )
    io.re := re
    // Not delayed
    io.currentStageToTwiddle.zipWithIndex.map { case (port, idx) => port := isStageBools(idx) }
    io.twiddleCountEnable := reNoDelay

    io.currentRadToBF.elements foreach { case (rad, port) =>
      // Goes to WFTA
      val additionalDelay = fftType match {
        case DIT => twDly
        case DIF => 0
      }
      port := ShiftRegister(
        in = currentRadBools(rad.toInt),
        // Match timing to butterfly input
        n = moduleDelay + memOutDelay + additionalDelay,
        resetData = false.B,
        en = io.stateInfo.inState
      )
    }
    io.we := ShiftRegister(
      in = re,
      // Match timing to butterfly output
      n = peAndMemOutDly,
      resetData = Vec(Seq.fill(maxNumBanks)(false.B)),
      en = io.stateInfo.inState
    )

    reScheduledCalcAddresses.zipWithIndex foreach { case (addr, idx) =>
      // Don't care about reset value (since WE is low)
      val out = ShiftRegister(
        in = addr,
        // Match timing to butterfly output
        n = peAndMemOutDly,
        en = io.stateInfo.inState
      )
      io.wlocs(idx).addr := out
    }
    calcBanks.zipWithIndex foreach { case (bank, idx) =>
      // Don't care about reset value (since WE is low)
      val out = ShiftRegister(
        in = bank,
        // Match timing to butterfly output
        n = peAndMemOutDly,
        en = io.stateInfo.inState
      )
      io.wlocs(idx).bank := out
    }

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

    // TODO: Fix concurrency bug
    val ffts = Seq(864)

    for (fft <- ffts) {
      // TODO: Change name of PeelingScheduling
      dsptools.Driver.execute(() =>
        new CalcCtrlWrapper(
          fftParams = PeelingScheduling.getFFTParams(fft),
          fftType = DIT, 
          memOutDelay = 1, 
          wftaDly = 2,
          twDly = 1                                                   // in + 1 constant multiply + 1 twiddle multiply
        ), TestParams.options0TolQuiet
      ) { c =>
        new CalcCtrlTester(c)
      } should be (true)
    }
  }
}

class CalcCtrlTester(c: CalcCtrlWrapper) extends DspTester(c) {

  require(c.fftParams.calc.getStages.length == 1)
  val stages = c.fftParams.calc.getStages.head.stages
  val addressConstants = c.fftParams.mem.addressConstants.head
  val maxNumBanks = c.fftParams.mem.maxNumBanks

  case class CalcCtrlTests(
    n: Seq[Int] = Seq.empty,
    rad: Int = 0,
    stageNum: Int = 0,
    bank: Seq[Int] = Seq.empty,
    addr: Seq[Int] = Seq.empty
  )

  val stageVec = c.fftType match {
    // DIT counts stages in reverse order
    case DIT => stages.zipWithIndex.reverse
    case DIF => stages.zipWithIndex
  }

  val stagedNExpected = for ((srad, idx) <- stageVec) yield {
    // Max stage count associated with current stage is zeroed
    val stagesNew = stages.updated(idx, 1)
    // TODO: Generalize! -- now assumes 4 is left-most
    // Uses half as many serial butterflies for radix 2 stage
    val stagesHack = 
      if (srad == 2 && stages.contains(4) && !stages.contains(5)) stagesNew.updated(0, 2)
      else stagesNew
    // TODO: Do I need reverse?
    val stagesReversed = stagesHack.reverse
    val stageCountFlipped = stagesReversed.zipWithIndex.map { case (rad, stageIdx) => 
      val origRightProduct = if (stageIdx == 0) 1 else stagesReversed.take(stageIdx).product 
      val origLeftProduct = stagesReversed.drop(stageIdx + 1).product
      Seq.fill(origLeftProduct)(
        (0 until rad).map(Seq.fill(origRightProduct)(_)).flatten
      ).flatten
    }.transpose
    stageCountFlipped.map(row => 
      CalcCtrlTests(
        n = row.reverse,
        rad = srad,
        stageNum = idx
      )
    )
  }
  val nExpected = stagedNExpected.flatten

  // TODO: Minimize copy-pasta from FW_BinToBankAddrMap
  // Incrementally add info
  val testVectors = nExpected.map { row =>
    val currentStageAddressConstant = addressConstants(row.stageNum)
    val addr0 = row.n.zip(addressConstants).map { case (n, ac) => n * ac }.sum
    val bank0 = row.n.sum % maxNumBanks
    val usedBankLength = 
      if (row.rad == 2 && stages.contains(4) && !stages.contains(5)) 4
      else row.rad
    val banks = Seq(bank0) ++ (1 until usedBankLength).map(i => (bank0 + i) % maxNumBanks)
    val addrs = Seq(addr0) ++ (1 until row.rad).map(i => addr0 + i * currentStageAddressConstant)
    val addrsNew = 
      // Hack: Repeat address 2x (different banks)
      if (row.rad == 2 && stages.contains(4) && !stages.contains(5))
        addrs ++ addrs 
      else 
        addrs
    row.copy(bank = banks, addr = addrsNew)
  }

  val simTime = testVectors.length + stages.length * c.mod.peAndMemOutDly + c.mod.moduleDelay + 5   // buffer

  // Start state
  reset(10)
  poke(c.io.stateInfo.start, true.B)
  step(1)
  poke(c.io.stateInfo.inState, true.B)
  poke(c.io.stateInfo.start, false.B)

  var weCount = 0
  var reCount = 0
  var stallCount = 0
  var prevRE = false
  var stallStartTime = 0
  var prevDone = false

  // TODO: Better way to input! Differentiate between # of banks + # of FFT lanes (Angie: not sure why I wrote this)
 
  // Note: Didn't check currentStageToTwiddle & currentRadToBF

  for (time <- (0 until simTime)) {

    // Counts are updated after the fact
    val done = peek(c.io.stateInfo.done)
    if (weCount == testVectors.length) 
      expect(done == true, "Done!")
    else
      expect(done == false, "Not done!")

    // TODO: Reduce copy + paste
    // First n should be valid
    val re0 = updatableDspVerbose.withValue(false) { peek(c.io.re(0)) }
    val we0 = updatableDspVerbose.withValue(false) { peek(c.io.we(0)) }
    // Start stall
    if (prevRE && !re0) {
      println(" ********** Started stalling!")
      stallStartTime = t
      stallCount += 1
    }
    // Finish stall
    // Stalling only happens after some amount of reading
    if (!prevRE && (re0 || (done && !prevDone)) && reCount != 0) {
      val stallTime = t - stallStartTime
      expect(stallTime == c.mod.peAndMemOutDly, s"Stall time: $stallTime")
    }
    prevRE = re0
    prevDone = done

    if (re0) {
      val currentTestVector = testVectors(reCount)
      val currRad = currentTestVector.rad
      // Hack
      val currRadNew = 
        if (currRad == 2 && stages.contains(4) && !stages.contains(5)) 4
        else currRad
      currentTestVector.addr.zipWithIndex foreach { case (addr, idx) =>
        expect(c.io.rlocs(idx).addr, addr)
        expect(c.io.rlocs(idx).bank, currentTestVector.bank(idx))
      }
      c.io.re.zipWithIndex foreach { case (re, idx) =>
        if (idx < currRadNew) expect(c.io.re(idx), true.B)
        else expect(c.io.re(idx), false.B)
      }
      reCount += 1 
    }
    if (we0) {
      val currentTestVector = testVectors(weCount)
      val currRad = currentTestVector.rad
      val currRadNew = 
        if (currRad == 2 && stages.contains(4) && !stages.contains(5)) 4
        else currRad
      currentTestVector.addr.zipWithIndex foreach { case (addr, idx) =>
        expect(c.io.wlocs(idx).addr, addr)
        expect(c.io.wlocs(idx).bank, currentTestVector.bank(idx))
      }
      c.io.we.zipWithIndex foreach { case (we, idx) =>
        if (idx < currRadNew) expect(c.io.we(idx), true.B)
        else expect(c.io.we(idx), false.B)
      }
      weCount += 1 
    }
    
    // val x = updatableDspVerbose.withValue(false) { peek(c.io.nNoDelay) }
    step(1)

  }

  require(reCount == testVectors.length, "Read addresses should be complete")
  require(weCount == testVectors.length, "Write addresses should be complete")
  require(stallCount == stages.length, 
    s"Should stall after every stage. Stalled $stallCount times.")

}