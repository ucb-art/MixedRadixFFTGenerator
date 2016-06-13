package FFT
import ChiselDSP._
import Chisel.{Pipe =>_,Complex => _,Mux => _, RegInit => _, RegNext => _, Counter => _, _}

class CalcCtrlI extends IOBundle {
  val enable = DSPBool(INPUT)
}

class CalcCtrlO extends IOBundle {

  val numBanks = Params.getMem.banks
  val addrMax = Params.getMem.lengths.max-1

  // Banks used for current butterfly
  val banks = Vec(numBanks,DSPUInt(OUTPUT,numBanks-1))
  // Memory addresses (associated with used banks)
  val addrs = Vec(numBanks,DSPUInt(OUTPUT,addrMax))
}

class CalcCtrlFlags extends IOBundle{

  val usedRads = Params.getBF.rad

  // Current radix -- for each used radix, true indicates it's the current one, false indicates it's not
  val currRad = Vec(usedRads.length,DSPBool(OUTPUT))
  // Current calculation stage
  val currStage = DSPUInt(OUTPUT,Params.getCalc.maxStages-1)
  // When calculation + twiddle counters need to be reset (note, different from IO, because IO wraps naturally --
  // calculation must wait for IO to wrap to reset)
  val reset = DSPBool(OUTPUT)
  // Calculating in DIT mode
  val isDIT = DSPBool(OUTPUT)
  // When stalling, don't write to memory
  val we = DSPBool(OUTPUT)
}

class CalcCtrl extends DSPModule {

  val intDly = Params.getDelays.calcTop
  val PEandMemOutRegDly = Params.getDelays.pe + Params.getDelays.memOutRegDly

  // Reset (precedence over IO enable), enable
  val ioCtrl = new IOCtrlIO
  // radStageSum (last used), addrConstants, stageRad (radix for each stage), maxRad used
  // rightMostStageIdx, stageMaxCount
  val generalSetup = (new GeneralSetupO).flip
  // wrapCond on IO enable, isDIF, isMemB
  val ioFlags = (new IOFlagsO).flip
  val calcCtrlI = new CalcCtrlI

  val o = new CalcCtrlO
  val calcFlagsNoDelay = new CalcCtrlFlags

  // Is calculation in DIT mode?
  // B_DIFIO A_DITIO B_DITIO A_DIFIO B_DIFIO
  // A_DIFCA B_DIFCA A_DITCA B_DITCA A_DIFCA
  // Transitions when IO Mem A -> B
  val calcDIT = RegInit(DSPBool(true))
  val calcDITTemp = Mux(ioFlags.wrapCond & !ioFlags.isMemB,!calcDIT,calcDIT)
  calcDIT := !ioCtrl.reset & calcDITTemp
  calcFlagsNoDelay.isDIT := calcDIT

  // Calculation reset = IO reset or IO is wrapping (calculation can finish early, whereby counters just hold their
  // value
  val calcReset = ioCtrl.reset | ioFlags.wrapCond
  calcFlagsNoDelay.reset := calcReset

  // (Local) Stage Counter
  class StageCounter extends Counter(CountParams(
    countMax = Params.getCalc.maxStages-1,
    wrapCtrl = External,
    countType = UpDown,
    customWrap = true
  ))
  val stageCounter = DSPModule(new StageCounter,"stageCounter")
  // TODO: Do I need this cloneType?
  val currentStage = stageCounter.io.out.cloneType
  currentStage := stageCounter.io.out
  calcFlagsNoDelay.currStage := currentStage

  // TODO: Implement more efficiently
  // Current radix; also represented as a list of bools
  val usedRad = Params.getBF.rad
  val currRad = generalSetup.stageRad.zipWithIndex.foldLeft(DSPUInt(0))((accum,e) => {
    (e._1 ? (currentStage === DSPUInt(e._2))) | accum
  })
  calcFlagsNoDelay.currRad := Vec(usedRad.map(x => currRad === DSPUInt(x)))

  // Address constant associated with current stage
  val currAddrConstant = generalSetup.addrConstants.zipWithIndex.foldLeft(DSPUInt(0))((accum,e) => {
    (e._1 ? (currentStage === DSPUInt(e._2))) | accum
  })

// TODO: NEEDED FOR SCHEDULING HACK. GENERALIZE AND REPLACE

  val maxStageCountWithScheduling = {
    // TODO: Generalize
    if (usedRad.contains(2) && usedRad.contains(4)){
      val idx2 = usedRad.indexOf(2)
      val currRad2 = calcFlagsNoDelay.currRad(idx2)
      val currRad2AndUse4 = currRad2 & generalSetup.use4
      val noUse5 = !generalSetup.use5
      // TODO: Generalize -- bad assumption = 4 is always first (true before scheduling)
      // Also, not sure why noUse5 is needed (meets conditions for the two N's that went over 2x clock cycles)
      // For FFTNs with both radix 4 and radix 2, when operating in the radix-2 stage,
      // want to do 2 radix-2 butterflies in parallel --> halve radix 4 counts
      val leftMost4MaxStageCount = Mux(currRad2AndUse4 & noUse5,DSPUInt(1),generalSetup.stageMaxCount.head)
      Vec(List(leftMost4MaxStageCount) ++ generalSetup.stageMaxCount.tail)
    }
    else generalSetup.stageMaxCount
  }

// END SCHEDULING HACK

  // Note that the counts for the current stage are zeroed out, because rather than being used sequentially,
  // they're used in parallel i.e. radix-4 -> 4 inputs/outputs so that the number of butterflies per stage * radix # = N
  // Count = associated radix - 1
  val maxStageCountUsed = Vec(maxStageCountWithScheduling.zipWithIndex.map(x => {
    val isNotCurrentStage = DSPUInt(x._2) =/= currentStage
    x._1 ? isNotCurrentStage
  }))

  val isRightMostStage = (currentStage === generalSetup.rightMostStageIdx)
  val isLeftMostStage = (currentStage === DSPUInt(0))

  // Stage counters
  class CalcCounter extends Counter(CountParams(countMax = Params.getBF.rad.max-1))
  val calcCounters = maxStageCountUsed.zipWithIndex.map{case (max,i) => {
    val counter = DSPModule(new CalcCounter,"calcCounters_" + i)
    counter.io.max.get := max
    counter.iCtrl.reset := calcReset
    counter
  }}

  // Change stage after all calc. counters have maxed out
  // Note: original change condition carried through chain
  // TODO: Tree style rather than chain?
  val stageChangeCond = calcCounters.head.oCtrl.change.get

  // Done with calculation when you're at the left-most stage (DIT) / right-most stage (DIF) and your n counters
  // have maxed out
  // Note that calcDoneChangeCond should go high on last clock cycle associated with last stage so that
  // calcCounters get reset to 0 on the next clock, but should stay 0 until the next phase starts
  val calcDoneChangeCond = Mux(calcDIT,isLeftMostStage,isRightMostStage) & stageChangeCond

  val calcDoneFlag = RegInit(DSPBool(true))
  // Should be high after last butterfly calculation is performed until new DIT/DIF calculation phase starts
  // (reset has precedence)
  calcDoneFlag := !calcReset & (calcDoneChangeCond | (!calcDoneChangeCond & calcDoneFlag))

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

  class StallCounter extends Counter(CountParams(
    countMax = PEandMemOutRegDly,
    resetVal = PEandMemOutRegDly
  ))
  val stallCounter = DSPModule (new StallCounter, "stallCounter")
  val stallCount = stallCounter.io.out.cloneType
  stallCount := stallCounter.io.out
  // Note: this DOESN'T contain enable
  // TODO: Pull out of Counter (duplicated)
  val stallWrapCond = (stallCount === DSPUInt(PEandMemOutRegDly))
  // First stage, don't stall (= always maxed out counter)
  // @ the start of subsequent stages, 'reset' + enable stall counter (it'll wrap to 0 initially)
  // i.e. keep running if not maxed out. If finishing last stage, hold @ max = don't write to memory.
  stallCounter.iCtrl.reset := ioCtrl.reset
  stallCounter.io.max.get := DSPUInt(PEandMemOutRegDly)
  val keepStalling = !stallWrapCond & calcCtrlI.enable
  // Note: stageChangeCond includes enable (reset on all subsequent stages except for last stage)
  val resetStallCounter = stageChangeCond & !(calcDoneChangeCond | calcDoneFlag)
  stallCounter.iCtrl.change.get := keepStalling | resetStallCounter

  // Calculation keeps chugging along if not done + enabled + not stalling --> calculation output
  // keeps getting written to memory
  calcFlagsNoDelay.we := stallWrapCond & (!calcDoneFlag) & calcCtrlI.enable

  val n = Vec(calcCounters.map(counter => counter.io.out.cloneType))
  calcCounters.zipWithIndex.foreach{case (counter,i) => {
    if (counter == calcCounters.last) counter.iCtrl.change.get := calcFlagsNoDelay.we
    // each counter changes when the counters to the right wrap
    else counter.iCtrl.change.get := calcCounters(i+1).oCtrl.change.get
    n(i) := counter.io.out
  }}

  // In order of precedence
  // On reset --> calculation in DIF (start at stage 0)
  stageCounter.iCtrl.reset := ioCtrl.reset
  // Stage change condition includes io enable, only enabled when calculation unfinished (allows for wrap @ the end)
  stageCounter.iCtrl.change.get := stageChangeCond
  // for DIT wrap when you're @ the left-most stage; for DIF wrap when you're @ the right-most stage
  stageCounter.iCtrl.wrap.get := Mux(calcDIT,isLeftMostStage,isRightMostStage)
  // When calcMem B -> A (ioMem A -> B), you switch from DIT/DIF and vice-versa. In such a case, instead of "wrapping",
  // you hold the value i.e. DIF 0 to right-most --> DIT right-most to 0
  // Otherwise, wrapping: DIT --> right-most; DIF --> 0
  stageCounter.io.wrapTo.get := Mux(ioFlags.isMemB,Mux(calcDIT,generalSetup.rightMostStageIdx,DSPUInt(0)),currentStage)
  // TODO: Make counter less annoying
  // DIT -> count down; DIF -> count up
  stageCounter.io.upDown.get := calcDIT

// UP TO THIS POINT, NO PIPELINE DELAY -----------------------------------------

  val nToAddrBank = DSPModule(new nToAddrBank)
  nToAddrBank.io.n := n
  nToAddrBank.generalSetup <> generalSetup

  // Use address constant associated with current stage (where counter is zeroed out -> parallel)
  // to generate the remaining butterfly input addresses.
  // Note that the second input has the value of the first input + the coefficient associated with the
  // current stage
  val currStageAddrConstant = currAddrConstant.pipe(nToAddrBank.delay)
  val calcAddr = Vec((0 until o.banks.length-1).scanLeft(nToAddrBank.io.addr)((accum,e) => {
    (accum + currStageAddrConstant).shorten(o.addrMax)
  }))
  o.addrs := Pipe(calcAddr,intDly)

  // Bank is just (bank0 + i)%maxRadix for the ith input to butterfly
  val bank0 = nToAddrBank.io.bank
  val calcBank = Vec(List(bank0) ++ (1 until o.banks.length).map(i => Mod(bank0 + DSPUInt(i),generalSetup.maxRad)._1))
  o.banks := Pipe(calcBank,intDly)

  val delay = intDly + nToAddrBank.delay
  Status("Calc Control delay: " + delay)
  Status("Stall cycles per stage: " + PEandMemOutRegDly)

}