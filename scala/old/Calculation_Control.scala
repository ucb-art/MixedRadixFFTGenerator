// Calculation control logic
// September 13, 2015
// Can combine currentStageMax/Min logic

package FFT
import Chisel.{Pipe => _,_}
import calc._
import generator._
import DSP._
import Count._
import memBanks._
import ChiselDSP.{when => _, _}

object calc{
  var maxNumStages = 1
  var stageBranch = Array.empty[Int]  
  var toAddrBankDly = Array(1,1)
}

class calc extends DSPModule {

  val maxRad = Params.getBF.rad.max

  override val io = new  IOBundle {


    val calcMemChangeCond = Bool(INPUT)
    val startFirstFrame = Bool(INPUT)
    val maxStageCount = Vec.fill(maxNumStages){Count(INPUT,maxRad-1)}
    val stageSumM1 = Vec.fill(powColCount){Count(INPUT,maxNumStages-1)}
    val addressConstant = Vec.fill(maxNumStages){Count(INPUT,math.pow(maxRad,maxNumStages-2).toInt)}
    val maxRadix = Count(INPUT,maxRad)
    val stageRadix = Vec.fill(maxNumStages){Count(INPUT,maxRad)}
    val calcBank = Vec.fill(numBanks){Count(OUTPUT,bankMax)}
    val calcAddr = Vec.fill(numBanks){Count(OUTPUT,addrMax)}
    val currentRadix = Count(OUTPUT,maxRad)                                                               // NOTE NOT DELAYED
    val currentStage = Count(OUTPUT,maxNumStages-1)
    val calcMemB = Bool(OUTPUT)
    val calcDoneFlag = Bool(OUTPUT)
    val calcResetCond = Bool(OUTPUT)
    val ioDIT = Bool(OUTPUT)
    val calcDIT = Bool(OUTPUT)
    val discardCalcWrite = Bool(OUTPUT)
    
  }
  
  val calcMemChangeCond = io.calcMemChangeCond
  val startFirstFrame = io.startFirstFrame
  val maxStageCount = io.maxStageCount
  val stageSumM1 = io.stageSumM1
  val addressConstant = io.addressConstant
  val maxRadix = io.maxRadix
  val stageRadix = io.stageRadix

  // Timing diagram: A, B are memory; DIFIO represents normal IO ordering; 
  // DITIO represents bit/ternary,etc. reversed IO ordering;
  // DITCA represents doing decimation in time equivalent calculation (going backwards)
  // DIFCA represents doing decimation in frequency equivalent calculation
  // ^ indicates starting condition

  // A_DIFIO ^ B_DIFIO A_DITIO B_DITIO A_DIFIO B_DIFIO
  // B_DITCA ^ A_DIFCA B_DIFCA A_DITCA B_DITCA A_DIFCA

  // Memory [A,B] changes every symbol
  // Phase [DIT,DIF] changes every other symbol
  
  val calcMemB = Reg(init = Bool(true))
  val calcDIT = Reg(init = Bool(true))
  val ioDIT = Reg(init = Bool(true))

  // Reset to known state (priority) or change 
  when(startFirstFrame){                        
    calcMemB := Bool(false)
  }.otherwise{
    when (calcMemChangeCond){
      calcMemB := ~calcMemB
    }
  }
  // Reset to known state (priority) OR when calculation memory changes from B to A, change calc phase
  when(startFirstFrame){                        
    calcDIT := Bool(false)
  }.otherwise{
    when (calcMemChangeCond & calcMemB){                
      calcDIT := ~calcDIT
    }
  }
  // Reset to known state (priority) OR when calculation memory changes from A to B, change IO phase
  when(startFirstFrame){                        
    ioDIT := Bool(false)
  }.otherwise{
    when (calcMemChangeCond & ~calcMemB){                
      ioDIT := ~ioDIT
    }
  }
  
  // Reset calculation counters
  val calcResetCond = startFirstFrame | calcMemChangeCond       

  // Total "counts" should take you though the whole range of 0 to N-1
  // Note that the counts for the current stage are zeroed out, because rather than being used sequentially,
  // they're used in parallel i.e. radix-4 -> 4 inputs/outputs so that the number of butterflies per stage * radix # = N
  // For N = 4*2*3, stage 1: 6 butterfly calcs * 4 butterfly io per calc = 24
  val currentStage = Reg(init=Count(0,maxNumStages-1))
  val maxStageCountUsed = Vec.fill(maxNumStages){Count(null,maxRad-1)}
  for (i <- 0 until maxNumStages){
    val sel = currentStage === Count(i,maxNumStages-1)
    val zro = Count(0,maxRad-1)
    maxStageCountUsed(i) := muxU(maxStageCount(i),zro,sel)
  }
  
  val currentStageMax = (currentStage === stageSumM1(powColCount-1))     
  val currentStageMin = (currentStage === Count(0,maxNumStages-1))

  val calcn = Vec.fill(maxNumStages){Count(null,maxRad-1)}
  val calcCounters = Vec.fill(maxNumStages){DSPModule( new accumulator(bw(maxRad-1)) ).io}
  val calcCountWrap = Vec.fill(maxNumStages){Bool()}  
  for (i <- 0 until maxNumStages){
    // Wrap after reached max count
    calcCountWrap(i) := (calcn(i) === maxStageCountUsed(i))
  }

  // Change stage after all of the counters have maxed out 
  val stageChangeCond = calcCounters(0).changeCond & calcCountWrap(0)

  // If changing stage and for DIF, you're @ last (right-most) stage or for DIT, you're at left-most stage, you're done with calculations
  // Note that calcDoneChangeCond should go high on last clock cycle associated with last stage so that
  // calcCounters get reset to 0 on the next clock, but should stay 0 until the next phase starts
  val calcDoneChangeCond = ((currentStageMax & ~calcDIT)|(currentStageMin & calcDIT)) & stageChangeCond 
  
  // Should be high after last butterfly calculation is performed until new DIT/DIF phase starts (priority reset)
  val calcDoneFlag = Reg(init = Bool(false))
  when (calcResetCond){
    calcDoneFlag := Bool(false)
  }.elsewhen(calcDoneChangeCond){
    calcDoneFlag := Bool(true)
  }  
  
  // When pipelining butterfly (pipeline cycles = 2); Data *,** are stale -> don't save results with them
  // ARBF0,stg1       x*             x**      ARBF0,stg2
  //              DRBF0,stg1        x*          x**             DRBF0,stg2
  //                              Dly1          x*                  x**
  //                                          Dly2                  x*                   x**
  //                                         AWBF0,stg1,WE          x*,~WE               x**,~WE
  //                                                            DataBF0,stg1_valid
  
  // i.e. for N= 3*3:       StallCnt  n1  n2
  // Stage 1:                   2     0   0
  //                            2     0   1
  //                            2     0   2
  // Stage 2:                   0     0   0
  //                            1     0   0
  //                            2     0   0
  //                            2     1   0
  //                            2     2   0
  //                            2     0   0
  
  // @ the beginning of each stage (not including the first stage), stall address count to flush out previous stage
  // results without writing results using stale values (note subsequent stages expect that the current stage
  // has finished calculation, which is not necessarily true when you're pipelining)
  // -> Hold counters @ 0 for {pipeBFWriteDly+1} cycles, where for the first pipeBFWriteDly cycles, associated
  // WE should be disabled (don't use results from stale data). @ First stage, set stallCount to max = pipeBFWriteDly-->
  // when stallCount is maxed, count change is enabled. 

  val stallCounter = DSPModule( new accumulator(bw(pipeBFWriteDly),pipeBFWriteDly)).io
  val stallCount = stallCounter.out
  val const = Count(pipeBFWriteDly)

  println("sss" + stallCounter.out.getWidth + "," + const.getWidth)

  val stallWrapCond = (stallCount === const)
  // After calculation done, stay at max value until end of first stage
  val stallChangeCond = ~stallWrapCond | (stageChangeCond & ~calcDoneChangeCond & ~calcDoneFlag) 
  stallCounter.inc := Count(1)
  stallCounter.changeCond := stallChangeCond
  stallCounter.globalReset := calcResetCond
  stallCounter.wrapCond := stallWrapCond

  for (i <- maxNumStages-1 to 0 by -1){
    calcCounters(i).inc := Count(1)
    // Don't update counters after last count wraps to 0
    if (i == maxNumStages-1) calcCounters(i).changeCond := ~calcDoneFlag & stallWrapCond
    // Changes when all the counters to the right of the current counter wrap          
    else calcCounters(i).changeCond := calcCounters(i+1).changeCond & calcCountWrap(i+1)   
    calcCounters(i).globalReset := calcResetCond              // Reset counts to 0 when starting new FFT calculation 
    calcCounters(i).wrapCond := calcCountWrap(i)              // Wrap counter when reached max value
    calcn(i) := calcCounters(i).out


    println("getw" + calcn(i).getWidth + "," + calcCounters(i).out.getWidth)
    println("needw" + calcn(i).needWidth + "," + calcCounters(i).out.needWidth)


  }

  when(startFirstFrame){                              // Always start DIF (priority reset)
    currentStage := UInt(0)
  }.otherwise{
    when(stageChangeCond & ~calcDoneFlag){            // Only change stage when calculation isn't done (allow rollover once after last stage)
      // Note all conditions defined
      when(~calcDIT){                                 // DIF
        when(currentStageMax){                        // If final stage of DIF -> setup for next symbol calculation
          when(~calcMemB){                            // When memory A used for calc, you know that the same type of calculation will be performed next cycle
            currentStage := UInt(0)                   // DIF -> reset to 0
          }.otherwise{
            currentStage := currentStage              // Otherwise, when memory B used for calc, if DIT (last min) -> (first min) DIF or DIF (last max) -> (first max) DIT
          }
        }.otherwise{
          currentStage := currentStage + UInt(1)      // DIF = do calculation forwards
        }
      }.otherwise{                                    // DIT
        when(currentStageMin){                        // Final stage of DIT (min) -> setup for next symbol calculation
          when(~calcMemB){                            // Mem A for calculation -> same type of calculation performed next cycle
            currentStage := stageSumM1(powColCount-1) // DIT -> reset to last
          }.otherwise{
            currentStage := currentStage              // Mem B for calculation -> switch type of calculation
          }
        }.otherwise{
          currentStage := currentStage - UInt(1)      // DIT = do calculation backwards
        }
      }
    }
  }

  // n1,n2,n3... -> calc address/banks
  val calcnToBankAddr = Module(new nToBankAddr(toAddrBankDly(0)))
  for (i <- 0 until maxNumStages){
    calcnToBankAddr.io.n(i) := calcn(i)
    calcnToBankAddr.io.addrConstant(i) := addressConstant(i)
  }
  calcnToBankAddr.io.maxRadix := maxRadix    
  
  val calcAddr0 = calcnToBankAddr.io.addr
  val calcBank0 = calcnToBankAddr.io.bank    

  // Determine current butterfly radix NOTE NOT DELAYED
  val currentRadix = stageRadix(currentStage)

  val currentStageAddrConstant = Pipe(addressConstant(currentStage),toAddrBankDly(0)).asInstanceOf[UInt]

  // Use address constant associated with current stage (where counter is zeroed out -> parallel)  
  // to generate the remaining butterfly input addresses. Take ex: 8 = 2*2*2, where n = 4n1 + 2n2 +
  // n3, if you zero-out the counter associated with the current stage, such that for stage 1, you
  // only have n2, n3 counting, then for the first input into butterfly, stg 1, stg 2, stg 3:
  // 0  0  0
  // 1  1  2
  // 2  4  4
  // 3  5  6
  // for the second input,
  // 4  2  1
  // 5  3  3
  // 6  6  5
  // 7  7  7
  // Note that the second input has the value of the first input + the coefficient associated with the
  // current stage (i.e. for stage 1, the second input is the first input value + 4). Although the
  // address constant isnot directly = to [4 2 1] since the 4 is zeroed out to account for banking
  // the logic still applies
  
  val calcAddr = Vec.fill(numBanks){Count(null,addrMax)}
  val calcBank = Vec.fill(numBanks){Count(null,bankMax)}
  for (i <- 0 until numBanks){
    if (i == 0){
      calcAddr(i) := calcAddr0
      calcBank(i) := calcBank0
    }
    else{
      calcAddr(i) := calcAddr(i-1) + currentStageAddrConstant
      val x = add1(calcBank0,Count(i))
      calcBank(i) := mod(x,maxRadix,maxRad)
    }
  }
  io.calcBank := Pipe(calcBank,toAddrBankDly(1)).asInstanceOf[Vec[UInt]]
  io.calcAddr := Pipe(calcAddr,toAddrBankDly(1)).asInstanceOf[Vec[UInt]]
  io.calcMemB :=Pipe(calcMemB,toAddrBankDly.sum).asInstanceOf[Bool]
  io.ioDIT := Pipe(ioDIT,toAddrBankDly.sum).asInstanceOf[Bool]
  
  // Need to be delayed externally
  io.calcDIT := calcDIT
  io.currentRadix := currentRadix
  io.currentStage := currentStage
  io.calcResetCond := calcResetCond
  io.calcDoneFlag := calcDoneFlag
  io.discardCalcWrite := ~stallWrapCond                            // Flag to disabled WE
  
}
