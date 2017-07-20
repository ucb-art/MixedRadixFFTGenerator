package dspblocks.fft
import org.scalatest.{FlatSpec, Matchers}
import dsptools.numbers._

// TODO: Support number of PEs > any stage radix (i.e. 8 instead of 4/2 for FFT = 4 * 2 * 3)
// Note that this is likely going to make banking overkill...
class BankSpec extends FlatSpec with Matchers {

  case class PETests(fft: Int, numPEs: Int)

  val tests = Seq(
    PETests(fft = 4 * 4 * 3, numPEs = 4), // NOT OK
    PETests(fft = 4 * 2 * 3, numPEs = 2), // NOT OK
    PETests(fft = 4 * 3 * 3, numPEs = 3), // NOT OK
    PETests(fft = 2 * 3 * 3, numPEs = 3), // OK 2 3 3

    PETests(fft = 5 * 5 * 3, numPEs = 5), // OK 3 5 5
    PETests(fft = 5 * 3 * 3, numPEs = 3), // OK 3 3 5

    PETests(fft = 5 * 4 * 4, numPEs = 4), // OK -- right most count 4 4 5
    PETests(fft = 5 * 4 * 2, numPEs = 2), // NOT OK 4 2 5
    PETests(fft = 5 * 5 * 4, numPEs = 5), // OK -- right most count 4 5 5
    PETests(fft = 5 * 5 * 2, numPEs = 5), // OK -- right most count 2 5 5

    PETests(fft = 4 * 4 * 4 * 3, numPEs = 4),






 //   PETests(fft = 5 * 4 * 4, numPEs = 2)  // Doesn't work
   PETests(fft = 4 * 4 * 3, numPEs = 2)      // Doesn't work
  )

  tests foreach { case test => 
    val fftParams = PeelingScheduling.getFFTParams(test.fft)
    val stages = fftParams.calc.getStages.head.stages
    println(s"FFT Stages: ${stages.mkString(", ")}, # PEs: ${test.numPEs}")
    Scheduler.run(DIF, stages, test.numPEs)
  }

}

///////////////////////////////////////////////////////////////////////////////

object Scheduler {

  def run(fftType: FFTType, stages: Seq[Int], numPEs: Int): Unit = {

    val maxRad = stages.max
    val fft = stages.filterNot(_ == 0).product

    val stagesLen = stages.length
    val maxNumBanks = maxRad * numPEs
    val memLen = fft / maxNumBanks

    require(stages.product % maxNumBanks == 0)

    // TODO: Generalize?
    require(numPEs <= stages.max, "# PEs should = the radix of ONE stage.")
    // For FFT = 4 * 2 * 3, # PE's can't be 4 or 3 but can be 2. For FFT = 3 * 3 * 2, # PE's can't be 2 but can be 3. 
    require(stages.filter(_ % numPEs == 0).length >= 2, "# PEs should be redundant in stages radices.")

    // Calculates address constants (after evenly dividing out # of banks, the
    // length of each bank is smaller i.e. N/#banks)
    val numBanksFactorizedPow = FactorizationParams(maxNumBanks).flatten
    require(numBanksFactorizedPow.filterNot(_ == 0).length <= 2, "Minimize # of PEs -- less general for now.")

    val radicesUsedForBankingTemp = numBanksFactorizedPow.zip(WFTA.groupedValidRad.flatten).map { 
      case (pow, rad) => Seq.fill(pow)(rad)
    }.flatten
    val stagesUnusedInBanking = stages.diff(radicesUsedForBankingTemp)
    val bankingUnusedInStages = radicesUsedForBankingTemp.diff(stages)
    val bankingUsedInStages = stages.intersect(radicesUsedForBankingTemp)
    
    // TODO: NOTE: The following code doesn't seem to work :( 
    // Collect stages that can be mapped to banking radices (i.e. rad 2 mapped to stage 4)
    // --> If 2 PE's are used for 4 * 4 * 3 FFT, banking stages should be 4, 4 since no radix 2 stage is used
    // Max on left
    val stageRadicesUsedForBanking = bankingUnusedInStages.foldLeft(
      (bankingUsedInStages.zip(Seq.fill(bankingUsedInStages.length)(1)), stagesUnusedInBanking.sorted.reverse)
    ) { case (accum, r) =>
      val maxStageDivisibleByR = accum._2.find(_ % r == 0).get
      (accum._1 :+ (maxStageDivisibleByR, maxStageDivisibleByR / r), accum._2.diff(Seq(maxStageDivisibleByR)))
    }._1












    //require(stages.contains(numPEs), "# PEs must equal a valid stage radix")







    






    // Set the first occurrence of a radix associated with banking in the list of stage radices to 1 (or more generally stage / bank radix)
    // (essentially tell it to skip over the stage when calculating address constants -- just pass value on the right through)
    // Ax = A(x+1) * r(x+1)
    val stagesMod = stageRadicesUsedForBanking.foldLeft(stages) { case (acc, (stage, divAmount)) => 
      acc.updated(acc.indexOf(stage), divAmount)
    }
    println(s"Stages Modified: ${stagesMod.mkString(", ")}")

    // Determines # of used stages, and trims list -- if no 0, then all stages are in use
    val numUsedStagesTemp = stages.indexOf(0)
    val numUsedStages = if (numUsedStagesTemp == -1) stagesLen else numUsedStagesTemp
    val usedStagesMod = stagesMod.dropRight(stagesMod.length - numUsedStages)
    val addressConstantTemp = usedStagesMod.tail.scanRight(1)((radix, ac) => ac * radix)
      
    // Zero out AC associated w/ bank radix stages
    val addressConstantsShort = addressConstantTemp.zip(usedStagesMod).map { case (ac, stage) => 
      if (stage == 1) 0 else ac
    }

// if doesn;t use stages mod -> divide externally. do on left most instead of right most for ac constant
























    
    // Pad back to max # of stages
    val addressConstants =  addressConstantsShort.padTo(stagesLen, 0)
    println(s"Address constants: ${addressConstants.mkString(", ")}")

    case class CalcCtrlTests(
      n: Seq[Int] = Seq.empty,
      rad: Int = 0,
      stageNum: Int = 0,
      bank: Seq[Int] = Seq.empty,
      addr: Seq[Int] = Seq.empty
    ) {
      def print() = 
        println("ns: " + n.mkString(", ") + "\t banks: " + bank.mkString(", ") + "\t addrs: " + addr.mkString(", "))
    }

    val stageVec = fftType match {
      // DIT counts stages in reverse order
      case DIT => stages.zipWithIndex.reverse
      case DIF => stages.zipWithIndex
    }

    // PE counter is essentially mixed radix counter up to # of PEs per stage
    val nsEachStage = for ((srad, idx) <- stageVec) yield {
      // Max stage count associated with current stage is zeroed
      val stagesNew = stages.updated(idx, 1)
      val numPEsPerStage = fft / srad
      for (count <- 0 until numPEsPerStage) yield {
        val tempMixedRadixCount = MixedRadix.toDigitSeqMSDFirst(count, radicesHighFirst = stagesNew)  
        // Pad to correct # of digits
        val mixedRadixCount = Seq.fill(stagesLen - tempMixedRadixCount.length)(0) ++ tempMixedRadixCount
        CalcCtrlTests(
          n = mixedRadixCount,
          rad = srad,
          stageNum = idx
        )
      }
    }

    // For banks, # of PEs multiplied @ index of right-most occurance of the largest radix
    // i.e. FFT = 2 * 3 * 3, 3 PEs in parallel --> bank = [n1 + n2 + 3 * n3] % 9
    // i.e. FFT = 4 * 2 * 3, 2 PEs in parallel --> bank = [2 * n1 + n2 + n3] % 8






    val rightMostIdxOfMaxRad = 
    stages.indexOf(maxRad)
      /*if (stages == Seq(4, 4, 4, 3))
        3
      else
        (stagesLen - 1) - stages.reverse.indexOf(maxRad)
*/








    val bankMul = Seq.fill(stagesLen)(1).updated(rightMostIdxOfMaxRad, numPEs)
    // Incrementally add info
    val testVectors = nsEachStage.map { case stageCounts => 
      for (countInfo <- stageCounts) yield {
        val nCount = countInfo.n
        val currentStageAddressConstant = addressConstants(countInfo.stageNum)
        val addr0 = nCount.zip(addressConstants).map { case (n, ac) => n * ac }.sum
        // Note that #PEs * [0, maxRad) < maxNumBanks!
        val bank0 = nCount.zip(bankMul).map { case (n, m) => n * m }.sum % maxNumBanks
        val banks = Seq(bank0) ++ (1 until countInfo.rad).map { case i => 
          (bank0 + i * bankMul(countInfo.stageNum)) % maxNumBanks
        }
        val addrs = Seq(addr0) ++ (1 until countInfo.rad).map { case i =>
          addr0 + i * currentStageAddressConstant
        }
        countInfo.copy(bank = banks, addr = addrs)
      }
    }

    testVectors foreach { case stage =>
      println(s"Stage: ${stage.head.rad}")
      stage foreach { case count =>
        count.print()
      }
    }

    // All data locations should be exercised (sorted bank, address)
    val completeDataLocs = (0 until maxNumBanks).map { case b =>
      Seq.fill(memLen)(b).zip(0 until memLen)
    }.flatten.sorted

    testVectors foreach { case stage =>
      // Already sorted address first
      val stageDataLocs = stage.map { case count =>
        count.bank.zip(count.addr)
      }.flatten.sorted
      require(completeDataLocs == stageDataLocs, s"This combination doesn't cover all data locations :(. Stage Idx: ${stage.head.stageNum}")
    }

  }

}




/*
4 * 4 * 3 * 3 * 5 * 5 -- try 4, 3, 5

find stride that's guaranteed to work

// Try for all FFT sizes supported!!

// TODO: Count differently (reduce # of counts) -- same address, diff banks

Get rid of right most count [n1 + n2 + 3*n3] % 9 

Pease is better except for reconfigurability

get rid of left count (for radix 2 stg can have >2 in parallel)

doubling up on smaller radix butterlfy is less power and more useful (to meet CC requirements)
need new twiddle

when # PEs = 4 but you actually want 2.... go to next highest


count in diff order then put back into n place


coprime to the right (wrap) if right < PE else 1???



*/