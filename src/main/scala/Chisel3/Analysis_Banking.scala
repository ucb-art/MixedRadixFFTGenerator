package dspblocks.fft
import org.scalatest.{FlatSpec, Matchers}
import dsptools.numbers._

// 1) Check that all expected banks/addresses are used, without redundancies per stage

// TODO: Support number of PEs > any stage radix (i.e. 8 instead of 4/2 for FFT = 4 * 2 * 3)
// Note that this is likely going to make banking overkill...
class BankSpec extends FlatSpec with Matchers {

  case class PETests(fft: Int, numPEs: Int)

  val testsLong = Seq(
    // WFTA currently doesn't support this
    // PETests(fft = 9 * 9 * 2, numPEs = 3),
    // PETests(fft = 9 * 9 * 3 * 2, numPEs = 3),

    // These don't have nice patterns where you can just 0 a count since there aren't 2 of the radix you want to use for # of PEs
    // PETests(fft = 4 * 3, numPEs = 3),
    // PETests(fft = 5 * 4 * 3, numPEs = 1),
    // PETests(fft = 4 * 3 * 5, numPEs = 2),
    // Note: For stage = 5, XOR of n1, n2 = 0 -> even, = 1 -> odd (alternates even/odd); combine adjacent
    // For stages 4/3, combine when n3 is even, odd; combine every other (count by 2 instead of 1 -- easy to do with mixed radix counter!)

    // Note that at least 2 stages must be divisble by numPEs for this to work
    PETests(fft = 4 * 4 * 3, numPEs = 4), 
    PETests(fft = 4 * 2 * 3, numPEs = 2), 
    PETests(fft = 4 * 3 * 3, numPEs = 3), 
    PETests(fft = 2 * 3 * 3, numPEs = 3), 
    PETests(fft = 4 * 3 * 3 * 3, numPEs = 3), 
    PETests(fft = 2 * 3 * 3 * 3, numPEs = 3), 
    PETests(fft = 3 * 5 * 5, numPEs = 5), 
    PETests(fft = 3 * 3 * 5, numPEs = 3),
    PETests(fft = 5 * 3 * 3 * 3, numPEs = 3), 
    PETests(fft = 4 * 4 * 5, numPEs = 4),
    PETests(fft = 4 * 2 * 5, numPEs = 2),
    PETests(fft = 4 * 5 * 5, numPEs = 5), 
    PETests(fft = 2 * 5 * 5, numPEs = 5), 
    PETests(fft = 4 * 4 * 4 * 3, numPEs = 4),
    PETests(fft = 5 * 4 * 4, numPEs = 2),
    PETests(fft = 4 * 4 * 3, numPEs = 2),
    PETests(fft = 5 * 5 * 4 * 3, numPEs = 5)
  )

  val tests = testsLong.slice(0, testsLong.length)

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
    
    // If you ignore this requirement, you can support 12, 60, 300, but with weird scheduling
    // require(stages.filter(_ % numPEs == 0).length >= 2, "# PEs should be redundant in stages radices.")

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

    // Set the first occurrence of a radix associated with banking in the list of stage radices to 1 (or more generally stage / bank radix)
    // (essentially tell it to skip over the stage when calculating address constants -- just pass value on the right through)
    // Ax = A(x+1) * r(x+1)
    val stagesMod = stageRadicesUsedForBanking.foldLeft(stages) { case (acc, (stage, divAmount)) => 
      acc.updated(acc.indexOf(stage), divAmount)
    }

    // Get stage location where divAmount != 1 is used
    // This is used for i.e. FFT = 4 * 4 * 3, PE = 2 (when # PEs isn't a stage radix, but still supported)
    // Only used for address generation, not bank generation
    val countIdxToDivide = stagesMod.zip(stages).map { case (newR, oldR) => (newR != oldR) & (newR != 1) }.indexOf(true)

    // Determines # of used stages, and trims list -- if no 0, then all stages are in use
    val numUsedStagesTemp = stages.indexOf(0)
    val numUsedStages = if (numUsedStagesTemp == -1) stagesLen else numUsedStagesTemp
    val usedStagesMod = stagesMod.dropRight(stagesMod.length - numUsedStages)
    val addressConstantTemp = usedStagesMod.tail.scanRight(1)((radix, ac) => ac * radix)
      
    // Zero out AC associated w/ bank radix stages
    val addressConstantsShort = addressConstantTemp.zip(usedStagesMod).map { case (ac, stage) => 
      if (stage == 1) 0 else ac
    }

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







      // Can also be accomplished by changing the order of the radices when you first decompose via PFA, if not 4 * 2, but 4 * 4 is OK
    // Want the prime associated with # of PEs to appear to the right (least significant mixed-radix digit)
    // Note for # PEs = 4 and FFT = 4 * 4 * 2, need to shift 1 (to have 4 be to the right)
    // This works out nicely if the requirement that # PEs should be redundant in stages radices is met
    // Circular shift to the right of count order!
    val rightMostOccuranceOfNumPEsIdx = 
      if (numPEs % 2 == 0)
        stagesNew.zipWithIndex.reverse.find(_._1 % numPEs == 0).get._2
      else 
        stagesNew.zipWithIndex.find(_._1 % numPEs == 0).get._2





      // 4, 2 weird
      val t = 
        if (rightMostOccuranceOfNumPEsIdx - 1 < 0)

          stages.length - (rightMostOccuranceOfNumPEsIdx - 1)
        else
          rightMostOccuranceOfNumPEsIdx - 1
      val shiftCount = stages.length - (t + 1)











      val stagesShifted = stagesNew.drop(stagesNew.length - shiftCount) ++ stagesNew.dropRight(shiftCount)

      for (count <- 0 until numPEsPerStage) yield {

        val tempMixedRadixCount = 
          if (numPEs == 1)
            MixedRadix.toDigitSeqMSDFirst(count, radicesHighFirst = stagesNew)  
          else 
            MixedRadix.toDigitSeqMSDFirst(count, radicesHighFirst = stagesShifted)  

        // Pad to correct # of digits
        val mixedRadixCountPadded = Seq.fill(stagesLen - tempMixedRadixCount.length)(0) ++ tempMixedRadixCount

        val mixedRadixCount =
          if (numPEs == 1)
            mixedRadixCountPadded
          else
            // Shift back to match IO ordering: must be done after padding!
            mixedRadixCountPadded.drop(shiftCount) ++ mixedRadixCountPadded.dropRight(stagesNew.length - shiftCount)

        CalcCtrlTests(
          n = mixedRadixCount,
          rad = srad,
          stageNum = idx
        )
      }
    }

    // For banks, # of PEs multiplied @ index of left-most occurance of the largest radix
    // i.e. FFT = 2 * 3 * 3, 3 PEs in parallel --> bank = [n1 + 3 * n2 + n3] % 9
    // i.e. FFT = 4 * 2 * 3, 2 PEs in parallel --> bank = [2 * n1 + n2 + n3] % 8
    // Left needed so that all banks + addresses are accessed
    val leftMostIdxOfMaxRad = stages.indexOf(maxRad)
    val bankMul = Seq.fill(stagesLen)(1).updated(leftMostIdxOfMaxRad, numPEs)
    // Incrementally add info
    val testVectors = nsEachStage.map { case stageCounts => 
      for (countInfo <- stageCounts) yield {
        val nCount = countInfo.n
        val currentStageAddressConstant = addressConstants(countInfo.stageNum)
        // Note: For FFT = 9 * 9 * 2, either you can use 9 * (3 * 3) * 2 i.e. count with n1, n2a, n2b, n3
        // which requires banking to be modified to: include 3 * n2a + n2b OR
        // (what is currently done): keeps as 9 * 9 * 2 with n1, n2, n3, except for addrss generation, use floor(n2 / 3) with LUT
        val addr0 = nCount.zip(addressConstants.zipWithIndex).map { case (n, (ac, idx)) => 
          if (idx == countIdxToDivide)
            // FLOOR! -- important to divide n first before multiplying by the address constant
            // Power of 2 is the simplest case (just shift)
            (n / numPEs) * ac
          else 
            n * ac
        }.sum
        // Note that #PEs * [0, maxRad) < maxNumBanks!
        val bank0 = nCount.zip(bankMul).map { case (n, m) => n * m }.sum % maxNumBanks
        val banks = Seq(bank0) ++ (1 until countInfo.rad).map { case i => 
          (bank0 + i * bankMul(countInfo.stageNum)) % maxNumBanks
        }
        val addrs = Seq(addr0) ++ (1 until countInfo.rad).map { case i =>
          // Need to do this here too
          if (countInfo.stageNum == countIdxToDivide)
            addr0 + (i / numPEs) * currentStageAddressConstant
          else
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

Use the most of the smallest radix 




For each stage, split into # PE groups. 0th element of ith group should then be merged -- should have same address but diff banks
Build visualizer
How to modify count to make it work??? -- circular count
Minimize banks! + twiddle factors (i.e. rather do more 3's to bring down cycle count rather than also do for 4's which already don't have that many PEs per stage)







// get original FFT, see if calc too high, if so, split minimum that works....

make sure uniquness still holds for all of LTE (with # calc cycles...)
then properly schedule --> rotate count







 // separate N / current stage into groups of (N/current stage/# PEs)
    //require(stages.contains(numPEs), "# PEs must equal a valid stage radix")
    // group # + 1 together -- check that they're diff (diff bank, same addr)










4 * 4 * 3 * 3 * 5 * 5 -- try 4, 3, 5

// Try for all FFT sizes supported!!

// TODO: Count differently (reduce # of counts) -- same address, diff banks

Get rid of right most count [n1 + n2 + 3*n3] % 9 

Pease is better except for reconfigurability

get rid of left count (for radix 2 stg can have >2 in parallel)

doubling up on smaller radix butterlfy is less power and more useful (to meet CC requirements)
need new twiddle

when # PEs = 4 but you actually want 2.... go to next highest





coprime to the right (wrap) if right < PE else 1???

when # PEs is 2, but can group by 4 -- can do more per stage?



*/