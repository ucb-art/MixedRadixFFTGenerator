package dspblocks.fft
import org.scalatest.{FlatSpec, Matchers}
import dsptools.numbers._
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._
import com.gilt.handlebars.scala.binding.dynamic._
// import com.gilt.handlebars.scala.Handlebars
import scala.io.Source

case class ScheduledCalcParams(
  fftn: Int,
  stageInfo: Seq[StageInfo],
  maxNumParallelPEs: Int,
  maxNumBanks: Int,
  memLen: Int,
  maxRad: Int
)
case class StageInfo(
  rad: Int,
  n: Seq[Seq[Int]],
  bank: Seq[Seq[Int]],
  addr: Seq[Seq[Int]],
  shiftCount: Int
) {
  def toJSONTuples = ("rad" -> rad) ~ ("n" -> n) ~ ("bank" -> bank)
}

// 1) Check that all expected banks/addresses are used, without redundancies per stage

// TODO: Support number of PEs > any stage radix (i.e. 8 instead of 4/2 for FFT = 4 * 2 * 3)
// Note that this is likely going to make banking overkill...
class BankSpec extends FlatSpec with Matchers {

  case class PETests(fft: Int, numPEs: Int)

  val testsLong = Seq(
    PETests(fft = 9 * 9 * 2, numPEs = 1),
    PETests(fft = 9 * 9 * 3 * 2, numPEs = 1),
    PETests(fft = 4 * 3, numPEs = 1),
    PETests(fft = 5 * 4 * 3, numPEs = 1),
    PETests(fft = 4 * 3 * 5, numPEs = 1),
    PETests(fft = 4 * 4 * 3, numPEs = 1), 
    PETests(fft = 4 * 3 * 3, numPEs = 1), 
    PETests(fft = 2 * 3 * 3, numPEs = 1), 
    PETests(fft = 4 * 3 * 3 * 3, numPEs = 1), 
    PETests(fft = 2 * 3 * 3 * 3, numPEs = 1), 
    PETests(fft = 3 * 5 * 5, numPEs = 1), 
    PETests(fft = 3 * 3 * 5, numPEs = 1),
    PETests(fft = 5 * 3 * 3 * 3, numPEs = 1), 
    PETests(fft = 4 * 4 * 5, numPEs = 1),
    PETests(fft = 4 * 5 * 5, numPEs = 1), 
    PETests(fft = 2 * 5 * 5, numPEs = 1), 
    PETests(fft = 4 * 4 * 4 * 3, numPEs = 1),
    PETests(fft = 5 * 5 * 4 * 3, numPEs = 1),
    PETests(fft = 4 * 2 * 3, numPEs = 1), 
    PETests(fft = 4 * 2 * 5, numPEs = 1),
    PETests(fft = 5 * 4 * 4, numPEs = 1),
    PETests(fft = 4 * 4 * 3, numPEs = 1),

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
    PETests(fft = 4 * 3 * 3, numPEs = 3), 
    PETests(fft = 2 * 3 * 3, numPEs = 3), 
    PETests(fft = 4 * 3 * 3 * 3, numPEs = 3), 
    PETests(fft = 2 * 3 * 3 * 3, numPEs = 3), 
    PETests(fft = 3 * 5 * 5, numPEs = 5), 
    PETests(fft = 3 * 3 * 5, numPEs = 3),
    PETests(fft = 5 * 3 * 3 * 3, numPEs = 3), 
    PETests(fft = 4 * 4 * 5, numPEs = 4),
    
    PETests(fft = 4 * 5 * 5, numPEs = 5), 
    PETests(fft = 2 * 5 * 5, numPEs = 5), 
    PETests(fft = 4 * 4 * 4 * 3, numPEs = 4),
    PETests(fft = 5 * 5 * 4 * 3, numPEs = 5),

    // The previous tests prefer to be ordered s.t. largest radix is on the left hand side (i.e. stages' = stages.sorted.reverse)
    // With these guys, after you do that, the first stage (on the left) prefers that the count least significant digit is shifted to the left by 1
    PETests(fft = 4 * 2 * 3, numPEs = 2), 
    PETests(fft = 4 * 2 * 5, numPEs = 2),

    // See "testScheduleTemp" for conditions -- requires special grouping
    PETests(fft = 5 * 4 * 4, numPEs = 2),
    PETests(fft = 4 * 4 * 3, numPEs = 2),

    // Duplicates
    PETests(fft = 4 * 4 * 3, numPEs = 1), 
    PETests(fft = 4 * 4 * 3, numPEs = 4) 
  )

  val tests = testsLong.slice(0, testsLong.length)

  val testVectors = tests.map { case test => 
    val fftParams = PeelingScheduling.getFFTParams(test.fft)
    // This is ideal, but requires changing some "firmware" stuff
    // val stages = fftParams.calc.getStages.head.stages.sorted.reverse
    val stages = fftParams.calc.getStages.head.stages
    println(s"\nFFT Stages: ${stages.mkString(", ")}, # PEs: ${test.numPEs}")
    test -> Scheduler.run(DIF, stages, test.numPEs)
  }

  // Redundant printing to make pattern observation easier (?)
  val validCombinations = testVectors.map { case (test, tvs) =>
    val fftParams = PeelingScheduling.getFFTParams(test.fft)
    val stages = fftParams.calc.getStages.head.stages
    println(s"\nFFT Stages: ${stages.mkString(", ")}, # PEs: ${test.numPEs}")
    tvs.map { case (stageCounts, combinedStageCounts, shiftCount, stageCountsNoShift) =>
      println(s"Stage: ${stageCounts.head.rad}")
      println(s"Least significant digit shift for counting (from right): $shiftCount")
      shiftCount == -1
    }
  }

  if (validCombinations.flatten.reduce(_ | _))
    throw new Exception(s"Could not find a schedule with a good pattern. See above.")
}

///////////////////////////////////////////////////////////////////////////////

object Scheduler {

  def run(fftType: FFTType = DIF, stages: Seq[Int], numPEs: Int) = { 

    // Internal
    case class CalcCtrlStageSeq(
      n: Seq[Int] = Seq.empty,
      rad: Int = 0,
      stageNum: Int = 0,
      bank: Seq[Int] = Seq.empty,
      addr: Seq[Int] = Seq.empty
    ) {
      def print() = 
        println("ns: " + n.mkString(", ") + "\t banks: " + bank.mkString(", ") + "\t addrs: " + addr.mkString(", "))
    }

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

    val stageVec = fftType match {
      // DIT counts stages in reverse order
      case DIT => stages.zipWithIndex.reverse
      case DIF => stages.zipWithIndex
    }

    // For banks, # of PEs multiplied @ index of left-most occurance of the largest radix
    // i.e. FFT = 2 * 3 * 3, 3 PEs in parallel --> bank = [n1 + 3 * n2 + n3] % 9
    // i.e. FFT = 4 * 2 * 3, 2 PEs in parallel --> bank = [2 * n1 + n2 + n3] % 8
    // Left needed so that all banks + addresses are accessed
    val leftMostIdxOfMaxRad = stages.indexOf(maxRad)
    val bankMul = Seq.fill(stagesLen)(1).updated(leftMostIdxOfMaxRad, numPEs)

    // PE counter is essentially mixed radix counter up to # of PEs per stage
    val testVectors = for ((srad, idx) <- stageVec) yield {
      // Max stage count associated with current stage is zeroed
      val stagesNew = stages.updated(idx, 1)
      val numPEsPerStage = fft / srad

      // Test count ordering, starting with right-most stage associated with least significant digit
      // Circular shift to the right of count order!
      val possibleSchedule = for (shiftCountOffset <- 0 until stages.length) yield {
        // Seems you have to shift less (different amounts of shift per stage of each FFT) when max radix is the left-most
        // But keep IO stuff static, so only shift for calculation!
        // If this is a constant amount, then there's no extra hardware required
        val shiftCount = 
          if (numPEs == 1)
            0
          else
            (shiftCountOffset + (stagesNew.length - leftMostIdxOfMaxRad)) % stages.length
        val stagesShifted = stagesNew.drop(stagesNew.length - shiftCount) ++ stagesNew.dropRight(shiftCount)

        val possibleCounts = for (count <- 0 until numPEsPerStage) yield {

          val tempMixedRadixCount = MixedRadix.toDigitSeqMSDFirst(count, radicesHighFirst = stagesShifted)  

          // Pad to correct # of digits
          val mixedRadixCountPadded = Seq.fill(stagesLen - tempMixedRadixCount.length)(0) ++ tempMixedRadixCount

          // Shift back to match IO ordering: must be done after padding!
          val mixedRadixCount = mixedRadixCountPadded.drop(shiftCount) ++ mixedRadixCountPadded.dropRight(stagesNew.length - shiftCount)

          CalcCtrlStageSeq(
            n = mixedRadixCount,
            rad = srad,
            stageNum = idx
          )
        }

        // Should be a separate block, done as an incremental step
        val schedule = for (countInfo <- possibleCounts) yield {
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
           
        val testScheduleTemp = 
          // TODO: Check that this is sufficiently generic (???) 
          if (leftMostIdxOfMaxRad == idx && !stages.contains(numPEs))
            schedule.toList.grouped(numPEs).toList
          else
            schedule.toList.grouped(numPEsPerStage / numPEs).toList.transpose

        val isGoodSchedule = testScheduleTemp.map { case group =>
          val groupedBankSet = group.map(_.bank).flatten
          val groupedAddrSet = group.map(_.addr)
          // Banks accessed simultaneously should be unique
          // Addresses per PE should be the same (not required, just makes life easier if true? -- seems like it doesn't work that well)
          (groupedBankSet.distinct.length == groupedBankSet.length) // && (groupedAddrSet.distinct.length == 1)
        }.reduce(_ & _)
        (schedule, testScheduleTemp, isGoodSchedule, shiftCount)
      }

      val goodSchedule = possibleSchedule.find { case (schedule, combinedSchedule, isGoodSchedule, shiftCount) => isGoodSchedule }
      goodSchedule match {
        case None =>
          (possibleSchedule(0)._1, possibleSchedule(0)._2, -1, possibleSchedule(0)._1)
        case Some(sch) =>
          // Last is without shift
          (sch._1, sch._2, sch._4, possibleSchedule(0)._1)
      }      
    }

    val (unsortedStageInfos, sortedStageInfos) = testVectors.map { case (stageCounts, combinedStageCounts, shiftCount, stageCountsNoShift) =>
      val stageRad = stageCounts.head.rad
      println(s"Stage: $stageRad")
      println(s"Least significant digit shift (from right): $shiftCount")
      combinedStageCounts foreach { case countSeq =>
        countSeq foreach { case count =>
          count.print()
        }
        println("----------")
      }
      val flattenedCombinedStageCounts = combinedStageCounts.flatten
      val unsorted = StageInfo(
        rad = stageRad,
        shiftCount = 0,
        n = stageCountsNoShift.map(_.n),
        bank = stageCountsNoShift.map(_.bank),
        addr = stageCountsNoShift.map(_.addr)
      )
      val sorted = unsorted.copy(
        shiftCount = shiftCount,
        n = flattenedCombinedStageCounts.map(_.n),
        bank = flattenedCombinedStageCounts.map(_.bank),
        addr = flattenedCombinedStageCounts.map(_.addr)
      )
      (unsorted, sorted)
    }.unzip

    val unsortedScheduledCalcParams = ScheduledCalcParams(
      fftn = fft,
      stageInfo = unsortedStageInfos,
      maxNumParallelPEs = numPEs,
      maxNumBanks = maxNumBanks,
      memLen = memLen,
      maxRad = maxRad
    )

    val scheduledCalcParams = unsortedScheduledCalcParams.copy(
      stageInfo = sortedStageInfos
    )

    val jsonUnsorted = compact(render(unsortedScheduledCalcParams.stageInfo.map(_.toJSONTuples)))
    val jsonSorted = compact(render(scheduledCalcParams.stageInfo.map(_.toJSONTuples)))    

    val htmlTemplate = Source.fromFile("visualization/scheduling_example.html").getLines.mkString("\n")
    val unsortedHtml = htmlTemplate.replace("{{#json}}", jsonUnsorted)
    val sortedHtml = htmlTemplate.replace("{{#json}}", jsonSorted)
    scala.tools.nsc.io.File(s"visualization/out/${fft}_${numPEs}_sorted.html").writeAll(sortedHtml)
    scala.tools.nsc.io.File(s"visualization/out/${fft}_${numPEs}_unsorted.html").writeAll(unsortedHtml)

    // All data locations should be exercised (sorted bank, address)
    val completeDataLocs = (0 until maxNumBanks).map { case b =>
      Seq.fill(memLen)(b).zip(0 until memLen)
    }.flatten.sorted

    testVectors foreach { case (stageCounts, combinedStageCounts, shiftCount, stageCountsNoShift) =>
      // Already sorted address first
      val stageDataLocs = stageCounts.map { case count =>
        count.bank.zip(count.addr)
      }.flatten.sorted
      require(completeDataLocs == stageDataLocs, s"This combination doesn't cover all data locations :(. Stage Idx: ${stageCounts.head.stageNum}")
    }

    testVectors

  }

}

/*

if !stages.contains(numPEs) & numPEsinParallsl > 1
  x + i, count should be + numPEs instead of + 1
else if numPEsinParallel > 1
group: x + (numPEsPerStage/PE) * i, where i [0, numPEs), count up to stagePEs/numPEs 

//////

totalCalcCycles
  * lower until meets spec -- parallelize smallest radix, right most so no twiddles
stages (including padding 0), stagesLen, stagesShortLen, stagesShort, leftMostIdxOfMaxRad
numPEsNotInStages (i.e. 2 for 4 4 3)
countIdxToDivide
  * only used for PE = 2, FFT = 4 * 4 * 3; generally -1 = don't care; signals renormalizing n before multiplying by ac
addressConstants
bankMul
per stage:
  bank, addr, n: Seq[Seq[Seq[Int]]]
  numPEsPerStage
  numPEsInGroup
  numParallelPEsPerStage
    * start w/ PE = 1, then go PE > prev PE and used radix. In bounds? If less, then optimize small -> large radix. Ideally 1 or worstCase
  numCycles, numCycles with pipe

// fft test vectors

// original FFT + 675 + 864 + 800 + 3780

Build visualizer

Minimize banks! + twiddle factors (i.e. rather do more 3's to bring down cycle count rather than also do for 4's which already don't have that many PEs per stage)

// get original FFT, see if calc too high, if so, split minimum that works....

make sure uniquness still holds for all of LTE (with # calc cycles...)

4 * 4 * 3 * 3 * 5 * 5 -- try 4, 3, 5

// Try for all FFT sizes supported!!

Pease is better except for reconfigurability

doubling up on smaller radix butterlfy is less power and more useful (to meet CC requirements)
need new twiddle

when # PEs is 2, but can group by 4 -- can do more per stage?

*/