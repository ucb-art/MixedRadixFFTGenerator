// August 19, 2015

package FFT
import Chisel._
import memBanks._
import calc._
import generator._

object FFTGenerator {

  def apply(): Unit = {

    // Valid primes override
    // NOTE: must be subset of those supported by butterflies (automatically gets intersection)
    // This would help save some logic if you don't need all supported butterflies/primes
    var validPrimesOverride: Array[Int] = Array(2,3,5)

    // # of cycles to complete butterfly operation
    val Bcycles: Int = 3

    // True: single port SRAM; false: dual-port SRAM
    val SPTF: Boolean = false

    // True: minimize # of banks
    val minBankTF: Boolean = true

    // Generate constants used by Chisel for desired FFT sizes
    generalConstants.generate(validPrimesOverride,true)

    schedule.generate(Bcycles,SPTF,minBankTF,false) //verboseTF)

    ioAddressConstants.generate(true)
    twiddleConstants.generate(false)
    updateMemConstants(generalConstants.numBanks,generalConstants.memoryLengths)
    maxNumStages = generalConstants.maxNumStages
    stageBranch = generalConstants.stageBranch
    val numPowerArray = generalConstants.numPowerArray.transpose
    powColCount = numPowerArray.length
    // TODO: change
    pipeBFWriteDly = 2//wftaDly.sum+butterfly.twDly

  }
}

