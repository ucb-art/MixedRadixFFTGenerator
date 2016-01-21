// August 19, 2015

package FFT
import Chisel._
import memBanks._
import calc._
import generator._

object FFTGenerator {






  def apply(): Unit = {

/////////////////////////////////////////////////////////////////////////////////////////////////////////////

    //val customFFTSizes: Array[Int] =  Array.empty[Int]
    var customFFTSizes:Array[Int] = Array(2,3,4,5,8,9,16,32,25,15,24,27,60,30,125,81,243,243*3,2048,648,864,972,
      15, 20, 2, 4, 25, 3, 9, 16, 6, 10, 8, 27, 5, 30, 125, 32, 81, 243, 18, 1728)
    
  //customFFTSizes = Array(16,27)
    
    // Add standard FFT sizes to list of possible FFT sizes    
    var standardRadioFFTSizesTF:Boolean = false
    var SCFDMAFFTSizesTF:Boolean = false

  //standardRadioFFTSizesTF = false
  //SCFDMAFFTSizesTF = false

    // Valid primes override
    // NOTE: must be subset of those supported by butterflies (automatically gets intersection)
    // This would help save some logic if you don't need all supported butterflies/primes
    var validPrimesOverride: Array[Int] = Array(2,3,5)

  //validPrimesOverride = Array(2,3)

    // LUT values printout (verbose) for debugging
    val verboseTF: Boolean = false





    // # of cycles to complete butterfly operation
    val Bcycles: Int = 3

    // True: single port SRAM; false: dual-port SRAM
    val SPTF: Boolean = false

    // True: minimize # of banks
    val minBankTF: Boolean = true

  


/////////////////////////////////////////////////////////////////////////////////////////////////////////////

    // Generate valid FFT sizes

    //customFFTSizes = Array(243)
    customFFTSizes = Array(24,15,16)
    //customFFTSizes = Array()
    //fftSizes.generate(customFFTSizes,true,true)
    fftSizes.generate(customFFTSizes,false,false)
    //fftSizes.generate(customFFTSizes,standardRadioFFTSizesTF,SCFDMAFFTSizesTF)
    // need 6 stages
    //fftSizes.generate(Array(2,3,4,5,8,9,16,32,25,15,24,27,60,30,125,81,243,243*3,2048,648,864,972),false,false)

    if (fftSizes.fftSizeArray.isEmpty){
      println(Console.RED + Console.BOLD + "\n\nNo FFT sizes chosen. Don't generate Verilog. Exit.\n\n")
    }
    else{
      // FFT sizes to be generated
      println(Console.BLUE + Console.BOLD + s"\n\n${fftSizes.count}" + " FFT sizes to generate:") 
      println(Console.RESET + Console.BLUE + fftSizes.fftSizeArray.mkString("*\t") + "*\n")
      
      // Appends any primes needed for radio that aren't specified by the user override
      validPrimesOverride = validPrimesOverride ++ fftSizes.neededPrimes
      validPrimesOverride = validPrimesOverride.sorted.distinct

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
}



