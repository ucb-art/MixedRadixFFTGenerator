// May 31, 2015
// Check that output is valid if validRadices doesn't contain 4 but contains 2 
// Can reduce twiddle4 LUT when max 2^N = 2 (only need 1 instead of 3 twiddles) 4_1 instead of 4_1,4_2,4_3
// CoprimeIndex needs to be cased out further. Right now, doesn't consider the case when
// radix-4 is used but without the need for radix-2
// Note seems like twiddleCountMax for last stage always = 0 --> can save on LUT* 
// Changed Complex int with rounding to double

package FFT
import scala.math._
import ChiselDSP._

object twiddleConstants{

	// Twiddles for all coprimes used
	//var twiddleConstantsArray = Array.ofDim[Complex[SInt]](generalConstants.validPrimes.length,0,0)
	var twiddleConstantsArray = Array.ofDim[ScalaComplex](generalConstants.validPrimes.length,0,0)
	
	var twiddleMemSize:Int = 0

	// Twiddle counter max for each FFT stage for all FFT values
	var twiddleCountMaxArray = Array.ofDim[Int](fftSizes.count,generalConstants.maxNumStages)

	// Twiddle count base scale factor to renormalize to full twiddle memory size
	var RadNTwiddleMulFactorArray = Array.ofDim[Int](generalConstants.validPrimes.length,0)

	// Generate constants
	def generate(verboseTF:Boolean) : Unit = {
		
		// Generate twiddles for all coprimes used
		for (i <- 0 to generalConstants.validPrimes.length-1){
			
			val currentRadix = generalConstants.validPrimes(i)
			val maxCoprime = generalConstants.maxCoprime(i)
			twiddleConstantsArray(i) = twiddleN(currentRadix,maxCoprime)
		}

		// For all FFT sizes, generate twiddle counter
		for (i <- 0 to fftSizes.count-1){
			
			// Placeholder
			var raised:Double = 0
			var radix:Int = 0
			var coprime:Int = 0

			val powerCounts = generalConstants.numPowerArray(i)
			val coprimes = generalConstants.coprimesArray(i)
			var stageSum = new Array[Int](powerCounts.length)

			// Ex: sum(0) = power(0)
			// sum(1) = power(0)+power(1)
			// sum(2) = sum(1) + power(2)
			// Keeps track of # of stages required up until current radix
			// Where the last array value represents total # of stages required for FFT calc
			stageSum(0) = powerCounts(0)
			for (j <- 1 to powerCounts.length-1){
				stageSum(j) = stageSum(j-1)+powerCounts(j)
			}

			// Fill in twiddle count maximums for all stages
			// Not accounting for twiddle sub counts (due to handling coprimes) and
			// twiddle scale factor (scaled each stage of current coprime + initial 
			// scale factor to normalize to full twiddle memory), twiddle counts 
			// have the following pattern (where N is the current coprime):
			// Stage 1: 0 to N/R1-1
			// Stage 2: 0 to N/R1/R2-1
			// Stage 3: 0 to N/R1/R2/R3-1
			for (j <- 0 to generalConstants.maxNumStages-1){
				var count:Int = 0
				var sumsDone:Boolean = false
				
				// Run through conditions: i.e. j < power(0) : i.e. radix-4,
				// j < power(0)+power(1) : i.e. radix-2 (for 4235 radices), 
				// until condition is true. Note that if no conditions are true 
				// up until (and including) j < total # of stages for current FFT, then
				// set flag indicating stage not used (set count to 0)
				while (j >= stageSum(count) && !sumsDone){
					if (count < powerCounts.length-1){
						count = count + 1
					}
					else{
						sumsDone = true
					}
				}
				if (sumsDone){
					twiddleCountMaxArray(i)(j) = 0
				}
				else{	
					radix = generalConstants.validRadices(count)
					// Handling radix-2 is edge case of handling radix-4
					// (twiddle = W0N = 1, last stage of 2^N FFT). 
					//Twiddle LUT is for handling radix-4					
					if (radix == 2){
						radix = 4
					}
					if (j < stageSum(0)){
						raised = j+1
					}
					// Radix-2 is edge case
					else if (generalConstants.validRadices(count) == 2){					
						raised = j+0.5
					}
					else{
						raised = j+1-stageSum(count-1)
					}
					// Note that if radix-4 butterflies are required
					// to handle some 2^N powers, radix-4 will always be first
					// followed by radix-2. However, both correspond to 2^N Coprime.
					// So radix-{4,2,3,5} handled by {2^N,2^N,3^M,5^k}
					// Note for coprimes, index >0 --> index-1
					var coprimeIndex:Int = count
					if(generalConstants.rad4Used && count > 0){
						coprimeIndex = count-1
					}
					coprime = coprimes(coprimeIndex)
					twiddleCountMaxArray(i)(j) = round(coprime/pow(radix,raised)-1).toInt
				}

			}
		}

		// Note that 2/4 = 1/2 = 4/8
		// Twiddle is some subset of maximum twiddle
		// If 2^N is smaller than 2^M, where M is max; can scale all of the 2^M
		// twiddles to accommodate 2^N twiddles by 2^M/2^N
		// Thus, if M = 3, N = 2, when k = 2, this is the same as looking through
		// twiddle memory for the M = 3 case and picking k = 4
		for (i <- 0 to generalConstants.validPrimes.length-1){
			// Note that 2^N stages always first in DIF if required &
			// Rad2TwiddleMulFactor is trivial to calculate, so don't need to generate LUT
			if (generalConstants.validPrimes(i) != 2){
				val powXm = round(log(generalConstants.maxCoprime(i))/log(generalConstants.validPrimes(i))).toInt
				for (j <- 0 to powXm){
					RadNTwiddleMulFactorArray(i) = RadNTwiddleMulFactorArray(i) :+ pow(generalConstants.validPrimes(i),(powXm-j)).toInt
				}
			}
		}

		// Debug
		if (verboseTF){
			
			println(Console.BLUE + Console.BOLD + s"Twiddle Memory Size: ${twiddleMemSize}")
			for (i <- 0 to generalConstants.validPrimes.length-1){
				println(Console.BLUE + Console.BOLD + s"\n\nRadix-${generalConstants.validPrimes(i)} Twiddles:" + Console.RESET + Console.BLUE)
				for (j <- 0 to twiddleConstantsArray(i).length-1){				// twiddle memory size (row)
					for (k <- 0 to twiddleConstantsArray(i)(j).length-1){		// radix-1 (col)
						print(s"${twiddleConstantsArray(i)(j)(k).toString}"+"\t")
					}
					print("\n")
				}
			}
			println(Console.BLUE + Console.BOLD + s"\n\nTwiddle Counts for each stage:")
			for (i <- 0 to fftSizes.count-1){
				println(Console.RESET + Console.BLUE + twiddleCountMaxArray(i).mkString("\t"))
			}
			print("\n")

			for (i <- 0 to generalConstants.validPrimes.length-1){
				if (RadNTwiddleMulFactorArray(i).nonEmpty){
					println(Console.BLUE + Console.BOLD + s"\nk to ${generalConstants.validPrimes(i)}^(MaxK-k)  Mapping:")
					println(Console.RESET + Console.BLUE + RadNTwiddleMulFactorArray(i).mkString("\t")+"\n\n")
				}
			}

		}
	}

	def twiddleN(currentRadixI:Int,maxCoprime:Int): Array[Array[ScalaComplex]] = {

		var currentRadix:Int = currentRadixI

		// When coprime is 2 and radix 4 is required, generate twiddles
		// assuming current radix is 4. Note that in doing FFT calculation,
		// first the radix-4 butterflies are handled followed by radix-2 butterflies
		// if needed in the final stage of the coprime = 2 calculation.
		// Assuming radix-2 butterflies are used, the associated twiddles are trivial = W0N = 1
		if (currentRadix == 2){
			currentRadix = 4
		}

		// Note that decimation in time is exactly reverse of decimation in
   		// frequency, with twiddles being multiplied before the butterfly instead of
   		// after (DIF)
 
		// First stage of coprime calculation requires the most unique twiddles = coprime#/radix
		// Twiddle factors for subsequent stages found through renormalization of twiddle LUT addresses. 
		// If max 2^N = 2, still should generate trivial W0N = 1 twiddles
		val twiddleNsize = ceil(maxCoprime/currentRadix).toInt	

		// Fill twiddles into memories
		// Note that # of twiddles for radix-a butterfly is a-1
		// row, col
		var twiddleNArray = Array.ofDim[ScalaComplex](twiddleNsize,currentRadix-1)

		for (k <- 0 to twiddleNsize-1;
			 n <- 0 to currentRadix-2){
			
			// Twiddle W_N^(nk) = exp(-j*2*pi*n*k/N)
			// exp(i*t) = cos(t) + i*sin(t)
			// fixed point
				
			val t = -2*Pi*k*(n+1)/maxCoprime
			//val real = round(SDR_FFT.complexScale*cos(t)).toInt
			//val imag = round(SDR_FFT.complexScale*sin(t)).toInt
			val real = cos(t)
			val imag = sin(t)
			//val twiddle = new Complex[SInt](SInt(real,width=Work.dataBitWidth),SInt(imag,width=Work.dataBitWidth))
			//if (real == -1.0 || imag == -1) {println("Twiddle has -1! :("); exit(3)}
			val twiddle = Complex(real,imag)
			
			twiddleNArray(k)(n) = twiddle

			// Keep track of total complex LUT memory needed
			twiddleMemSize = twiddleMemSize+1

		}
		return twiddleNArray
	}

}
