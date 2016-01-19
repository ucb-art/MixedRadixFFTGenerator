// May 29, 2015
// Valid prime numbers for generator specified here!
// Currently only supports powers of 2, 3, and 5!
// Currently if radix-4 used but not radix-2 in generated FFT sizes, I'm still implementing 2 columns
// Ideally, I'd only have 1 column (generator needs to be smarter) to save area -- don't need column for radix-2
// Don't have to have user input for prime override; can auto calculate

package FFT
import scala.math._
import Chisel.Error

object generalConstants{

	// Valid prime numbers (FFT length must be factored only by these values)
	// Lowest first
	var validPrimes: Array[Int] = Array(2, 3, 5)

	// Factorization into coprimes for each FFT length
	// rows, cols
	var coprimesArray = Array.ofDim[Int](fftSizes.count,validPrimes.length)

	// If FFT N = 4^a1 * 2^a2 * 3^b * 5^c for validPrimes = [2 3 5] 
	// then for each N, row has values [a1 a2 b c]
	// Initialize (not final array size)
	var numPowerArray = Array.ofDim[Int](fftSizes.count,validPrimes.length)
		
	// Max 2^a, 3^b, 5^c power used for determining twiddle memory size, coprime counter sizes needed, etc
	var maxCoprime = new Array[Int](validPrimes.length)

	// Max # of stages needed to complete FFT
	var maxNumStages: Int = 0
	
	// Indicates how many times to branch a particular calculation i.e.
	// For address/bank 
	var stageBranch = new Array[Int](ceil(log(1)/log(2)).toInt)

	// Number of banks needed to handle all desired FFT sizes = max radix
	var numBanks: Int = 0
	var maxRadix: Int = 0

	// Worst case memory lengths
	var memoryLengths = new Array[Int](numBanks)

	// Test if radix-4 is needed (i.e. FFTN % 4 == 0 for any FFTN)
	var rad4Used: Boolean = false
	
	var pow2SupportedTF: Boolean = false

	// Butterfly radices supported & used
	var validRadices = new Array[Int](validPrimes.length)

	// # of coprimes 
	var numCoprimes: Int = 0

	// Counts number of times that the selected FFT size can be factored by the valid primes
	// Throws error if the FFT size requires an unsupported prime
	// i = index in FFT array for current FFT size
	// Returns array containing factor counts
	def factorize(i:Int) : Array[Int] = {
		
		val FFTN:Int = fftSizes.fftSizeArray(i)
		var num:Int = FFTN
		var factorizationCount = new Array[Int](validPrimes.length)
		
		// Test if N can be factored by current prime (mod = 0) & update
		// Count # of times it can be factored by current prime
		for ( j <- 0 to validPrimes.length-1){
			var modval:Int = 0
			while (modval == 0){
				modval = num % validPrimes(j)
				if (modval == 0){
					factorizationCount(j) = factorizationCount(j)+1
					num = num/validPrimes(j)
				}	
			}
		}
		
		// If number hasn't completely been factorized, then an unsupported prime is required
		if (num != 1){
			println(Console.RED + Console.BOLD + s"\nFFT size [$FFTN] is invalid. FFT size must be a factor of [" + validPrimes.mkString(", ") +"]\n\n")
			Error("")
		}

		return factorizationCount
	}

	def generate(validPrimesOverride: Array[Int], verboseTF: Boolean) : Unit = {
		
		// Gets intersection of valid primes and valid primes override specified by user
		validPrimes = validPrimes.intersect(validPrimesOverride)
		validPrimes = validPrimes.sorted
		
		// Is power of 2 supported
		pow2SupportedTF = validPrimes.contains(2)

		// Update sizes based off validPrimes length
 		maxCoprime = new Array[Int](validPrimes.length)
		coprimesArray = Array.ofDim[Int](fftSizes.count,validPrimes.length)

		// Check if radix-4 needed (N%4 = 0)
		if (fftSizes.fftSizeArray.map(_%4).min == 0){
			rad4Used = true
		}

		// For calculating worst case # of memory banks and memory lengths
		var maxRadixArray = new Array[Int](fftSizes.count)

		// Note that powers of 2 are split between powers of 4 [and 2 (if power of two is odd)]
		// to take advantage of higher radix butterfly only if power of 2 is supported
		if (pow2SupportedTF && rad4Used){
			numPowerArray = Array.ofDim[Int](fftSizes.count,validPrimes.length+1)
			validRadices = Array(4) ++ validPrimes
		}
		else{
			numPowerArray = Array.ofDim[Int](fftSizes.count,validPrimes.length)
			validRadices = validPrimes
		}

		// For all FFT sizes
		for ( i <- 0 to fftSizes.count-1){

			// Number of times values in validPrimes can factor into FFT size
			val factorizationCount = factorize(i)

			// Get coprime factors 2^a, 3^b, 5^c, etc. of FFT size
			for (j <- 0 to validPrimes.length-1){
				coprimesArray(i)(j) = pow(validPrimes(j),factorizationCount(j)).toInt
			}

			if (pow2SupportedTF && rad4Used){
				// Because valid coprimes are sorted, 2 is always left-most
				// If 2 is valid prime, separate 2^a into 4^a1*2^a2, save a1 & a2
				numPowerArray(i)(0) = floor(factorizationCount(0)/2).toInt
				numPowerArray(i)(1) = (factorizationCount(0)%2).toInt
				// Insert power counts for remaining primes
				for (j <- 2 to numPowerArray(0).length-1){
					numPowerArray(i)(j) = factorizationCount(j-1)
				}
			}
			else{
				for (j <- 0 to numPowerArray(0).length-1){
					numPowerArray(i)(j) = factorizationCount(j)
				}
			}

			// Update maximum # of stages required
			val numStages = numPowerArray(i).sum
			if (numStages > maxNumStages){
				maxNumStages = numStages
			}

			// Prototype for hardware generation. Note that the first non-zero
			// power count from the right most likely corresponds to the maximum radix
			// needed in the calculation, except when 4 is used and is greater than this value
			var maxRadixTemp : Int = 0
			for (j <- numPowerArray(0).length-1 to 0 by -1){
				if (numPowerArray(i)(j) != 0 && maxRadixTemp == 0){
					maxRadixTemp = validRadices(j)
				}
			}
			if (pow2SupportedTF && rad4Used){
				if (numPowerArray(i)(0) != 0){				// radix-4 used
					if ( 4.toInt > maxRadixTemp){
						maxRadixTemp = 4.toInt
					} 
				}
			}
			maxRadixArray(i) = maxRadixTemp	

		}

		// # Banks = max radix needed
		numBanks = maxRadixArray.max
		maxRadix = maxRadixArray.max

		// If max radix used is 5, need 5 memory banks
		memoryLengths = new Array[Int](numBanks)
		// Sort lowest to highest ie. 2,3,4,5
		val radicesOrdered = validRadices.sorted
		for (i <- 0 to fftSizes.count-1){
			// Memory length needed for given FFT size
			val memLenTemp = fftSizes.fftSizeArray(i)/maxRadixArray(i)
			// Map maximum radix for current FFT size to equivalent location in memoryLengths array if larger than stored value
			// (Want to find maximum memory required for supporting each radix)
			if (memLenTemp > memoryLengths(maxRadixArray(i)-1)){
				memoryLengths(maxRadixArray(i)-1) = memLenTemp
			}
		}
		// Note that if radix-5 is used and requires the largest memory size, 
		// bank 5 and all lower banks must use that size (the lower banks are active too)
		for (i <- numBanks-2 to 0 by -1){
			if (memoryLengths(i+1) > memoryLengths(i)){
				memoryLengths(i) = memoryLengths(i+1)
			}
		}

		// Calculate maximum values of 2^a, 3^b, 5^c, etc. needed to determine counter maximums, bit sizes, memory lengths, etc. 
		var temp = coprimesArray.transpose
		for (i <- 0 to validPrimes.length-1){
			maxCoprime(i) = temp(i).max
		}

		numCoprimes = coprimesArray(0).length

		// I.e. for max # stages = 6, log(6)/log(2) = 2.59 ceil -> 3 so 3 branch stages needed
		// ceil(6/2) = 3 i.e. a{0,1},b{2,3},c{4,5}
		// ceil(3/2) = 2 x{a,b},y{c}
		// ceil(2/2) = 1 {x,y}
		// for n1...n6 -> address, bank 
		stageBranch = new Array[Int](ceil(log(maxNumStages)/log(2)).toInt)
		for(r <- 0 until stageBranch.length){
			if (r == 0){
				stageBranch(r) = ceil(maxNumStages/2.toDouble).toInt
			}
			else{
				stageBranch(r) = ceil(stageBranch(r-1)/2.toDouble).toInt
			}
		}

		// Debug
		if (verboseTF){
			println(Console.BLUE + Console.BOLD + s"\nArray of [${numCoprimes}] coprimes for all FFT sizes:")
			for (i <- 0 to fftSizes.count-1){
				println(Console.RESET + Console.BLUE + coprimesArray(i).mkString("\t"))
			}
			println(Console.BLUE + Console.BOLD + "\n\n" + validRadices.mkString("\t") + "\tpower counts for all FFT sizes:")
			for (i <- 0 to fftSizes.count-1){
				println(Console.RESET + Console.BLUE + numPowerArray(i).mkString("\t"))
			}
			println(Console.BLUE + Console.BOLD + "\n\nMaximum coprime values: [" + maxCoprime.mkString(", ") + "]\n")
			println(Console.BLUE + Console.BOLD + "\nMaximum # of stages: " + s"${maxNumStages}\n")
			println(Console.BLUE + Console.BOLD + s"\n${numBanks} banks with lengths [" + memoryLengths.mkString(", ") + "]\n")
			println(Console.BLUE + Console.BOLD + s"\nStage Branch #'s [" + stageBranch.mkString(", ") + "]\n")
		}

	}

}
