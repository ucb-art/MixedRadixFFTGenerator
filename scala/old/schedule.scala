// August 2, 2015
// # banks for a given FFTN = maxRadix * (A prime factor of {FFTN/maxRadix}) 
// Minimum prime factor chosen if minBankTF is true
// Maximum prime factor chosen if minBankTF is false 
// Exception is when prime factor is 2; can be scaled to 4 if necessary
// Schedule tested up to ~40k (ignoring some invalid FFTNs due to bank rule)


// check highest always on left for addressing not  banking

// If you want to just make an FFT engine with FFTN = 2, minimum # of cycles for this configuration is 3 (BFcycles) same for 235
// So should case FFTN = 2 out separately
// check that first butterfly never gets last IO data
// possibly reoptimize of easier to divide N/banks if haven't utilized max bank, # butterflies for given N? probably not worth reoptimizing, don't want
// to use extra butterfly when can be avoided (assume max # of banks as # of extra mem slots not that big)


// mimo???

package FFT
import scala.math._
import scala.util.control.Breaks._
import Chisel.Error

object schedule{

	def exit(): List[List[Int]] = {
		Error("blah"); List(List(0))
	}



  var numBFs = 1





	// Radix stages for each FFTN explicitly listed i.e. 60 = 4,3,5
	var radixStagesArray: Array[List[Int]] = Array.empty[List[Int]]
	// Max radix for each FFTN
	var maxRadixArray : Array[Int] = Array.empty[Int]
	// # of stages to complete each FFTN
	var numStgArray: Array[Int] = Array.empty[Int]
	// # of cycles to complete FFT calculation with single butterfly
	var BF1CycleCntArray: Array[Int] = Array.empty[Int]
	// # of butterflies needed to complete FFT calculation in < N cycles
	var optBFNumArray: Array[Int] = Array.empty[Int]
	// # of cycles to complete FFT calculation using optimal # of butterflies
	var optCycleCntArray: Array[Int] = Array.empty[Int]
	// Optimal # of single-port RAM banks needed
	var SPBankArray: Array[Int] = Array.empty[Int]
	// Optimal # of dual-port RAM banks needed
	var DPBankArray: Array[Int] = Array.empty[Int]
	// Length of SP memory needed per bank
	var SPLengthArray: Array[Int] = Array.empty[Int]
	// Length of DP memory needed per bank
	var DPLengthArray: Array[Int] = Array.empty[Int]

	// Custom length for each memory bank
	var memBankLenArray: Array[Int] = Array.empty[Int]

	// FFTNs that can't complete calculation in <=N clock cycles
	var FFTCycleCntExceedsN: Array[Int] = Array.empty[Int]
	// Max # of parallel butterflies needed
	var maxButterflies:Int = 0
	// Max # single-port SRAMs
	var maxSPbanks:Int = 0
	// Max @ dual-port SRAMs
	var maxDPbanks:Int = 0
	// FFTNs that have sub-optimal memory lengths for SP RAM
	var FFTSubOptLenSP: Array[Int] = Array.empty[Int]
	// FFTNs that have sub-optimal memory lengths for DP RAM
	var FFTSubOptLenDP: Array[Int] = Array.empty[Int]

	// Final # of banks used should be numUniqueBankGroups * maxRadix (FFTN should be divisible by this number)
	var uniqueBankGroupsArray: Array[Int] = Array.empty[Int]
	// Re-ordered radices array (bank group value should be at the end)
	var reorderedRadicesArray: Array[Array[Int]] = Array.empty[Array[Int]]
	// Re-ordered radix stages array
	var reorderedRadixStagesArray: Array[List[Int]] = Array.empty[List[Int]]
	// Final tally of RAM banks needed
	var MemBankArray: Array[Int] = Array.empty[Int]
	// Final tally of blengths needed per bank
	var MemLengthArray: Array[Int] = Array.empty[Int]

	// List of FFTNs with failed scheduling
	var scheduleFailed: Array[Int] = Array.empty[Int]
	var failureReport: Array[String] = Array.empty[String]

//////////////////////////////////////////////////////////////////////////////////////

	// BFcycles = # of cycles to complete butterfly operation
	// SPTF -> T uses single-port RAM; F uses dual-port RAM
	def generate(BFcycles:Int, SPTF:Boolean, minBankTF:Boolean, verboseTF:Boolean) : Unit = {




    val verboseTF2 = true


		if (SPTF){
			println(Console.RED + Console.BOLD + "\nOnly dual-port memory supported currently. :(\n")
			exit()
		}
		if (verboseTF2){
			printf(Console.BLUE + Console.BOLD + "\n[FFTN] \t[MaxR] \t[#Stg] \t[#BFs] \t[#SPb] ")
			printf("\t[#DPb] \t[FFTN] \t[NCyc] \t[#1BFC] \t[SP_S] \t[DP_S] \t[Radix Stages]\n")
		}

		// Iterate through all valid FFT sizes
		for (idx <- 0 until fftSizes.count){
			initializeN(idx,BFcycles,SPTF,verboseTF2)			
		}
		
		

		
		
		
		
		// Max # of butterflies needed to support streaming for all FFTN's
		maxButterflies = optBFNumArray.max

		if (verboseTF){
			printf("\n")
			println(Console.BLUE + Console.BOLD + "\nMaximum # of parallel butterflies to achieve <N Cycles: " + maxButterflies + "\n")
		}
		if (!FFTCycleCntExceedsN.isEmpty){
			println(Console.RED + Console.BOLD + "\nFFTNs that require >N clock cycles: [" + FFTCycleCntExceedsN.mkString(", ") + "]")
			println(Console.RESET + Console.RED + "Note that FFTN = [" + generalConstants.validRadices.mkString(", ")+ "] are considered trivial cases.\n" + Console.RESET)
			for (w <- 0 until FFTCycleCntExceedsN.length){
				// If more than trivial radices fail timing, error out
				if (!generalConstants.validRadices.contains(FFTCycleCntExceedsN(w))){
					exit()
				}
			}
		}
		
		updateMemSpecs1(SPTF,verboseTF)
	
		// Pack memory into fewest number of banks while taking advantage of FFTN/# of banks divisibility as much as possible
		packMem(SPTF,minBankTF,verboseTF)

		// Second pass
		updateMemSpecs2(SPTF,verboseTF)

		// Test schedule

val FFTN:Int = 12500
var idx:Int = fftSizes.fftSizeArray.indexOf(FFTN)


		for (idx <- 0 until fftSizes.count){
			if (verboseTF){
				println(Console.BLUE + Console.BOLD + "\nTesting schedule for FFTN = "+fftSizes.fftSizeArray(idx))
			}
			test(idx,1,verboseTF)
			test(idx,2,verboseTF)
		}

		if (!scheduleFailed.isEmpty){
			println(Console.RED + Console.BOLD + "\nFFTNs failing scheduling: [" + scheduleFailed.distinct.mkString(", ") + "]\n" + Console.RESET)
			for (t <- 0 until failureReport.length){
				println(failureReport(t))
			}
			println(Console.RED + Console.BOLD + "\nEnd of Scheduling Error Report")
			printf("\n")
			exit()
		}


	}

//////////////////////////////////////////////////////////////////////////////////////
// First-pass cycle, butterfly calculation

	def initializeN(idx:Int, BFcycles:Int, SPTF:Boolean, verboseTF:Boolean) : Unit = {

		var radixStages: List[Int] = List.empty[Int]

		// Figures out what radix stages are needed based off of stored power constants (i.e. a1, a2, b, c 
		// in 4^(a1)*2^(a2)*3^b*5^c for each FFTN) 
		for (i <- 0 until generalConstants.validRadices.length){
			val numPow = generalConstants.numPowerArray(idx)(i)
			if (numPow != 0){
				for (j <- 0 until numPow){
					radixStages = radixStages :+ generalConstants.validRadices(i)
				}
			}
		}

		// # of stages needed to complete FFTN
		val stageCount:Int = generalConstants.numPowerArray(idx).sum

		// To maximize butterfly logic utilization, for all practical purposes
		// Overlap = BFcycles - 1 --> V = P - 1
		val overlap:Int = BFcycles - 1
		
		// # of clock cycles = (S-1) + V + (P-V) Sum[0 to S-1]Ceil[N/r_i/B]
		// S = # of stages (to deal with in-between stage conflict); N = FFT length
		// r_i is radix of current stage; B = # of parallel butterflies

		// For 1 butterfly
		val extraCycles:Int = (stageCount-1) + overlap 
		var cycleCnt1BF:Int = 0//extraCycles
		val FFTN:Int = fftSizes.fftSizeArray(idx)
		for (i <- 0 until radixStages.length){
			val r:Double = radixStages(i).toDouble
			cycleCnt1BF = cycleCnt1BF + (BFcycles-overlap)*(FFTN/r).ceil.toInt
		}

		// If FFTN = valid radix, cannot parallelize (i.e. 2, 3, 4, 5)
		var parallelBF:Int = 0
		if (generalConstants.validRadices.contains(FFTN)){
			parallelBF = 1
		}
		else{
			// # of clock cycles should be less than or equal to N
			// Dictates # of parallel butterflies
			// B >= Ceil[(P-V)*N/(N-S-V+1)Sum[0 to S-1][1/r_i]]
			var optimalBF:Double = 0
			for (i <- 0 until radixStages.length){
				val r:Double = radixStages(i).toDouble
				optimalBF = optimalBF + (BFcycles-overlap)*FFTN/(FFTN-extraCycles).toDouble/r
			}
			parallelBF = optimalBF.ceil.toInt
			
		}

		optBFNumArray = optBFNumArray :+ parallelBF

		// Calculate # of clock cycles needed with optimum butterfly number
		var cycleCntOpt:Int = extraCycles
		for (i <- 0 until radixStages.length){
			val r:Double = radixStages(i).toDouble
			cycleCntOpt = cycleCntOpt + (BFcycles-overlap)*(FFTN/r/parallelBF.toDouble).ceil.toInt
		}

		val maxRadix:Int = radixStages.max
		radixStagesArray = radixStagesArray :+ radixStages
		maxRadixArray = maxRadixArray :+ maxRadix
		numStgArray = numStgArray :+ stageCount
		BF1CycleCntArray = BF1CycleCntArray :+ cycleCnt1BF
		optCycleCntArray = optCycleCntArray :+ cycleCntOpt

		// # of banks needed for single-port memory design
		val numSPbanks:Int = maxRadix * (overlap + 1) * parallelBF
		SPBankArray = SPBankArray :+ numSPbanks
		// # of banks needed for dual-port memory
		val numDPbanks:Int = maxRadix * parallelBF
		DPBankArray = DPBankArray :+ numDPbanks

		// Memory length of each bank (single-port/dual-port)
		val SPlength:Int = (FFTN/numSPbanks.toDouble).ceil.toInt
		SPLengthArray = SPLengthArray :+ SPlength
		val DPlength:Int = (FFTN/numDPbanks.toDouble).ceil.toInt
		DPLengthArray = DPLengthArray :+ DPlength

		// If # of banks doesn't divide evenly into FFTN, then you have extra memory slots to handle all cases (sub-optimal) for SP/DP
		if ((FFTN % numSPbanks) != 0){
			FFTSubOptLenSP = FFTSubOptLenSP :+ FFTN
		}
		if ((FFTN % numDPbanks) != 0){
			FFTSubOptLenDP = FFTSubOptLenDP :+ FFTN
		}

		// If # of clock cycles to complete FFTN calculation is greater than N, doesn't meet timing
		if (cycleCntOpt > FFTN){
			FFTCycleCntExceedsN = FFTCycleCntExceedsN :+ FFTN
		}

		if (verboseTF){
			printf(Console.RESET + Console.BLUE + "[" + FFTN + "] \t")
			printf("[" + maxRadix + "] \t")
			printf("[" + stageCount + "] \t")
			printf("[" + parallelBF + "] \t")
			printf("[" + numSPbanks + "] \t")
			printf("[" + numDPbanks + "] \t")
			printf("[" + FFTN + "] \t")
			printf("[" + cycleCntOpt + "] \t")
			printf("[" + cycleCnt1BF + "]       \t")
			printf("[" + SPlength + "] \t")
			printf("[" + DPlength + "] \t")
			printf("" + radixStages.mkString("\t") + "\t")
			printf("\n")
		}

	}

//////////////////////////////////////////////////////////////////////////////////////
// Determine memory sizes, etc. for current banking arrangement (1st pass)

	def updateMemSpecs1(SPTF:Boolean,verboseTF:Boolean) : Unit = {

		val maxSPbanks:Int = SPBankArray.max
		val maxDPbanks:Int = DPBankArray.max
		val maxDPlen:Int = DPLengthArray.max
		val maxSPlen:Int = SPLengthArray.max

		println(Console.BLUE + Console.BOLD + "\n//////////////////////// Pass 1 ////////////////////////\n")		

		if (verboseTF){
			
			println(Console.BLUE + Console.BOLD + "\nMaximum # of single-port RAM banks: " + maxSPbanks + "")
			println(Console.BLUE + Console.BOLD + "Maximum length of single-port RAM banks: " + maxSPlen + "\n")
			println(Console.BLUE + Console.BOLD + "\nMaximum # of dual-port RAM banks: " + maxDPbanks + "")
			println(Console.BLUE + Console.BOLD + "Maximum length of dual-port RAM banks: " + maxDPlen + "\n")

			if (!FFTSubOptLenSP.isEmpty && SPTF){
				println(Console.RED + Console.BOLD + "\nFFTNs with sub-optimal SP memory lengths: [" + FFTSubOptLenSP.mkString(", ") + "]\n" + Console.RESET)
			}
			if (!FFTSubOptLenDP.isEmpty && !SPTF){
				println(Console.RED + Console.BOLD + "\nFFTNs with sub-optimal DP memory lengths: [" + FFTSubOptLenDP.mkString(", ") + "]\n" + Console.RESET)
			}

		}

		var memType:String = "single-port"
		if (SPTF){
			memBankLenArray = new Array[Int](maxSPbanks)
		}
		else{
			memBankLenArray = new Array[Int](maxDPbanks)
			memType = "dual-port"
		}

		// Go through all FFT sizes to figure out what is the worst case memory length needed for each bank
		for (i <- 0 until fftSizes.count){
			var banks:Int = 0
			var memLen:Int = 0
			if (SPTF){
				banks = SPBankArray(i)
				memLen = SPLengthArray(i)
			}
			else{
				banks = DPBankArray(i)
				memLen = DPLengthArray(i)
			}
			// For all banks needed for a specific FFTN, update with new length if larger
			for (j <- 0 until banks){
				if (memLen > memBankLenArray(j)){
					memBankLenArray(j) = memLen
				}
			}
		}

		println(Console.BLUE + Console.BOLD + "\nBank lengths for "+ Console.MAGENTA + memType+ Console.BLUE + " memory: [" + memBankLenArray.mkString(", ") + "]\n" + Console.RESET)

	}

//////////////////////////////////////////////////////////////////////////////////////
// Determine memory sizes, etc. for current banking arrangement (2nd pass)

	def updateMemSpecs2(SPTF:Boolean,verboseTF:Boolean) : Unit = {

		val maxbanks:Int = MemBankArray.max
		val maxlen:Int = MemLengthArray.max

		var memType:String = "single-port"
		if(!SPTF){
			memType = "dual-port"
		}	

		if (verboseTF){
			println(Console.BLUE + Console.BOLD + "\n//////////////////////// Pass 2 ////////////////////////\n")
			println(Console.BLUE + Console.BOLD + "\nMaximum # of "+memType+" RAM banks: " + maxbanks + "")
			println(Console.BLUE + Console.BOLD + "Maximum length of "+memType+" RAM banks: " + maxlen + "\n")

		}

		memBankLenArray = new Array[Int](maxbanks)

		// Go through all FFT sizes to figure out what is the worst case memory length needed for each bank
		for (i <- 0 until fftSizes.count){
			var banks:Int = 0
			var memLen:Int = 0

			banks = MemBankArray(i)
			memLen = MemLengthArray(i)
			
			// For all banks needed for a specific FFTN, update with new length if larger
			for (j <- 0 until banks){
				if (memLen > memBankLenArray(j)){
					memBankLenArray(j) = memLen
				}
			}
		}

		println(Console.BLUE + Console.BOLD + "\nBank lengths for "+ Console.MAGENTA + memType+ Console.BLUE + " memory: [" + memBankLenArray.mkString(", ") + "]\n" + Console.RESET)

	}

//////////////////////////////////////////////////////////////////////////////////////
// Remove element from List
	def remove(li:List[Int],elem:Int) : List[Int] = {
		return (li diff List(elem))
	}

//////////////////////////////////////////////////////////////////////////////////////
// Pack memory into fewest # of banks while maintaining FFTN/#banks where applicable

	def packMem(SPTF:Boolean,minBankTF:Boolean,verboseTF:Boolean) : Unit = {

		if (verboseTF){
			println(Console.BLUE + Console.BOLD + "\n//////////////////////// Successful Memory Allocation ////////////////////////\n")
			printf(Console.BLUE + Console.BOLD + "\n[FFTN] \t[MaxR] \t[#BFs] \t[#BGp] \t[#Bnk] \t[MLen] ")
			printf("\t[Radix Stages]\n")
		}

		for(i <- 0 until fftSizes.count){
			// Get leftover radices used after 1 element = max radix is removed from the list
			// New # of banks = max radix * (one of the leftover radices) to keep FFTN divisible by # of banks (minimize wasted memory slots)
			var temp:List[Int] = remove(radixStagesArray(i),maxRadixArray(i)).distinct.sorted
			
			// Note when only 1 butterfly is needed, # of banks won't change; otherwise, find among the remaining factors, a value >= # of butterflies
			val parallelBF:Int = optBFNumArray(i)
			var uniqueBankGroups:Int = 0
			if (parallelBF != 1 && !SPTF){
				temp = temp.filter(_>=parallelBF)
				// Get minimum/maximum factor that meets criterion to have # of banks
				if (!temp.isEmpty){
					if (minBankTF){
						uniqueBankGroups = temp.min
					}
					else{
						uniqueBankGroups = temp.max
						println(Console.RED + Console.BOLD + "\nMore banks currently not supported. :( Please select minimum bank requirement! \n")
						exit()
					}	
				}
				else{
					println(Console.RED + Console.BOLD + "\nUpdate to generator needed to complete calculation in N cycles (if possible) for N = "+fftSizes.fftSizeArray(i)+". Currently, # of banks for a given FFTN = maxRadix * (A prime factor of {FFTN/maxRadix}).\n")
					exit()
				}
			}
			else if(parallelBF == 1 && !SPTF){
				uniqueBankGroups = 1
			}

			// # of unique bank groups should always be = last radix stage in DIF FFT calculation
			var usedRadices = generalConstants.validRadices
			usedRadices = remove(usedRadices.toList,uniqueBankGroups).toArray
			// Case out 4,2 separately (need to be grouped together for PFA)
			// Note that 2 needs to be after 4 to minimize # of twiddles
			if (uniqueBankGroups == 4 && generalConstants.validRadices.contains(2)){
				usedRadices = remove(usedRadices.toList,2).toArray
			}
			else if (uniqueBankGroups == 2 && generalConstants.validRadices.contains(4)){
				usedRadices = remove(usedRadices.toList,4).toArray
				usedRadices = usedRadices :+ 4
			}
			if (uniqueBankGroups != 1){
				usedRadices = usedRadices :+ uniqueBankGroups
			}
			if (uniqueBankGroups == 4 && generalConstants.validRadices.contains(2)){
				usedRadices = usedRadices :+ 2
			}

			reorderedRadicesArray = reorderedRadicesArray :+ usedRadices

			// Reorder radix stages
			var radixStages: List[Int] = List.empty[Int]
			for (j <- 0 until usedRadices.length){
				val radix:Int = usedRadices(j)
				val radIdx:Int = generalConstants.validRadices.indexOf(radix)
				val numPow = generalConstants.numPowerArray(i)(radIdx)
				if (numPow != 0){
					for (k <- 0 until numPow){
						radixStages = radixStages :+ radix
					}
				}
			}

			reorderedRadixStagesArray = reorderedRadixStagesArray :+ radixStages

			val FFTN:Int = fftSizes.fftSizeArray(i)
			val maxRadix = maxRadixArray(i)

			var numBanks:Int = 0
			
			numBanks = maxRadix * uniqueBankGroups
			// Sometimes, even though 2 butterflies are needed, there is no radix-2 stage
			// But can evenly break 4 into sets of 2
			if(minBankTF && uniqueBankGroups == 4 && parallelBF == 2){
				numBanks = maxRadix * 2
				uniqueBankGroups = 2
			}
			
			uniqueBankGroupsArray = uniqueBankGroupsArray :+ uniqueBankGroups

			val memLen:Int = FFTN/numBanks


			MemLengthArray = MemLengthArray :+ memLen
			MemBankArray = MemBankArray :+ numBanks

			if (verboseTF){
				printf(Console.RESET + Console.BLUE + "[" + FFTN + "] \t")
				printf("[" + maxRadix + "] \t")
				printf("[" + parallelBF + "] \t")
				printf("[" + uniqueBankGroups + "] \t")
				printf("[" + numBanks + "] \t")
				printf("[" + memLen + "] \t")
				printf("" + radixStages.mkString("\t") + "\t")
				printf("\n")
			}
		}
		if (verboseTF){
			printf(Console.BLUE + Console.BOLD + "[FFTN] \t[MaxR] \t[#BFs] \t[#BGp] \t[#Bnk] \t[MLen] ")
			printf("\t[Radix Stages]\n")
		}
	}

//////////////////////////////////////////////////////////////////////////////////////
// Test schedule
// Note, banks conflict maximum of 1 time in each butterfly group prior to repair
// Pass 1: Try to fix
// Pass 2: Print out error

	var banks:Array[List[List[Int]]] = Array.empty[List[List[Int]]] 

	def test(idx:Int,pass:Int,verboseTF:Boolean) : Unit = {

		val radixStages:Array[Int] = reorderedRadixStagesArray(idx).toArray
		val numStages:Int = numStgArray(idx)
		val maxRadix:Int = maxRadixArray(idx)
		val bankGroups:Int = uniqueBankGroupsArray(idx)
		val lastRadix:Int = radixStages.last
		val numBFs:Int = optBFNumArray(idx)

		val FFTN:Int = fftSizes.fftSizeArray(idx)

		// Go through default scheduling if first pass (normal routine assuming scheduling works)
		if (pass == 1){

			banks = Array.empty[List[List[Int]]]

			// Go through all stages required for FFT
			for (currStg <- 0 until numStages){

				val currRadix:Int = radixStages(currStg)

				if (verboseTF){
					println(Console.BLUE + Console.BOLD + "Current Stage = " + currStg + "\t Current Radix = " +currRadix+ Console.RESET + Console.BLUE)
				}

				// Second-to-last stage count up flag
				var UPTF:Boolean = true

				// From radix stages, determine counter max values
				var maxStgCnt: Array[Int] = radixStages.map(_ - 1)
				// Count values for radix associated with current stage are separately brought out
				maxStgCnt(currStg) = 0
				// Counters (initialized to 0)
				var count: Array[Int] = new Array[Int](numStages)

				// Nested for loop counting up, starting with last stage counter
				// until all counts are maxed out
				banks = banks :+ getBanks(List.empty[List[Int]],currRadix,maxRadix,count,currStg,numStages,bankGroups,lastRadix,verboseTF)
				
				// If # of stages = 1, then you have only 1 radix-r butterfly operation
				if(numStages != 1){
					do{
						// Increment counters, starting with right-most
						// Previous counters (x-1) only incremented if counters to the right have wrapped
						breakable{
							for (x <- numStages-1 to 0 by -1){
								if (count(x) == maxStgCnt(x)){
									count(x) = 0
								}
								else{
									count(x) = count(x) + 1
									break
								}
							}
						}

						// NOT used
						// If on the last stage, to eliminate conflict, alternate 0 to max_count (up) and max_count to 0 (down) for second-to-last stage counter
						// Note that 0 2 2 0 and 1 0 0 0 result in the same banks (if mod 3) and they occur right after each other :(
						var countNew = count.clone()
						if (currStg == numStages-1){
							// Prevent exception
							if (currStg != 0){
								if (!UPTF){
									countNew(numStages-2) = (radixStages(numStages-2)-1)-count(numStages-2)
								}
								// Flip counter
								if (count(numStages-2) == radixStages(numStages-2)-1){
									//UPTF = !UPTF
								}
							}
						}

						banks(currStg) = getBanks(banks(currStg),currRadix,maxRadix,countNew,currStg,numStages,bankGroups,lastRadix,verboseTF)
						
					}while(!maxStgCnt.sameElements(count))
				}
			}

			if (verboseTF){
				printf("\n")
			}
		}		

		// Testing to see that butterfly groups don't have bank conflicts
		// i.e. the values in "banks" represent the banks needed per butterfly calculation at time t = index of banks
		// The for loop below groups multiple time indices together, as dictated by numBFs, and checks for bank conflicts
		// when multiple butterflies are operated in parallel
		for (z <- 0 until numStages){
			val numCalcs:Int = banks(z).length
			for (x <- 0 until numCalcs by numBFs){
				var usedBanks:Array[Int] = Array.empty[Int]
				for (y <- 0 until numBFs){
					// Prevent exception
					if (x+y < numCalcs){
						usedBanks = usedBanks ++ banks(z)(x+y)	
					}
				}

				val distinctUsedBanks = usedBanks.distinct
				// Indicates bank conflict! (i.e. distinctUsedBanks is smaller)
				if (distinctUsedBanks.length != usedBanks.length){
					// Only error if it fails on the second pass; try to fix error on the first pass!
					if(pass == 2){

						val errorString:String = Console.RED+Console.BOLD+"FFTN = "+FFTN+"\tStage = "+z+"/"+(numStages-1)+"\tFailed Butterfly Iteration = "+x+" to "+(x+numBFs-1)+"\t Banks = ["+usedBanks.mkString(",")+"]"
						println(errorString)
						failureReport = failureReport :+ errorString

						scheduleFailed = scheduleFailed :+ FFTN
					}

					// For each bank, counts # of times it appears in a group
					var duplicatedBanks:Array[Int] = Array.empty[Int]
					val bankCount = usedBanks.groupBy(identity).mapValues(_.size)
					for((key,value) <- bankCount){
						// Bank #, # of occurrences
						//println(key + "-->" + value)

						// If any bank occurs more than twice, there are multiple conflicts within a group! (bad)
						if (value > 2){
							println(Console.RED + Console.BOLD + "Multiple conflicts within a group. Currently only 1 supported i.e. bank 1 only appears max 2 times. :(")
							exit()
						}
						// Keep track of banks that are used twice in a group
						else if (value == 2){
							duplicatedBanks = duplicatedBanks :+ key
						}
					} 

					var firstDuplicateFlag:Boolean = false
					// Go through butterfly groups
					for (y <- 0 until numBFs){
						// Prevent exception
						if (x+y < numCalcs){
							val currBankSet = banks(z)(x+y).toArray
							val intersection = duplicatedBanks.intersect(currBankSet)	
							val numIntersect = intersection.length
							val numDuplicates = duplicatedBanks.length
							// If only some (but not all) bank #s of current butterfly are duplicated, that means that the pattern is less uniform :(
							if (numIntersect > 0 && numIntersect < numDuplicates){
								println(Console.RED + Console.BOLD + "Not all banks used by a butterfly in the group conflict, but some do :(")
								//println("Duplicates: " + duplicatedBanks.mkString(","))
								//println("Intersect: " + intersection.mkString(","))
								exit()
							}
							// If all conflicting banks for a butterfly conflict with all respective banks for another butterfly in the same group
							else if (numIntersect == numDuplicates && firstDuplicateFlag == false){
								// Number of conflicting banks for current butterfly != all banks needed by butterfly (some banks not conflicting)
								if (numDuplicates < currBankSet.length){
								}
								else{
									// Get bank order to see if the bank orders are the same the second time around
									duplicatedBanks = currBankSet
								}
								firstDuplicateFlag = true
							}
							// Second occurrence of duplicated banks
							else if (numIntersect == numDuplicates && firstDuplicateFlag == true){
								
								// Second set of duplicate banks not in the same order (or not all banks duplicated)
								// Note that this seems to be the result of 4->2 and 2->4 group conversion
								if(!duplicatedBanks.sameElements(currBankSet)){
									//println(Console.RED + Console.BOLD + "Conflicting butterfly banks don't occur in the same input order\n")
									//exit()
								}















// individual butterfly won't fail (always group of 2 or more)
								// Try to correct only on 1st pass
								if (pass == 1){
									var swapIdx:Int = x + numBFs + 1
									// Swap 2nd set of conflicting banks with banks from 1st butterfly in the next group
									// Swapping must be done within the same stage
									if (swapIdx < numCalcs){
										var swapBankSet = banks(z)(swapIdx)
										val currStageBanks = banks(z).toArray

										//println(swapBankSet.mkString(".")+":"+currStageBanks(x+y).mkString(","))
										// on second element, so x+y-1 should be in same group
										if (!swapBankSet.intersect(currStageBanks(x+y-1)).isEmpty){
											if (numBFs == 3)
												swapIdx = swapIdx+1
											else
												swapIdx = swapIdx-1
											swapBankSet = banks(z)(swapIdx)
										}
										
										currStageBanks(x+y) = swapBankSet
										currStageBanks(swapIdx) = currBankSet.toList
										banks(z) = currStageBanks.toList


										//banks(z)(x+y) = banks(z)(firstNextIdx).clone()
										//banks(z)(firstNextIdx) = currBankSet.toList

									}
									else{
										println(Console.RED + Console.BOLD + "Conflict at end of stage. Not supported :(")
										exit()
									}
								}
			
								// greatest distance to search? -- 2? only check first 2 #'s?





















							}
							// Otherwise, banks are unique
						}
					}

				}
			}	
		}

	}

//////////////////////////////////////////////////////////////////////////////////////
// Get banks from counter values

	// Facilitates real-time schedule conflict resolution
	var conflictFlag:Boolean = false
	var bankCache:Array[Int] = Array.empty[Int]
	var countsCache:Array[Int] = Array.empty[Int]
	var lastConflict:Int = -3*generalConstants.validRadices.max

	def getBanks(banks:List[List[Int]],currRadix:Int,maxRadix:Int,count:Array[Int],currStg:Int,numStages:Int,bankGroups:Int,lastRadix:Int,verboseTF:Boolean) : List[List[Int]] = {

		// Get normal banking = (n1+n2+n3...)%maxRadix i.e. single butterfly banking
		// Parallelize banks from current stage radix
		var banks1: List[Int] = List.empty[Int]
		var banks2: List[Int] = List.empty[Int]
		for (i <- 0 until currRadix){
			if (i == 0){
				banks1 = banks1 :+ (count.sum)%maxRadix
			}
			else{
				banks1 = banks1 :+ (banks1(i-1) + 1)%maxRadix
			}
		}
	
		// Get actual banking scheme = bank1 + n_last*maxRadix
		// Need to account for parallelization (in last stage)
		if (currStg != numStages-1){

			// If last radix is 4, but # of bank groups required is 2, map 0 to 3 to (0 to 3)%2 i.e. 8000
			if (bankGroups == 2 && lastRadix == 4){
				banks2 = banks1.map(_ + maxRadix*(count(numStages-1)%2))
			}
			// If last radix is 2, but # of bank groups required is 4, then need to borrow LSB from second-to-last count (from radix-4 stage) i.e. 8192
			else if (bankGroups == 4 && lastRadix == 2 && currStg != numStages-2){
				banks2 = banks1.map(_ + maxRadix*(2*(count(numStages-2)%2) + count(numStages-1)))
			}
			else if (bankGroups == 4 && lastRadix == 2 && currStg == numStages-2){
				// Account for parallelization in second-to-last stage
				for(i <- 0 until currRadix){
					banks2 = banks2 :+ (banks1(i) + maxRadix*(2*(i%2) + count(numStages-1)))
				}
			}
			else{
				banks2 = banks1.map(_ + maxRadix*count(numStages-1))
			}

		}
		// last stage
		else{
			// Don't need to do anything fancy if for simple radix butterfly
			if (numStages == 1){
				banks2 = banks1
			}
			else{
				for(i <- 0 until currRadix){
					if (bankGroups == 2 && lastRadix == 4){
						banks2 = banks2 :+ (banks1(i) + maxRadix*(i%2))
					}
					else if (bankGroups == 4 && lastRadix == 2){
						banks2 = banks2 :+ (banks1(i) + maxRadix*(2*(count(numStages-2)%2)+i))
					}
					else{
						banks2 = banks2 :+ (banks1(i) + maxRadix*i)
					}
				}
			}
		}

		// NOT used
		// If adjacent butterfly iterations have the same banks, there is a potential for conflict
		// Save bank values, and don't perform butterfly on second set of conflicting banks
		var adjacentSame: Boolean = false
		if (!banks.isEmpty){
			//adjacentSame = banks2.sorted.sameElements(banks(banks.length-1).sorted)
		}
		if(adjacentSame && !conflictFlag){

			// Only 1 conflict allowed within 2 sets of bank groups (conservative)
			if (abs(banks.length-lastConflict) < 1*bankGroups){
				println(Console.RED + Console.BOLD + "Too many conflicts within acceptable grouping of banks")
				exit()
			}

			conflictFlag = true
			bankCache = banks2.toArray.clone()
			countsCache = count.clone()
			return banks
		}
		// Sequence of 3 butterflies have the same bank (fail!)
		else if (adjacentSame && conflictFlag){
			println(Console.RED + Console.BOLD + "Sequence of 3 butterflies use the same banks! :(")
			exit()
		}
		else{
			if (verboseTF){
				printf("BF#: " + (banks.length) + "\t")
				printf("\tCounts: " + count.mkString("\t"))
				//printf("\t Banks1: " + banks1.mkString("\t"))
				printf("\t Banks2: " + banks2.mkString("\t") + "\n")

				// Add conflicting banks back in
				if(conflictFlag){
					printf("BF#: " + (banks.length+1) + "\t")
					printf("\tCounts: " + countsCache.mkString("\t"))
					//printf("\t Banks1: " + banks1.mkString("\t"))
					printf("\t Banks2: " + bankCache.mkString("\t") + "\n")	
				}
			}

			var banksTemp = banks :+ banks2
			// Add conflicting banks back in, clear conflict
			if (conflictFlag){
				banksTemp = banksTemp :+ bankCache.toList
				conflictFlag = false
				lastConflict = banks.length+1
			}
			return banksTemp
		}

	}

}
