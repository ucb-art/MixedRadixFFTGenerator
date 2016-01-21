// August 19, 2015

package FFT 
import Chisel.{Pipe =>_,Complex => _,Mux => _, _}
import DSP._
import scala.math._
import memBanks._
import calc._
import Count._
import generator._

import scala.reflect.runtime.universe._

import ChiselDSP.{Reg => _, when => _, _}

class FFT[T <: DSPQnm[T]](gen : => T) extends GenDSPModule (gen) {

 val wftaDly = 1
	val seqRdDly = 1

	override val io = new  IOBundle {

		// SETUP_INIT should be true when new FFT_INDEX, FFT is presented.
		// FFT_INDEX and FFT will be registered on the next rising clock edge
		// FFT = true -> FFT calculation; FFT = false -> IFFT calculation
		val SETUP_INIT = Bool(INPUT)
		val FFT_INDEX = UInt(INPUT,width=Helper.bitWidth(fftSizes.count-1))
		val FFT = Bool(INPUT)
		
		// SETUP_DONE = true -> Ready to take FFT data 
		val SETUP_DONE = Bool(OUTPUT)

		// START_FIRST_FRAME should be high when 1st Frame Data_0 is input
		val START_FIRST_FRAME = Bool(INPUT)

		// High when first data of a valid symbol is output i.e. k = 0 
		// Note that after the 1st symbol has been streamed to the FFT block, 
		// the corresponding FFT output begins at the start of the 3rd input symbol
		// (lags a full symbol - 1 cycle for input, 1 cycle for calculation, starts outputting on 3rd cycle)
		val FIRST_OUT = Bool(OUTPUT)
		
		//val DATA_IN = new Complex(SInt(width = SDR_FFT.dataBitWidth),SInt(width = SDR_FFT.dataBitWidth)).asInput
		//val DATA_OUT = new Complex(SInt(width = SDR_FFT.dataBitWidth),SInt(width = SDR_FFT.dataBitWidth)).asOutput
		val DATA_IN = Complex(gen,gen).asInput
		val DATA_OUT = Complex(gen,gen).asOutput
		// DATA_OUT should be valid at the start of the next frame (streaming)
		
	}

/////////////////////////////////////////////////////////////////////////////////////////////////////////////
// FFT SIZE-DEPENDENT CONSTANTS (FROM LUTS) SETUP 

	// Register inputs to FFT module 
	// clk 1
	val fftIndex = Reg(UInt(width=Helper.bitWidth(fftSizes.count-1))) 
	val fftTF = Reg(Bool())
	when(io.SETUP_INIT === Bool(true)){
		fftIndex := io.FFT_INDEX
		fftTF := io.FFT
	}
	debug(fftIndex)
	debug(fftTF)

////// Setup FFT length-dependent general constants
	// Final setup constants are all registered
	// Array columns broken into separate LUTs

	// Powers: a1, a2, b, c in 4^a1*2^a2*3^b*5^c
	// clk 2
	val numPowerArray = generalConstants.numPowerArray.transpose
	val powColCount = numPowerArray.length
	val numPowerLUT = Vec((0 until powColCount).map( x => Module( new UIntLUT(numPowerArray(x)) ).io ))
	val numPower = Vec.fill(powColCount){Reg(UInt())}
	for (i <- 0 until powColCount){
		numPowerLUT(i).addr := fftIndex
		numPower(i) := numPowerLUT(i).dout
		debug(numPower(i))
	}

	//  Coprimes: 2^N, 3^M, 5^k
	// clk 2
	val coprimesArray = generalConstants.coprimesArray.transpose
	val coprimesColCount = coprimesArray.length
	val coprimesLUT = Vec((0 until coprimesColCount).map( x => Module( new UIntLUT(coprimesArray(x)) ).io ))
	val coprimes = Vec.fill(coprimesColCount){Reg(UInt())}
	val coprimesFlipped = Vec.fill(coprimesColCount){Reg(UInt())}
	for (i <- 0 until coprimesColCount){
		coprimesLUT(i).addr := fftIndex
		coprimes(i) := coprimesLUT(i).dout
		coprimesFlipped(coprimesColCount-1-i) := coprimesLUT(i).dout
		debug(coprimes(i))
		debug(coprimesFlipped(i))
	}

	// Ex: sum(0) = power(0)
	// sum(1) = power(0)+power(1)
	// sum(2) = sum(1) + power(2)
	// Keeps track of # of stages required up until current radix
	// Where the last array value represents total # of stages required for FFT calc
	// clk 3 : Note combinational logic registered after everything is completed
	val stageSumTemp = Vec.fill(powColCount){UInt()}
	val stageSum = Vec.fill(powColCount){Reg(UInt())}
	val stageSumM1 = Vec.fill(powColCount){UInt()}
	for ( j <- 0 until powColCount){
		if (j == 0){
			stageSumTemp(0) := numPower(0)
		}
		else if (j == powColCount-1){
			stageSumTemp(powColCount-1) := UInt(stageSumTemp(powColCount-2)+numPower(powColCount-1),width=Helper.bitWidth(generalConstants.maxNumStages))
		}
		else{
			stageSumTemp(j) := stageSumTemp(j-1)+numPower(j)
		}
	}
	stageSum := stageSumTemp
	for (i <- 0 until powColCount){
		stageSumM1(i) := stageSum(i)-UInt(1)
		debug(stageSum(i))
	}

	// Max radix for current FFT calculation (see generalConstants for logic explanation)
	// clk 3
	val maxRadixTemp = Vec.fill(powColCount){UInt(width=Helper.bitWidth(generalConstants.maxRadix))}
	val maxRadix = Reg(UInt(width=Helper.bitWidth(generalConstants.maxRadix)))
	for (i <- powColCount-1 to 0 by -1){
		if (i == powColCount-1){
			when(numPower(powColCount-1) != UInt(0)){
				maxRadixTemp(powColCount-1) := UInt(generalConstants.validRadices(powColCount-1))
			} .otherwise{
				maxRadixTemp(powColCount-1) := UInt(0)
			}
		}
		else{
			when((numPower(i) != UInt(0)) && (maxRadixTemp(i+1) === UInt(0))){
				maxRadixTemp(i) := UInt(generalConstants.validRadices(i))
			} .otherwise{
				maxRadixTemp(i) := maxRadixTemp(i+1)
			}	
		}
	}
	if (generalConstants.pow2SupportedTF && generalConstants.rad4Used){					// radix-4 is always first
		when ((numPower(0) != UInt(0)) && (UInt(4) > maxRadixTemp(0))){
			maxRadix := UInt(4)
		}.otherwise{
			maxRadix := maxRadixTemp(0)
		}
	}
	else{
		maxRadix := maxRadixTemp(0)
	}
	debug(maxRadix)
		
	// Determine radix of each stage 
	// If stage # < stageSum(i), the corresponding radix can be found from validRadices(i)	
	// clk 4
	val stageRadix = Vec.fill(generalConstants.maxNumStages){Reg(UInt(width=Helper.bitWidth(generalConstants.maxRadix)))}
	val maxStageCount = Vec.fill(generalConstants.maxNumStages){Reg(UInt(width=Helper.bitWidth(generalConstants.maxRadix-1)))}
	for (i <- 0 until generalConstants.maxNumStages){
		maxStageCount(i) := UInt(0)					// only if other conditions not met (priority dependent)
		stageRadix(i) := UInt(0)
		for (j <- powColCount-1 to 0 by -1){
			when (UInt(i) < stageSum(j)){
				stageRadix(i) := UInt(generalConstants.validRadices(j))
				maxStageCount(i) := UInt(generalConstants.validRadices(j)-1)
			}
		}
		debug(stageRadix(i))
		debug(maxStageCount(i))
	}	
	
	// Generate IO + Calculation memory address constants
	// Ex: for 300 = 4*3*5*5, Address = 15n1 + 5n2 + (1n3 + 0n4)
	// Note that because of banking with maxRadix banks (in the simplest case),
	// each memory requires N/maxRadix addresses. This is accomplished
	// by zero-ing the address constant associated with the max radix (right-most unless
	// the max radix is 4, then left-most). You generate address constants starting
	// from the right. The first non-zero address constant = 1.
	// The second non-zero address constant is the radix
	// associated with the first non-zero address constant. The third is the previous
	// address constant (from the right) * the previous radix. See the trend. 
	// Address "grows" from right to left
	// clk 5
	val addressConstantTemp = Vec.fill(generalConstants.maxNumStages){Reg(UInt())}      // angie's noob piped
	val addressConstant = Vec.fill(generalConstants.maxNumStages){Reg(UInt(width=Helper.bitWidth(pow(generalConstants.maxRadix,generalConstants.maxNumStages-2).toInt)))}
	// To save logic, handle edge cases separately
	// Chisel gives weird error with nested when -- need to specify initial condition
	// Handle in-between cases
	val highStageIndex = stageSumM1(powColCount-1)
	for (i <- generalConstants.maxNumStages-1 to 0 by -1){
		if (i == generalConstants.maxNumStages-1){
			addressConstantTemp(generalConstants.maxNumStages-1) := UInt(0,width=1)	
			when (stageRadix(generalConstants.maxNumStages-1) === UInt(0)) {					
				addressConstantTemp(generalConstants.maxNumStages-1) := UInt(0,width=1)					// Unused stage
			}
			.otherwise{
				when (stageRadix(generalConstants.maxNumStages-1) === maxRadix) {
					addressConstantTemp(generalConstants.maxNumStages-1) := UInt(0,width=1)
				}
				.otherwise{
					addressConstantTemp(generalConstants.maxNumStages-1) := UInt(1,width=1)
				}		
			}
		}
		else if (i == 0){
			// Handle left-most stage -- some edge cases
			when ((stageRadix(0)===maxRadix) && (stageRadix(highStageIndex) != maxRadix)){			// Note if 4x4, the right-most 
																									// address constant is zero-ed first
				addressConstantTemp(0) := UInt(0,width=Helper.bitWidth(pow(generalConstants.maxRadix,generalConstants.maxNumStages-2).toInt))
				// handles case when only 1 stage (AC = 0) i.e. 4
		        // handles 4x3, 4x3x3, 4x4x3, 5
			}
			.elsewhen (stageSum(powColCount-1) === UInt(2)){										// # of used stages
				addressConstantTemp(0) := UInt(1,width=Helper.bitWidth(pow(generalConstants.maxRadix,generalConstants.maxNumStages-2).toInt))
				// For 2 stages with N1 = N2, N2 AC already 0ed so N1 AC = 1 i.e. 4x4
		        // handles 2x3, 4x5, 3x3, 3x5, 5x5
			}
			.otherwise {
				addressConstantTemp(0) := UInt(stageRadix(1)*addressConstantTemp(1),width=Helper.bitWidth(pow(generalConstants.maxRadix,generalConstants.maxNumStages-2).toInt))
				// when first, last are same radix and have more than just N1 = N2 (>2 stages)
		        // handles 4x4x4, 4x4x5, 2x3x3, 3x3x3, 3x5x5, 5x5x5
			}
		}
		else{
			when (stageRadix(i) === UInt(0)) {														// unused stage
				addressConstantTemp(i) := UInt(0,width=Helper.bitWidth(pow(generalConstants.maxRadix,generalConstants.maxNumStages-1-i).toInt))													
			}
			.elsewhen (UInt(i) === highStageIndex) {
				when (stageRadix(i) === maxRadix) {
					addressConstantTemp(i) := UInt(0,width=Helper.bitWidth(pow(generalConstants.maxRadix,generalConstants.maxNumStages-1-i).toInt))
				}
				.otherwise{
					addressConstantTemp(i) := UInt(1,width=Helper.bitWidth(pow(generalConstants.maxRadix,generalConstants.maxNumStages-1-i).toInt))
				}		
			}
			.elsewhen (UInt(i) === stageSumM1(powColCount-1)-UInt(1)) {
				when (addressConstantTemp(i+1) === UInt(1)) {
					addressConstantTemp(i) := UInt(stageRadix(i+1),width=Helper.bitWidth(pow(generalConstants.maxRadix,generalConstants.maxNumStages-1-i).toInt))
				}
				.otherwise{
					addressConstantTemp(i) := UInt(1,width=Helper.bitWidth(pow(generalConstants.maxRadix,generalConstants.maxNumStages-1-i).toInt))
				}		
			}
			.otherwise{
			  val tx = UInt(stageRadix(i+1)*addressConstantTemp(i+1),width=Helper.bitWidth(pow(generalConstants.maxRadix,generalConstants.maxNumStages-1-i).toInt))
				addressConstantTemp(i) := tx //pipeD(tx,1).asInstanceOf[UInt]
			}
		}
	}
	addressConstant := addressConstantTemp
	for (i <- 0 until generalConstants.maxNumStages){
		debug(addressConstant(i))
	}

////// Setup FFT length-dependent address constants
	// Final setup constants are all registered
	// Array columns broken into separate LUTs

	// IO Q' LUTS
	// clk 2
	val qDIFiArray = ioAddressConstants.qDIFiArray.transpose
	val qColCount = qDIFiArray.length
	val qDIFiLUT = Vec((0 until qColCount).map( x => Module( new UIntLUT(qDIFiArray(x)) ).io ))
	val qDIFi = Vec.fill(qColCount){Reg(UInt())}
	val qDIFoArray = ioAddressConstants.qDIFoArray.transpose
	val qDIFoLUT = Vec((0 until qColCount).map( x => Module( new UIntLUT(qDIFoArray(x)) ).io ))
	val qDIFo = Vec.fill(qColCount){Reg(UInt())}
	for (i <- 0 until qColCount){
		qDIFiLUT(i).addr := fftIndex
		qDIFi(i) := qDIFiLUT(i).dout
		qDIFoLUT(i).addr := fftIndex
		qDIFo(i) := qDIFoLUT(i).dout

		debug(qDIFo(i))
		debug(qDIFi(i))
	}

////// Setup FFT length-dependent twiddle constants
	// Final setup constants are all registered
	// Array columns broken into separate LUTs

	// Max twiddle address counter values for each stage
	// Note right most count always = 0
	// clk 2
	val twiddleCountArray = twiddleConstants.twiddleCountMaxArray.transpose
	val twiddleCountColCount = twiddleCountArray.length
	val twiddleCountLUT = Vec((0 until twiddleCountColCount).map( x => Module( new UIntLUT(twiddleCountArray(x)) ).io ))
	val twiddleCount = Vec.fill(twiddleCountColCount){Reg(UInt())}
	var maxTwiddleCountBitWidth:Int = 0
	for (i <- 0 until twiddleCountColCount){
		twiddleCountLUT(i).addr := fftIndex
		twiddleCount(i) := twiddleCountLUT(i).dout
		debug(twiddleCount(i))

		var twiddleCountBitWidth = Helper.bitWidth(twiddleCountArray(i).max)
		if (twiddleCountBitWidth > maxTwiddleCountBitWidth){
			maxTwiddleCountBitWidth = twiddleCountBitWidth
		}
	}

	// For N = 32 = 4*4*2, 
	// Stage 1: Count 0 to 32/4-1 for radix-4 = 8 count * 4 radix
	// Stage 2: Count 0 to 32/4/4-1 for radix-4 => 2 count * 4 radix * repeat 4x (for already calculated stage)
	// Stage 3: Count 0 to 32/4/4/2-1 for radix 2 => 1 count * radix 2 * repeat 16x (for already calculated stages)
	// For N = 24 = 4*2*3
	// Note for N = 8 = 4*2, Stage 1: Count 0 to 8/4-1 and Stage 2: Count 0 to 8/4/2-1
	// But for 24, 
	// Stage 1: count 0 (repeat 3x = hold count for 3 clks) to 1 (repeat 3x = hold count for 3 clks)  
	// => count 2 * repeat 3 (hold) * radix-4 = 24 points
	// Stage 2:Count 0 to 0 (repeat 3x) => count 1 * repeat 3x (hold) * radix 2 * repeat 4x (prev. stage) = 24 points
	// Stage 3: Count 0 to 0 repeat 8x = count 1 * radix 3 * repeat 8x (prev. stages) = 24.
	// NOTE THAT for stage 3, repeat 8x is not the same as holding the count
	// Holding a particular count is due to waiting for subCounter to max out.
	// This sub counter has a max count defined by the product of coprimes still
	// to be calculated (product of coprimes to the right of current coprime)
	// Right-most count always = 0 
	// clk 3
	val twiddleSubCountMaxTemp = Vec.fill(coprimesColCount){Reg(UInt())}    // poor man's pipeD
	val twiddleSubCountMax = Vec.fill(coprimesColCount){Reg(UInt(width=Helper.bitWidth(fftSizes.fftSizeArray.max)))}
	for (i <- coprimesColCount-1 to 0 by -1){
		if (i == coprimesColCount-1){
			twiddleSubCountMaxTemp(coprimesColCount-1) := UInt(0,width=1)	
		}
		else if (i == coprimesColCount-2){
			twiddleSubCountMaxTemp(coprimesColCount-2) := coprimes(coprimesColCount-1)
		}
		else if (i == 0){
		  val tt = UInt(twiddleSubCountMaxTemp(i+1)*coprimes(i+1),width=Helper.bitWidth(fftSizes.fftSizeArray.max))
			twiddleSubCountMaxTemp(i) := tt //pipeD(tt,1).asInstanceOf[UInt]
		}
		else{
			twiddleSubCountMaxTemp(i) := twiddleSubCountMaxTemp(i+1)*coprimes(i+1) //pipeD(twiddleSubCountMaxTemp(i+1)*coprimes(i+1),1).asInstanceOf[UInt]
		}	
	}
	for (i <- 0 until coprimesColCount){
		if (i < coprimesColCount-1){
			twiddleSubCountMax(i) := twiddleSubCountMaxTemp(i) - UInt(1,width=1)		// Count max is -1 of product of coprimes
		}
		else{
			twiddleSubCountMax(i) := twiddleSubCountMaxTemp(i)
		}
		debug(twiddleSubCountMax(i))
	}

	// Base Twiddle Address Multiplier (Renormalize to Twiddle ROM size)
	// Unused signal should be optimized out in Verilog [ie Mul factor for powers of 2]
	// clk 3
	val radNTwiddleMulArray = twiddleConstants.RadNTwiddleMulFactorArray
	val radNTwiddleMulRowCount = radNTwiddleMulArray.length
	val radNTwiddleMulLUT = Vec((0 until radNTwiddleMulRowCount).map( x => Module( new UIntLUT(radNTwiddleMulArray(x)) ).io ))
	val radNTwiddleMul = Vec.fill(radNTwiddleMulRowCount){Reg(UInt())}
	for (i <- 0 until radNTwiddleMulRowCount){
		var ip:Int = 0
		if (generalConstants.pow2SupportedTF && generalConstants.rad4Used){	// If radix-4 used, the indices 0+1 of numPow will be associated with rad 2
			ip = i+1
		}
		else{
			ip = i
		}
		radNTwiddleMulLUT(i).addr := numPower(ip)							// power of 2 address = don't care (LUT value unused)
		if (i != 0){
			radNTwiddleMul(i) := radNTwiddleMulLUT(i).dout
		}
		else{
			if (generalConstants.pow2SupportedTF){							// If power of 2 is supported, will always be first
				val pow2mTemp = (log(generalConstants.maxCoprime(0))/log(generalConstants.validPrimes(0))).toInt
				val pow2m = UInt(pow2mTemp,width=Helper.bitWidth(pow2mTemp))
				val Num2 = UInt()
				if (generalConstants.rad4Used){
					Num2 := numPower(1)+(numPower(0) << UInt(1))			// Multiply count associated with 4 by 2 to get total 2 count
				}
				else{														// Only radix 2 used (no 4)
					Num2 := numPower(0)
				}
				val pow2 = UInt(pow2m-Num2)
				radNTwiddleMul(i):= UInt(1,width=1) << pow2					// Renormalize (reasoning in twiddleConstants)
	
			}
			else{
				radNTwiddleMul(i) := radNTwiddleMulLUT(i).dout
			}
		}
		debug(radNTwiddleMul(i))
	}

	// For DIF: Initial RadXTwiddleMulFactor due to scaling max memory
	// based off of max coprime N to the coprime N actually used (see address gen block)
	// Subsequent radix-X stages have the mul
	// factor modified to correspond to *R1 for stage 2, *R1*R2 for
	// stage 3, etc. as seen above
	// clk 5
	val twiddleMulTemp = Vec.fill(generalConstants.maxNumStages){Reg(UInt())}
	val twiddleMul = Vec.fill(generalConstants.maxNumStages){Reg(UInt())}
	// Note that whenever a radix-2 stage is used, regardless of the Mul value, the
	// final twiddle address will always be 0 
	for (i <- 0 until generalConstants.maxNumStages){
		if (i == 0){																// Always the start of a new radix, unless
																					// radix-4 is allowed but there is no radix-4 used in this FFT N
																					// i.e. if the first stage is radix-2, don't care
			
			twiddleMulTemp(0) := radNTwiddleMul(radNTwiddleMulRowCount-1)
			for (j <- radNTwiddleMulRowCount-2 to 0 by -1){
				if (generalConstants.pow2SupportedTF && generalConstants.rad4Used){
					when (stageSum(j+1) != UInt(0)){								// Radix {(42)35} -> Primes{235} re-index
						twiddleMulTemp(0) := radNTwiddleMul(j)
					}
				}
				else{
					when (stageSum(j) != UInt(0)){									// Re-index not needed
						twiddleMulTemp(0) := radNTwiddleMul(j)
					}
				}
			}	
		}
		else{
			when (stageRadix(i) === UInt(0) ){										// Unused stages (or radix-2, but ddon't care)										
				twiddleMulTemp(i) := UInt(0)
			}.elsewhen(stageRadix(i) != stageRadix(i-1)){							// Current radix is different from previous radix --> need new Mul base
				twiddleMulTemp(i) := radNTwiddleMul(radNTwiddleMulRowCount-1)
				for (j <- radNTwiddleMulRowCount-2 to 1 by -1){ 					// If Rad2 (first) base mul was relevant, it would have already been used in stage 0
					if (generalConstants.pow2SupportedTF && generalConstants.rad4Used){
						when (stageSum(j) === UInt(i)){								// Radix {(42)35} -> Primes{235} re-index
							twiddleMulTemp(i) := radNTwiddleMul(j)
						}
					}
					else{
						when (stageSum(j-1) === UInt(i)){							// Shift index compared to before
							twiddleMulTemp(i) := radNTwiddleMul(j)
						}
					}
				}	
			}.otherwise{
				twiddleMulTemp(i) := twiddleMulTemp(i-1)*stageRadix(i) //pipeD(twiddleMulTemp(i-1)*stageRadix(i),2).asInstanceOf[UInt] 				// If current stage has the same radix as the previous stage,
																					// Change the multiplication factor * radix
			}
		}
	}
	twiddleMul := twiddleMulTemp
	for (i <- 0 until generalConstants.maxNumStages){
		debug(twiddleMul(i))
	}

	// Counter reset whenever new FFTN desired, stays constant after setup is done SHOULD OPTIMIZE
	val setupDoneCount: Int = 4	 + generalConstants.maxNumStages*3
	val setupCounter = Module(new accumulator(Helper.bitWidth(setupDoneCount)))
	val setupDoneTemp = (UInt(setupDoneCount) === setupCounter.io.out)
	setupCounter.io.inc := UInt(1,width=1)
	setupCounter.io.changeCond := ~setupDoneTemp
	setupCounter.io.globalReset := io.SETUP_INIT
	setupCounter.io.wrapCond := Bool(false)

	val setupDoneTempD1 = Reg(next = setupDoneTemp)
	io.SETUP_DONE := setupDoneTemp || setupDoneTempD1								// Hold for 2 cycles


////////////////////////////////////////////////////////////////////////////////////////////////////////
// IO Addressing


	// Slow clock for IO (vs. fast clock for calculations)
	val slwClkCnt = Reg(init = UInt(0,width=1))
	when (io.START_FIRST_FRAME){
		slwClkCnt := UInt(1)
	}
	.otherwise{
		slwClkCnt := ~slwClkCnt
	}
	val slwClkEn = (slwClkCnt===UInt(0))						// = 0 next clock cycle after START_FIRST_FRAME high
	val ioWriteFlag = slwClkEn
	// debug(slwClkEn)










  val qDIFiTbl = ioAddressConstants.qDIFiArray.toList.transpose.map(x => x.toList)									// Columns
  val qDIFiLUTs = qDIFiTbl.zipWithIndex.map{case (x,i) => {
      val temp = Params.getIO.primes(i)
      val rad = if (temp == 2) 4 else temp
      DSPModule(new BaseNLUT(x, rad = rad))
	}}
  val qDIFis =Vec(qDIFiLUTs.zipWithIndex.map{ case(x,i) => {
    x.io.addr := DSPUInt(fftIndex, Params.getFFT.sizes.length - 1)
    val tempOut = x.io.dout.cloneType
    tempOut := x.io.dout
    //Reg(tempOut) weirdest reg problem ever/!??!
    BaseN(tempOut.map(x => x.reg()), tempOut.rad)
  }})
  // debug list of stuff
  // CHANGE ALL REG TO PIPE -- should there be an enclosing vec? for all iterables
	val coprimeCols = Params.getIO.coprimes.transpose
	// Always have at least 1 'digit' even when a particular prime isn't used
	val primeDigitsTbl = coprimeCols.zipWithIndex.map { case (x, i) =>
		x.map(y => {
      // take max count allowed to determine how many digits need to be represented
			val temp = (y-1)
      // seems i use length pretty frequently how about wrapper function?
			BaseN.toIntList(temp, Params.getIO.primes(i)).length
		})
	}
	val primeDigitsLUT = DSPModule(new IntLUT2D(primeDigitsTbl.transpose))
	primeDigitsLUT.io.addr := DSPUInt(fftIndex,Params.getFFT.sizes.length-1)
	val primeDigitsTemp = primeDigitsLUT.io.dout.cloneType
  primeDigitsTemp := primeDigitsLUT.io.dout
  // vec reg has issues?!?!?!
  val primeDigits = Vec(primeDigitsTemp.map(x => x.reg()))
	val (ioIncCounters,ioModCounters) = Params.getIO.maxCoprimes.zipWithIndex.map{case (x,i) => {
		val temp = Params.getIO.primes(i)
		// TODO: Generalize rad also reused? temp rad
		val rad = if (temp == 2) 4 else temp
		val c1 = BaseNIncCounter(rad = rad, maxCoprime = x, nameExt = "rad_" + rad.toString)
    val c2 = {
      if (x != Params.getIO.maxCoprimes.last)
        Some(BaseNAccWithWrap(rad = rad, maxCoprime = x, nameExt = "rad_" + rad.toString))
      else
        None
    }
    (c1,c2)
	}}.unzip

	// Right-most counter is "least significant"
	val ioIncCounts = Vec(ioIncCounters.zipWithIndex.map{ case (e,i) => {
		val iChange = if (e != ioIncCounters.last) ioIncCounters(i + 1).oCtrl.change else DSPBool(slwClkEn)
		e.iCtrl.change := iChange
		e.iCtrl.reset := DSPBool(io.START_FIRST_FRAME)
		// Should not need? can also trim to io.primDigits length
		e.io.primeDigits := primeDigits(i).shorten(e.io.primeDigits.getRange.max)
		val temp = e.io.out.cloneType
    temp := e.io.out
    temp
	}})
	// Do zip together
	val ioModCounts = Vec(ioModCounters.init.zipWithIndex.map{ case (etemp,i) => {
    val e = etemp.get
		e.iCtrl.change := DSPBool(slwClkEn)
		e.iCtrl.reset := DSPBool(io.START_FIRST_FRAME)
		// Should not need? can also trim to io.primDigits length
		e.io.primeDigits := primeDigits(i).shorten(e.io.primeDigits.getRange.max)
    e.io.inc.get := qDIFis(i).padTo(e.io.inc.get.length).asOutput // ???
		e.iCtrl.wrap.get := ioIncCounters(i).iCtrl.change
    val temp = e.io.out.cloneType
    temp := e.io.out
    temp
	}})
  // Should try to minimize mem output length and pad where there is width mismatch
  // CAN PIPELINE DELAY ioFinalConuts
  val ioFinalCounts = Vec(Params.getIO.maxCoprimes.zipWithIndex.map{case (x,i) => {
    if (x == Params.getIO.maxCoprimes.last) ioIncCounts(i)
    else (ioIncCounts(i) + ioModCounts(i)).maskWithMaxCheck(primeDigits(i))._1
  }})
  debug(ioFinalCounts)
  val iDIFtemp1 = Vec(ioFinalCounts.zipWithIndex.map{ case (x,i) => {
    if (Params.getBF.rad.contains(2) && Params.getIO.primes(i) == 2 && Params.getBF.rad.contains(4)){
      // switch to getBF.rad(i) == 2  ????
      // Convert to Mixed radix [4,...4,2] if current FFT size requires radix 2 stage & operating on 2^n coprime
      // FOR DIF 2 always follows 4 and it's the only one with list of length 2
      Mux(DSPBool(numPower(i+1)(0)),x.toRad42(),x)
    }
    else x
  }})
  // registered here?
	// Need to be same length to address properly (really Chisel should error)
	val colLengths = iDIFtemp1.map(x => x.length).max
  val iDIFtemp = Vec(iDIFtemp1.map(x => {
    val set = x.map(y => {y.reg()})
    BaseN(set, x.rad).padTo(colLengths)
		//Vec(set) // basen gets misinterpretted?
  }))
  iDIFtemp.foreach{debug(_)}
	// when doing addr, warn when not vec col nto same length











  // StageRadix === should be just series of Bools
	// table use2 YN? but otherwise lump stage cnt into 4 -- stage sum should not separate 4,2 (should be by coprime)
	// bad assumption here (i.e. 4 must be first)
	val newStageSum = {
		if (generalConstants.validRadices.contains(2) && generalConstants.validRadices.contains(4))
			Vec(stageSum.tail)
		else stageSum
	}
	debug(newStageSum)

  val ions = Vec((0 until generalConstants.maxNumStages).map (i => {
    val primeVal = {
			if (Params.getBF.rad.contains(4)) {
				// Mux has problems interpretting without doing explicit .toBool, etc.?
				Mux((stageRadix(i) === UInt(4)).toBool, UInt(2), stageRadix(i)).toUInt
			}
			else stageRadix(i)
		}
    val primes = Params.getIO.primes.zipWithIndex
		// Mutually exclusive conditions
		val primeIdx = primes.tail.foldLeft (
			DSPUInt(primes.head._2) ? DSPBool(primeVal === UInt(primes.head._1))
		)((b,a) => {
			val selIdx = DSPUInt(a._2) ? DSPBool(primeVal === DSPUInt(a._1))
			selIdx /| b
		})
		// Why all these explicit conversions?!
		// primeIdx defaults to 0 when prime unused (which has smallest sum value so muxes to 0 if unused)
		val currStageSum = newStageSum(primeIdx.toUInt).toUInt			// NOT MINUS 1 (since 0 - 1 = 3)
		val activeStage = (UInt(i) < currStageSum).toBool
		val digitIdx = (currStageSum-UInt(1+i)).toUInt
		// Addressing Vec should mark as used (override vec)

		val countSet = Vec(iDIFtemp(primeIdx.toUInt).map(x => x.toUInt))
		val digit = countSet(digitIdx).toUInt
		Mux(activeStage, digit, UInt(0)).toUInt
		// := auto pads if right range smaller?
		// Seems searchable vec needs to be made of uints only?

  }))
	ions.foreach{debug(_)}
	//separate mux??? don't need bc newstagesumm1(0) has smallest val
































































////// DIF INPUT
	// DIF I Counters
	val iDIFCounts = Vec.fill(coprimesColCount){UInt()}
	val iDIFCounters = Vec((0 until coprimesColCount).map( x => Module( new accumulator(Helper.bitWidth(generalConstants.maxCoprime(x)-1)) ).io ))
	val iDIFCountWrap = Vec.fill(coprimesColCount){Bool()}	
	for (i <- coprimesColCount-1 to 0 by -1){
		// Wrap after reached max count
		iDIFCountWrap(i) := (iDIFCounters(i).out === coprimes(i)-UInt(1))
		iDIFCounters(i).inc := UInt(1,width=1)
		// If right-most count, change only on slowClk = 0
		// Otherwise, only change when previous (right) counters are about to wrap around
		if (i == coprimesColCount-1){
			iDIFCounters(i).changeCond := slwClkEn
		}
		else{
			iDIFCounters(i).changeCond := iDIFCounters(i+1).changeCond && iDIFCountWrap(i+1) 
		}
		iDIFCounters(i).globalReset := io.START_FIRST_FRAME		// Reset counts to 0 on next clk cycle after START_FIRST_FRAME high
		iDIFCounters(i).wrapCond := iDIFCountWrap(i)
		iDIFCounts(i) := iDIFCounters(i).out
		debug(iDIFCounts(i))
	}
	
	

	
	// Derivation: N = N1N2N3
    // A1 = q1N2N3+1
    // n = (N2N3n1+A1n2~)modN = (N2N3(n1+q1n2~)+n2~)modN
    // n1' = (n1+q1n2~)modN1 == index in for loop n1p
    // Let q1' = N1-q1
    // Can show that n1 = (n1'+q1'n2~)modN1 =
    // [(n1+q1n2~)modN1+(N1-q1)n2~]modN1 where n1 in this case =
    // n1pp below
    // Also, A2 = _q2N3+1
    // n2~=(N3n2+A2n3)modN2N3=[N3(n2+n3q2)+n3]modN2N3
    // n2'=(n2+n3q2)modN2 == index in for loop n2p
    // Let q2'=N2-q2
    // Can show that n2 = (n2'+q2'n3)modN2 =
    // [(n2+n3q2)modN2+(N2-q2)n3]modN2 where n2 in this case =
    // n2pp below
    // Therefore n = (N2N3n1'+N3n2'+n3)modN
    // NOTE THAT QUSED = (N1-q1)modN1 and (N2-q2)modN2
    // n1pp = mod(n1p+qused(1)*(factorizationin(3)*n2p+n3p),factorizationin(1))         
    // n2pp = mod(n2p+qused(2)*(n3p),factorizationin(2))
 	// Also note that (factorizationin(3)*n2p+n3p) and (n3p) are up counters, meaning that
 	// you just need to add qused(1), qused(2) to the previous value and take appropriate mods (not a + 1 counter). 
    // See GMR paper

// NOTE INSTEAD OF TABLE OF COPRIMES, SHOULD BE USING TABLE OF DIGITS

	val iDIFModCounters = (0 until coprimesColCount-1).map(x => ModCounter(generalConstants.maxCoprime(x)-1,
		generalConstants.maxCoprime(x)-1, inputDelay = 0, nameExt = "iDIF"+generalConstants.maxCoprime(x).toString) )

	val iDIFModCounts =	Vec(iDIFModCounters.zipWithIndex.map{
			case (e,i) => {
				e.io.inc.get := DSPUInt(qDIFi(i),generalConstants.maxCoprime(i)-1)
				e.io.modN.get := DSPUInt(coprimes(i),generalConstants.maxCoprime(i))
				e.iCtrl.change.get := DSPBool(slwClkEn)
				e.iCtrl.reset := DSPBool(io.START_FIRST_FRAME)
				e.iCtrl.wrap.get := DSPBool(iDIFCounters(i).changeCond)
				e.io.out.toUInt
			}
		})


	// nxpp

	val iDIFNewCounts = Vec((0 until coprimesColCount).map(
		i => {
			val out = {
				if (i == coprimesColCount - 1) iDIFCounts(i)
				else {
					Mod(DSPUInt(iDIFCounts(i), generalConstants.maxCoprime(i) - 1) + DSPUInt(iDIFModCounts(i), generalConstants.maxCoprime(i) - 1), DSPUInt(coprimes(i), generalConstants.maxCoprime(i)))._1
				}
			}
			out.toUInt
		}
	))








	// Switch rows/cols so Scala doesn't complain (originally columns are associated with n1...n6, but want to address "column" first -> tranpose)
	var dec2xAryArray = Array.ofDim[Int](generalConstants.validPrimes.length,0,0)
	for (i <- 0 until dec2xAryArray.length){
		dec2xAryArray(i) = ioAddressConstants.dec2xAryArray(i).transpose
	}

	val dec2xAryLUT = Vec((0 until dec2xAryArray.length).map(y => {Vec((0 until dec2xAryArray(y).length).map( x => Module( new UInt2LUT(dec2xAryArray(y)(x)) ).io ))}))
	val dec2xAryDIFi = Vec((0 until dec2xAryArray.length).map(y => {Vec.fill(dec2xAryArray(y).length){UInt()}}))
	for (i <- 0 until dec2xAryArray.length;
		 j <- 0 until dec2xAryArray(i).length){
		// i corresponds to particular coprime; j corresponds to which constant brought out; all constants for particular coprime brought out with same address in
		dec2xAryLUT(i)(j).addr1 := iDIFNewCounts(i)
		dec2xAryDIFi(i)(j) := dec2xAryLUT(i)(j).dout1
	}
	val iDIFn = Vec.fill(generalConstants.maxNumStages){Reg(UInt(width=Helper.bitWidth(generalConstants.maxRadix-1)))}
	val rad4startingBit = Vec.fill(generalConstants.maxNumStages){UInt()}








	// For max 2^N = 2048, max count = 2047 -> 11 bits (worst case # of bits needed)
	val pow2MaxCountBits = (log(generalConstants.maxCoprime(0))/log(2)).toInt 			// If radix 4 is used, guaranteed first coprime is related to 2^N
	var pow2NewMaxCountBits = 0
	if ((pow2MaxCountBits % 2) == 0){													// If even # of bits needed
		pow2NewMaxCountBits = pow2MaxCountBits											// If the max 2^N requires even bits, i.e. 1024, there would be no divide by 2 for that case; also
																						// note that for radix-4 related addressing, you're grouping bits in groups of 2 
	}
	else{ 																				// If odd # of bits needed: i.e. if 2048 = 2^N = 4^5*2, then know rad4iDIFNewCount is original/2
		pow2NewMaxCountBits = pow2MaxCountBits-1										// so one less bit is needed (= 10 bits); any other possible 2^N less than 2048 requires at least
																						// 10 bits, so handles worst case
	} 
	val rad4iDIFNewCount = UInt(width=pow2NewMaxCountBits)	
	if (generalConstants.rad4Used){
		for (y <- 0 until generalConstants.maxNumStages){
			rad4startingBit(y) := (stageSumM1(0)-UInt(y)) << UInt(1) 					// if radix 4 is used, it's always first, *2 because we're grouping in 2 bits 
		}
		if (generalConstants.validRadices(1) == 2){ 									// if radix-2 is a valid butterfly
			when (numPower(1) === UInt(1)){ 												// separate out power of 2 from power of 4; if it's 4^a*2, extract 4^a										
				rad4iDIFNewCount := iDIFNewCounts(0) >> UInt(1)							// If radix 4 is used, will be first
			}.otherwise{																// Otherwise leave as is; no *2
				rad4iDIFNewCount := iDIFNewCounts(0)
			}
		}
		else{
			rad4iDIFNewCount := iDIFNewCounts(0)										// If 4 used in generator but not 2, then don't need to modify
		}
	}











	for (y <- 0 until generalConstants.maxNumStages){
		iDIFn(y) := UInt(0) 												// Unused stages
		for (z <- generalConstants.validRadices.length-1 to 0 by -1){
			when(stageRadix(y)===UInt(generalConstants.validRadices(z))){
				if (generalConstants.validRadices(z) == 2){ 				// Coprime 2^N always handled first
					iDIFn(y) := iDIFNewCounts(0)(0) 						// LSB of count associated with 2^N coprime (indicates if multiple of 2)
				}
				else if (generalConstants.validRadices(z) == 4){			 
					iDIFn(y) := Cat(rad4iDIFNewCount(UInt(rad4startingBit(y)+UInt(1),width=Helper.bitWidth(pow2NewMaxCountBits-1))),rad4iDIFNewCount(rad4startingBit(y))) 		
					// Grouped in 2 bits (base 4)
				}
				else{
					//  Eg: radix-3 stages if existing; codes in ternary (0 to 2); highest set of 3 (like MSB) stored left-most stage-wise
					if (generalConstants.rad4Used){
						iDIFn(y) := dec2xAryDIFi(z-1)(stageSumM1(z)-UInt(y))	
					}
					else{
						iDIFn(y) := dec2xAryDIFi(z)(stageSumM1(z)-UInt(y))
					}
				}
			}
		}
		debug(iDIFn(y))														// Note: 1 cycle delay
	}













	// n1,n2,n3... -> input DIF address/banks

	val iDIFnToBankAddr = Module(new nToBankAddr(toAddrBankDly(1)))
	for (i <- 0 until generalConstants.maxNumStages){
		iDIFnToBankAddr.io.n(i) := iDIFn(i)//ions(i)
		iDIFnToBankAddr.io.addrConstant(i) := addressConstant(i)
	}
	iDIFnToBankAddr.io.maxRadix := maxRadix									// two kinds of max radices: generalConstants.maxRadix = overall max radix for generator; maxRadix = max radix for current FFT
	val iDIFAddr = Count(null,addrMax); iDIFAddr := iDIFnToBankAddr.io.addr
	val iDIFBank = Count(null,bankMax); iDIFBank := iDIFnToBankAddr.io.bank
	debug (iDIFAddr)														// Note: total delay 2 cycles
	debug(iDIFBank)


////// DIF OUTPUT
	// DIF O Counters -- uses reverse coprime factorization
	val oDIFCounts = Vec.fill(coprimesColCount){UInt()}
	val oDIFCounters = Vec((0 until coprimesColCount).map( x => Module( new accumulator(Helper.bitWidth(generalConstants.maxCoprime.reverse(x)-1)) ).io ))
	val oDIFCountWrap = Vec.fill(coprimesColCount){Bool()}	
	for (i <- coprimesColCount-1 to 0 by -1){
		// Wrap after reached max count
		oDIFCountWrap(i) := (oDIFCounters(i).out === coprimesFlipped(i)-UInt(1))
		oDIFCounters(i).inc := UInt(1,width=1)
		// If right-most count, change only on slowClk = 0
		// Otherwise, only change when previous (right) counters are about to wrap around
		if (i == coprimesColCount-1){
			oDIFCounters(i).changeCond := slwClkEn
		}
		else{
			oDIFCounters(i).changeCond := oDIFCounters(i+1).changeCond && oDIFCountWrap(i+1) 
		}
		oDIFCounters(i).globalReset := io.START_FIRST_FRAME		// Reset counts to 0 on next clk cycle after START_FIRST_FRAME high
		oDIFCounters(i).wrapCond := oDIFCountWrap(i)
		oDIFCounts(i) := oDIFCounters(i).out
		debug(oDIFCounts(i))
	}
	
	// output DIF counts: See iDIF calculations for explanation
	// Note order of coprimes used is backwards

	val maxCoprimeFlipped = generalConstants.maxCoprime.reverse



	val oDIFModCounters = (0 until coprimesColCount-1).map(x => ModCounter(maxCoprimeFlipped(x)-1,
		maxCoprimeFlipped(x)-1, inputDelay = 0, nameExt = "oDIF"+maxCoprimeFlipped(x).toString) )

	val oDIFModCounts =	Vec(oDIFModCounters.zipWithIndex.map{
		case (e,i) => {
			e.io.inc.get := DSPUInt(qDIFo(i),maxCoprimeFlipped(i)-1)
			e.io.modN.get := DSPUInt(coprimesFlipped(i),maxCoprimeFlipped(i))
			e.iCtrl.change.get := DSPBool(slwClkEn)
			e.iCtrl.reset := DSPBool(io.START_FIRST_FRAME)
			e.iCtrl.wrap.get := DSPBool(oDIFCounters(i).changeCond)
			e.io.out.toUInt
		}
	})

	// nxpp

	val oDIFNewCounts = Vec((0 until coprimesColCount).map(
		i => {
			val out = {
				if (i == coprimesColCount - 1) oDIFCounts(i)
				else {
					Mod(DSPUInt(oDIFCounts(i), maxCoprimeFlipped(i) - 1) + DSPUInt(oDIFModCounts(i), maxCoprimeFlipped(i) - 1), DSPUInt(coprimesFlipped(i), maxCoprimeFlipped(i)))._1
				}
			}
			out.toUInt
		}
	))

	val dec2xAryDIFo = Vec((0 until dec2xAryArray.length).map(y => {Vec.fill(dec2xAryArray(y).length){UInt()}}))
	for (i <- 0 until dec2xAryArray.length;
		 j <- 0 until dec2xAryArray(i).length){
		// i corresponds to particular coprime; j corresponds to which constant brought out; all constants for particular coprime brought out with same address in
		// Note: n1pp is used for radix 5 constants instead of n3pp
		dec2xAryLUT(i)(j).addr2 := oDIFNewCounts(dec2xAryArray.length-1-i)
		dec2xAryDIFo(i)(j) := dec2xAryLUT(i)(j).dout2
	}
	val oDIFn = Vec.fill(generalConstants.maxNumStages){Reg(UInt(width=Helper.bitWidth(generalConstants.maxRadix-1)))}
	
	// To get in-place IO addressing, recall for N1,N2,N3 coprime, you have a mapping of (see ioAddressConstants)
	// n1 (2) -> k3
	// n2 (3) -> k2
	// n3 (5) -> k1
	// (After which you proceed to do a "DIT" equivalent calculation in reverse order)
	// Thus, in terms of generating the right address from n1...n3, k1...k3, and AC1...AC3, you have
	// A1n1+A2n2+A3n3 where the n3...n1 correspond to coprimes associated with 5,3,2
	// A1k3+A2k2+A3k3 where the k1...k3 correspond to cprimes associated with 5,3,2
	// Now if, after breaking the N up into coprimes (PFA), we further break down each of the separate coprimes into appropriate factors (Cooley-Tukey)
	// such that n3 --> most significant [n3b n3a] least significant
	// we would have n1b, n1a, n2b, n2a, n3b, n3a -> k3a, k3b, k2a, k2b, k1a, k1b and
	// A1b*n1b+A1a*n1a+A2b*n2b+A2a*n2a+A3b*n3b+A3a*n3a -> A1b*k3a+A1a*k3b+A2b*k2a+A2a*k2b+A3b*k1a+A3a*k1b
	// Notice that in order to keep the address constants (Ax) in the same order, n1 is swapped with k3 i.e. still handle 2 related sub indices first (wrt stage count) despite
	// 2 being associated with n3p instead of n1p; note also that a is also swapped with b which implies that rather than starting with the highest coprime grouping (bit, ternary, etc.)
	// first, you start with the lowest grouping first; by first, i mean left (opposite of the iDIF case)

	var pow2NewMaxCountBitso = 0
	if ((pow2MaxCountBits % 2) == 0){													// If even # of bits needed
		pow2NewMaxCountBitso = pow2MaxCountBits											
	}
	else{ 																				// If odd # of bits needed: i.e. if 2048 = 2^N = 4^5*2 for max 2^N
		pow2NewMaxCountBitso = pow2MaxCountBits+1										// Because 4-ary requires groups of 2 bits, pad with 0 for worst case (i.e. 11 bits -> 12 bits)									
	} 																					// Doesn't handle 2 separately

	val rad4startingBito = Vec.fill(generalConstants.maxNumStages){UInt()}
	val rad4oDIFNewCount = UInt(width=pow2NewMaxCountBitso)
	rad4oDIFNewCount := oDIFNewCounts(coprimesColCount-1) 								// 2^N is right-most when order flipped
	
	if (generalConstants.rad4Used){
		for (y <- 0 until generalConstants.maxNumStages){
			rad4startingBito(y) := UInt(y) << UInt(1) 									// *2 because we're grouping in 2 bits 
		}
		
	}

	for (y <- 0 until generalConstants.maxNumStages){
		oDIFn(y) := UInt(0) 												// Unused stages
		for (z <- generalConstants.validRadices.length-1 to 0 by -1){
			when(stageRadix(y)===UInt(generalConstants.validRadices(z))){
				// 4,2 associated with z 0,1
				if (generalConstants.validRadices(z) == 4 || generalConstants.validRadices(z) == 2){			 
					oDIFn(y) := Cat(rad4oDIFNewCount(UInt(rad4startingBito(y)+UInt(1),width=Helper.bitWidth(pow2NewMaxCountBitso-1))),rad4oDIFNewCount(rad4startingBito(y))) 		
					// Grouped in 2 bits (base 4)
				}
				else{
					//  Eg: radix-3 stages if existing; codes in ternary (0 to 2); lowest set of 3 stored left-most
					if (generalConstants.rad4Used){
						oDIFn(y) := dec2xAryDIFo(z-1)(UInt(y)-stageSum(z-1))
					}
					else{
						oDIFn(y) := dec2xAryDIFo(z)(UInt(y)-stageSum(z-1))
					}
				}
			}
		}
		debug(oDIFn(y))														// Note: 1 cycle delay
	}

	// n1,n2,n3... -> output DIF address/banks

	val oDIFnToBankAddr = Module(new nToBankAddr(toAddrBankDly(1)))
	for (i <- 0 until generalConstants.maxNumStages){
		oDIFnToBankAddr.io.n(i) := oDIFn(i)
		oDIFnToBankAddr.io.addrConstant(i) := addressConstant(i)
	}
	oDIFnToBankAddr.io.maxRadix := maxRadix									// two kinds of max radices: generalConstants.maxRadix = overall max radix for generator; maxRadix = max radix for current FFT
	val oDIFAddr = Count(null,addrMax); oDIFAddr := oDIFnToBankAddr.io.addr
	val oDIFBank = Count(null,bankMax); oDIFBank := oDIFnToBankAddr.io.bank
	debug(oDIFAddr)															// Note: total delay 2 cycles second is toAddrBankDly(1); after difcount is (0)
	debug(oDIFBank)

////////////////////////////////////////////////////////////////////////////////////////////////////////
// Calculation Addressing

  val calcMemChangeCond = (iDIFCountWrap(0) && iDIFCounters(0).changeCond)	// IO counters all wrapping
  
  
  
  
  
  
  
  val calcControl = Module( new calc() )
  calcControl.io.calcMemChangeCond := calcMemChangeCond
  calcControl.io.startFirstFrame := io.START_FIRST_FRAME
  calcControl.io.maxStageCount := maxStageCount
  calcControl.io.stageSumM1 := stageSumM1
  calcControl.io.addressConstant := addressConstant
  calcControl.io.maxRadix := maxRadix
  calcControl.io.stageRadix := stageRadix
  val calcBank = calcControl.io.calcBank
  val calcAddr = calcControl.io.calcAddr
  val currentRadix = calcControl.io.currentRadix          // not delayed internally
  val currentStage = calcControl.io.currentStage          // not delayed internally
  val calcMemB = calcControl.io.calcMemB
  val calcDoneFlag = calcControl.io.calcDoneFlag          // not delayed internally
  val calcResetCond = calcControl.io.calcResetCond        // not delayed internally
  val ioDIT = calcControl.io.ioDIT
  val calcDIT = calcControl.io.calcDIT                    // not delayed internally
  val discardCalcWrite = calcControl.io.discardCalcWrite  // not delayed internally
  
  
  
  
  
  
  
 
   

// Twiddle addressing

val twiddleAddrMax = 2000
	
	val twiddleCountMaxUsed = UInt(twiddleCount(currentStage),width=maxTwiddleCountBitWidth)
	val twiddleSubCountMaxUsed = UInt(width=Helper.bitWidth(fftSizes.fftSizeArray.max))
	// Note for subcount, power of 2 is default. 
	// Power of 2 includes radix 4. Also note that when calculating
	// for the radix-2 stage, the overall twiddle count should be 0,
	// so it doesn't matter what the subcount value is
	// Subcount max depends on remaining coprimes
	twiddleSubCountMaxUsed := twiddleSubCountMax(0)							
	for (i <- generalConstants.validPrimes.length-1 to 0 by -1){
		when (currentRadix === UInt(generalConstants.validPrimes(i))){
			twiddleSubCountMaxUsed := twiddleSubCountMax(i)
		}
	}
	debug(twiddleCountMaxUsed)
	debug(twiddleSubCountMaxUsed)

	// Non-scaled twiddle counters
	// Subcounter to handle coprimes (holds main count value)
	// Counter to deal with current coprime 
	val twiddleSubCounter = Module(new accumulator(Helper.bitWidth(fftSizes.fftSizeArray.max))).io
	val twiddleCounter = Module(new accumulator(maxTwiddleCountBitWidth)).io
	val twiddleSubCounterWrap = (twiddleSubCounter.out === twiddleSubCountMaxUsed)
	val twiddleCounterWrap = (twiddleCounter.out === twiddleCountMaxUsed)
	twiddleSubCounter.inc := UInt(1,width=1)
	twiddleCounter.inc := UInt(1,width=1)
	twiddleSubCounter.changeCond := ~calcDoneFlag & ~discardCalcWrite
	twiddleCounter.changeCond := twiddleSubCounter.changeCond && twiddleSubCounterWrap				// max twiddle count differs depending on stage of given radix; only change when sub counter goes from max to 0 (to handle coprimes)
	twiddleSubCounter.globalReset := calcResetCond
	twiddleCounter.globalReset := calcResetCond
	twiddleSubCounter.wrapCond := twiddleSubCounterWrap
	twiddleCounter.wrapCond := twiddleCounterWrap
	val twiddleAddrTemp = UInt()
	twiddleAddrTemp := twiddleCounter.out
	debug(twiddleAddrTemp)															// 0 cycle delay

	val twiddleMulUsed = twiddleMul(currentStage)									// twiddle address scale factor
	debug(twiddleMulUsed)

	// Switch rows/cols so Scala doesn't complain (originally columns are associated with twiddle up to radix-1, but want to address "column" first -> tranpose)
	var twiddleArray = Array.ofDim[ScalaComplex](generalConstants.validPrimes.length,0,0)
	for (i <- 0 until twiddleArray.length){
		twiddleArray(i) = twiddleConstants.twiddleConstantsArray(i).transpose
	}	
	val twiddleLUT = Vec((0 until twiddleArray.length).map(y => {Vec((0 until twiddleArray(y).length).map( x => Module( new ComplexLUT(twiddleArray(y)(x), gen)).io ))}))
	// For each radix, radix-1 twiddle factors being fed to butterfly (1 to radix-1)

	val twiddleAddr = Count(null,twiddleAddrMax); twiddleAddr := Pipe(twiddleAddrTemp * twiddleMulUsed,toAddrBankDly(0)).asInstanceOf[UInt]									// Total delay: 1 cycles
	debug(twiddleAddr)

	val currentRadixD1 = Count(null,maxRad); currentRadixD1 := Pipe(currentRadix,toAddrBankDly(0)).asInstanceOf[UInt] //Reg(next = currentRadix)									// Match current radix delay to twiddleAddr total delay							
	debug(currentRadixD1)

	// For each of the coprimes
	val twiddleAddrX = Vec.fill(twiddleArray.length){UInt()}
	
	// Distribute address to correct twiddle LUT
	// Zeros LUT address when different radix used
	for (i <- 0 until twiddleArray.length){
		if (i == 0){																								// radix-4/radix-2 always first	if used
			if (generalConstants.rad4Used && generalConstants.pow2SupportedTF){																
				when ((currentRadixD1 === UInt(generalConstants.validPrimes(i)))||currentRadixD1 === UInt(4)){
					twiddleAddrX(i) := twiddleAddr
				}.otherwise{
					twiddleAddrX(i) := UInt(0)
				}
			}
			else{
				when ((currentRadixD1 === UInt(generalConstants.validPrimes(i)))){
					twiddleAddrX(i) := twiddleAddr
				}.otherwise{
					twiddleAddrX(i) := UInt(0)
				}
			}
		}
		else{
			when ((currentRadixD1 === UInt(generalConstants.validPrimes(i)))){
				twiddleAddrX(i) := twiddleAddr
			}.otherwise{
				twiddleAddrX(i) := UInt(0)
			}
		}
		debug(twiddleAddrX(i))
	}

	// todo: labeltwiddlelut
	val calcDITD1 = Bool(); calcDITD1 := Pipe(calcDIT,toAddrBankDly.sum+toMemAddrDly).asInstanceOf[Bool]



	println(twiddleArray(0))

	// Twiddles from 1(-1) to radix-1(-1) (indexed starting at 0) for each coprime
	val twiddles = Vec((0 until twiddleArray.length).map(y => {Vec.fill(twiddleArray(y).length){Complex(gen,gen)}}))
	for (i <- 0 until twiddleArray.length; j <- 0 until twiddleArray(i).length){
		// i corresponds to particular coprime; j corresponds to which twiddle brought out; all twiddles for particular coprime brought out with same address in
		val DITtwiddleAddr = Count(null,twiddleAddrMax); DITtwiddleAddr := Pipe(twiddleAddrX(i),toAddrBankDly(1)+toMemAddrDly).asInstanceOf[UInt]
		// Twiddles delayed by wftaDly cycles in DIF (multiplication occurs after WFTA)
		val DIFtwiddleAddr = Count(null,twiddleAddrMax); DIFtwiddleAddr := Pipe(DITtwiddleAddr,wftaDly).asInstanceOf[UInt]
		twiddleLUT(i)(j).addr := muxU(DIFtwiddleAddr,DITtwiddleAddr,calcDITD1)
		twiddles(i)(j) := twiddleLUT(i)(j).dout
		debug(twiddles(i)(j))

		//println(twiddles(i)(j).real.getRange + "," + twiddles(i)(j).imag.getRange)
	}

	// e^0 = 1 + 0 j ( = twiddle fed to butterfly's 0th input/output)
	val e0Complex = Complex(double2T(1),double2T(0))

	val twiddleXReal = Vec.fill(generalConstants.maxRadix-1){gen.cloneType}
	val twiddleXImag = Vec.fill(generalConstants.maxRadix-1){gen.cloneType}

	// Radix-M requires M-1 twiddle factors
	for (i <- 0 until twiddleXReal.length){										
		if (i == 0){																
			twiddleXReal(i) := e0Complex.real 											// DIF radix-2 has twiddles W^0_N = 1 (calculated last in a 2^N FFT so no special twiddle needed) - default
			twiddleXImag(i) := e0Complex.imag
		}
		else{ 																			// Default twiddle value for butterfly indices with larger N = those associated with
																						// largest radix (i.e. for radix-5, all inputs 1-4 (except 0) would need to use 
																						// twiddles associated with radix 5)
			if (generalConstants.validRadices(0) > generalConstants.validRadices(generalConstants.validRadices.length-1)){ 									
				// If the first valid radix is larger then the last one (i.e. when only radix 4,2,3 supported rather than 5)
				// the default would be associated with radix-4 rather than radix-3 (left most)	
				twiddleXReal(i) := twiddles(0)(i).real
				twiddleXImag(i) := twiddles(0)(i).imag 	 												
			}
			else{
				// If radix-4 isn't the largest, then the largest prime used is the right-most one
				twiddleXReal(i) := twiddles(twiddles.length-1)(i).real 
				twiddleXImag(i) := twiddles(twiddles.length-1)(i).imag 
			}
		}
		for (j <- generalConstants.validPrimes.length-1 to 0 by -1){ 							// All possible twiddle types (corresponding to valid primes)
			var jj:Int = 0
			 if (j != 0 && generalConstants.rad4Used && generalConstants.pow2SupportedTF){ 		// If radix=4 is used, corresponding radix index is + 1 of prime index (i.e. for 3,5)
				jj = j+1 																
			}
			else{
				jj = j																			
				// Note that radix-2 butterfly doesn't need specific twiddle; only radix-4 does for 2^N, 
				// so prime of 2 -> radix of 4 (same index)	
				// Otherwise, if radix-4 not used, then prime and radix indices should match																
			}
			val radixTemp:Int = generalConstants.validRadices(jj)
			if (radixTemp > i+1){ 	
				// i = input index -1; therefore i = 0 actually corresponds to second input since first input is trivial		
				// If I have twiddle inputs 0 to 4 corresponding with supporting up to radix-5,
				// Where input 0 doesn't have an associated special twiddle
				// Input 1 would need to support twiddles associated with radix-2,-3,-4,-5 (where radix-2 twiddle = trivial 1)
				// Input 2 would need to support twiddles associated with radix-3,-4,-5 
				// Input 3 would need to support twiddles associated with radix-4,-5
				// i.e. radix-3 only has input indices 0-2, so it doesn't need to be supported
				// by higher input #'s
				// Input 4 would need to support twiddles associated with radix-5
				
				
				
				
				
				
				
				
				
				
				
				
				
				
				
				// d2 = 1 + memAddrDly, wftaDly or not
				
				
				val currentRadixDx = Count(null,maxRad); currentRadixDx := Pipe(currentRadixD1,toAddrBankDly(1)+toMemAddrDly).asInstanceOf[UInt]
				val currentRadixDx2 = Count(null,maxRad); currentRadixDx2 := Pipe(currentRadixDx,wftaDly).asInstanceOf[UInt]
				val cr = muxU(currentRadixDx2,currentRadixDx,calcDITD1) // NOTE TO SELF: DELAY CALCDIT appropriately even if still works
				 
				when (cr === UInt(radixTemp)){
					twiddleXReal(i) := twiddles(j)(i).real		
					twiddleXImag(i) := twiddles(j)(i).imag								
				}
			}
		}
		debug(twiddleXReal(i))
		debug(twiddleXImag(i))
	}




// seq read dly
						// Total delay: 3 cycles


	val twiddleX = Vec((0 until generalConstants.maxRadix-1).map( i => {
		Complex(twiddleXReal(i), twiddleXImag(i)).pipe(1)

	}))





	/*Vec((0 until generalConstants.maxRadix-1).map(
		i => {
			Complex(twiddleXReal(i),twiddleXImag(i)).pipe(1)
		}
	))*/
	//85-87

	debug(twiddleX)















//////////////////////////////////////////////////////////////////////////////////////
// Memory + Butterfly interface 

	val calcDoneFlagD = Pipe(calcDoneFlag,toAddrBankDly.sum).asInstanceOf[Bool]
	val ioAddr = muxU(iDIFAddr,oDIFAddr,ioDIT)
	val ioBank = muxU(iDIFBank,oDIFBank,ioDIT)







	val memBanks = DSPModule( new memBanks(gen) )

	CheckDelay.off()


	memBanks.io.ioBank := ioBank
	memBanks.io.ioAddr := ioAddr
	memBanks.io.calcMemB:= calcMemB
	memBanks.io.calcDoneFlag := calcDoneFlagD
	memBanks.io.calcBank := calcBank
	memBanks.io.calcAddr := calcAddr
	memBanks.io.discardCalcWrite := Pipe(discardCalcWrite,toAddrBankDly.sum).asInstanceOf[Bool]

	// If first value comes in when START_FIRST_FRAME is asserted high,
	// there is a delay until address to the memory is valid
	// IFFT --> real+imaginary inputs/outputs swapped

	val DINusedreal = Mux(DSPBool(io.FFT), io.DATA_IN.real, io.DATA_IN.imag)
	val DINusedimag = Mux(DSPBool(io.FFT),io.DATA_IN.imag,io.DATA_IN.real)
	val DINused =  Complex(DINusedreal,DINusedimag)
	io.DATA_OUT.real := Mux(DSPBool(io.FFT),memBanks.io.Dout.real,memBanks.io.Dout.imag) 
	io.DATA_OUT.imag := Mux(DSPBool(io.FFT), memBanks.io.Dout.imag, memBanks.io.Dout.real)  
	// START_FIRST_FRAME held for ioToCalcClkRatio cycles -> Count 0 valid on the 1st cycle START_FIRST_FRAME is low
	memBanks.io.Din := Pipe(DINused,ioToCalcClkRatio+toAddrBankDly.sum+toMemAddrDly).asInstanceOf[Complex[T]]

	
	
	
	



















	memBanks.io.ioWriteFlag := Pipe(ioWriteFlag,0).asInstanceOf[Bool]

	val firstDataFlag = Reg(next = calcMemChangeCond && ~io.START_FIRST_FRAME)	// Cycle 0 - don't output when first frame is being fed in (output data not valid)

	val secondInPassedFlag = Reg(init = Bool(false))
	when (io.START_FIRST_FRAME){
		secondInPassedFlag := Bool(false)										// Reset
	}.elsewhen(firstDataFlag){													// Will go high at the beginning of each new input symbol starting with the 2nd input symbol
		secondInPassedFlag := Bool(true)										// True indicates second input symbol has already been processed
	}

	val firstDataFlagD1 = Reg(next = firstDataFlag && secondInPassedFlag && ~io.START_FIRST_FRAME)		// Output data only valid at the start of 3rd input symbol (when secondInPassedFlag is high)
	val firstDataFlagD2 = Reg(next = firstDataFlagD1 && ~io.START_FIRST_FRAME)							// Reset all registers at start of first symbol to make sure unknown states aren't propagated
	val firstDataFlagD3 = Reg(next = firstDataFlagD2 && ~io.START_FIRST_FRAME)
	val firstDataFlagD4 = Reg(next = firstDataFlagD3 && ~io.START_FIRST_FRAME)							// Flag needs to be 2 fast clock cycles long
	io.FIRST_OUT := ~io.START_FIRST_FRAME & Pipe((firstDataFlagD3	| firstDataFlagD4) && ~io.START_FIRST_FRAME,seqRdDly).asInstanceOf[Bool]													// Delayed appropriately to be high when k = 0 output is read (held for 2 cycles)
	
	val currentRadixD2 = Reg(next = currentRadixD1)
	val currentRadixD3 = Reg(next = currentRadixD2)
	debug(currentRadixD3)

	val calcPhaseD3 = Pipe(calcDIT,toAddrBankDly.sum+toMemAddrDly+seqRdDly)
	debug(calcPhaseD3)									// DIT or DIF sent to butterfly
	
	// Butterfly calculation performed on current x data + twiddles
	// Current radix configures the WFTA butterfly
	// Current calcPhase configures twiddle input/output multiplication for DIT/DIF calculations



	val rad = Pipe(currentRadixD2,toMemAddrDly+seqRdDly).asInstanceOf[UInt]



	val peNum = 0
	val butterfly = DSPModule(new PE(gen,num = peNum), nameExt = peNum.toString)

	CheckDelay.off()


  val eq2 = DSPBool(rad.toUInt === UInt(2))


	butterfly.io.twiddles.zipWithIndex.foreach{case (e,i) => {
		/*if (i < generalConstants.maxRadix-1) {
			println("xxx" + e.real.getRange + "," + e.imag.getRange + ",") //+ twiddleX(i).real.getRange)
			e := twiddleX(i)

			println("ttt" + e.real.getRange + "," + e.imag.getRange)
			//e.imag := twiddleX(i).imag
		}*/
		/*if (Params.getBF.rad.contains(2)){
			if (i == 1 || i == 2) {											// twiddle index is 1 off -- maybe not necessary? check always (1,0) for rad = 2

				//val check2 = Mux(eq2,twiddleX(0),twiddleX(i))
				//e :=  //Mux(eq2,twiddleX(0),twiddleX(i))
        e.real := Mux(eq2,twiddleX(0).real,twiddleX(i).real)
        e.imag := Mux(eq2,twiddleX(0).imag,twiddleX(i).imag)
				//e := Complex(test.real,test.imag) //twiddleX(i)//Mux(DSPBool(rad.toUInt === UInt(2)),twiddleX(0),twiddleX(i))
				//e := twiddleX(i)
			}
			else if (i < generalConstants.maxRadix-1){
				e := twiddleX(i)
			}
		}     NOTE NEEDADDRESS AT 0 FOR RAD 2 twiddle!!!! for idx 0-3 seems i already did -- twiddles delayed earlier
		else*/ if (i < generalConstants.maxRadix-1){
			e := twiddleX(i)
		}
	}}






		// Can update twiddle port in butterfly??






	

	memBanks.io.currRad := currentRadixD2
	//memBanks.io.discardCalcWrite := Bool(false)
  
  
  
 
	
	val currRad = Vec((0 until Params.getBF.rad.length).map( x => { val r = DSPBool(); r := DSPBool(rad === Count(Params.getBF.rad(x))); r }))
	
	butterfly.io.currRad.get := currRad
	butterfly.io.calcDIT := DSPBool(calcPhaseD3)

	
	for (i <- 0 until generalConstants.maxRadix){
		memBanks.io.y(i) := butterfly.io.y(i)
		butterfly.io.x(i) := memBanks.io.x(i)
	}


	/*// Debug
	for (i <- 0 until generalConstants.numBanks){
		memBanks.io.y(i) := pipeD(memBanks.io.x(i) * Complex(double2T(10.0),double2T(0)),pipeBFWriteDly).asInstanceOf[Complex[T]]
	}*/
	

/////////////////////////////////////////////////////////////////////////////////////////////////////////////

 

}
