// August 19, 2015

// TODO: NOTE CURRENT IMPLEMENTATION REQUIRES 4^n

package FFT
import Chisel.{Pipe => _, Complex => _, Mux => _, RegInit => _, RegNext => _, _}
import DSP._
import scala.math._
import memBanks._
import calc._
import Count._
import generator._

import scala.reflect.runtime.universe._

import ChiselDSP.{when => _, _}

class FFT[T <: DSPQnm[T]](gen : => T, p: GeneratorParams) extends GenDSPModule (gen) {

  // Setup FFT with user-defined parameters
  Params(p)

  // TODO: Get rid of placeholder
  FFTGenerator()

  val peNum = 0
  val butterfly = DSPModule(new PE(gen,num = peNum), nameExt = peNum.toString)
  // from memBanks
  pipeBFWriteDly = butterfly.delay
  val wftaDly = butterfly.wfta.delay
  val seqRdDly = 2

  CheckDelay.off()





//////////////////////////////////////////////////////////////////

  // Data IO, setup IO, Operating controls
  override val io = new FFTIO(gen)
  val setup = new SetupTopIO
  val ctrl = new IOCtrlIO

  // Derive IO clock from main clock
  val clkDiv = DSPModule(new ClkDiv(Params.getIO.clkRatio))
  val slowEn = clkDiv.io.slowEn


  val globalInit = DSPModule(new GlobalInit)
  globalInit.setupI <> setup
  globalInit.ioCtrlI <> ctrl


  // Length + FFT/IFFT setup
  val setupEn = slowEn & setup.enable
  val fftIndex = RegInit(DSPUInt(0,Params.getFFT.nCount - 1))
  val fftTF = RegInit(DSPBool(false))
  fftIndex := Mux(setupEn,setup.fftIdx,fftIndex)
  fftTF := Mux(setupEn,setup.isFFT,fftTF)

  // Setup location valid an IO clock cycle after (used to derive the remaining setup constants)
  val setupEnDly = setupEn.pipe(Params.getIO.clkRatio)

  // Make sure that IO reset on the right fast clock cycle
  val startFirstFrame = slowEn & ctrl.reset



  val GeneralSetup =  DSPModule(new GeneralSetup)
  GeneralSetup.setupTop <> globalInit.setupO

  val IOSetup = DSPModule (new IOSetup)
  IOSetup.setupTop <> globalInit.setupO
  IOSetup.generalSetup <> GeneralSetup.o

  val IOCtrl = DSPModule(new IOCtrl)
  //IOCtrl.ctrl <> globalInit.ioCtrlO
  IOCtrl.ctrl.enable := globalInit.ioCtrlO.enable
  IOCtrl.ctrl.reset := globalInit.ioCtrlO.reset
  IOCtrl.generalSetup <> GeneralSetup.o

  IOSetup.o <> IOCtrl.ioSetup

  // Total delay sum of all 3 (GeneralSetup.setupDelay + IOSetup.setupDelay) + twiddle

  val TwiddleSetup = DSPModule(new TwiddleSetup)
  TwiddleSetup.setupTop <> globalInit.setupO
  //TwiddleSetup.generalSetup <> GeneralSetup.o
  TwiddleSetup.generalSetup.radStageSum := GeneralSetup.o.radStageSum
  TwiddleSetup.generalSetup.stageRad := GeneralSetup.o.stageRad
  TwiddleSetup.ioSetup.stagePrimeIdx := IOSetup.o.stagePrimeIdx

  // Anything where only 1 is used? shorten



  ////// Setup FFT length-dependent general constants
  // Final setup constants are all registered
  // Array columns broken into separate LUTs

  // Powers: a1, a2, b, c in 4^a1*2^a2*3^b*5^c
  // clk 2
  val numPowerArray = generalConstants.numPowerArray.transpose
  val powColCount = numPowerArray.length
  val numPowerLUT = Vec((0 until powColCount).map(x => Module(new UIntLUT(numPowerArray(x))).io))
  val numPower = Vec.fill(powColCount) {
    Reg(UInt())
  }
  for (i <- 0 until powColCount) {
    numPowerLUT(i).addr := fftIndex
    numPower(i) := numPowerLUT(i).dout
    debug(numPower(i))
  }

  // Ex: sum(0) = power(0)
  // sum(1) = power(0)+power(1)
  // sum(2) = sum(1) + power(2)
  // Keeps track of # of stages required up until current radix
  // Where the last array value represents total # of stages required for FFT calc
  // clk 3 : Note combinational logic registered after everything is completed
  val stageSumTemp = Vec.fill(powColCount) {
    UInt()
  }
  val stageSum = Vec.fill(powColCount) {
    Reg(UInt())
  }
  val stageSumM1 = Vec.fill(powColCount) {
    UInt()
  }
  for (j <- 0 until powColCount) {
    if (j == 0) {
      stageSumTemp(0) := numPower(0)
    }
    else if (j == powColCount - 1) {
      stageSumTemp(powColCount - 1) := UInt(stageSumTemp(powColCount - 2) + numPower(powColCount - 1), width = Helper.bitWidth(generalConstants.maxNumStages))
    }
    else {
      stageSumTemp(j) := stageSumTemp(j - 1) + numPower(j)
    }
  }
  stageSum := stageSumTemp
  for (i <- 0 until powColCount) {
    stageSumM1(i) := stageSum(i) - UInt(1)
    debug(stageSum(i))
  }

  // Max radix for current FFT calculation (see generalConstants for logic explanation)
  // clk 3
  val maxRadixTemp = Vec.fill(powColCount) {
    UInt(width = Helper.bitWidth(generalConstants.maxRadix))
  }
  val maxRadix = Reg(UInt(width = Helper.bitWidth(generalConstants.maxRadix)))
  for (i <- powColCount - 1 to 0 by -1) {
    if (i == powColCount - 1) {
      when(numPower(powColCount - 1) != UInt(0)) {
        maxRadixTemp(powColCount - 1) := UInt(generalConstants.validRadices(powColCount - 1))
      }.otherwise {
        maxRadixTemp(powColCount - 1) := UInt(0)
      }
    }
    else {
      when((numPower(i) != UInt(0)) && (maxRadixTemp(i + 1) === UInt(0))) {
        maxRadixTemp(i) := UInt(generalConstants.validRadices(i))
      }.otherwise {
        maxRadixTemp(i) := maxRadixTemp(i + 1)
      }
    }
  }
  if (generalConstants.pow2SupportedTF && generalConstants.rad4Used) {
    // radix-4 is always first
    when((numPower(0) != UInt(0)) && (UInt(4) > maxRadixTemp(0))) {
      maxRadix := UInt(4)
    }.otherwise {
      maxRadix := maxRadixTemp(0)
    }
  }
  else {
    maxRadix := maxRadixTemp(0)
  }
  debug(maxRadix)

  // Determine radix of each stage
  // If stage # < stageSum(i), the corresponding radix can be found from validRadices(i)
  // clk 4
  val stageRadix = Vec.fill(generalConstants.maxNumStages) {
    Reg(UInt(width = Helper.bitWidth(generalConstants.maxRadix)))
  }
  val maxStageCount = Vec.fill(generalConstants.maxNumStages) {
    Reg(UInt(width = Helper.bitWidth(generalConstants.maxRadix - 1)))
  }
  for (i <- 0 until generalConstants.maxNumStages) {
    maxStageCount(i) := UInt(0) // only if other conditions not met (priority dependent)
    stageRadix(i) := UInt(0)
    for (j <- powColCount - 1 to 0 by -1) {
      when(UInt(i) < stageSum(j)) {
        stageRadix(i) := UInt(generalConstants.validRadices(j))
        maxStageCount(i) := UInt(generalConstants.validRadices(j) - 1)
      }
    }
    debug(stageRadix(i))
    debug(maxStageCount(i))
  }


  val addrConstantLUT = DSPModule(new IntLUT2D(Params.getMem.addrC))
  addrConstantLUT.io.addr := fftIndex
  //addressConstant := addrConstantLUT.io.dout

  val addressConstant = Vec(addrConstantLUT.io.dout.map(_.cloneType.toUInt))
  addressConstant := Vec(addrConstantLUT.io.dout.map(_.reg().toUInt))

  // Counter reset whenever new FFTN desired, stays constant after setup is done SHOULD OPTIMIZE
  val setupDoneCount: Int = 4 + generalConstants.maxNumStages * 3 + 20
  val setupCounter = Module(new accumulator(Helper.bitWidth(setupDoneCount)))
  val setupDoneTemp = (UInt(setupDoneCount) === setupCounter.io.out)
  setupCounter.io.inc := UInt(1, width = 1)
  setupCounter.io.changeCond := ~setupDoneTemp
  setupCounter.io.globalReset := setup.enable
  setupCounter.io.wrapCond := Bool(false)

  val setupDoneTempD1 = Reg(next = setupDoneTemp)
  setup.done := DSPBool(setupDoneTemp || setupDoneTempD1)
  // Hold for 2 cycles


  ////////////////////////////////////////////////////////////////////////////////////////////////////////
  // IO Addressing

  val ioWriteFlag = slowEn

////////////////////////////////////////////////


  ////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Calculation Addressing

  val calcMemChangeCond = Bool(OUTPUT)
  calcMemChangeCond := IOCtrl.ioFlagsNoDelay.wrapCond

  //ioIncCounters1.head.ctrl.isMax & slowEn & ioIncCounters1(1).ctrl.isMax & ioIncCounters1(2).ctrl.isMax
  // (iDIFCountWrap(0) && iDIFCounters(0).changeCond)	// IO counters all wrapping

  val calcControl = Module(new calc())

  val calcAddr = calcControl.io.calcAddr
  val currentRadix = calcControl.io.currentRadix
  // not delayed internally
  val currentStage = calcControl.io.currentStage
  // not delayed internally
  val calcMemB = calcControl.io.calcMemB
  val calcDoneFlag = calcControl.io.calcDoneFlag
  // not delayed internally
  val calcResetCond = calcControl.io.calcResetCond
  // not delayed internally
  val ioDIT = calcControl.io.ioDIT
  val calcDIT = calcControl.io.calcDIT
  // not delayed internally
  val discardCalcWrite = calcControl.io.discardCalcWrite // not delayed internally


  CheckDelay.on()

  val ioAddr = IOCtrl.o.addr
  val ioBank = IOCtrl.o.bank

  //////////////

  val rad2DblOrig = (currentRadix.toUInt === UInt(2)).toBool & (numPower(0).toUInt =/= UInt(0)).toBool
  val newMaxStageCount = Vec(maxStageCount.zipWithIndex.map { case (e, i) => {
    val temp = {
      if (i == 0) {
        Mux(rad2DblOrig, UInt(1), e.toUInt).toUInt
      }
      else e
    }
    Mux(numPower(3) === UInt(0), temp, e)
  }
  })
  debug(newMaxStageCount)

  // **** CHANGED

  // Mux(numPower(3) === UInt(0),newMaxStageCount.asOutput,maxStageCount)


  calcControl.io.calcMemChangeCond := calcMemChangeCond
  calcControl.io.startFirstFrame := ctrl.reset

  calcControl.io.maxStageCount := newMaxStageCount.asOutput

  //Mux(numPower(3) === UInt(0),newMaxStageCount.asOutput,maxStageCount)


  //maxStageCount //newMaxStageCount.asOutput //maxStageCount
  calcControl.io.stageSumM1 := stageSumM1
  calcControl.io.addressConstant := addressConstant
  calcControl.io.maxRadix := maxRadix
  calcControl.io.stageRadix := stageRadix


  val calcBank = calcControl.io.calcBank














  // Twiddle addressing

  val twiddleAddrMax = 2000

  val twiddleCountMaxUsed = TwiddleSetup.o.twiddleCounts(currentStage).toUInt
  val twiddleSubCountMaxUsed = UInt(width = Helper.bitWidth(Params.getFFT.sizes.max))
  // Note for subcount, power of 2 is default.
  // Power of 2 includes radix 4. Also note that when calculating
  // for the radix-2 stage, the overall twiddle count should be 0,
  // so it doesn't matter what the subcount value is
  // Subcount max depends on remaining coprimes
  twiddleSubCountMaxUsed := TwiddleSetup.o.twiddleSubCounts(currentStage) /*twiddleSubCountMax(0)
  for (i <- generalConstants.validPrimes.length - 1 to 0 by -1) {
    when(currentRadix === UInt(generalConstants.validPrimes(i))) {
      twiddleSubCountMaxUsed := twiddleSubCountMax(i)
    }
  }*/
  debug(twiddleCountMaxUsed)
  debug(twiddleSubCountMaxUsed)

  // Non-scaled twiddle counters
  // Subcounter to handle coprimes (holds main count value)
  // Counter to deal with current coprime
  val twiddleSubCounter = Module(new accumulator(Helper.bitWidth(Params.getFFT.sizes.max))).io
  val twiddleCounter = Module(new accumulator(twiddleCountMaxUsed.getWidth)).io
  val twiddleSubCounterWrap = (twiddleSubCounter.out === twiddleSubCountMaxUsed)
  val twiddleCounterWrap = (twiddleCounter.out === twiddleCountMaxUsed)
  twiddleSubCounter.inc := UInt(1, width = 1)
  twiddleCounter.inc := UInt(1, width = 1)
  twiddleSubCounter.changeCond := ~calcDoneFlag & ~discardCalcWrite
  twiddleCounter.changeCond := twiddleSubCounter.changeCond && twiddleSubCounterWrap // max twiddle count differs depending on stage of given radix; only change when sub counter goes from max to 0 (to handle coprimes)
  twiddleSubCounter.globalReset := calcResetCond
  twiddleCounter.globalReset := calcResetCond
  twiddleSubCounter.wrapCond := twiddleSubCounterWrap
  twiddleCounter.wrapCond := twiddleCounterWrap
  val twiddleAddrTemp = UInt()
  twiddleAddrTemp := twiddleCounter.out
  debug(twiddleAddrTemp)
  // 0 cycle delay

  val twiddleMulUsed = TwiddleSetup.o.twiddleMuls(currentStage).toUInt // twiddle address scale factor
  debug(twiddleMulUsed)


  // Switch rows/cols so Scala doesn't complain (originally columns are associated with twiddle up to radix-1, but want to address "column" first -> tranpose)
  var twiddleArray = Params.getTw.vals.map(
    _.transpose
  )
  val twiddleLUT = Vec((0 until twiddleArray.length).map(y => {
    Vec((0 until twiddleArray(y).length).map(x => Module(new ComplexLUT(twiddleArray(y)(x), gen)).io))
  }))
  // For each radix, radix-1 twiddle factors being fed to butterfly (1 to radix-1)

  val twiddleAddr = Count(null, twiddleAddrMax);
  twiddleAddr := Pipe(twiddleAddrTemp * twiddleMulUsed, toAddrBankDly(0)).asInstanceOf[UInt] // Total delay: 1 cycles
  debug(twiddleAddr)

  val currentRadixD1 = Count(null, maxRad);
  currentRadixD1 := Pipe(currentRadix, toAddrBankDly(0)).asInstanceOf[UInt] //Reg(next = currentRadix)									// Match current radix delay to twiddleAddr total delay
  debug(currentRadixD1)

  // For each of the coprimes
  val twiddleAddrX = Vec.fill(twiddleArray.length) {
    UInt()
  }

  // Distribute address to correct twiddle LUT
  // Zeros LUT address when different radix used
  for (i <- 0 until twiddleArray.length) {
    if (i == 0) {
      // radix-4/radix-2 always first	if used
      if (generalConstants.rad4Used && generalConstants.pow2SupportedTF) {
        when((currentRadixD1 === UInt(generalConstants.validPrimes(i))) || currentRadixD1 === UInt(4)) {
          twiddleAddrX(i) := twiddleAddr
        }.otherwise {
          twiddleAddrX(i) := UInt(0)
        }
      }
      else {
        when((currentRadixD1 === UInt(generalConstants.validPrimes(i)))) {
          twiddleAddrX(i) := twiddleAddr
        }.otherwise {
          twiddleAddrX(i) := UInt(0)
        }
      }
    }
    else {
      when((currentRadixD1 === UInt(generalConstants.validPrimes(i)))) {
        twiddleAddrX(i) := twiddleAddr
      }.otherwise {
        twiddleAddrX(i) := UInt(0)
      }
    }
    debug(twiddleAddrX(i))
  }

  // todo: labeltwiddlelut
  val calcDITD1 = Bool();
  calcDITD1 := Pipe(calcDIT, toAddrBankDly.sum + toMemAddrDly).asInstanceOf[Bool]



  //println(twiddleArray(0))

  // Twiddles from 1(-1) to radix-1(-1) (indexed starting at 0) for each coprime
  val twiddles = Vec((0 until twiddleArray.length).map(y => {
    Vec.fill(twiddleArray(y).length) {
      Complex(gen, gen)
    }
  }))

  // TODO: Rename LUTs
  for (i <- 0 until twiddleArray.length; j <- 0 until twiddleArray(i).length) {
    // i corresponds to particular coprime; j corresponds to which twiddle brought out; all twiddles for particular coprime brought out with same address in
    val DITtwiddleAddr = Count(null, twiddleAddrMax); DITtwiddleAddr := Pipe(twiddleAddrX(i), toAddrBankDly(1) + toMemAddrDly).asInstanceOf[UInt]
    // Twiddles delayed by wftaDly cycles in DIF (multiplication occurs after WFTA)
    val DIFtwiddleAddr = Count(null, twiddleAddrMax); DIFtwiddleAddr := Pipe(DITtwiddleAddr, wftaDly).asInstanceOf[UInt]
    val tempAddr = muxU(DIFtwiddleAddr, DITtwiddleAddr, calcDITD1)
    twiddleLUT(i)(j).addr := DSPUInt(tempAddr,twiddleLUT(i)(j).addr.getRange.max )
    twiddles(i)(j) := twiddleLUT(i)(j).dout
    debug(twiddles(i)(j))

    //println(twiddles(i)(j).real.getRange + "," + twiddles(i)(j).imag.getRange)
  }

  // e^0 = 1 + 0 j ( = twiddle fed to butterfly's 0th input/output)
  val e0Complex = Complex(double2T(1), double2T(0))

  val twiddleXReal = Vec.fill(generalConstants.maxRadix - 1) {
    gen.cloneType()
  }
  val twiddleXImag = Vec.fill(generalConstants.maxRadix - 1) {
    gen.cloneType()
  }

  // Radix-M requires M-1 twiddle factors
  for (i <- 0 until twiddleXReal.length) {
    if (i == 0) {
      twiddleXReal(i) := e0Complex.real // DIF radix-2 has twiddles W^0_N = 1 (calculated last in a 2^N FFT so no special twiddle needed) - default
      twiddleXImag(i) := e0Complex.imag
    }
    else {
      // Default twiddle value for butterfly indices with larger N = those associated with
      // largest radix (i.e. for radix-5, all inputs 1-4 (except 0) would need to use
      // twiddles associated with radix 5)
      if (generalConstants.validRadices(0) > generalConstants.validRadices(generalConstants.validRadices.length - 1)) {
        // If the first valid radix is larger then the last one (i.e. when only radix 4,2,3 supported rather than 5)
        // the default would be associated with radix-4 rather than radix-3 (left most)
        twiddleXReal(i) := twiddles(0)(i).real
        twiddleXImag(i) := twiddles(0)(i).imag
      }
      else {
        // If radix-4 isn't the largest, then the largest prime used is the right-most one
        twiddleXReal(i) := twiddles(twiddles.length - 1)(i).real
        twiddleXImag(i) := twiddles(twiddles.length - 1)(i).imag
      }
    }
    for (j <- generalConstants.validPrimes.length - 1 to 0 by -1) {
      // All possible twiddle types (corresponding to valid primes)
      var jj: Int = 0
      if (j != 0 && generalConstants.rad4Used && generalConstants.pow2SupportedTF) {
        // If radix=4 is used, corresponding radix index is + 1 of prime index (i.e. for 3,5)
        jj = j + 1
      }
      else {
        jj = j
        // Note that radix-2 butterfly doesn't need specific twiddle; only radix-4 does for 2^N,
        // so prime of 2 -> radix of 4 (same index)
        // Otherwise, if radix-4 not used, then prime and radix indices should match
      }
      val radixTemp: Int = generalConstants.validRadices(jj)
      if (radixTemp > i + 1) {
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


        val currentRadixDx = Count(null, maxRad);
        currentRadixDx := Pipe(currentRadixD1, toAddrBankDly(1) + toMemAddrDly).asInstanceOf[UInt]
        val currentRadixDx2 = Count(null, maxRad);
        currentRadixDx2 := Pipe(currentRadixDx, wftaDly).asInstanceOf[UInt]
        val cr = muxU(currentRadixDx2, currentRadixDx, calcDITD1) // NOTE TO SELF: DELAY CALCDIT appropriately even if still works

        when(cr === UInt(radixTemp)) {
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


  val twiddleX = Vec((0 until generalConstants.maxRadix - 1).map(i => {
    Complex(twiddleXReal(i), twiddleXImag(i)).pipe(1)

  }))


  /*Vec((0 until generalConstants.maxRadix-1).map(
		i => {
			Complex(twiddleXReal(i),twiddleXImag(i)).pipe(1)
		}
	))*/
  //85-87

  debug(twiddleX)






  Status(Params.getTw.addrMax.toString)






















  //////////////////////////////////////////////////////////////////////////////////////
  // Memory + Butterfly interface

  val calcDoneFlagD = Pipe(calcDoneFlag, toAddrBankDly.sum).asInstanceOf[Bool]

  val memBanks = DSPModule(new memBanks(gen))

  CheckDelay.off()

  memBanks.io.ioBank := ioBank
  memBanks.io.ioAddr := ioAddr
  memBanks.io.calcMemB := calcMemB
  memBanks.io.calcDoneFlag := calcDoneFlagD

  val currentRadixD2 = Reg(next = currentRadixD1)

  // ASSUMES 4 ALWAYS EXISTS (BAD ASSUMPTION)
  // 4 is used and current radix = 2
  val rad2Dbl = (currentRadixD2.toUInt === UInt(2)).toBool & (numPower(0).toUInt =/= UInt(0)).toBool
  val newCalcAddr = Vec(calcAddr.zipWithIndex.map { case (e, i) => {
    val eNew = e.cloneType
    eNew := e.toUInt
    if (i == 2 || i == 3) {
      val temp = calcAddr(i - 2).cloneType
      temp := calcAddr(i - 2).toUInt
      Mux(rad2Dbl, temp, e).toUInt
    }
    else eNew
  }
  })
  debug(newCalcAddr)


  memBanks.io.discardCalcWrite := Pipe(discardCalcWrite,toAddrBankDly.sum).asInstanceOf[Bool]

  // If first value comes in when reset is asserted high,
  // there is a delay until address to the memory is valid
  // IFFT --> real+imaginary inputs/outputs swapped

  val DINusedreal = Mux(DSPBool(setup.isFFT), io.din.real, io.din.imag)
  val DINusedimag = Mux(DSPBool(setup.isFFT),io.din.imag,io.din.real)
  val DINused =  Complex(DINusedreal,DINusedimag).pipe(1+0)

  // Added normalization

  val normalizedDelay = if (Params.getFFT.normalized) {
    val Normalize = DSPModule(new Normalize(gen), "normalize")
    Normalize.io.din := memBanks.io.Dout
    Normalize.setupTop.fftIdx := fftIndex
    Normalize.setupTop.isFFT := fftTF
    val normalizedOut = Normalize.io.dout.cloneType()
    normalizedOut := Normalize.io.dout

    io.dout.real := Mux(DSPBool(setup.isFFT), normalizedOut.real, normalizedOut.imag).pipe(1)
    io.dout.imag := Mux(DSPBool(setup.isFFT), normalizedOut.imag, normalizedOut.real).pipe(1) // reg b/c delayed 1 cycle from memout reg, but delay another to get back to io cycle
    Normalize.delay
  }
  else {
    io.dout.real := Mux(DSPBool(setup.isFFT),memBanks.io.Dout.real,memBanks.io.Dout.imag).pipe(1)
    io.dout.imag := Mux(DSPBool(setup.isFFT), memBanks.io.Dout.imag, memBanks.io.Dout.real).pipe(1)   // reg b/c delayed 1 cycle from memout reg, but delay another to get back to io cycle
    0
  }

  // reset held for ioToCalcClkRatio cycles -> Count 0 valid on the 1st cycle reset is low
  memBanks.io.Din := Pipe(DINused,ioToCalcClkRatio+toAddrBankDly.sum+toMemAddrDly).asInstanceOf[Complex[T]]

  memBanks.io.ioWriteFlag := Pipe(ioWriteFlag,0).toBool

  val firstDataFlag = Reg(next = calcMemChangeCond && ~ctrl.reset.toBool)	// Cycle 0 - don't output when first frame is being fed in (output data not valid)

  val secondInPassedFlag = Reg(init = Bool(false))
  when (ctrl.reset.toBool){
    secondInPassedFlag := Bool(false)										// Reset
  }.elsewhen(firstDataFlag){													// Will go high at the beginning of each new input symbol starting with the 2nd input symbol
    secondInPassedFlag := Bool(true)										// True indicates second input symbol has already been processed
  }

  val firstDataFlagD1 = Reg(next = firstDataFlag && secondInPassedFlag && ~ctrl.reset.toBool)		// Output data only valid at the start of 3rd input symbol (when secondInPassedFlag is high)
  val firstDataFlagD2 = Reg(next = firstDataFlagD1 && ~ctrl.reset.toBool)							// Reset all registers at start of first symbol to make sure unknown states aren't propagated
  val firstDataFlagD3 = Reg(next = firstDataFlagD2 && ~ctrl.reset.toBool)
  val firstDataFlagD4 = Reg(next = firstDataFlagD3 && ~ctrl.reset.toBool)							// Flag needs to be 2 fast clock cycles long
  val firstDataFlagD5 = Reg(next = firstDataFlagD4 && ~ctrl.reset.toBool)
  val firstDataFlagD6 = Reg(next = firstDataFlagD5 && ~ctrl.reset.toBool)

  val firstDataFlagD7 = Reg(next = firstDataFlagD6 && ~ctrl.reset.toBool)
  val firstDataFlagD8 = Reg(next = firstDataFlagD7 && ~ctrl.reset.toBool)

  // note seqrd dly was already incremented so instead of originally starting at cycle 82 for 12, it starts at cycle 83, add 1 to make consistent w/ io

  // + 1 used to be inside  2nd pipe (i.e. seqrddly + 1)
  val frameFirstOutPre =  Pipe(!ctrl.reset & Pipe(DSPBool(firstDataFlagD3	| firstDataFlagD4) & !ctrl.reset,seqRdDly),normalizedDelay)
  val frameFirstOutPreTemp = !ctrl.reset & frameFirstOutPre
  ctrl.outValid := Pipe(frameFirstOutPreTemp,1)

  // don't let frame first out change before setup done?

  // TODO: check timing of offsetCountEnable w/ frameFirstOutPreTemp
  val offsetCountEnable = RegInit(DSPBool(false))
  offsetCountEnable := Mux(frameFirstOutPreTemp & setup.done,DSPBool(true),offsetCountEnable)
  val offsetCounter = IncReset(Params.getFFT.sizes.max-1,nameExt="offset")
  offsetCounter.iCtrl.reset := frameFirstOutPreTemp
  offsetCounter.iCtrl.change.get := slowEn & offsetCountEnable
  ctrl.k := offsetCounter.io.out

  // Delayed appropriately to be high when k = 0 output is read (held for 2 cycles)

  val currentRadixD3 = Reg(next = currentRadixD2)
  debug(currentRadixD3)

  val calcPhaseD3 = Pipe(calcDIT,toAddrBankDly.sum+toMemAddrDly+seqRdDly)
  debug(calcPhaseD3)									// DIT or DIF sent to butterfly

  // Butterfly calculation performed on current x data + twiddles
  // Current radix configures the WFTA butterfly
  // Current calcPhase configures twiddle input/output multiplication for DIT/DIF calculations


  val rad = Pipe(currentRadixD2,toMemAddrDly+seqRdDly).asInstanceOf[UInt]


  CheckDelay.off()


  val eq2 = DSPBool(rad.toUInt === UInt(2))


  butterfly.io.twiddles.zipWithIndex.foreach{case (e,i) => {
    if (i < generalConstants.maxRadix-1){
      e := twiddleX(i).reg() // noob reg to match dly on data out of mem (should move to reg address instead of data)
    }
  }}


  // Can update twiddle port in butterfly??

  // if radix 2 --> 4

  val tempcurrRad = Mux(currentRadixD2 === UInt(2),UInt(4),currentRadixD2).toUInt

  Mux(numPower(3) === UInt(0),tempcurrRad,currentRadixD2)
  memBanks.io.currRad := Mux(numPower(3) === UInt(0),tempcurrRad,currentRadixD2)

  // *** CHANGED

  //currentRadixD2 //Mux(currentRadixD2 === UInt(2),UInt(4),currentRadixD2).toUInt


  //currentRadixD2
  //memBanks.io.discardCalcWrite := Bool(false)

  //val currRad = Vec((0 until Params.getBF.rad.length).map( x => { val r = DSPBool(); r := DSPBool(rad === Count(Params.getBF.rad(x))); r }))

  val currRad = Vec((0 until butterfly.wfta.p.rad.length).map( x => { val r = DSPBool(); r := DSPBool(rad === Count(butterfly.wfta.p.rad(x))); r }))

  // ***** CHANGED FOR 2048 */
  butterfly.io.currRad.get := currRad //Vec(currRad(0),currRad(1))
  butterfly.io.calcDIT := DSPBool(calcPhaseD3)


  for (i <- 0 until butterfly.wfta.p.rad.max) {//generalConstants.maxRadix){
    memBanks.io.y(i) := butterfly.io.y(i)
    butterfly.io.x(i) := memBanks.io.x(i)
  }



  // CHANGED
  for (i <- 0 until butterfly.wfta.p.rad.max) {
    memBanks.io.calcBank(i) := calcBank(i)
    memBanks.io.calcAddr(i) := newCalcAddr(i).asOutput //calcAddr
  }






  // butterfly has 2x
  // *** mem rad needs 4 to 2
  // *** counter needs to be halved
  // address is duplicated 01 -> 23
  // *** needs to be changed to switch between versions i.e. support 3x, fail 120

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////



// Same caps




  val CalcCtrl = DSPModule(new CalcCtrl)
  CalcCtrl.ioCtrl.enable := globalInit.ioCtrlO.enable
  CalcCtrl.ioCtrl.reset := globalInit.ioCtrlO.reset
  CalcCtrl.generalSetup <> GeneralSetup.o
  CalcCtrl.ioFlags <> IOCtrl.ioFlagsNoDelay
  CalcCtrl.calcCtrlI <> globalInit.calcCtrlO
  if (CalcCtrl.delay != IOCtrl.delay) Error("Delays must match")







}