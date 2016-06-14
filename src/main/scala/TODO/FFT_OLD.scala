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

  val CalcCtrl = DSPModule(new CalcCtrl)

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

  //////////////////////////////////////////////////////////////////////////////////////
  // Memory + Butterfly interface

  val calcDoneFlagD = Pipe(calcDoneFlag, toAddrBankDly.sum).asInstanceOf[Bool]

  val memBanks = DSPModule(new memBanks(gen))

  CheckDelay.off()

  memBanks.io.ioBank := ioBank
  memBanks.io.ioAddr := ioAddr
  memBanks.io.calcMemB := (!IOCtrl.ioFlagsNoDelay.isMemB).pipe(toAddrBankDly.sum).toBool  // not delayed in my version
  memBanks.io.calcDoneFlag := (!CalcCtrl.calcFlagsNoDelay.we).pipe(toAddrBankDly.sum).toBool  //not delayed in my version

  val currentRadixD2 = Pipe(currentRadix,2)//Reg(next = currentRadixD1)

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


  memBanks.io.discardCalcWrite :=(!CalcCtrl.calcFlagsNoDelay.we).pipe(toAddrBankDly.sum).toBool//Pipe(discardCalcWrite,toAddrBankDly.sum).asInstanceOf[Bool]

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

  // NEW VERSION EXPECTS MEMADDRDLY INTERNAL SO LESS THAT


  // reset held for ioToCalcClkRatio cycles -> Count 0 valid on the 1st cycle reset is low
  memBanks.io.Din := Pipe(DINused,ioToCalcClkRatio+toAddrBankDly.sum+toMemAddrDly).asInstanceOf[Complex[T]]

  memBanks.io.ioWriteFlag := IOCtrl.ioFlagsNoDelay.we //Pipe(ioWriteFlag,0).toBool

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
  ctrl.outValid := IOCtrl.ctrl.outValid //Pipe(frameFirstOutPreTemp,1)

  // don't let frame first out change before setup done?


  ctrl.k := IOCtrl.ctrl.k

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


  // Can update twiddle port in butterfly??

  // if radix 2 --> 4

  val tempcurrRad = Mux(currentRadixD2 === UInt(2),UInt(4),currentRadixD2).toUInt

  Mux(numPower(3) === UInt(0),tempcurrRad,currentRadixD2)
  // dly through calc
  memBanks.io.currRad := CalcCtrl.calcFlagsNoDelay.currRadNum.pipe(2).toUInt

  //Mux(numPower(3) === UInt(0),tempcurrRad,currentRadixD2)










  val currRad = Vec((0 until butterfly.wfta.p.rad.length).map( x => { val r = DSPBool(); r := DSPBool(rad === Count(butterfly.wfta.p.rad(x))); r }))

  // ***** CHANGED FOR 2048 */
  // for calc dly
  butterfly.io.currRad.get := Pipe(CalcCtrl.calcFlagsNoDelay.currRad,2+toMemAddrDly+seqRdDly)    //currRad //Vec(currRad(0),currRad(1))









  butterfly.io.calcDIT := CalcCtrl.calcFlagsNoDelay.isDIT.pipe(2+toMemAddrDly+seqRdDly)



  //  val calcPhaseD3 = Pipe(calcDIT,toAddrBankDly.sum+toMemAddrDly+seqRdDly)

  // DSPBool(calcPhaseD3)












  for (i <- 0 until butterfly.wfta.p.rad.max) {//generalConstants.maxRadix){
    memBanks.io.y(i) := butterfly.io.y(i)
    butterfly.io.x(i) := memBanks.io.x(i)
  }



  // CHANGED
  for (i <- 0 until butterfly.wfta.p.rad.max) {
    memBanks.io.calcBank(i) := CalcCtrl.o.banks(i)//calcBank(i)
    memBanks.io.calcAddr(i) := CalcCtrl.o.addrs(i)//newCalcAddr(i).asOutput //calcAddr
  }






  // butterfly has 2x
  // *** mem rad needs 4 to 2
  // *** counter needs to be halved
  // address is duplicated 01 -> 23
  // *** needs to be changed to switch between versions i.e. support 3x, fail 120

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////



// Same caps

  val SetupDone = DSPModule(new SetupDone(GeneralSetup.setupDelay + IOSetup.setupDelay + TwiddleSetup.setupDelay))
  SetupDone.io <> globalInit.setupDone
  setup.done := SetupDone.io.done


  //val CalcCtrl = DSPModule(new CalcCtrl)
  CalcCtrl.ioCtrl.enable := globalInit.ioCtrlO.enable
  CalcCtrl.ioCtrl.reset := globalInit.ioCtrlO.reset
  CalcCtrl.generalSetup <> GeneralSetup.o
  CalcCtrl.ioFlags <> IOCtrl.ioFlagsNoDelay
  CalcCtrl.calcCtrlI <> globalInit.calcCtrlO

  val TwiddleGen = DSPModule(new TwiddleGen(gen))
  TwiddleGen.twiddleSetup <> TwiddleSetup.o
  TwiddleGen.calcCtrlFlag <> CalcCtrl.calcFlagsNoDelay
  TwiddleGen.ioSetup <> IOSetup.o


  butterfly.io.twiddles.zipWithIndex.foreach{case (e,i) => {
    if (i < generalConstants.maxRadix-1){
      e := TwiddleGen.o.twiddles(i) // noob reg to match dly on data out of mem (should move to reg address instead of data)
    }
  }}

}