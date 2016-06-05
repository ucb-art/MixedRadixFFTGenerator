// August 19, 2015

// TODO: NOTE CURRENT IMPLEMENTATION REQUIRES 4^n

package FFT
import Chisel.{Pipe =>_,Complex => _,Mux => _, RegInit => _, RegNext => _, _}
import DSP._
import scala.math._
import memBanks._
import calc._
import Count._
import generator._

import scala.reflect.runtime.universe._

import ChiselDSP.{when => _, BackwardsCompatibility, _}

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
  val setup = new TopSetupIO
  val ctrl = new FFTCtrlIO

  // Derive IO clock from main clock
  val clkDiv = DSPModule(new ClkDiv(Params.getIO.clkRatio))
  val slowEn = clkDiv.io.slowEn

  // Length + FFT/IFFT setup
  val setupEn = slowEn & setup.SETUP_INIT
  val fftIndex = RegInit(DSPUInt(0,Params.getFFT.nCount - 1))
  val fftTF = RegInit(DSPBool(false))
  fftIndex := Mux(setupEn,setup.FFT_INDEX,fftIndex)
  fftTF := Mux(setupEn,setup.FFT,fftTF)

  // Setup location valid an IO clock cycle after (used to derive the remaining setup constants)
  val setupEnDly = setupEn.pipe(Params.getIO.clkRatio)

  // Make sure that IO reset on the right fast clock cycle
  val startFirstFrame = slowEn & ctrl.START_FIRST_FRAME



  val GeneralSetup =  DSPModule(new GeneralSetup)
  GeneralSetup.setupTop.fftIdx := fftIndex
  GeneralSetup.setupTop.enable := setupEnDly

  val IOSetup = DSPModule (new IOSetup(GeneralSetup.setupDelay))
  IOSetup.setupTop.fftIdx := fftIndex
  IOSetup.setupTop.enable := setupEnDly
  Status("ffff" + IOSetup.o.isUsed(0).getDelay)


  val IOCtrl = DSPModule(new IOCtrlX)
  IOCtrl.setupTop.fftIdx := fftIndex
  IOCtrl.setupTop.enable := setupEnDly
  IOCtrl.ctrl.ioEnable := slowEn
  IOCtrl.ctrl.startFrameIn := startFirstFrame
  IOCtrl.generalSetup <> GeneralSetup.o




  // IO constants

  // Used for masking to get coprime mod (when operating in Base N and not binary)
  // i.e. mod 4 is equivalent to a 000...00011 bit mask, except this is a digit mask
  // coprimes -> [coprime, corresponding prime, digit mask]
  val primeDigitsLUT = DSPModule(new IntLUT2D(Params.getIO.coprimes.map(_.map(_._3))), "primeDigits")
  primeDigitsLUT.io.addr := fftIndex
  val primeDigits = primeDigitsLUT.io.dout.cloneType
  primeDigits := RegNext(Mux(setupEnDly,primeDigitsLUT.io.dout,primeDigits))

  // Indices indicating order of prime decomposition i.e. (3,2,5) might have indices (1,0,2) if
  // Params.getIO.global has primes stored as (2,3,5)
  // global -> [prime used, prime base (max radix), max coprime]
  // globalPrimes(0) associated with unused
  val globalPrimes = List(1) ++ Params.getIO.global.map(_._1)
  val globalRads = List(0) ++ Params.getIO.global.map(_._2)
  val primeIndices = Params.getIO.coprimes.map(_.map{ x =>
    val prime = x._2
    globalPrimes.indexOf(prime)
  })
  val primeIdxLUT = DSPModule(new IntLUT2D(primeIndices), "primeIdx")
  primeIdxLUT.io.addr := fftIndex
  val primeIdx = primeIdxLUT.io.dout.cloneType
  //primeIdx := RegNext(Mux(setupEnDly,primeIdxLUT.io.dout,primeIdx))

  // TODO: Reduce redundant code
  // IO Q DIF + DIT
  val qDIFLUT = Params.getIO.qDIF.transpose.zipWithIndex.map{
    case (x,i) => DSPModule(new MixedBaseLUT(x), "qDIF_" + i.toString)
  }
  val qDITLUT = Params.getIO.qDIT.transpose.zipWithIndex.map{
    case (x,i) => DSPModule(new MixedBaseLUT(x), "qDIT_" + i.toString)
  }







































  val qDIFis = Vec(qDIFLUT.zipWithIndex.map { case (x, i) => {
    x.io.addr := DSPUInt(fftIndex, Params.getFFT.sizes.length - 1)
    val rad = if (i == 0) 4 else 3
    val usedOut = x.io.dout.find(_.rad == rad).get
    val tempOut = usedOut.cloneType

    println(usedOut.length)
    println(tempOut.length)

    tempOut := usedOut
    //Reg(tempOut) weirdest reg problem ever/!??!
    BaseN(tempOut.map(x => x.reg()), tempOut.rad)
  }
  })





























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


  // angie's noob piped
  /*val addressConstant = Vec.fill(generalConstants.maxNumStages) {
    Reg(UInt(width = Helper.bitWidth(pow(generalConstants.maxRadix, generalConstants.maxNumStages - 2).toInt)))
  }

  //addressConstant := addressConstantTemp
  for (i <- 0 until generalConstants.maxNumStages) {
    debug(addressConstant(i))
  }*/


println(Params.getMem.addrC)


  val addrConstantLUT = DSPModule(new IntLUT2D(Params.getMem.addrC))
  addrConstantLUT.io.addr := fftIndex
  //addressConstant := addrConstantLUT.io.dout

  val addressConstant = Vec(addrConstantLUT.io.dout.map(_.cloneType.toUInt))
  addressConstant := Vec(addrConstantLUT.io.dout.map(_.reg().toUInt))









  val twiddleCountArray = Params.getTw.countMax.transpose //twiddleConstants.twiddleCountMaxArray.transpose
  val twiddleCountColCount = twiddleCountArray.length
  val twiddleCountLUT = Vec((0 until twiddleCountColCount).map(x => Module(new UIntLUT(twiddleCountArray(x).toArray)).io))
  val twiddleCount = Vec.fill(twiddleCountColCount) {
    Reg(UInt())
  }
  var maxTwiddleCountBitWidth: Int = 0
  for (i <- 0 until twiddleCountColCount) {
    twiddleCountLUT(i).addr := fftIndex
    twiddleCount(i) := twiddleCountLUT(i).dout
    debug(twiddleCount(i))

    var twiddleCountBitWidth = Helper.bitWidth(twiddleCountArray(i).max)
    if (twiddleCountBitWidth > maxTwiddleCountBitWidth) {
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

  // poor man's pipeD
  val twiddleSubCountMax = Vec.fill(3) {
    Reg(UInt(width = Helper.bitWidth(Params.getFFT.sizes.max)))
  }






  val twiddleSubcountLUT = DSPModule(new IntLUT2D(Params.getTw.subcountMax))
  twiddleSubcountLUT.io.addr := fftIndex

  twiddleSubCountMax.init.zipWithIndex.foreach{ case(x,i) => {
    x := twiddleSubcountLUT.io.dout(i).toUInt
  }}
  twiddleSubCountMax.last := UInt(0,width=twiddleSubCountMax.last.getWidth)


  //twiddleSubCountMax := Vec(twiddleSubcountLUT.io.dout.map(_.toUInt)) //twiddleSubcountLUT.io.dout

  // Base Twiddle Address Multiplier (Renormalize to Twiddle ROM size)
  // Unused signal should be optimized out in Verilog [ie Mul factor for powers of 2]
  // clk 3


  println("ccc" + Params.getTw.LUTScale)


   val radNTwiddleMul = Vec.fill(3) {
    Reg(UInt())
  }



  val testRadNMul = DSPModule (new IntLUT2D(Params.getTw.LUTScale))
  testRadNMul.io.addr := fftIndex
  radNTwiddleMul := Vec(testRadNMul.io.dout.map(_.toUInt))













  // For DIF: Initial RadXTwiddleMulFactor due to scaling max memory
  // based off of max coprime N to the coprime N actually used (see address gen block)
  // Subsequent radix-X stages have the mul
  // factor modified to correspond to *R1 for stage 2, *R1*R2 for
  // stage 3, etc. as seen above
  // clk 5
  val twiddleMulTemp = Vec.fill(generalConstants.maxNumStages) {
    Reg(UInt())
  }
  val twiddleMul = Vec.fill(generalConstants.maxNumStages) {
    Reg(UInt())
  }
  // Note that whenever a radix-2 stage is used, regardless of the Mul value, the
  // final twiddle address will always be 0
  for (i <- 0 until generalConstants.maxNumStages) {
    if (i == 0) {
      // Always the start of a new radix, unless
      // radix-4 is allowed but there is no radix-4 used in this FFT N
      // i.e. if the first stage is radix-2, don't care

      twiddleMulTemp(0) := radNTwiddleMul(3 - 1)
      for (j <- 3 - 2 to 0 by -1) {
        if (generalConstants.pow2SupportedTF && generalConstants.rad4Used) {
          when(stageSum(j + 1) != UInt(0)) {
            // Radix {(42)35} -> Primes{235} re-index
            twiddleMulTemp(0) := radNTwiddleMul(j)
          }
        }
        else {
          when(stageSum(j) != UInt(0)) {
            // Re-index not needed
            twiddleMulTemp(0) := radNTwiddleMul(j)
          }
        }
      }
    }
    else {
      when(stageRadix(i) === UInt(0)) {
        // Unused stages (or radix-2, but ddon't care)
        twiddleMulTemp(i) := UInt(0)
      }.elsewhen(stageRadix(i) != stageRadix(i - 1)) {
        // Current radix is different from previous radix --> need new Mul base
        twiddleMulTemp(i) := radNTwiddleMul(3 - 1)
        for (j <- 3 - 2 to 1 by -1) {
          // If Rad2 (first) base mul was relevant, it would have already been used in stage 0
          if (generalConstants.pow2SupportedTF && generalConstants.rad4Used) {
            when(stageSum(j) === UInt(i)) {
              // Radix {(42)35} -> Primes{235} re-index
              twiddleMulTemp(i) := radNTwiddleMul(j)
            }
          }
          else {
            when(stageSum(j - 1) === UInt(i)) {
              // Shift index compared to before
              twiddleMulTemp(i) := radNTwiddleMul(j)
            }
          }
        }
      }.otherwise {
        twiddleMulTemp(i) := twiddleMulTemp(i - 1) * stageRadix(i) //pipeD(twiddleMulTemp(i-1)*stageRadix(i),2).asInstanceOf[UInt] 				// If current stage has the same radix as the previous stage,
        // Change the multiplication factor * radix
      }
    }
  }
  twiddleMul := twiddleMulTemp
  for (i <- 0 until generalConstants.maxNumStages) {
    debug(twiddleMul(i))
  }

  // Counter reset whenever new FFTN desired, stays constant after setup is done SHOULD OPTIMIZE
  val setupDoneCount: Int = 4 + generalConstants.maxNumStages * 3 + 10
  val setupCounter = Module(new accumulator(Helper.bitWidth(setupDoneCount)))
  val setupDoneTemp = (UInt(setupDoneCount) === setupCounter.io.out)
  setupCounter.io.inc := UInt(1, width = 1)
  setupCounter.io.changeCond := ~setupDoneTemp
  setupCounter.io.globalReset := setup.SETUP_INIT
  setupCounter.io.wrapCond := Bool(false)

  val setupDoneTempD1 = Reg(next = setupDoneTemp)
  setup.SETUP_DONE := DSPBool(setupDoneTemp || setupDoneTempD1)
  // Hold for 2 cycles


  ////////////////////////////////////////////////////////////////////////////////////////////////////////
  // IO Addressing














  
  val ioWriteFlag = slowEn













////////////////////////////////////////////////



























  val (ioIncCounters1, ioModCounters1) = Params.getIO.global.map{ case (prime,rad,maxCoprime) => {
    val c1 = BaseNIncCounter(rad, maxCoprime, Params.getIO.clkRatio, nameExt = "rad_" + rad.toString)
    val c2 = {
      if (maxCoprime != Params.getIO.global.last._3)
        Some(BaseNAccWithWrap(rad, maxCoprime, Params.getIO.clkRatio, nameExt = "rad_" + rad.toString))
      else
        None
    }
    (c1, c2)
  }}.unzip



  // Right-most counter is "least significant"
  val ioIncCounts1 = Vec(ioIncCounters1.zipWithIndex.map { case (e, i) => {
    val iChange = {
      if (e == ioIncCounters1.last) DSPBool(true)
      else if (i == ioIncCounters1.length-2) ioIncCounters1.last.ctrl.isMax
      else ioIncCounters1(1).ctrl.isMax & ioIncCounters1(2).ctrl.isMax
    }

    //val iChange = if (e != ioIncCounters1.last) ioIncCounters1(i + 1).ctrl.isMax else DSPBool(true)//DSPBool(slowEn)
    e.ctrl.change.get := iChange
    e.ctrl.en.get := slowEn

    // already delay 1
    e.ctrl.reset := startFirstFrame //DSPBool(ctrl.START_FIRST_FRAME)
    // Should not need? can also trim to io.primDigits length
    e.io.primeDigits := primeDigits(i).shorten(e.io.primeDigits.getRange.max)
    val temp = e.io.out.cloneType
    temp := e.io.out
    temp
  }
  })
  // Do zip together
  val ioModCounts1 = Vec(ioModCounters1.init.zipWithIndex.map { case (etemp, i) => {
    val e = etemp.get
    e.ctrl.en.get := slowEn //DSPBool(slowEn)
    e.ctrl.reset := startFirstFrame.pipe(0) //DSPBool(ctrl.START_FIRST_FRAME)
    // Should not need? can also trim to io.primDigits length
    e.io.primeDigits := primeDigits(i).shorten(e.io.primeDigits.getRange.max)
    e.io.inc.get := qDIFis(i).padTo(e.io.inc.get.length).asOutput // ???
    e.ctrl.wrap.get := ioIncCounters1(i).ctrl.change.get
    val temp = e.io.out.cloneType
    temp := e.io.out
    temp
  }
  })





  // Should try to minimize mem output length and pad where there is width mismatch
  // CAN PIPELINE DELAY ioFinalConuts
  val ioFinalCounts = Vec(Params.getIO.global.map(_._3).zipWithIndex.map { case (x, i) => {
    if (x == Params.getIO.global.last._3) ioIncCounts1(i)
    else (ioIncCounts1(i) + ioModCounts1(i)).maskWithMaxCheck(primeDigits(i))._1
  }
  })
  debug(ioFinalCounts)





  val iDIFtemp1 = Vec(ioFinalCounts.zipWithIndex.map { case (x, i) => {
    if (Params.getBF.rad.contains(2) && Params.getIO.global(i)._1 == 2 && Params.getBF.rad.contains(4)) {
      // switch to getBF.rad(i) == 2  ????
      // Convert to Mixed radix [4,...4,2] if current FFT size requires radix 2 stage & operating on 2^n coprime
      // FOR DIF 2 always follows 4 and it's the only one with list of length 2
      Mux(DSPBool(numPower(i + 1)(0)), x.toRad42(), x)
    }
    else x
  }
  })
  // registered here?
  // Need to be same length to address properly (really Chisel should error)
  val colLengths = iDIFtemp1.map(x => x.length).max
  val iDIFtemp = Vec(iDIFtemp1.map(x => {
    val set = x.map(y => {
      // delay 2 (calc clk)
      y.reg()
    })
    BaseN(set, x.rad).padTo(colLengths)
    //Vec(set) // basen gets misinterpretted?
  }))
  iDIFtemp.foreach {
    debug(_)
  }
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






  val ions = Vec((0 until generalConstants.maxNumStages).map(i => {
    val primeVal = {
      if (Params.getBF.rad.contains(4)) {
        // Mux has problems interpretting without doing explicit .toBool, etc.?
        Mux((stageRadix(i) === UInt(4)).toBool, UInt(2), stageRadix(i)).toUInt
      }
      else stageRadix(i)
    }
    val primes = Params.getIO.global.map(_._1).zipWithIndex
    // Mutually exclusive conditions
    val primeIdx = primes.tail.foldLeft(
      DSPUInt(primes.head._2) ? DSPBool(primeVal === UInt(primes.head._1))
    )((b, a) => {
      val selIdx = DSPUInt(a._2) ? DSPBool(primeVal === DSPUInt(a._1))
      selIdx /| b
    })
    // Why all these explicit conversions?!
    // primeIdx defaults to 0 when prime unused (which has smallest sum value so muxes to 0 if unused)
    val currStageSum = newStageSum(primeIdx.toUInt).toUInt // NOT MINUS 1 (since 0 - 1 = 3)
    val activeStage = (UInt(i) < currStageSum).toBool
    val digitIdx = (currStageSum - UInt(1 + i)).toUInt
    // Addressing Vec should mark as used (override vec)

    val countSet = Vec(iDIFtemp(primeIdx.toUInt).map(x => x.toUInt))
    val digit = countSet(digitIdx).toUInt
    Mux(activeStage, digit, UInt(0)).toUInt
    // := auto pads if right range smaller?
    // Seems searchable vec needs to be made of uints only?

  }))
  ions.foreach {
    debug(_)
  }















  //separate mux??? don't need bc newstagesumm1(0) has smallest val

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


  // OUTPUT






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



















  val qDIFoLUTs = Params.getIO.qDIT.transpose.map { x => {DSPModule(new MixedBaseLUT(x))}}
  val qDIFos = Vec(qDIFoLUTs.zipWithIndex.map { case (x, i) => {
    x.io.addr := DSPUInt(fftIndex, Params.getFFT.sizes.length - 1)
    val rad = if (i == 0) 5 else 3
    val usedOut = x.io.dout.find(_.rad == rad).get
    val tempOut = usedOut.cloneType
    tempOut := usedOut
    //Reg(tempOut) weirdest reg problem ever/!??!
    BaseN(tempOut.map(x => x.reg()), tempOut.rad)
  }
  })

  val (oIncCounters, oModCounters) = Params.getIO.global.reverse.map{ case (prime,rad,maxCoprime) => {
    val c1 = BaseNIncCounter(rad, maxCoprime,Params.getIO.clkRatio, nameExt = "rad_" + rad.toString)
    val c2 = {
      if (maxCoprime != Params.getIO.global.reverse.last._3)
        Some(BaseNAccWithWrap(rad, maxCoprime, Params.getIO.clkRatio,nameExt = "rad_" + rad.toString))
      else
        None
    }
    (c1, c2)
  }}.unzip




  // debug list of stuff
  // CHANGE ALL REG TO PIPE -- should there be an enclosing vec? for all iterables


  // Right-most counter is "least significant"
  val oIncCounts = Vec(oIncCounters.zipWithIndex.map { case (e, i) => {
    val iChange = {
      if (e == oIncCounters.last) DSPBool(true)
      else if (i == oIncCounters.length-2) oIncCounters(2).ctrl.isMax
      else oIncCounters(1).ctrl.isMax & oIncCounters(2).ctrl.isMax
    }/*if (e != oIncCounters.last) {
      (i+1 until oIncCounters.length).fold(oIncCounters(_).ctrl.isMax & oIncCounters(_).ctrl.isMax) //oIncCounters(i + 1).ctrl.isMax
    } else DSPBool(true)//slowEn)*/
    e.ctrl.change.get := iChange
    e.ctrl.reset := startFirstFrame
    e.ctrl.en.get := slowEn
    // Should not need? can also trim to io.primDigits length
    e.io.primeDigits := Vec(primeDigits.reverse)(i).shorten(e.io.primeDigits.getRange.max)
    val temp = e.io.out.cloneType
    temp := e.io.out
    temp
  }
  })



  // Do zip together
  val oModCounts = Vec(oModCounters.init.zipWithIndex.map { case (etemp, i) => {
    val e = etemp.get
    e.ctrl.en.get := slowEn
    e.ctrl.reset := ctrl.START_FIRST_FRAME.pipe(0)
    // Should not need? can also trim to io.primDigits length
    e.io.primeDigits := Vec(primeDigits.reverse)(i).shorten(e.io.primeDigits.getRange.max)
    e.io.inc.get := qDIFos(i).padTo(e.io.inc.get.length).asOutput // ???
    e.ctrl.wrap.get := oIncCounters(i).ctrl.change.get
    val temp = e.io.out.cloneType
    temp := e.io.out
    temp
  }
  })


  val oFinalCounts = Vec(Params.getIO.global.reverse.map(_._3).zipWithIndex.map { case (x, i) => {
    if (x == Params.getIO.global.reverse.last._3) oIncCounts(i)
    else (oIncCounts(i) + oModCounts(i)).maskWithMaxCheck(Vec(primeDigits.reverse)(i))._1
  }
  })



  debug(oFinalCounts)




  /*

  // Should try to minimize mem output length and pad where there is width mismatch
  // CAN PIPELINE DELAY ioFinalConuts
  val oFinalCounts = Vec(Params.getIO.maxCoprimes.reverse.zipWithIndex.map { case (x, i) => {
    if (x == Params.getIO.maxCoprimes.reverse.last) oIncCounts(i)
    else (oIncCounts(i) + oModCounts(i)).maskWithMaxCheck(Vec(primeDigits.reverse)(i))._1
  }
  })


  */

  val oDIFtemp1 = Vec(oFinalCounts.zipWithIndex.map { case (x, i) => {
    if (Params.getBF.rad.contains(2) && Params.getIO.global.map(_._1).reverse(i) == 2 && Params.getBF.rad.contains(4)) {
      // switch to getBF.rad(i) == 2  ????
      // Convert to Mixed radix [4,...4,2] if current FFT size requires radix 2 stage & operating on 2^n coprime
      // FOR DIF 2 always follows 4 and it's the only one with list of length 2
      /// FIX BETTER PARAM

      // WHY DOES OUT NOT NEED CONVERSION TO RAD42?
      //Mux(DSPBool(numPower(1)(0)), x.toRad42(), x)
      x
    }
    else x
  }
  })





  ///// UP to here is right

  // registered here?
  // Need to be same length to address properly (really Chisel should error)
  val colLengthso = oDIFtemp1.map(x => x.length).max
  val oDIFtemp = Vec(oDIFtemp1.map(x => {
    val set = x.map(y => {
      y.reg()
    })
    BaseN(set, x.rad).padTo(colLengthso)
    //Vec(set) // basen gets misinterpretted?
  }).reverse).asOutput                                         // NOTE REVERSED HERE TO MATTCH ADDRESS

  // can't mix constants + vals
  oDIFtemp.foreach {
    debug(_)
  }
  // when doing addr, warn when not vec col nto same length

  // StageRadix === should be just series of Bools
  // table use2 YN? but otherwise lump stage cnt into 4 -- stage sum should not separate 4,2 (should be by coprime)
  // bad assumption here (i.e. 4 must be first)

















  // for output , to match /w address, keep other logic same, just flip order of oDIFtemp and also their internal digits

  val ons = Vec((0 until generalConstants.maxNumStages).map(i => {
    val primeVal = {
      if (Params.getBF.rad.contains(4)) {
        // Mux has problems interpretting without doing explicit .toBool, etc.?
        Mux((stageRadix(i) === UInt(4)).toBool, UInt(2), stageRadix(i)).toUInt
      }
      else stageRadix(i)
    }







    val primes = Params.getIO.global.map(_._1).zipWithIndex
    // Mutually exclusive conditions
    val primeIdx = primes.tail.foldLeft(
      DSPUInt(primes.head._2) ? DSPBool(primeVal === UInt(primes.head._1))
    )((b, a) => {
      val selIdx = DSPUInt(a._2) ? DSPBool(primeVal === DSPUInt(a._1))
      selIdx /| b
    })


    // Why all these explicit conversions?!
    // primeIdx defaults to 0 when prime unused (which has smallest sum value so muxes to 0 if unused)

    val currStageSum = newStageSum(primeIdx.toUInt).toUInt // NOT MINUS 1 (since 0 - 1 = 3)

    val countSet = Vec(oDIFtemp(primeIdx.toUInt).map(x => x.toUInt))

    val activeStage = (UInt(i) < currStageSum).toBool

    //val prevstgidx = Mux((stageRadix(i) === UInt(2)).toBool,(primeIdx.toUInt-UInt(2)).toUInt,(primeIdx.toUInt-UInt(1)).toUInt).toUInt
    val prevStageSumidx = primeIdx.toUInt-UInt(1) //Mux((primeIdx.toUInt ===UInt(0)).toBool,UInt(0),primeIdx.toUInt-UInt(1)).toUInt  //if (i == 0) UInt(0) else newStageSum(primeIdx.toUInt-UInt(1)).toUInt
    val prevStageSum =Mux((primeIdx.toUInt ===UInt(0)).toBool,UInt(0),newStageSum(prevStageSumidx).toUInt)

    val digitIdx = (UInt(i)-prevStageSum).toUInt //    (currStageSum - UInt(1 + i)).toUInt
    // Addressing Vec should mark as used (override vec)


    val digit = countSet(digitIdx).toUInt
    Mux(activeStage, digit, UInt(0)).toUInt
    // := auto pads if right range smaller?
    // Seems searchable vec needs to be made of uints only?

  }))
  ons.foreach {
    debug(_)
  }

  // store rad by index instead of rad








  ////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Calculation Addressing

  val calcMemChangeCond = Bool(OUTPUT)
  calcMemChangeCond := ioIncCounters1.head.ctrl.isMax & slowEn & ioIncCounters1(1).ctrl.isMax & ioIncCounters1(2).ctrl.isMax
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

  /////// REDUNDANT LOGIC FROM CALC CTRL: Put in IO Ctrl instead
  /*
  val calcMemBTemp = DSPBool(OUTPUT)
  val calcMemBTemp1 =  Mux(DSPBool(calcMemChangeCond),!calcMemBTemp,calcMemBTemp)
  calcMemBTemp := ChiselDSP.Reg(Mux(DSPBool(io.START_FIRST_FRAME),DSPBool(false),calcMemBTemp1))
  val ioDITTemp = DSPBool(OUTPUT)
  val ioDITTemp1 =  Mux(DSPBool(calcMemChangeCond) & !calcMemBTemp,!ioDITTemp,ioDITTemp)
  CheckDelay.off()
ioDITTemp := Pipe(Mux(DSPBool(io.START_FIRST_FRAME),DSPBool(false),ioDITTemp1),2)


  val io_ns = Vec(ions.zip(oDIFn).map{case (in,on) => Mux(ioDITTemp, DSPUInt(on,DSPUInt.toMax(on.getWidth)), DSPUInt(in,DSPUInt.toMax(in.getWidth)))})
*/
  // n1,n2,n3... -> input DIF address/banks

  val iDIFnToBankAddr = Module(new nToBankAddr(toAddrBankDly(1)))
  for (i <- 0 until generalConstants.maxNumStages) {
    iDIFnToBankAddr.io.n(i) := ions(i) //io_ns(i).toUInt  //ions(i)//iDIFn(i)//ions(i)
    iDIFnToBankAddr.io.addrConstant(i) := addressConstant(i) //Pipe(addressConstant(i),1)
  }
  iDIFnToBankAddr.io.maxRadix := maxRadix
  //Pipe(maxRadix,1)									// two kinds of max radices: generalConstants.maxRadix = overall max radix for generator; maxRadix = max radix for current FFT
  val iDIFAddr = Count(null, addrMax);
  iDIFAddr := iDIFnToBankAddr.io.addr
  val iDIFBank = Count(null, bankMax);
  iDIFBank := iDIFnToBankAddr.io.bank
  debug(iDIFAddr) // Note: total delay 2 cycles
  debug(iDIFBank)

  CheckDelay.on()

  val oDIFnToBankAddr = Module(new nToBankAddr(toAddrBankDly(1)))
  for (i <- 0 until generalConstants.maxNumStages) {
    oDIFnToBankAddr.io.n(i) := ons(i)//oDIFn(i)
    oDIFnToBankAddr.io.addrConstant(i) := addressConstant(i)
  }
  oDIFnToBankAddr.io.maxRadix := maxRadix
  // two kinds of max radices: generalConstants.maxRadix = overall max radix for generator; maxRadix = max radix for current FFT
  val oDIFAddr = Count(null, addrMax);
  oDIFAddr := oDIFnToBankAddr.io.addr
  val oDIFBank = Count(null, bankMax);
  oDIFBank := oDIFnToBankAddr.io.bank
  debug(oDIFAddr) // Note: total delay 2 cycles second is toAddrBankDly(1); after difcount is (0)
  debug(oDIFBank)

  val ioAddr = muxU(iDIFAddr, oDIFAddr, ioDIT)
  val ioBank = muxU(iDIFBank, oDIFBank, ioDIT)

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
  calcControl.io.startFirstFrame := ctrl.START_FIRST_FRAME



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

  val twiddleCountMaxUsed = UInt(twiddleCount(currentStage), width = maxTwiddleCountBitWidth)
  val twiddleSubCountMaxUsed = UInt(width = Helper.bitWidth(Params.getFFT.sizes.max))
  // Note for subcount, power of 2 is default.
  // Power of 2 includes radix 4. Also note that when calculating
  // for the radix-2 stage, the overall twiddle count should be 0,
  // so it doesn't matter what the subcount value is
  // Subcount max depends on remaining coprimes
  twiddleSubCountMaxUsed := twiddleSubCountMax(0)
  for (i <- generalConstants.validPrimes.length - 1 to 0 by -1) {
    when(currentRadix === UInt(generalConstants.validPrimes(i))) {
      twiddleSubCountMaxUsed := twiddleSubCountMax(i)
    }
  }
  debug(twiddleCountMaxUsed)
  debug(twiddleSubCountMaxUsed)

  // Non-scaled twiddle counters
  // Subcounter to handle coprimes (holds main count value)
  // Counter to deal with current coprime
  val twiddleSubCounter = Module(new accumulator(Helper.bitWidth(Params.getFFT.sizes.max))).io
  val twiddleCounter = Module(new accumulator(maxTwiddleCountBitWidth)).io
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

  val twiddleMulUsed = twiddleMul(currentStage) // twiddle address scale factor
  debug(twiddleMulUsed)


  // Switch rows/cols so Scala doesn't complain (originally columns are associated with twiddle up to radix-1, but want to address "column" first -> tranpose)
  var twiddleArray = Params.getTw.vals.map(
    _.transpose
  )//Array.ofDim[ScalaComplex](generalConstants.validPrimes.length, 0, 0)
  /*for (i <- 0 until twiddleArray.length) {
    twiddleArray(i) = Params.getTw.twiddles(i).transpose  //twiddleConstants.twiddleConstantsArray(i).transpose
  }*/
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

  // If first value comes in when START_FIRST_FRAME is asserted high,
  // there is a delay until address to the memory is valid
  // IFFT --> real+imaginary inputs/outputs swapped

  val DINusedreal = Mux(DSPBool(setup.FFT), io.DATA_IN.real, io.DATA_IN.imag)
  val DINusedimag = Mux(DSPBool(setup.FFT),io.DATA_IN.imag,io.DATA_IN.real)
  val DINused =  Complex(DINusedreal,DINusedimag).pipe(1+0)








  // Added normalization

  val normalizedDelay = if (Params.getFFT.normalized) {
    val Normalize = DSPModule(new Normalize(gen), "normalize")
    Normalize.io.in := memBanks.io.Dout
    Normalize.io.fftIdx := fftIndex
    Normalize.io.FFT := fftTF
    val normalizedOut = Normalize.io.normalizedOut.cloneType()
    normalizedOut := Normalize.io.normalizedOut

    io.DATA_OUT.real := Mux(DSPBool(setup.FFT), normalizedOut.real, normalizedOut.imag).pipe(1)
    io.DATA_OUT.imag := Mux(DSPBool(setup.FFT), normalizedOut.imag, normalizedOut.real).pipe(1) // reg b/c delayed 1 cycle from memout reg, but delay another to get back to io cycle
    Normalize.delay
  }
  else {
    io.DATA_OUT.real := Mux(DSPBool(setup.FFT),memBanks.io.Dout.real,memBanks.io.Dout.imag).pipe(1)
    io.DATA_OUT.imag := Mux(DSPBool(setup.FFT), memBanks.io.Dout.imag, memBanks.io.Dout.real).pipe(1)   // reg b/c delayed 1 cycle from memout reg, but delay another to get back to io cycle
    0
  }












  // START_FIRST_FRAME held for ioToCalcClkRatio cycles -> Count 0 valid on the 1st cycle START_FIRST_FRAME is low
  memBanks.io.Din := Pipe(DINused,ioToCalcClkRatio+toAddrBankDly.sum+toMemAddrDly).asInstanceOf[Complex[T]]
























  memBanks.io.ioWriteFlag := Pipe(ioWriteFlag,0).toBool

  val firstDataFlag = Reg(next = calcMemChangeCond && ~ctrl.START_FIRST_FRAME.toBool)	// Cycle 0 - don't output when first frame is being fed in (output data not valid)

  val secondInPassedFlag = Reg(init = Bool(false))
  when (ctrl.START_FIRST_FRAME.toBool){
    secondInPassedFlag := Bool(false)										// Reset
  }.elsewhen(firstDataFlag){													// Will go high at the beginning of each new input symbol starting with the 2nd input symbol
    secondInPassedFlag := Bool(true)										// True indicates second input symbol has already been processed
  }

  val firstDataFlagD1 = Reg(next = firstDataFlag && secondInPassedFlag && ~ctrl.START_FIRST_FRAME.toBool)		// Output data only valid at the start of 3rd input symbol (when secondInPassedFlag is high)
  val firstDataFlagD2 = Reg(next = firstDataFlagD1 && ~ctrl.START_FIRST_FRAME.toBool)							// Reset all registers at start of first symbol to make sure unknown states aren't propagated
  val firstDataFlagD3 = Reg(next = firstDataFlagD2 && ~ctrl.START_FIRST_FRAME.toBool)
  val firstDataFlagD4 = Reg(next = firstDataFlagD3 && ~ctrl.START_FIRST_FRAME.toBool)							// Flag needs to be 2 fast clock cycles long
  val firstDataFlagD5 = Reg(next = firstDataFlagD4 && ~ctrl.START_FIRST_FRAME.toBool)
  val firstDataFlagD6 = Reg(next = firstDataFlagD5 && ~ctrl.START_FIRST_FRAME.toBool)


  val firstDataFlagD7 = Reg(next = firstDataFlagD6 && ~ctrl.START_FIRST_FRAME.toBool)
  val firstDataFlagD8 = Reg(next = firstDataFlagD7 && ~ctrl.START_FIRST_FRAME.toBool)












  // note seqrd dly was already incremented so instead of originally starting at cycle 82 for 12, it starts at cycle 83, add 1 to make consistent w/ io

  // + 1 used to be inside  2nd pipe (i.e. seqrddly + 1)
  val frameFirstOutPre =  Pipe(!ctrl.START_FIRST_FRAME & Pipe(DSPBool(firstDataFlagD3	| firstDataFlagD4) & !ctrl.START_FIRST_FRAME,seqRdDly),normalizedDelay)
  val frameFirstOutPreTemp = !ctrl.START_FIRST_FRAME & frameFirstOutPre
  ctrl.FRAME_FIRST_OUT := Pipe(frameFirstOutPreTemp,1)



  // don't let frame first out change before setup done?



  // TODO: check timing of offsetCountEnable w/ frameFirstOutPreTemp
  val offsetCountEnable = RegInit(DSPBool(false))
  offsetCountEnable := Mux(frameFirstOutPreTemp & setup.SETUP_DONE,DSPBool(true),offsetCountEnable)
  val offsetCounter = IncReset(Params.getFFT.sizes.max-1,nameExt="offset")
  offsetCounter.iCtrl.reset := frameFirstOutPreTemp
  offsetCounter.iCtrl.change.get := slowEn & offsetCountEnable
  ctrl.OFFSET := offsetCounter.io.out















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

  /*// Debug
  for (i <- 0 until generalConstants.numBanks){
    memBanks.io.y(i) := pipeD(memBanks.io.x(i) * Complex(double2T(10.0),double2T(0)),pipeBFWriteDly).asInstanceOf[Complex[T]]
  }*/




/*

  val myArray = Vec(UInt(1),UInt(2),UInt(3),UInt(4),UInt(5),UInt(1),UInt(1),UInt(2))



  val sum = myArray.foldLeft(UInt(0,width=5))((accum, e) => accum + e)
  





  debug(sum)*/



  /* val butterfly2 = DSPModule(new PE(gen,num = peNum), nameExt = peNum.toString)
   val bfio = new PEIO(gen)
   bfio <> butterfly2.io*/

  // butterfly has 2x
  // *** mem rad needs 4 to 2
  // *** counter needs to be halved
  // address is duplicated 01 -> 23
  // *** needs to be changed to switch between versions i.e. support 3x, fail 120

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////



}