package FFT
import ChiselDSP._
import Chisel.{Pipe =>_,Complex => _,Mux => _, RegInit => _, RegNext => _, Counter => _, _}

class TwiddleGenO[T <: DSPQnm[T]](gen : => T) extends IOBundle {
  val twiddles = Vec(Params.getBF.rad.max-1,Complex(gen))
}

class TwiddleGen[T <: DSPQnm[T]](gen : => T) extends GenDSPModule(gen) {

  val intDly = List(1,1)

  // twiddleCounts, twiddleSubCounts, twiddleMuls
  val twiddleSetup = (new TwiddleSetupO).flip
  // currStage, we, isDIT, reset
  val calcCtrlFlag = (new CalcCtrlFlags).flip
  // stagePrimeIdx
  val ioSetup = (new IOSetupO).flip

  val o = new TwiddleGen(gen)

  // TODO: Function-ize
  // Gets associated twiddle counter max, sub counter max, & mul for each stage
  val twiddleCountUsed = twiddleSetup.twiddleCounts.zipWithIndex.foldLeft(DSPUInt(0))((accum,e) => {
    (e._1 ? (calcCtrlFlag.currStage === DSPUInt(e._2))) | accum
  })
  val twiddleSubCountUsed = twiddleSetup.twiddleSubCounts.zipWithIndex.foldLeft(DSPUInt(0))((accum,e) => {
    (e._1 ? (calcCtrlFlag.currStage === DSPUInt(e._2))) | accum
  })
  val twiddleMulUsed = twiddleSetup.twiddleMuls.zipWithIndex.foldLeft(DSPUInt(0))((accum,e) => {
    (e._1 ? (calcCtrlFlag.currStage === DSPUInt(e._2))) | accum
  })


  class TwiddleCounter extends Counter(CountParams(
    countMax = twiddleSetup.twiddleCounts.map(x => x.getRange.max.intValue).max
  ))
  class TwiddleSubCounter extends Counter(CountParams(
    countMax = twiddleSetup.twiddleSubCounts.map(x => x.getRange.max.intValue).max
  ))

  // TODO: Make a vec of counters?
  val twiddleCounter = DSPModule (new TwiddleCounter, "twiddleCounter")
  val twiddleSubCounter = DSPModule (new TwiddleSubCounter, "twiddleSubCounter")
  twiddleCounter.io.max.get := twiddleCountUsed
  twiddleSubCounter.io.max.get := twiddleSubCountUsed
  twiddleCounter.iCtrl.reset := calcCtrlFlag.reset
  twiddleSubCounter.iCtrl.reset := calcCtrlFlag.reset
  twiddleSubCounter.iCtrl.change.get := calcCtrlFlag.we
  twiddleCounter.iCtrl.change.get := twiddleSubCounter.oCtrl.change.get

  // Complete twiddle address
  val twiddleAddr = (twiddleCounter.io.out * twiddleMulUsed).shorten(Params.getTw.addrMax).pipe(intDly.head)

  // Originally columns associated with twiddles up to radix-1, but want to address column first
  val twiddleList = Params.getTw.vals.map(_.transpose)
  val twiddleLUTs = twiddleList.zipWithIndex.map{ case (primeSet,primeSetIdx) => {
    val setMaxRadix = Params.getIO.global(primeSetIdx)._2
    primeSet.zipWithIndex.map{ case (radixSet,radixSetIdx) => {
      // For each radix, radix-1 twiddle factors fed into butterfly (positions 1 to radix-1)
      val setButterflyInput = radixSetIdx + 1
      DSPModule(new ComplexLUT(radixSet,gen),"twiddleLUT_radix" + setMaxRadix + "_idx" + setButterflyInput)
    }}
  }}

  // Get prime index (associated with List (1) ++ global) for current stage
  val currPrimeIdx = ioSetup.stagePrimeIdx.zipWithIndex.foldLeft(DSPUInt(0))((accum,e) => {
    (e._1 ? (calcCtrlFlag.currStage === DSPUInt(e._2))) | accum
  })

  // Assign the right address to the twiddle LUTs (0 if current LUT not needed)
  // TODO: Don't need?
  val assignedTwiddleAddrs = Vec(twiddleLUTs.zipWithIndex.map{ case(lut,i) => {
    twiddleAddr ? (currPrimeIdx === DSPUInt(i))
  }})








  /*val assignedTwiddleAddrsDIT = Pipe(assignedTwiddleAddrs,)

    // calcnbankaddr dly

  nToAddrAndAddrToMemDly
    Pipe(twiddleAddrX(i), toAddrBankDly(1) + toMemAddrDly).asInstanceOf[UInt]

*/
















/*





  val calcDITD1 = Bool();
  calcDITD1 := Pipe(calcDIT, toAddrBankDly.sum + toMemAddrDly).asInstanceOf[Bool]






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



  debug(twiddleX)

 */

}