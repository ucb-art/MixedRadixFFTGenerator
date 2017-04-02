package FFT
import ChiselDSP._
import Chisel.{Pipe =>_,Complex => _,Mux => _, RegInit => _, RegNext => _, _}

class nToAddrBankIO extends IOBundle {
  val n = Vec(Params.getCalc.maxStages,DSPUInt(INPUT,Params.getBF.rad.max-1))
  // Mem bank
  val bank = DSPUInt(OUTPUT,Params.getMem.banks-1)
  // Address in bank
  val addr = DSPUInt(OUTPUT,Params.getMem.lengths.max-1)
}
class nToAddrBank extends DSPModule {

  val intDly = Params.getDelays.nToAddrBank

  // TODO: possibly add additional pipeline in nesting

  val addrMax = Params.getMem.lengths.max-1

  val generalSetup = (new GeneralSetupO).flip
  override val io = new nToAddrBankIO

  // Tree operations -- calculate depth
  val nestDepth = math.ceil(math.log(Params.getCalc.maxStages)/math.log(2)).toInt

  // Address = AC0*n0 + AC1*n1 + AC2*n2 + AC3*n3 + ...grouped doing addition of 2 numbers at a time (0,1),(2,3),etc.
  // Address range for ex: N = N1N2N3 where N3 largest = N2*n1+n2 up to N1N2-1 (reduction due to banking)
  // Bank = (n0 + n1 + n2 + n3 + ...) mod maxRadix broken up into mods of the sums of 2 n's at a time
  // ((n0+n1)%MR + (n2+n3)%MR)%MR, etc. mod is distributive
  // For 6 stages:
  // 0 1 2 3 4 5
  //  v   v   v
  //  0   1   2
  //    v     |
  //    0     1
  //       v
  //       0
  // For 7 stages:
  // 0 1 2 3 4 5 6
  //  v   v   v  |
  //  0   1   2  3
  //    v      v
  //    0      1
  //        v
  //        0

  val addrProd = Vec(generalSetup.addrConstants.zip(io.n).map{case (ac,n) => {
    (ac * n).shorten(addrMax).pipe(intDly)
  }})

  val addrVec = (0 until nestDepth-1).foldLeft(
    Vec(addrProd.grouped(2).map(e => e.reduceLeft(_+_).shorten(addrMax)).toList)
  )(
    (accum,e) => Vec(accum.grouped(2).map(e => e.reduceLeft(_+_).shorten(addrMax)).toList)
  )

  if (addrVec.length > 1) Error("Nest depth calculation incorrect")
  io.addr := addrVec.head

  // TODO: Use more functions! :) -- can I use reduceLeft isntead of foldLeft for top level?
  // TODO: Make tree function on List default method
  val addModFunc = (x:DSPUInt,y:DSPUInt) => Mod(x+y,generalSetup.maxRad)._1

  val bankVec = (0 until nestDepth-1).foldLeft(
    Vec(io.n.grouped(2).map(e => e.reduceLeft(addModFunc)).toList)
  )(
    (accum,e) => Vec(accum.grouped(2).map(e => e.reduceLeft(addModFunc)).toList)
  )
  io.bank := bankVec.head.pipe(intDly)

  // TODO: Support parallel butterflies

  if (io.addr.getDelay != io.bank.getDelay) Error("Address, bank delays don't match")
  val delay = io.addr.getDelay
  // Status ("nToAddBank module delay: " + delay)

}