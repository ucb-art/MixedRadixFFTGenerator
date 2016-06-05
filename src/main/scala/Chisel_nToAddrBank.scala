package FFT
import ChiselDSP._
import Chisel.{Pipe =>_,Complex => _,Mux => _, RegInit => _, RegNext => _, _}

class nToAddrBankIO extends IOBundle {
  val n = Vec(Params.getCalc.maxStages,DSPUInt(INPUT,Params.getBF.rad.max-1))
  // Mem bank
  val bank = DSPUInt(OUTPUT,Params.getMem.banks)
  // Address in bank
  val addr = DSPUInt(OUTPUT,Params.getMem.lengths.max-1)
}
class nToAddrBank extends DSPModule {

  // TODO: possibly add additional pipeline in nesting

  val generalSetup = (new GeneralSetupO).flip
  override val io = new nToAddrBankIO

  val addrProd = Vec(generalSetup.addrConstants.zip(io.n).map{case (ac,n) => {
    (ac * n).shorten(Params.getMem.lengths.max-1).pipe(1)
  }})

  // Tree adds -- calculate depth
  val nestDepth = math.ceil(math.log(Params.getCalc.maxStages)/math.log(2)).toInt

  (0 until nestDepth-1).foldLeft(Vec(addrProd.grouped(2).map(e => e.reduceLeft(_+_)).toList))(
    (accum,e) => Vec(accum.grouped(2).map(e => e.reduceLeft(_+_)).toList)
  )

/// TODO: Make function, check last elem has length 1, scanleft?









}


/*







  // How many stages needed to perform full address/bank calculation; number of nodes at each stage
  val modOut = Vec((0 until stageBranch.length).map(x => {Vec.fill(stageBranch(x)){ Count(null,maxRad-1) }}))
  val partialSums = Vec((0 until stageBranch.length).map(x => {Vec.fill(stageBranch(x)){ Count(null,addrMax) }}))

  val addrProducts = Vec.fill(maxNumStages){Count(null,addrMax)}

  // Address = AC0*n0 + AC1*n1 + AC2*n2 + AC3*n3 + ...grouped doing addition of 2 numbers at a time (0,1),(2,3),etc.
  // Address range for ex: N = N1N2N3 where N3 largest = N2*n1+n2 up to N1N2-1 (reduction due to banking)
  // Bank = (n0 + n1 + n2 + n3 + ...) mod maxRadix broken up into mods of the sums of 2 n's at a time
  // ((n0+n1)%MR + (n2+n3)%MR)%MR, etc. mod is distributive
  // For 6 stages (stageBranch = [3 2 1]):
  // 0 1 2 3 4 5
  //  v   v   v
  //  0   1   2
  //    v     |
  //    0     1
  //       v
  //       0
  // For 7 stages (stageBranch = [4 2 1]):
  // 0 1 2 3 4 5 6
  //  v   v   v  |
  //  0   1   2  3
  //    v      v
  //    0      1
  //        v
  //        0

  for (i <- 0 until maxNumStages){
    addrProducts(i) := Pipe(io.n(i) * io.addrConstant(i),dly).asInstanceOf[UInt]
  }

  for (i <- 0 until stageBranch.length; j <- 0 until stageBranch(i)){
    // Odd # of n stages, just pass through last n
    if (i == 0 && maxNumStages%2 != 0 && j == stageBranch(i)-1){
      modOut(i)(j) := io.n(2*j)
      partialSums(i)(j) := addrProducts(2*j)
    }
    // Odd # of modOut/Partial sum values in previous calculation branch, just pass last modOut/Partial sum
    else if (i !=0 && stageBranch(i-1)%2 != 0 && j == stageBranch(i)-1 ){
      modOut(i)(j) := modOut(i-1)(2*j)
      partialSums(i)(j) := partialSums(i-1)(2*j)
    }
    else{
      val x = Count(null,maxRad*2-1)
      // First stage always takes in inputs (2 inputs -> 1 ouput reduction)
      if (i == 0){
        x := add1(io.n(2*j),io.n(2*j+1))
        partialSums(i)(j) := addrProducts(2*j)+addrProducts(2*j+1)
      }
      // Take in outputs of previous stage (2 inputs -> 1 ouput reduction)
      else{
        x := add1(modOut(i-1)(2*j),modOut(i-1)(2*j+1))
        partialSums(i)(j) := partialSums(i-1)(2*j)+partialSums(i-1)(2*j+1)
      }
      modOut(i)(j) := mod(x,io.maxRadix,maxRad)
    }
  }

  io.bank := Pipe(modOut(stageBranch.length-1)(0),dly).asInstanceOf[UInt]
  io.addr := partialSums(stageBranch.length-1)(0)

  // Currently doesn't support parallel butterflies

}



*/
