// September 12, 2015

package FFT 

import Chisel.{Complex => _, Mux => _, RegNext => _, RegInit => _, Pipe => _, Mem => _,
               Module => _, ModuleOverride => _, when => _, switch => _, is => _, unless => _, Round => _,  _}
import ChiselDSP.{Reg =>_,_}
import scala.math._
import memBanks._
import schedule._
import DSP._
import Count._

object memBanks{

  val maxRad = Params.getBF.rad.max

  // Extra write delay due to butterfly pipelining
  var pipeBFWriteDly = 0  
  var numBanks = maxRad
  var memLengths = Array.fill[Int](numBanks)(1)
  var addrMax = memLengths.max-1
  var bankMax = numBanks-1
  
  // Amount to delay control signals dependent on whether address to mem was registered
  var toMemAddrDly = 1
  
  def updateMemConstants(numBnks:Int,memLens:Array[Int]){
    numBanks = numBnks
    memLengths = memLens
    addrMax = memLengths.max-1
    bankMax = numBanks-1
  }
  
  /* CONSTANTS */
  val A = 0
  val B = 1
  
}

class memBanks[T <: DSPQnm[T]](gen : => T) extends GenDSPModule (gen) {

  CheckDelay.off()

  val maxRad = Params.getBF.rad.max

  override val io = new  IOBundle {

    val calcBank = Vec.fill(numBanks){Count(INPUT,bankMax)}
    val calcAddr = Vec.fill(numBanks){Count(INPUT,addrMax)}
    val ioBank = Count(INPUT,bankMax)
    val ioAddr = Count(INPUT,addrMax)
    
    val currRad = Count(INPUT,maxRad)
    
    // y (from butterflies) -> write to memory 
    // read from memory -> x (to butterflies)
    val y = Vec.fill(numBanks){Complex(gen,gen).asInput}
    val x = Vec.fill(numBanks){Complex(gen,gen).asOutput}
    val Din = Complex(gen,gen).asInput
    val Dout = Complex(gen,gen).asOutput
    
    // If Mem A is in calculation, Mem B is collecting IO
    val calcMemB = Bool(INPUT)
    // If done, hold calculation memory values
    val calcDoneFlag = Bool(INPUT)
    // When to read/write IO (when calculation clock != IO clock)
    val ioWriteFlag = Bool(INPUT)
    // Don't save bad value (pipeline stall between stages)
    val discardCalcWrite = Bool(INPUT)
    
  }
  
  val currRadL = Count(null,WFTA.getValidRad.max)
  currRadL := io.currRad
  val rad7WE = currRadL(2) & currRadL(1) & currRadL(0)
  // Flags indicating active banks (on per butterfly basis) i.e. when radix-3, only 0-2 should be active
  val activeBanksPR = Vec.fill(maxRad){Bool()}
  activeBanksPR(0) := Bool(true)
  activeBanksPR(1) := Bool(true)
  if (maxRad > 2) activeBanksPR(2) := (currRadL(0) & currRadL(1)) | currRadL(2)
  if (maxRad > 3) activeBanksPR(3) := currRadL(2)
  if (maxRad > 4) activeBanksPR(4) := currRadL(2) & (currRadL(1) | currRadL(0))
  if (maxRad > 5) activeBanksPR(5) := rad7WE
  if (maxRad > 6) activeBanksPR(6) := rad7WE
  // Distribute active banks for all butterflies 
  val activeBanks = Vec((0 until numBanks).map( x => activeBanksPR(x%maxRad) ))
  debug(activeBanks)
  
  // X corresponds to butterfly(n) input #, Y corresponds to used bank #
  val calcBankXeqY = Vec.fill(numBanks){Vec.fill(numBanks){Bool()}}
  for (i <- 0 until numBanks; j <- 0 until numBanks){
    calcBankXeqY(i)(j) := (io.calcBank(i) === Count(j,bankMax))
  }
  val ioBankeqY = Vec((0 until numBanks).map( j => { val b = Bool(); b := (io.ioBank === Count(j,bankMax)); b }))
  
  // If butterfly 1 1st input X,i = 0 is from 3rd bank Y,j = 2, use 1st input address (addr0) to address bank 2
  // If butterfly 1 2nd input X,i = 1 is from 3rd bank Y,j = 2, use 2nd input address (addr1) to address bank 2, etc.
  // R = A (0) or B (1) memory used; S = bank #
  val addrCalcBankS = Vec.fill(numBanks){Count(null,addrMax)}
  val addrCalcBankSL = Vec.fill(numBanks){Vec.fill(numBanks){Count(null,addrMax)}}
  // Write enables for each bank
  val weCalcBankS = Vec.fill(numBanks){Bool()}
  val weCalcBankSL = Vec.fill(numBanks){Vec.fill(numBanks){Bool()}}
  // Match address to bank where the ORing of all select conditions -> corresponding calculation memory write enable
  // Ex: bank3_addr = [BF0addr [if [BF0 -> bank3] & [input0_active]]] 
  //                | [BF1addr [if [BF1 -> bank3] & [input1_active]]] 
  //                | [BF2addr [if [BF2 -> bank3] & [input2_active]]]
  for (i <- 0 until numBanks; j <- 0 until numBanks){
    val selCond = (calcBankXeqY(j)(i) & activeBanks(j))
    val selV = selU(io.calcAddr(j),selCond)
    if (j == 0){
      weCalcBankSL(i)(j) := selCond
      addrCalcBankSL(i)(j) := selV
    }
    else{
      weCalcBankSL(i)(j) :=  selCond | weCalcBankSL(i)(j-1)
      addrCalcBankSL(i)(j) := selV | addrCalcBankSL(i)(j-1) 
    }
    if (j == numBanks-1){
      weCalcBankS(i) := weCalcBankSL(i)(j)  & ~io.calcDoneFlag & ~io.discardCalcWrite
      addrCalcBankS(i) := addrCalcBankSL(i)(j)
    }
  }
   
  // Write enable for IO memory
  val weIObank = Vec((0 until numBanks).map( x => { val b = Bool(); b := ioBankeqY(x) & io.ioWriteFlag; b }))
  
  /////////////////////////////////////////////////////
  // ABOVE THIS, THERE IS NO DELAY FROM MODULE INPUT //
  /////////////////////////////////////////////////////
  
  // Timing diagram for butterfly calculation when sequential read enabled (no butterfly pipeline)
  // t = n     | t = n+1                         | t = n+2
  // RA0       |                                 |
  //           | D0 -> BF                        |
  //           | WA0                             |
  //           |             (BF->mem D0 valid)  | Valid D0 in mem
  
  // Timing diagram for IO
  // t = n        | t = n+1
  // RA0          |
  //              | RD0 -> FFT OUT
  // WA0          | 
  // FFT IN -> D0 |
  //              | Valid D0 in mem


  // Memories
  val mems = Vec((0 until 2).map( a => {Vec((0 until numBanks).map( b =>
  {val m = DSPModule(new Memory(Complex(gen),depth = memLengths(b), seqRead = true, outReg = false), nameExt = +a+"_"+b)
    m.io}   ))}))
  // TODO: assign properly
  val seqRdDly = 1 //mems(0)(0).delay



  // Note when you pipeline, during calculation* stage transitions [NOT IO], it's possible to have R/W conflicts
  // where you're reading a stale value as it's in the process of being written to (it's also possible to read
  // before valid value has been written due to the fact that processing is pipelined -- not addressed here)
  // i.e. at t = n, Stage 0 current bank Address 0 should be written to w/ data (properly stored in memory sequentially @ t = n+1)
  // BUT at t = n, Stage 1 current bank Address 0 should be read (current stage expects previous stage values to be valid),
  // so @ t = n+1 (sequential read), the previous [stale] data @ address 0 is read out instead of the data that has now just been saved.
  // t = n                                        | t = n+1
  // Write Address 0 + Write Enable               | Write Address x + WE
  //                                              | Valid Data saved in memory @ A0 = x0
  // Read Address 0                               | Read Address x
  // Stale A0 Data (combinational) from memory    | Data x
  // Data y0                                      | Stale A0 Data output via io.x (sequential read)






  // Therefore, in sequential read mode, IO rAddr, wAddr, we should be sent to memory modules at the same time
  // Calculation rAddr should be at time t = n; wAddr + we should be at time t = n+m+1 (where m is from butterfly pipelining)
  val weCalcBank = Vec.fill(numBanks){Bool()}; weCalcBank := Pipe(weCalcBankS,pipeBFWriteDly+seqRdDly).asInstanceOf[Vec[Bool]]
  val wAddrCalc = Vec.fill(numBanks){Count(null,addrMax)}; wAddrCalc := Pipe(addrCalcBankS,pipeBFWriteDly+seqRdDly).asInstanceOf[Vec[UInt]]
  val rAddrCalc = addrCalcBankS
  val calcMemBD = Pipe(io.calcMemB,toMemAddrDly).asInstanceOf[Bool]
  
  // Match calculation/IO addressing + WE to correct set of memories (A or B) + banks
  val weMemRBankS = Vec.fill(2){Vec.fill(numBanks){Bool()}}
  val wAddrMemRBankS = Vec.fill(2){Vec.fill(numBanks){Count(null,addrMax)}}
  val rAddrMemRBankS = Vec.fill(2){Vec.fill(numBanks){Count(null,addrMax)}}
  val calcPh = Vec.fill(2){Vec.fill(numBanks){Bool()}}
  for (i <- 0 until numBanks){
    rAddrMemRBankS(B)(i) := muxU(io.ioAddr,rAddrCalc(i),io.calcMemB) 
    rAddrMemRBankS(A)(i) := muxU(rAddrCalc(i),io.ioAddr,io.calcMemB)
    wAddrMemRBankS(B)(i) := muxU(io.ioAddr,wAddrCalc(i),io.calcMemB) 
    wAddrMemRBankS(A)(i) := muxU(wAddrCalc(i),io.ioAddr,io.calcMemB)
    weMemRBankS(B)(i) := muxU(weIObank(i),weCalcBank(i),io.calcMemB)
    weMemRBankS(A)(i) := muxU(weCalcBank(i),weIObank(i),io.calcMemB)
    calcPh(B)(i) := calcMemBD
    calcPh(A)(i) := ~calcMemBD
  }
  
  val wAddr = Pipe(wAddrMemRBankS,toMemAddrDly).asInstanceOf[Vec[Vec[UInt]]]
  val rAddr = Pipe(rAddrMemRBankS,toMemAddrDly).asInstanceOf[Vec[Vec[UInt]]]
  val we = Pipe(weMemRBankS,toMemAddrDly).asInstanceOf[Vec[Vec[Bool]]]


  
  // All Memory module IO delayed accordingly wrt address
  for (i <- 0 until 2; j <- 0 until numBanks){
    mems(i)(j).rAddr := DSPUInt(rAddr(i)(j),mems(i)(j).rAddr.getRange.max)
    mems(i)(j).wAddr := DSPUInt(wAddr(i)(j),mems(i)(j).wAddr.getRange.max)
    mems(i)(j).WE := DSPBool(we(i)(j))
    // TODO: Conflict handling
    mems(i)(j).passThrough.get := DSPBool(calcPh(i)(j))
  }
  
  // Data FROM memory comes out 1 cycle after address is valid when sequential read is true
  val calcMemBD_READ = Pipe(calcMemBD,seqRdDly).asInstanceOf[Bool]
  
  // Select from appropriate A/B memory; n indicates which bank # is read 
  val XCn = Vec.fill(numBanks){Complex(gen,gen)}
  for (i <- 0 until numBanks){
    XCn(i) := Mux(DSPBool(calcMemBD_READ),mems(B)(i).dOut,mems(A)(i).dOut)
  }
  
  // Align control signal with delayed address. NOTE if sequential read is enabled, data out from memory
  // comes one cycle after address is valid
  val calcBankXeqYD_READ =  Pipe(calcBankXeqY,toMemAddrDly+seqRdDly).asInstanceOf[Vec[Vec[Bool]]]
  val ioBankeqYD_READ =  Pipe(ioBankeqY,toMemAddrDly+seqRdDly).asInstanceOf[Vec[Bool]]
  
  // Input X,i to butterfly comes from which bank Y,j : BFX_val = bankY_out if {BFX comes from bankY}
  val xtemp = Vec.fill(numBanks){Vec.fill(numBanks){Complex(gen,gen)}}
  for (i <- 0 until numBanks; j <- 0 until numBanks){
    val selX = XCn(j) ? DSPBool(calcBankXeqYD_READ(i)(j))
    if (j == 0) xtemp(i)(j) := selX
    else xtemp(i)(j) := selX /| xtemp(i)(j-1)
    if (j == numBanks-1) io.x(i) := xtemp(i)(j)
  }

  // FFT IO Output from memory - comes from which bank Y,j
  val XIOn = Vec.fill(numBanks){Complex(gen,gen)}
  for (i <- 0 until numBanks){
    XIOn(i) := Mux(DSPBool(calcMemBD_READ),mems(A)(i).dOut,mems(B)(i).dOut)
  }
  val DoutTemp = Vec.fill(numBanks){Complex(gen,gen)}
  for (j <- 0 until numBanks){
    val selV = XIOn(j) ? DSPBool(ioBankeqYD_READ(j))
    if (j == 0) DoutTemp(j) := selV
    else DoutTemp(j) := selV /| DoutTemp(j-1)
  }
  io.Dout := DoutTemp(numBanks-1)

  // Data from Butterfly valid n cycles (butterfly pipeline delay) after data output to butterfly 
  // (should come in when calculation write address is ready)
  val calcBankXeqYD_WRITE = Pipe(calcBankXeqYD_READ,pipeBFWriteDly).asInstanceOf[Vec[Vec[Bool]]]
  val activeBanksD_WRITE = Pipe(activeBanks,toMemAddrDly+seqRdDly+pipeBFWriteDly).asInstanceOf[Vec[Bool]] 

  // Saves output of butterfly to input location (in-place) - logic for routing same as address to bank (different from X -> BF) 
  // Note X mapping is unique (only 1 bank associated w/ each butterfly input) whereas Y mapping is not unique 
  // Data to memory valid when address is valid
  val ytemp = Vec.fill(numBanks){Vec.fill(numBanks){Complex(gen,gen)}}
  val Yn = Vec.fill(numBanks){Complex(gen,gen)}
  for (i <- 0 until numBanks; j <- 0 until numBanks){
    val selY = io.y(j) ? DSPBool(calcBankXeqYD_WRITE(j)(i) & activeBanksD_WRITE(j))
    if (j == 0) ytemp(i)(j) := selY
    else ytemp(i)(j) := selY /| ytemp(i)(j-1)
    if (j == numBanks-1) Yn(i) := ytemp(i)(j)
  }
  
  // Input to memory either from butterfly or from FFT IO (input valid when address is valid)
  // This assumes there are enough cycles between end of calculation and beginning of new IO to complete all memory saves
  // Note for calculation, y is already delayed through the butterfly, sequential read register
  for (i <- 0 until numBanks){
    mems(B)(i).dIn := Mux(DSPBool(calcMemBD), Yn(i),io.Din)
    mems(A)(i).dIn := Mux(DSPBool(calcMemBD), io.Din,Yn(i))
  }

  CheckDelay.on()

}

