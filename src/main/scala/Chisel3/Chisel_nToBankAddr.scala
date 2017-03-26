package dspblocks.fft
import chisel3._
import chisel3.util._
import barstools.tapeout.transforms._
import chisel3.experimental._

class NToBankAddrIO(fftParams: FactorizationParams) extends Bundle {
  // TODO: Support runtime reconfiguration
  require(fftParams.calc.getStages.length == 1, "Only support 1 FFT at a time now (non reconfigurable)")
  val stages = fftParams.calc.getStages.head.stages
  val maxDepth = fftParams.mem.bankLengths.max
  val maxNumBanks = fftParams.mem.maxNumBanks
  
  val n = CustomIndexedBundle(stages.map(r => Input(UInt(range"[0, $r)"))))
  val loc = new BankAddressBundle(Seq(maxNumBanks - 1, maxDepth - 1))
  val clk = Input(Clock())

  override def cloneType = (new NToBankAddrIO(fftParams)).asInstanceOf[this.type]
}

class NToBankAddr(fftParams: FactorizationParams) extends Module with DelayTracking {
  // TODO: Don't hard code
  def moduleDelay = 1
  val maxNumBanks = fftParams.mem.maxNumBanks
  val maxAddress = fftParams.mem.bankLengths.max - 1
  // TODO: Rename
  val maxNumStages = fftParams.calc.maxStages
  // TODO: Generalize
  require(fftParams.mem.addressConstants.length == 1, "Only support 1 FFT at a time now (non reconfigurable)")
  val addressConstants = fftParams.mem.addressConstants.head

  val io = IO(new NToBankAddrIO(fftParams))

  val ion = io.n.elements.map { case (name, element) => element }.toSeq

  withClock(io.clk) {
    // Tree operations -- calculate depth
    // TODO: Use something besides log?
    val nestDepth = math.ceil(math.log(maxNumStages)/math.log(2)).toInt
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

    // TODO: Figure out how to not pre-allocate
    val addressProducts = addressConstants.zip(ion) map { case (ac, n) => 
      val out = Wire(UInt(range"[0, $maxAddress]"))
      out := (ac.U * n)
      ShiftRegister(out, moduleDelay)
    }
    // TODO: Figure out how to not pre-allocate
    def groupAndSum(in: Seq[UInt]): Seq[UInt] = {
      in.grouped(2).map { case e => 
        val out = Wire(UInt(range"[0, $maxAddress]"))
        out := e.reduceLeft(_ +& _) 
        out
      }.toSeq
    }

    // TODO: Something better than foldLeft? Since index is kind of wasted
    // As you go deeper, the # of elements in the rows decreases
    val addrTree = 
      if (nestDepth == 1) groupAndSum(addressProducts)
      else {
        (0 until nestDepth - 1).foldLeft(groupAndSum(addressProducts)) { case (accum, _) =>
          // Deal with previous row
          groupAndSum(accum)
        }
      }

    require(addrTree.length == 1, "Nest depth calculation was incorrect! Deepest layer should only have 1 element!")
    io.loc.addr := addrTree.head

    // TODO: Use more of these mapping functions
    // TODO: Can I use reduceLeft?
    // TODO: Switch to using max radix instead of max number of banks
    val addModFunction = (x: UInt, y: UInt) => ShortConstantMod(x +& y, maxNumBanks, 2 * maxNumBanks - 2)
    def groupAndModSum(in: Seq[UInt]): Seq[UInt] = {
      in.grouped(2).map(e => e.reduceLeft(addModFunction)).toSeq
    }

    val bankTree = 
      if (nestDepth == 1) groupAndModSum(ion)
      else {
        (0 until nestDepth - 1).foldLeft(groupAndModSum(ion)) { case (accum, _) => 
          groupAndModSum(accum)
        }
      }

    io.loc.bank := ShiftRegister(bankTree.head, moduleDelay)

    // TODO: Support parallel butterflies
  }
}