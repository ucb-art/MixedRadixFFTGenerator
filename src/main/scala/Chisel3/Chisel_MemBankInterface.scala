package dspblocks.fft
import chisel3._
import dsptools.numbers._
import dsptools.numbers.implicits._
import chisel3.experimental._
import barstools.tapeout.transforms._

// Crossbar + Membanks

class MemInputLane[T <: Data:Ring](dataType: => T, maxNumBanks: Int, maxDepth: Int) extends Bundle {
  val din = Input(dataType)
  val we = Input(Bool())
  // Closed range: bank followed by addr
  // TODO: Make semantics consistent -- all use [)
  val loc = Flipped(new BankAddressBundle(Seq(maxNumBanks - 1, maxDepth - 1)))
  override def cloneType = (new MemInputLane(dataType, maxNumBanks, maxDepth)).asInstanceOf[this.type]
}

class MemOutputLane[T <: Data:Ring](dataType: => T, maxNumBanks: Int, maxDepth: Int) extends Bundle {
  val dout = Output(dataType)
  val re = Input(Bool())
  // Closed range: bank followed by addr
  val loc = Flipped(new BankAddressBundle(Seq(maxNumBanks - 1, maxDepth - 1)))
  override def cloneType = (new MemOutputLane(dataType, maxNumBanks, maxDepth)).asInstanceOf[this.type] 
}

class MemBankInterfaceIO[T <: Data:Ring](dataType: T, maxNumBanks: Int, maxDepth: Int) extends Bundle {
  val clk = Input(Clock())
  val i = Vec(maxNumBanks, new MemInputLane(dataType, maxNumBanks, maxDepth))
  val o = Vec(maxNumBanks, new MemOutputLane(dataType, maxNumBanks, maxDepth))
  override def cloneType = (new MemBankInterfaceIO(dataType, maxNumBanks, maxDepth)).asInstanceOf[this.type]
}

@chiselName
class MemBankInterface[T <: Data:Ring](dataType: T, bankLengths: Seq[Int], name: String = "") extends Module with DelayTracking {
  // Read data valid 1 clk cycle after read address
  // Have as many input ports as # of banks -- that then gets sorted
  // TODO: Should get delay from sub-modules
  val moduleDelay = 1
  val io = IO(new MemBankInterfaceIO(dataType, maxNumBanks = bankLengths.length, maxDepth = bankLengths.max))
  val memBanks = Module(new MemBanks(dataType, bankLengths, name = name))
  withClock(io.clk) {

    // Wire to give a better name :\
    val writeBankSel = Wire(CustomIndexedBundle(
      // TODO: Make length helper in CustomIndexedBundle
      Seq.fill(memBanks.io.bank.elements.toSeq.length)(
        CustomIndexedBundle(Seq.fill(io.i.length)(Bool()))
    )))
    // Same length
    val readBankSel = Wire(writeBankSel.cloneType)
    /*
    val writeBankSel = memBanks.io.bank.elements.map { case (bankIdx, _) =>
      bankIdx.toInt -> io.i.map { case lane => lane.loc.bank === bankIdx.toInt.U }
    }.toMap
    val readBankSel = memBanks.io.bank.elements.map { case (bankIdx, _) =>
      bankIdx.toInt -> io.o.map { case lane => lane.loc.bank === bankIdx.toInt.U }
    }.toMap
    */

    memBanks.io.bank.elements foreach { case (bankIdxS, bankIo) =>

      val bankIdx = bankIdxS.toInt

      io.i.zipWithIndex foreach { case (lane, laneIdx) => 
        writeBankSel(bankIdx)(laneIdx) := (lane.loc.bank === bankIdx.U) 
      }
      io.o.zipWithIndex foreach { case (lane, laneIdx) => 
        readBankSel(bankIdx)(laneIdx) := (lane.loc.bank === bankIdx.U) 
      }
      bankIo.din := Mux1H(io.i.zipWithIndex.map { case (lane, laneIdx) => 
        (writeBankSel(bankIdx)(laneIdx), lane.din)
      })
      bankIo.we := Mux1H(io.i.zipWithIndex.map { case (lane, laneIdx) => 
        (writeBankSel(bankIdx)(laneIdx), lane.we)
      })
      bankIo.waddr := Mux1H(io.i.zipWithIndex.map { case (lane, laneIdx) => 
        (writeBankSel(bankIdx)(laneIdx), lane.loc.addr)
      })
      bankIo.raddr := Mux1H(io.o.zipWithIndex.map { case (lane, laneIdx) => 
        (readBankSel(bankIdx)(laneIdx), lane.loc.addr)
      })
      bankIo.re := Mux1H(io.o.zipWithIndex.map { case (lane, laneIdx) => 
        (readBankSel(bankIdx)(laneIdx), lane.re)
      })
      bankIo.clk := io.clk
      
    }

    io.o.zipWithIndex foreach { case (lane, laneIdx) =>
      lane.dout := Mux1H(memBanks.io.bank.elements.map { case (bankIdx, bankIo) => 
        // Read delay happens one cycle after address valid -- need to delay match
        (RegNext(readBankSel(bankIdx.toInt)(laneIdx)), bankIo.dout)
      })
    }
  }
}