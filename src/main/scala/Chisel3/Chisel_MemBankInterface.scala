package dspblocks.fft
import chisel3._
import chisel3.util.Mux1H
import dsptools.numbers._
import dsptools.numbers.implicits._
import chisel3.experimental._

// Crossbar + Membanks

class MemInputLane[T <: Data:Ring](dataType: => T, maxNumBanks: Int, maxDepth: Int) extends Bundle {
  val din = Input(dataType)
  val we = Input(Bool())
  // Closed range: bank followed by addr
  // TODO: Make semantics consistent -- all use [)
  val loc = Input(new BankAddressBundle(Seq(maxNumBanks - 1, maxDepth - 1)))
  override def cloneType = (new MemInputLane(dataType, maxNumBanks, maxDepth)).asInstanceOf[this.type]
}

class MemOutputLane[T <: Data:Ring](dataType: => T, maxNumBanks: Int, maxDepth: Int) extends Bundle {
  val dout = Output(dataType)
  // Closed range: bank followed by addr
  val loc = Input(new BankAddressBundle(Seq(maxNumBanks - 1, maxDepth - 1)))
  override def cloneType = (new MemOutputLane(dataType, maxNumBanks, maxDepth)).asInstanceOf[this.type] 
}

class MemBankInterfaceIO[T <: Data:Ring](dataType: T, maxNumBanks: Int, maxDepth: Int) extends Bundle {
  val clk = Input(Clock())
  val i = Input(Vec(maxNumBanks, new MemInputLane(dataType, maxNumBanks, maxDepth)))
  val o = Output(Vec(maxNumBanks, new MemOutputLane(dataType, maxNumBanks, maxDepth)))
  override def cloneType = (new MemBankInterfaceIO(dataType, maxNumBanks, maxDepth)).asInstanceOf[this.type]
}

class MemBankInterface[T <: Data:Ring](dataType: T, bankLengths: Seq[Int]) extends Module with DelayTracking {
  // Read data valid 1 clk cycle after read address
  // Have as many input ports as # of banks -- that then gets sorted
  val moduleDelay = 1
  val io = IO(new MemBankInterfaceIO(dataType, maxNumBanks = bankLengths.length, maxDepth = bankLengths.max))
  val memBanks = Module(new MemBanks(dataType, bankLengths))
  withClock(io.clk) {
    val writeBankSel = memBanks.io.bank.zipWithIndex map { case (_, bankIdx) =>
      io.i.map { case lane => lane.loc.bank === bankIdx.U }
    }

    val readBankSel = memBanks.io.bank.zipWithIndex map { case (_, bankIdx) =>
      io.o.map { case lane => lane.loc.bank === bankIdx.U }
    }

    memBanks.io.bank.zipWithIndex foreach { case (bankIo, bankIdx) =>
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
      bankIo.clk := io.clk
    }

    io.o.zipWithIndex foreach { case (lane, laneIdx) =>
      lane.dout := Mux1H(memBanks.io.bank.zipWithIndex.map { case (bankIo, bankIdx) => 
        // Read delay happens one cycle after address valid -- need to delay match
        (RegNext(readBankSel(bankIdx)(laneIdx)), bankIo.dout)
      })
    }
  }
}