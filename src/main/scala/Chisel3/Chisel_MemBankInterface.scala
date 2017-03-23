package dspblocks.fft
import chisel3._
import chisel3.util.Mux1H
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
  val loc = Input(new BankAddressBundle(Seq(maxNumBanks - 1, maxDepth - 1)))
  override def cloneType = (new MemInputLane(dataType, maxNumBanks, maxDepth)).asInstanceOf[this.type]
}

class MemOutputLane[T <: Data:Ring](dataType: => T, maxNumBanks: Int, maxDepth: Int) extends Bundle {
  val dout = Output(dataType)
  val re = Input(Bool())
  // Closed range: bank followed by addr
  val loc = Input(new BankAddressBundle(Seq(maxNumBanks - 1, maxDepth - 1)))
  override def cloneType = (new MemOutputLane(dataType, maxNumBanks, maxDepth)).asInstanceOf[this.type] 
}

class MemBankInterfaceIO[T <: Data:Ring](dataType: T, maxNumBanks: Int, maxDepth: Int) extends Bundle {
  val clk = Input(Clock())
  val i = Vec(maxNumBanks, new MemInputLane(dataType, maxNumBanks, maxDepth))
  val o = Vec(maxNumBanks, new MemOutputLane(dataType, maxNumBanks, maxDepth))
  override def cloneType = (new MemBankInterfaceIO(dataType, maxNumBanks, maxDepth)).asInstanceOf[this.type]
}

@chiselName
class MemBankInterface[T <: Data:Ring](dataType: T, bankLengths: Seq[Int]) extends Module with DelayTracking {
  // Read data valid 1 clk cycle after read address
  // Have as many input ports as # of banks -- that then gets sorted
  val moduleDelay = 1
  val io = IO(new MemBankInterfaceIO(dataType, maxNumBanks = bankLengths.length, maxDepth = bankLengths.max))
  val memBanks = Module(new MemBanks(dataType, bankLengths))
  withClock(io.clk) {



    
    val writeBankSel = Wire(CustomIndexedBundle(Seq.fill(memBanks.io.bank.elements.toSeq.length)(
      CustomIndexedBundle(Seq.fill(io.i.length)(Bool()))
    )))


    /*val zero = Wire(dataType)//DspComplex.wire(Ring[T].zero, Ring[T].zero) 
    zero.real := Ring[T].zero
    zero.imag := Ring[T].zero*/

/*
    val writeBankSel = memBanks.io.bank.elements.map { case (bankIdx, _) =>
      bankIdx.toInt -> io.i.map { case lane => lane.loc.bank === bankIdx.toInt.U }
    }.toMap
*/

    memBanks.io.bank.elements foreach { case (bankIdx, _) =>
      io.i.zipWithIndex foreach { case (lane, laneIdx) => writeBankSel(bankIdx.toInt)(laneIdx) := (lane.loc.bank === bankIdx.toInt.U) }
    }

    val readBankSel = memBanks.io.bank.elements.map { case (bankIdx, _) =>
      bankIdx.toInt -> io.o.map { case lane => lane.loc.bank === bankIdx.toInt.U }
    }.toMap

    memBanks.io.bank.elements foreach { case (bankIdxS, bankIo) =>

      val bankIdx = bankIdxS.toInt

      /*bankIo.din := Mux1H(Seq(
        writeBankSel(bankIdx)(0) -> io.i(0).din//, //right
        //writeBankSel(bankIdx)(1) -> io.i(1).din
      ))*///io.i(0).din


      /*bankIo.din := Mux1H(io.i.zipWithIndex.map { case (lane, laneIdx) => 
        (writeBankSel(bankIdx)(laneIdx), lane.din)
      })*/


      //println("xxx" + bankIdx)
      //bankIo.din := Mux(writeBankSel(bankIdx)(1), io.i(1).din, Mux(writeBankSel(bankIdx)(0), io.i(0).din, zero))



      bankIo.din := Mux1H(Seq(
        writeBankSel(bankIdx)(0) -> io.i(0).din,
        writeBankSel(bankIdx)(1) -> io.i(1).din
      ))




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