package barstools.modules
import chisel3._
import chisel3.util.ShiftRegister
import chisel3.experimental._
import barstools.tapeout.transforms._
import chisel3.util.HasBlackBoxInline
import org.scalatest.{FlatSpec, Matchers}
import barstools.tapeout.TestParams
import dsptools.{DspTester, DspTesterOptionsManager}
import dspblocks.fft._
import dsptools._

class UIntLUT2DSpec extends FlatSpec with Matchers {
  behavior of "2D UInt LUT"
  it should "store the right Lit values and output them in the correct order" in {
    val testLUT = Seq(
      Seq(1, 7, 3),
      Seq(34, 768, 2033),
      Seq(155, 680, 9)
    )
    val opt = new DspTesterOptionsManager {
      dspTesterOptions = TestParams.options0Tol.dspTesterOptions
      testerOptions = TestParams.options0Tol.testerOptions
      commonOptions = TestParams.options0Tol.commonOptions.copy(targetDirName = s"test_run_dir/UIntLUT2D")
    }
    dsptools.Driver.execute(() => new UIntLUT2DWrapper("TestLUT", testLUT), opt) { c =>
      new UIntLUT2DTester(c)
    } should be (true)
  }
  it should "get bank, address for FFT bins" in {
    val fftns = Seq(675, 800, 864)
    fftns foreach { n => 
      val lutName = s"DITBankAddr$n"
      val opt = new DspTesterOptionsManager {
        dspTesterOptions = TestParams.options0Tol.dspTesterOptions
        testerOptions = TestParams.options0Tol.testerOptions
        commonOptions = TestParams.options0Tol.commonOptions.copy(targetDirName = s"test_run_dir/$lutName")
      }
      val lut = dspblocks.fft.PeelingScheduling.getIOMemBankAddr(n, dspblocks.fft.DIT).map(x => x.getBankAddr)
      dsptools.Driver.execute(() => new UIntLUT2DWrapper(lutName, lut), opt) { c =>
        new UIntLUT2DTester(c)
      } should be (true)
    }
  }
}

class UIntLUT2DTester(c: UIntLUT2DWrapper) extends DspTester(c) {
  c.tableIn.zipWithIndex foreach { case (row, idx) =>
    poke(c.io.addr, idx)
    val peekedVals = c.io.dout.elements.map { case (key, value) => peek(value) }
    expect(peekedVals == row, s"Peeked LUT value must match for ${c.blackBoxName}")
  }
}











class ComplexLUTIO[T <: Data:RealBits](dspDataType: => T, depth: Int) extends Bundle {
  val clk = Input(Clock())
  val addr = Input(UInt(range"[0, $depth)"))
  val dout = Output(DspComplex(dspDataType))

  override def cloneType = (new ComplexLUTDIO(dspDataType)).asInstanceOf[this.type]
}

class ComplexLUTWrapper[T <: Data:RealBits](dspDataType: => T, val blackBoxName: String, val tableIn: Seq[Complex], outputReg: Boolean = false)  
    extends Module {
  val mod = Module(new ComplexLUT(dspDataType, blackBoxName, tableIn, outputReg))
  val io = IO(mod.io.cloneType)
  mod.io.clk := clock
  mod.io.addr := io.addr
  io.dout := mod.io.dout
}

class ComplexLUT[T <: Data:RealBits](dspDataType: => T, val blackBoxName: String, val tableIn: Seq[Complex], outputReg: Boolean = false) 
    extends Module with DelayTracking {

  def moduleDelay = if (outputReg) 1 else 0

  // Handle edge case where LUT is empty (just attach to 0)    
  val table = if (tableIn.isEmpty) Seq(Complex(0.0, 0.0)) else tableIn
  val depth = table.length

  // TODO: Elsewhere you want to check for widthKnown? 

  val colBits = Seq.fill(2)(
    dspDataType match {
      case r: DspReal => 
        r.node.getWidth
      case _ => 
        require(dspDataType.widthKnown, "Width must be defined!")
        dspDataType.getWidth
    }
  )
  val rowWidth = colBits.sum
  val colStart = colBits.scanRight(0)((width, accum) => accum + width)
  val colBitShift = colStart.tail

  // Inclusive bit extraction (highest, lowest)
  val colRange = colStart.init.map(x => x - 1).zip(colBitShift)

  val io = IO(new ComplexLUTIO(depth))

  withClock(io.clk) {

    // TODO: Simplify compared to LUT 2D case
    // Concatenate data
    val lutRows = table map { case rowComplex => 
      val row = Seq(rowComplex.real, rowComplex.imag)
      val rowWithColBitShift = row.zip(colBitShift)
      rowWithColBitShift.foldRight(0) { case ((append, shift), result) =>

        val appendBits = dspDataType match {
          case r: DspReal =>
            DspTesterUtilities.doubleToBigIntBits(append)
          case f: FixedPoint =>
            require(f.binaryPoint.known, "Binary point must be known!")
            DspTesterUtilities.toBigIntUnsigned(append, r.getWidth, r.binaryPoint.get)
          // TODO: Support UInt, SInt
          case _ => throw new Exception("Currently, only FixedPoint and DspReal are supported")
        }
        result + (appendBits << shift)

      }
    }

    val bb = Module(new LUTBlackBox(blackBoxName, lutRows))
    bb.io.addr := io.addr

    // TODO: Don't need to do it the LUT2D way
    Seq(io.dout.real, io.dout.imag).zipWithIndex foreach { case (out, idx) =>
      val (high, low) = colRange(idx)
      // TODO: Support SInt, UInt
      val interpretOutput = dspDataType match {
        case r: DspReal =>
          val out = Wire(DspReal())
          out.node := bb.io.dout(high, low)
          out
        case f: FixedPoint =>
          bb.io.dout(high, low).asFixedPoint(f.binaryPoint.get)
      }
      out := ShiftRegister(interpretOutput, moduleDelay)
    }
  }
}