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
import breeze.math.Complex
import dsptools.numbers._
import dsptools.numbers.implicits._

class ComplexLUTSpec extends FlatSpec with Matchers {
  behavior of "Complex LUT"
  it should "store the right Lit values and output them in the correct order" in {
    
    val lut = PeelingScheduling.getFFTParams(675).twiddle.twiddles.map { 
      // First lane of first coprime
      case (coprime, luts) => luts.map(row => row.head) 
    }.toSeq.head    

    dsptools.Driver.execute(() => 
      new ComplexLUTWrapper(
        //DspReal(),
        FixedPoint(16.W, 14.BP),
        "TwiddleLUT", 
        lut
      ), TestParams.options1TolWaveform) { c =>
      new ComplexLUTTester(c)
    } should be (true)
  }
}

class ComplexLUTTester[T <: Data:RealBits](c: ComplexLUTWrapper[T]) extends DspTester(c) {
  c.tableIn.zipWithIndex foreach { case (row, idx) =>
    poke(c.io.addr, idx)
    expect(c.io.dout, row)
    step(1)
  }
}

class ComplexLUTIO[T <: Data:RealBits](dspDataType: => T, depth: Int) extends Bundle {
  val clk = Input(Clock())
  val addr = Input(UInt(range"[0, $depth)"))
  val dout = Output(DspComplex(dspDataType))

  override def cloneType = (new ComplexLUTIO(dspDataType, depth)).asInstanceOf[this.type]
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
      case b: Bits => 
        require(b.widthKnown, "Width must be defined!")
        dspDataType.getWidth
    }
  )
  val rowWidth = colBits.sum
  val colStart = colBits.scanRight(0)((width, accum) => accum + width)
  val colBitShift = colStart.tail

  // Inclusive bit extraction (highest, lowest)
  val colRange = colStart.init.map(x => x - 1).zip(colBitShift)

  val io = IO(new ComplexLUTIO(dspDataType, depth))

  withClock(io.clk) {

    // TODO: Simplify compared to LUT 2D case
    // Concatenate data
    val lutRows = table map { case rowComplex => 
      val row = Seq(rowComplex.real, rowComplex.imag)
      val rowWithColBitShift = row.zip(colBitShift)
      rowWithColBitShift.foldRight(BigInt(0)) { case ((append, shift), result) =>

        val appendBits = dspDataType match {
          case r: DspReal =>
            DspTesterUtilities.doubleToBigIntBits(append)
          case f: FixedPoint =>
            require(f.binaryPoint.known, "Binary point must be known!")
            // TODO: Turn into function
            FixedPoint.toBigInt(append, f.binaryPoint.get) & BigInt((1 << f.getWidth) - 1)
          // TODO: Support UInt, SInt
          case _ => throw new Exception("Currently, only FixedPoint and DspReal are supported")
        }
        result + (appendBits << shift)

      }
    }

    val bb = Module(new LUTBlackBox(blackBoxName, lutRows, widthOverride = Some(rowWidth)))
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
          bb.io.dout(high, low).asSInt.asFixedPoint(f.binaryPoint)
      }
      out := ShiftRegister(interpretOutput, moduleDelay)
    }
  }
}