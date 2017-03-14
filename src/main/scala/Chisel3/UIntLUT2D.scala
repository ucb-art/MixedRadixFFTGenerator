package barstools.modules
import chisel3._
import chisel3.util.ShiftRegister
import chisel3.experimental._
import barstools.tapeout.transforms._
import chisel3.util.HasBlackBoxInline
import org.scalatest.{FlatSpec, Matchers}
import barstools.tapeout.TestParams
import dsptools.DspTester

// TODO: Try 675, 800, 864 LUTs (bin to bank, addr); then check if the mod thing works

class UIntLUT2DSpec extends FlatSpec with Matchers {
  behavior of "2D UInt LUT"
  it should "store the right Lit values and output them in the correct order" in {
    val testLUT = Seq(
      Seq(1, 7, 3),
      Seq(34, 768, 2033),
      Seq(155, 680, 9)
    )
    dsptools.Driver.execute(() => new UIntLUT2D("TestLUT", testLUT), TestParams.options0Tol) { c =>
      new UIntLUT2DTester(c)
    } should be (true)
  }
}

class UIntLUT2DTester(c: UIntLUT2D) extends DspTester(c) {
  poke(c.io.addr, 0)
  val out = peek(c.io.dout).map(x => x.intValue).toSeq
  println(out.mkString(","))
}

class UIntLUT2DIO(depth: Int, colMax: Seq[Int], colNames: Seq[String]) extends Bundle {
  require(colNames.isEmpty || colNames.length == colMax.length, 
    "Column names should either all be specified or none specified")
  val doutDataTypes = colMax map (n => UInt(range"[0, $n]")) 

  val addr = Input(UInt(range"[0, $depth)"))
  val dout = Output(
    if (colNames.isEmpty) 
      new CustomBundle(doutDataTypes.zipWithIndex.map { case (tpe, idx) => idx.toString -> tpe } : _*)
    else 
      new CustomBundle(colNames.zip(doutDataTypes): _*)
  )

  override def cloneType = (new UIntLUT2DIO(depth, colMax, colNames)).asInstanceOf[this.type]
}

class UIntLUT2D(blackBoxName: String, tableIn: Seq[Seq[Int]], colNames: Seq[String] = Seq(), outputReg: Boolean = false) 
    extends Module {

  // Handle edge case where LUT is empty (just attach to 0)    
  val table = if (tableIn.isEmpty) Seq(Seq.fill(colNames.length.max(1))(0)) else tableIn
  require(table.flatten.find(_ < 0) == None, "LUT values must be >= 0")

  val depth = table.length
  val colMax = table.transpose.map(col => col.max)
  // At least want 1 bit, even for 0
  val colBits = colMax.map(x => BigInt(x).bitLength.max(1))
  val rowWidth = colBits.sum
  val colStart = colBits.reverse.scanLeft(0)((accum, width) => accum + width).reverse
  val colBitShift = colStart.tail
  // Inclusive bit extraction (highest, lowest)
  val colRange = colStart.init.map(x => x - 1).zip(colBitShift)

  val io = IO(new UIntLUT2DIO(depth, colMax, colNames))

  // Concatenate data
  val lutRows = table map { case row => 
    val rowWithColBitShift = row.zip(colBitShift)
    rowWithColBitShift.foldRight(0) { case ((append, shift), result) => 
      result + (append << shift)
    }
  }

  val bb = Module(new LUTBlackBox(blackBoxName, lutRows))
  bb.io.addr := io.addr

  val keys = if (colNames.nonEmpty) colNames else (0 until colMax.length).map(_.toString)

  keys.zipWithIndex foreach { case (key, idx) =>
    val (high, low) = colRange(idx)
    io.dout(key) := ShiftRegister(bb.io.dout(high, low), if (outputReg) 1 else 0)
  }

}

class BlackBoxLUTIO(addressWidth: Int, dataWidth: Int) extends Bundle {
  val addr = Input(UInt(addressWidth.W))
  val dout = Output(UInt(dataWidth.W))
}

class LUTBlackBox(blackBoxName: String, table: Seq[Int]) extends HasBlackBoxInline {

  require(blackBoxName != "", "LUT name must be provided!")

  val dataWidth = BigInt(table.head).bitLength
  val addrWidth = BigInt(table.length).bitLength

  // WARNING: No uniqueness check (user has to guarantee!!)
  override def desiredName = blackBoxName

  val io = IO(new BlackBoxLUTIO(addressWidth = addrWidth, dataWidth = dataWidth))

  // Dumps in hex
  val tableString = table.zipWithIndex.map { 
    case (data,idx) => s"    $idx: dout = $dataWidth'h${BigInt(data).toString(16)};" }.mkString("\n")

  val verilog = s"""
    |module $blackBoxName(
    |  input [${addrWidth - 1}:0] addr,
    |  output reg [${dataWidth - 1}:0] dout
    |);
    |  always @(*) case (addr)
    |$tableString
    |    default: dout = $dataWidth'h${BigInt(dataWidth, new scala.util.Random()).toString(16)};
    |  endcase
    |endmodule  
    """.stripMargin

  setInline(s"$blackBoxName.v", verilog)
}