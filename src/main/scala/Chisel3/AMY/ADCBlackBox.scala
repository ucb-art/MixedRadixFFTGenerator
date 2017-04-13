package dspblocks.fft

import chisel3._
import chisel3.experimental._
import org.scalatest.{FlatSpec, Matchers}
import dsptools.{DspTesterOptionsManager, DspTester}
import chisel3.util._
import barstools.tapeout.TestParams
import AmyParams._

object AmyParams {
  val memWidth = 128
  val scanWidth = 64
  val memDepth = 512

  val scrWidth = 64

  val scrSplit = memWidth / scrWidth
  require(memWidth % scrWidth == 0)
  require(scanWidth == scrWidth)
}

trait PeripheryAmyBundle {
  val RX_ADC_RST_ACTHIGH = Input(Bool())
  val RX_ADC_DIG_CLK = Input(Bool())
  val RX_ADC_SCAN_IN = Input(Bool())
  val RX_ADC_SCAN_EN = Input(Bool())
  val RX_ADC_MEM_WRITE_EN = Input(Bool())
  val RX_ADC_DIG_OUT = Output(Bool())
  val RX_ADC_VCO_P_OUT = Output(Bool())
  val RX_ADC_VCO_N_OUT = Output(Bool())

  // BUMPS ONLY
  val RX_ADC_RF_INP = Input(Bool())
  val RX_ADC_RF_INN = Input(Bool())
  val RX_ADC_CLK_10G_P = Input(Bool())
  val RX_ADC_CLK_10G_N = Input(Bool())
  val RX_ADC_VDDSAR = Input(Bool())
  val RX_ADC_VDDFIR = Input(Bool())
  val RX_ADC_VDDVCO = Input(Bool())
  
}

class RX_ADC_TOP_IO extends Bundle with PeripheryAmyBundle {
  // Need async FIFO    
  val rx_adc_mem_clk_out = Output(Clock())
  
  val rx_adc_scan_bank = Vec(15, Input(UInt(scanWidth.W)))

  val rx_adc0_out_sar = Output(UInt(memWidth.W))
  val rx_adc1_out_sar = Output(UInt(memWidth.W))
  val rx_adc2_out_sar = Output(UInt(memWidth.W))
  val rx_adc3_out_sar = Output(UInt(memWidth.W))

  val rx_adc0_out_vcop = Output(UInt(memWidth.W))
  val rx_adc1_out_vcop = Output(UInt(memWidth.W))
  val rx_adc2_out_vcop = Output(UInt(memWidth.W))
  val rx_adc3_out_vcop = Output(UInt(memWidth.W))

  val rx_adc0_out_vcon = Output(UInt(memWidth.W))
  val rx_adc1_out_vcon = Output(UInt(memWidth.W))
  val rx_adc2_out_vcon = Output(UInt(memWidth.W))
  val rx_adc3_out_vcon = Output(UInt(memWidth.W))

  val rx_adc02_out_lsb = Output(UInt(memWidth.W))
  val rx_adc13_out_lsb = Output(UInt(memWidth.W))
}

class AmyScr extends SCRBundle {
  val rx_adc_scan_bank = Vec(15, Input(UInt(scanWidth.W)))
  
  val rx_adc0_out_sar = Output(Vec(scrSplit, UInt(scrWidth.W)))
  val rx_adc1_out_sar = Output(Vec(scrSplit, UInt(scrWidth.W)))
  val rx_adc2_out_sar = Output(Vec(scrSplit, UInt(scrWidth.W)))
  val rx_adc3_out_sar = Output(Vec(scrSplit, UInt(scrWidth.W)))

  val rx_adc0_out_vcop = Output(Vec(scrSplit, UInt(scrWidth.W)))
  val rx_adc1_out_vcop = Output(Vec(scrSplit, UInt(scrWidth.W)))
  val rx_adc2_out_vcop = Output(Vec(scrSplit, UInt(scrWidth.W)))
  val rx_adc3_out_vcop = Output(Vec(scrSplit, UInt(scrWidth.W)))

  val rx_adc0_out_vcon = Output(Vec(scrSplit, UInt(scrWidth.W)))
  val rx_adc1_out_vcon = Output(Vec(scrSplit, UInt(scrWidth.W)))
  val rx_adc2_out_vcon = Output(Vec(scrSplit, UInt(scrWidth.W)))
  val rx_adc3_out_vcon = Output(Vec(scrSplit, UInt(scrWidth.W)))

  val rx_adc02_out_lsb = Output(Vec(scrSplit, UInt(scrWidth.W)))
  val rx_adc13_out_lsb = Output(Vec(scrSplit, UInt(scrWidth.W)))

  val rIdx = Input(UInt(range"[0, $memDepth)"))
  val re = Input(Bool())
}

class AmyAdcIo extends Bundle with PeripheryAmyBundle {
  val scr = new AmyScr
  val rx_adc_mem_clk_out = Output(Clock())
  // WARNING: scr inputs/outputs should already be synchronized to this clk
}

class RX_ADC_TOP(useBlackBox: Boolean) extends BlackBox {
  val io = IO(new RX_ADC_TOP_IO)
  // Simulation
  if (!useBlackBox)
    setInline("amy_RX_ADC_TOP.v",
"""

module RX_ADC_TOP(
    input RX_ADC_RST_ACTHIGH,
    input RX_ADC_DIG_CLK,
    input RX_ADC_SCAN_IN,
    input RX_ADC_SCAN_EN,
    input RX_ADC_MEM_WRITE_EN,
    input [63:0] rx_adc_scan_bank_0,
    input [63:0] rx_adc_scan_bank_1,
    input [63:0] rx_adc_scan_bank_2,
    input [63:0] rx_adc_scan_bank_3,
    input [63:0] rx_adc_scan_bank_4,
    input [63:0] rx_adc_scan_bank_5,
    input [63:0] rx_adc_scan_bank_6,
    input [63:0] rx_adc_scan_bank_7,
    input [63:0] rx_adc_scan_bank_8,
    input [63:0] rx_adc_scan_bank_9,
    input [63:0] rx_adc_scan_bank_10,
    input [63:0] rx_adc_scan_bank_11,
    input [63:0] rx_adc_scan_bank_12,
    input [63:0] rx_adc_scan_bank_13,
    input [63:0] rx_adc_scan_bank_14,
    output RX_ADC_DIG_OUT,
    output RX_ADC_VCO_P_OUT,
    output RX_ADC_VCO_N_OUT,
    output reg rx_adc_mem_clk_out,
    output reg [127:0] rx_adc0_out_sar,
    output reg [127:0] rx_adc0_out_vcop,
    output reg [127:0] rx_adc0_out_vcon,
    output reg [127:0] rx_adc1_out_sar,
    output reg [127:0] rx_adc1_out_vcop,
    output reg [127:0] rx_adc1_out_vcon,
    output reg [127:0] rx_adc2_out_sar,
    output reg [127:0] rx_adc2_out_vcop,
    output reg [127:0] rx_adc2_out_vcon,
    output reg [127:0] rx_adc3_out_sar,
    output reg [127:0] rx_adc3_out_vcop,
    output reg [127:0] rx_adc3_out_vcon,
    output reg [127:0] rx_adc02_out_lsb,
    output reg [127:0] rx_adc13_out_lsb,

    // BUMPS
    inout RX_ADC_RF_INP,
    inout RX_ADC_RF_INN,
    inout RX_ADC_CLK_10G_P,
    inout RX_ADC_CLK_10G_N,
    inout RX_ADC_VDDSAR,
    inout RX_ADC_VDDFIR,
    inout RX_ADC_VDDVCO
);

// Fake connection for testing

reg rx_adc_2400MHz_clk;
assign rx_adc_2400MHz_clk = RX_ADC_CLK_10G_P;

reg [3:0] clk_counter;

always @(posedge rx_adc_2400MHz_clk) begin
    if (~RX_ADC_RST_ACTHIGH) begin
        clk_counter <= clk_counter + 1;
        if (clk_counter== 7) begin
            clk_counter <= 0;
            rx_adc_mem_clk_out <= ~rx_adc_mem_clk_out;
        end
    end
    else if (RX_ADC_RST_ACTHIGH) begin
        clk_counter <= 0;
        rx_adc_mem_clk_out <= 0;
        rx_adc0_out_vcop <= 1;
        rx_adc0_out_vcon <= 2;
        rx_adc1_out_sar <= 3;
        rx_adc1_out_vcop <= 4;
        rx_adc1_out_vcon <= 5;
        rx_adc2_out_sar <= 6;
        rx_adc2_out_vcop <= 7;
        rx_adc2_out_vcon <= 8;
        rx_adc3_out_sar <= 9;
        rx_adc3_out_vcop <= 10;
        rx_adc3_out_vcon <= 11;
        rx_adc02_out_lsb <= 12;
        rx_adc13_out_lsb <= 13;
        rx_adc0_out_sar <= 14;
    end
end

wire [9:0] mul;
assign mul = 10;

always @(negedge rx_adc_mem_clk_out) begin
    rx_adc0_out_sar <= rx_adc0_out_sar * mul;
    rx_adc0_out_vcop <= rx_adc0_out_vcop * mul;
    rx_adc0_out_vcon <= rx_adc0_out_vcon * mul;
    rx_adc1_out_sar <= rx_adc1_out_sar * mul;
    rx_adc1_out_vcop <= rx_adc1_out_vcop * mul;
    rx_adc1_out_vcon <= rx_adc1_out_vcon * mul;
    rx_adc2_out_sar <= rx_adc2_out_sar * mul;
    rx_adc2_out_vcop <= rx_adc2_out_vcop * mul;
    rx_adc2_out_vcon <= rx_adc2_out_vcon * mul;
    rx_adc3_out_sar <= rx_adc3_out_sar * mul;
    rx_adc3_out_vcop <= rx_adc3_out_vcop * mul;
    rx_adc3_out_vcon <= rx_adc3_out_vcon * mul;
    rx_adc02_out_lsb <= rx_adc02_out_lsb * mul;
    rx_adc13_out_lsb <= rx_adc13_out_lsb * mul;
end

endmodule
""")
}

// Only connect used signals for testing
class AmyWrapperWrapper extends chisel3.Module {
  val io = IO(new Bundle {
    val scr = new AmyScr
    val rx_adc_mem_clk_out = Output(Bool())
    val RX_ADC_RST_ACTHIGH = Input(Bool())
    val RX_ADC_MEM_WRITE_EN = Input(Bool())
  })
  val mod = Module(new AmyWrapper(false))
  // Hijack scan clk for testing
  mod.io.RX_ADC_RST_ACTHIGH := io.RX_ADC_RST_ACTHIGH
  mod.io.RX_ADC_MEM_WRITE_EN := io.RX_ADC_MEM_WRITE_EN
  io.rx_adc_mem_clk_out := mod.io.rx_adc_mem_clk_out.asUInt
  mod.io.RX_ADC_CLK_10G_P := clock.asUInt
  mod.io.scr <> io.scr
}

class AmySpec extends FlatSpec with Matchers {
  behavior of "Amy's ADC stuff"
  it should "work" in {
    val opt = TestParams.optionsBTolWaveformTB(lsbs = 0, outDir = "test_run_dir/AmyTB")
    dsptools.Driver.execute(() => new AmyWrapperWrapper, opt) { c =>
      new AmyTester(c)
    } should be (true)
  }
}

class AmyBuildSpec extends FlatSpec with Matchers {
  behavior of "Amy's ADC stuff with memories"
  it should "not fail to build" in {
    chisel3.Driver.execute(TestParams.buildWithMemories(name = "AmyBuildWithMemories", topName = "AmyWrapperWrapper"), () => 
      new AmyWrapperWrapper 
    ) 
  }
}

class Mem1PIO(depth: Int, dwidth: Int) extends Bundle {
  val clk = Input(Clock())
  // val wclk = Input(Clock())
  val waddr = Input(UInt(range"[0, $depth)"))
  // val rclk = Input(Clock())
  val raddr = Input(UInt(range"[0, $depth)"))
  val we = Input(Bool())
  val re = Input(Bool())
  val din = Input(UInt(dwidth.W))
  val dout = Output(UInt(dwidth.W))
  override def cloneType = (new Mem1PIO(depth, dwidth)).asInstanceOf[this.type]
}

@chiselName
class Mem1P(depth: Int, dwidth: Int, name: String) extends Module {
  val io = IO(new Mem1PIO(depth, dwidth))
  val mem = SyncReadMem(depth, io.din.cloneType)
  mem.suggestName(name)
/* 
  withClock(io.wclk) {
    when (io.we) { mem.write(io.waddr, io.din) }
  }
  withClock(io.rclk) {
    // 1 clk delay (read address registered)
    io.dout := mem.read(io.raddr, io.re)
  }
*/
  withClock(io.clk) {
    when (io.we) { mem.write(io.waddr, io.din) }
    val re = io.re & ~io.we 
    // 1 clk delay (read address registered)
    io.dout := mem.read(io.raddr, re)
  }
}

// Needs to use chisel3.Module and *not* dspblocks.fft.Module (local scope)
// to have access to implicit clock, reset
class AmyWrapper(useBlackBox: Boolean) extends chisel3.Module {
  val io = IO(new AmyAdcIo)
  val amyBlackBox = Module(new RX_ADC_TOP(useBlackBox))

  amyBlackBox.io.RX_ADC_RST_ACTHIGH := io.RX_ADC_RST_ACTHIGH
  amyBlackBox.io.RX_ADC_DIG_CLK := io.RX_ADC_DIG_CLK
  amyBlackBox.io.RX_ADC_SCAN_IN := io.RX_ADC_SCAN_IN
  amyBlackBox.io.RX_ADC_SCAN_EN := io.RX_ADC_SCAN_EN
  amyBlackBox.io.RX_ADC_MEM_WRITE_EN := io.RX_ADC_MEM_WRITE_EN
  amyBlackBox.io.rx_adc_scan_bank := io.scr.rx_adc_scan_bank
  io.RX_ADC_DIG_OUT := amyBlackBox.io.RX_ADC_DIG_OUT
  io.RX_ADC_VCO_P_OUT := amyBlackBox.io.RX_ADC_VCO_P_OUT
  io.RX_ADC_VCO_N_OUT := amyBlackBox.io.RX_ADC_VCO_N_OUT
  io.rx_adc_mem_clk_out := amyBlackBox.io.rx_adc_mem_clk_out
  amyBlackBox.io.RX_ADC_RF_INP := io.RX_ADC_RF_INP
  amyBlackBox.io.RX_ADC_RF_INN := io.RX_ADC_RF_INN
  amyBlackBox.io.RX_ADC_CLK_10G_P := io.RX_ADC_CLK_10G_P
  amyBlackBox.io.RX_ADC_CLK_10G_N := io.RX_ADC_CLK_10G_N
  amyBlackBox.io.RX_ADC_VDDSAR := io.RX_ADC_VDDSAR
  amyBlackBox.io.RX_ADC_VDDFIR := io.RX_ADC_VDDFIR
  amyBlackBox.io.RX_ADC_VDDVCO := io.RX_ADC_VDDVCO
  
  val clk = amyBlackBox.io.rx_adc_mem_clk_out
  
  val we = withClock(clk) {
    ShiftRegister(io.RX_ADC_MEM_WRITE_EN, 3)
  }

  // WARNING: Using implicit reset here!
  val rst = withClock(clk) {
    ShiftRegister(reset, 3)
  }

  val syncCounterReset = rst | ~we

  val wIdx = withClockAndReset(clk, syncCounterReset) {
    val count = Wire(UInt(range"[0, $memDepth)"))
    val isMaxCount = count === (memDepth - 1).U
    val countNext = Mux(isMaxCount, 0.U, count + 1.U)
    count := RegNext(next = countNext, init = 0.U)
    count
  }

  val memNames = Seq(
    "rx_adc0_out_vcop",
    "rx_adc0_out_vcon",
    "rx_adc1_out_sar",
    "rx_adc1_out_vcop",
    "rx_adc1_out_vcon",
    "rx_adc2_out_sar",
    "rx_adc2_out_vcop",
    "rx_adc2_out_vcon",
    "rx_adc3_out_sar",
    "rx_adc3_out_vcop",
    "rx_adc3_out_vcon",
    "rx_adc02_out_lsb",
    "rx_adc13_out_lsb",
    "rx_adc0_out_sar"
  )

  val memMods = memNames.map { case n => 
    val memMod = Module(new Mem1P(depth = memDepth, dwidth = memWidth, n))
    memMod.io.clk := clk
    memMod.io.we := we
    memMod.io.waddr := wIdx
    memMod.io.din := amyBlackBox.io.elements(n)
    memMod.io.re := io.scr.re
    memMod.io.raddr := io.scr.rIdx
    // Should be Vec
    io.scr.elements(n) match {
      case v: Vec[_] =>
        v.zipWithIndex.foreach { case (out, idx) =>
          out := memMod.io.dout((idx + 1) * scrWidth - 1, idx * scrWidth)
        }
      case _ => 
        throw new Exception("Not a Vec :(")
    }
    n -> memMod
  }.toMap
}

class AmyTester(c: AmyWrapperWrapper) extends DspTester(c) {

  val numTestCycles = 50
  val clkDiv = 16

  updatableDspVerbose.withValue(false) { 

    // Set true to make life easier
    poke(c.io.scr.re, true)

    // Everything is actually clocked via fake internal clock,
    // but I think the time steps should correspond go Chisel testers "step"
    poke(c.io.RX_ADC_RST_ACTHIGH, true)
    step(2 * clkDiv)
    poke(c.io.RX_ADC_RST_ACTHIGH, false)

    poke(c.io.RX_ADC_MEM_WRITE_EN, true)
    step(clkDiv * numTestCycles)
    // 3 slow cycle delay.
    poke(c.io.RX_ADC_MEM_WRITE_EN, false)
    step(clkDiv * 3)

    for (i <- 0 until numTestCycles) {
      poke(c.io.scr.rIdx, i)
      step(clkDiv)
      c.io.scr.elements.filter { case (name, port) => c.mod.memNames.contains(name) }.foreach { case (name, v) => 
        v match {
          case vec: Vec[_] => 
            val temp = vec.map { case p => dspPeekWithBigInt(p)._2 }
            require(temp.length == 2)
            val out = temp(0) + (temp(1) << scrWidth)
            val startingCount = BigInt(c.mod.memNames.indexOf(name) + 1)
            val mulFactor = i + 3
            // Deserializer output starts 3 cycles earlier (due to counter reset)
            val expectedOut = (startingCount * BigInt(10).pow(mulFactor)).mod(BigInt(1) << memWidth)
            expect(out == expectedOut, s"Output $out should match $expectedOut")
          case _ =>
        }
      }
      step(clkDiv)
    }

  }
}