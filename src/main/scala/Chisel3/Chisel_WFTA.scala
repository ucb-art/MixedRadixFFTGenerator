package dspblocks.fft
import chisel3._
import chisel3.experimental._
import barstools.tapeout.transforms._
import chisel3.util._
import dsptools.{hasContext, DspContext}
import math._
import dsptools.numbers._
import dsptools.numbers.implicits._

object WFTA {
  // Supported butterfly radices, grouped 
  // NOTE: This is fixed and should not be changed unless WFTA is redesigned (ORDER MATTERS)
  val groupedValidRad = Seq(Seq(4, 2), Seq(3), Seq(5), Seq(7))
  // WFTA stages (WFTA operation broken up into add/multiply stages)
  // Note: This is fixed and should not be changed unless WFTA is redesigned
  val stages = Seq(WFTAAdd, WFTAAdd, WFTAAdd, WFTAMul, WFTAAdd, WFTAAdd, WFTAAdd, WFTAAdd)

  def getValidRad = groupedValidRad.flatten
  def getRadIdx(rad: Int) = getValidRad.indexOf(rad)
}

abstract class WFTAStageType
case object WFTAMul extends WFTAStageType
case object WFTAAdd extends WFTAStageType

// TODO: Make an only real version (only real inputs)
// TODO: Already stated -- rename FactorizationParams
class WFTAIO[T <: Data:RealBits](dspType: => T, fftParams: FactorizationParams) extends Bundle {
  val usedRads = fftParams.butterfly.rad 
  // TODO: Less hacky
  // Radix-2: Perform 2 in parallel to decrease cycle count
  val maxRad = fftParams.butterfly.maxRad.max(if (usedRads.contains(2)) 4 else 0)
  val currRad = new CustomIndexedBundle(usedRads.map(r => r -> Input(Bool())): _*)
  // Output
  val y = Vec(maxRad, Output(DspComplex(dspType)))
  // Input
  val x = Vec(maxRad, Input(DspComplex(dspType)))
  val clk = Input(Clock())
  override def cloneType = (new WFTIO(dspType, fftParams)).asInstanceOf[this.type]
}

@chiselName
class WFTA[T <: Data:RealBits](dspType: => T, fftParams: FactorizationParams) extends Module with hasContext with DelayTracking {

  // TODO: Move to some delay params?
  val inPipe = 1

  // TODO: Move to some delay params?
  // TODO: Support fractional numAddPipes (even though DspContext doesn't?) / check functional correctness
  // Delays through WFTA stages (as dictated by amount to pipeline fixed-point multiply and add operations)
  def wftaInternalDelays = {
    var count = 0
    // Spreads out add delays (handles addPipe < 1 too) + mul delays
    WFTA.stages.map { case x => 
      count = {
        // Reset count on multiply (only handle sequence of adds)
        if (x != Add) 0
        // Previously already added pipe, so "reset" (to 1 since this is also an Add)
        else if (math.floor(count * context.numAddPipes.toDouble) == 1) 1
        else count + 1
      }
      if (x == Mul) context.numMulPipes
      // Add a Pipe if enough adds in series
      else if (context.numAddPipes < 1) math.floor(count * context.numAddPipes.toDouble).toInt
      else math.floor(context.numAddPipes.toDouble).toInt
    }
  }

  val internalDelays = wftaInternalDelays

  val io = new WFTAIO(dspType, fftParams)

  // TODO: Fix name with @chiselName elsewhere
  withClock(io.clk) {

    // Maps used radices to whatever is supported
    val radIn = Wire(CustomIndexedBundle(Bool(), WFTA.getValidRad))

    WFTA.getValidRad foreach { case rad => 
      val isUsed = io.usedRads.contains(rad)
      // Always true if only 1 radix is used (never switches)
      if (isUsed && io.usedRads.length == 1) 
        radIn(rad) := true.B
      // Unused
      else if (!isUsed) 
        radIn(rad) := false.B
      // Match valid radices to used radices
      else 
        radIn(rad) := ShiftRegister(io.currRad(rad), inPipe)
    }

    // Control signal for muxing inputs
    // TODO: Can you avoid hard-coding? Maybe impossible given that WFTA structure is fixed...
    val r2i = radIn(2)
    val r3i = radIn(3)
    val r4i = radIn(4)
    val r5i = radIn(5)
    val r7i = radIn(7)
    val r2r4i = r2i | r4i
    val r4r7i = r4i | r7i
    val r3r4i = r3i | r4i

    // 2x rad 2 butterfly
    // TODO: Pick better adder (reuse rather than add more)
    val r2r5i = r2i | r5i

    val zero = Complex(Ring[T].zero, Ring[T].zero)

    // Assign internal "inputs" to 0 if >= max used radix
    val xp = Wire(Vec(WFTA.getValidRad.max, DspComplex(dspType)))
    xp.zipWithIndex foreach { case (x, idx) =>
      if (idx >= io.maxRad) x := zero
      else x := ShiftRegister(io.x(idx), inPipe)
    }
    // TODO: Move shift register here?
    // Input mux (see ICASSP paper for mapping)
    val x = Wire(xp.cloneType)
    x(0) := Mux1H(Seq((~r2r4i) -> xp(0)))
    x(1) := Mux(r2r4i, xp(0), xp(1))
    x(2) := Mux1H(Seq(r7i -> xp(2)))
    x(3) := Mux1H(Seq(
      r2r5i -> xp(3),                         // To support 2x radix 2
      r4r7i -> xp(3)
    ))
    x(4) := Mux1H(Seq(
      r4i -> xp(1),
      r2r5i -> xp(2),                         // To support 2x radix 2
      r7i -> xp(4)
    ))
    x(5) := Mux1H(Seq(r7i -> xp(5)))
    x(6) := Mux1H(Seq(
      r2i -> xp(1),
      r5i -> xp(4),
      r7i -> xp(6),
      r3r4i -> xp(2)
    ))

    // Values of rad(t) = outputs of delay stages @ t
    // i.e. rad(0) is the output of [radIn, which is a CustomIndexedBundle fed through 1 delay stage]
    // (Drop drops first element)
    val rad = Vec(internalDelays.scanLeft(radIn)((accum, dly) => ShiftRegister(accum, dly)).drop(1))

    // s0 = r7; s1 = !r4; s2 = !r3; s3 = (r5 + r7)

    // rad(stage # -- associated with delay)(radix)
    // TODO: Come up with better naming
    // Select into s0, s1, @ 2nd stage (index 1)
    val s0_1 = rad(0)(7)
    // Original optimization doesn't work for 2x rad 2 butterflies
    // val s1_1 = ~rad(0)(4)
    val s1_1 = ~(rad(0)(4) | rad(0)(2))

    // Other selects  sx_t
    val s3_4 = rad(3)(5) | rad(3)(7) | rad(3)(3)
    val s1_2 = ~rad(1)(4)
    val s2_5 = ~(rad(4)(3) | rad(4)(5))
    val s1_5 = ~rad(4)(4)
    val s0_6 = rad(5)(7)

    // Multiplier is 4th in chain (index 3 where input is from index 2)
    val r2m = rad(2)(2)
    val r3m = rad(2)(3)
    val r4m = rad(2)(4)
    val r5m = rad(2)(5)
    val r7m = rad(2)(7)
    val r5r7r3m = r5m | r7m | r3m 
    val r2r4m = r2m | r4m 

    val c1 = -2 * Pi / 3
    val C30 = dspType.fromDouble(cos(c1) - 1)
    val C31 = dspType.fromDouble(sin(c1))
    val C41 = dspType.fromDouble(-1.0)

    val c2 = -2 * Pi / 5
    val C50 = dspType.fromDouble((cos(c2) + cos(2 * c2)) / 2 - 1)
    val C51 = dspType.fromDouble((cos(c2) - cos(2 * c2)) / 2)
    val C52 = dspType.fromDouble((sin(c2) + sin(2 * c2)))
    val C53 = dspType.fromDouble(sin(c2))
    val C54 = dspType.fromDouble(-1 * (sin(c2) - sin(2 * c2)))

    val c3 = -2 * Pi / 7
    val C70 = dspType.fromDouble((cos(c3) + cos(2 * c3) + cos(3 * c3)) / 3 - 1)
    val C71 = dspType.fromDouble((2 * cos(c3) - cos(2 * c3) - cos(3 * c3)) / 3)
    val C72 = dspType.fromDouble((cos(c3) + cos(2 * c3) - 2 * cos(3 * c3)) / 3)
    val C73 = dspType.fromDouble((cos(c3) - 2 * cos(2 * c3) + cos(3 * c3)) / 3)
    val C74 = dspType.fromDouble((sin(c3) + sin(2 * c3) - sin(3 * c3)) / 3)
    val C75 = dspType.fromDouble((2 * sin(c3) - sin(2 * c3) + sin(3 * c3)) / 3)
    val C76 = dspType.fromDouble((sin(c3) + sin(2 * c3) + 2 * sin(3 * c3)) / 3)
    val C77 = dspType.fromDouble((sin(c3) - 2 * sin(2 * c3) - sin(3 * c3)) / 3)

    // TODO: Does using Ring[T].one vs. dspType.fromDouble(1.0) affect anything?
    val one = dspType.fromDouble(1.0)

    val maxConstantWidth = 
      Seq(C30, C31, C41 C50, C51, C52, C53, C54, C70, C71, C72, C73, C74, C75, C76, C77).map(x => x.getWidth).max
    // one should be the largest represented constant
    require(maxConstantWidth <= one.getWidth, "Cannot chop off MSBs")

    // Select between constants @ multiplier
    val A = Wire(Vec(WFTA.getValidRad.max + 1, one.cloneType))
    A(0) := Mux1H(Seq(
      r3m -> C30,
      r5m -> C50,
      r7m -> C70
    ))
    A(1) := Mux1H(Seq(
      r5m -> C51,
      r7m -> C71
    ))
    A(2) := Mux1H(Seq(
      r7m -> C72
    ))
    A(3) := Mux1H(Seq(
      r7m -> C73
    ))
    A(4) := Mux1H(Seq(
      r3m -> C31,
      r4m -> C41,
      r5m -> C53,
      r7m -> C74
    ))
    A(5) := Mux1H(Seq(
      r4m -> one,
      r5m -> C52,
      r7m -> C75,
      r2m -> one
    ))
    A(6) := Mux1H(Seq(
      r7m -> C76
    ))
    A(7) := Mux1H(Seq(
      r4m -> one,
      r5m, C54, 
      r7m -> C77
    ))

    // TODO: Change caps convention
    // Flag to select multiply by imaginary (T) or real (F) component
    val MulI7 = ~r4m
    val MulI5 = ~r2r4m

    // Butterfly diagram (unified WFTA) stage 0 - 7
    DSPContext.withNumAddPipes(internalDelays(0)) {
      val n0a = x(1) context_+ x(6)
      val n0b = x(1) context_- x(6)
      val n0cTemp = x(4) context_+ x(3)                     // **
      val n0dTemp = x(4) context_- x(3)
      val n0e = x(2) context_+ x(5)
      val nr2_1 = ~rad(0)(2)                                // Don't propagate result (so it won't mess up other results)
      val n0c = Mux1H(Seq(nr2_1 -> n0cTemp))                // **
      val n0d = Mux1H(Seq(nr2_1 -> n0dTemp))
      val n0f = x(2) context_- x(5)
      val n0g = ShiftRegister(x(0), internalDelays(0))    
    }

    DSPContext.withNumAddPipes(internalDelays(1)) {
      val n1a = n0a context_+ n0c
      val n1b = n0a context_- n0c
      val n1c = n0e context_- n0a
      val n1d = n0cTemp context_- n0e                       // ** Can pass through
      val n1e = n0dTemp + Mux1H(Seq(s1_1 -> n0b))           // **
      val n1f = n0b context_- (n0d ? s0_1)
      val n1g = n0f context_- n0b
      val n1h = n0d context_- n0f
      val n1i = ShiftRegister(n0e, internalDelays(1))
      val n1j = ShiftRegister(n0f, internalDelays(1))
      val n1k = ShiftRegister(n0g, internalDelays(1))
    }

    DSPContext.withNumAddPipes(internalDelays(2)) {
      val n2a = n1a context_+ n1i
      val n2b = n1e context_+ n1j
      val n2c = ShiftRegister(Mux(s1_2, n1h, n1b), internalDelays(2))
      val n2d = ShiftRegister(n1b, internalDelays(2))
      val n2e = ShiftRegister(n1c, internalDelays(2))
      val n2f = ShiftRegister(n1d, internalDelays(2))
      val n2g = ShiftRegister(n1f, internalDelays(2))
      val n2h = ShiftRegister(n1g, internalDelays(2))
      val n2i = ShiftRegister(n1k, internalDelays(2))
    }

    // TODO: Check drop is right; can minimize registers some more
    val X0b = ShiftRegister(n1d, internalDelays.drop(2).sum)          // ** First output of second radix 2 butterfly
    val X1b = ShiftRegister(n2b, internalDelays.drop(3).sum)          // ** Second output of second radix 2 butterfly

    // TODO: Add to dsptools?
    abstract class Scalar
    object Real extends Scalar
    object Imag extends Scalar
    // TODO: More interesting modes? -- using -& now (no pipe)
    def context_complex_scalar_*(a: DspComplex[T], b: T, mode: Scalar): DspComplex = {
      mode match {
        case Real => DspComplex.wire(a.real context_* b, a.imag context_* b)
        case Imag => DspComplex.wire(a.imag context_* (Ring[T].zero -& b), a.real context_* b)
      }
    }
    def context_complex_scalar_*(a: DspComplex[T], b: T, bIsImag: Bool): DspComplex = {
      val newB = Mux(bIsImag, Ring[T].zero -& b, b)
      val x = a.real context_* b
      val y = a.imag context_* newB
      val bIsImagDelay = ShiftRegister(bIsImag, context.numMulPipes)
      DspComplex.wire(Mux(bIsImagDelay, y, x), Mux(bIsImagDelay, x, y))
    }

    // Multiplier logic optimized
    // Delay same as context
    val n3a = context_complex_scalar_*(n2a, A(0), Real)
    val n3b = context_complex_scalar_*(n2d, A(1), Real)
    val n3c = context_complex_scalar_*(n2e, A(2), Real)
    val n3d = context_complex_scalar_*(n2f, A(3), Real)
    val n3e = context_complex_scalar_*(n2b, A(4), Imag)
    val n3f = context_complex_scalar_*(n2g, A(5), bIsImag = MulI5)
    val n3g = context_complex_scalar_*(n2h, A(6), Imag)
    val n3h = context_complex_scalar_*(n2c, A(7), bIsImag = MulI7)
    val n3i = DSPContext.withNumAddPipes(context.numMulPipes) { n2i context_+ n2a }

    DSPContext.withNumAddPipes(internalDelays(4)) {
      val n4a = n3a context_+ Mux1H(Seq(s3_4 -> n3i))       
      val n4b = ShiftRegister(n3b, internalDelays(4))
      val n4c = ShiftRegister(n3c, internalDelays(4))
      val n4d = ShiftRegister(n3d, internalDelays(4))
      val n4e = ShiftRegister(n3e, internalDelays(4))
      val n4f = ShiftRegister(n3f, internalDelays(4))
      val n4g = ShiftRegister(n3g, internalDelays(4))
      val n4h = ShiftRegister(n3h, internalDelays(4))
      val n4i = ShiftRegister(n3i, internalDelays(4))
    }



/*
  val n5a = n4a + (n4b,dly(5))
  val n5b = n4a - (n4b,dly(5))
  val n5c = n4a - (n4d,dly(5))
  val n5d = n4e + ((n4f ? s2_5),dly(5))
  val n5e = n4e - (n4f,dly(5))
  val n5f = (n4e ? s1_5) - (n4h,dly(5))
  val n5g = Mux(s1_5,n4h,n4g).pipe(dly(5))
  val n5h = n4d.pipe(dly(5))
  val n5i = n4c.pipe(dly(5))
  val n5j = n4g.pipe(dly(5))
  val n5k = n4i.pipe(dly(5))


  val n6a = n5a + (n5h,dly(6))
  val n6b = n5b - (n5i,dly(6))
  val n6c = n5i + (n5c,dly(6))
  val n6d = n5d + (n5g,dly(6))
  val n6e = n5e - ((n5j ? s0_6),dly(6))
  val n6f = n5j + (n5f,dly(6))
  val n6g = n5k.pipe(dly(6))


  val y = Vec(
    n6g.pipe(dly(7)),
    n6a + (n6d,dly(7)),
    n6b + (n6e,dly(7)),
    n6c - (n6f,dly(7)),
    n6c + (n6f,dly(7)),
    n6b - (n6e,dly(7)),
    n6a - (n6d,dly(7))
  )
  debug(y)

*/





    
    

   
   
    
    
    

    




  }
}
// todo: externally set inputs to zero when not doing FFT