// TODO: Separate out ctrl signals for multi-butterfly support

package FFT
// TODO: Rework logic and vectorize from the start

// ------- Imports START -- DO NOT MODIFY BELOW
import Chisel.{Complex => _, Mux => _, Reg => _, RegNext => _, RegInit => _, Pipe => _, Mem => _,
               Module => _, ModuleOverride => _, when => _, switch => _, is => _, unless => _, Round => _,  _}
import ChiselDSP._
// ------- Imports END -- OK TO MODIFY BELOW
import math._

object WFTA{
  // Supported butterfly radices, grouped by multiples
  val validRad = List(List(4,2),List(3),List(5),List(7))
  // WFTA stages
  val stages = List(Add,Add,Add,Mul,Add,Add,Add,Add)

  def getValidRad = validRad.flatten
  def vRadIdx(rad: Int) = getValidRad.indexOf(rad)
}

abstract class StageType
case object Mul extends StageType
case object Add extends StageType

class WFTAIO[T <: DSPQnm[T]](gen : => T) extends IOBundle (outDlyMatch = true) {

  val p = Params.getBF
  val maxRad = p.rad.max

  // Array location x maps to radix stored at location x in Param.butterfly.rad. i.e.
  // if rad = (2,3,4,5,7), currRad(2) is true when current radix = 4
  val currRad = Vec(p.rad.length,DSPBool(INPUT))
  // Butterfly output
  val y = Vec(maxRad,Complex(gen).asOutput)
  // Butterfly input
  val x = Vec(maxRad,Complex(gen).asInput)
}

/** Butterfly
  * num = butterfly index
  */
class WFTA[T <: DSPQnm[T]](gen : => T , num: Int = 0) extends GenDSPModule (gen) {

  val p = Params.getBF
  val maxRad = p.rad.max

  // Output delays should be matched
  override val io = new WFTAIO(gen)

  // Set constraints on radix-related signals based off of used radices
  val radIn = Vec(WFTA.getValidRad.map( x => {
    val isUsed = p.rad.contains(x)
    // Always true if only 1 radix is used (never switches)
    if(isUsed && p.rad.length == 1) DSPBool(true)
    // Supported radix unused
    else if(!isUsed) DSPBool(false)
    // Otherwise matches valid radices to supported radices
    else io.currRad(p.rad.indexOf(x))
  }))
  debug(radIn)

  // Control signal for muxing inputs
  val r2i = radIn(WFTA.vRadIdx(2))
  val r3i = radIn(WFTA.vRadIdx(3))
  val r4i = radIn(WFTA.vRadIdx(4))
  val r5i = radIn(WFTA.vRadIdx(5))
  val r7i = radIn(WFTA.vRadIdx(7))
  val r2r4i = r2i|r4i
  val r4r7i = r4i|r7i
  val r3r4i = r3i|r4i

  // Assign internal "inputs" to 0 if >= max used radix
  val xp = Vec((0 until WFTA.getValidRad.max).map( i => {
    if (i >= maxRad) Complex(double2T(0),double2T(0))
    else io.x(i)
  }))

  // Input mux
  val x = Vec (
    xp(0) ? (!r2r4i),
    Mux(r2r4i,xp(0),xp(1)),
    xp(2) ? r7i,
    (xp(3) ? r5i) | (xp(3) ? r4r7i),
    (xp(1) ? r4i) | (xp(2) ? r5i) | (xp(4) ? r7i),
    xp(5) ? r7i,
    ((xp(1) ? r2i) | (xp(4) ? r5i)) | ((xp(6) ? r7i) | (xp(2) ? r3r4i))
  )
  debug(x)

  var count = 0
  val addPipe = Complex.getAddPipe
  // Spreads out add delays (handles addPipe < 1 too) + mul delays
  val dly = WFTA.stages.map{ x => {
    count = {
      if (x != Add) 0
      else if (floor(count*addPipe) == 1) 1
      else count + 1
    }
    if (x == Mul) Complex.getMulPipe
    else if (addPipe < 1) floor(count*addPipe).toInt
    else floor(addPipe).toInt
  }}

  // Values of rad(t) = outputs of delay stages @ t
  // i.e. rad(0) is the output of [radIn[Vec] fed through 1 delay stage]
  val rad = Vec(dly.scanLeft(radIn)(Pipe(_,_)).drop(1))
  debug (rad)

  // s0 = r7; s1 = !r4; s2 = !r3; s3 = (r5 + r7)

  // Select into s0, s1 @ 2nd stage (index 1)
  val s0_1 = rad(0)(WFTA.vRadIdx(7))
  val s1_1 = !rad(0)(WFTA.vRadIdx(4))

  // Other selects sx_t
  val s3_4 = rad(3)(WFTA.vRadIdx(5)) | rad(3)(WFTA.vRadIdx(7)) | rad(3)(WFTA.vRadIdx(3))
  val s1_2 = !rad(1)(WFTA.vRadIdx(4))
  val s2_5 = !( rad(4)(WFTA.vRadIdx(3)) | rad(4)(WFTA.vRadIdx(5)) )
  val s1_5 = !rad(4)(WFTA.vRadIdx(4))
  val s0_6 = rad(5)(WFTA.vRadIdx(7))

  // Multiplier is 4th in chain  (index 3 where input is from index 2)
  val r2m = rad(2)(WFTA.vRadIdx(2))
  val r3m = rad(2)(WFTA.vRadIdx(3))
  val r4m = rad(2)(WFTA.vRadIdx(4))
  val r5m = rad(2)(WFTA.vRadIdx(5))
  val r7m = rad(2)(WFTA.vRadIdx(7))
  val r5r7r3m = r5m | r7m | r3m
  val r2r4m = r2m | r4m

  // Butterfly constants
  var u = -2*Pi/3
  val C30 = double2T(cos(u)-1)
  val C31 = double2T(sin(u))
  val C41 = double2T(-1)
  u = -2*Pi/5
  val C50 = double2T((cos(u)+cos(2*u))/2-1)
  val C51 = double2T((cos(u)-cos(2*u))/2)
  val C52 = double2T((sin(u)+sin(2*u)))
  val C53 = double2T(sin(u))
  val C54 = double2T(-1*(sin(u)-sin(2*u)))
  u = -2*Pi/7
  val C70 = double2T((cos(u)+cos(2*u)+cos(3*u))/3-1)
  val C71 = double2T((2*cos(u)-cos(2*u)-cos(3*u))/3)
  val C72 = double2T((cos(u)+cos(2*u)-2*cos(3*u))/3)
  val C73 = double2T((cos(u)-2*cos(2*u)+cos(3*u))/3)
  val C74 = double2T((sin(u)+sin(2*u)-sin(3*u))/3)
  val C75 = double2T((2*sin(u)-sin(2*u)+sin(3*u))/3)
  val C76 = double2T((sin(u)+sin(2*u)+2*sin(3*u))/3)
  val C77 = double2T((sin(u)-2*sin(2*u)-sin(3*u))/3)

  val one = double2T(1.0)

  // Select between constants @ multiplier
  val A = Vec(
    (C30 ? r3m) | (C50 ? r5m) | (C70 ? r7m),
    (C51 ? r5m) | (C71 ? r7m),
    C72 ? r7m,
    C73 ? r7m,
    ((C31 ? r3m) | (C41 ? r4m)) | ((C53 ? r5m) | (C74 ? r7m)),
    ((one ? r4m) | (C52 ? r5m)) | ((C75 ? r7m) | (one ? r2m)),
    C76 ? r7m,
    ((one ? r4m) | (C54 ? r5m)) | (C77 ? r7m)
  )

  debug(A)

  // Flag to select multiply by imaginary (T) or real (F) component
  val MulI7 = !r4m
  val MulI5 = !r2r4m

  // Butterfly diagram (unified WFTA) stage 0 - 7
  val n0a = x(1) + (x(6),dly(0))
  val n0b = x(1) - (x(6),dly(0))
  val n0c = x(4) + (x(3),dly(0))
  val n0d = x(4) - (x(3),dly(0))
  val n0e = x(2) + (x(5),dly(0))
  val n0f = x(2) - (x(5),dly(0))
  val n0g = x(0).pipe(dly(0))
  val n0 = Vec(n0a,n0b,n0c,n0d,n0e,n0f,n0g)
  debug(n0)

  val n1a = n0a + (n0c,dly(1))
  val n1b = n0a - (n0c,dly(1))
  val n1c = n0e - (n0a,dly(1))
  val n1d = n0c - (n0e,dly(1))
  val n1e = n0d + ((n0b ? s1_1),dly(1))
  val n1f = n0b - ((n0d ? s0_1),dly(1))
  val n1g = n0f - (n0b,dly(1))
  val n1h = n0d - (n0f,dly(1))
  val n1i = n0e.pipe(dly(1))
  val n1j = n0f.pipe(dly(1))
  val n1k = n0g.pipe(dly(1))
  val n1 = Vec(n1a,n1b,n1c,n1d,n1e,n1f,n1g,n1h,n1i,n1j,n1k)
  debug(n1)

  val n2a = n1a + (n1i,dly(2))
  val n2b = n1e + (n1j,dly(2))
  val n2c = Mux(s1_2,n1h,n1b).pipe(dly(2))
  val n2d = n1b.pipe(dly(2))
  val n2e = n1c.pipe(dly(2))
  val n2f = n1d.pipe(dly(2))
  val n2g = n1f.pipe(dly(2))
  val n2h = n1g.pipe(dly(2))
  val n2i = n1k.pipe(dly(2))
  val n2 = Vec(n2a,n2b,n2c,n2d,n2e,n2f,n2g,n2h,n2i)

  // Default mul delay used
  // Multiplier logic optimized
  val n3a = n2a ** (A(0),Real)
  val n3b = n2d ** (A(1),Real)
  val n3c = n2e ** (A(2),Real)
  val n3d = n2f ** (A(3),Real)
  val n3e = n2b ** (A(4),Imag)
  val n3f = n2g *? (A(5),im = MulI5)
  val n3g = n2h ** (A(6),Imag)
  val n3h = n2c *? (A(7),im = MulI7)
  val n3i = n2i + (n2a,dly(3))
  val n3 = Vec(n3a,n3b,n3c,n3d,n3e,n3f,n3g,n3h,n3i)
  debug(n3)

  val n4a = n3a + (n3i ? s3_4, dly(4))
  val n4b = n3b.pipe(dly(4))
  val n4c = n3c.pipe(dly(4))
  val n4d = n3d.pipe(dly(4))
  val n4e = n3e.pipe(dly(4))
  val n4f = n3f.pipe(dly(4))
  val n4g = n3g.pipe(dly(4))
  val n4h = n3h.pipe(dly(4))
  val n4i = n3i.pipe(dly(4))
  val n4 = Vec(n4a,n4b,n4c,n4d,n4e,n4f,n4g,n4h,n4i)
  debug(n4)

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
  val n5 = Vec(n5a,n5b,n5c,n5d,n5e,n5f,n5g,n5h,n5i,n5j,n5k)
  debug(n5)

  val n6a = n5a + (n5h,dly(6))
  val n6b = n5b - (n5i,dly(6))
  val n6c = n5i + (n5c,dly(6))
  val n6d = n5d + (n5g,dly(6))
  val n6e = n5e - ((n5j ? s0_6),dly(6))
  val n6f = n5j + (n5f,dly(6))
  val n6g = n5k.pipe(dly(6))
  val n6 = Vec(n6a,n6b,n6c,n6d,n6e,n6f,n6g)
  debug(n6)

  val y = Vec(
    n6g.pipe(dly(7)),
    n6a + (n6d,dly(7)),
    n6b + (n6e,dly(7)),
    n6c - (n6f,dly(7)),
    n6c + (n6f,dly(7)),
    n6b - (n6e,dly(7)),
    n6a - (n6d,dly(7))
  )

  // Output after 8th delay stage
  val r2o = rad(7)(WFTA.vRadIdx(2))
  val r3o = rad(7)(WFTA.vRadIdx(3))
  val r4o = rad(7)(WFTA.vRadIdx(4))
  val r5o = rad(7)(WFTA.vRadIdx(5))
  val r7o = rad(7)(WFTA.vRadIdx(7))

  // Assign outputs when used

  io.y(0) := y(0).trim(Complex.getFrac)
  io.y(1) := Mux(r2o,y(5),y(1)).trim(Complex.getFrac)
  if (maxRad > 2)
    io.y(2) := (((y(6) ? r3o) | (y(3) ? r4o)) | ((y(5) ? r5o) | (y(2) ? r7o))).trim(Complex.getFrac)
  if (maxRad > 3)
    io.y(3) := ((y(2) ? r5o) | (y(3) ? r7o) | (y(5) ? r4o)).trim(Complex.getFrac)
  if (maxRad > 4)
    io.y(4) := ((y(6) ? r5o) | (y(4) ? r7o)).trim(Complex.getFrac)
  if (maxRad > 5)
    io.y(5) := (y(5) ? r7o).trim(Complex.getFrac)
  if (maxRad > 6)
    io.y(6) := (y(6) ? r7o).trim(Complex.getFrac)

}
