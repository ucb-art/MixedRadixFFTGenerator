// TODO: IFFT, don't repeat same frame

package FFT
import scala.math._
import scala.util.Random
import ChiselDSP._

object TestVectors{

  private var in:List[List[ScalaComplex]] = List(List())
  private var out:List[List[ScalaComplex]] = List(List())
  def getIn(idx: Int) = in(idx)
  def getOut(idx: Int) = out(idx)

  /** Fake twiddles for full butterfly test */
  val twiddles = List(
    Complex(-0.25,-0.1),
    Complex(-0.025,0.04),
    Complex(-0.15,0.13),
    Complex(-0.05,-0.2),
    Complex(-0.005,-0.01),
    Complex(-0.125,0.1)
  )

  /** Fake inputs for full butterfly test */
  val inputs = Array(
    Complex(-0.0016156958292784854,-0.0038205920103660867),
    Complex(0.08396018021512272,-0.0013820177961438253),
    Complex(-0.013933768021206223,0.013053573473671093),
    Complex(-0.033684289651395055,0.028591395636659137),
    Complex(-0.015584356598410773,0.00337343167302713),
    Complex(0.015103657363909739,-0.012791286461996752),
    Complex(0.01926837435299409,-0.02547371574646024)
  )

  /** Tones @ fraction of fs */
  val realf = List(0.2,0.3,0.4,0.25)
  val reala = List(0.25,0.15,0.02,0.03)

  /** Init test vectors */
  def apply(sizes: List[Int], frames: Int) : Unit = {
    val (i,o) = (for (e <- sizes) yield {apply(e,frames)}).unzip
    in = i
    out = o
  }

  /** Create list of inputs */
  def populateIn(FFTN: Int): List[ScalaComplex] = {
    var inProto = Array.empty[ScalaComplex]
    // Butterfly tests known set
    if (FFTN <= WFTA.getValidRad.max){
      inputs.zipWithIndex.foreach {
        case (e, idx) => {
          if (idx < 2 | FFTN > idx) inProto = inProto :+ e
        }
      }
    }
    // Larger FFT sizes
    else{
      for (i <- 0 until FFTN){
        val r1 = (reala,realf).zipped.map( (a,f) => a*sin(2*Pi*f*i))
        val r2 = r1.foldLeft(0.0001+i.toDouble/FFTN/100)(_ + _)
        inProto = inProto :+ Complex(r2, 0.0) // + 0.001*Random.nextGaussian,0.04*Random.nextGaussian)
      }
    }
    inProto.toList
  }

  /** Create list of outputs */
  def populateOut(inProto: List[ScalaComplex], FFTN: Int) : List[ScalaComplex] = {
    var outProto = Array.fill(FFTN){Complex(0.0,0.0)}
    // Direct (inefficient) FFT calculation
    // exp(ix) = cos(x) + i*sin(x)
    // exp(-j*2*pi*n*k/FFTN) = cos(-2*pi*n*k/FFTN) + i*sin(-2*pi*n*k/FFTN)
    for (k <- 0 until FFTN;
         n <- 0 until FFTN){
      val s = sin(-2*Pi*n*k/FFTN)
      val c = cos(-2*Pi*n*k/FFTN)
      val ir = inProto(n).real
      val ii = inProto(n).imag
      outProto(k).real = outProto(k).real + ir * c - ii * s
      outProto(k).imag = outProto(k).imag + ii * c + ir * s
    }
    outProto.toList
  }

  /** Create test vectors for particular FFTN */
  def apply(FFTN : Int, frames: Int) : Tuple2[List[ScalaComplex],List[ScalaComplex]] = {
    val inProto = populateIn(FFTN)
    val outProto = populateOut(inProto,FFTN)
    // Repeat for specified # of frames
    val inN = List.fill(frames)(inProto).flatten
    val outN = List.fill(frames)(outProto).flatten
    (inN,outN)
  }

}