// TODO: Support IFFT, check signal limit (in double test)

package FFT
import ChiselDSP._
import Chisel.{Complex => _, _}

/** Module tester that allows switching between fixed and floating point testing */
class FFTTests[T <: FFT[_ <: DSPQnm[_]]](c: T) extends DSPTester(c) {

  // Set tolerance for comparing expected values
  DSPTester.setTol(floTol = 32,fixedTol = 4)
  runAll()

  ////////////////////////////////////////////////////////////////////////

  /** Run all tests for all FFTNs */
  def runAll() : Unit = runTo(Params.getFFT.sizes.last)

  /** Run for all FFT sizes until (and including) N */
  def runTo(n: Int): Unit = {
    reset()
    val sizes = Params.getFFT.sizes
    val idx = sizes.indexOf(n)
    for (i <- 0 to idx) run(sizes(i))
  }

  /** Run tests for desired FFTN */
  def run(n: Int) : Unit = {
    Tracker.reset(n)
    if (traceOn) Status("FFTN = " + n)
    stepThrough(n)
  }

  /** Step through test vectors for FFTN */
  def stepThrough(n: Int): Unit = {
    val idxN = Params.getFFT.sizes.indexOf(n)
    if (idxN < 0) Error("FFTN is not included in this generated output")
    val inVec = TestVectors.getIn(idxN)
    val outVec = TestVectors.getOut(idxN)
    // For WFTA tests
    val radIdx = Params.getBF.rad.indexOf(Tracker.FFTN).max(0)
    poke(c.io2.currRad(radIdx),true)
    for ( i <- 0 until Tracker.FFTN) {poke(c.io2.x(i),inVec(i))}
    step(c.io2.getOutDelay)
    for ( i <- 0 until Tracker.FFTN)
      expect(c.io2.y(i),outVec(i), test = Tracker.FFTN.toString, error = "")
    poke(c.io2.currRad(radIdx),false)
  }

}

object Tracker {

  // Variables to track tester progress
  var startSymbol = false
  var outValid = false
  var inStep = 0
  var outStep = 0
  var FFTN = 0

  // Reset variables on new test
  def reset(n: Int) : Unit = {
    startSymbol = false
    outValid = false
    inStep = 0
    outStep = 0
    FFTN = n
  }
}
