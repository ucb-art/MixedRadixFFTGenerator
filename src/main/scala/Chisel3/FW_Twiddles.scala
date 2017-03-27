package dspblocks.fft
import breeze.math.Complex

// TODO: There's actually overlap between 675, 800, 864 twiddles -- could theoretically reuse(?)
// TODO: Convert 2D Seq into Map (more informative)
case class TwiddleParams(
  // Row = per FFT
  twiddleCountMax: Seq[Seq[Int]] = Seq(Seq.empty),
  twiddleLUTScale: Seq[Seq[Int]] = Seq(Seq.empty),
  // Coprime --> LUT --> Twiddle Lane (col) 
  twiddles: Map[Int, Seq[Seq[Complex]]] = Map(0 -> Seq(Seq.empty)),
  twiddleSubcountMax: Seq[Seq[Int]] = Seq(Seq.empty)
)
/*
twiddleCountMax main twiddle count max for each calculation stage (CTA)
twiddleLUTScale base multiply amount to scale range of twiddle counts to full twiddle LUT size
twiddles for each radix
twiddleSubcountMax is the sub count to hold the twiddle value when using PFA
*/

object Twiddles {
  def apply(fftParams: FactorizationParams): FactorizationParams = {
    val coprimes = fftParams.io.coprimes
    val global = fftParams.io.global
    val stagesInfo = fftParams.calc.getStages
    val updatedTwiddleParams = apply(coprimes, global, stagesInfo)
    fftParams.copy(twiddle = updatedTwiddleParams)
  }
  def apply(
      coprimes: Seq[Seq[CoprimeInfo]], 
      global: Seq[GlobalPrimeInfo],
      stagesInfo: Seq[FFTNStageInfo]
  ): TwiddleParams = {

    // Count max for CTA (within a coprime)
    val twiddleCountMax = stagesInfo.zip(coprimes).map { case (fftStagesInfo, fftCoprimes) => 
      // For a particular FFTN, gets the coprime corresponding to the current radix
      def rad2Coprime(rad: Int, fftCoprimes: Seq[CoprimeInfo]): Int = {
        // Gets base prime associated with radix
        val basePrime = WFTA.groupedValidRad.filter(radGroup => radGroup.contains(rad)).head.min
        fftCoprimes.filter(_.associatedPrime == basePrime).head.coprime
      }

      val stages = fftStagesInfo.stages
      val prevStages = fftStagesInfo.prevStages

      val firstRad = stages.head
      stages.zip(prevStages).tail.scanLeft(rad2Coprime(firstRad, fftCoprimes) / firstRad - 1) { 
        case (accum, (currentRad, prevRad)) =>
          // For radices in a coprime, the previous radix is >= the current radix (also divisible by)
          // However, @ coprime boundaries there will always be a non-zero mod i.e.
          // Stage 1: 0 to N_coprime/R1-1
          // Stage 2: 0 to N_coprime/R1/R2-1
          // Unused stages have 0 count
          // TODO: Double check if I used 1 or 0
          require(currentRad != 1)
          if (currentRad == 0) 0
          else if (prevRad % currentRad == 0) (accum + 1) / currentRad - 1
          else rad2Coprime(currentRad, fftCoprimes) / currentRad - 1
      }
    }

    // Initial coprime twiddle count renormalization (multiply count)
    // To renormalize twiddle count to the full twiddle LUT range i.e. addr. 2 of 4 --> addr. 4 of 8
    // max coprime / current coprime (same base), except when current coprime < max radix (associated w/
    // the coprime) or coprime = 1 i.e. no twiddle used

    val twiddleLUTScale = coprimes.map(row => row.map { case CoprimeInfo(coprime, prime, _) =>
      // If a coprime slot is "unused" for a given FFT size, prime = 1 and global.find will return nothing
      // In that case, twiddles are unused, so just use a scale of 0  
      val foundMaxGlobal = global.find(info => info.prime == prime).getOrElse(GlobalPrimeInfo(1, 1, 1))
      if (coprime == 1 || coprime < foundMaxGlobal.maxRadix) 0 else foundMaxGlobal.maxCoprime / coprime
    })

    // TODO: Turn all of these into maps
    val (twiddlesTemp, twiddleLUTDepths) = global.map { case GlobalPrimeInfo(_, maxRadix, maxCoprime) => 
      twiddleN(maxRadix, maxCoprime)
    }.unzip
    val twiddles = twiddlesTemp.zip(global).map { case (tw, GlobalPrimeInfo(_, _, maxCoprime)) => 
      maxCoprime -> tw
    }.toMap

    println("Twiddle Memory Size: " + twiddleLUTDepths.sum)

    // Twiddle sub-count max is calculated by the product of the coprimes to the right of the current coprime
    val twiddleSubcountMax = coprimes.map { case fftRow => 
      val nextCoprimes = fftRow.tail
      // TODO: Easier to understand w/o first doing this tail thing
      (0 until nextCoprimes.length).map(idx => nextCoprimes.drop(idx).map(col => col.coprime).product - 1) 
    }

    TwiddleParams(twiddleCountMax, twiddleLUTScale, twiddles, twiddleSubcountMax)
  }

  // Calculates twiddles associated with each coprime set
  def twiddleN(rad: Int, maxCoprime: Int): (Seq[Seq[Complex]], Int) = {
    // TODO: Can you expand out to radix-8?
    // First stage of coprime calculation requires the most unique twiddles (coprime/radix)
    // Subsequent stages use renormalized addresses
    val twiddleNSize = maxCoprime/rad

    val twiddles = (0 until twiddleNSize).map { case k => 
      // Radix-n requires n-1 subsets of twiddles (1 to n-1); n = 0 is trivial
      (1 until rad).map { case n => 
        // Twiddle W_N^(nk) = exp(-j*2*pi*n*k/N)
        // exp(i*t) = cos(t) + i*sin(t)
        val t = -2 * math.Pi * k * n / maxCoprime
        Complex(math.cos(t), math.sin(t))
      }
    }
    (twiddles, twiddleNSize * (rad - 1))
  }
}