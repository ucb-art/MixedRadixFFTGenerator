// May 31, 2015
// Need to verify correctness for > 3 coprime factors and < 3 coprime factors

package FFT
import scala.math._

object ioAddressConstants{

	// See GMR paper
	// For each FFT size, get necessary q' values for input/output
	// Because q' is derived from "grouping" coprimes, there are (# of coprimes -1) groups
	var qDIFiArray = Array.ofDim[Int](fftSizes.count,generalConstants.numCoprimes-1)
	var qDIFoArray = Array.ofDim[Int](fftSizes.count,generalConstants.numCoprimes-1)
	var dec2xAryArray = Array.ofDim[Int](generalConstants.validPrimes.length,0,0)

	// Generate constants
	def generate(verboseTF:Boolean) : Unit = {
		for (i <- 0 to fftSizes.count-1){
			// Calculate q' values from coprimes of FFT N
			val coprimeFactors = generalConstants.coprimesArray(i)
			qDIFiArray(i) = qCalculate(coprimeFactors)

			// Output N requires flipping N1N2N3 -> N3N2N1
			val coprimeFactorsFlipped = coprimeFactors.reverse
			qDIFoArray(i) = qCalculate(coprimeFactorsFlipped)
		}

		// Make constants for LUTS to convert from decimal to X-ary (decimal to ternary, etc.)
		// for all used primes
		for (i <- 0 to generalConstants.validPrimes.length-1){
			val primeVal = generalConstants.validPrimes(i)	
			if (primeVal != 2){		
				val maxVal = generalConstants.maxCoprime(i)

				val numCounters:Int = (log(maxVal)/log(primeVal)).round.toInt		// # of counters to generate
				println(maxVal + "," + primeVal + ", numcounts" + numCounters)
				dec2xAryArray(i) = dec2Xary(maxVal,numCounters,primeVal-1)
			}
			// Note can handle 2 with bits rather than LUT
		}
		
		// Debug
		if (verboseTF){
			println(Console.BLUE + Console.BOLD + s"\nDIF Q' In:")
			for (i <- 0 to fftSizes.count-1){
				println(Console.RESET + Console.BLUE + qDIFiArray(i).mkString("\t"))
			}
			println(Console.BLUE + Console.BOLD + "\n\nDIF Q' Out:")
			for (i <- 0 to fftSizes.count-1){
				println(Console.RESET + Console.BLUE + qDIFoArray(i).mkString("\t"))
			}

			for (i <- 0 to generalConstants.validPrimes.length-1){
				if (dec2xAryArray(i).nonEmpty){
					println(Console.BLUE + Console.BOLD + s"\n\nDecimal to ${generalConstants.validPrimes(i)}-ary Constants (Lowest first):")
					for (j <- 0 to generalConstants.maxCoprime(i)-1){
						println(Console.RESET + Console.BLUE + dec2xAryArray(i)(j).mkString("\t"))
					}
				}
			}
			println("\n")
		}

	}

	// GCD in scala
	def gcd_recursive(x:Int, y:Int): Int = {
		if (y == 0){
			return x
		}
		else{
			return gcd_recursive(y,x%y)
		}
	}

	// Extended Euclidean Algorithm
	def extended_euclid(N1: Int, N2:Int): Array[Int] = {
		
		// Out = [p q GCD]
		var out = new Array[Int](3)

		// p1N1 = q1N2 + 1 for coprime
		// p1N1-q1N2 = 1
		// Algorithm ax + by = gcd(a,b)
		// Coprime: ax + by = 1

		var s:Int = 0
		var t:Int = 1
		var r:Int = N2
		var old_s:Int = 1
		var old_t:Int = 0
		var old_r:Int = N1

		while (r != 0){
			var quotient = floor(old_r/r).toInt
			var temp = r
			r = old_r - quotient*temp
			old_r = temp
			temp = s
			s = old_s - quotient*temp
			old_s = temp
			temp = t
			t = old_t - quotient*temp
			old_t = temp
		}

		out(2) = old_r		// gcd
		out(0) = old_s		// p
		out(1) = -1*old_t	// to get positive q

		return out
		
	}

	// Calculate q' values
	def qCalculate(coprimeFactors: Array[Int]): Array[Int] = {

		var qOut = new Array[Int](coprimeFactors.length-1)
		
		// Count to keep track of places with non-1 coprime factors
		var count: Int = 0

		//Get rid of {2,3,5}^0 
		val coprimeFactorsStripped = coprimeFactors.filterNot(_==1)
		
		// For N1*N2*N3 = N, there are 2 equation groups relating to
		// n,k and n2~,k2~ --> N1*(N2*N3), N2*N3
		val factorLen = coprimeFactorsStripped.length
		val numEqnGroups = factorLen-1

		// n= (Nright1*n1 + A1*n2~) mod N
        // k= (B1*k1 + Nleft1*k2~) mod N
        // n2~=(Nright2*n2 + A2*n3) mod Nright1
        // k2~=(B2*k2+Nleft2*k3) mod Nright1
        // where Nright1=N2*N3, Nleft1=N1; Nright2=N3, Nleft2=N2

		// If N1 and (N2*N3) are relatively prime
        // A1 = p1*N1 = q1*N2*N3+1
        // If N2 and N3 are relatively prime
        // A2 = p2*N2 = _q2*N3 + 1
        // B2 = p3*N3 = _q3*N2 + 1
        
        // For Cooley-Tukey (N1, N2 not relatively prime)
        // n = (N2*n1 + A1*n2)modN = (N2*n1 + n2)modN
        // k = (B1*k1 + N1*k2)modN = (k1 + N1*k2)modN
        // A1 = B1 = 1
        
        // Above rule guarantee conflict-free in-place FFT calculation (but not
        // necessarily concurrent I/O in place)
        
        // For I/O: 
       
        // If N1, N2, and N3 relatively co-prime then
        // p1*N1 = q1*N2*N3+1 = A1
        // p2*N2 = q2*N1*N3+1 = A2
        // p3*N3 = q3*N1*N2+1 = B2
        
        // --> _q2 = q2*N1
        // --> _q3 = q3*N1
        
        // n = (N2*N3*n1 + p1*N1*n2~) modN
        // k = (p2*p3*N2*N3*k1 + N1*k2~)modN
        // n2~ = (N3*n2 + p2*N2*n3) mod N2*N3
        // k2~ = (p3*N3*k2 + N2*k3)mod N2*N3
        
        // n = (N2*N3*n1 + p1*N1*N3*n2 + p1*N1*p2*N2*n3) mod N
        // k = (p2*p3*N2*N3*k1 + N1*p3*N3*k2 + N1*N2*k3) mod N
        // decomposing as N1*N2*N3
        
        // HOWEVER, from GMR paper, if decomse N3*N2*N1 
        // n = (N1*N2*n1 + p3*N1*N3*n2 + p2*p3*N2*N3*n3)modN
        // k = (p1*p2*N1*N2*k1 + p1*N1*N3*k2 + N2*N3*k3)modN
        
        // Note: the above eqns seem to be missing intermediate mods that
        // don't cancel out (???) But regardless, PFA DIF <-> DIT equivalent
        // should be interchangeable
        
        // first k -> second n where (k3,k2,k1)-->(n1,n2,n3)

		for (i <- 1 to numEqnGroups){			
			
			// Note Matlab to normal programming index conversion
			// Left/right constants for in-place calculations
			val leftConstant = coprimeFactorsStripped(i-1)
			val rightConstant = coprimeFactorsStripped.drop(i).product

			// GCD != 1 means not coprime
			val coprimeCheck = gcd_recursive(leftConstant, rightConstant)

			// Initialize
			var A:Int = 0
			var rightConstantN:Int = 0

			if (coprimeCheck != 1){
				A = 1.toInt
			}
			else{

				// To handle concurrent IO _q2, _q3 (when you have >1
                // equation groups) - all co-prime
				if (i > 1){
					val prevLeftConstant = coprimeFactorsStripped.dropRight(factorLen-i+1).product
					rightConstantN = rightConstant*prevLeftConstant
				}
				else{
					rightConstantN = rightConstant
				}

				// Solve for valid p,q values in the relatively prime equation
				// pa = qb + 1
				val euclid = extended_euclid(leftConstant,rightConstantN)
				
				A = euclid(0)*leftConstant	//p*leftConstant
			}
			val Nright = rightConstant

			// NOTE: qUsed = (N1-q1)modN1 and (N2-q2)modN2
			// Getting q1, _q2, _q3 instead of q1, q2, q3
        	// A1 = q1*Nright1 + 1
        	// A2 = _q2*Nright2 + 1
        	// B2 = _q3*Nleft2 + 1
			while (coprimeFactors(count) == 1){
				count = count + 1
			}
			// Skip where coprime factor is 1

			val coprimeFactor:Int = coprimeFactors(count)
			val temp:Int = (coprimeFactor-(A-1)/Nright).toInt
			// Scala bug (?) % returns remainder instead of modulus. For modulus to be >=0 when temp is negative
			if (temp >=0){
				qOut(count) = temp%coprimeFactor
			}
			else{
				qOut(count) = coprimeFactor+(temp%coprimeFactor)		// modulo returns negative; need to re-normalize
			}

			count = count + 1

		}
		return qOut
	}

	def dec2Xary(maxVal:Int,numCounters:Int,counterMax:Int): Array[Array[Int]] = {


		println(maxVal + "," + numCounters + "," + counterMax)
		// Count values to keep track of (values represent Xary equivalent)		
		var counts = new Array[Int](numCounters)
		// Generalized: whether all of the counters following the current counter are maxed out (to the right)
		var nextCountersMaxTF:Array[Boolean] = if (numCounters > 1) new Array[Boolean](numCounters-1) else Array()
		var out = Array.ofDim[Int](maxVal,numCounters)		
		// Fill in Xary array		
		for (i <- 1 to maxVal-1){
			// Handle smallest counter separately -- always changes			
			if (counts(numCounters-1) != counterMax){
				counts(numCounters-1) = counts(numCounters-1)+1
				if (numCounters > 1) nextCountersMaxTF(numCounters-2) = false
			}
			else{
				counts(numCounters-1) = 0
				nextCountersMaxTF(numCounters-2) = true		// Note that "software" timing is different from hardware
															// i.e. (registered) hardware checks before change, 
															// software checks after
			}
			for (j <- numCounters-2 to 0 by -1){
				// Increment current counter if previous counters are maxed and current counter isn't maxed
				if (counts(j) != counterMax && nextCountersMaxTF(j) == true){
					counts(j) = counts(j)+1
					if (j != 0){
						nextCountersMaxTF(j-1) = false
					}
				}
				else{
					// If next counters are maxed but first condition isn't met, then
					// the current counter needs to be reset and in SOFTWARE, indicate 
					// current counter was just maxed
					if (nextCountersMaxTF(j) == true){			
						counts(j) = 0
						if (j != 0){						// to not run into out of bounds error
							nextCountersMaxTF(j-1) = true
						}
					}
					// Otherwise, propagate falseness indicating not all of the subsequent counters
					// were maxed out
					else{
						if (j != 0){
							nextCountersMaxTF(j-1) = false
						}
					}
				}
			}
			out(i) = counts.reverse 						// smallest counter first
		}
		return out
	}
}
