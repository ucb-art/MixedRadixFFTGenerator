// May 28, 2015

package FFT
import scala.math._

object fftSizes{
	
	var fftSizeArray: Array[Int] = Array.empty[Int]
	var count:Int = 0											// # of FFT sizes
	var neededPrimes: Array[Int] = Array.empty[Int]

	def generate(customFFTSizes: Array[Int], standardRadioFFTSizesTF: Boolean, SCFDMAFFTSizesTF: Boolean ) : Unit = {
	
		//  For SC-FDMA, M{PUSCH,RB}=2^a*3^b*5^c<=110
    	//  M{PUSCH,SC}=M{PUSCH,RB}*N{RB,SC} where N{RB,SC}=12
		val max235SCFDMA:Int = 110
		val NRBSC:Int = 12
		var num2:Int = ceil(log(max235SCFDMA)/log(2)).toInt		// max a
		var num3:Int = ceil(log(max235SCFDMA)/log(3)).toInt		// max b
		var num5:Int = ceil(log(max235SCFDMA)/log(5)).toInt		// max c

		// go through all possible combinations of M{PUSCH,RB} and then some
		var MPUSCHRB: Array[Int] = Array.empty[Int]				
		for ( i <- 0 to num5+1;
			  j <- 0 to num3+1;
			  k <- 0 to num2+1){
			MPUSCHRB = MPUSCHRB :+ (pow(5,i)*pow(3,j)*pow(2,k)).toInt	// append to array
		}
	

// DEBUG
/*
		val maxN:Int = 13000 //40000
		num2 = ceil(log(maxN)/log(2)).toInt		// max a
		num3 = ceil(log(maxN)/log(3)).toInt		// max b
		num5 = ceil(log(maxN)/log(5)).toInt		// max c

		// go through all possible combinations 
		var FULL: Array[Int] = Array.empty[Int]				
		for ( i <- 0 to num5+1;
			  j <- 0 to num3+1;
			  k <- 0 to num2+1){
			FULL = FULL :+ (pow(5,i)*pow(3,j)*pow(2,k)).toInt	// append to array
		}

		FULL = FULL.sorted
		FULL = FULL.dropRight(FULL.length-FULL.indexWhere(_ > maxN))
		fftSizeArray = fftSizeArray ++ FULL.map(_*1)
		fftSizeArray = fftSizeArray ++ Array(16384,32768)
		fftSizeArray = (fftSizeArray diff Array(13122,17496,19683,21870,39366))
*/
// END DEBUG

		
		// get rid of >110
		MPUSCHRB = MPUSCHRB.sorted
		MPUSCHRB = MPUSCHRB.dropRight(MPUSCHRB.length-MPUSCHRB.indexWhere(_ > max235SCFDMA))			
		
		// If SCFDMA sizes should be generated
		if (SCFDMAFFTSizesTF){
			fftSizeArray = fftSizeArray ++ MPUSCHRB.map(_*NRBSC)
		}  

		// If standard Wifi/LTE sizes should be generated
		if (standardRadioFFTSizesTF){
			val standardRadioFFTSizes: Array[Int] = Array(1536, 64, 128, 256, 512, 1024, 2048)
			fftSizeArray = fftSizeArray ++ standardRadioFFTSizes
		}

		// Add custom FFT sizes
		fftSizeArray = fftSizeArray ++ customFFTSizes

		fftSizeArray = fftSizeArray.sorted

		// If someone typed FFT sizes <=1, get rid of them
		if (fftSizeArray.nonEmpty){
			fftSizeArray = fftSizeArray.drop(fftSizeArray.indexWhere(_ >= 2))
		}
		// Get rid of duplicates
		fftSizeArray = fftSizeArray.distinct

		count = fftSizeArray.length

		// Needed primes
		if (SCFDMAFFTSizesTF){
			neededPrimes = neededPrimes ++ Array(2,3,5)
		}
		if (standardRadioFFTSizesTF){
			neededPrimes = neededPrimes ++ Array(2,3)
		}
		
	}
}




