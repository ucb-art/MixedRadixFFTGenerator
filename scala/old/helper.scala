// Helper functions
// June 4, 2015

package FFT
import scala.math._

object Helper{

	// Custom value -> bitwidth needed to support value function
	def bitWidth(num: Int) : Int = {
		val temp = log(num)/log(2)
		val up = ceil(temp)
		if (temp < up){
			return up.toInt
		}
		else{
			return temp.toInt + 1
		}
	}

}
