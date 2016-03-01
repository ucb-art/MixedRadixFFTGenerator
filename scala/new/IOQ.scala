package FFT

object IOQ {

  /** Calculates the Q needed for PFA + CTA DIT/DIF IO (in-place)
    * Inputs: # of FFT lengths and coprimes corresponding to the FFT sizes
    * Outputs: DIF Q, DIT Q
    */
  def apply(nCount: Int, coprimes:List[List[Int]]): Tuple2[List[List[Int]],List[List[Int]]] = {
    // N1,N2,N3 reversed between DIT/DIF
    val temp = (0 until nCount).map{ i => (Q(coprimes(i)),Q(coprimes(i).reverse))}
    temp.toList.unzip
  }

  /** Extended Euclidean Algorithm
    * ax + by = gcd(a,b)
    * Inputs: a,b
    * Outputs: gcd, x, y
    */
  def egcd(a: Int, b: Int): Tuple3[Int,Int,Int] = {
    if (a == 0) (b,0,1)
    else {
      val (gcd, y, x) = egcd(b % a, a)
      (gcd, x - (b/a) * y, y)
    }
  }

  /** Calculate Q values for N = N1*N2*N3*... where Nx are coprimes */
  def Q(coprimes: List[Int]): List[Int] ={
    // # of coprime decomposition equation sets is 1 less than # of coprimes
    (1 until coprimes.length).toList.map { i => {
      // Factor N into Na, Nb
      // where Na is a coprime and Nb is the product of the other coprimes
      val Na = coprimes(i-1)
      val Nb = coprimes.filterNot (_ == Na).product
      val (gcd,p,q) = egcd(Na,Nb)
      // CTA (A = 1) vs PFA
      // Normal A = p * Nx = q * L * R + 1 --> Q = q * L, where L = product of factors to the left of Na
      // and L * R = Nb
      val right = coprimes.drop(i).product
      val Q = -q * (if (gcd != 1) 0 else Nb/right)
      // Q' = (Na-Q)% Na
      val QPrime = (Na-Q)%Na

      if (Na == 1) 0                    // Skip if coprime slot unused
      else if (QPrime < 0) QPrime + Na  // Remainder can be negative; want only >= 0 modulus, so renormalize
      else QPrime
    }}

  }}