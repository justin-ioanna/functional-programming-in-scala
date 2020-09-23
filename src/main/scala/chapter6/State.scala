package chapter6

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object State {

  type Rand[+A] = RNG => (A, RNG)

  /**
   * Exercise 6.1
   */
  def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
    case (n, nextRNG) if n < 0 => (math.abs(n + 1), nextRNG)
    case (n, nextRNG)          => (n, nextRNG)
  }

  /**
   * Exercise 6.2
   */
  def double(rng: RNG): (Double, RNG) = {
    val (n, nextRNG) = nonNegativeInt(rng)
    (n / Int.MaxValue.toDouble + 1, nextRNG)
  }

  /**
   * Exercise 6.3
   */
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng1) = nonNegativeInt(rng)
    val (d, rng2) = double(rng1)
    ((i, d), rng2)
  }

  /**
   * Exercise 6.3
   */
  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, rng1) = double(rng)
    val (i, rng2) = nonNegativeInt(rng1)
    ((d, i), rng2)
  }

  /**
   * Exercise 6.3
   */
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1, d2, d3), rng3)
  }

  /**
   * Exercise 6.4
   */
  def ints(counts: Int)(rng: RNG): (List[Int], RNG) = {
    val buffer = new collection.mutable.ListBuffer[A]
    @annotation.tailrec
    def loop(count: Int, rng: RNG): (List[Int], RNG) =
      if (count <= 0) (buffer.toList, rng)
      else {
        val (n, nextRNG) = rng.nextInt
        buffer += n
        loop(count - 1, nextRNG)
      }
    loop(counts, rng)
  }

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = 
    rng => {                                            
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  /**
   * Exercise 6.5
   */
  def doubleViaMap: Rand[Double] = map(nonNegativeInt)(x => x / (Int.MaxValue.toDouble + 1))

}