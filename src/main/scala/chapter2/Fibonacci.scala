package chapter2

object Fibonacci {

  /**
   * Exercise 2.1
   */
  def fibonacciNumber(n: Int): Int = {
    @annotation.tailrec
    def loop(current: Int, next: Int, count: Int): Int =
      if (count == n) current
      else loop(next, current + next, count + 1)
    loop(0, 1, 1)
  }

}
