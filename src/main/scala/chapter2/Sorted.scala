package chapter2

object Sorted {

  /**
   * Exercise 2.2
   */
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(as: Array[A]): Boolean =
      if (as.length < 2) true
      else if (! ordered(as(0), as(1))) false
      else loop(as.tail)
    loop(as)
  }

}
