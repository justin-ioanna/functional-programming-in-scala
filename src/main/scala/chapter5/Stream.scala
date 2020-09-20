package chapter5

import Stream._

sealed trait Stream[+A] {

  def headOption: Option[A] = this match {
    case Cons(h, t) => Some(h())
    case Empty      => None
  }

  /**
   * Exercise 5.1
   */
  def toList: List[A] = {
    val buffer = new collection.mutable.ListBuffer[A]
    @annotation.tailrec
    def loop(s: Stream[A]): List[A] = s match {
      case Cons(head, tail) => {
        buffer += head()
        loop(tail())
      }
      case Empty => buffer.toList 
    }
    loop(this)
  }

  /**
   * Exercise 5.2
   */
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1  => cons(h(), t().take(n - 1))
    case Cons(h, t) if n == 1 => cons(h(), empty)
    case _                    => empty
  }

  /**
   * Exercise 5.2
   */
  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case _                   => this
  }


  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _          => z
  }

  def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  /**
   * Exercise 5.4
   */
  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  /**
   * Exercise 5.3 & 5.5
   */
  def takeWhile(p: A => Boolean): Stream[A] = foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else empty)

  /**
   * Exercise 5.6
   */
  def headOptionViaFoldRight: Option[A] = foldRight(None: Option[A])((head, _) => Some(head))

  /**
   * Exercise 5.7
   */
  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((a, b) => cons(f(a), b))
  
  /**
   * Exercise 5.7
   */
  def filter(p: A => Boolean): Stream[A] = foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else b)

  /**
   * Exercise 5.7
   */
  def append[B >: A](other: => Stream[B]): Stream[B] = foldRight(other)((a, b) => cons(a, b))

  /**
   * Exercise 5.7
   */
  def flapMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((a, b) => f(a).append(b))

  /**
   * Exercise 5.13
   */
  def mapViaUnfold[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(head, tail) => Some((f(head()), tail()))
    case _ => None
  }

  /**
   * Exercise 5.13
   */
  def takeViaUnfold(n: Int): Stream[A] = unfold((this, n)) {
    case (Cons(head, tail), x) if x > 1  => Some((head(), (tail(), n - 1)))
    case (Cons(head, tail), 1)           => Some((head(), (empty, n - 1)))
    case _                               => None
  }

  /**
   * Exercise 5.13
   */
  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(head, tail) if p(head()) => Some((head(), tail()))
    case _                             => None
  }

  /**
   * Exercise 5.13
   */
  def zipWith[B, C](other: Stream[B])(f: (A, B) => C): Stream[C] = unfold((this, other)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
    case _                            => None
  }

  /**
   * Exercise 5.13
   */
  def zipAll[B](other: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, other)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
    case (Cons(h1, t1), Empty)        => Some(((Some(h1()), None), (t1(), empty)))
    case (Empty, Cons(h2, t2))        => Some(((None, Some(h2())), (empty, t2())))
    case _                            => None
  }

  /**
   * Exercise 5.14
   */
  def startsWith[A](other: Stream[A]): Boolean =
    this.zipAll(other).takeWhile(!_._2.isEmpty).forAll({ case (a, b) => a == b })

  /**
   * Exercise 5.15
   */
  def tails: Stream[Stream[A]] = unfold(this) {
    case Cons(head, tail) => Some((Cons(head, tail), tail()))
    case _                => None
  }

  def hasSubsequence[A](s: Stream[A]): Boolean = tails.exists(_.startsWith(s))
  
  /**
   * Exercise 5.16
   */
  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, p0) => {
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, cons(b2, p1._2))
    })._2


}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  /**
   * Exercise 5.8
   */
  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  /**
   * Exercise 5.9
   */
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  /**
   * Exercise 5.10
   */
  def fibs: Stream[Int] = {
    def loop(a: Int, b: Int): Stream[Int] = cons(a, loop(b, a + b))
    loop(0, 1)
  }

  /**
   * Exercise 5.11
   */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case None         => empty
  }

  /**
   * Exercise 5.12
   */
  def fibsViaUnfold: Stream[Int] = unfold((0, 1))({ case (a, b) => Some(a, (b, a + b)) })

  /**
   * Exercise 5.12
   */
  def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(n => Some(n, n + 1))

  /**
   * Exercise 5.12
   */
  def constantViaUnfold[A](a: A): Stream[A] = unfold(a)(_ => Some(a, a))

  /**
   * Exercise 5.12
   */
  val onesViaUnfold: Stream[Int] = constantViaUnfold(1)

}