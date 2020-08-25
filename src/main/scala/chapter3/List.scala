package chapter3

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  /**
   * Exercise 3.2
   */
  def tail[A](x: List[A]) = x match {
    case Cons(head, tail) => tail
    case Nil => throw new NoSuchElementException
  }

  /**
   * Exercise 3.3
   */
  def setHead[A](x: List[A], h: A) = x match {
    case Cons(head, tail) => Cons(h, tail)
    case Nil => throw new NoSuchElementException
  }

 /**
   * Exercise 3.4
   */
  def drop[A](x: List[A], n: Int): List[A] =
    if (n < 1) x
    else x match {
      case Cons(head, tail) => drop(tail, n - 1)
      case Nil => Nil
    }

  /**
   * Exercise 3.5
   */
  def dropWhile[A](x: List[A])(f: A => Boolean): List[A] = x match {
    case Cons(head, tail) if f(head) => dropWhile(tail)(f)
    case _ => x
  }

  def reverse[A](x: List[A]): List[A] = {
    @annotation.tailrec
    def loop(acc: List[A], x: List[A]): List[A] = x match {
      case Cons(head, tail) => loop(Cons(head, acc), tail)
      case Nil => acc
    }
    loop(Nil, x)
  }

  /**
   * Exercise 3.6
   */
  def init[A](x: List[A]): List[A] = {
    @annotation.tailrec
    def loop(acc: List[A], x: List[A]): List[A] = x match {
      case Cons(head, Nil) => reverse(acc)
      case Cons(head, tail) => loop(Cons(head, acc), tail)
      case Nil => Nil
    }
    loop(Nil, x)
  }

}