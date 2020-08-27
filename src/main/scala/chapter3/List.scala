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

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  /**
   * Exercise 3.9
   */
  def length[A](as: List[A]): Int = foldRight(as, 0)({ case (_, acc) => acc + 1 })
    
  /**
   * Exercise 3.10
   */
  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  /**
   * Exercise 3.11
   */
  def sumLeft(as: List[Int]): Int = foldLeft(as, 0)(_ + _)

  /**
   * Exercise 3.11
   */
  def productLeft(as: List[Double]): Double = foldLeft(as, 1.0)(_ * _)

  /**
   * Exercise 3.11
   */
  def lengthLeft[A](as: List[A]): Int = foldLeft(as, 0)({ case (acc, _) => acc + 1 })

  /**
   * Exercise 3.12
   */
  def reverseLeft[A](as: List[A]): List[A] = foldLeft(as, Nil: List[A])({ case (acc, x) => Cons(x, acc) })

  def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(reverse(l), z)((b,a) => f(a,b))

  /**
   * Exercise 3.14
   */
  def append[A](l1: List[A], l2: List[A]): List[A] = foldRightViaFoldLeft(l1, l2)(Cons(_, _))

  /**
   * Exercise 3.15
   */
  def concatenate[A](l: List[List[A]]): List[A] = foldRightViaFoldLeft(l, Nil: List[A])({ case (x, acc) => append(x, acc) })

  /**
   * Exercise 3.16
   */
  def add1(l: List[Int]): List[Int] = foldRightViaFoldLeft(l, Nil: List[Int])({ case (x, acc) => Cons(x + 1, acc) })

  /**
   * Exercise 3.17
   */
  def doubleToString(l: List[Double]): List[String] = foldRightViaFoldLeft(l, Nil: List[String])({ case (x, acc) => Cons(x.toString, acc) })

  /**
   * Exercise 3.18
   */
  def map[A,B](l: List[A])(f: A => B): List[B] = foldRightViaFoldLeft(l, Nil: List[B])({ case (x, acc) => Cons(f(x), acc) })

  /**
   * Exercise 3.19
   */
  def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRightViaFoldLeft(as, Nil: List[A])({ case (x, acc) => if (f(x)) Cons(x, acc) else acc })

  /**
   * Exercise 3.20
   */
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = concatenate(map(as)(f))

  /**
   * Exercise 3.21
   */
  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(x => if (f(x)) List(x) else Nil)

  /**
   * Exercise 3.22
   */
  def zipAdd(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(a, as), Cons(b, bs)) => Cons(a + b, zipAdd(as, bs))
  }

  /**
   * Exercise 3.23
   */
  def zipWith[A,B,C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = (l1, l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(a, as), Cons(b, bs)) => Cons(f(a, b), zipWith(as, bs)(f))
  }

}