package chapter3

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  /**
   * Exercise 3.25
   */
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  /**
   * Exercise 3.26
   */
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(value) => value
    case Branch(left, right) => maximum(left) max maximum(right)
  }

  /**
   * Exercise 3.27
   */
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(left, right) => 1 + (depth(right) max depth(left))
  }

  /**
   * Exercise 3.28
   */
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  /**
   * Exercise 3.29
   */
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(value) => f(value)
    case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
  }

  /**
   * Exercise 3.29
   */
  def sizeViaFold[A](t: Tree[A]): Int = fold(t)(x => 1)((l, r) => 1 + l + r)

  /**
   * Exercise 3.29
   */
  def maximumViaFold(t: Tree[Int]): Int = fold(t)(x => x)((l, r) => l max r)

  /**
   * Exercise 3.29
   */
  def depthViaFold[A](t: Tree[A]): Int = fold(t)(x => 0)((l, r) => 1 + (l max r))

  /**
   * Exercise 3.29
   */
  def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(x => Leaf(f(x)): Tree[B])((l, r) => Branch(l, r))

}