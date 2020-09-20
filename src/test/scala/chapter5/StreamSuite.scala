package chapter5

import org.scalatest.funsuite.AnyFunSuite

import Stream._

class StreamSuite extends AnyFunSuite {

  def square(x: Int): Int = x * x

  test("toList: should evaluate each element of a stream and store in a list") {
    val stream: Stream[Int] = cons(square(1), cons(square(2), cons(square(3), empty)))
    assert(stream.toList === List(1, 4, 9))
  }

  test("take: take(n) should return first n elements of stream") {
    val stream: Stream[Int] = cons(square(1), cons(square(2), cons(square(3), empty)))
    assert(stream.take(2).toList === List(1, 4))
  }

  test("takeViaUnfold: take(n) should return first n elements of stream") {
    val stream: Stream[Int] = cons(square(1), cons(square(2), cons(square(3), empty)))
    assert(stream.takeViaUnfold(2).toList === List(1, 4))
  }

  test("drop: drop(n) should drop first n elements of stream") {
    val stream: Stream[Int] = cons(square(1), cons(square(2), cons(square(3), empty)))
    assert(stream.drop(2).toList === List(9))
  }

  test("forAll: should return true if all elements satisfy predicate") {
    val stream: Stream[Int] = cons(2, cons(4, cons(6, empty)))
    assert(stream.forAll(a => a % 2 == 0) === true)
  }

  test("forAll: should terminate early if element does not satisfy predicate") {
    val stream: Stream[Int] = cons(2, cons(3, cons(6, empty)))
    assert(stream.forAll(a => a % 2 != 0) === false)
  }

  test("takeWhile: should return elements up until predicate is no longer satisfied") {
    val stream: Stream[Int] = cons(2, cons(4, cons(5, empty)))
    assert(stream.takeWhile(a => a % 2 == 0).toList === List(2, 4))
  }

  test("takeWhileViaUnfold: should return elements up until predicate is no longer satisfied") {
    val stream: Stream[Int] = cons(2, cons(4, cons(5, empty)))
    assert(stream.takeWhileViaUnfold(a => a % 2 == 0).toList === List(2, 4))
  }

  test("map: should apply f to elements of stream") {
    val stream: Stream[Int] = cons(2, cons(3, cons(4, empty)))
    assert(stream.map(a => a * a).toList === List(4, 9, 16))
  }

  test("mapViaUnfold: should apply f to elements of stream") {
    val stream: Stream[Int] = cons(2, cons(3, cons(4, empty)))
    assert(stream.mapViaUnfold(a => a * a).toList === List(4, 9, 16))
  }

  test("filter: should return stream containing elements that satisfy the predicate") {
    val stream: Stream[Int] = cons(2, cons(3, cons(4, empty)))
    assert(stream.filter(a => a % 2 == 0).toList === List(2, 4))
  }

  test("append: should append s1 and s2") {
    val s1: Stream[Int] = cons(2, cons(3, cons(4, empty)))
    val s2: Stream[Int] = cons(5, cons(6, cons(7, empty)))
    assert(s1.append(s2).toList === List(2, 3, 4, 5, 6, 7))
  }

  test("constant: should return a stream of given A") {
    assert(constant(5).take(5).toList === List(5, 5, 5, 5, 5))
  }
  
  test("constantViaUnfold: should return a stream of given A") {
    assert(constantViaUnfold(5).take(5).toList === List(5, 5, 5, 5, 5))
  }

  test("from: should return a stream of consecutive integers starting from n") {
    assert(from(10).take(5).toList === List(10, 11, 12, 13, 14))
  }

  test("fromViaUnfold: should return a stream of consecutive integers starting from n") {
    assert(fromViaUnfold(10).take(5).toList === List(10, 11, 12, 13, 14))
  }

  test("fibs: should return a stream of fibonacci numbers") {
    assert(fibs.take(10).toList === List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34))
  }

  test("fibsViaUnfold: should return a stream of fibonacci numbers") {
    assert(fibsViaUnfold.take(10).toList === List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34))
  }

  test("zipWith: should return a stream of fibonacci numbers") {
    assert(fibsViaUnfold.take(10).toList === List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34))
  }

  test("zipWith: should add corresponding elements of two lists") {
    val s1: Stream[Int] = cons(1, cons(2, cons(3, cons(4, empty))))
    val s2: Stream[Int] = cons(4, cons(5, cons(6, empty)))
    assert(s1.zipWith(s2)((a, b) => a + b).take(4).toList === List(5, 7, 9))
  }

  test("startsWith: should return true if s2 is prefix of s1") {
    val s1: Stream[Int] = cons(1, cons(2, cons(3, empty)))
    val s2: Stream[Int] = cons(1, cons(2, empty))
    assert(s1.startsWith(s2) === true)
  }

  test("startsWith: should return false if s2 is not prefix of s1") {
    val s1: Stream[Int] = cons(1, cons(2, cons(3, empty)))
    val s2: Stream[Int] = cons(1, cons(2, cons(3, cons(4, empty))))
    assert(s1.startsWith(s2) === false)
  }

  test("tails: should return stream of suffics") {
    val s1: Stream[Int] = cons(1, cons(2, cons(3, empty)))
    assert(s1.tails.toList.map(_.toList) === List(List(1, 2, 3), List(2, 3), List(3)))
  }

  test("scanRight: should return stream of intermediate results") {
    val s1: Stream[Int] = cons(1, cons(2, cons(3, empty)))
    assert(s1.scanRight(0)(_ + _).toList === List(6, 5, 3, 0))
  }


}