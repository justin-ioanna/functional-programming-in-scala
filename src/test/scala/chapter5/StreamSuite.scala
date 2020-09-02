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

  test("map: should apply f to elements of stream") {
    val stream: Stream[Int] = cons(2, cons(3, cons(4, empty)))
    assert(stream.map(a => a * a).toList === List(4, 9, 16))
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

}