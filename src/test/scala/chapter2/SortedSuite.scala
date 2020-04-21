package chapter2

import org.scalatest.funsuite.AnyFunSuite

import Sorted._

class SortedSuite extends AnyFunSuite {

  def integerOrdering(a: Int, b: Int): Boolean = a <= b

  test("isSorted: array of sorted integers.") {
    assert(isSorted(Array(1, 2, 3, 4), integerOrdering) === true)
  }

  test("isSorted: array of unsorted integers.") {
    assert(isSorted(Array(1, 2, 4, 3), integerOrdering) === false)
  }

  def stringOrdering(a: String, b: String): Boolean = a.length <= b.length

  test("isSorted: array of sorted strings (by length).") {
    assert(isSorted(Array("a", "ab", "abc", "abcd"), stringOrdering) === true)
  }

  test("isSorted: array of unsorted strings (by length).") {
    assert(isSorted(Array("a", "ab", "abcd", "abc"), stringOrdering) === false)
  }

}

