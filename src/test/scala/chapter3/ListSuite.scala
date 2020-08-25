package chapter3

import org.scalatest.funsuite.AnyFunSuite

import List._

class ListSuite extends AnyFunSuite {

  test("tail: tail of non-empty list should return list without first element") {
    assert(tail(List(1, 2, 3, 4)) === List(2, 3, 4))
  }

  test("tail: tail of empty list should throw exception") {
    assertThrows[NoSuchElementException] { // Result type: Assertion
      tail(Nil)
    }
  }

  test("setHead: setHead on list should return new list with head replaced.") {
    assert(setHead(List(1, 2, 3, 4), 10) === List(10, 2, 3, 4))
  }

  test("setHead: setHead on empty list should throw exception") {
    assertThrows[NoSuchElementException] { // Result type: Assertion
      setHead(Nil, 10)
    }
  }

  test("drop: drop 2 on list of 5 elements should return list containing last 3 elements") {
    assert(drop(List(1, 2, 3, 4, 5), 2) == List(3, 4, 5))
  }

  test("drop: drop with n is 0 should return input list") {
    assert(drop(List(1, 2, 3, 4, 5), 0) == List(1, 2, 3, 4, 5))
  }

  test("drop: drop with negative n should return input list") {
    assert(drop(List(1, 2, 3, 4, 5), -2) == List(1, 2, 3, 4, 5))
  }

  test("drop: drop with n larger than number of elements in the list should return Nil") {
    assert(drop(List(1, 2, 3, 4, 5), 10) == Nil)
  }

  test("dropWhile: dropWhile should drop elements up to the point where the predicate is no longer satisfied.") {
    assert(dropWhile(List(1, 2, 3, 4, 5))(x => x < 3) == List(3, 4, 5))
  }

  test("dropWhile: dropWhile on list with elements that all satisfy the predicate should return Nil") {
    assert(dropWhile(List(1, 2, 3, 4, 5))(x => x < 6) == Nil)
  }

  test("reverse: reverse of List(1, 2, 3) should be List(3, 2, 1)") {
    assert(reverse(List(1, 2, 3)) === List(3, 2, 1))
  }

  test("reverse: reverse of empty list should be an empty list") {
    assert(reverse(List()) === List())
    assert(reverse(Nil) === Nil)
  }

  test("init: init on list should return list minus last element") {
    assert(init(List(1, 2, 3, 4, 5)) == List(1, 2, 3, 4))
  }

  test("init: init on empty list should return Nil") {
    assert(init(Nil) == Nil)
  }

  // test("length: length of 5 element list should be 5") {
  //   assert(length(List(1, 2, 3, 4, 5)) == 5)
  // }

  // test("foldLeft: sum elements of list with foldLeft") {
  //   assert(foldLeft(List(1, 2, 3, 4, 5), 0)(_ + _) == 15)
  // }

  // test("sumLeft: sum elements of list with foldLeft") {
  //   assert(sumLeft(List(1, 2, 3, 4, 5)) == 15)
  // }

  // test("productLeft: product elements of list with foldLeft") {
  //   assert(productLeft(List(1, 2, 3, 4, 5)) == 120)
  // }

  // test("lengthLeft: length of list with foldLeft") {
  //   assert(lengthLeft(List(1, 2, 3, 4, 5)) == 5)
  // }

  // test("reverse: reverse list using foldLeft") {
  //   assert(reverse(List(1, 2, 3, 4, 5)) == List(5, 4, 3, 2, 1))
  // }

  // test("reverse: reverse empty list should return empty list") {
  //   assert(reverse(List()) == List())
  // }

  // test("append: should add second list to end of first") {
  //   assert(append(List(1, 2, 3), List(4, 5, 6)) == List(1, 2, 3, 4, 5, 6))
  // }

  // test("concatenate: should concatenate a list of lists") {
  //   assert(concatenate(List(List(1, 2), List(3, 4), List(5, 6))) == List(1, 2, 3, 4, 5, 6))
  // }

  // test("add1: should add 1 to each element in the list") {
  //   assert(add1(List(1, 2, 3)) == List(2, 3, 4))
  // }

  // test("convertToString: should convert elements in list from Double to String") {
  //   assert(convertToString(List(1.0, 2.0, 3.0)) == List("1.0", "2.0", "3.0"))
  // }

  // test("map: should apply a function to each element in a list") {
  //   assert(map(List(1, 2, 3))(x => 2 * x) == List(2, 4, 6))
  // }

  // test("filter: should remove elements from list based on predicate") {
  //   assert(filter(List(5, 4, 1, 2, 3))(x => x > 2) == List(5, 4, 3))
  // }

  // test("flatmap: should map then flatten") {
  //   assert(flatMap(List(1, 2, 3))(x => List(x, x)) == List(1, 1, 2, 2, 3, 3))
  // }

  // test("filterViaFlatMap: should filter list") {
  //   assert(filterViaFlatMap(List(5, 4, 1, 2, 3))(x => x > 2) == List(5, 4, 3))
  // }

}