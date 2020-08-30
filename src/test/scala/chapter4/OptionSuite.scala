package chapter4

import org.scalatest.funsuite.AnyFunSuite

import Option._

class OptionSuite extends AnyFunSuite {

  test("map: should apply function to element contained in Some") {
    assert(Some(4).map(x => x * x) === Some(16))
    val a: Option[Int] = None
    assert(a.map(x => x * x) === None)
  }

  test("getOrElse: should return value in Some if Some, else default") {
    assert(Some(4).getOrElse(10) === 4)
    val a: Option[Int] = None
    assert(a.getOrElse(10) === 10)
  }

  test("flatMap: should apply function to element contained in Some then flatten") {

    def divide(n: Int, d: Int): Option[Int] = if (d == 0) None else Some(n / d) 

    assert(Some(4).flatMap(x => divide(x, 0)) === None)
    assert(Some(4).flatMap(x => divide(x, 1)) === Some(4))

    val a: Option[Int] = None
    assert(a.flatMap(x => divide(x, 1)) === None)
  }

  test("orElse: should return first Option if defined, otherwise return second") {

    assert(Some(4).orElse(Some(10)) === Some(4))

    val a: Option[Int] = None
    assert(a.orElse(Some(10)) === Some(10))
  }

  test("filter: should return Some(a) if a satifies predicate otherwise None") {

    assert(Some(4).filter(x => x % 2 == 0) === Some(4))
    assert(Some(4).filter(x => x % 2 != 0) === None)
    val a: Option[Int] = None
    assert(a.filter(x => x % 2 != 0) === None)
  }

  test("sequence: should convert list of option into option list") {
    assert(sequence(List(Some(1), Some(2), Some(3))) === Some(List(1, 2, 3)))
    assert(sequence(List(Some(1), None, Some(3))) === None)
  }

}