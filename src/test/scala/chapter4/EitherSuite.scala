package chapter4

import org.scalatest.funsuite.AnyFunSuite

import Either.{sequence, traverse}

class EitherSuite extends AnyFunSuite {

  test("map") {
    def square(x: Int): Int = x * x

    val a: Either[String, Int] = Right(4)
    assert(a.map(square) === Right(16))

    val b: Either[String, Int] = Left("Parsing error!")
    assert(b.map(square) === Left("Parsing error!"))
  }

  test("flatMap") {
    def divide(n: Int, d: Int): Either[String, Int] =
      if (d == 0) Left("division by zero") else Right(n / d) 

    val a: Either[String, Int] = Right(4)
    assert(a.flatMap(x => divide(x, 2)) === Right(2))

    val b: Either[String, Int] = Right(4)
    assert(b.flatMap(x => divide(x, 0)) === Left("division by zero"))

    val c: Either[String, Int] = Left("Parsing error!")
    assert(c.flatMap(x => divide(x, 2)) === Left("Parsing error!"))

    val d: Either[String, Int] = Left("Parsing error!")
    assert(d.flatMap(x => divide(x, 0)) === Left("Parsing error!"))
  }

  test("orElse") {
    val a: Either[String, Int] = Right(4)
    val b: Either[String, Int] = Left("Error!")

    assert(a.orElse(b) === a)
    assert(b.orElse(a) === a)
  }

  test("map2") {
    val ra: Either[String, Int] = Right(4)
    val la: Either[String, Int] = Left("a error!")
    val rb: Either[String, Int] = Right(10)
    val lb: Either[String, Int] = Left("b Error!")

    assert(ra.map2(rb)(_ + _) === Right(14))
    assert(ra.map2(lb)(_ + _) === lb)
    assert(la.map2(rb)(_ + _) === la)
    assert(la.map2(lb)(_ + _) === la)
  }

  test("sequence: should convert list of Either without errors to Right of list") {
    val l: List[Either[String, Int]] = List(
      Right(1),
      Right(2),
      Right(3),
    )
    assert(sequence(l) === Right(List(1, 2, 3)))
  }

  test("sequence: should return the first error encountered") {
    val l: List[Either[String, Int]] = List(
      Right(1),
      Left("first error"),
      Right(3),
      Left("second error")
    )
    assert(sequence(l) === Left("first error"))
  }

}
