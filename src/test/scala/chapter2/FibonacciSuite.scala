package chapter2

import org.scalatest.funsuite.AnyFunSuite

import Fibonacci._

class FibonacciSuite extends AnyFunSuite {

  test("fibonacciNumber: first Fibonacci number is 0.") {
    assert(fibonacciNumber(1) === 0)
  }

  test("fibonacciNumber: second Fibonacci number is 1.") {
    assert(fibonacciNumber(2) === 1)
  }

  test("fibonacciNumber: fifth Fibonacci number is 3.") {
    assert(fibonacciNumber(5) === 3)
  }

  test("fibonacciNumber: tenth Fibonacci number is 34.") {
    assert(fibonacciNumber(10) === 34)
  }

}
