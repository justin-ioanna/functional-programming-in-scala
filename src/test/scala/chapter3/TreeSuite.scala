package chapter3

import org.scalatest.funsuite.AnyFunSuite

import Tree._

class TreeSuite extends AnyFunSuite {

  val tree: Tree[Int] = Branch(
    Branch(
      Leaf(1),
      Leaf(2)
    ),
    Branch(
      Branch(
        Leaf(8),
        Leaf(4)
      ),
      Leaf(3)
    )
  )

  val mapped: Tree[Int] = Branch(
    Branch(
      Leaf(2),
      Leaf(3)
    ),
    Branch(
      Branch(
        Leaf(9),
        Leaf(5)
      ),
      Leaf(4)
    )
  )

  test("size: should count branches and leaves") {
    assert(size(tree) == 9)
  }

  test("sizeViaFold: should count branches and leaves") {
    assert(sizeViaFold(tree) == 9)
  }

  test("maximum: should find max value in tree") {
    assert(maximum(tree) == 8)
  }

  test("maximumViaFold: should find max value in tree") {
    assert(maximumViaFold(tree) == 8)
  }

  test("depth: should find max path from root to leaf") {
    assert(depth(tree) == 3)
  }

  test("depthViaFold: should find max path from root to leaf") {
    assert(depthViaFold(tree) == 3)
  }

  test("map: should apply function to each leaf value") {
    assert(map(tree)(x => x + 1) == mapped)
  }

  test("mapViaFold: should apply function to each leaf value") {
    assert(mapViaFold(tree)(x => x + 1) == mapped)
  }

}
