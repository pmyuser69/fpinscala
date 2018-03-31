package fpinscala.datastructures

import org.scalatest.{FunSuite, Matchers}

class TreeTest extends FunSuite with Matchers {

  //////////////
  // Exercise 25
  test("size") {
    Tree.size(Leaf(1)) shouldBe 1
    Tree.size(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) shouldBe 5
  }

  //////////////
  // Exercise 26
  test("maximum") {
    Tree.maximum(Leaf(1)) shouldBe 1
    Tree.maximum(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) shouldBe 3
  }

  //////////////
  // Exercise 27

  test("depth") {
    Tree.depth(Leaf(1)) shouldBe 0
    Tree.depth(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) shouldBe 2
  }

  //////////////
  // Exercise 28

  //////////////
  // Exercise 29
  test("sizeViaFold") {
    Tree.sizeViaFold(Leaf(1)) shouldBe 1
    Tree.sizeViaFold(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) shouldBe 5
  }

  test("maximumViaFold") {
    Tree.maximumViaFold(Leaf(1)) shouldBe 1
    Tree.maximumViaFold(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) shouldBe 3
  }

  test("depthViaFold") {
    Tree.depthViaFold(Leaf(1)) shouldBe 0
    Tree.depthViaFold(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) shouldBe 2
  }

}
