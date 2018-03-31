package fpinscala.datastructures

import org.scalatest.{FunSuite, Matchers}

class ListTest extends FunSuite with Matchers {

  //////////////
  // Exercise 1

  test("some tests") {
    List.x shouldBe 3
  }

  //////////////
  // Exercise 2

  test("tail") {
    List.tail(List(1)) shouldBe Nil
    List.tail(List(2, 1)) shouldBe List(1)
  }

  //////////////
  // Exercise 3

  test("drop") {
    List.drop(Cons(2, Cons(1, Nil)), 2) shouldBe Nil
  }

  //////////////
  // Exercise 4

  test("dropWhile") {
    List.dropWhile(Cons(2, Cons(1, Nil)))(_ > 0) shouldBe Nil
  }

  //////////////
  // Exercise 5

  test("setHead") {
    List.setHead(List(1), 2) shouldBe List(2)
  }

  //////////////
  // Exercise 6

  test("init") {
    an[RuntimeException] should be thrownBy List.init(Nil)
    List.init(List(2, 1)) should be(List(2))
    List.init(List(3, 2, 1)) should be(List(3, 2))
  }

  //////////////
  // Exercise 7

  //////////////
  // Exercise 8

  //////////////
  // Exercise 9

  test("length") {
    List.length(Nil) should be(0)
    List.length(List(2, 1)) should be(2)
    List.length(List(1, 2, 3)) shouldBe 3
  }

  test("foldRight") {
    List.foldRight(List('a', 'b', 'c'), "")(_ + _) should be("abc")
  }

  //////////////
  // Exercise 10

  test("foldLeft") {
    List.foldLeft(List('a', 'b', 'c'), "")(_ + _) should be("abc")
  }

  //////////////
  // Exercise 11

  test("lengthWithFoldLeft") {
    List.lengthWithFoldLeft(Nil) should be(0)
    List.lengthWithFoldLeft(List('a', 'b', 'c')) should be(3)
  }

  //////////////
  // Exercise 12

  test("reverse") {
    List.reverse(List('a', 'b', 'c')) should be(List('c', 'b', 'a'))
  }

  //////////////
  // Exercise 13

  test("foldLeftViaFoldRight") {
    List.foldRightViaFoldLeft(List('a', 'b', 'c'), "")(_ + _) should be("abc")
  }

  //////////////
  // Exercise 14

  test("appendViaFoldRight") {
    List.appendViaFoldRight(List(1, 2, 3), List(4, 5)) should be(List(1, 2, 3, 4, 5))
  }

  //////////////
  // Exercise 15

  test("concat") {
    List.concat(List(List(1, 2, 3), List(4, 5), List(6, 7))) should be(List(1, 2, 3, 4, 5, 6, 7))
  }

  //////////////
  // Exercise 16

  test("add1") {
    List.add1(List(1, 2, 3, 3, 2)) should be(List(2, 3, 4, 4, 3))
  }

  //////////////
  // Exercise 17

  test("doubleToString") {
    List.doubleToString(List(1.1, 2.8)) should be(List("1.1", "2.8"))
  }

  //////////////
  // Exercise 18

  //////////////
  // Exercise 19

  test("filter") {
    List.filter(List(1, 2, 3, 4))((i => (i % 2) == 0)) should be(List(2, 4))
  }

  //////////////
  // Exercise 20

  test("flatMap") {
    List.flatMap(List(1, 2))(i => List(i, i)) should be(List(1, 1, 2, 2))
  }

  //////////////
  // Exercise 21

  test("filterViaFlatMap") {
    List.filterViaFlatMap(List(1, 2, 3, 4))((i => (i % 2) == 0)) should be(List(2, 4))
  }

  //////////////
  // Exercise 22

  test("addPairwise") {
    List.addPairwise(List(1, 2, 3), List(4, 5, 6)) should be(List(5, 7, 9))
  }

  //////////////
  // Exercise 23

  //////////////
  // Exercise 24

  test("hasSubsequence") {
    List.hasSubsequence(Nil, Nil) should be(true)
    List.hasSubsequence(List(1, 2, 3), Nil) should be(true)
    List.hasSubsequence(List(1, 2, 3), List(1)) should be(true)
    List.hasSubsequence(List(1, 2, 3), List(1, 2)) should be(true)
    List.hasSubsequence(List(1, 2, 3), List(1, 2, 3)) should be(true)
    List.hasSubsequence(List(1, 2, 3), List(2)) should be(true)
    List.hasSubsequence(List(1, 2, 3), List(2, 3)) should be(true)
    List.hasSubsequence(List(1, 2, 3), List(3)) should be(true)

    List.hasSubsequence(List(1, 2, 3), List(1, 3)) should be(false)
    List.hasSubsequence(List(1, 2, 3), List(2, 1)) should be(false)
    List.hasSubsequence(Nil, List(1)) should be(false)
  }

}
