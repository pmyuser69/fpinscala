package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  //////////////
  // Exercise 1

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  //////////////
  // Exercise 2

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("tail of empty list")
    case Cons(_, t) => t
  }

  //////////////
  // Exercise 3

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(h, t) => drop(t, n - 1)
    }
  }

  //////////////
  // Exercise 4

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t)(f)
    case _ => l
  }

  //////////////
  // Exercise 5

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("head of empty list")
    case Cons(_, t) => Cons(h, t)
  }

  //////////////
  // Exercise 6

  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("init of empty list")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  //////////////
  // Exercise 7

  //////////////
  // Exercise 8

  //////////////
  // Exercise 9

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, acc) => acc + 1)

  //////////////
  // Exercise 10

  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  //////////////
  // Exercise 11

  def sumWithFoldLeft(l: List[Int]): Int =
    foldLeft(l, 0)(_ + _)

  def productWithFoldLeft(l: List[Int]): Int =
    foldLeft(l, 1)(_ * _)

  def lengthWithFoldLeft[A](l: List[A]): Int =
    foldLeft(l, 0)((acc, _) => acc + 1)

  //////////////
  // Exercise 12

  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, Nil: List[A])((acc, a) => Cons(a, acc))

  //////////////
  // Exercise 13

  def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(l), z)((b: B, a: A) => f(a, b))
  }

  //////////////
  // Exercise 14

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] = {
    foldRight(l, r)((x, xs) => Cons(x, xs))
  }

  //////////////
  // Exercise 15

  def concat[A](l: List[List[A]]): List[A] = {
    foldRight(l, Nil: List[A])(append)
  }

  //////////////
  // Exercise 16

  def add1(l: List[Int]): List[Int] = {
    foldRight(l, Nil: List[Int])((a, b) => Cons(a + 1, b))
  }

  //////////////
  // Exercise 17

  def doubleToString(l: List[Double]): List[String] = {
    foldRight(l, Nil: List[String])((a, b) => Cons(a.toString, b))
  }

  //////////////
  // Exercise 18

  def map[A, B](l: List[A])(f: A => B): List[B] = {
    foldRight(l, Nil: List[B])((h, t) => Cons(f(h), t))
  }

  //////////////
  // Exercise 19

  def filter[A](l: List[A])(p: A => Boolean): List[A] = {
    foldRight(l, Nil: List[A])((h, t) => if (p(h)) Cons(h, t) else t)
  }

  //////////////
  // Exercise 20

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = {
    concat(map(l)(f))
  }

  //////////////
  // Exercise 21

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] = {
    flatMap(l)(a => if (f(a)) List(a) else Nil)
  }

  //////////////
  // Exercise 22

  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairwise(t1, t2))
  }

  //////////////
  // Exercise 23

  def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  //////////////
  // Exercise 24

  @tailrec
  def startWith[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match {
    case (_, Nil) => true
    case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => startWith(t1, t2)
    case _ => false
  }

  @tailrec
  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = l match {
    case Nil => sub == Nil
    case _ if startWith(l, sub) => true
    case Cons(h, t) => hasSubsequence(t, sub)
  }
}
