package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  } 
  
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  
  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  lazy val EmptyListError = sys.error("Empty list")

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42 
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101 
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  
  def sum2(ns: List[Int]) = 
    foldRight(ns, 0)((x,y) => x + y)
  
  def product2(ns: List[Double]) = 
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => EmptyListError
      case Cons(_, t) => t
    }

  def setHead[A](l: List[A], h: A): List[A] =
    l match {
    case Nil => EmptyListError
    case Cons(head, tail) => Cons(h, tail)
  }

  // 3.4
  @tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    if (n > 0) l match {
      case Nil => Nil
      case Cons(h, t) => drop(t, n - 1)
    } else l

  // 3.5
  @tailrec
  def dropWhile[A](l: List[A], filter: A => Boolean): List[A] =
    l match {
      case Cons(h, t) => if (filter(h)) dropWhile(t, filter) else l
      case _ => l
    }

  // 3.6
  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => EmptyListError
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }

  // 3.9
  def length[A](l: List[A]): Int = foldRight(l, 0)((_, total) => total + 1)

  //3.10
  @tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
    }

  def sumViaFoldLeft(nums: List[Int]): Int = foldLeft(nums, 0) { (x, y) => x + y }

  def productViaFoldLeft(nums: List[Double]): Double = foldLeft(nums, 1d)(_ * _)

  def lengthViaFoldLeft(l: List[_]): Int = foldRight(l, 0)((_, total) => total + 1)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A]())((tail, head) => Cons(head, tail))

  def appendViaFoldRight[A](l1: List[A], l2: List[A]): List[A] =
    foldRight(l1, l2)((itemL2: A, listL1: List[A]) => Cons(itemL2, listL1))

  def appendViaFoldLeft[A](a1: List[A], a2: List[A]): List[A] =
    foldLeft(reverse(a1), a2)((list: List[A], item: A) => Cons(item, list))

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil: List[A])((xs, ys) => append(xs, ys))

  def add1(nums: List[Int]): List[Int] =
    foldRight(nums, Nil: List[Int])((x, ys) => Cons(x + 1, ys))

  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])((x, ys) => Cons(x.toString, ys))

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B])((x, ys) => Cons(f(x), ys))

  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, Nil: List[A])((x, ys) => if (f(x)) Cons(x, ys) else ys)

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
    foldLeft(l, Nil: List[B])((ys, x) => append(ys, f(x)))

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(x => if (f(x)) List(x) else Nil)

  def addPairwise(listA: List[Int], listB: List[Int]): List[Int] =
    (listA, listB) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(a, as), Cons(b, bs)) => Cons(a + b, addPairwise(as, bs))
    }

  def zipWith[A,B,C](xs: List[A], ys: List[B])(f: (A,B) => C): List[C] =
    (xs, ys) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(xH, xT), Cons(yH, yT)) => Cons(f(xH, yH), zipWith(xT, yT)(f))
    }


  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean =
    (l, sub) match {
      case (Nil, _) => false
      case (_, Nil) => true
      case (Cons(xH, xT), y) =>
        if (y == xT || y == l) true
        else hasSubsequence(init(l), sub) || hasSubsequence(xT, y)
    }
}
