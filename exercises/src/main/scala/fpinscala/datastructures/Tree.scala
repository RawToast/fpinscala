package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size(tree: Tree[_]): Int =
    tree match {
      case (Leaf(value)) => 1
      case (Branch(left, right)) => 1 + size(left) + size(right)
    }

  def maximum(tree: Tree[Int]): Int =
    tree match {
      case (Leaf(value)) => value
      case (Branch(left, right)) => maximum(left).max(maximum(right))
    }

  def depth(tree: Tree[_]): Int =
    tree match {
      case (Leaf(value)) => 0
      case (Branch(left, right)) => 1 + depth(left).max(depth(right))
    }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    tree match {
      case (Leaf(value)) => Leaf(f(value))
      case (Branch(left, right)) => Branch(map(left)(f), map(right)(f))
    }

  def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B =
    tree match {
      case Leaf(v) => f(v)
      case Branch(l: Tree[A], r: Tree[A]) => g(fold(l)(f)(g), fold(r)(f)(g))
    }

  def sizeViaFold[A](t: Tree[A]): Int =
    fold(t)(a => 1)(1 + _ + _)

  def maximumViaFold[T](t: Tree[T])(implicit ev: Numeric[T]): T =
    fold(t)(a => a)((x: T, y: T) => ev.max(x, y))

  def depthViaFold[A](t: Tree[A]): Int =
    fold(t)(a => 0)((x:Int, y:Int) => 1 + x.max(y))
}