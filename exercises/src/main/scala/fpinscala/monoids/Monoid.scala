package fpinscala.monoids

import fpinscala.parallelism.Nonblocking._
import fpinscala.parallelism.Nonblocking.Par.toParOps
import fpinscala.testing.{Gen, Prop}

import scala.Option
// infix syntax for `Par.map`, `Par.flatMap`, etc

trait Monoid[A] {
  def op(a1: A, a2: A): A

  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2

    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2

    val zero = Nil
  }

  lazy val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int) = a1 + a2

    override def zero = 0
  }

  lazy val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int) = a1 * a2

    override def zero = 1
  }

  lazy val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean) = a1 || a2

    override def zero = false
  }

  lazy val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean) = a1 && a2

    override def zero = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]) = (a1, a2) match {
      case (None, None) => None
      case (_, None) => a1
      case (None, _) => a2
      case _ => a1
    }

    override def zero = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[(A) => A] {
    override def op(a1: (A) => A, a2: (A) => A) = a1 andThen a2

    override def zero = (x: A) => x
  }

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
  // Associativity
    Prop.forAll(for {
      x <- gen
      y <- gen
      z <- gen
    } yield (x, y, z))(p =>
      m.op(p._1, m.op(p._2, p._3)) == m.op(m.op(p._1, p._2), p._3)) &&
      // Identity
      Prop.forAll(gen)((a: A) =>
        m.op(a, m.zero) == a && m.op(m.zero, a) == a)
  def trimMonoid(s: String): Monoid[String] = sys.error("todo")

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b, a) => m.op(f(a), b))

  // The function type `(A, B) => B`, when curried, is `A => (B => B)`.
  // And of course, `B => B` is a monoid for any `B` (via function composition).
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = {
    foldMap(as, endoMonoid[B])(f.curried)(z)
  }

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    ???

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    val (xs, ys) = as.splitAt(as.length / 2)

    lazy val l = xs.foldRight(m.zero)((a, b) => m.op(f(a), b))
    lazy val r = ys.foldRight(m.zero)((a, b) => m.op(f(a), b))
    m.op(l, r)
  }

  // Hard: Use foldMap to detect whether a given IndexedSeq[Int] is ordered.
  // You’ll need to come up with a creative Monoid.
  def ordered(ints: IndexedSeq[Int]): Boolean = {

    def intTruth = new Monoid[Option[(Int, Int, Boolean)]] {
      override def op(a1: Option[(Int, Int, Boolean)],
                      a2: Option[(Int, Int, Boolean)]): Option[(Int, Int, Boolean)] = {
        (a1, a2) match {
          case (Some((a1, b1, c1)), Some((a2, b2, c2))) =>
            val ord = (c1 && c2) && (a1 >= b2 && b1 >= b2) || (a2 >= b1 && b2 >= b1)
            Some(a1.min(a2), b1.max(b2), ord)
          case (x, None) => x
          case (None, x) => x
        }
      }

      override def zero = None
    }

    foldMap(ints.toList, intTruth)(a => Some(a, a, true)) match {
      case Some(a) => a._3
      case None => true
    }
  }

  sealed trait WC

  case class Stub(chars: String) extends WC

  case class Part(lStub: String, words: Int, rStub: String) extends WC

  def par[A](m: Monoid[A]): Monoid[Par[A]] = {
    new Monoid[Par[A]] {
      override def op(a1: Par[A], a2: Par[A]): Par[A] = {
        Par.map2(a1, a2)((a, b) => m.op(a, b))
      }

      override def zero: Par[A] = Par.unit(m.zero)
    }
  }

  def parFoldMap[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = {

    val pm: Monoid[Par[B]] = par(m)

    val (xs, ys) = as.splitAt(as.length / 2)

    lazy val l = xs.foldRight(pm.zero)((a: A, b: Par[B]) => pm.op(b, Par.unit(f(a))))
    lazy val r = ys.foldRight(pm.zero)((a, b) => pm.op(b, Par.unit(f(a))))
    pm.op(l, r)
  }

  lazy val wcMonoid: Monoid[WC] = new Monoid[WC] {
    override def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (w1: Part, w2: Part) =>
        val mid = w1.rStub + w2.lStub
        val midWords = if (mid.isEmpty) 0 else 1 + mid.count(p => p == ' ')

        Part(w1.lStub, w1.words + w2.words + midWords, w2.rStub)
      case (w1: Part, w2: Stub) =>

        val mid = w1.rStub + w2.chars
        val midWords = if (mid.isEmpty) 0 else 1 + mid.count(p => p == ' ')

        Part(w1.lStub, w1.words + midWords, "")
      case (w1: Stub, w2: Part) =>
        val mid = w2.lStub + w1.chars
        val midWords = if (mid.isEmpty) 0 else 1 + mid.count(p => p == ' ')

        Part("", w2.words + midWords, w2.rStub)
      case (w1: Stub, w2: Stub) => Stub(w1.chars + w2.chars)
    }

    override def zero = Stub("")
  }

  def countWords(s: String): Int = sys.error("todo")

  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    sys.error("todo")

  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] =
    sys.error("todo")

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] =
    sys.error("todo")

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    sys.error("todo")
}

trait Foldable[F[_]] {

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    sys.error("todo")

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    sys.error("todo")

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    sys.error("todo")

  def toList[A](as: F[A]): List[A] =
    sys.error("todo")
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")

  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")

  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")

  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")

  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")

  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")
}

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")

  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")

  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")

  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")

  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")
}

