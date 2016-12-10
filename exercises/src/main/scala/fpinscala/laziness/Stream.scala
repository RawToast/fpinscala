package fpinscala.laziness

import scala.annotation.tailrec

sealed trait Stream[+A] {

  import Stream.{cons, empty, unfold}

  /*
  * The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument
  * by name and may choose not to evaluate it.
  */
  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream.
  // If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = {
    this match {
      case Empty => List.empty[A]
      case Cons(h, t) => List(h()) ++ t().toList
    }
  }

  def take(i: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) => if (i == 0) Empty else Cons(h, () => t().take(i - 1))
  }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this,n)) {
      case (Cons(h, _), i) if i == 1 => Some((h(), (empty, i-1)))
      case (Cons(h, t), i) if i > 0 => Some((h(), (t(), i-1)))
      case _ => None
    }

  @tailrec
  final def drop(n: Int): Stream[A] =
    if (n == 0) this
    else this match {
      case Empty => Empty
      case Cons(_, t) => t().drop(n - 1)
    }

  def takeWhile(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, as) =>
      if (p(a)) cons(a, as) else empty[A])

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) =>
      if (p(h())) Option(h() -> t().takeWhileViaUnfold(p)) else None
    case _ => None
  }

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) =>
      if (p(h())) Option(h() -> t().takeWhileViaFoldRight(p)) else None
    case _ => None
  }

  def headOption: Option[A] = foldRight(None: Option[A])((a, _) => Some(a))

  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((a, b) => cons(f(a), b))

  def mapViaUnfold[B](f: A => B): Stream[B] = unfold[B, Stream[A]](this) {
    case Cons(h, t) => Option(f(h()) -> t())
    case _ => None
  }

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A]) { (a, b) =>
      if (f(a)) cons(a, b) else b
    }

  def append[B >: A](other: Stream[B]): Stream[B] =
    foldRight(other)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, b) => f(a).append(b))

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] = unfold(this -> s2) {
    case (Empty, _) => None
    case (_, Empty) => None
    case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()) -> (t1(), t2()))
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = unfold(this -> s2) {
    case (Empty, Empty) => None
    case (Empty, Cons(h2, t2)) => Option(None -> Some(h2()), empty[A] -> t2())
    case (Cons(h1, t1), Empty) => Option((Some(h1()) -> None) -> (t1() -> empty[B]))
    case (Cons(h1, t1), Cons(h2, t2)) => Option(Some(h1()) -> Some(h2()), t1() -> t2())
  }

  def startsWith[B](s: Stream[B]): Boolean =
    zipAll(s).takeWhile { case (_, b) => b.nonEmpty }.forAll { case (a, b) => a == b }

  def tails: Stream[Stream[A]] = unfold(this) {
    case Cons(h, t) => Option(Cons(h, t), t())
    case _ => None
  }.append(Stream(empty))

  def scanRight[B](s: B)(f: (A, B) => B): Stream[B] =
    tails.map(a => a.foldRight(s)((a, b) => f(a, b)))
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  lazy val fibs: Stream[Int] = {
    def fibonacci(a: Int, b: Int): Stream[Int] = Stream.cons(a, fibonacci(b, a + b))

    fibonacci(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((value, state)) => Stream.cons(value, unfold(state)(f))
      case _ => empty[A]
    }
  }

  lazy val fibsViaUnfold: Stream[Int] = unfold(0 -> 1)(i => Some(i._1, i._2 -> (i._1 + i._2)))

  def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(i => Some(i, i + 1))

  def constantViaUnfold[A](a: A): Stream[A] = unfold(a)(a => Some(a, a))

  lazy val onesViaUnfold: Stream[Int] = constantViaUnfold(1)
}

