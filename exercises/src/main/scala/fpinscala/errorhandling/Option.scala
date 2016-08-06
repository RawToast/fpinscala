package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _}

// hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(x) => Some(f(x))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(x) => x
    case None => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f).getOrElse(None)

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    if (this == None) ob else this

  def filter(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) this else None)
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch {
      case e: Exception => 43
    } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch {
      case e: Exception => 43
    }
  }

  def variance(xs: Seq[Double]): Option[Double] = {
    /**
      * Implement the variance function in terms of flatMap.
      * If the mean of a sequence is m, the variance is the mean of math.pow(x - m, 2)
      * for each element x in the sequence.
      */
    def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

    mean(xs)
      .flatMap(m =>
        mean(xs.map(x => math.pow(x - m, 2))))
  }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(x =>
      b.flatMap(y => Some(f(x, y))))

  def sequence[A](as: List[Option[A]]): Option[List[A]] =
//    as.foldRight(Some(List.empty): Option[List[A]]) {
//      (oa: Option[A], as: Option[List[A]]) => map2(as, oa)((x: List[A], y: A) => y :: x)
//    }
    traverse(as)((a: Option[A]) => a)

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as.foldRight(Some(List.empty): Option[List[B]]) {
      (a, obs) => map2(f(a), obs)((x: B, y) => x :: y)
    }
}