package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.parallelism.Par.Par
import fpinscala.state._
import fpinscala.testing.Prop._

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait Prop0 {
  self =>
  def check: Boolean

  def &&(p: Prop0): Prop0 = self && p
}

trait Prop1 {
  self =>
  def check: Either[(FailedCase, SuccessCount), SuccessCount]

  def &&(p: Prop1): Prop1 =
    new Prop1 {
      override def check = self.check match {
        case Right(_) => p.check
        case left@Left(e) => left
      }
    }
}

case class Prop(run: (TestCases, RNG) => Result) {

  def &&(p: Prop) = Prop {
    (n, rng) =>
      run(n, rng) match {
        case Passed | Proved => p.run(n, rng)
        case x => x
      }
  }

    def ||(p: Prop) = Prop {
      (n,rng) => run(n,rng) match {
        // In case of failure, run the other Proved
        case Falsified(msg, _) => p.tag(msg).run(n,rng)
        case x => x
      }
    }

  def tag(msg: String) = Prop { (tc, rng) =>
    run(tc, rng) match {
      case Falsified(e, c) => Falsified(msg + "\n" + e, c)
      case x => x
    }
  }
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    def isFalsified = false
  }

  case object Proved extends Result {
    def isFalsified = false
  }

  case class Falsified(failure: FailedCase,
                       successes: SuccessCount) extends Result {
    def isFalsified = true
  }

  /* Produce an infinite random stream from a `Gen` and a starting `RNG`. */
  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n, rng) =>
      randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
        case (a, i) => try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch {
          case e: Exception => Falsified(buildMsg(a, e), i)
        }
      }.find(_.isFalsified).getOrElse(Passed)
  }

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
}

object ListProps {
  // Exercise 8.14: Prop for List.sorted
  lazy val intListGen: Gen[List[Int]] = ???
  lazy val sortedProp: Prop =
    Prop.forAll(intListGen) { l: List[Int] =>
      ???
    }

  // Exercise 8.14: Prop for List.takeWhile
  lazy val takeWhileProp: Prop = {
    val f = (_: Int) <= 0
    val p1 = Prop.forAll(intListGen) { l: List[Int] =>
      l.takeWhile(f).forall(f) == true
    }
    val p2: Prop = ???
    p1 && p2
  }
}

object Gen {
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  def boolean: Gen[Boolean] =
    Gen(State(RNG.boolean))

  def double: Gen[Double] =
    Gen(State(RNG.double))

  // here is an example on how to combine generators in a for-comprehension
  def option[A](gen: Gen[A]): Gen[Option[A]] =
    for {
      b <- Gen.boolean
      a <- gen
    } yield if (b) Some(a) else None

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def stringN(n: Int): Gen[String] = listOfN(n, choose(0, 127)).map(_.map(_.toChar).mkString)

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if (b) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = ???

  def listOf[A](g: Gen[A]): SGen[List[A]] = ???

  def listOf1[A](g: Gen[A]): SGen[List[A]] = ???

  lazy val parInt: Gen[Par[Int]] = ???
}

case class Gen[+A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] =
    Gen(sample.map2(g.sample)(f))

  def listOfN(size: Int): Gen[List[A]] = Gen.listOfN(size, this)

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size flatMap (Gen.listOfN(_, this))

  def listOf: SGen[List[A]] = Gen.listOf(this)

  def listOf1: SGen[List[A]] = Gen.listOf1(this)

  def **[B](g: Gen[B]): Gen[(A, B)] =
    (this map2 g) ((_, _))

  def unsized: SGen[A] = ???
}

case class SGen[+A](forSize: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = ???

  def map[B](f: A => B): SGen[B] = ???

  def flatMap[B](f: A => SGen[B]): SGen[B] = ???

  def **[B](s2: SGen[B]): SGen[(A, B)] = ???
}
