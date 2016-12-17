package fpinscala.state

import scala.annotation.tailrec


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {

  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed)
      // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val nxt = rng.nextInt

    if (nxt._1 >= 0) nxt
    else if (nxt._1 == Int.MinValue) (Int.MaxValue, nxt._2)
    else -nxt._1 -> nxt._2
  }

  def double(rng: RNG): (Double, RNG) = {
    //Write a function to generate a Double between 0 and 1, not including 1.
    //Note: You can use Int.MaxValue to obtain the maximum positive integer value,
    //and you can use x.toDouble to convert an x: Int to a Double.
    val (int, r) = nonNegativeInt(rng)

    int.toDouble / (Int.MaxValue.toDouble + 1) -> r
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (d, r) = double(rng)
    val (i, nr) = r.nextInt

    ((i, d), nr)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def boolean: Rand[Boolean] = map(nonNegativeInt)(_ % 2 == 0)

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {

    @tailrec
    def intList(count: Int, list: List[Int], rNG: RNG): (List[Int], RNG) = {
      if (count == 0) (list, rNG)
      else {
        val (i, r) = rNG.nextInt
        intList(count - 1, i +: list, r)
      }
    }

    intList(count, List.empty, rng)
  }

  def doubleViaMap: Rand[Double] =
    map(nonNegativeInt)(i => i.toDouble / (Int.MaxValue.toDouble + 1))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)

      (f(a, b), rng3)
    }

  //  Hard: If you can combine two RNG transitions, you should be able to combine a whole list of them.
  // Implement sequence for combining a List of transitions into a single transition.
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    rng => {
      @tailrec
      def stuff(fs: List[Rand[A]], res: Rand[List[A]]): Rand[List[A]] = {
        fs match {
          case (h :: t) => stuff(t, map2[List[A], A, List[A]](res, h)((a: List[A], b: A) => b +: a))
          case _ => res
        }
      }

      val initialRand: Rand[List[A]] = rng => (List.empty[A], rng)
      stuff(fs, initialRand)(rng)
    }
  }

  /*
   * flatMap allows us to generate a random A with Rand[A], and then
   * take that A and choose a Rand[B] based on its value.
   */
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (i: A, r) = f(rng)
      g(i)(r)
    }
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt)((i: Int) => {
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod)
      else nonNegativeLessThan(n)
    })
  }

  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(i => (rng: RNG) => (f(i), rng))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap[A, C](ra)(i => (rng: RNG) => {
      val (b, r) = rb(rng)
      f(i, b) -> r
    })
  }
}

case class State[S, +A](run: S => (A, S)) {


  def map[B](f: A => B): State[S, B] =
    flatMap((a: A) => State.unit(f(a)))

  // Alt implementation
  def mapx[B](f: A => B): State[S, B] = {
    State(s => {
      val (a, ns) = run(s)
      (f(a), ns)
    })
  }

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  // Alt
  def map2x[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = {
    State(s => {
      val (a, r) = run(s)
      val (b, r2) = sb.run(r)
      (f(a, b), r2)
    })
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, r) = run(s)
    val (b, rs) = f(a).run(r)
    (b, rs)
  })
}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] = State(s => {
    (a, s)
  })

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
    // alternative?
    //    sas.foldLeft(unit[S, List[A]](List.empty)){
    //      (b: State[S, List[A]], s: State[S, A]) => b.flatMap(la => s.map(a => a +: la))
    //    }

    sas.foldRight(unit[S, List[A]](List.empty[A])) {
      (s: State[S, A], b: State[S, List[A]]) => b.flatMap(la => s.map(a => a +: la))
    }
  }

  /*
   * I found the solutions in the answers and around the web not only difficult to read as the
   * concepts of get, set, modify were briefly explained, but the solutions introduced functions
   * that have not been introduced (e.g. compose). On top of this other solutions use a Candy object,
   * which would not work as the tests do not expect this...
   *
   * So this solution uses fold & copy
   *
   * Likewise, there seems to be no need to return the Int tuple, since the machine already
   * contains both values.
   *
   */
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    State { (machine: Machine) =>

      val r: Machine = inputs.foldLeft(machine)((m: Machine, i: Input) => {
        i match {
          case Coin =>
            if (m.locked && m.candies > 0) m.copy(locked = false, coins = m.coins + 1) else m
          case Turn =>
            if (!m.locked && m.candies > 0) m.copy(locked = true, candies = m.candies - 1) else m
        }
      })

      ((r.coins, r.candies), r)
    }
  }
}

