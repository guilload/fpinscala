package fpinscala.state


trait RNG {

  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.

}

object RNG {

  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {

    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
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
    val (n, next) = rng.nextInt
    (if (n < 0) -(n + 1) else n, next)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (n, next) = rng.nextInt
    (n / (Int.MaxValue.toDouble + 1), next)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (n, next) = rng.nextInt
    val (f, nnext) = double(next)
    ((n, f), nnext)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (tuple, next) = intDouble(rng)
    (tuple.swap, next)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (f0, next) = double(rng)
    val (f1, nnext) = double(next)
    val (f2, nnnext) = double(nnext)
    ((f0, f1, f2), nnnext)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {

    @annotation.tailrec
    def inner(count: Int, rng: RNG, acc: List[Int] = List()): (List[Int], RNG) = {
      if (count == 0) (acc, rng)
      else {
        val (n, next) = rng.nextInt
        inner(count - 1, next, n :: acc)
      }
    }

    inner(count, rng)
  }

  val doubleViaMap: Rand[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, next) = ra(rng)
      val (b, nnext) = rb(next)
      (f(a, b), nnext)
    }
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  def intsViaSequence(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, next) = f(rng)
      g(a)(next)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod)
      else nonNegativeLessThan(n)
    }

  def mapViaFlatMap[A, B](rand: Rand[A])(f: A => B): Rand[B] =
    flatMap(rand)(a => unit(f(a)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra)(a => map(rb)(b => f(a, b)))
  }

}


case class State[S, +A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(
      s => {
        val (a, next) = run(s)
        f(a).run(next)
      }
    )

}


object State {

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def sequence[S, A](xs: List[State[S, A]]): State[S, List[A]] =
    xs.foldRight(unit[S, List[A]](List()))((s, acc) => s.map2(acc)(_ :: _))

  def modify[S](f: S => S): State[S, Unit] = for {
    state <- get
    _ <- set(f(state))
  } yield ()

  def get[S]: State[S, S] = State(state => (state, state))

  def set[S](state: S): State[S, Unit] = State(_ => ((), state))

}

sealed trait Input

case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {

  def unlocked: Boolean = !locked

  def update(input: Input): Machine =
    input match {
      case Coin if locked && candies > 0 => copy(locked = false, coins = coins + 1)
      case Turn if unlocked => copy(locked = true, candies = candies - 1)
      case _ => this
    }

}

object Machine {

  import State._

  def simulate(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs.map(input => modify[Machine](m => m.update(input))))
    machine <- get
  } yield (machine.coins, machine.candies)

}
