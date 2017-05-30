package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

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

  val example: Int = List(1,2,3,4,5) match {
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

  def sum2(ns: List[Int]): Int =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]): Double =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => throw new UnsupportedOperationException("tail of empty list")
    case Cons(_, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => throw new UnsupportedOperationException("tail of empty list")
    case Cons(_, xs) => Cons(h, xs)
  }

  def isEmpty[A](l: List[A]): Boolean = l match {
    case Nil => true
    case _ => false
  }

  def nonEmpty[A](l: List[A]): Boolean = !isEmpty(l)

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n < 0) throw new UnsupportedOperationException("n must be greater than or equal to zero")
    else if (n == 0 || isEmpty(l)) l
    else drop(tail(l), n - 1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => throw new UnsupportedOperationException("init of empty list")
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs)) // not stack-safe
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, acc) => acc + 1) // not stack-safe

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def sum3(l: List[Int]): Int =
    foldLeft(l, 0)(_ + _)

  def product3(l: List[Double]): Double =
    foldLeft(l, 1.0)(_ * _)

  def length3[A](l: List[A]): Int =
    foldLeft(l, 0)((acc, _) => acc + 1)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil[A])((acc, x) => Cons(x, acc))

  def foldRight2[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z)((acc, x) => f(x, acc))

  def append2[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)(Cons(_,_)) // not stack-safe

  def flatten[A](ll: List[List[A]]): List[A] =
    foldRight(ll, Nil[A])(append) // not stack-safe

  def addOne(l: List[Int]): List[Int] =
    foldRight(l, Nil[Int])((x, acc) => Cons(x + 1, acc)) // not stack-safe

  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil[Int])((x, acc) => Cons(x.toString, acc)) // not stack-safe

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil[B])((x, acc) => Cons(f(x), acc)) // not stack-safe

  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, Nil[A])((x, acc) => if (f(x)) Cons(x, acc) else acc) // not stack-safe

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
    foldRight(l, Nil[B])((x, acc) => append(f(x), acc)) // or `flatten(map(l)(f))`, not stack-safe either way

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(x => if (f(x)) List(x) else Nil) // not stack-safe

  def zipSum(l: List[Int], r: List[Int]): List[Int] = (l, r) match {
    case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, zipSum(xs, ys)) // still not stack-safe... :)
    case _ => Nil
  }

  def zipWith[A, B, C](l: List[A], r: List[B])(f: (A, B) => C): List[C] = (l, r) match {
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f)) // still not stack-safe... :)
    case _ => Nil
  }

  @annotation.tailrec
  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = {

    @annotation.tailrec
    def startsWith(l: List[A], sub: List[A]): Boolean = (l, sub) match {
      case (_, Nil) => true
      case (Cons(x, xs), Cons(y, ys)) => x == y && startsWith(xs, ys)
      case _ => false
    }

    startsWith(l, sub) || nonEmpty(l) && hasSubsequence(tail(l), sub)
  }

}
