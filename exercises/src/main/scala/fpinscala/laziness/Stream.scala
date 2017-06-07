package fpinscala.laziness


import Stream._


trait Stream[+A] {

  def toList: List[A] =
    foldRight(List[A]())(_ :: _) // not stack-safe

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def isEmpty: Boolean = this match {
    case Empty => true
    case _ => false
  }

  def nonEmpty: Boolean = !isEmpty

  def take(n: Int): Stream[A] = this match {
    case Cons(head, tail) if n > 0 => cons(head(), tail().drop(n - 1))
    case _ => empty
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, tail) if n > 0 => tail().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(head, tail) if p(head()) => cons(head(), tail().takeWhile(p))
    case _ => empty
  }

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((head, tail) => if (p(head)) cons(head, tail) else empty)

  @annotation.tailrec
  final def forAll(p: A => Boolean): Boolean = this match {
    case Cons(head, tail) => p(head()) && tail().forAll(p)
    case _ => true
  }

  def headOption: Option[A] =
    foldRight(None: Option[A])((head, _) => Some(head))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((head, tail) => cons(f(head), tail))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((head, tail) => if (f(head)) cons(head, tail) else tail)

  def append[B >: A](stream: => Stream[B]): Stream[B] =
    foldRight(stream)(cons(_, _))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((head, tail) => f(head).append(tail))

  def startsWith[B](s: Stream[B]): Boolean = ???

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

  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  val fibs: Stream[Int] = {
    def inner(current: Int, next: Int): Stream[Int] =
      cons(current, inner(next, current + next))

    inner(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = ???

}