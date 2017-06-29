package fpinscala
package monads

import fpinscala.parallelism._
import fpinscala.parallelism.Par._
import fpinscala.parsing._
import fpinscala.state._
import fpinscala.testing._

import scala.language.higherKinds


trait Functor[F[_]] {

  def map[A, B](fa: F[A])(f: A => B): F[B]

  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab) { case (first, _) => first }, map(fab) { case (_, second) => second })

  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left.apply)
    case Right(fb) => map(fb)(Right.apply)
  }

}

object Functor {

  val listFunctor = new Functor[List] {
    def map[A, B](as: List[A])(f: A => B): List[B] = as map f
  }

}

trait Monad[M[_]] extends Functor[M] {

  def unit[A](a: => A): M[A]

  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

  def map[A, B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def sequence[A](lma: List[M[A]]): M[List[A]] =
    lma.foldRight(unit(List.empty[A]))((ma, acc) => map2(ma, acc)(_ :: _))

  def traverse[A, B](la: List[A])(f: A => M[B]): M[List[B]] =
    la.foldRight(unit(List.empty[B]))((a, acc) => map2(f(a), acc)(_ :: _))

  def replicateM[A](n: Int, ma: M[A]): M[List[A]] = ???

  def compose[A,B,C](f: A => M[B], g: B => M[C]): A => M[C] = ???

  // Implement in terms of `compose`:
  def _flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] = ???

  def join[A](mma: M[M[A]]): M[A] = ???

  // Implement in terms of `join`:
  def __flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] = ???

}

case class Reader[R, A](run: R => A)

object Monad {

  val genMonad = new Monad[Gen] {

    def unit[A](a: => A): Gen[A] = Gen.unit(a)

    override def flatMap[A, B](ma: Gen[A])(f: A => Gen[B]): Gen[B] = ma.flatMap(f)
  }

  val parMonad: Monad[Par] = new Monad[Par] {

    def unit[A](a: => A): Par[A] = Par.unit(a)

    def flatMap[A, B](pa: Par[A])(f: A => Par[B]): Par[B] = Par.flatMap(pa)(f)

  }

  def parserMonad[P[+_]](p: Parsers[P]): Monad[P] = ???

  val optionMonad: Monad[Option] = new Monad[Option] {

    def unit[A](a: => A): Option[A] = Option(a)

    def flatMap[A, B](opt: Option[A])(f: A => Option[B]): Option[B] = opt.flatMap(f)

  }

  val streamMonad: Monad[Stream] = new Monad[Stream] {

    def unit[A](a: => A): Stream[A] = Stream(a)

    def flatMap[A, B](opt: Stream[A])(f: A => Stream[B]): Stream[B] = opt.flatMap(f)

  }

  val listMonad: Monad[List] = new Monad[List] {

    def unit[A](a: => A): List[A] = List(a)

    def flatMap[A, B](opt: List[A])(f: A => List[B]): List[B] = opt.flatMap(f)

  }

  def stateMonad[S]: Monad[S] = ???

  val idMonad: Monad[Id] = ???

  def readerMonad[R]: Monad[R] = ???

}

case class Id[A](value: A) {

  def map[B](f: A => B): Id[B] = ???

  def flatMap[B](f: A => Id[B]): Id[B] = ???

}

object Reader {

  def readerMonad[R] = new Monad[({type f[x] = Reader[R, x]})#f] {

    def unit[A](a: => A): Reader[R, A] = ???

    override def flatMap[A, B](st: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] = ???
  }

}

