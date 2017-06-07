package fpinscala.errorhandling


import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter


sealed trait Either[+E,+A] {

  def isLeft: Boolean = this match {
    case Left(_) => true
    case _ => false
  }

  def isRight: Boolean = !isLeft

 def map[B](f: A => B): Either[E, B] = this match {
   case left @ Left(_) => left
   case Right(value) => Right(f(value))
 }

 def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
   case left @ Left(_) => left
   case Right(value) => f(value)
 }

 def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
   if (isRight) this else b

 def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
   b.flatMap { bb => map { aa => f(aa, bb) } } // or for-comprehension

}

case class Left[+E](get: E) extends Either[E, Nothing]

case class Right[+A](get: A) extends Either[Nothing, A]

object Either {

  def traverse[E, A, B](xs: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    xs.foldRight[Either[E, List[B]]](Right(List()))((x, acc) => f(x).map2(acc)(_ :: _))

  def sequence[E, A](xs: List[Either[E, A]]): Either[E, List[A]] =
    traverse(xs)(identity)

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if (xs.isEmpty) 
      Left("mean of empty list!")
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = 
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

}