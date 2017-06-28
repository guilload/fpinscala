package fpinscala.monoids

import fpinscala.parallelism.Nonblocking._
import fpinscala.parallelism.Nonblocking.Par.toParOps // infix syntax for `Par.map`, `Par.flatMap`, etc
import language.higherKinds

trait Monoid[A] {
  self =>
  def op(a1: A, a2: A): A
  def zero: A

  def dual: Monoid[A] = new Monoid[A] {
    def op(a1: A, a2: A): A = self.op(a2, a1)
    val zero: A = self.zero
  }

}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String): String = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
    val zero = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 + a2
    val zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 * a2
    val zero: Int = 1
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    val zero: Boolean = true
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    val zero: Boolean = false
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
    def zero: Option[A] = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(a1: A => A, a2: A => A): A => A = a1 compose a2
    def zero: A => A = identity
  }

  // TODO: Placeholder for `Prop`. Remove once you have implemented the `Prop`
  // data type from Part 2.
  trait Prop {}

  // TODO: Placeholder for `Gen`. Remove once you have implemented the `Gen`
  // data type from Part 2.

  import fpinscala.testing._
  import Prop._
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = ???

  def trimMonoid(s: String): Monoid[String] = ???

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    ???

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(a => b => f(a, b))(z)

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, endoMonoid[B].dual)(a => b => f(b, a))(z)

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    as.length match {
      case 0 => m.zero
      case 1 => f(as.head)
      case l =>
        val (left, right) = as.splitAt(l / 2)
        m.op(foldMapV(left, m)(f), foldMapV(right, m)(f))
    }

  def ordered(ints: IndexedSeq[Int]): Boolean =
    ???

  sealed trait WC {
    def +(other: WC): WC
    def wordCount: Int

    protected def carry(s: String): Int = if (s.isEmpty) 0 else 1
  }

  case class Stub(chars: String) extends WC {

    def +(other: WC): WC = other match {
      case Stub(s) => Stub(chars + s)
      case p @ Part(l, _, _) => p.copy(lStub = chars + l)
    }

    def wordCount: Int = carry(chars)

  }

  case class Part(lStub: String, words: Int, rStub: String) extends WC {

    def +(other: WC): WC = other match {
      case Stub(s) => copy(rStub = rStub + s)
      case Part(l, w, r) =>
        copy(words = words + w + carry(rStub + l), rStub = r)
    }

    def wordCount: Int = words + carry(lStub) + carry(rStub)

  }

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    def op(a1: Par[A], a2: Par[A]): Par[A] = Par.map2(a1, a2)(m.op)
    def zero: Par[A] = Par.unit(m.zero)
  }

  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    foldMapV(v, par(m))(a => Par.unit(a).map(f))

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def op(a1: WC, a2: WC): WC = a1 + a2
    def zero: WC = Stub("")
  }

  implicit class WordCountOps(val c: Char) extends AnyVal {
    def toWC: WC = if (c.isWhitespace) Part("", 0, "") else Stub(c.toString)
  }

  def count(s: String): Int = foldMapV(s, wcMonoid)(_.toWC).wordCount

  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    ???

  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] =
    ???

  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] =
    ???

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    ???
}


trait Foldable[F[_]] {

  import Monoid.endoMonoid

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    foldMap(as)(a => (b: B) => f(a, b))(endoMonoid)(z)

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    foldMap(as)(a => (b: B) => f(b, a))(endoMonoid.dual)(z)

  def foldMap[A, B](as: F[A])(f: A => B)(m: Monoid[B]): B =
    foldLeft(as)(m.zero)((b, a) => m.op(b, f(a)))

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  def toList[A](as: F[A]): List[A] =
    foldRight(as)(List[A]())(_ :: _)

}


object ListFoldable extends Foldable[List] {

  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: List[A])(f: A => B)(m: Monoid[B]): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

}


object IndexedSeqFoldable extends Foldable[IndexedSeq] {

  import Monoid.foldMapV

  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(m: Monoid[B]): B =
    foldMapV(as, m)(f)

}


object StreamFoldable extends Foldable[Stream] {

  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)

}


sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {

  override def foldMap[A, B](as: Tree[A])(f: A => B)(m: Monoid[B]): B = as match {
    case Branch(left, right) => m.op(foldMap(left)(f)(m), foldMap(right)(f)(m))
    case Leaf(value) => f(value)
  }

  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = as match {
    case Branch(left, right) => foldLeft(right)(foldLeft(left)(z)(f))(f)
    case Leaf(value) => f(z, value)
  }

  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = as match {
    case Branch(left, right) => foldRight(left)(foldRight(right)(z)(f))(f)
    case Leaf(value) => f(value, z)
  }

}


object OptionFoldable extends Foldable[Option] {

  override def foldMap[A, B](as: Option[A])(f: A => B)(m: Monoid[B]): B =
    as.map(f).getOrElse(m.zero)

  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B =
    as.map(a => f(z, a)).getOrElse(z)

  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B =
    as.map(a => f(a, z)).getOrElse(z)

}

