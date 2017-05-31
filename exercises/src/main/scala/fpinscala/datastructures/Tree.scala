package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](tree: Tree[A]): Int = tree match {
    case Branch(left, right) => 1 + size(left) + size(right) // not stack-safe
    case _ => 1
  }

  def maximum(tree: Tree[Int]): Int = tree match {
    case Branch(left, right) => maximum(left) max maximum(right) // not stack-safe
    case Leaf(value) => value
  }

  def depth[A](tree: Tree[A]): Int = tree match {
    case Branch(left, right) => 1 + (depth(left) max depth(right)) // not stack-safe
    case _ => 0
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Branch(left, right) => Branch(map(left)(f), map(right)(f)) // not stack-safe
    case Leaf(value) => Leaf(f(value))
  }

  def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = tree match {
    case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g)) // not stack-safe
    case Leaf(value) => f(value)
  }

  // none of these functions are stack-safe
  def sizeViaFold[A](tree: Tree[A]): Int =
    fold(tree)(_ => 1)(1 + _ + _)

  def maximumViaFold(tree: Tree[Int]): Int =
    fold(tree)(identity)(_ max _)

  def depthViaFold[A](tree: Tree[A]): Int =
    fold(tree)(_ => 0)((left, right) => 1 + (left max right))

  def mapViaFold[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    fold(tree)(value => Leaf(f(value)): Tree[B])(Branch(_, _))

}