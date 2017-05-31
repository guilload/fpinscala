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

}