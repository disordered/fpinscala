package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](tree: Tree[A]): Int = tree match {
    case Branch(left, right) => size(left) + size(right) + 1
    case _                   => 1
  }

  def maximum(tree: Tree[Int]): Int = tree match {
    case Branch(left, right) => maximum(left) max maximum(right)
    case Leaf(value)         => value
  }

  def depth[A](tree: Tree[A]): Int = tree match {
    case Branch(left, right) => depth(left) max depth(right) + 1
    case _                   => 0
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    case Leaf(value)         => Leaf(f(value))
  }

  def main(args: Array[String]) = {
    println(map(Leaf(1))(_.toFloat))
    println(map(Branch(Leaf(3), Branch(Leaf(2), Branch(Leaf(1), Leaf(1)))))(_.toFloat))
  }
}
