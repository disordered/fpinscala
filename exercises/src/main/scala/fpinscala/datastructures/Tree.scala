package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](tree: Tree[A]): Int = tree match {
      case Branch(left, right) => size(left) + size(right) + 1
      case _ => 1
    }

  def maximum(tree: Tree[Int]): Int = tree match {
    case Branch(left, right) => maximum(left) max maximum(right)
    case Leaf(value) => value
  }

  def main(args: Array[String]) = {
    println(maximum(Leaf(1)))
    println(maximum(Branch(Leaf(3), Branch(Leaf(2), Leaf(1)))))
  }
}