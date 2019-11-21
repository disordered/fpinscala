package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](tree: Tree[A]): Int = tree match {
      case Branch(left, right) => size(left) + size(right) + 1
      case _ => 1
    }

  def main(args: Array[String]) = {
    println(size(Leaf(1)))
    println(size(Branch(Leaf(1), Branch(Leaf(1), Leaf(1)))))
  }
}