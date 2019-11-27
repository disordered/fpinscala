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

  def fold[A, B](tree: Tree[A])(a: A => B)(f: (B, B) => B): B = tree match {
    case Leaf(value)         => a(value)
    case Branch(left, right) => f(fold(left)(a)(f), fold(right)(a)(f))
  }

  def sizeViaFold[A](tree: Tree[A]): Int = fold(tree)(_ => 1)(1 + _ + _)

  def maximumViaFold(tree: Tree[Int]): Int = fold(tree)(i => i)(_ max _)

  def depthViaFold[A](tree: Tree[A]): Int =
    fold(tree)(_ => 0)((l, r) => 1 + (l max r))

  def main(args: Array[String]) = {
    println(sizeViaFold(Leaf(1)))
    println(sizeViaFold(Branch(Leaf(1), Branch(Leaf(1), Leaf(1)))))

    println(maximumViaFold(Leaf(1)))
    println(maximumViaFold(Branch(Leaf(3), Branch(Leaf(2), Leaf(1)))))

    println(depthViaFold(Leaf(1)))
    println(
      depthViaFold(Branch(Leaf(3), Branch(Leaf(2), Branch(Leaf(1), Leaf(1)))))
    )
  }
}
