package fpinscala.datastructures

import scala.concurrent.{Await, ExecutionContext, Future}

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int =
    ints match { // A function that uses pattern matching to add up a list of integers
      case Nil => 0 // The sum of the empty list is 0.
      case Cons(x, xs) =>
        x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
    }

  def product(ds: List[Double]): Double = ds match {
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _)))          => x
    case Nil                                   => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t)                            => h + sum(t)
    case _                                     => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil        => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil         => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = l match {
    case Nil        => l
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil        => sys.error("Empty list")
    case Cons(_, t) => Cons(h, t)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else
      l match {
        case Nil        => l
        case Cons(_, t) => drop(t, n - 1)
      }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _                  => l
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil          => sys.error("Empty list")
      case Cons(_, Nil) => Nil
      case Cons(h, t)   => Cons(h, init(t))
    }
  }

  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((_, acc) => acc + 1)
  }

  @annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil        => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def reverse[A](l: List[A]): List[A] = {
    foldLeft(l, Nil: List[A])((l, h) => Cons(h, l))
  }

  def append2[A](l1: List[A], l2: List[A]): List[A] = {
    foldRight(l1, l2)(Cons(_, _))
  }

  def concatenate[A](l: List[List[A]]): List[A] = {
    foldRight(l, Nil: List[A])(append)
  }

  def add1(l: List[Int]): List[Int] = {
    foldRight(l, Nil: List[Int])((h, l) => Cons(h + 1, l))
  }

  def doubleToString(l: List[Double]): List[String] = {
    foldLeft(l, Nil: List[String])((l, h) => Cons(h.toString, l))
  }

  def main(args: Array[String]): Unit = {
    println(hasSubsequence(List(1, 2, 3, 4), List(1, 2)))
    println(hasSubsequence(List(1, 2, 3, 4), List(2, 3)))
    println(hasSubsequence(List(1, 2, 3, 4), List(4)))
    println(hasSubsequence(List(1, 2, 3, 4), List(2, 1)))
    println(hasSubsequence(List(1, 2, 3, 4), Nil))
    println(hasSubsequence(Nil, Nil))
  }
  def map[A, B](l: List[A])(f: A => B): List[B] = {
    foldRight(l, Nil: List[B])((h, l) => Cons(f(h), l))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight(as, Nil: List[A])((h, l) => if (f(h)) Cons(h, l) else l)
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    concatenate(map(as)(f))
  }

  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)(i => if (f(i)) List(i) else Nil)
  }

  def addTwoLists(l1: List[Int], l2: List[Int]): List[Int] = {
    (l1, l2) match {
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addTwoLists(t1, t2))
      case (Nil, l)                     => l
      case (l, Nil)                     => l
    }
  }

  def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = {
    (l1, l2) match {
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
      case (Nil, _)                     => Nil
      case (_, Nil)                     => Nil
    }
  }

  def startsWith[A](list: List[A], prefix: List[A]): Boolean =
    (list, prefix) match {
      case (_, Nil)                                 => true
      case (Cons(lh, lt), Cons(ph, pt)) if lh == ph => startsWith(lt, pt)
      case _                                        => false
    }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil                       => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(_, tail)             => hasSubsequence(tail, sub)
  }
}
