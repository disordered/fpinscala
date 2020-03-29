package fpinscala.laziness

import Stream._
import scala.annotation.tailrec
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def forAll(p: A => Boolean): Boolean = this.foldRight(true)((a, b) => p(a) && b)

  def takeWhileViaFold(p: A => Boolean): Stream[A] = this.foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else empty)

  def headOption: Option[A] = this.foldRight(None: Option[A])((a, _) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] = this.foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(p: A => Boolean): Stream[A] = this.foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else b)

  def append[B >: A](s: => Stream[B]): Stream[B] = this.foldRight(s)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] = this.foldRight(empty[B])((a, b) => f(a).append(b))

  def startsWith[B](s: Stream[B]): Boolean = this.zipAll(s).takeWhile(_._2.isDefined).forAll { case (a, b) => a == b }

  def toList: List[A] = foldRight(Nil: List[A])(_ :: _)

  def mapViaUnfold[B](f: A => B): Stream[B] = Stream.unfold(this){
    case Cons(h, t) => Some(f(h()), t())
    case _ => None
  }

  def takeViaUnfold(n: Int): Stream[A] = Stream.unfold((this, n)){
    case (Cons(h, t), n) if n > 1 => Some(h(), (t(), n - 1))
    case (Cons(h, _), 1) => Some(h(), (empty, 0))
    case _ => None
  }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = Stream.unfold(this){
    case Cons(h, t) if p(h()) => Some(h(), t())
    case _ => None
  }

  def zipWithViaUnfold[B, C](s: Stream[B])(f: (A, B) => C): Stream[C] = Stream.unfold((this, s)){
    case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
    case _ => None
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = unfold((this, s2)){
    case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
    case (Cons(h1, t1), empty) => Some((Some(h1()), None), (t1(), empty))
    case (empty, Cons(h2, t2)) => Some((None, Some(h2())), (empty, t2()))
    case _ => None
  }

  def tails: Stream[Stream[A]] = Stream.unfold(this){
    case Empty => Option.empty
    case s => Some((s, s.drop(1)))
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def main(args: Array[String]): Unit = {
    println(Stream(1, 2, 3, 4).zipAll(Stream(1, 2, 3, 4, 5)).takeViaUnfold(5).toList)
  }
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant(n: Int): Stream[Int] = Stream.cons(n, constant(n))

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def go(current: Int, next: Int): Stream[Int] = Stream.cons(current, go(next, current + next))
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((h, t)) => cons(h, unfold(t)(f))
    case None => empty
  }

  def fibsViaUnfold: Stream[Int] = unfold((0, 1)){ case (current, next) => Some((current, (next, current + next))) }

  def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(n => Some(n, n + 1))

  def constantViaUnfold(n: Int): Stream[Int] = unfold(n)(n => Some(n, n))

  def onesViaUnfold: Stream[Int] = unfold(1)(_ => Some(1, 1))
}