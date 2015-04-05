package fpinscala.datastructures
import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = foldLeft(ints, 0) { _ + _ }

  def product(ds: List[Double]): Double = foldLeft(ds, 0.0) { _ * _ }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(a, as) => as
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Cons(h, Nil)
    case Cons(a, as) => Cons(h, as)
  }

  def drop[A](l: List[A], n: Int): List[A] = (l, n) match {
    case (_, n) if n < 0 => Nil
    case (_, 0) => l
    case (Nil, _) => Nil
    case (Cons(a, as), _) => drop(as, n - 1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(a, as) if f(a) => dropWhile(as, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(a, Nil) => Nil
    case Cons(a, as) => Cons(a, init(as))
  }

  def length[A](l: List[A]): Int = foldLeft(l, 0) { (length: Int, _) => length + 1 }

  @tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(a, Nil) => f(z, a)
    case Cons(a, as) => foldLeft(as, f(z, a))(f)
  }

  def reverse[A](l: List[A]) = foldLeft(l, Nil:List[A]) { (as: List[A], a: A) =>
    Cons(a,as)
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = sys.error("todo")
}
