package fpinscala.datastructures
import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

object List { // `List` companion object. Contains functions for creating and working with lists.
  def cons[A](head: A)(tail: List[A]): List[A] = Cons(head, tail)
  def unit[A](v: A) = Cons(v, Nil)
  def empty[A] = Nil:List[A]

  def sum(ints: List[Int]): Int = ints |> foldLeft(0) { _ + _ }

  def product(ds: List[Double]): Double = ds |> foldLeft(0.0) { _ * _ }

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

  def append[A](a2: List[A])(a1: List[A]): List[A] =
    a1 |> foldRight(a2) { Cons(_, _) }

  def foldRight[A,B](z: B)(f: (A, B) => B)(as: List[A]): B = // Utility functions
    reverse(as) |> foldLeft(z) { (z: B, a: A) =>
      f(a, z)
    }

  def sum2(ns: List[Int]) = ns |> foldRight(0) { (x,y) => x + y }

  def product2(ns: List[Double]) = ns |> foldRight(1.0) { _ * _ }

  def head[A](l: List[A]): Option[A] = l match {
    case Nil => None
    case Cons(a, _) => Some(a)
  }

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(a, as) => as
  }

  def setHead[A](h: A)(l: List[A]): List[A] = l match {
    case Nil => Cons(h, Nil)
    case Cons(a, as) => Cons(h, as)
  }

  def drop[A](n: Int)(l: List[A]): List[A] = (l, n) match {
    case (_, n) if n < 0 => Nil
    case (_, 0) => l
    case (Nil, _) => Nil
    case (Cons(a, as), _) => drop(n - 1)(as)
  }

  def dropWhile[A](f: A => Boolean)(l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(a, as) if f(a) => dropWhile(f)(as)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(a, Nil) => Nil
    case Cons(a, as) => Cons(a, init(as))
  }

  def length[A](l: List[A]): Int = l |> foldLeft(0) { (length: Int, _) => length + 1 }

  @tailrec
  def foldLeft[A,B](z: B)(f: (B, A) => B)(l: List[A]): B = l match {
    case Nil => z
    case Cons(a, Nil) => f(z, a)
    case Cons(a, as) => foldLeft(f(z, a))(f)(as)
  }

  def reverse[A](l: List[A]) = l |> foldLeft(Nil:List[A]) { (as: List[A], a: A) =>
    Cons(a,as)
  }

  def map[A,B](f: A => B)(l: List[A]): List[B] = l |> foldRight(Nil:List[B]) { (a: A, bs: List[B]) =>
    Cons(f(a), bs)
  }

  def flatten[A](ll: List[List[A]]): List[A] = ll |> foldLeft(Nil:List[A]) {
    _ |> List.append(_)
  }

  def filter[A](f: A => Boolean) = flatMap({ a: A =>
    if (f(a))
      unit(a)
    else
      empty
  }) _

  def flatMap[A,B](f: A => List[B])(as: List[A]) =
    as |> map(f) |> flatten

  def zipWith[A,B](f: (A, A) => B)(lists: (List[A], List[A])): List[B] = lists match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(l, ls), Cons(r, rs)) =>
      Cons(f(l, r), zipWith(f)(ls -> rs))
  }

  def startsWith[A](seq: List[A])(l: List[A]): Boolean =
    (seq, l) match {
      case (Nil, _) => true
      case (_, Nil) => false
      case (Cons(a, as), Cons(b, bs)) =>
        if (a == b)
          startsWith(as)(bs)
        else
          false
    }

  def hasSubSequence[A](seq: List[A])(l: List[A]): Boolean =
    (seq, l) match {
      case (Nil, _) => true
      case (_, Nil) => false
      case (_, Cons(_, t)) =>
        if (l |> startsWith(seq))
          true
        else
          t |> hasSubSequence(seq)
    }


  implicit class Piper[A](val x: A) extends AnyVal {
    def |>[B](f: A => B) = f(x)
  }
}
