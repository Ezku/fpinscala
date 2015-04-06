package fpinscala.datastructures
import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

object List { // `List` companion object. Contains functions for creating and working with lists.
  def cons[A](head: A)(tail: List[A]): List[A] = Cons(head, tail)
  def unit[A](v: A) = Cons(v, Nil)
  def empty[A] = Nil:List[A]

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

  def append[A](a2: List[A])(a1: List[A]): List[A] =
    foldRight(a1, a2) { Cons(_, _) }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    foldLeft(reverse(as), z) { (z: B, a: A) =>
      f(a, z)
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

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

  def map[A,B](f: A => B)(l: List[A]): List[B] = foldRight(l, Nil:List[B]) { (a: A, bs: List[B]) =>
    Cons(f(a), bs)
  }

  def flatten[A](ll: List[List[A]]): List[A] = foldRight(ll, Nil:List[A]) {
    _ |> List.append(_)
  }

  def filter[A](f: A => Boolean) = flatMap({ a: A =>
    if (f(a))
      unit(a)
    else
      empty
  }) _

  def flatMap[A,B](f: A => List[B])(as: List[A]) =
    flatten(map(f)(as))

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
