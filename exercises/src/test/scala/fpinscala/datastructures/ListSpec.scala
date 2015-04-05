package fpinscala.datastructures

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

object ListSpec extends Properties("list") {
  property("length increases by one when adding an element") = forAll { (l: List[Int], a: Int) =>
    List.length(Cons(a, l)) == (List.length(l) + 1)
  }

  property("tail decreases the length of a non-empty list by one") = forAll { l: List[Int] =>
    List.length(List.tail(l)) == (List.length(l) - 1)
  }

  property("setHead maintains the length of a non-empty list") = forAll { (l: List[Int], a: Int) =>
    List.length(List.setHead(l, a)) == List.length(l)
  }

  property("drop decreases the length of a list by n up to list length") = forAll { (l: List[Int], n: Int) =>
    ((List.length(l) >= n) && (n >= 0)) ==> {
      List.length(List.drop(l, n)) == (List.length(l) - n)
    }
  }

  property("dropWhile ensures the list will not start with an element that matches the predicate") = forAll { (l: List[Int], p: Int => Boolean) =>
    List.dropWhile(l, p) match {
      case Nil => true
      case Cons(a, as) => !p(a)
    }
  }

  property("init decreases the length of a non-empty list by one") = forAll { l: List[Int] =>
    List.length(List.init(l)) == (List.length(l) - 1)
  }

  property("reverse maintains list length") = forAll { l: List[Int] =>
    List.length(List.reverse(l)) == List.length(l)
  }

  property("a tail's reverse is the same as a reverse's init") = forAll { l: List[Int] =>
    List.reverse(List.tail(l)) == List.init(List.reverse(l))
  }

  property("append yiels a list equal in length to the sum of two lists' lengths") = forAll { (l: List[Int], r: List[Int]) =>
    (List.length(l) + List.length(r)) == List.length(List.append(l, r))
  }

  property("flatten yields a list with length equal to the sum of its lists' lengths") = forAll { (ll: List[List[Int]]) =>
    List.sum(List.map(ll) { l => List.length(l) }) == List.length(List.flatten(ll))
  }

  property("map yields the same list, given identity") = forAll { (l: List[Int]) =>
    List.map(l)(identity) == l
  }

  property("map ignores function composition order") = forAll { (l: List[Int], f: Int => Double, g: Double => String) =>
    List.map(l)(g compose f) == List.map(List.map(l)(f))(g)
  }

  property("filter ensures no items matching predicate will be left") = forAll { (l: List[Int], p: Int => Boolean) =>
    (l |> List.filter(p) |> List.filter { i => !p(i) }) == Nil
  }

  property("consecutive applications of filter ignore application order") = forAll { (l: List[Int], a: Int => Boolean, b: Int => Boolean) =>
    (l |> List.filter(a) |> List.filter(b)) == (l |> List.filter(b) |> List.filter(a))
  }

  implicit def arbNonEmptyList[A](implicit a: Arbitrary[A]): Arbitrary[List[A]] = Arbitrary {
    def genList: Gen[List[A]] =
      for {
        value <- arbitrary[A]
        list <- oneOf[List[A]](Nil, genList)
      } yield(Cons(value, list))
    genList
  }

  implicit class Piper[A](val x: A) extends AnyVal {
    def |>[B](f: A => B) = f(x)
  }
}
