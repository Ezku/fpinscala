package fpinscala.datastructures

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

object ListSpec extends Properties("list") {
  import List.{map => fmap, flatMap => bind, _}

  property("length increases by one when adding an element") = forAll { (l: List[Int], a: Int) =>
    (l |> cons(a) |> length) == length(l) + 1
  }

  property("tail decreases the length of a non-empty list by one") = forAll { l: List[Int] =>
    (l |> tail |> length) == length(l) - 1
  }

  property("setHead maintains the length of a non-empty list") = forAll { (l: List[Int], a: Int) =>
    (l |> setHead(a) |> length) == length(l)
  }

  property("drop decreases the length of a list by n up to list length") = forAll { (l: List[Int], n: Int) =>
    ((length(l) >= n) && (n >= 0)) ==> {
      (l |> drop(n) |> length) == length(l) - n
    }
  }

  property("dropWhile ensures the list will not start with an element that matches the predicate") = forAll { (l: List[Int], p: Int => Boolean) =>
    (l |> dropWhile(p)) match {
      case Nil => true
      case Cons(a, as) => !p(a)
    }
  }

  property("init decreases the length of a non-empty list by one") = forAll { l: List[Int] =>
    (l |> init |> length) == (length(l) - 1)
  }

  property("reverse maintains list length") = forAll { l: List[Int] =>
    (l |> reverse |> length) == (l |> length)
  }

  property("a tail's reverse is the same as a reverse's init") = forAll { l: List[Int] =>
    (l |> tail |> reverse) == (l |> reverse |> init)
  }

  property("append yiels a list equal in length to the sum of two lists' lengths") = forAll { (l: List[Int], r: List[Int]) =>
    (length(l) + length(r)) == (l |> append(r) |> length)
  }

  property("flatten yields a list with length equal to the sum of its lists' lengths") = forAll { (ll: List[List[Int]]) =>
    (ll |> fmap { length _ } |> sum) == (ll |> flatten |> length)
  }

  property("map yields the same list, given identity") = forAll { (l: List[Int]) =>
    (l |> fmap(identity)) == l
  }

  property("map ignores function composition order") = forAll { (l: List[Int], f: Int => Double, g: Double => String) =>
    (l |> fmap(g compose f)) ==  (l |> fmap(f) |> fmap(g))
  }

  property("filter ensures no items matching predicate will be left") = forAll { (l: List[Int], p: Int => Boolean) =>
    (l |> filter(p) |> filter { i => !p(i) }) == Nil
  }

  property("consecutive applications of filter ignore application order") = forAll { (l: List[Int], a: Int => Boolean, b: Int => Boolean) =>
    (l |> filter(a) |> filter(b)) == (l |> filter(b) |> filter(a))
  }

  property("flatMap yields identity given identity") = forAll { l: List[Int] =>
    (l |> bind(unit)) == l
  }

  property("flatMap ignores function composition order") = forAll { (l: List[Int], f: Int => List[Double], g: Double => List[String]) =>
    (l |> bind(f) |> bind(g)) == (l |> List.flatMap { i: Int =>
      bind(g)(f(i))
    })
  }

  implicit def arbNonEmptyList[A](implicit a: Arbitrary[A]): Arbitrary[List[A]] = Arbitrary {
    def genList: Gen[List[A]] =
      for {
        value <- arbitrary[A]
        list <- oneOf[List[A]](Nil, genList)
      } yield(Cons(value, list))
    genList
  }
}
