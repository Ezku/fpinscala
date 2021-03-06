package fpinscala.datastructures

import org.scalacheck._
import Prop._

import org.scalacheck.Shapeless._

object ListSpec extends Properties("list") {
  import List.{map => fmap, flatMap => bind, _}
  implicitly[Arbitrary[List[Int]]]

  property("length increases by one when adding an element") = forAll { (l: List[Int], a: Int) =>
    (l |> cons(a) |> length) == length(l) + 1
  }

  property("tail decreases the length of a non-empty list by one") = forAll { l: List[Int] =>
    (l |> length) > 0 ==> {
      (l |> tail |> length) == length(l) - 1
    }
  }

  property("setHead maintains the length of a non-empty list") = forAll { (l: List[Int], a: Int) =>
    (l |> length) > 0 ==> {
      (l |> setHead(a) |> length) == length(l)
    }
  }

  property("drop decreases the length of a list by n up to list length") = forAll { (l: List[Int], n: Int) =>
    ((length(l) >= n) && (n >= 0)) ==> {
      (l |> drop(n) |> length) == length(l) - n
    }
  }

  property("dropWhile ensures the list will not start with an element that matches the predicate") = forAll { (l: List[Int], p: Int => Boolean) =>
    (l |> dropWhile(p) |> head) match {
      case None => true
      case Some(a) => !p(a)
    }
  }

  property("init decreases the length of a non-empty list by one") = forAll { l: List[Int] =>
    (l |> length) > 0 ==> {
      (l |> init |> length) == (length(l) - 1)
    }
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

  property("append yields a list with the two inputs lists as subsequences") = forAll { (l: List[Int], r: List[Int]) =>
    (l |> append(r) |> hasSubSequence(l)) && (l |> append(r) |> hasSubSequence(r))
  }

  property("flatten yields a list with length equal to the sum of its lists' lengths") = forAll { (ll: List[List[Int]]) =>
    (ll |> fmap { length _ } |> sum) == (ll |> flatten |> length)
  }

  property("flatten yields a list with all of the individual lists as subsequences") = forAll { (ll: List[List[Int]]) =>
    (ll |> fmap { l => ll |> flatten |> hasSubSequence(l) } |> foldLeft(true) { _ && _ })
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
    (l |> bind(f) |> bind(g)) == (l |> bind { f andThen bind(g) })
  }

  property("the sum of a zipWith sum is the same as the sum over the individual lists") = forAll { (a: List[Int], b: List[Int]) =>
    (length(a) == length(b)) ==> {
      ((a -> b) |> zipWith { _ + _ } |> sum) == sum(a) + sum(b)
    }
  }
}
