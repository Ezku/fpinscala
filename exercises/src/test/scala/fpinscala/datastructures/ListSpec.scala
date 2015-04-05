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

  implicit def arbNonEmptyList[A](implicit a: Arbitrary[A]): Arbitrary[List[A]] = Arbitrary {
    def genList: Gen[List[A]] =
      for {
        value <- arbitrary[A]
        list <- oneOf[List[A]](Nil, genList)
      } yield(Cons(value, list))
    genList
  }
}
