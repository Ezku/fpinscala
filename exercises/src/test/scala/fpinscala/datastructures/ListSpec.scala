package fpinscala.datastructures

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

object ListSpec extends Properties("list") {
  property("length increases by one when adding an element") = forAll { (l: List[Int], a: Int) =>
    List.length(Cons(a, l)) == (List.length(l) + 1)
  }

  implicit def arbList[A](implicit a: Arbitrary[A]): Arbitrary[List[A]] = Arbitrary {
    def genList: Gen[List[A]] =
      for {
        value <- arbitrary[A]
        list <- oneOf[List[A]](Nil, genList)
      } yield(Cons(value, list))
    genList
  }
}
