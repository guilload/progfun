package quickcheck

import scala.annotation.tailrec
import scala.language.implicitConversions

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  implicit def toList(h: H): List[A] = {

    @tailrec
    def impl(current: H, acc: List[A] = Nil): List[A] =
      if (isEmpty(current)) acc else impl(deleteMin(current), findMin(current) :: acc)

    impl(h).reverse
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("hint1") = forAll { (a: A, b: A) => BooleanOperators(a < b) ==> {
    val heap = insert(b, insert(a, empty))
    findMin(heap) == a
    }
  }

  property("hint2") = forAll { a: A =>
    val heap = deleteMin(insert(a, empty))
    isEmpty(heap)
  }

  property("hint3") = forAll { h: H =>
    val hlist = h.toList
    hlist == hlist.sorted
  }

  property("hint4") = forAll { (h: H, i: H) =>
    val min = findMin(meld(h, i))
    min == findMin(h) || min == findMin(i)
  }

  property("meld") = forAll { (h: H, i: H) =>
    meld(h, i).toList == (h.toList ++ i.toList).sorted
  }

  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[A]
    h <- oneOf(const(empty), genHeap)
  } yield insert(a, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
