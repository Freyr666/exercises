package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] =
    for {
      v <- arbitrary[Int]
      h <- oneOf(const(empty), genHeap)
    } yield insert(v, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  def toList(h: H, acc: List[Int]): List[Int] =
      if (isEmpty(h)) acc
      else {
        val min = findMin(h)
        toList(deleteMin(h), min::acc)
      }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { (i: Int) =>
    val h = insert(i, empty)
    findMin(h) == i
  }

  property("smallest1") = forAll { (i: Int, j: Int) =>
    val h = insert(i, insert(j, empty))
    findMin(h) == (if (i < j) i else j)
  }

  property("remove single1") = forAll { (i: Int) =>
    val h = insert(i, empty)
    isEmpty(deleteMin(h))
  }

  property("deleted elements are sorted1") = {
    def isRevSorted(l: List[Int]): Boolean = l match {
      case Nil => true
      case _::Nil => true
      case x::y::tl =>
        if (x >= y) isRevSorted(y::tl)
        else false
    }
    forAll { (h: H) => isRevSorted(toList(h, Nil)) }
  }

  property("minimum of two heaps is minimum of either the first or the second1") =
    forAll { (h1: H, h2: H) =>
      val min = findMin(meld(h1, h2))
      min == findMin(h1) || min == findMin(h2)
    }

  property("associative meld") = forAll { (h:H, i:H, j:H) =>
    val left = meld(meld(h, i), j)
    val right = meld(h, meld(i, j))
    toList(left, Nil) == toList(right, Nil)
  }

  property("proper element is removed1") = {
    def equal(h1: H, h2: H): Boolean =
      toList(h1, Nil) == toList(h2, Nil)
    forAll { (h: H) =>
      val min = findMin(h)
      val h1 = deleteMin(h)
      equal(h, insert(min, h1))
    }
  }


}
