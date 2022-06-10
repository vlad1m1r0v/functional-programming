package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

import scala.annotation.tailrec

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  @tailrec
  final def heapEquals(h1: H, h2: H): Boolean = (h1, h2) match {
    case (Nil, Nil) => true
    case _ if (isEmpty(h1) || isEmpty(h2)) => false
    case (hs1, hs2) => {
      findMin(hs1) == findMin(hs2) && heapEquals(deleteMin(hs1), deleteMin(hs2))
    }
  }

  @tailrec
  final def checkElem(h: H, el: Int): Boolean = h match {
    case Nil => false
    case hs1 => findMin(hs1) == el || checkElem(deleteMin(hs1), el)
  }

  lazy val genHeap: Gen[H] = for {
    x <- arbitrary[A]
    heap <- oneOf(const(empty), genHeap)
  } yield insert(x, heap)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  //main

  property("findMin from a 2-element heap should yield the min") = forAll { (a: Int, b: Int) =>
    val h1 = insert(a, empty)
    val h2 = insert(b, h1)
    findMin(h2) == Math.min(a, b)
  }

  property("insert and delete from an empty heap should yield an empty heap") = forAll { a: Int =>
    val h1 = insert(a, empty)
    val h2 = deleteMin(h1)
    isEmpty(h2)
  }

  property("findMin and deleteMin from a random heap should yield a sorted sequence") = forAll { h: H =>
    def rec(h: H): List[Int] = h match {
      case h if h == empty => Nil
      case h => findMin(h) :: rec(deleteMin(h))
    }

    val l = rec(h)
    l == l.sorted
  }

  property("findMin a meld heap should yield the min of the mins") = forAll { (h1: H, h2: H) =>
    findMin(meld(h1, h2)) == Math.min(findMin(h1), findMin(h2))
  }

  // additional

  property("2 insertions and 2 deletions should be empty") = forAll { (a: Int) =>
    val h1 = insert(a, insert(a, empty))
    val h2 = deleteMin(h1)
    val h3 = deleteMin(h2)
    isEmpty(h3)
  }

  property("adding an element larger than the minimum should yield minimum") = forAll { (h: H, x: Int) =>
    !isEmpty(h) && x < findMin(h) ==> {
      val heap = insert(x, h)
      findMin(heap) == x
    }
  }

  property("merging two heaps and comparing the same without the minimum with insertion") = forAll { (h1: H, h2: H) =>
    !isEmpty(h1) ==> {
      val minH1 = findMin(h1)
      val h1Mod = deleteMin(h1)
      val h2Mod = insert(minH1, h2)
      heapEquals(meld(h1, h2), meld(h1Mod, h2Mod))
    }
  }

  property("order of the meld arguments does not matter") = forAll { (h1: H, h2: H) =>
    heapEquals(meld(h1, h2), meld(h2, h1))
  }

  property("inserted element is in the heap") = forAll { (h: H, x: Int) =>
    val heap = insert(x, h)
    checkElem(heap, x)
  }

}
