package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.forAll

import scala.annotation.tailrec

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap:
  lazy val genHeap: Gen[H] =  for{
    v <-Arbitrary.arbitrary[A]
    m <- Gen.oneOf(Gen.const(empty), genHeap)
  } yield insert (v,m)

  given Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll {(a:A) =>
    val m = insert(a, empty)
    findMin(m)==a
  }

  /**
    * If you insert any two elements into an empty heap, finding the minimum of the resulting heap should get the smallest of the two elements back.
    */
  property("minOfTwoElem") = forAll {(x1:A, x2:A) =>
    val min = if x1<x2 then x1 else x2
    val heap = insert(x1,insert(x2,empty))
    findMin(heap)==min
  }

  /**
    * If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.
    */
  property("deleteTheMinimum") = forAll { (x: A) =>
    isEmpty(deleteMin(insert(x, empty)))
  }

  /**
  Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima. (Hint: recursion and helper functions are your friends.)
  */

  property("sorting") = forAll { (x1: A, x2: A, x3: A) =>
    def insertAll(elem: List[Int], h: H): H =
      elem match {
        case Nil => h
        case x:: xs => insertAll(xs, insert(x, h))
      }
    def deleteMinRecur(h: H, acc: List[A]): List[A] =
      if isEmpty(h) then acc
      else deleteMinRecur(deleteMin(h), acc :+ findMin(h))
    val list = List(x1, x2, x3)
    val heap = insertAll(list, empty)
    val elem = deleteMinRecur(heap, List())
    elem == list.sorted
  }

  /**
    * Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
  */
  property("minimumOfMelding") = forAll{ (h1:H, h2:H)=>
    val minOfMeld = findMin(meld(h1,h2))
    if (isEmpty(h1) || isEmpty(h2)) true
    else{
      val min1 = findMin(h1)
      val min2 = findMin(h2)
      minOfMeld==min1 || minOfMeld== min2
    }
  }
 /**
   *
   *
   *   property("contains all inserted elements") = forAll { (a: Int, b: Int) =>
    val list = List(a, b)
    val h = insertAll(list, empty)
    val elements = deleteRecur(h, List())
    list.toSet == elements.toSet
  }
 */


