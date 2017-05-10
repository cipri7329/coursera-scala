package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      i <- arbitrary[A]
      h <- oneOf(const(empty), genHeap)
    } yield insert(i, h)
  )


  lazy val genMap: Gen[Map[Int,Int]] = oneOf(
    const(Map.empty[Int,Int]),
    for {
      k <- arbitrary[Int]
      v <- arbitrary[Int]
      m <- oneOf(const(Map.empty[Int,Int]), genMap)
    } yield m.updated(k, v)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)


  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  /**
    * If you insert any two elements into an empty heap,
    * finding the minimum of the resulting heap
    * should get the smallest of the two elements back.
    */
  property("insert1") = forAll { (h: H, a: A, b: A) =>
    val min = if (a < b) a else b
    val h1 = insert(b, insert(a, h))
    if (isEmpty(h)) {
      findMin(h1) == min
    }
    else {
      val m = findMin(h1)
      m <= min
    }
  }


  /**
    * If you insert an element into an empty heap,
    * then delete the minimum,
    * the resulting heap should be empty.
    */
  property("delete1") = forAll { (h: H, a: A) =>
    if (isEmpty(h)) {
      isEmpty(deleteMin(insert(a, h)))
    }
    else {
      !isEmpty(deleteMin(insert(a, h)))
    }
  }


  /**
    * Given any heap, you should get a sorted sequence of elements
    * when continually finding and deleting minima.
    * (Hint: recursion and helper functions are your friends.)
    */

  property("sorted1") = forAll { (h: H) =>
    def isSorted(h: H): Boolean =
      if (isEmpty(h)) true
      else {
        val m = findMin(h)
        val h2 = deleteMin(h)
        isEmpty(h2) || (m <= findMin(h2) && isSorted(h2))
      }
    isSorted(h)
  }

  /**
    * Put at least 3 different integers in empty heap,
    * and findMin will be the smallest one of those integers.
    * If you deleteMin, findMin should return bigger value.
    *
    * The direction to go is that Bogus4BinomialHeap breaks deleteMin(),
    * so that the element being deleted may not be the right one,
    * which is returned by findMin().
    */

  property("min3") = forAll { (h: H) =>

    val nums3 = for {
      a <- Gen.choose(10,20)
      b <- Gen.choose(2*a, 500)
      c <- Gen.choose(2*b, 5000)
    } yield (a,b,c)

    forAll(nums3) {
      case (a, b, c) => {
        if (isEmpty(h)){
          val hc = insert(c, insert(b, insert(a, h)))
          findMin(deleteMin(hc)) == b
        }
        else {
          val min = findMin(h)
          if (a > min) {
            val hc = insert(c, insert(b, insert(a, h)))
            findMin(deleteMin(hc)) != min
          }
          else {
            val hc = insert(c, insert(b, insert(a, h)))
            findMin(deleteMin(hc)) != a
          }
        }
      }
    }
  }



  /**
    * Finding a minimum of the melding of any two heaps
    * should return a minimum of one or the other.
    */
  property("min1") = forAll { (h1: H, h2: H) =>

    (isEmpty(h1), isEmpty(h2)) match {
      case (true, true) => {
        isEmpty(meld(h1, h2))
      }
      case (true, false) => {
        val m2 = findMin(h2)
        val min = findMin(meld(h1, h2))
        min == m2
      }
      case (false, true) => {
        val m1 = findMin(h1)
        val min = findMin(meld(h1, h2))
        min == m1
      }
      case (false, false) => {
        val m1 = findMin(h1)
        val m2 = findMin(h2)
        val min = findMin(meld(h1, h2))
        min == m1 || min == m2
      }
    }
  }

}
