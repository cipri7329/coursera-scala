package funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s4 = singletonSet(4)

    val s10 = singletonSet(10)
    val s15 = singletonSet(15)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
      assert(contains(s2, 2), "Singleton")
      assert(contains(s3, 3), "Singleton")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s1_2 = union(s1, s2)
      assert(contains(s1_2, 1), "Union 1")
      assert(contains(s1_2, 2), "Union 2")
      assert(!contains(s1_2, 3), "Union 3")

      val s3t10 = union(s3, s10)
      assert(contains(s3t10, 3), "Union s3t10 3")
      assert(contains(s3t10, 10), "Union s3t10 10")
      assert(!contains(s3t10, 2), "Union s3t10 2")
      assert(!contains(s3t10, 11), "Union s3t10 11")

      val s1t2t3t10 = union(s1_2, s3t10)
      assert(contains(s1t2t3t10, 1), "Union s1t2t3t10 1")
      assert(contains(s1t2t3t10, 2), "Union s1t2t3t10 2")
      assert(contains(s1t2t3t10, 3), "Union s1t2t3t10 3")
      assert(contains(s1t2t3t10, 10), "Union s1t2t3t10 10")

      assert(!contains(s1t2t3t10, 0), "Union s1t2t3t10 0")
      assert(!contains(s1t2t3t10, 4), "Union s1t2t3t10 4")
    }
  }



  test("intersect - contains only common elements of each set") {
    new TestSets {
      val s1_2 = union(s1, s2)
      val s3t10 = union(s3, s10)
      val s1t2t3t10 = union(s1_2, s3t10)

      val s2_3 = union(s2, s3)
      val s1_2_4 = union(s1_2, s4)

      val i23 = intersect(s2_3, s1t2t3t10)
      assert(contains(i23, 2), "Intersect i23 2")
      assert(contains(i23, 3), "Intersect i23 3")
      assert(!contains(i23, 1), "Intersect i23 1")
      assert(!contains(i23, 10), "Intersect i23 10")

      val i12 = intersect(s1_2_4, s1t2t3t10)
      assert(contains(i12, 1), "Intersect i23 1")
      assert(contains(i12, 2), "Intersect i23 2")
      assert(!contains(i12, 4), "Intersect i23 4")
      assert(!contains(i12, 3), "Intersect i23 3")
      assert(!contains(i12, 10), "Intersect i23 10")
    }
  }


  test("diff - contains all elements of `s` that are not in `t`") {
    new TestSets {
      val s1_2 = union(s1, s2)
      val s3t10 = union(s3, s10)
      val s1t2t3t10 = union(s1_2, s3t10)

      val s2_3 = union(s2, s3)
      val s1_2_4 = union(s1_2, s4)

      val i23 = diff(s2_3, s1t2t3t10) //empty set
      assert(!contains(i23, 2), "Intersect i23 2")
      assert(!contains(i23, 3), "Intersect i23 3")
      assert(!contains(i23, 1), "Intersect i23 1")
      assert(!contains(i23, 10), "Intersect i23 10")

      val i12 = diff(s1_2_4, s1t2t3t10)
      assert(!contains(i12, 1), "Intersect i23 1")
      assert(!contains(i12, 2), "Intersect i23 2")
      assert(contains(i12, 4), "Intersect i23 4")
      assert(!contains(i12, 3), "Intersect i23 3")
      assert(!contains(i12, 10), "Intersect i23 10")
    }
  }


  test("filter - Returns the subset of `s` for which `p` holds.") {
    new TestSets {
      val s1_2 = union(s1, s2)
      val s3t10 = union(s3, s10)
      val s1t2t3t10 = union(s1_2, s3t10)

      val s2_3 = union(s2, s3)
      val s1_2_4 = union(s1_2, s4)

      val i23 = filter(s2_3, _ > 2) //empty set
      assert(!contains(i23, 2), "Intersect i23 2")
      assert(contains(i23, 3), "Intersect i23 3")
      assert(!contains(i23, 1), "Intersect i23 1")
      assert(!contains(i23, 10), "Intersect i23 10")

      val i12 = filter(s1_2_4, _ % 2 == 0)
      assert(!contains(i12, 1), "Intersect i23 1")
      assert(contains(i12, 2), "Intersect i23 2")
      assert(contains(i12, 4), "Intersect i23 4")
      assert(!contains(i12, 3), "Intersect i23 3")
      assert(!contains(i12, 10), "Intersect i23 10")
    }
  }


  test("forall - Returns whether all bounded integers within `s` satisfy `p`") {
    new TestSets {
      val s1_2 = union(s1, s2)
      val s3t10 = union(s3, s10)
      val s1t2t3t10 = union(s1_2, s3t10)

      val s2_3 = union(s2, s3)
      val s1_2_4 = union(s1_2, s4)


      assert(forall(s1_2_4, _ < 5), "forall s1_2_4 < 5")
      assert(!forall(s1_2_4, _ < 4), "forall s1_2_4 < 4")
      assert(forall(s1t2t3t10, _ < 15), "forall s1t2t3t10 < 15")
      assert(!forall(s1t2t3t10, _ < 0), "forall s1t2t3t10 < 0")
    }
  }



  test("exists - Returns whether there exists a bounded integer within `s` that satisfies `p`.") {
    new TestSets {
      val s1_2 = union(s1, s2)
      val s3t10 = union(s3, s10)
      val s1t2t3t10 = union(s1_2, s3t10)

      val s2_3 = union(s2, s3)
      val s1_2_4 = union(s1_2, s4)


      assert(exists(s1_2_4, _ < 5), "exists s1_2_4 < 5")
      assert(!exists(s1_2_4, _ < 1), "exists s1_2_4 < 1")
      assert(exists(s1t2t3t10, _ < 15), "exists s1t2t3t10 < 15")
      assert(!exists(s1t2t3t10, _ == 4), "exists s1t2t3t10 == 4")
    }
  }


  test("map - Returns a set transformed by applying `f` to each element of `s`") {
    new TestSets {
      val s1_2 = union(s1, s2)
      val s3t10 = union(s3, s10)
      val s1t2t3t10 = union(s1_2, s3t10)

      val s2_3 = union(s2, s3)
      val s1_2_4 = union(s1_2, s4)

      printSet(s1_2_4)
      printSet(map(s1_2_4, _ * 2))

      assert(contains(map(s1_2_4, _ * 2), 8), "map s1_2_4 _ * 2")
      assert(contains(map(s1_2_4, _ * 3), 3), "map s1_2_4 _ * 2")
      assert(contains(map(s1_2_4, _ * 3), 6), "map s1_2_4 _ * 2")
      assert(contains(map(s1_2_4, _ * 3), 12), "map s1_2_4 _ * 2")
      assert(!contains(map(s1_2_4, _ * 3), 1), "map s1_2_4 _ * 2")
      assert(!contains(map(s1_2_4, _ * 3), 2), "map s1_2_4 _ * 2")
      assert(!contains(map(s1_2_4, _ * 3), 4), "map s1_2_4 _ * 2")

    }
  }



}
