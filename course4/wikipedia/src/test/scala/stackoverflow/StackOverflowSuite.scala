package stackoverflow

import org.scalatest.{FunSuite, BeforeAndAfterAll}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.rdd.RDD
import java.io.File

@RunWith(classOf[JUnitRunner])
class StackOverflowSuite extends FunSuite with BeforeAndAfterAll {


  lazy val testObject = new StackOverflow {
    override val langs =
      List(
        "JavaScript", "Java", "PHP", "Python", "C#", "C++", "Ruby", "CSS",
        "Objective-C", "Perl", "Scala", "Haskell", "MATLAB", "Clojure", "Groovy")
    override def langSpread = 50000
    override def kmeansKernels = 45
    override def kmeansEta: Double = 20.0D
    override def kmeansMaxIterations = 120


    lazy val resFile10k = getClass.getResource("/stackoverflow/stackoverflow10k.csv").getPath
    lazy val lines10k   = StackOverflow.sc.textFile(resFile10k)
    lazy val raw10k: RDD[Posting]   = rawPostings(lines10k)
    lazy val grouped10k: RDD[(Int, Iterable[(Posting, Posting)])] = groupedPostings(raw10k)
    lazy val scored10k: RDD[(Posting, Int)]  = scoredPostings(grouped10k)
    lazy val scored210k: RDD[(Posting, Int)]    = scoredPostings2(raw10k)
    lazy val vectors10k: RDD[(Int, Int)]   = vectorPostings(scored10k)

  }


  test("testObject can be instantiated") {
    val instantiatable = try {
      testObject
      true
    } catch {
      case _: Throwable => false
    }
    assert(instantiatable, "Can't instantiate a StackOverflow object")
  }

  test("'rawPostings' should work.") {
    val raw = testObject.raw10k
    val rawCount = raw.count()
    val firstId = raw.first().id
    val res1 = rawCount == 10000
    val res2 = firstId == 27233496
    assert(res1, s"expected=10000 found=$rawCount")
    assert(res2, s"expected=27233496 found=$firstId")
  }

  test("'groupedPostings' should work.") {
    val grouped = testObject.grouped10k
    val groupedCount = grouped.count()
    val res1 = groupedCount == 2576
    val firstTuple = grouped.sortBy((tup: (Int, Iterable[(Posting, Posting)])) => tup._1).first()
    val res2Count = firstTuple._2.size
    val res2 = res2Count == 12
    assert(res1, s"expected=2576 found=$groupedCount")
    assert(res2, s"expected=2 found=$res2Count")
  }

  test("'scoredPostings' should work.") {
    val scored = testObject.scored10k
    val scoredCount = scored.count()
    val res1 = scoredCount == 2576
    val firstTupleScore = scored.sortBy((tup: (Posting, Int)) => tup._2, ascending = false).first()._2
    val res2 = firstTupleScore == 575
    assert(res1, s"expected=2576 found=$scoredCount")
    assert(res2, s"expected=575 found=$firstTupleScore")
  }

  test("'scoredPostings2' should work.") {
    val scored2 = testObject.scored210k
    val scoredCount = scored2.count()
    val res1 = scoredCount == 2576
    val firstTupleScore = scored2.sortBy((tup: (Posting, Int)) => tup._2, ascending = false).first()._2
    val res2 = firstTupleScore == 575
    assert(res1, s"expected=2576 found=$scoredCount")
    assert(res2, s"expected=575 found=$firstTupleScore")
  }

  test("'vectorPostings' should work.") {
    val vectors = testObject.vectors10k
    val res1 = vectors.count() == 2576
    val res2 = vectors.sortBy((tup: (Int, Int)) => tup._1).first() == (100000, 3)
    assert(res1)
    assert(res2)
  }

  test("'kmeans' should work.") {
    val a = Array.range(0, 45)
    val b = a.map(_ * 50000)
    val means = b zip a
    val vectors = testObject.vectors10k
    val newmeans = testObject.kmeans(means, vectors)
    val res1 = newmeans.length == 45
    val res2 = newmeans(2) == (100000, 3) && newmeans(5) == (250000, 4)
    assert(res1)
    assert(res2)
  }

  test("'clusterResults' should work.") {
    val a = Array.range(0, 45)
    val b = a.map(_ * 50000)
    val means = b zip a
    means(2) = (100000, 3)
    means(5) = (250000, 4)
    val vectors = testObject.vectors10k
    val result = testObject.clusterResults(means, vectors)
    val res1 = result(0) == ("PHP", 100.0, 1, 3)
    val res2 = result(1) == ("C++", 100.0, 1, 4)
    assert(res1)
    assert(res2)
  }


  val fixture = new {
    lazy val resFileAll = getClass.getResource("/stackoverflow/stackoverflow.csv").getPath
    lazy val linesAll   = StackOverflow.sc.textFile(resFileAll)

    val raw: RDD[Posting] = testObject.rawPostings(linesAll)
  }

  test("scoredPostings should contain expected values") {
    val f = fixture

    val grouped = testObject.groupedPostings(f.raw)
    val scored: RDD[(Posting, Int)] = testObject.scoredPostings(grouped)
    val map = scored.collectAsMap()

    val shouldContain = List(
      ((1,6,None,None,140,Some("CSS")),67),
      ((1,42,None,None,155,Some("PHP")),89),
      ((1,72,None,None,16,Some("Ruby")),3),
      ((1,126,None,None,33,Some("Java")),30),
      ((1,174,None,None,38,Some("C#")),20)
    )

    shouldContain.foreach { elem =>
      val posting = new Posting(
        elem._1._1, elem._1._2, elem._1._3,
        elem._1._4, elem._1._5, elem._1._6)
      assert(map.contains(posting))
      assert(map.get(posting).get == elem._2)
    }
  }

  test("vectorPostings should contain expected values") {
    val f = fixture

    val grouped = testObject.groupedPostings(f.raw)
    val scored = testObject.scoredPostings(grouped)
    val vectors: RDD[(Int, Int)] = testObject.vectorPostings(scored)

    val counted = vectors.count()
    assert ( counted == 2121822, s"count failed. expected=2121822 found=$counted")

    val map = vectors.collect()

    val shouldContain = List(
      (350000,67),
      (100000,89),
      (300000,3),
      (50000,30),
      (200000,20)
    )

    val score5k = map.filter{ case(lang, score) => lang > 50000}
    val score5kcounted = score5k.length
    assert ( score5kcounted > 4, s"score5kcounted failed. expected=5 found=$score5kcounted")

    shouldContain.foreach { elem =>
      assert(map.contains(elem), s"element not found: $elem")
    }

  }

}
