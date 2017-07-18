package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    @tailrec
    def balanceCount(idx: Int, count: Int): Int = {
      if(count == -1) -1
      else if(idx == chars.length) count
      else if(chars(idx) == '(') balanceCount(idx + 1, count + 1)
      else if(chars(idx) == ')') balanceCount(idx + 1, count - 1)
      else balanceCount(idx + 1, count)
    }

    balanceCount(0, 0) == 0
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {


    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int): (Int, Int) = {
      var minNest = 0
      var totNest = 0
      for(current <- idx until until) {
        if(chars(current) == '(') totNest = totNest + 1
        if(chars(current) == ')') totNest = totNest - 1
        if(totNest < minNest) minNest = totNest //keep track of closed parenthesis
      }

      (minNest, totNest)
    }



    def reduce(from: Int, until: Int): (Int, Int) = {
      if(until - from <= threshold) traverse(from, until, 0, 0)
      else {
        val middle = from + (until-from)/2
        val (resL, resR) = parallel(reduce(from, middle), reduce(middle, until))
        (Math.min(resL._1, resL._2 + resR._1), resL._2 + resR._2)
      }
    }

    reduce(0, chars.length) == (0, 0)

  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
