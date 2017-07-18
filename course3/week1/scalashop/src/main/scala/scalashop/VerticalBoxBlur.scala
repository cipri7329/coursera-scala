package scalashop

import org.scalameter._
import common._

object VerticalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 3
    val width = 1920
    val height = 1080
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      VerticalBoxBlur.blur(src, dst, 0, width, radius)
    }
    println(s"sequential blur time: $seqtime ms")

    val numTasks = 64
    val partime = standardConfig measure {
      VerticalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }

}

/** A simple, trivially parallelizable computation. */
object VerticalBoxBlur {

  /** Blurs the columns of the source image `src` into the destination image
   *  `dst`, starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each column, `blur` traverses the pixels by going from top to
   *  bottom.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    // TODO implement this method using the `boxBlurKernel` method
    for(x <- from until end){
      for(y <- 0 until src.height){
        val blured = boxBlurKernel(src, x, y, radius)
        dst(x,y) = blured
      }
    }
  }

  /** Blurs the columns of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  columns.
    *
    *
    *  Use Scala ranges to create a list of splitting points (hint: use the by method on ranges).
    *  Then use collection combinators on the list of splitting points to create a list of start and end tuples,
    *  one for each strip (hint: use the zip and tail methods).
    *
    *  Finally, use the task construct to start a parallel task for each strip,
    *  and then call join on each task to wait for its completion.
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    // TODO implement using the `task` construct and the `blur` method
    val colsPerTaks:Int = Math.max(src.width / numTasks,1)
    val startPoints = Range(0, src.width) by colsPerTaks

    val tasks = startPoints.map(t => {
      task {
        blur(src, dst, t, t + colsPerTaks, radius)
      }
    })

    tasks foreach {_.join}
  }

}
