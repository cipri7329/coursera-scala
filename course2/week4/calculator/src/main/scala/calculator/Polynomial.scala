package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
                   c: Signal[Double]): Signal[Double] = {

    Signal{
      val delta = math.pow(b(), 2) - 4 * a() * c()
      delta
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    //(-b ± √Δ) / 2a
    Signal {
      val deltaValue = delta()
      if (deltaValue < 0) Set()
      else {
        val bVal = b()
        val aVal = a()

        Set(
          (-bVal + math.sqrt(deltaValue)) / (2 * aVal),
          (-bVal - math.sqrt(deltaValue)) / (2 * aVal)
        )
      }
    }
  }

}
