package calculator

object Polynomial {

  def computeDelta(a: Signal[Double], b: Signal[Double], c: Signal[Double]): Signal[Double] = Signal {
    val _b = b()
    _b * _b - 4 * a() * c()
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double], c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = Signal {
    val _delta = delta()

    if (_delta >= 0) {
      val sqrtDelta = math.sqrt(_delta)
      val minusB = -b()
      val twoA = 2 * a()

      Set(-1, 1).map(x => (minusB + x * sqrtDelta) / twoA)
    }

    else Set()
  }
}
