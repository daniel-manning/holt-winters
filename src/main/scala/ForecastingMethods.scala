object ForecastingMethods {

  def naive[A](series: Seq[A]): A = series.last

  def average(series: Seq[Double]): Double = {
    series.sum / series.length
  }

  def movingAverage(series: Seq[Double], window: Int): Double = {
    average(series.reverse.take(window))
  }

  def weightedMovingAverage(series: Seq[Double], weights: Seq[Double]): Double =
    weights.reverse.zip(series.reverse).map(x => x._1 * x._2).sum

  def exponentialSmoothing(series: Seq[Double], alpha: Double): Seq[Double] = {
    series.tail.foldLeft(Seq(series.head)) {
      (b, a) => b :+ (alpha * a + (1 - alpha) * b.last)
    }
  }

  def doubleExponentialSmoothing(series: Seq[Double], alpha: Double, beta: Double): Seq[Double] = {
    val lAndb: Seq[(Double, Double)] =
      series.tail.foldLeft(Seq((series.head, series.drop(1).head - series.head))) {
        (lb, y) =>

          val lx = alpha * y + (1 - alpha) * (lb.last._1 + lb.last._2)
          val bx = beta * (lx - lb.last._1) + (1 - beta) * lb.last._2

          lb :+ (lx, bx)
      }
    series.head +: lAndb.drop(1).map(x => x._1 + x._2)
  }

}