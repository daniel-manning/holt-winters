object ForecastingMethods {

  def naive[A](series: Seq[A]): A = series.last

  def average(series: Seq[Double]):Double = {
    series.sum/series.length
  }

  def movingAverage(series: Seq[Double], window: Int): Double = {
    average(series.reverse.take(window))
  }

  def weightedMovingAverage(series: Seq[Double], weights: Seq[Double]): Double =
    weights.reverse.zip(series.reverse).map(x => x._1 * x._2).sum

  def exponentialSmoothing(series: Seq[Double], alpha: Double): Seq[Double] ={
    series.tail.foldLeft(Seq(series.head)){
      (b, a) => b :+ (alpha*a + (1 - alpha)*b.last)
    }
  }

}
