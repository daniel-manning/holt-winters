object HoltWinters {

  def forecast(series: Seq[Double], alpha: Double, beta: Double, gamma: Double, seasonLength: Int): Seq[Double] = {
    /*val lAndb:Seq[(Double, Double, Double)] =
      series.tail.foldLeft(Seq((series.head, initialTrend(series, seasonLength), ???))){
        (lb, y) =>

          val lx = alpha*(y - lb.drop(seasonLength).head._3) + (1-alpha)*(lb.last._1 + lb.last._2)
          val bx = beta*(lx - lb.last._1) + (1-beta)*lb.last._2
          val sx = gamma*(y - lx) + (1-gamma)*lb.drop(seasonLength).head._3
          lb :+ (lx, bx, sx)
      }
    series.head +: lAndb.drop(1).zipWithIndex.map(x => x._1._1 + lAndb.drop(x._2 - seasonLength)._3)*/
    ???
  }

  def initialTrend(series: Seq[Double], seasonLength: Int): Double = {
    val firstSeason = series.take(seasonLength)
    val secondSeason = series.drop(seasonLength).take(seasonLength)

    secondSeason.zip(firstSeason).map(x => x._1 - x._2).sum / Math.pow(seasonLength, 2)
  }

  def initialSeasonalComponents(series: Seq[Double], seasonLength: Int): Seq[Double] = {
    val noOfSeasons = Math.ceil(series.length/seasonLength)

    val seasons: Seq[Seq[Double]] = series.grouped(seasonLength).toSeq

    val seasonalAverage: Seq[Double] = seasons.map(_.sum / seasonLength)

    val seasonsCorrected = seasons.zip(seasonalAverage).map(x => x._1.map(y => y / x._2))

    val columnArray = rowListsToColumnLists(seasonsCorrected)

    columnArray.map(x => x.zip(seasonalAverage).map(y => y._1 / y._2).sum / noOfSeasons)
  }

  def rowListsToColumnLists[A](array: Seq[Seq[A]]):Seq[Seq[A]] =
    array.map(_.head) +: (1 until array.head.length).toSeq.map(i => array.map(_.drop(i).head))

}
