import org.scalatest.{FreeSpec, MustMatchers}

class ForecastingMethodsSpec extends FreeSpec with MustMatchers {

  val inputSequence = Seq(3,10,12,13,12,10,12).map(_.toDouble)

  "Naive Method"  - {
    "when given a series" - {
      "should repeat it's last value" in {
        ForecastingMethods.naive(inputSequence) mustEqual 12
      }
    }
  }

  "Average"  - {
    "when given a series of values" - {
      "should give it's mean" in {
        ForecastingMethods.average(inputSequence) mustEqual 10.285714285714286
      }
    }
  }

  "Moving Average"  - {
    "when given a series of values" - {
      "should give it 3 value window" in {
        ForecastingMethods.movingAverage(inputSequence, 3) mustEqual 11.333333333333334
      }

      "should give it 4 value window" in {
        ForecastingMethods.movingAverage(inputSequence, 4) mustEqual 11.75
      }
    }
  }

  "Weighted Moving Average"  - {
    "when given a series of values and weights" - {
      "should calculate correctly" in {
        ForecastingMethods.weightedMovingAverage(inputSequence, Seq(0.1, 0.2, 0.3, 0.4)) mustEqual 11.5 +- 0.0001 //floating point equivalence nonsense
      }
    }
  }

  "Exponential Smoothing" - {
    "when given a low alpha" - {
      "should calculate the average correctly" in {
        ForecastingMethods.exponentialSmoothing(inputSequence, 0.1) mustEqual Seq(3, 3.7, 4.53, 5.377, 6.0393, 6.43537, 6.991833)
      }
    }

    "when given a high alpha" - {
      "should calculate the average correctly" in {
        ForecastingMethods.exponentialSmoothing(inputSequence, 0.9) mustEqual Seq(3, 9.3, 11.73, 12.873000000000001, 12.0873, 10.20873, 11.820873)
      }
    }
  }



}
