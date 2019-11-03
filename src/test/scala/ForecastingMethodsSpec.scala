import org.scalatest.{FreeSpec, MustMatchers}

class ForecastingMethodsSpec extends FreeSpec with MustMatchers {

  val inputSequence = Seq(3,10,12,13,12,10,12)

  "Naive Method"  - {
    "when given a series" - {
      "should repeat it's last value" in {
        ForcastingMethods.naive(inputSequence) mustEqual 12
      }
    }
  }

  "Average"  - {
    "when given a series of values" - {
      "should give it's mean" in {
        ForcastingMethods.average(inputSequence.map(_.toDouble)) mustEqual 10.285714285714286
      }
    }
  }

  "Moving Average"  - {
    "when given a series of values" - {
      "should give it 3 value window" in {
        ForcastingMethods.movingAverage(inputSequence.map(_.toDouble), 3) mustEqual 11.333333333333334
      }

      "should give it 4 value window" in {
        ForcastingMethods.movingAverage(inputSequence.map(_.toDouble), 4) mustEqual 11.75
      }
    }
  }

  "Weighted Moving Average"  - {
    "when given a series of values and weights" - {
      "should calculate correctly" in {
        ForcastingMethods.weightedMovingAverage(inputSequence.map(_.toDouble), Seq(0.1, 0.2, 0.3, 0.4)) mustEqual 11.5 +- 0.0001 //floating point equivalence nonsense
      }
    }
  }



}
