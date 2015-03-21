import DataStructures.SlopeOneDeviation.Matrix
import Predictions.ItemItemPrediction._
import Strategys.ItemItemStrategys.Deviation
import User.UserPreference
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by Rudie on 21-3-2015.
 */
class ItemItemPrediction$Test extends FlatSpec with Matchers {

  "Predict rating" should "return 3.375" in {
    val matrix = Matrix()
      .addDeviation("Whitney Houston", "Taylor Swift", Deviation(-1, 2))
      .addDeviation("Whitney Houston", "PSY", Deviation(0.75, 2))
      .addDeviation("Whitney Houston", "Whitney Houston", Deviation(0, 2))

    val user = UserPreference("Ben")
      .addRating("Taylor Swift", 5)
      .addRating("PSY", 2)

    predictRating(user, "Whitney Houston", matrix) should be(3.375)
  }

  "Predict rating" should "return 4.6 and 3.4" in {
    val matrix = Matrix()
      .addDeviation("a", "a", Deviation(0, 0))
      .addDeviation("a", "b", Deviation(1, 3))
      .addDeviation("a", "c", Deviation(2, 2))
      .addDeviation("b", "b", Deviation(0, 0))
      .addDeviation("b", "c", Deviation(-1, 3))
      .addDeviation("c", "c", Deviation(0, 0))

    val userOne = UserPreference("Lucy")
      .addRating("b", 2)
      .addRating("c", 5)

    val userTwo = UserPreference("Mark")
      .addRating("a", 3)
      .addRating("b", 4)

    predictRating(userOne, "a", matrix) should be(4.6)
    predictRating(userTwo, "c", matrix) should be(3.4)
  }
}
