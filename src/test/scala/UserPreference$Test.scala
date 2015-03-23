import Strategys.UserItemStrategys.RatingComparison
import User.UserPreference
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by Rudie on 17-3-2015.
 */
class UserPreference$Test extends FlatSpec with Matchers {

  "Average" should "return average" in {
    val userOne = UserPreference("Amy").addRating("a", 3.5).addRating("b", 3.0).addRating("c", 4.0)
    val userTwo = UserPreference("Bill").addRating("a", 2.0).addRating("b", 5.0).addRating("c", 2.0)
    val userThree = UserPreference("Clara").addRating("a", 3).addRating("c", 5.0)

    userOne.averageRating should be(3.5)
    userTwo.averageRating should be(3.0)
    userThree.averageRating should be(4.0)
  }

  "Has different ratings" should "return TRUE" in {
    val userOne = UserPreference("Amy").addRating("a", 3.5).addRating("b", 3.0).addRating("c", 4.0)
    val userTwo = UserPreference("Clara").addRating("a", 3).addRating("c", 5.0)

    userOne.hasDiffRatings(userTwo) should be(true)
  }

  "Has different ratings" should "return FALSE" in {
    val userOne = UserPreference("Amy").addRating("a", 3.5).addRating("b", 3.0).addRating("c", 4.0)
    val userTwo = UserPreference("Bill").addRating("a", 2.0).addRating("b", 5.0).addRating("c", 2.0)

    userOne.hasDiffRatings(userTwo) should be(false)
  }

  "To rating comparisons" should "return a ordered list" in {
    val userOne = UserPreference("Amy").addRating("a", 3.5).addRating("b", 3.0).addRating("c", 4.0)
    val userTwo = UserPreference("Bill").addRating("c", 2.0).addRating("a", 2.0).addRating("b", 5.0)

    userOne.toRatingComparisons(userTwo) should be(List(RatingComparison(3.5, 2.0), RatingComparison(3.0, 5.0), RatingComparison(4.0, 2.0)))
  }
}
