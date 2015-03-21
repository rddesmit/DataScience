import User.UserPreference
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by Rudie on 17-3-2015.
 */
class UserPreference$Test extends FlatSpec with Matchers {

  "average" should "return average" in {
    val userOne = UserPreference("Amy").addRating("a", 3.5).addRating("b", 3.0).addRating("c", 4.0)
    val userTwo = UserPreference("Bill").addRating("a", 2.0).addRating("b", 5.0).addRating("c", 2.0)
    val userThree = UserPreference("Clara").addRating("a", 3).addRating("c", 5.0)

    userOne.averageRating should be(3.5)
    userTwo.averageRating should be(3.0)
    userThree.averageRating should be(4.0)
  }
}
