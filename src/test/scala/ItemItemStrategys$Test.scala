import Strategys.ItemItemStrategys._
import User.UserPreference
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by Rudie on 17-3-2015.
 */
class ItemItemStrategys$Test extends FlatSpec with Matchers {
  "Slope one" should "return 2" in {
    val userOne = UserPreference("Amy").addRating("Taylor Swift", 4).addRating("PSY", 3).addRating("Whitney Houston", 4)
    val userTwo = UserPreference("Ben").addRating("Taylor Swift", 5).addRating("PSY", 2)
    val userThree = UserPreference("Clara").addRating("PSY", 3.5).addRating("Whitney Houston", 4)
    val userFour = UserPreference("Daisy").addRating("Taylor Swift", 5).addRating("Whitney Houston", 3)

    slopeOne("Taylor Swift", "PSY", List(userOne, userTwo, userThree, userFour)) should be(Deviation(2, 2))
  }

  "acs" should "return -0.9701425001453319" in {
    val userOne = UserPreference("Amy").addRating("a", 3.5).addRating("b", 3.0).addRating("c", 4.0)
    val userTwo = UserPreference("Bill").addRating("a", 2.0).addRating("b", 5.0).addRating("c", 2.0)
    val userThree = UserPreference("Clara").addRating("a", 3).addRating("c", 5.0)

    acs("a", "b", List(userOne, userTwo, userThree)) should be(-0.9701425001453319)
  }
}
