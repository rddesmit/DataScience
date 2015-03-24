import DataStructures.SlopeOneDeviation
import Strategys.ItemItemStrategys._
import User.UserPreference
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by Rudie on 21-3-2015.
 */
class SlopeOneDeviationTest extends FlatSpec with Matchers {

  "SlopeOneDeviation" should "be filled" in {
    val userOne = UserPreference("Amy").addRating("Taylor Swift", 4).addRating("PSY", 3).addRating("Whitney Houston", 4)
    val userTwo = UserPreference("Ben").addRating("Taylor Swift", 5).addRating("PSY", 2)
    val userThree = UserPreference("Clara").addRating("PSY", 3.5).addRating("Whitney Houston", 4)
    val userFour = UserPreference("Daisy").addRating("Taylor Swift", 5).addRating("Whitney Houston", 3)

    val users = List(userOne, userTwo, userThree, userFour)
    val items = List("Taylor Swift", "PSY", "Whitney Houston")

    val matrix = (SlopeOneDeviation() /: items.flatMap(i => items.map(j => (i, j, slopeOne(i, j, users)))))((r, c) => r.addDeviation(c._1, c._2, c._3))

    matrix.getDeviation("Taylor Swift", "Taylor Swift") should be(Deviation(0, 3))
    matrix.getDeviation("Taylor Swift", "PSY") should be(Deviation(2, 2))
    matrix.getDeviation("Taylor Swift", "Whitney Houston") should be(Deviation(1, 2))
    matrix.getDeviation("PSY", "Taylor Swift") should be(Deviation(-2, 2))
    matrix.getDeviation("Whitney Houston", "Taylor Swift") should be(Deviation(-1, 2))
  }
}
