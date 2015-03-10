import org.scalatest.{Matchers, FlatSpec, FunSuite}
import NearestNeighbour._

/**
 * Created by Rudie on 4-3-2015.
 */
class NearestNeighbour$Test extends FlatSpec with Matchers {
  "Predict rating" should "return 2.225" in {
    val data = List(RatingDistance(5.0, 0.3), RatingDistance(2.0, 0.8), RatingDistance(1.5, 0.9))
    predictRating(data) should be(2.225)
  }
}
