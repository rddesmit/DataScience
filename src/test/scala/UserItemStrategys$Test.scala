import UserItemStrategys._
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by Rudie on 3-3-2015.
 */
class UserItemStrategys$Test extends FlatSpec with Matchers {
  "Euclidean Distance" should "return 0.28989794855663564" in {
    val data = List(RatingComparison(2.0, 0.0), RatingComparison(4.0, 3.0), RatingComparison(1.0, 2.0))
    euclideanDistance(data) should be (0.28989794855663564)
  }

  "Manhattan Distance" should "return 0.2" in {
    val data = List(RatingComparison(2.0, 0.0), RatingComparison(4.0, 3.0), RatingComparison(1.0, 2.0))
    manhattanDistance(data) should be (0.2)
  }

  "Pearson Coefficient" should "return 0.9999999999999998" in {
    val data = List(RatingComparison(4.75, 4.0), RatingComparison(4.5, 3.0), RatingComparison(5.0, 5.0), RatingComparison(4.25, 2.0), RatingComparison(4.0, 1.0))
    pearsonCoefficient(data) should be (0.9999999999999998)
  }

  "Cosine Similarity" should "return 0.9351534585705217" in {
    val data = List(RatingComparison(4.75, 4.0), RatingComparison(4.5, 3.0), RatingComparison(5.0, 5.0), RatingComparison(4.25, 2.0), RatingComparison(4.0, 1.0))
    cosineSimilarity(data) should be (0.9351534585705217)
  }
}
