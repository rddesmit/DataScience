import scala.collection.immutable.HashMap
import RichDataStructures.RichHashMap._

/**
 * Created by Rudie on 3-3-2015.
 */
object NearestNeighbour {
  /**
   * Returns the nearest neighbours
   * @param userPreferences list of preferences to compare the user with
   * @param target user to compare
   * @param strategy user similarity strategy to use
   * @param threshold minimal user similarity
   * @param amount maximal amount of neighbours
   * @return
   */
  def nearestNeighbours(userPreferences: List[UserPreference], target: UserPreference, strategy: List[(Double, Double)] => Double, threshold: Double, amount: Int) = {
    userPreferences
      .filter(u => u.id != target.id && u.ratings.diff(target.ratings).size > 0)
      .map(u => u.copy(distance = strategy(u.ratings.zipper(target.ratings).values toList)))
      .filter(u => u.distance >= threshold)
      .sortWith(_.distance > _.distance)
      .take(amount)
  }

  /**
   * Returns the top predicted rated products
   * @param nearestNeighbours list of nearest neighbours to the target user
   * @param target target user
   * @param amount max predicted product ratings
   * @return
   */
  def predictRatings(nearestNeighbours: List[UserPreference], target: UserPreference, amount: Int) = {
    nearestNeighbours
      .flatMap(u => u.ratings.map(r => ProductRatingDistance(r._1, r._2, u.distance)))
      .filter(r => !target.ratings.contains(r.id))
      .groupBy(r => r.id)
      .map(g => PredictedRating(g._1, predictRating(g._2.map(r => RatingDistance(r.rating, r.distance)))))
      .toList
      .sortWith(_.rating > _.rating)
      .take(amount)
  }

  /** Returns the predicted user rating*/
  def predictRating(data: List[RatingDistance]) = {
    val total = (0.0 /: data)((r,c) => r + c.distance)
    (0.0 /: data.map(r => r.rating * (r.distance / total)))((r,c) => r + c)
  }

  case class RatingDistance(rating: Double, distance: Double)
  case class PredictedRating(id: String, rating: Double)
  case class ProductRatingDistance(id: String, rating: Double, distance: Double)
}
