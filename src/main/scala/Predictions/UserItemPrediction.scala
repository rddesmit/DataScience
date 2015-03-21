package Predictions

import User.UserPreference

/**
 * Created by Rudie on 10-3-2015.
 */
object UserItemPrediction {

  /**
   * Returns the top predicted rated products
   * @param nearestNeighbours list of nearest neighbours to the target user
   * @param target target user
   * @param amount max predicted product ratings
   * @return
   */
  def predictRatings(nearestNeighbours: List[UserPreference], target: UserPreference, threshold: Int, amount: Int) =
    nearestNeighbours
      .par
      .flatMap(u => u.ratings.map(r => ProductRatingDistance(r._1, r._2, u.distance)))
      .filter(r => !target.hasRating(r.id))
      .groupBy(r => r.id)
      .filter(r => r._2.size > threshold)
      .map(g => PredictedRating(g._1, predictRating(g._2.map(r => RatingDistance(r.rating, r.distance)) toList)))
      .toList
      .sortWith(_.rating >= _.rating)
      .take(amount)

  /** Returns the predicted user rating*/
  def predictRating(data: List[RatingDistance]) = {
    val total = (0.0 /: data)((r, c) => r + c.distance)
    (0.0 /: data.map(r => r.rating * (r.distance / total)))((r, c) => r + c)
  }

  case class RatingDistance(rating: Double, distance: Double)

  private case class ProductRatingDistance(id: String, rating: Double, distance: Double)
}
