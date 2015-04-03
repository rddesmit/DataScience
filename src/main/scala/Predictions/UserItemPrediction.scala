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
   * @param threshold minimum amount of items the prediction must be based on
   * @param amount max predicted product ratings
   */
  def predictRatings(nearestNeighbours: List[UserPreference], target: UserPreference, threshold: Int, amount: Int) =
    nearestNeighbours
      .par
      .flatMap(u => u.ratings.map(r => ProductRatingDistance(r._1, r._2, u.distance)))
      .filterNot(r => target.hasRating(r.id))
      .groupBy(_.id)
      .filter(_._2.size >= threshold)
      .map(g => ProductRatingGroup(g._1, g._2.map(r => RatingDistance(r.rating, r.distance)) toList))
      .map(g => PredictedRating(g.id, predictRating(g.ratings)))
      .toList
      .sortWith(_.rating >= _.rating)
      .take(amount)

  /** Returns the predicted user rating*/
  def predictRating(data: List[RatingDistance]) = {
    val total = (0.0 /: data)((r, c) => r + c.distance)
    (0.0 /: data)((r, c) => r + (c.rating * (c.distance / total)))
  }

  case class RatingDistance(rating: Double, distance: Double)

  private case class ProductRatingGroup(id: String, ratings: List[RatingDistance])
  private case class ProductRatingDistance(id: String, rating: Double, distance: Double)
}
