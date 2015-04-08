package Predictions

import User.UserPreference

/**
 * Created by Rudie on 10-3-2015.
 */
object UserItemPrediction {

  /**
   * Returns the top predicted rated products
   * @param nearestNeighbours list of nearest neighbours to the target user
   * @param target target [[UserPreference]]
   * @param threshold minimum amount of items the prediction must be based on
   * @param amount max predicted product ratings
   */
  def predictTopRatings(nearestNeighbours: List[UserPreference], target: UserPreference, threshold: Int, amount: Int) =
    nearestNeighbours
      .par
      .flatMap(u => u.ratings.map(r => ProductRatingDistance(r._1, r._2, u.distance)))
      .filterNot(r => target.hasRating(r.id))
      .groupBy(_.id)
      .filter(_._2.size >= threshold)
      .map(g => predictRating(g._1, g._2 toList))
      .toList
      .sortWith(_.rating >= _.rating)
      .take(amount)

  /** Returns the [[PredictedRating]] rating for this data set */
  private def predictRating(id: String, data: List[ProductRatingDistance]) = {
    val RatingDistances = data.map(r => RatingDistance(r.rating, r.distance))
    val predictedRating = predictRating(RatingDistances)
    PredictedRating(id, predictedRating)
  }

  /** Returns the predicted user rating*/
  def predictRating(data: List[RatingDistance]) = {
    val total = (0.0 /: data)((r, c) => r + c.distance)
    (0.0 /: data)((r, c) => r + (c.rating * (c.distance / total)))
  }

  /**
   * Returns the predicted rating for one item
   * @param nearestNeighbours list of nearest neighbours to the target user
   * @param target target [[UserPreference]]
   * @param id item to predict to rating to
   */
  def predictOneRating(nearestNeighbours: List[UserPreference], target: UserPreference, id: String) = {
    val data = nearestNeighbours
      .par
      .flatMap(u => u.ratings.map(r => ProductRatingDistance(r._1, r._2, u.distance)))
      .filter(r => !target.hasRating(r.id) && r.id == id)
      .toList

    predictRating(id, data)
  }

  case class RatingDistance(rating: Double, distance: Double)

  private case class ProductRatingDistance(id: String, rating: Double, distance: Double)
}
