package Predictions

import DataStructures.SlopeOneDeviation.Matrix
import User.UserPreference

/**
 * Created by Rudie on 21-3-2015.
 */
object ItemItemPrediction {

  def predictRatings(target: UserPreference, items: List[String], matrix: Matrix, amount: Int) = {
    items
      .par
      .filter(i => !target.hasRating(i))
      .map(i => PredictedRating(i, predictRating(target, i, matrix)))
      .filter(r => !r.rating.isNaN)
      .toList
      .sortWith(_.rating >= _.rating)
      .take(amount)
  }

  def predictRating(target: UserPreference, item: String, matrix: Matrix) = {
    val result = (PredictRating(0, 0) /: target.ratings)((r, c) => {
      val deviation = matrix.getDeviation(item, c._1)
      PredictRating(
        r.numerator + (c._2 + deviation.deviation) * deviation.amount,
        r.denominator + deviation.amount
      )
    })

    result.numerator / result.denominator
  }

  private case class PredictRating(numerator: Double, denominator: Double)

}
