package Predictions

import DataStructures.SlopeOneMatrix
import User.UserPreference

/**
 * Created by Rudie on 21-3-2015.
 */
object ItemItemPrediction {

  /** Returns the top N [[PredictedRating]] for the target [[UserPreference]] */
  def predictTopRatings(target: UserPreference, items: List[String], matrix: SlopeOneMatrix, amount: Int) = {
    items
      .par
      .filterNot(i => target.hasRating(i))
      .map(i => PredictedRating(i, predictRating(target, i, matrix)))
      .toList
      .sortWith(_.rating >= _.rating)
      .take(amount)
  }

  /** Returns the predict rating for the given item and [[UserPreference]] based on the [[SlopeOneMatrix]] */
  def predictRating(target: UserPreference, item: String, matrix: SlopeOneMatrix) = {
    val result = (PredictRating(0, 0) /: target.ratings)((r, c) => {
      if (matrix hasDeviation(item, c._1)) {
        val deviation = matrix getDeviation(item, c._1)
        PredictRating(
          r.numerator + (c._2 + deviation.deviation) * deviation.amount,
          r.denominator + deviation.amount
        )
      } else r
    })

    result.numerator / result.denominator
  }

  private case class PredictRating(numerator: Double, denominator: Double)

}
