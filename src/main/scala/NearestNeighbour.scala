import Strategys.UserItemStrategys._
import User.UserPreference

/**
 * Created by Rudie on 3-3-2015.
 */
object NearestNeighbour {
  /**
   * Returns the nearest neighbours
   * @param userPreferences [[List]] of [[UserPreference]] to compare the target [[UserPreference]] with
   * @param target [[UserPreference]] to compare
   * @param strategy user-item similarity strategy to use
   * @param threshold minimal user-item similarity
   * @param amount maximal amount of nearest neighbours
   */
  def nearestNeighbours(userPreferences: List[UserPreference], target: UserPreference, strategy: List[RatingComparison] => Double, threshold: Double, amount: Int) =
    userPreferences
      .par
      .filter(u => !u.equals(target) && u.hasDiffRatings(target))
      .map(u => u.copy(distance = strategy(u.toRatingComparisons(target))))
      .filter(_.distance >= threshold)
      .toList
      .sortWith(_.distance > _.distance)
      .take(amount)

}
