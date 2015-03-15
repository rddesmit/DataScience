import UserItemStrategys._

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
  def nearestNeighbours(userPreferences: List[UserPreference], target: UserPreference, strategy: List[RatingComparison] => Double, threshold: Double, amount: Int) = {
    userPreferences
      .par
      .filter(u => u.id != target.id && u.hasDiffRatings(target))
      .map(u => u.copy(distance = strategy(u.toRatingComparisons(target))))
      .filter(u => u.distance >= threshold)
      .toList
      .sortWith(_.distance > _.distance)
      .take(amount)
  }
}
