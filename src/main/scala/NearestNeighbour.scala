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
      .filter(x => x.id != target.id && x.ratings.diff(target.ratings).size > 0)
      .map(x => x.copy(distance = strategy(x.ratings.zipper(target.ratings).values toList)))
      .filter(x => x.distance >= threshold)
      .sortWith(_.distance > _.distance)
      .take(amount)
  }

  def predictRatings(nearestNeighbours: List[UserPreference], target: UserPreference, amount: Int) = {
  }

  def predictRating(data: List[(Double, Double)]) = {
    val total = (0.0 /: data)((r,c) => r + c._2)
    (0.0 /: data.map(x => x._1 * (x._2 / total)))((r,c) => r + c)
  }
}
