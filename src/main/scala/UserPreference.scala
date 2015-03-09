import scala.collection.immutable.HashMap

/**
 * Holds user preferences.
 *
 * Created by Rudie on 17-2-2015.
 */
case class UserPreference(id: String, ratings: HashMap[String, Double] = new HashMap, distance: Double = -1) {
  require(id != null)

  /** Returns a [[UserPreference]] with the added rating */
  def addRating(product: String, rating: Double) = new UserPreference(id, ratings + (product -> rating))

  /** Returns a rating to the [[UserPreference]] */
  def getRating(product: String) = ratings get product
}
