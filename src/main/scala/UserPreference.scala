import scala.collection.immutable.HashMap

/**
 * Holds user preferences.
 *
 * Created by Rudie on 17-2-2015.
 */
class UserPreference(_id: String, _ratings: HashMap[Int, Double] = new HashMap) {
  require(_id != null)

  val id = _id
  val ratings = _ratings

  /** Returns a [[UserPreference]] with the added rating */
  def addRating(product: Int, rating: Double) = new UserPreference(id, ratings + (product -> rating))

  /** Returns a rating to the [[UserPreference]] */
  def getRating(product: Int) = ratings get product

  override def toString = "UserPreference " + id + ": " + ratings
}
