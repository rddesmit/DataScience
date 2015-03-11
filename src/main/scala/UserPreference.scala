import UserItemStrategys._

import scala.collection.immutable.HashMap
import RichDataStructures.RichHashMap._

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

  /** Returns whether or not this user has rated different products */
  def hasDiffRatings(other: UserPreference) = ratings.diff(other.ratings).size > 0

  /** Returns a list of rating comparisons between this and the given user*/
  def toRatingComparisons(other: UserPreference) = ratings.zipper(other.ratings).values.map(r => RatingComparison(r._1, r._2)) toList
}
