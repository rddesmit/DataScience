package User

import DataStructures.RichHashMap._
import Strategys.UserItemStrategys._

import scala.collection.immutable.HashMap

/**
 * Stores the ratings of a user.
 *
 * Created by Rudie on 17-2-2015.
 */
case class UserPreference(id: String, ratings: HashMap[String, Double] = new HashMap, distance: Double = -1) {
  require(id != null)

  /** Returns a [[UserPreference]] with the added rating */
  def addRating(product: String, rating: Double) = new UserPreference(id, ratings + (product -> rating))

  /** Returns a rating to the [[UserPreference]] */
  def getRating(product: String) = ratings get product

  /** Returns true if the [[UserPreference]] has rated the product */
  def hasRating(product: String) = ratings contains product

  /** Returns the average rating of the [[UserPreference]] */
  def averageRating = (0.0 /: ratings)((r, c) => r + c._2) / ratings.size

  /** Returns whether or not this [[UserPreference]] has rated different products */
  def hasDiffRatings(other: UserPreference) = ratings.hasDiff(other.ratings)

  /** Returns a [[List]] of [[RatingComparison]] between this and the given [[UserPreference]] */
  def toRatingComparisons(other: UserPreference) = ratings.zipper(other.ratings).values.map(r => RatingComparison(r._1, r._2)) toList

  /** Returns true if the id's of the [[UserPreference]] are the same */
  override def equals(that: Any): Boolean = that match {
    case that: UserPreference => this.id == that.id
  }
}
