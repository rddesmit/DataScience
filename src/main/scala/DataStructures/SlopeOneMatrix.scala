package DataStructures

import Strategys.ItemItemStrategys._
import User.UserPreference

import scala.collection.immutable.HashMap

/**
 * Created by Rudie on 21-3-2015.
 */
object SlopeOneMatrix {

  /** Returns a new [[SlopeOneMatrix]] which contains all [[Deviation]] based on all the items and [[UserPreference]]].
    * It won't contain [[Deviation]] that are based on 0 [[UserPreference]] or NaN values. */
  def apply(items: List[String], users: List[UserPreference]): SlopeOneMatrix = {
    val data = items
      .par
      .flatMap(i => items.map(j => (i, j, slopeOne(i, j, users))))
      .filter(r => r._3.amount > 0 && !r._3.deviation.isNaN)

    (SlopeOneMatrix() /: data)((r, c) => r.addDeviation(c._1, c._2, c._3))
  }
}

case class SlopeOneMatrix(matrix: Map[(String, String), Deviation] = new HashMap[(String, String), Deviation]) {

  /** Returns a new [[SlopeOneMatrix]] with the added [[Deviation]] for item i and j */
  def addDeviation(i: String, j: String, deviation: Deviation) = SlopeOneMatrix(matrix + ((i, j) -> deviation) + ((j, i) -> -deviation))

  /** Returns True if the [[SlopeOneMatrix]] contains a [[Deviation]] for item i and j */
  def hasDeviation(i: String, j: String) = matrix contains(i, j)

  /** Returns the [[Deviation]] for item i and j */
  def getDeviation(i: String, j: String) = matrix(i, j)

  /** Returns a new [[SlopeOneMatrix]] with updated [[Deviation]] involving item j and the [[UserPreference]].
    * It won't contain [[Deviation]] that are based on 0 [[UserPreference]] or NaN values. */
  def updateDeviation(user: UserPreference, j: String) = {
    val data = user.ratings.keys
      .par
      .filter(i => hasDeviation(i, j))
      .map(i => (i, j, slopeOneUpdate(getDeviation(i, j), i, j, user)))
      .filter(r => r._3.amount > 0 && !r._3.deviation.isNaN)

    (this /: data)((r, c) => r.addDeviation(c._1, c._2, c._3))
  }
}
