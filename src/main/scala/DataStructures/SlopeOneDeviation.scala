package DataStructures

import Strategys.ItemItemStrategys._
import User.UserPreference

import scala.collection.immutable.HashMap

/**
 * Created by Rudie on 21-3-2015.
 */
object SlopeOneDeviation {

  def apply(items: List[String], users: List[UserPreference]): SlopeOneDeviation =
    (SlopeOneDeviation() /: items.par.flatMap(i => items.map(j => (i, j, slopeOne(i, j, users)))))((r, c) => r.addDeviation(c._1, c._2, c._3))
}

case class SlopeOneDeviation(matrix: Map[(String, String), Deviation] = new HashMap[(String, String), Deviation]) {

  def addDeviation(i: String, j: String, deviation: Deviation) = SlopeOneDeviation(matrix + ((i, j) -> deviation) + ((j, i) -> -deviation))

  def hasDeviation(i: String, j: String) = matrix contains ((i, j))

  def getDeviation(i: String, j: String) = matrix((i, j))
}
