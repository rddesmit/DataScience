package DataStructures

import Strategys.ItemItemStrategys._
import User.UserPreference

import scala.collection.immutable.HashMap

/**
 * Created by Rudie on 21-3-2015.
 */
object SlopeOneMatrix {

  def apply(items: List[String], users: List[UserPreference]): SlopeOneMatrix = {
    val data = items
      .par
      .flatMap(i => items.map(j => (i, j, slopeOne(i, j, users))))
      .filter(r => r._3.amount > 0 && !r._3.deviation.isNaN)

    (SlopeOneMatrix() /: data)((r, c) => r.addDeviation(c._1, c._2, c._3))
  }
}

case class SlopeOneMatrix(matrix: Map[(String, String), Deviation] = new HashMap[(String, String), Deviation]) {

  def addDeviation(i: String, j: String, deviation: Deviation) = SlopeOneMatrix(matrix + ((i, j) -> deviation) + ((j, i) -> -deviation))

  def hasDeviation(i: String, j: String) = matrix contains(i, j)

  def getDeviation(i: String, j: String) = matrix(i, j)
}
