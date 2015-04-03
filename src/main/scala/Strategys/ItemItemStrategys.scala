package Strategys

import User.UserPreference


/**
 * Created by Rudie on 17-3-2015.
 */
object ItemItemStrategys {

  /** Returns the [[Deviation]] between items i and j */
  def slopeOne(i: String, j: String, users: List[UserPreference]) = {
    val data = users.filter(u => u.hasRating(i) && u.hasRating(j))
    val result = (SlopeOne(0, 0) /: data)((r, c) => {
      SlopeOne(
        r.currdev + (c.getRating(i).get - c.getRating(j).get),
        r.size + 1
      )
    })

    Deviation(result.currdev / result.size, result size)
  }

  /** Returns the updated [[Deviation]] between items i and j based on the given previous [[Deviation]] and [[UserPreference]] */
  def slopeOneUpdate(deviation: Deviation, i: String, j: String, user: UserPreference) = {
    val amount = deviation.amount + 1
    Deviation(((deviation.deviation * deviation.amount) + (user.getRating(i).get - user.getRating(j).get)) / amount, amount)
  }

  def acs(i: String, j: String, userPreferences: List[UserPreference]) = {
    val data = userPreferences.filter(u => u.hasRating(i) && u.hasRating(j))
    val result = (ACS(0, 0, 0) /: data)((r, c) => {
      val averageI = c.getRating(i).get - c.averageRating
      val averageJ = c.getRating(j).get - c.averageRating
      ACS(
        r.mult + averageI * averageJ,
        r.powI + Math.pow(averageI, 2),
        r.powJ + Math.pow(averageJ, 2))
    })

    result.mult / (Math.sqrt(result.powI) * Math.sqrt(result.powJ))
  }

  case class Deviation(deviation: Double, amount: Double) {
    def unary_- = copy(deviation = -deviation)
  }

  private case class SlopeOne(currdev: Double, size: Int)
  private case class ACS(mult: Double, powI: Double, powJ: Double)
}
