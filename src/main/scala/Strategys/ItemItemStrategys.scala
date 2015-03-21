package Strategys

import User.UserPreference


/**
 * Created by Rudie on 17-3-2015.
 */
object ItemItemStrategys {
  def slopeOne(i: String, j: String, userPreferences: List[UserPreference]) = {
    val data = userPreferences.filter(u => u.hasRating(i) && u.hasRating(j))
    val currdev = (0.0 /: data)((r, c) => r + (c.getRating(i).get - c.getRating(j).get))

    Deviation(currdev / data.size, data size)
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

  private case class ACS(mult: Double, powI: Double, powJ: Double)

}
