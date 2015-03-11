/**
 * Created by Rudie on 18-2-2015.
 */
object UserItemStrategys {
  /** Returns the euclidean distance */
  def euclideanDistance(data: List[RatingComparison]) = {
    val result = (0.0 /: data)((r, c) => r + Math.pow(c.ratingOne - c.ratingTwo, 2))
    1 / (1 + Math.sqrt(result))
  }

  /** Returns the manhattan distance */
  def manhattanDistance(data: List[RatingComparison]) = {
    val result = (0.0 /: data)((r, c) => r + Math.abs(c.ratingOne - c.ratingTwo))
    1 / (1 + result)
  }

  /** Returns the pearson coefficient */
  def pearsonCoefficient(data: List[RatingComparison]) = {
    val result = (PearsonCoefficient(0, 0, 0, 0, 0, 0) /: data)((r, c) =>
      PearsonCoefficient(
        r.mult + (c.ratingOne * c.ratingTwo),
        r.sumX + c.ratingOne,
        r.sumY + c.ratingTwo,
        r.powX + Math.pow(c.ratingOne, 2),
        r.powY + Math.pow(c.ratingTwo, 2),
        r.length + 1))

    (result.mult - ((result.sumX * result.sumY) / result.length)) /
      (Math.sqrt(result.powX - (Math.pow(result.sumX, 2) / result.length)) *
        Math.sqrt(result.powY - (Math.pow(result.sumY, 2) / result.length)))
  }

  /** Returns the cosine similarity */
  def cosineSimilarity(data: List[RatingComparison]): Double = {
    val result = (CosineSimilarity(0, 0, 0) /: data)((r, c) =>
      CosineSimilarity(
        r.mult + (c.ratingOne * c.ratingTwo),
        r.powX + Math.pow(c.ratingOne, 2),
        r.powY + Math.pow(c.ratingTwo, 2)))

    result.mult / (Math.sqrt(result.powX) * Math.sqrt(result.powY))
  }

  case class RatingComparison(ratingOne: Double, ratingTwo: Double)

  private case class PearsonCoefficient(mult: Double, sumX: Double, sumY: Double, powX: Double, powY: Double, length: Int)

  private case class CosineSimilarity(mult: Double, powX: Double, powY: Double)
}
