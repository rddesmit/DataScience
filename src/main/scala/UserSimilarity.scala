/**
 * Created by Rudie on 18-2-2015.
 */
object UserSimilarity {
  /** Returns the euclidean distance */
  def euclideanDistance(data: List[(Double, Double)]) = {
    val result = data.foldLeft[Double](0.0)((r, c) => r + Math.pow(c._1 - c._2, 2))
    1 / (1 + Math.sqrt(result))
  }

  /** Returns the manhattan distance */
  def manhattanDistance(data: List[(Double, Double)]) = {
    val result = data.foldLeft[Double](0)((r, c) => r + Math.abs(c._1 - c._2))
    1 / (1 + result)
  }

  /** Returns the pearson coefficient */
  def pearsonCoefficient(data: List[(Double, Double)]) = {
    val result = data.foldLeft[PearsonCoefficient](PearsonCoefficient(0, 0, 0, 0, 0, 0))((r, c) =>
      PearsonCoefficient(
        r.mult + (c._1 * c._2),
        r.sumX + c._1,
        r.sumY + c._2,
        r.powX + Math.pow(c._1, 2),
        r.powY + Math.pow(c._2, 2),
        r.length + 1
      ))

    (result.mult - ((result.sumX * result.sumY) / result.length)) /
      (Math.sqrt(result.powX - (Math.pow(result.sumX, 2) / result.length)) *
        Math.sqrt(result.powY - (Math.pow(result.sumY, 2) / result.length)))
  }

  /** Returns the cosine similarity */
  def cosineSimilarity(data: List[(Double, Double)]): Double = {
    val result = data.foldLeft[(Double, Double, Double)]((0, 0, 0))((r, c) => (r._1 + (c._1 * c._2), r._2 + Math.pow(c._1, 2), r._3 + Math.pow(c._2, 2)))
    result._1 / (Math.sqrt(result._2) * Math.sqrt(result._3))
  }

  case class PearsonCoefficient(mult: Double, sumX: Double, sumY: Double, powX: Double, powY: Double, length: Int)

}
