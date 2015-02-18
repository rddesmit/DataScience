/**
 * Created by Rudie on 18-2-2015.
 */
object UserSimilarity {
  /** Returns the euclidean distance */
  def euclideanDistance(data: List[(Double, Double)]) = {
    val result = (0.0 /: data)((r, c) => r + Math.pow(c._1 - c._2, 2))
    1 / (1 + Math.sqrt(result))
  }

  /** Returns the manhattan distance */
  def manhattanDistance(data: List[(Double, Double)]) = {
    val result = (0.0 /: data)((r, c) => r + Math.abs(c._1 - c._2))
    1 / (1 + result)
  }

  /** Returns the pearson coefficient */
  def pearsonCoefficient(data: List[(Double, Double)]) = {
    val result = (PearsonCoefficient(0, 0, 0, 0, 0, 0) /: data)((r, c) =>
      PearsonCoefficient(
        r.mult + (c._1 * c._2),
        r.sumX + c._1,
        r.sumY + c._2,
        r.powX + Math.pow(c._1, 2),
        r.powY + Math.pow(c._2, 2),
        r.length + 1))

    (result.mult - ((result.sumX * result.sumY) / result.length)) /
      (Math.sqrt(result.powX - (Math.pow(result.sumX, 2) / result.length)) *
        Math.sqrt(result.powY - (Math.pow(result.sumY, 2) / result.length)))
  }

  /** Returns the cosine similarity */
  def cosineSimilarity(data: List[(Double, Double)]): Double = {
    val result = (CosineSimilarity(0, 0, 0) /: data)((r, c) =>
      CosineSimilarity(
        r.mult + (c._1 * c._2),
        r.powX + Math.pow(c._1, 2),
        r.powY + Math.pow(c._2, 2)))

    result.mult / (Math.sqrt(result.powX) * Math.sqrt(result.powY))
  }

  case class PearsonCoefficient(mult: Double, sumX: Double, sumY: Double, powX: Double, powY: Double, length: Int)

  case class CosineSimilarity(mult: Double, powX: Double, powY: Double)
}
