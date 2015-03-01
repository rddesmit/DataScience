import scala.collection.immutable.HashMap
import scala.io.Source

val uri = getClass getResource "userItem.data" toURI
val lines = Source.fromFile(uri).getLines().toList
//map data to something use full
val data = lines.map(_.split(",") match {
  case Array(userId: String, productId: String, productRating: String) => (userId, productId, productRating.toDouble)
}).toList.groupBy(_._1)
//map data to HashMap with UserPreferences
val preferences = data.map(x => (UserPreference(x._1) /: x._2)((r, c) => r addRating(c._2, c._3))).toList

/*Exercise one*/
println("\nExercise one")

def calculateSimilarity(userPreferences: List[UserPreference], target: UserPreference, strategy: List[(Double, Double)] => Double): List[(String, Double)] = {
  userPreferences
    .filter(x => x != target)
    .map(x => x -> zipper(x.ratings, target.ratings))
    .map(x => x._1.id -> strategy(x._2.values.toList))
    .sortBy(x => x._2)
}

def zipper[K, V](map1: HashMap[K, V], map2: HashMap[K, V]): HashMap[K, (V, V)] = {
  map1.keys.toList.intersect(map2.keys.toList).map(x => x ->(map1(x), map2(x)))
}

case class UserPreference(id: String, ratings: HashMap[String, Double] = new HashMap) {
  require(id != null)

  /** Returns a [[UserPreference]] with the added rating */
  def addRating(product: String, rating: Double) = new UserPreference(id, ratings + (product -> rating))

  /** Returns a rating to the [[UserPreference]] */
  def getRating(product: String) = ratings get product

  override def toString = "UserPreference " + id + ": " + ratings
}

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

calculateSimilarity(preferences.toList, preferences.head, UserSimilarity.euclideanDistance)
calculateSimilarity(preferences.toList, preferences.head, UserSimilarity.manhattanDistance)
calculateSimilarity(preferences.toList, preferences.head, UserSimilarity.pearsonCoefficient)
calculateSimilarity(preferences.toList, preferences.head, UserSimilarity.cosineSimilarity)