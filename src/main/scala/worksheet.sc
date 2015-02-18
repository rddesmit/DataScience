import scala.collection.immutable.HashMap
import scala.io.Source

case class UserPreference(id: String, ratings: HashMap[String, Double] = new HashMap) {
  require(id != null)

  /** Returns a [[UserPreference]] with the added rating */
  def addRating(product: String, rating: Double) = new UserPreference(id, ratings + (product -> rating))

  /** Returns a rating to the [[UserPreference]] */
  def getRating(product: String) = ratings get product

  override def toString = "UserPreference " + id + ": " + ratings
}

val uri = getClass getResource "userItem.data" toURI
val lines = Source.fromFile(uri).getLines().toList

/*Exercise one*/
println("\nExercise one")
//map data to something use full
val data = lines.map(_.split(",") match {
  case Array(userId: String, productId: String, productRating: String) => (userId, productId, productRating.toDouble)
}).toList.groupBy(_._1)

//map data to HashMap with UserPreferences
data.map(x => (UserPreference(x._1) /: x._2)((r, c) => r addRating(c._2, c._3))).toList.foreach(println)