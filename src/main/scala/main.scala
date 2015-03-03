import RichHashMap._
import RichList._
import UserSimilarity._

import scala.collection.immutable.HashMap
import scala.io.Source

/**
 * Created by Rudie on 16-2-2015.
 */
object Main extends App {

  val uri = getClass getResource "userItem.data" toURI
  val lines = Source.fromFile(uri).getLines().toList

  /*Exercise one*/
  println("\nExercise one")

  //map data to something use full
  val data = lines.map(_.split(",") match {
    case Array(userId: String, productId: String, productRating: String) => (userId, productId, productRating.toDouble)
  }).toList.groupBy(_._1)

  //map data to HashMap with UserPreferences
  val preferences = data.map(x => (UserPreference(x._1) /: x._2)((r, c) => r addRating(c._2, c._3)))
    .toList.toHashMap(x => x.id)
  println(preferences)


  /*Exercise two*/
  println("\nExercise two")

  def nearestNeighbours(userPreferences: HashMap[String, UserPreference], target: UserPreference, strategy: List[(Double, Double)] => Double, threshold: Double, max: Int) = {
    userPreferences
      .toList
      .filter(x => x._1 != target.id)
      .map(x => x._2 -> strategy(x._2.ratings.zipper(target.ratings).values toList))
      .filter(x => x._2 >= threshold && x._1.ratings.keyDiff(target.ratings).length > 0)
      .sortWith(_._2 > _._2)
      .take(max)
  }

  println("Comparing user: \t" + preferences("7"))
  println("Euclidean distance: \t" + nearestNeighbours(preferences, preferences("7"), UserSimilarity.euclideanDistance, 0.35, 3))
  println("Manhattan distance: \t" + nearestNeighbours(preferences, preferences("7"), UserSimilarity.manhattanDistance, 0.35, 3))
  println("Pearson coefficient: \t" + nearestNeighbours(preferences, preferences("7"), UserSimilarity.pearsonCoefficient, 0.35, 3))
  println("Cosine similarity: \t" + nearestNeighbours(preferences, preferences("7"), UserSimilarity.cosineSimilarity, 0.35, 3))
}
