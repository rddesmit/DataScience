import RichList._
import UserSimilarity._

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
  data.map(x => (UserPreference(x._1) /: x._2)((r, c) => r addRating(c._2, c._3)))
    .toList.toHashMap(_.id).foreach(println)


  /*Exercise two*/
  println("\nExercise two")

  val data2 = List((2.0, 0.0), (4.0, 3.0), (1.0, 2.0))
  println("Euclidean distance: \t" + euclideanDistance(data2))
  println("Manhattan distance: \t" + manhattanDistance(data2))

  val data3 = List((4.75, 4.0), (4.5, 3.0), (5.0, 5.0), (4.25, 2.0), (4.0, 1.0))
  println("Pearson coefficient: \t" + pearsonCoefficient(data3))
  println("Cosine similarity: \t" + cosineSimilarity(data3))
}
