import RichList._

import scala.io.Source

/**
 * Created by Rudie on 16-2-2015.
 */
object Main extends App {

  val uri = getClass getResource "userItem.data" toURI
  val lines = Source.fromFile(uri).getLines().toList

  //map data to something use full
  val data = lines.map(_.split(",") match {
    case Array(userId: String, productId: String, productRating: String) => (userId, productId, productRating.toDouble)
  }).toList.groupBy(_._1)

  //map data to HashMap with UserPreferences
  val result = data.map(t => t._2.callRecursive[UserPreference](UserPreference(t._1), (y, z) => y.addRating(z._2, z._3)))
    .toList.toHashMap(x => x.id)

  result.foreach(println)
}
