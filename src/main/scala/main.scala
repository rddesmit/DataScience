import RichList._

import scala.io.Source

/**
 * Created by Rudie on 16-2-2015.
 */
object main extends App {

  //load file
  val lines = Source.fromFile("D:\\Documents\\OneDrive\\Data Science 1\\UserItem.data").getLines().toList
  //map data to something use full
  val data = lines.map(x => x.split(",").toList).map(x => new Tuple3(x(0), x(1) toInt, x(2) toDouble)).groupBy(x => x._1)
  //map data to HashMap with UserPreferences
  val result = data.map(x => x._2.callRecursive[UserPreference](new UserPreference(x._1), (y, z) => y.addRating(z._2, z._3))).toList.toHashMap(x => x.id)

  println(result)
}
