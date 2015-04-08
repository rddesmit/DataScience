import scala.io.Source

val lines = Source.fromFile("D:\\Documents\\OneDrive\\Data Science 1\\exercise_c.csv").getLines().toList
val movies = lines.head.split("\t").map(m => Movie(m)).zipWithIndex.toList
val ratings = lines.tail.map(r => r.split("\t").tail.toList)
val data = movies.map(m => m._1.copy(ratings = ratings.map(r => r(m._2)).filter(_ != " ").map(_.toDouble)))
//calculate the mean, then sort the results and take the top 5
var means = data.map(m => m -> m.ratings.sum / m.ratings.size).sortWith(_._2 > _._2).take(5)
//calculate the size of the ratings list, then sort the results and take the top 5
var counts = data.map(m => m -> m.ratings.size).sortWith(_._2 > _._2).take(5)
means.foreach(m => println(m._2 + "\t" + m._1.title))

case class Movie(title: String, ratings: List[Double] = List())

counts.foreach(m => println(m._2 + "\t" + m._1.title))