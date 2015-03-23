import DataStructures.RichList._
import DataStructures.SlopeOneDeviation
import NearestNeighbour._
import Predictions.{ItemItemPrediction, UserItemPrediction}
import Strategys.UserItemStrategys
import User.UserPreference

/**
 * Created by Rudie on 16-2-2015.
 */
object Main extends App {

  val user = "186"
  val nearestNeighbourThreshold = 0.35
  val predictedRatingThreshold = 3
  val nearestNeighboursAmount = 25
  val predictedRatingsAmount = 8
  val dataUri = getClass getResource "MovieLens/u.data" toURI
  val itemUri = getClass getResource "MovieLens/u.item" toURI

  //import data
  val ratings = FileLoader.getRatings(dataUri).groupBy(_.id)
  val movies = FileLoader.getMovies(itemUri).toHashMap(_.id)

  //map data to HashMap with UserPreferences
  println("Loading data")
  val preferences = ratings.map(g => (UserPreference(g._1) /: g._2)((r, c) => r addRating(c.product, c.rating))) toList
  val matrix = SlopeOneDeviation.init(movies.keys.toList, preferences)
  val target = preferences.find(x => x.id == user) get

  //user-item
  println("Calculating User-Item")
  println("User-Item Time: " + Benchmark.time(1, {
    //find nearest neighbours and predict the ratings
    val neighbours = nearestNeighbours(preferences, target, UserItemStrategys.pearsonCoefficient, nearestNeighbourThreshold, nearestNeighboursAmount)
    val ratings = UserItemPrediction.predictRatings(neighbours, target, predictedRatingThreshold, predictedRatingsAmount)
    ratings.foreach(r => println(r.rating + "\t" + movies(r.id).title))
  }) / 1000000.0 + " mil. sec.")

  //item-item
  println("Calculating Item-Item")
  println("Item-Item Time: " + Benchmark.time(1, {
    val ratings = ItemItemPrediction.predictRatings(target, movies.keys.toList, matrix, predictedRatingsAmount)
    ratings.foreach(r => println(r.rating + "\t" + movies(r.id).title))
  }) / 1000000.0 + " mil. sec.")
}