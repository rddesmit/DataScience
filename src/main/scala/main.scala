import java.nio.charset.CodingErrorAction

import MovieLens.{Movie, Rating}
import RichDataStructures.RichList._
import NearestNeighbour._
import RatingPrediction._

import scala.io.{Codec, Source}

/**
 * Created by Rudie on 16-2-2015.
 */
object Main extends App {

  val user = "7"
  val nearestNeighbourThreshold = 0.35
  val predictedRatingThreshold = 0
  val amount = 3
  val dataUri = getClass getResource "userItem.data" toURI
  val itemUri = getClass getResource "MovieLens/u.item" toURI

  implicit val codec = Codec("UTF-8")
  codec.onMalformedInput(CodingErrorAction.IGNORE)

  //import data
  val data = Source fromFile dataUri getLines() toList
  val ratings = data.map(_.replace(",", "\t").split("\t") match {
    case Array(id: String, product: String, rating: String) => Rating(id, product, rating toDouble)
    case Array(id: String, product: String, rating: String, _) => Rating(id, product, rating toDouble)
  }).toList.groupBy(_.id)

  val items = Source fromFile itemUri getLines() toList
  val movies = items.map(_.split("\\|") match {
    case Array(id: String, title: String, releaseDate: String, videoReleaseData: String, imdbUrl: String, _*) => Movie(id, title, releaseDate, videoReleaseData, imdbUrl)
  }).toList.toHashMap(_.id)

  //map data to HashMap with UserPreferences
  val preferences = ratings.map(g => (UserPreference(g._1) /: g._2)((r, c) => r addRating(c.product, c.rating)))
    .toList.toHashMap(_.id)

  //calculate nearest neighbours with all user similarity strategy's
  val euclideanDistance = nearestNeighbours(preferences.toList.map(x => x._2), preferences(user), UserItemStrategys.euclideanDistance, nearestNeighbourThreshold, amount)
  val manhattanDistance = nearestNeighbours(preferences.toList.map(x => x._2), preferences(user), UserItemStrategys.manhattanDistance, nearestNeighbourThreshold, amount)
  val pearsonCoefficient = nearestNeighbours(preferences.toList.map(x => x._2), preferences(user), UserItemStrategys.pearsonCoefficient, nearestNeighbourThreshold, amount)
  val cosineSimilarity = nearestNeighbours(preferences.toList.map(x => x._2), preferences(user), UserItemStrategys.cosineSimilarity, nearestNeighbourThreshold, amount)

  println("Comparing user: \t" + preferences(user))
  println("Euclidean distance: \t" + euclideanDistance.map(u => (u.id, u.distance)))
  println("Manhattan distance: \t" + manhattanDistance.map(u => (u.id, u.distance)))
  println("Pearson coefficient: \t" + pearsonCoefficient.map(u => (u.id, u.distance)))
  println("Cosine similarity: \t" + cosineSimilarity.map(u => (u.id, u.distance)))

  //calculate all predicted top rated products for all strategy's
  var euclideanDistancePredictions = predictRatings(euclideanDistance, preferences(user), predictedRatingThreshold, amount)
  val manhattanDistancePredictions = predictRatings(manhattanDistance, preferences(user), predictedRatingThreshold, amount)
  val pearsonCoefficientPredictions = predictRatings(pearsonCoefficient, preferences(user), predictedRatingThreshold, amount)
  val cosineSimilarityPredictions = predictRatings(cosineSimilarity, preferences(user), predictedRatingThreshold, amount)

  println("Predictions for user: \t" + preferences(user))
  euclideanDistancePredictions.foreach(p => println("Euclidean distance: \t" + p))
  manhattanDistancePredictions.foreach(p => println("Manhattan distance: \t" + p))
  pearsonCoefficientPredictions.foreach(p => println("Pearson coefficient: \t" + p))
  cosineSimilarityPredictions.foreach(p => println("Cosine similarity: \t" + p))
}
