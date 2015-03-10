import java.nio.charset.CodingErrorAction

import MovieLens.{Movie, Rating}
import RichDataStructures.RichList._
import NearestNeighbour._

import scala.io.{Codec, Source}

/**
 * Created by Rudie on 16-2-2015.
 */
object Main extends App {

  val user = "43"
  val threshold = 0.35
  val amount = 10
  val dataUri = getClass getResource "MovieLens/u.data" toURI
  val itemUri = getClass getResource "MovieLens/u.item" toURI

  implicit val codec = Codec("UTF-8")
  codec.onMalformedInput(CodingErrorAction.IGNORE)

  //import data
  val data = Source fromFile dataUri getLines() toList
  val ratings = data.map(_.split("\t") match {
    case Array(id: String, product: String, rating: String, timestamp: String) => Rating(id, product, rating toDouble, timestamp)
  }).toList.groupBy(_.id)

  val items = Source fromFile itemUri getLines() toList
  val movies = items.map(_.split("\\|") match {
    case Array(id: String, title: String, releaseDate: String, videoReleaseData: String, imdbUrl: String, _*) => Movie(id, title, releaseDate, videoReleaseData, imdbUrl)
  }).toList.toHashMap(_.id)

  //map data to HashMap with UserPreferences
  val preferences = ratings.map(x => (UserPreference(x._1) /: x._2)((r, c) => r addRating(c.product, c.rating)))
    .toList.toHashMap(_.id)

  val euclideanDistance = nearestNeighbours(preferences.toList.map(x => x._2), preferences(user), UserSimilarity.euclideanDistance, threshold, amount)
  val manhattanDistance = nearestNeighbours(preferences.toList.map(x => x._2), preferences(user), UserSimilarity.manhattanDistance, threshold, amount)
  val pearsonCoefficient = nearestNeighbours(preferences.toList.map(x => x._2), preferences(user), UserSimilarity.pearsonCoefficient, threshold, amount)
  val cosineSimilarity = nearestNeighbours(preferences.toList.map(x => x._2), preferences(user), UserSimilarity.cosineSimilarity, threshold, amount)

  println("Comparing user: \t" + preferences(user))
  println("Euclidean distance: \t" + euclideanDistance.map(x => (x.id, x.distance)))
  println("Manhattan distance: \t" + manhattanDistance.map(x => (x.id, x.distance)))
  println("Pearson coefficient: \t" + pearsonCoefficient.map(x => (x.id, x.distance)))
  println("Cosine similarity: \t" + cosineSimilarity.map(x => (x.id, x.distance)))

  var euclideanDistancePredictions = predictRatings(euclideanDistance, preferences(user), amount)
  val manhattanDistancePredictions = predictRatings(manhattanDistance, preferences(user), amount)
  val pearsonCoefficientPredictions = predictRatings(pearsonCoefficient, preferences(user), amount)
  val cosineSimilarityPredictions = predictRatings(cosineSimilarity, preferences(user), amount)

  println("Predictions for user: \t" + preferences(user))
  euclideanDistancePredictions.foreach(p => println("Euclidean distance: \t" + movies(p.id)))
  manhattanDistancePredictions.foreach(p => println("Manhattan distance: \t" + movies(p.id)))
  pearsonCoefficientPredictions.foreach(p => println("Pearson coefficient: \t" + movies(p.id)))
  cosineSimilarityPredictions.foreach(p => println("Cosine similarity: \t" + movies(p.id)))
}
