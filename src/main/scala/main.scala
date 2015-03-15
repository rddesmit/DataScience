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
  val predictedRatingThreshold = 2
  val amount = 25
  val dataUri = getClass getResource "MovieLens/u.data" toURI
  val itemUri = getClass getResource "MovieLens/u.item" toURI

  //import data
  val ratings = FileLoader.getRatings(dataUri).groupBy(_.id)
  val movies = FileLoader.getMovies(itemUri).toHashMap(_.id)

  //map data to HashMap with UserPreferences
  val preferences = ratings.map(g => (UserPreference(g._1) /: g._2)((r, c) => r addRating(c.product, c.rating))) toList

  //println("Time: " + Benchmark.time(100, {
  //calculate nearest neighbours with all user similarity strategy's
  val euclideanDistance = nearestNeighbours(preferences, preferences.find(x => x.id == user) get, UserItemStrategys.euclideanDistance, nearestNeighbourThreshold, amount)
  val manhattanDistance = nearestNeighbours(preferences, preferences.find(x => x.id == user) get, UserItemStrategys.manhattanDistance, nearestNeighbourThreshold, amount)
  val pearsonCoefficient = nearestNeighbours(preferences, preferences.find(x => x.id == user) get, UserItemStrategys.pearsonCoefficient, nearestNeighbourThreshold, amount)
  val cosineSimilarity = nearestNeighbours(preferences, preferences.find(x => x.id == user) get, UserItemStrategys.cosineSimilarity, nearestNeighbourThreshold, amount)

  println("Comparing user: \t" + preferences.find(x => x.id == user).get)
  println("Euclidean distance: \t" + euclideanDistance.map(u => (u.id, u.distance)))
  println("Manhattan distance: \t" + manhattanDistance.map(u => (u.id, u.distance)))
  println("Pearson coefficient: \t" + pearsonCoefficient.map(u => (u.id, u.distance)))
  println("Cosine similarity: \t" + cosineSimilarity.map(u => (u.id, u.distance)))

  //calculate all predicted top rated products for all strategy's
  var euclideanDistancePredictions = predictRatings(euclideanDistance, preferences.find(x => x.id == user) get, predictedRatingThreshold, amount)
  val manhattanDistancePredictions = predictRatings(manhattanDistance, preferences.find(x => x.id == user) get, predictedRatingThreshold, amount)
  val pearsonCoefficientPredictions = predictRatings(pearsonCoefficient, preferences.find(x => x.id == user) get, predictedRatingThreshold, amount)
  val cosineSimilarityPredictions = predictRatings(cosineSimilarity, preferences.find(x => x.id == user) get, predictedRatingThreshold, amount)

  println("Predictions for user: \t" + preferences.find(x => x.id == user).get)
  euclideanDistancePredictions.foreach(p => println("Euclidean distance: \t" + p))
  manhattanDistancePredictions.foreach(p => println("Manhattan distance: \t" + p))
  pearsonCoefficientPredictions.foreach(p => println("Pearson coefficient: \t" + p))
  cosineSimilarityPredictions.foreach(p => println("Cosine similarity: \t" + p))
  //}))
}
