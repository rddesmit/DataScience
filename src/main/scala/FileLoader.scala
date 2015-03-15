import java.net.URI
import java.nio.charset.CodingErrorAction

import MovieLens.{Movie, Rating}

import scala.io.{Codec, Source}

/**
 * Created by Rudie on 15-3-2015.
 */
object FileLoader {
  implicit val codec = Codec("UTF-8")
  codec.onMalformedInput(CodingErrorAction.IGNORE)

  def getMovies(uri : URI) = {
    val data = Source fromFile uri getLines() toList

    data.par.map(_.split("\\|") match {
      case Array(id: String, title: String, releaseDate: String, videoReleaseData: String, imdbUrl: String, _*) => Movie(id, title, releaseDate, videoReleaseData, imdbUrl)
    }) toList
  }

  def getRatings(uri: URI) = {
    val data = Source fromFile uri getLines() toList

    data.par.map(_.replace(",", "\t").split("\t") match {
      case Array(id: String, product: String, rating: String) => Rating(id, product, rating toDouble)
      case Array(id: String, product: String, rating: String, _) => Rating(id, product, rating toDouble)
    }) toList
  }
}
