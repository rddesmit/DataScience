package MovieLens

/**
  * Created by Rudie on 3-3-2015.
  */
case class Movie(id: String, title: String, releaseDate: String, videoReleaseData: String, imdbUrl: String) {
  override def toString() = id + ": \t" + title
}
