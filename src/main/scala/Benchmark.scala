/**
 * Created by Rudie on 15-3-2015.
 */
object Benchmark {
  def time[T](repeat: Int, f: => T) = {
    val startTime = System.nanoTime()
    (1 to repeat).foreach(_ => f)
    (System.nanoTime() - startTime) / repeat
  }
}
