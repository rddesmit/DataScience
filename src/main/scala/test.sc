import scala.annotation.tailrec
import scala.collection.immutable.HashMap
import scala.io.Source


val lines = Source.fromFile("D:\\Documents\\OneDrive\\Data Science 1\\UserItem.data").getLines().toList
val data = lines.map(x => x.split(",").toList).map(x => (x(0), x(1) toInt, x(2) toDouble)).groupBy(x => x._1).toList
val list = List(1, 2, 3, 4, 5, 6)

class UserPreference(_id: String, _ratings: HashMap[Int, Double] = new HashMap) {
  require(_id != null)

  val id = _id
  val ratings = _ratings

  def addRating(product: Int, rating: Double) = new UserPreference(id, ratings + (product -> rating))

  def getRating(product: Int) = ratings get product

  override def toString = "UserPreference " + id + ": " + ratings
}

object RichList {

  implicit def list2MyRichList[T](x: List[T]) = new MyRichList[T](x)

  class MyRichList[V](list: List[V]) {
    def toHashMap[K](key: V => K) = {
      @tailrec
      def convert(k: V => K, list: List[V], hashMap: HashMap[K, V]): HashMap[K, V] = {
        list match {
          case Nil => hashMap
          case x :: tail => convert(key, tail, hashMap + (key(x) -> x))
        }
      }

      convert(key, list, new HashMap[K, V])
    }

    def callRecursive[T](obj: T, method: (T, V) => T) = {
      @tailrec
      def call(list: List[V], obj: T, method: (T, V) => T): T = {
        list match {
          case Nil => obj
          case x :: tail => call(tail, method(obj, x), method)
        }
      }

      call(list, obj, method)
    }
  }

}

//val result = data.map(x => callRecursive[UserPreference, (String, Int, Double)](x._2, new UserPreference(x._1), (y, z) => y.addRating(z._2, z._3))).toList.toHashMap(x => x.id)