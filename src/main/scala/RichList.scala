import scala.annotation.tailrec
import scala.collection.immutable.HashMap

/**
 * Implicit conversion and helpers for [[List]]
 *
 * Created by Rudie on 17-2-2015.
 */
object RichList {

  implicit def list2MyRichList[T](x: List[T]) = new MyRichList[T](x)

  class MyRichList[V](list: List[V]) {

    /**
     * Converts a [[List]] to a [[HashMap]].
     * @param key Gets the key for each value of the list.
     * @tparam K Type of the key.
     * @return Converted list.
     */
    def toHashMap[K](key: V => K) = {
      @tailrec
      def convert(k: V => K, list: List[V], hashMap: HashMap[K, V]): HashMap[K, V] = {
        list match {
          case Nil => hashMap
          case head :: tail => convert(key, tail, hashMap + (key(head) -> head))
        }
      }

      convert(key, list, new HashMap[K, V])
    }

    /**
     * Calls the given method recursive with each element of the [[List]].
     * Using the result of the function as input for the next function.
     * @param obj Object that contains the function to be called.
     * @param func function which will be called for each element of the list.
     * @tparam T Type of the object.
     * @return The result of the last called function.
     */
    def callRecursive[T](obj: T, func: (T, V) => T) = {
      @tailrec
      def call(list: List[V], obj: T, func: (T, V) => T): T = {
        list match {
          case Nil => obj
          case head :: tail => call(tail, func(obj, head), func)
        }
      }

      call(list, obj, func)
    }
  }

}
