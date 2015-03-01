import scala.collection.immutable.HashMap

/**
 * Created by Rudie on 1-3-2015.
 */
object RichHashMap {
  implicit def hashMap2MyRichHashMap[K, V](x: HashMap[K, V]) = new MyRichHashMap[K, V](x)

  class MyRichHashMap[K, V](hashMap: HashMap[K, V]) {

    def zipper(map: HashMap[K, V]) = {
      (HashMap[K, (V, V)]() /: hashMap.keys.toList.intersect(map.keys.toList).map(x => x ->(hashMap(x), map(x))))((r, c) => r + c)
    }

    def keyDiff(map: HashMap[K, V]) = {
      hashMap.keys.toList.diff(map.keys.toList)
    }
  }

}
