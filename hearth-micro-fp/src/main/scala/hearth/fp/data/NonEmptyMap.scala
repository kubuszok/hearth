package hearth
package fp
package data

import scala.collection.immutable.ListMap

/** Non-empty map, guaranteed to hold at least one key-value pair and insertion-ordered (backed by a `ListMap`).
  *
  * Unlike an ordered collection, keys are unique: adding a pair whose key already exists upserts that entry rather than
  * duplicating the key, and `map`/`flatMap` can therefore *shrink* the map when the key mapping collides.
  *
  * @tparam K
  *   key type
  * @tparam V
  *   value type
  * @since 0.1.0
  */
final case class NonEmptyMap[K, +V](head: (K, V), tail: ListMap[K, V]) {

  /** Prepends a pair: the given pair becomes the new head, de-duplicating its key (any existing entry for that key is
    * dropped from the tail). Insertion order is preserved via the backing `ListMap`, so this is NOT a plain cons.
    *
    * @since 0.1.0
    */
  def +:[V2 >: V](pair: (K, V2)): NonEmptyMap[K, V2] =
    if (head._1 == pair._1) NonEmptyMap(pair, tail)
    else NonEmptyMap(pair, ListMap.from(Iterable(head) ++ tail.removed(pair._1)))

  /** Appends/upserts a pair: if its key already exists the entry is updated in place, otherwise it is added at the end.
    * De-duplicates the key rather than growing the map with a duplicate.
    *
    * @since 0.1.0
    */
  def :+[V2 >: V](pair: (K, V2)): NonEmptyMap[K, V2] =
    if (head._1 == pair._1)
      tail.headOption match {
        case Some(head2) => NonEmptyMap(head2, tail.tail.removed(head._1) + pair)
        case None        => NonEmptyMap(pair, ListMap.empty[K, V2])
      }
    else NonEmptyMap(head, tail.removed(pair._1) + pair)

  /** Transforms every key-value pair. May shrink the map if `f` maps distinct keys onto the same key (backed by
    * `ListMap.from`, so the last colliding entry wins); stays non-empty.
    *
    * @since 0.1.0
    */
  def map[K2, V2](f: ((K, V)) => (K2, V2)): NonEmptyMap[K2, V2] =
    NonEmptyMap.fromListMap(ListMap.from(iterator.map(f))).get

  /** Transforms and flattens every key-value pair. May shrink the map when produced keys collide (last one wins); stays
    * non-empty.
    *
    * @since 0.1.0
    */
  def flatMap[K2, V2](f: ((K, V)) => NonEmptyMap[K2, V2]): NonEmptyMap[K2, V2] =
    NonEmptyMap.fromListMap(ListMap.from(iterator.flatMap(kv => f(kv).iterator))).get

  def iterator: Iterator[(K, V)] = Iterator(head) ++ tail.iterator
  def toListMap: ListMap[K, V] = ListMap.from(iterator)
  def toList: List[(K, V)] = iterator.toList
  def toVector: Vector[(K, V)] = iterator.toVector
  def toNonEmptyList: NonEmptyList[(K, V)] = NonEmptyList(head, tail.toList)
  def toNonEmptyVector: NonEmptyVector[(K, V)] = NonEmptyVector(head, tail.toVector)

  def ++[V2 >: V](other: NonEmptyMap[K, V2]): NonEmptyMap[K, V2] = {
    val combined = this.toListMap ++ other.toListMap
    NonEmptyMap.fromListMap(combined).get // safe: both non-empty
  }

  def size: Int = 1 + tail.size

  def mkString(sep: String): String = iterator.mkString(sep)
  def mkString(start: String, sep: String, end: String): String = iterator.mkString(start, sep, end)

  override def toString: String = mkString("NonEmptyMap(", ", ", ")")
}
object NonEmptyMap {

  def apply[K, V](kv: (K, V), kvs: ((K, V))*): NonEmptyMap[K, V] = NonEmptyMap(kv, ListMap.from(kvs))
  def fromListMap[K, V](listMap: ListMap[K, V]): Option[NonEmptyMap[K, V]] = listMap.headOption.map { case (k, v) =>
    NonEmptyMap((k, v), listMap.tail.toSeq*)
  }
  def one[K, V](kv: (K, V)): NonEmptyMap[K, V] = NonEmptyMap(kv, ListMap.empty[K, V])
}
