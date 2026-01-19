package hearth
package fp
package data

import scala.collection.immutable.ListMap

/** Non-empty map.
  *
  * @since 0.1.0
  */
final case class NonEmptyMap[K, +V](head: (K, V), tail: ListMap[K, V]) {

  def +:[V2 >: V](pair: (K, V2)): NonEmptyMap[K, V2] =
    if (head._1 == pair._1) NonEmptyMap(pair, tail)
    else NonEmptyMap(pair, ListMap.from(Iterable(head) ++ tail.removed(pair._1)))
  def :+[V2 >: V](pair: (K, V2)): NonEmptyMap[K, V2] =
    if (head._1 == pair._1)
      tail.headOption match {
        case Some(head2) => NonEmptyMap(head2, tail.tail.removed(head._1) + pair)
        case None        => NonEmptyMap(pair, ListMap.empty[K, V2])
      }
    else NonEmptyMap(head, tail.removed(pair._1) + pair)

  def map[K2, V2](f: ((K, V)) => (K2, V2)): NonEmptyMap[K2, V2] =
    NonEmptyMap(f(head), ListMap.from(tail.iterator.map(f)))
  def flatMap[K2, V2](f: ((K, V)) => NonEmptyMap[K2, V2]): NonEmptyMap[K2, V2] = {
    val NonEmptyMap(head2, tail2) = f(head)
    NonEmptyMap(head2, ListMap.from(tail2.iterator ++ tail.iterator.flatMap(a => f(a).iterator)))
  }

  def iterator: Iterator[(K, V)] = Iterator(head) ++ tail.iterator
  def toListMap: ListMap[K, V] = ListMap.from(iterator)
  def toList: List[(K, V)] = iterator.toList
  def toVector: Vector[(K, V)] = iterator.toVector
  def toNonEmptyList: NonEmptyList[(K, V)] = NonEmptyList(head, tail.toList)
  def toNonEmptyVector: NonEmptyVector[(K, V)] = NonEmptyVector(head, tail.toVector)

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
