package hearth
package fp
package data

import fp.instances.*
import fp.syntax.*

final case class NonEmptyList[+A](head: A, tail: List[A]) {

  def +:[B >: A](a: B): NonEmptyList[B] = NonEmptyList(a, head +: tail)
  def :+[B >: A](a: B): NonEmptyList[B] = NonEmptyList(head, tail :+ a)

  def ++[B >: A](nel: NonEmptyList[B]): NonEmptyList[B] = NonEmptyList(head, tail ++ nel.toList)

  def toList: List[A] = head +: tail
}
object NonEmptyList {

  def apply[A](a: A, as: A*): NonEmptyList[A] = NonEmptyList(a, as.toList)
  def fromList[A](list: List[A]): Option[NonEmptyList[A]] = list match {
    case head :: tail => Some(NonEmptyList(head, tail))
    case Nil          => None
  }
  def one[A](a: A): NonEmptyList[A] = NonEmptyList(a, List.empty)

  implicit final val NonEmptyListTraverse: Traverse[NonEmptyList] = new Traverse[NonEmptyList] {
    def traverse[G[_]: Applicative, A, B](fa: NonEmptyList[A])(f: A => G[B]): G[NonEmptyList[B]] =
      f(fa.head).map2(fa.tail.traverse(f))(NonEmptyList(_, _))
    def parTraverse[G[_]: Parallel, A, B](fa: NonEmptyList[A])(f: A => G[B]): G[NonEmptyList[B]] =
      f(fa.head).parMap2(fa.tail.parTraverse(f))(NonEmptyList(_, _))
  }
}
