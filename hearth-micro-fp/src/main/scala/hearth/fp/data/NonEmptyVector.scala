package hearth
package fp
package data

import fp.instances.*
import fp.syntax.*

final case class NonEmptyVector[+A](head: A, tail: Vector[A]) {

  def +:[B >: A](a: B): NonEmptyVector[B] = NonEmptyVector(a, head +: tail)
  def :+[B >: A](a: B): NonEmptyVector[B] = NonEmptyVector(head, tail :+ a)

  def ++[B >: A](nev: NonEmptyVector[B]): NonEmptyVector[B] = NonEmptyVector(head, tail ++ nev.toVector)

  def toVector: Vector[A] = head +: tail
}
object NonEmptyVector {

  def apply[A](a: A, as: A*): NonEmptyVector[A] = NonEmptyVector(a, as.toVector)
  def fromVector[A](vector: Vector[A]): Option[NonEmptyVector[A]] = vector match {
    case head +: tail => Some(NonEmptyVector(head, tail))
    case _            => None
  }
  def one[A](a: A): NonEmptyVector[A] = NonEmptyVector(a, Vector.empty)

  implicit final val NonEmptyVectorTraverse: Traverse[NonEmptyVector] = new Traverse[NonEmptyVector] {
    def traverse[G[_]: Applicative, A, B](fa: NonEmptyVector[A])(f: A => G[B]): G[NonEmptyVector[B]] =
      f(fa.head).map2(fa.tail.traverse(f))(NonEmptyVector(_, _))
    def parTraverse[G[_]: Parallel, A, B](fa: NonEmptyVector[A])(f: A => G[B]): G[NonEmptyVector[B]] =
      f(fa.head).parMap2(fa.tail.parTraverse(f))(NonEmptyVector(_, _))
  }
}
