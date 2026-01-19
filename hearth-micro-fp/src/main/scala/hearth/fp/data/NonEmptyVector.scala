package hearth
package fp
package data

import fp.instances.*
import fp.syntax.*

/** Non-empty vector.
  *
  * @since 0.1.0
  */
final case class NonEmptyVector[+A](head: A, tail: Vector[A]) {

  def +:[B >: A](a: B): NonEmptyVector[B] = NonEmptyVector(a, head +: tail)
  def :+[B >: A](a: B): NonEmptyVector[B] = NonEmptyVector(head, tail :+ a)

  def ++[B >: A](nev: NonEmptyVector[B]): NonEmptyVector[B] = NonEmptyVector(head, tail ++ nev.toVector)

  def map[B](f: A => B): NonEmptyVector[B] = NonEmptyVector(f(head), tail.map(f))
  def flatMap[B](f: A => NonEmptyVector[B]): NonEmptyVector[B] = {
    val NonEmptyVector(head2, tail2) = f(head)
    NonEmptyVector(head2, (tail2.iterator ++ tail.iterator.flatMap(a => f(a).iterator)).toVector)
  }

  def iterator: Iterator[A] = Iterator(head) ++ tail.iterator
  def toVector: Vector[A] = head +: tail
  def toList: List[A] = head :: tail.toList
  def toNonEmptyList: NonEmptyList[A] = NonEmptyList(head, tail.toList)

  def size: Int = 1 + tail.size

  def mkString(sep: String): String = toVector.mkString(sep)
  def mkString(start: String, sep: String, end: String): String = toVector.mkString(start, sep, end)

  override def toString: String = mkString("NonEmptyVector(", ", ", ")")
}
object NonEmptyVector {

  def apply[A](a: A, as: A*): NonEmptyVector[A] = NonEmptyVector(a, as.toVector)
  def fromVector[A](vector: Vector[A]): Option[NonEmptyVector[A]] = vector match {
    case head +: tail => Some(NonEmptyVector(head, tail))
    case _            => None
  }
  def one[A](a: A): NonEmptyVector[A] = NonEmptyVector(a, Vector.empty)

  implicit final val TraverseForNonEmptyVector: Traverse[NonEmptyVector] = new Traverse[NonEmptyVector] {
    override def traverse[G[_]: Applicative, A, B](fa: NonEmptyVector[A])(f: A => G[B]): G[NonEmptyVector[B]] =
      f(fa.head).map2(fa.tail.traverse(f))(NonEmptyVector(_, _))
    override def parTraverse[G[_]: Parallel, A, B](fa: NonEmptyVector[A])(f: A => G[B]): G[NonEmptyVector[B]] =
      f(fa.head).parMap2(fa.tail.parTraverse(f))(NonEmptyVector(_, _))
  }

  implicit def ParallelTraverseForEitherNonEmptyVector[Errors]: ParallelTraverse[Either[NonEmptyVector[Errors], *]] =
    new ParallelTraverse[Either[NonEmptyVector[Errors], *]] {

      override def pure[A](a: A): Either[NonEmptyVector[Errors], A] = Right(a)

      override def map2[A, B, C](fa: Either[NonEmptyVector[Errors], A], fb: => Either[NonEmptyVector[Errors], B])(
          f: (A, B) => C
      ): Either[NonEmptyVector[Errors], C] = (fa, fb) match {
        case (Left(e), _)         => Left(e)
        case (_, Left(e))         => Left(e)
        case (Right(a), Right(b)) => Right(f(a, b))
      }

      override def parMap2[A, B, C](fa: Either[NonEmptyVector[Errors], A], fb: => Either[NonEmptyVector[Errors], B])(
          f: (A, B) => C
      ): Either[NonEmptyVector[Errors], C] = (fa, fb) match {
        case (Left(e1), Left(e2)) => Left(e1 ++ e2)
        case (Left(e), Right(_))  => Left(e)
        case (Right(_), Left(e))  => Left(e)
        case (Right(a), Right(b)) => Right(f(a, b))
      }

      override def traverse[G[_]: Applicative, A, B](
          fa: Either[NonEmptyVector[Errors], A]
      )(f: A => G[B]): G[Either[NonEmptyVector[Errors], B]] = fa match {
        case Left(e)  => (Left(e): Either[NonEmptyVector[Errors], B]).pure[G]
        case Right(a) => f(a).map(Right(_))
      }

      override def parTraverse[G[_]: Parallel, A, B](
          fa: Either[NonEmptyVector[Errors], A]
      )(f: A => G[B]): G[Either[NonEmptyVector[Errors], B]] = fa match {
        case Left(e)  => (Left(e): Either[NonEmptyVector[Errors], B]).pure[G]
        case Right(a) => f(a).map(Right(_))
      }
    }
}
