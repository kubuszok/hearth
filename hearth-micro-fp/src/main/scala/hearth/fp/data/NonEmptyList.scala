package hearth
package fp
package data

import fp.instances.*
import fp.syntax.*

/** Non-empty list.
  *
  * @since 0.1.0
  */
final case class NonEmptyList[+A](head: A, tail: List[A]) {

  def +:[B >: A](a: B): NonEmptyList[B] = NonEmptyList(a, head +: tail)
  def :+[B >: A](a: B): NonEmptyList[B] = NonEmptyList(head, tail :+ a)

  def ++[B >: A](nel: NonEmptyList[B]): NonEmptyList[B] = NonEmptyList(head, tail ++ nel.toList)

  def map[B](f: A => B): NonEmptyList[B] = NonEmptyList(f(head), tail.map(f))

  def toList: List[A] = head +: tail
  def toVector: Vector[A] = head +: tail.toVector
  def toNonEmptyVector: NonEmptyVector[A] = NonEmptyVector(head, tail.toVector)

  def mkString(sep: String): String = toList.mkString(sep)
  def mkString(start: String, sep: String, end: String): String = toList.mkString(start, sep, end)
}
object NonEmptyList {

  def apply[A](a: A, as: A*): NonEmptyList[A] = NonEmptyList(a, as.toList)
  def fromList[A](list: List[A]): Option[NonEmptyList[A]] = list match {
    case head :: tail => Some(NonEmptyList(head, tail))
    case Nil          => None
  }
  def one[A](a: A): NonEmptyList[A] = NonEmptyList(a, List.empty)

  implicit final val TraverseForNonEmptyList: Traverse[NonEmptyList] = new Traverse[NonEmptyList] {
    override def traverse[G[_]: Applicative, A, B](fa: NonEmptyList[A])(f: A => G[B]): G[NonEmptyList[B]] =
      f(fa.head).map2(fa.tail.traverse(f))(NonEmptyList(_, _))
    override def parTraverse[G[_]: Parallel, A, B](fa: NonEmptyList[A])(f: A => G[B]): G[NonEmptyList[B]] =
      f(fa.head).parMap2(fa.tail.parTraverse(f))(NonEmptyList(_, _))
  }

  implicit def ParallelTraverseForEitherNonEmptyList[Errors]: ParallelTraverse[Either[NonEmptyList[Errors], *]] =
    new ParallelTraverse[Either[NonEmptyList[Errors], *]] {

      override def pure[A](a: A): Either[NonEmptyList[Errors], A] = Right(a)

      override def map2[A, B, C](fa: Either[NonEmptyList[Errors], A], fb: => Either[NonEmptyList[Errors], B])(
          f: (A, B) => C
      ): Either[NonEmptyList[Errors], C] = (fa, fb) match {
        case (Left(e), _)         => Left(e)
        case (_, Left(e))         => Left(e)
        case (Right(a), Right(b)) => Right(f(a, b))
      }

      override def parMap2[A, B, C](fa: Either[NonEmptyList[Errors], A], fb: => Either[NonEmptyList[Errors], B])(
          f: (A, B) => C
      ): Either[NonEmptyList[Errors], C] = (fa, fb) match {
        case (Left(e1), Left(e2)) => Left(e1 ++ e2)
        case (Left(e), Right(_))  => Left(e)
        case (Right(_), Left(e))  => Left(e)
        case (Right(a), Right(b)) => Right(f(a, b))
      }

      override def traverse[G[_]: Applicative, A, B](
          fa: Either[NonEmptyList[Errors], A]
      )(f: A => G[B]): G[Either[NonEmptyList[Errors], B]] = fa match {
        case Left(e)  => (Left(e): Either[NonEmptyList[Errors], B]).pure[G]
        case Right(a) => f(a).map(Right(_))
      }

      override def parTraverse[G[_]: Parallel, A, B](
          fa: Either[NonEmptyList[Errors], A]
      )(f: A => G[B]): G[Either[NonEmptyList[Errors], B]] = fa match {
        case Left(e)  => (Left(e): Either[NonEmptyList[Errors], B]).pure[G]
        case Right(a) => f(a).map(Right(_))
      }
    }
}
