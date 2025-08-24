package hearth
package fp

import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}
import hearth.fp.syntax.*

/** Instances for basic types (which couldn't be placed in type class modules, because they are hierarchical AND they
  * are not types defined in this library).
  *
  * @since 0.1.0
  */
object instances extends instances0
private[fp] trait instances0 extends instances1 { this: instances.type =>

  /** While it sounds weird that [[Id]] is a [[Parallel]], it's actually quite useful, because it assumes that there are
    * _no_ errors, nor side effects, so the code is _always_ behaving as if computed independent sections in parallel.
    *
    * @since 0.1.0
    */
  implicit lazy val ParallelTraverseForId: ParallelTraverse[Id] = new ParallelTraverse[Id] {

    override def pure[A](a: A): A = a

    override def map2[A, B, C](fa: A, fb: => B)(f: (A, B) => C): C = f(fa, fb)

    override def parMap2[A, B, C](fa: A, fb: => B)(f: (A, B) => C): C = f(fa, fb)

    override def traverse[G[_]: Applicative, A, B](fa: A)(f: A => G[B]): G[B] = f(fa)

    override def parTraverse[G[_]: Parallel, A, B](fa: A)(f: A => G[B]): G[B] = f(fa)
  }

  /** While it sounds weird that [[Option]] is a [[Parallel]], it's actually quite useful, because we have only an
    * information that some value is missing, so the code is _always_ behaving as if computed independent sections in
    * parallel and aggegated information about missing values.
    *
    * @since 0.1.0
    */
  implicit lazy val ParallelTraverseForOption: ParallelTraverse[Option] = new ParallelTraverse[Option] {

    override def pure[A](a: A): Option[A] = Some(a)

    override def map2[A, B, C](fa: Option[A], fb: => Option[B])(f: (A, B) => C): Option[C] =
      fa.flatMap(a => fb.map(b => f(a, b)))

    override def parMap2[A, B, C](fa: Option[A], fb: => Option[B])(f: (A, B) => C): Option[C] = fa.zip(fb).map(f.tupled)

    override def traverse[G[_]: Applicative, A, B](fa: Option[A])(f: A => G[B]): G[Option[B]] =
      fa.fold(Option.empty[B].pure[G])(f(_).map(Some(_)))

    override def parTraverse[G[_]: Parallel, A, B](fa: Option[A])(f: A => G[B]): G[Option[B]] =
      fa.fold(Option.empty[B].pure[G])(f(_).map(Some(_)))
  }

  /** While [[scala.Either]] on its own is not a [[Parallel]], with [[List]] as left value, we can aggregate errors,
    * thus it makes sense to make such special case parallel.
    *
    * @since 0.1.0
    */
  implicit def ParallelTraverseForEitherList[Errors]: ParallelTraverse[Either[List[Errors], *]] =
    new ParallelTraverse[Either[List[Errors], *]] {

      override def pure[A](a: A): Either[List[Errors], A] = Right(a)

      override def map2[A, B, C](fa: Either[List[Errors], A], fb: => Either[List[Errors], B])(
          f: (A, B) => C
      ): Either[List[Errors], C] = fa.flatMap(a => fb.map(b => f(a, b)))

      override def parMap2[A, B, C](fa: Either[List[Errors], A], fb: => Either[List[Errors], B])(
          f: (A, B) => C
      ): Either[List[Errors], C] = (fa, fb) match {
        case (Left(e1), Left(e2)) => Left(e1 ++ e2)
        case (Left(e), Right(_))  => Left(e)
        case (Right(_), Left(e))  => Left(e)
        case (Right(a), Right(b)) => Right(f(a, b))
      }

      override def traverse[G[_]: Applicative, A, B](
          fa: Either[List[Errors], A]
      )(f: A => G[B]): G[Either[List[Errors], B]] =
        fa match {
          case Left(e)  => (Left(e): Either[List[Errors], B]).pure[G]
          case Right(a) => f(a).map(Right(_))
        }

      override def parTraverse[G[_]: Parallel, A, B](
          fa: Either[List[Errors], A]
      )(f: A => G[B]): G[Either[List[Errors], B]] =
        fa match {
          case Left(e)  => (Left(e): Either[List[Errors], B]).pure[G]
          case Right(a) => f(a).map(Right(_))
        }
    }

  /** While [[scala.Either]] on its own is not a [[Parallel]], with [[Vector]] as left value, we can aggregate errors,
    * thus it makes sense to make such special case parallel.
    *
    * @since 0.1.0
    */
  implicit def ParallelTraverseForEitherVector[Errors]: ParallelTraverse[Either[Vector[Errors], *]] =
    new ParallelTraverse[Either[Vector[Errors], *]] {

      override def pure[A](a: A): Either[Vector[Errors], A] = Right(a)

      override def map2[A, B, C](fa: Either[Vector[Errors], A], fb: => Either[Vector[Errors], B])(
          f: (A, B) => C
      ): Either[Vector[Errors], C] = fa.flatMap(a => fb.map(b => f(a, b)))

      override def parMap2[A, B, C](fa: Either[Vector[Errors], A], fb: => Either[Vector[Errors], B])(
          f: (A, B) => C
      ): Either[Vector[Errors], C] = (fa, fb) match {
        case (Left(e1), Left(e2)) => Left(e1 ++ e2)
        case (Left(e), Right(_))  => Left(e)
        case (Right(_), Left(e))  => Left(e)
        case (Right(a), Right(b)) => Right(f(a, b))
      }

      override def traverse[G[_]: Applicative, A, B](
          fa: Either[Vector[Errors], A]
      )(f: A => G[B]): G[Either[Vector[Errors], B]] =
        fa match {
          case Left(e)  => (Left(e): Either[Vector[Errors], B]).pure[G]
          case Right(a) => f(a).map(Right(_))
        }

      override def parTraverse[G[_]: Parallel, A, B](
          fa: Either[Vector[Errors], A]
      )(f: A => G[B]): G[Either[Vector[Errors], B]] =
        fa match {
          case Left(e)  => (Left(e): Either[Vector[Errors], B]).pure[G]
          case Right(a) => f(a).map(Right(_))
        }
    }

  /** Try is [[Applicative]] as it stores exactly one error, so we cannot aggregate errors.
    *
    * @since 0.1.0
    */
  implicit def ApplicativeTraverseForTry: ApplicativeTraverse[Try] = new ApplicativeTraverse[Try] {

    override def pure[A](a: A): Try[A] = Success(a)

    override def map2[A, B, C](fa: Try[A], fb: => Try[B])(f: (A, B) => C): Try[C] =
      fa.flatMap(a => fb.map(b => f(a, b)))

    override def traverse[G[_]: Applicative, A, B](fa: Try[A])(f: A => G[B]): G[Try[B]] =
      fa match {
        case Failure(e) => (Failure(e): Try[B]).pure[G]
        case Success(a) => f(a).map(Success(_))
      }

    override def parTraverse[G[_]: Parallel, A, B](fa: Try[A])(f: A => G[B]): G[Try[B]] =
      fa match {
        case Failure(e) => (Failure(e): Try[B]).pure[G]
        case Success(a) => f(a).map(Success(_))
      }
  }

  /** [[List]] isn't ment to represent fallible computation, so we don't have parallel instance for it.
    *
    * @since 0.1.0
    */
  implicit lazy val ApplicativeTraverseForList: ApplicativeTraverse[List] = new ApplicativeTraverse[List] {

    override def pure[A](a: A): List[A] = List(a)

    override def map2[A, B, C](fa: List[A], fb: => List[B])(f: (A, B) => C): List[C] =
      fa.flatMap(a => fb.map(b => f(a, b)))

    override def traverse[G[_]: Applicative, A, B](fa: List[A])(f: A => G[B]): G[List[B]] =
      fa.foldLeft(new ListBuffer[B].pure[G]) { (bufferG, a) =>
        bufferG.map2(f(a))(_.append(_))
      }.map(_.toList)

    override def parTraverse[G[_]: Parallel, A, B](fa: List[A])(f: A => G[B]): G[List[B]] =
      fa.foldLeft(new ListBuffer[B].pure[G]) { (bufferG, a) =>
        bufferG.parMap2(f(a))(_.append(_))
      }.map(_.toList)
  }

  /** [[Vector]] isn't ment to represent fallible computation, so we don't have parallel instance for it.
    *
    * @since 0.1.0
    */
  implicit lazy val ApplicativeTraverseForVector: ApplicativeTraverse[Vector] = new ApplicativeTraverse[Vector] {

    override def pure[A](a: A): Vector[A] = Vector(a)

    override def map2[A, B, C](fa: Vector[A], fb: => Vector[B])(f: (A, B) => C): Vector[C] =
      fa.flatMap(a => fb.map(b => f(a, b)))

    override def traverse[G[_]: Applicative, A, B](fa: Vector[A])(f: A => G[B]): G[Vector[B]] =
      fa.foldLeft(new ListBuffer[B].pure[G]) { (bufferG, a) =>
        bufferG.map2(f(a))(_.append(_))
      }.map(_.toVector)

    override def parTraverse[G[_]: Parallel, A, B](fa: Vector[A])(f: A => G[B]): G[Vector[B]] =
      fa.foldLeft(new ListBuffer[B].pure[G]) { (bufferG, a) =>
        bufferG.parMap2(f(a))(_.append(_))
      }.map(_.toVector)
  }

}
private[fp] trait instances1 { this: instances.type =>

  /** [[scala.Either]] on its own is [[Applicative]] as it stores exactly one error, so we cannot aggregate errors.
    *
    * If you need [[Parallel]] instance for Either use it with combination of:
    *   - [[List]]
    *   - [[Vector]]
    *   - [[NonEmptyList]]
    *   - [[NonEmptyVector]]
    *
    * @since 0.1.0
    */
  implicit def ApplicativeTraverseForEither[Errors]: ApplicativeTraverse[Either[Errors, *]] =
    new ApplicativeTraverse[Either[Errors, *]] {

      override def pure[A](a: A): Either[Errors, A] = Right(a)

      override def map2[A, B, C](fa: Either[Errors, A], fb: => Either[Errors, B])(f: (A, B) => C): Either[Errors, C] =
        fa match {
          case Left(e)  => Left(e)
          case Right(a) =>
            fb match {
              case Left(e)  => Left(e)
              case Right(b) => Right(f(a, b))
            }
        }

      override def traverse[G[_]: Applicative, A, B](fa: Either[Errors, A])(f: A => G[B]): G[Either[Errors, B]] =
        fa match {
          case Left(e)  => (Left(e): Either[Errors, B]).pure[G]
          case Right(a) => f(a).map(Right(_))
        }

      override def parTraverse[G[_]: Parallel, A, B](fa: Either[Errors, A])(f: A => G[B]): G[Either[Errors, B]] =
        fa match {
          case Left(e)  => (Left(e): Either[Errors, B]).pure[G]
          case Right(a) => f(a).map(Right(_))
        }
    }
}
