package hearth
package fp

import scala.util.{Failure, Success, Try}
import scala.util.control.{ControlThrowable, NoStackTrace}

/** Direct-style operations.
  *
  * The idea is that we could use some effect (sequentially) by doing something like:
  *
  * {{{
  * DirectStyle[F].scoped { runSafe =>
  *   val a: A = runSafe(fa: F[A])
  *   val b: B = runSafe(fb: F[B])
  *   ...
  *   c: C
  * } // : F[C]
  * }}}
  *
  * and it would handle: errors, missing values, etc. as if we used something like for-comprehension/traverse, etc.
  *
  * It is not as seamless as Scala 3 direct-style relying on context functions and passing `runSafe` as `given`, but
  * still, occasionally it can make the code easier to read, especially if we're working with some structure that does
  * not support `Applicative`/`Traverse`/etc., e.g.:
  *
  * {{{
  * def computeExprB[A](a: Expr[A]): MIO[Expr[B]] = ... // fallible computation with logs
  *
  * val mio: MIO[TypeClass[A]] = DirectStyle[MIO].scoped { runSafe =>
  *   '{
  *     new TypeClass[A] {
  *       def method(a: A): B = ${ runSafe(computeExprB('{ a })) } // <-- extracts Expr[B] out of MIO[Expr[B]],
  *     }                                                          //     kinda impossible with normal combinators
  *   }
  * } // all potential errors and logs are preserved
  * }}}
  *
  * Warning: [[DirectStyle]] is not guarantted to be stack-safe. As a matter of the fact if might cause stack overflow
  * if you use it with some effect that would be stack-safe with normal combinators, e.g. [[MEval]] or [[MIO]].
  *
  * @since 0.1.0
  */
trait DirectStyle[F[_]] {
  import DirectStyle.*

  def scoped[A](thunk: RunSafe[F] => A): F[A] = {
    val runSafe = new RunSafe(this)
    scopedUnsafe(runSafe.asOwner)(thunk(runSafe))
  }

  protected def scopedUnsafe[A](owner: ScopeOwner[F])(thunk: => A): F[A]
  protected def runUnsafe[A](owner: ScopeOwner[F])(value: F[A]): A
}
object DirectStyle {

  def apply[F[_]](implicit F: DirectStyle[F]): DirectStyle[F] = F

  final class RunSafe[F[_]](directStyle: DirectStyle[F]) {
    def apply[A](value: F[A]): A = directStyle.runUnsafe(this.asOwner)(value)
    private[DirectStyle] def asOwner: ScopeOwner[F] = this.asInstanceOf[ScopeOwner[F]]
  }

  /** When we use nested direct style operations, we need a way to distinct who is the managing which `runSafe`. Scope
    * owner is such an opaque value that we can use in the implementation to distinct scopes.
    */
  type ScopeOwner[F[_]] <: Any

  /** Instance useful if we want to use [[DirectStyle]] without any effect.
    *
    * @since 0.1.0
    */
  implicit def DirectStyleForId: DirectStyle[Id] = new DirectStyle[Id] {
    override protected def scopedUnsafe[A](owner: ScopeOwner[Id])(thunk: => A): Id[A] = thunk
    override protected def runUnsafe[A](owner: ScopeOwner[Id])(value: Id[A]): A = value
  }

  /** Allows using [[DirectStyle]] with [[scala.Either]].
    *
    * @since 0.1.0
    */
  implicit def DirectStyleForEither[Errors]: DirectStyle[Either[Errors, *]] = new DirectStyle[Either[Errors, *]] {
    final private case class PassErrors(owner: Any, error: Errors) extends ControlThrowable with NoStackTrace

    @scala.annotation.nowarn
    override protected def scopedUnsafe[A](owner: ScopeOwner[Either[Errors, *]])(thunk: => A): Either[Errors, A] = try
      Right(thunk)
    catch {
      case PassErrors(`owner`, error) => Left(error.asInstanceOf[Errors])
    }
    override protected def runUnsafe[A](owner: ScopeOwner[Either[Errors, *]])(value: Either[Errors, A]): A =
      value match {
        case Left(error)  => throw PassErrors(owner, error)
        case Right(value) => value
      }
  }

  /** Allows using [[DirectStyle]] with [[scala.Option]].
    *
    * @since 0.1.0
    */
  implicit lazy val DirectStyleForOption: DirectStyle[Option] = new DirectStyle[Option] {
    final private case class PassErrors(owner: Any) extends ControlThrowable with NoStackTrace

    @scala.annotation.nowarn
    override protected def scopedUnsafe[A](owner: ScopeOwner[Option])(thunk: => A): Option[A] = try
      Some(thunk)
    catch {
      case PassErrors(`owner`) => None
    }
    override protected def runUnsafe[A](owner: ScopeOwner[Option])(value: Option[A]): A = value match {
      case None        => throw PassErrors(owner)
      case Some(value) => value
    }
  }

  /** Allows using [[DirectStyle]] with [[scala.util.Try]].
    *
    * @since 0.1.0
    */
  implicit lazy val DirectStyleForTry: DirectStyle[Try] = new DirectStyle[Try] {
    final private case class PassErrors(owner: Any, error: Throwable) extends ControlThrowable with NoStackTrace

    @scala.annotation.nowarn
    override protected def scopedUnsafe[A](owner: ScopeOwner[Try])(thunk: => A): Try[A] = try
      Success(thunk)
    catch {
      case PassErrors(`owner`, error) => Failure(error.asInstanceOf[Throwable])
    }
    override protected def runUnsafe[A](owner: ScopeOwner[Try])(value: Try[A]): A = value match {
      case Failure(error) => throw PassErrors(owner, error)
      case Success(value) => value
    }
  }
}
