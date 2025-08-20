package hearth
package fp

import scala.util.control.{ControlThrowable, NoStackTrace}

/** Direct-style operations.
  *
  * The idea is that we could use some effect (sequentially) by doing something like:
  *
  * {{{
  * DirectStyle[F].async { await =>
  *   val a: A = await(fa: F[A])
  *   val b: B = await(fb: F[B])
  *   ...
  *   c: C
  * } // : F[C]
  * }}}
  *
  * and it would handle: errors, missing values, etc. as if we used something like for-comprehension/traverse, etc.
  *
  * It is not as seamless as Scala 3 direct-style relying on context functions and passing `await` as `given`, but
  * still, occasionally it can make the code easier to read, especially if we're working with some structure that does
  * not support `Applicative`/`Traverse`/etc., e.g.:
  *
  * {{{
  * def computeExprB[A](a: Expr[A]): MIO[Expr[B]] = ... // fallible computation with logs
  *
  * val mio: MIO[TypeClass[A]] = DirectStyle[MIO].async { await =>
  *   '{
  *     new TypeClass[A] {
  *       def method(a: A): B = ${ await(computeExprB('{ a })) } // <-- extracts Expr[B] out of MIO[Expr[B]],
  *     }                                                        //     kinda impossible with normal combinators
  *   }
  * } // all potential errors and logs are preserved
  * }}}
  *
  * @since 0.1.0
  */
trait DirectStyle[F[_]] {
  import DirectStyle.*

  protected def asyncUnsafe[A](owner: Await[F])(thunk: => A): F[A]
  protected def awaitUnsafe[A](owner: Await[F])(value: F[A]): A

  final private class AwaitImpl extends Await[F] {
    def apply[A](value: F[A]): A = awaitUnsafe(this)(value)
  }

  def async[A](await: Await[F] => A): F[A] = {
    val owner = new AwaitImpl
    asyncUnsafe(owner)(await(owner))
  }
}
object DirectStyle {

  sealed trait Await[F[_]] {
    def apply[A](value: F[A]): A
  }

  def apply[F[_]](implicit F: DirectStyle[F]): DirectStyle[F] = F

  implicit def DirectStyleForEither[Errors]: DirectStyle[Either[Errors, *]] = new DirectStyle[Either[Errors, *]] { ds =>
    private case class PassErrors(owner: Any, error: Errors) extends ControlThrowable with NoStackTrace

    @scala.annotation.nowarn
    override protected def asyncUnsafe[A](owner: Await[Either[Errors, *]])(thunk: => A): Either[Errors, A] = try
      Right(thunk)
    catch {
      case PassErrors(`owner`, error) => Left(error.asInstanceOf[Errors])
    }
    override protected def awaitUnsafe[A](owner: Await[Either[Errors, *]])(value: Either[Errors, A]): A = value match {
      case Left(error)  => throw PassErrors(owner, error)
      case Right(value) => value
    }
  }
}
