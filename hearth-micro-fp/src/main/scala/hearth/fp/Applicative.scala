package hearth
package fp

trait Applicative[F[_]] extends Functor[F] {

  def pure[A](a: A): F[A]

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

  override def map[A, B](fa: F[A])(f: A => B): F[B] = map2(fa, pure(()))((a, _) => f(a))
}

object Applicative {

  def apply[F[_]](implicit F: Applicative[F]): Applicative[F] = F

  final class PureOps[A](private val a: A) extends AnyVal {

    def pure[F[_]](implicit F: Applicative[F]): F[A] = F.pure(a)
  }

  final class Ops[F[_], A](private val fa: F[A]) extends AnyVal {

    def map2[B, C](fb: F[B])(f: (A, B) => C)(implicit F: Applicative[F]): F[C] = F.map2(fa, fb)(f)
  }
}
