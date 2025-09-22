package hearth
package fp

/** Things that can be mapped over in parallel.
  *
  * By parallel, we don't mean "parallel" in the sense of "concurrency" or "event loops", but rather "parallel" in the
  * sense, that each computation is independent, and even if one fails we would still compute the other one, even though
  * we already know that computation as a whole will fail.
  *
  * If [[Applicative]] is "sequential" and with "fail-fast" semantics, Parallel can aggregate errors.
  *
  * @since 0.1.0
  */
trait Parallel[F[_]] extends Applicative[F] {

  def parMap2[A, B, C](fa: F[A], fb: => F[B])(f: (A, B) => C): F[C]
}

object Parallel {

  def apply[F[_]](implicit F: Parallel[F]): Parallel[F] = F

  final class Ops[F[_], A](private val fa: F[A]) extends AnyVal {

    def parMap2[B, C](fb: => F[B])(f: (A, B) => C)(implicit F: Parallel[F]): F[C] = F.parMap2(fa, fb)(f)
    def parTuple[B](fb: => F[B])(implicit F: Parallel[F]): F[(A, B)] = F.parMap2(fa, fb)(_ -> _)
  }
}
