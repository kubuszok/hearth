package hearth
package fp

/** Dirty workaround to the problem that [[Traverse]] and [[Applicative]] both inherit from [[Functor]], and if both
  * would be in implicit scope as separate values, then when we asked for a [[Functor]] compiler would be confused and
  * return us none.
  *
  * @since 0.1.0
  */
trait ApplicativeTraverse[F[_]] extends Traverse[F] with Applicative[F] {

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    traverse[Id, A, B](fa)(f)(instances.IdentityApplicative)
}
object ApplicativeTraverse {

  def apply[F[_]](implicit F: ApplicativeTraverse[F]): ApplicativeTraverse[F] = F
}
