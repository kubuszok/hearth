package hearth
package fp

trait ApplicativeTraverse[F[_]] extends Traverse[F] with Applicative[F] {

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    traverse[Id, A, B](fa)(f)(instances.IdentityApplicative)
}
object ApplicativeTraverse {

  def apply[F[_]](implicit F: ApplicativeTraverse[F]): ApplicativeTraverse[F] = F
}
