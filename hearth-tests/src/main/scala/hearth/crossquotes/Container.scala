package hearth
package crossquotes

trait Container[F[_]] {
  def value: F[String]
}
