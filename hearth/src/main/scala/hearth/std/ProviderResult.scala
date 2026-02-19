package hearth
package std

import hearth.fp.data.NonEmptyMap

/** Result type for provider-based extractors.
  *
  * Captures either the matched value or a [[NonEmptyMap]] of skip/error reasons keyed by provider name. This allows
  * callers to inspect why a particular type wasn't recognized.
  *
  * @tparam A
  *   the type of the matched value
  *
  * @since 0.6.0
  */
sealed trait ProviderResult[+A] extends Product with Serializable {

  def toOption: Option[A] = this match {
    case ProviderResult.Matched(value) => Some(value)
    case _: ProviderResult.Skipped     => None
  }

  def map[B](f: A => B): ProviderResult[B] = this match {
    case ProviderResult.Matched(value) => ProviderResult.Matched(f(value))
    case s: ProviderResult.Skipped     => s
  }

  def flatMap[B](f: A => ProviderResult[B]): ProviderResult[B] = this match {
    case ProviderResult.Matched(value) => f(value)
    case s: ProviderResult.Skipped     => s
  }
}
object ProviderResult {
  final case class Matched[A](value: A) extends ProviderResult[A]
  final case class Skipped(reasons: NonEmptyMap[String, Either[Throwable, String]]) extends ProviderResult[Nothing]

  /** Helper to create a single-entry Skipped with a string reason. */
  def skipped(providerName: String, reason: String): Skipped =
    Skipped(NonEmptyMap.one(providerName -> Right(reason)))

  /** Helper to create a single-entry Skipped with a throwable. */
  def failed(providerName: String, error: Throwable): Skipped =
    Skipped(NonEmptyMap.one(providerName -> Left(error)))
}
