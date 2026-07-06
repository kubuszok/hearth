package hearth
package std

import hearth.fp.data.NonEmptyMap

/** Result type for provider-based extractors.
  *
  * Captures either the matched value or a [[NonEmptyMap]] of skip/error reasons keyed by provider name. This allows
  * callers to inspect why a particular type wasn't recognized.
  *
  * A result is one of two shapes:
  *   - [[ProviderResult.Matched]]`(value)` - a provider recognised the type and produced its proof;
  *   - [[ProviderResult.Skipped]]`(reasons)` - every provider declined, carrying a per-provider reason map whose values
  *     are either a `String` (the provider deliberately declined) or a `Throwable` (the provider recognised the shape
  *     but errored while building). Create single-entry skips with [[ProviderResult.skipped]] /
  *     [[ProviderResult.failed]].
  *
  * @since 0.3.0
  *
  * @tparam A
  *   the type of the matched value
  */
sealed trait ProviderResult[+A] extends Product with Serializable {

  /** `Some(value)` on [[ProviderResult.Matched]], `None` on [[ProviderResult.Skipped]] (discarding the reasons).
    * @since 0.3.0
    */
  def toOption: Option[A] = this match {
    case ProviderResult.Matched(value) => Some(value)
    case _: ProviderResult.Skipped     => None
  }

  /** Transforms the matched value; a [[ProviderResult.Skipped]] is returned unchanged (its reasons are preserved).
    * @since 0.3.0
    */
  def map[B](f: A => B): ProviderResult[B] = this match {
    case ProviderResult.Matched(value) => ProviderResult.Matched(f(value))
    case s: ProviderResult.Skipped     => s
  }

  /** Chains another provider result off the matched value; a [[ProviderResult.Skipped]] short-circuits unchanged.
    * @since 0.3.0
    */
  def flatMap[B](f: A => ProviderResult[B]): ProviderResult[B] = this match {
    case ProviderResult.Matched(value) => f(value)
    case s: ProviderResult.Skipped     => s
  }
}

/** Provenance of the provider that produced a matched std result.
  *
  * Skip reasons already name the providers that declined, but a matched result historically carried no way to tell
  * WHICH provider produced it - so callers could not distinguish a built-in from a specific `StandardMacroExtension`
  * except by fragile type filters. This captures the matching provider's `name` and implementing class. See issue #329.
  *
  * @since 0.4.1
  */
final case class ProviderProvenance(providerName: String, providerClassName: String) {

  /** Whether the matched provider is one of Hearth's built-ins (which all live in the `hearth.std.extensions` package),
    * as opposed to a third-party `StandardMacroExtension` loaded from the classpath.
    */
  def isBuiltIn: Boolean = providerClassName.startsWith("hearth.std.extensions.")
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
