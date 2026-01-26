package hearth
package std

import hearth.fp.DirectStyle
import hearth.fp.data.{NonEmptyList, NonEmptyMap, NonEmptyVector}
import scala.collection.immutable.ListMap

/** An utility which helps combining a sequence of rules into the rule application result.
  *
  * @since 0.3.0
  */
final class Rules[R <: Rule] private (rules: NonEmptyList[R]) {
  import Rules.*

  /** Apply the rules to the given context.
    *
    * You need to tell it how to check if rule applies, e.g. pass some value to some method defined on it.
    *
    * @since 0.3.0
    */
  def apply[A](attempt: R => Rule.Applicability[A]): ApplicationResult[R, A] =
    applyRules(rules.toList, Vector.empty)(attempt)

  /** Apply the rules to the given context, but with some effect (e.g. [[MIO]]).
    *
    * You need to tell it how to check if rule applies, e.g. pass some value to some method defined on it.
    *
    * @since 0.3.0
    */
  def apply[F[_]: DirectStyle, A](attempt: R => F[Rule.Applicability[A]]): F[ApplicationResult[R, A]] =
    DirectStyle[F].scoped { runSafe =>
      applyRules(rules.toList, Vector.empty) { rule =>
        runSafe(attempt(rule))
      }
    }
}
object Rules {

  /** Create a new [[Rules]] instance from the given rules.
    *
    * @since 0.3.0
    */
  def apply[R <: Rule](head: R, tail: R*): Rules[R] = new Rules(NonEmptyList(head, tail.toList))

  /** Create a new [[Rules]] instance from the given rules.
    *
    * @since 0.3.0
    */
  def from[R <: Rule](rules: NonEmptyList[R]): Rules[R] = new Rules(rules)

  /** Create a new [[Rules]] instance from the given rules.
    *
    * @since 0.3.0
    */
  def from[R <: Rule](rules: NonEmptyVector[R]): Rules[R] = new Rules(rules.toNonEmptyList)

  /** The result of applying a rule to a context.
    *
    * It's:
    *
    *   - either the map of reasons why the rules did not apply (if provided)
    *   - or the result of the rule application
    *
    * @since 0.3.0
    */
  type ApplicationResult[R <: Rule, A] = Either[NonEmptyMap[R, Vector[String]], A]

  @scala.annotation.tailrec
  private def applyRules[R <: Rule, A](
      remaining: List[R],
      failed: Vector[(R, Vector[String])]
  )(
      attempt: R => Rule.Applicability[A]
  ): ApplicationResult[R, A] = remaining match {
    case Nil          => Left(NonEmptyMap.fromListMap(ListMap.from(failed)).get)
    case rule :: rest =>
      attempt(rule) match {
        case Rule.Applicability.Matched(result) => Right(result)
        case Rule.Applicability.Yielded(reason) => applyRules(rest, failed :+ (rule -> reason))(attempt)
      }
  }
}
