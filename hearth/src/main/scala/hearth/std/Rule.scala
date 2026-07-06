package hearth
package std

/** Trait used to mark values of something that can:
  *   - represent some macro expansion "rule"
  *   - which would check if some condition applies ("if type represents an enum")
  *   - attempt to expand if it does ("then generate the code as follows")
  *   - and yielding to the next rule in line when it does not apply ("or try the next rule")
  *
  * @see
  *   [[Rules]] for combining a sequence of rules into a single application result
  *
  * @since 0.3.0
  */
trait Rule {

  def name: String

  override def toString: String = name
}
object Rule {

  /** Signals the rule applied and produced `result`; [[Rules]] stops here and returns it.
    * @since 0.3.0
    */
  def matched[A](result: A): Applicability[A] = Applicability.Matched(result)

  /** Signals the rule did not apply, carrying `reasons` so [[Rules]] can move on and aggregate them.
    * @since 0.3.0
    */
  def yielded(reasons: String*): Applicability[Nothing] = Applicability.Yielded(reasons.toVector)

  /** The result of applying a rule to a context.
    * @since 0.3.0
    */
  sealed trait Applicability[+A] extends Product with Serializable
  object Applicability {
    final case class Matched[A](result: A) extends Applicability[A]
    final case class Yielded(reason: Vector[String]) extends Applicability[Nothing]
  }
}
