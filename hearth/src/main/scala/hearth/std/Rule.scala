package hearth
package std

/** Trait used to mark values of something that can:
  *   - represent some makr expansion "rule"
  *   - which would check if some condition applies ("if type represents an enum")
  *   - attempt to expand if it does ("then genereate the code as follows")
  *   - and yielding to the next rule in line when it does not apply ("or try the next rule")
  *
  * @since 0.3.0
  */
trait Rule {

  def name: String

  override def toString: String = name
}
object Rule {

  def matched[A](result: A): Applicability[A] = Applicability.Matched(result)
  def yielded(reasons: String*): Applicability[Nothing] = Applicability.Yielded(reasons.toVector)

  /** The result of applying a rule to a context.
    *
    * @since 0.3.0
    */
  sealed trait Applicability[+A] extends Product with Serializable
  object Applicability {
    final case class Matched[A](result: A) extends Applicability[A]
    final case class Yielded(reason: Vector[String]) extends Applicability[Nothing]
  }
}
