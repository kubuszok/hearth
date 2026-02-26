package hearth
package typename

// $SCOVERAGE-OFF$ This file is analyzed mostly in the compile time.

/** A type class that provides 4 different ways to print the name of a type.
  *
  * Instances are auto-derived via `implicit def`/`inline given` using macro expansion. The derivation uses
  * runtime-aware type printing, so abstract type parameters are replaced with runtime values from any `TypeName`
  * instances found in the implicit scope.
  *
  * @tparam A
  *   the type whose name is being printed
  *
  * @since 0.3.0
  */
trait TypeName[A] {

  /** ANSI colored, fully qualified names. */
  def prettyPrint: String

  /** No ANSI, fully qualified names. */
  def plainPrint: String

  /** No ANSI, short names with type parameters (e.g. `Option[String]`). */
  def simplePrint: String

  /** No ANSI, short name only, no type parameters (e.g. `Option`). */
  def shortPrint: String

  override def toString: String = plainPrint
}
object TypeName extends TypeNameCompanion {

  def apply[A](implicit ev: TypeName[A]): TypeName[A] = ev
}
