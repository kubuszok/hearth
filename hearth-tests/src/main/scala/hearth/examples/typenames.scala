package hearth
package examples

// $SCOVERAGE-OFF$ This file is analyzed mostly in the compile time.

/** Test type class for the runtime-aware type name printer. */
trait TypeName[A] {
  def name: String
}
object TypeName {
  def apply[A](implicit ev: TypeName[A]): TypeName[A] = ev
  def instance[A](n: String): TypeName[A] = new TypeName[A] { def name = n }
}
