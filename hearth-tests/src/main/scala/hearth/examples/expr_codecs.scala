package hearth
package examples
package expr_codecs

case class ServerConfig(host: String, port: Int)

case class AppConfig(server: ServerConfig, debug: Boolean)

sealed trait Shape
object Shape {
  case class Circle(radius: Double) extends Shape
  case class Rectangle(width: Double, height: Double) extends Shape
  case object Origin extends Shape
}

case object Sentinel

case class DataHolder(label: String, payload: hearth.data.Data)

case class VarargNumbers(xs: Int*)

case class ConfigWithFunctions(
    mapper: String => String = identity,
    flag: Boolean = false,
    name: String = "default"
)

class Preference[A]
object Preference {
  def apply[A]: Preference[A] = new Preference[A]
}

/** Not a case class, not a singleton, not a sealed hierarchy — nothing in `semiQuoteInternal` can handle it. */
class NotQuotable(val value: Int)

/** A case class with a field whose type (`NotQuotable`) has no codec and cannot be derived — derivation of the whole
  * type must fail naming the offending nested type when `toExpr` is exercised.
  */
case class HolderOfNotQuotable(label: String, payload: NotQuotable)

/** Two fields of the SAME nested type — used to prove the override-map `=:=` lookup serves repeated occurrences of one
  * type for BOTH fields (dedup-by-`=:=` must not drop one).
  */
case class TwoHosts(primary: String, secondary: String)
