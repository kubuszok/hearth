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
