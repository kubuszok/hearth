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
