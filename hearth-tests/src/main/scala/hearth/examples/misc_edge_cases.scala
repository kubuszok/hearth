package hearth
package examples
package miscedge

final case class Box[A](value: A)

object Outer {
  final case class Nested(value: Int)
}

object SingletonObject

object Aliases {
  type StringOption = Option[String]
}

/** No implicit instance of it exists anywhere. */
trait NoInstanceAvailable
