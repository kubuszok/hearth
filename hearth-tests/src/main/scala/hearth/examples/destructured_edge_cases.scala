package hearth
package examples
package destructurededge

// $COVERAGE-OFF$ analyzed by macros at compile time, never run
final case class Box[A](value: A)

final case class Street(name: String)
final case class Home(street: Street)
final case class Resident(home: Home)

case object Marker

sealed trait Shape
object Shape {
  case object Circle extends Shape
  final case class Square(side: Int) extends Shape
}

class TwoClauses(val a: Int)(val b: String)

final case class Scale(factor: Int)
object Scale {
  implicit val default: Scale = Scale(2)
}

class Calc {
  def add(a: Int)(b: Int): Int = a + b
  def pick[A](a: A, b: A): A = { val _ = b; a }
  def withImplicit(a: Int)(implicit scale: Scale): Int = a * scale.factor
  def nullary(): Int = 1
  def parameterless: Int = 1
}

class Wrapped(val underlying: Int) {
  def doubled: Int = underlying * 2
}

object conversions {
  import scala.language.implicitConversions
  implicit def toWrapped(i: Int): Wrapped = new Wrapped(i)
}

object functions {
  def twice(i: Int): Int = i * 2
}
trait VarargFactory {
  def inheritedOf[A](elems: A*): Int = elems.size
}
class VarargHolder extends VarargFactory {
  def genericOf[A](elems: A*): Int = elems.size
  def monomorphicOf(elems: Int*): Int = elems.size
}
// $COVERAGE-ON$
