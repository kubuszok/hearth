package hearth
package examples
package methodsedge

@scala.annotation.nowarn
abstract class Modifiers {
  def abstractDef(a: Int): Int
  final def finalDef: Int = 1
  def concreteDef: Int = 1
  private def privateDef: Int = 1
  protected def protectedDef: Int = 1
  private[methodsedge] def packagePrivateDef: Int = 1
  protected[methodsedge] def packageProtectedDef: Int = 1
  implicit def implicitDef: String = ""
}

class ModifiersImpl extends Modifiers {
  override def abstractDef(a: Int): Int = a
  final override def concreteDef: Int = 2
}

trait Stackable {
  def stack(a: Int): Int
}
trait StackableOverride extends Stackable {
  abstract override def stack(a: Int): Int = super.stack(a) + 1
}

class Bounds {
  def lower[B >: Int](b: B): B = b
  def both[C >: String <: CharSequence](c: C): C = c
}

class Overloads {
  def over(a: Int): Int = a
  def over(a: String): String = a
  def over(a: Int, b: Int): Int = a + b
}

class Symbolic(val value: Int) {
  def +(other: Int): Int = value + other
  def unary_- : Int = -value
}

class Shapes {
  def laterDefaults(a: Int)(b: Int = 2, c: Int = 3): Int = a + b + c
  def byName(a: => Int): Int = a
  def curried(a: Int)(b: Int)(implicit c: Int): Int = a + b + c
  def threeArgs(a: Int, b: Int, c: Int): Int = a + b + c
}

@scala.annotation.nowarn
class Accessors {
  val plainVal: Int = 1
  var plainVar: Int = 2
  lazy val plainLazy: Int = 3
  def plainDef: Int = 4
}

trait AbstractMembers {
  val abstractVal: Int
  def abstractDef: Int
}

@scala.annotation.nowarn
class ScalaBean {
  def getFoo: Int = 1
  def isBar: Boolean = true
  def isBaz: Int = 1 // `is` prefix but not a Boolean
  def getUnit: Unit = ()
  def getWithArg(a: Int): Int = a
  def setFoo(v: Int): Unit = ()
  def setReturning(v: Int): Int = v // `set` prefix but not returning Unit
  def setTwo(a: Int, b: Int): Unit = ()
}

class MultiCtor(val a: Int, val b: String) {
  def this(a: Int) = this(a, "")
  def this() = this(0)
}

@scala.annotation.nowarn
class PrivateCtor private (val a: Int)

trait NoCtorTrait {
  def foo: Int
}

class GenDefaults[A](val a: Int, val xs: List[A] = Nil) {
  override def toString(): String = s"GenDefaults($a, $xs)"
}

class GenMethodDefaults[A](val a: A) {
  def withDefault(x: Int, y: A = a): String = s"$x:$y"
}

class AbstractPathDep {
  trait Box { type T }
  @scala.annotation.nowarn
  def unbox(b: Box)(t: b.T): Int = 1
}

class Registry
object Registry {
  def count: Int = 3
  def make(a: Int): Int = a * 2
}

class GenParent[A] {
  def get(a: A): A = a
}
class IntChild extends GenParent[Int]

object Aliases {
  type ModifiersAlias = ModifiersImpl
  type GenParentOfString = GenParent[String]
}

class Ordered(val first: Int, val second: Int) {
  def zeta: Int = 1
  def alpha: Int = 2
  def mid: Int = 3
}
