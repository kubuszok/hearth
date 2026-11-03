package hearth
package examples
package classesedge

// --- Class ---

class ClassWithOverloads {
  def foo(i: Int): Int = i
  def foo(s: String): String = s
  def bar: Int = 1
}

// --- CaseClass ---

case class CCEmpty()

// Declared in an order that differs from the alphabetical one.
case class CCDeclarationOrder(zeta: Int, alpha: String, mid: Boolean)

case class CCGeneric2[A, B](a: A, b: B)
object CCGeneric2 {
  type Flipped[B, A] = CCGeneric2[A, B]
  type IntFirst[B] = CCGeneric2[Int, B]
}

object CCAliases {
  type Plain = CCDeclarationOrder
}

case class CCMultipleParamLists(i: Int)(val s: String) {
  override def toString(): String = s"CCMultipleParamLists($i)($s)"
}

case class CCImplicitParam(i: Int)(implicit val s: String) {
  override def toString(): String = s"CCImplicitParam($i)($s)"
}

case class CCWithAuxiliaryCtor(i: Int, s: String) {
  def this(i: Int) = this(i, "aux")
}

case class CCPrivateCtor private (i: Int)

case class CCHearthPrivateCtor private[hearth] (i: Int)

case class CCValueClass(value: Int) extends AnyVal

object CCOuter {
  case class CCNested(i: Int)
}

class CCContainer {
  case class CCInner(i: Int, s: String)
}
object CCContainerInstance extends CCContainer

// --- Enum ---

sealed abstract class SealedAbstractShape
object SealedAbstractShape {
  final case class Circle(r: Int) extends SealedAbstractShape
  case object Dot extends SealedAbstractShape
}

sealed trait Animal
object Animal {
  sealed trait Mammal extends Animal
  final case class Dog(name: String) extends Mammal
  case object Cat extends Mammal
  final case class Fish(name: String) extends Animal
}

sealed trait SealedWithoutChildren

// --- SingletonValue ---

object PlainObject {
  override def toString(): String = "PlainObject"
}

// --- JavaBean ---

class ScalaBean {
  private var name: String = ""
  private var count: Int = 0
  private var active: Boolean = false

  def getName(): String = name
  def setName(name: String): Unit = this.name = name
  def getCount(): Int = count
  def setCount(count: Int): Unit = this.count = count
  def isActive(): Boolean = active
  def setActive(active: Boolean): Unit = this.active = active

  override def toString(): String = s"ScalaBean($name, $count, $active)"
}

class ScalaBeanHearthPrivateCtor private[hearth] () {
  override def toString(): String = "ScalaBeanHearthPrivateCtor"
}

// --- AnonymousInstance ---

trait AITraitWithProtected {
  def publicMethod: Int
  protected def protectedMethod: Int
  def callProtected: Int = protectedMethod
}

trait AITraitWithByName {
  def lazily(x: => Int): Int
}

trait AITraitWithVar {
  var abstractVar: String
}
