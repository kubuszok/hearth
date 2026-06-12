package hearth
package examples
package methods

class ExampleAnnotation extends scala.annotation.StaticAnnotation
class ExampleAnnotation2(val value: Int) extends scala.annotation.StaticAnnotation

class ParentAnnotation extends scala.annotation.StaticAnnotation
class ChildAnnotation extends ParentAnnotation

@ChildAnnotation
class WithChildAnnotation {

  @ChildAnnotation
  def annotatedMethod(@ChildAnnotation arg: Int): Int = arg

  @ExampleAnnotation2(42)
  @ParentAnnotation
  def annotatedMethod2(arg: Int): Int = arg
}

@ExampleAnnotation
trait Trait {

  @ExampleAnnotation2(2)
  def inheritedAbstractMethod(arg: Int): Int

  final def inheritedFinalMethod(arg: Int): Int = arg + 1
}

@ExampleAnnotation2(1)
class NoCompanionClass extends Trait {

  def method(arg: Int): Int = arg + 1

  def methodWithDefault(arg: Int = 1): Int = arg + 1

  @ExampleAnnotation
  def methodWithAnnotation(arg: Int): Int = arg + 1

  def methodWithAnnotatedParam(@ExampleAnnotation arg: Int): Int = arg + 1

  val scalaValue: Int = 1

  var scalaVariable: Int = 1

  lazy val scalaLazyValue: Int = 1

  override def inheritedAbstractMethod(arg: Int): Int = arg + 1
}

case class WithAnnotatedParams(
    @ExampleAnnotation a: Int,
    @ExampleAnnotation2(1) b: String,
    c: Double
)

final class WithCompanion(arg: Int) {

  def method(arg2: Int): Int = arg + arg2
}
object WithCompanion {

  def apply(arg: Int): WithCompanion = new WithCompanion(arg)

  def call(arg: Int, arg2: Int): Int = apply(arg).method(arg2)
}

class WithConstructorDefaults(val a: Int, val b: Int = 10) {
  override def toString(): String = s"WithConstructorDefaults($a, $b)"
}

case class CaseWithDefaults(a: Int, b: Int = 20) {
  def methodWithDefault(c: Int = 30): Int = a + b + c
}

final class WithCompanionDefaults(val a: Int, val b: Int) {
  override def toString(): String = s"WithCompanionDefaults($a, $b)"
}
object WithCompanionDefaults {
  def apply(a: Int, b: Int = 15): WithCompanionDefaults = new WithCompanionDefaults(a, b)
}

@scala.annotation.nowarn
abstract class ScopeVisibility(privateCtorArg: Int, val publicCtorArg: Int) {

  def publicMethod: Int = 1

  private def privateMethod: Int = 1

  protected def protectedMethod: Int = 1

  private def privateThisMethod: Int = 1

  private[hearth] def privateHearthMethod: Int = 1

  private[examples] def privateHearthExamplesMethod: Int = 1
}

@scala.annotation.nowarn
class WithImplicits {
  implicit val implicitValue: Int = 42
  implicit def implicitConversion(x: Int): String = x.toString
  def methodWithImplicitParam(implicit x: Int): String = x.toString
}

// Regression examples for commit 68e1781: setters (name_=) must not be marked as
// constructor arguments nor case fields, and visibility declared on a var must
// propagate from the getter/val to the synthesized setter.

// Reproducer for issue #283: a plain (non-case) `final` annotation class with `val` constructor
// parameters, attached to a case class constructor parameter; literal args must be readable
// cross-platform (String, Boolean, Double — not just Int).

final class fieldName(val name: String) extends scala.annotation.StaticAnnotation
final class fieldFlags(val enabled: Boolean, val weight: Double) extends scala.annotation.StaticAnnotation

final case class Person(
    @fieldName("first_name") firstName: String,
    @fieldName("age") @fieldFlags(true, 1.5) age: Int
)

final class WithVarCtorParam(var name: String)

case class CaseClassWithVarCtorParam(var name: String)

class WithRestrictedVar {
  private[examples] var counter: Int = 0
}
