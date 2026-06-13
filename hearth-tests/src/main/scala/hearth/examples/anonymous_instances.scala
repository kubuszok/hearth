package hearth
package examples
package anonymous_instances

trait SimpleTrait {
  def value: Int
}

trait TraitWithConcreteMethod {
  def abstractMethod: String
  def concreteMethod: Int = 42
}

trait TraitWithFinalMethod {
  final def finalMethod: Int = 42
  def abstractMethod: String
}

trait TraitWithTypeParam[A] {
  def value: A
}

abstract class AbstractClassNoArgs {
  def abstractMethod: Int
  def concreteMethod: String = "concrete"
}

abstract class AbstractClassWithArgs(val x: Int, val y: String) {
  def abstractMethod: Int
}

abstract class AbstractClassMultipleCtors(val x: Int) {
  @scala.annotation.nowarn
  def this(x: Int, y: String) = this(x)
  def abstractMethod: String
}

final class FinalClass {
  def method: Int = 42
}

sealed trait SealedTrait {
  def method: Int
}

trait DiamondBase {
  def method: Int
}

trait DiamondLeft extends DiamondBase {
  override def method: Int = 1
}

trait DiamondRight extends DiamondBase {
  override def method: Int = 2
}

trait MixinA {
  def fromA: String
}

trait MixinB {
  def fromB: Int
}

abstract class ClassWithMixins extends MixinA {
  def abstractMethod: Int
}

trait TraitWithOverloads {
  def overloaded(a: Int): String
  def overloaded(a: String): Int
  def overloaded(a: Int, b: Int): Int
}

trait TraitWithGenericMethod {
  def identity[T](t: T): T
}

trait TraitWithBoundedGenericMethod {
  def firstOf[T <: AnyRef](xs: List[T]): T
}

@scala.annotation.nowarn
trait TraitWithVisibility {
  def publicMethod: Int
  protected def protectedMethod: Int
  private[anonymous_instances] def packagePrivateMethod: Int = 0
}

trait TraitWithValAndVar {
  val abstractVal: Int
  var abstractVar: String
  def abstractDef: Boolean
}

trait GenericParent[A] {
  def value: A
  def transform(a: A): A
}

abstract class PrivateConstructor private (val x: Int) {
  def abstractMethod: String
}

abstract class PackagePrivateConstructor private[anonymous_instances] (val x: Int) {
  def abstractMethod: String
}

abstract class AbstractClassWithDefaults(val x: Int, val y: String = "default") {
  def abstractMethod: Int
}

trait TraitWithVarargMethod {
  def sum(xs: Int*): Int
}

/** Only visible inside `hearth.examples.anonymous_instances` — test fixtures resolve it via `UntypedType.fromClassName`
  * to exercise the `TypeInaccessible` error (the call site cannot even name it).
  */
private[anonymous_instances] trait PackagePrivateTrait {
  def value: Int
}
