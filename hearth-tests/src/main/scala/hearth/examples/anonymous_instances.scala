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

/** Generic method whose declared return is CONCRETE (`String`), independent of the method's type parameter. The
  * override body must see `OverrideContext.returnType = String` (not `Any`) — the regression Kindlings hit while
  * porting ScalaMock.
  */
trait TraitWithGenericConcreteReturn {
  def describe[T](x: T): String
}

/** Generic method with NO value parameter list, so the override body cannot recover the type parameter from any
  * argument — it must use `OverrideContext.typeParameters` to name `T` and `returnType` (`List[T]`) to build a result.
  */
trait TraitWithGenericFactory {
  def emptyList[T]: List[T]
}

/** Symbolic / operator method name — the override member must be emitted under the operator name `+`. */
trait TraitWithSymbolicMethod {
  def +(x: Int): Int
}

/** Method with a trailing implicit parameter clause — the override must keep the `implicit` modifier on the second
  * clause, otherwise it stays abstract (signature mismatch) and the instance cannot be constructed.
  */
trait TraitWithImplicitParam {
  def run(cmd: String)(implicit cfg: Int): String
}

/** Abstract `val` member — overriding it with a `def` is rejected ("stable, immutable value required"), so the override
  * must be emitted as a `val`.
  */
trait TraitWithAbstractVal {
  val abstractVal: String
}

/** `this.type` return — the override's declared result must be the synthesized subtype's `this.type`, otherwise
  * returning `this` does not conform (the member type's `this.type` points at the parent trait). `sibling` is an
  * ordinary method (`returnsThisType = false`), so an override body must tell the two apart via
  * `OverrideContext.returnsThisType` — returning `self` for `chain` but a real value for `sibling`. A wrong flag in
  * either direction is a compile error (self is not an `Int`; `7` is not a `this.type`).
  */
trait TraitWithThisTypeReturn {
  def chain: this.type
  def sibling: Int
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
