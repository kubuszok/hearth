package hearth
package examples
package methods

class ExampleAnnotation extends scala.annotation.StaticAnnotation
class ExampleAnnotation2(val value: Int) extends scala.annotation.StaticAnnotation

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

  // TODO: lazy val

  override def inheritedAbstractMethod(arg: Int): Int = arg + 1
}
