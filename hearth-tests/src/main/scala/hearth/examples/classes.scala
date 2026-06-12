package hearth
package examples
package classes

trait ExampleTrait
trait ExampleTraitWithTypeParam[A]

abstract class ExampleAbstractClass
abstract class ExampleAbstractClassWithTypeParam[A]

final class ExampleClass
final class ExampleClassWithTypeParam[A]

case class ExampleCaseClass(a: Int)
case class ExampleCaseClassWithTypeParam[A](a: A)
case class ExampleCaseClassWithDefaults(a: Int, b: String = "default-b")

// Regression example for commit 68e1781: caseFieldValuesAt(instance, visibility) filters fields
// by accessibility. `b` is accessible from within the `hearth` package (so AtCallSite keeps it when
// expanding inside hearth), but it is not accessible "everywhere" (so Everywhere skips it).
case class ExampleCaseClassWithPrivateField(a: Int, private[hearth] val b: String)
