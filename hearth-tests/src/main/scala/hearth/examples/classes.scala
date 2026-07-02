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
case class ExampleCaseClassWithVarargs(xs: Int*)
case class ExampleCaseClassWithTypeParam[A](a: A)
case class ExampleCaseClassWithDefaults(a: Int, b: String = "default-b")

// Examples for CaseClass.copyMethod (issue #246).

// Multiple parameter lists: the canonical `copy` mirrors them, i.e. `def copy(i: Int = i)(s: String = s)`.
case class ExampleCaseClassMultipleParamLists(i: Int)(val s: String)

// A private primary constructor makes the synthesized `copy` non-public, so it is dropped by AtCallSite/Everywhere
// but kept by Anywhere.
case class ExampleCaseClassPrivateCopy private (a: Int)

// An abstract case class has no synthesized `copy` at all, so `copyMethod` is None under every visibility.
sealed abstract case class ExampleSealedAbstractCaseClass(a: Int)

// Example for caseFieldValuesAt(instance, visibility) field filtering by accessibility.
// `b` is `private[hearth]`: accessible from within the `hearth` package, so it is kept by the
// default (AtCallSite) and by AtCallSite when expanding inside hearth, but it is not public, so
// explicit Everywhere skips it. Anywhere keeps it regardless. This class CANNOT distinguish
// AtCallSite from Anywhere at an in-hearth call site (both keep `b`) - use the class-private
// example below for that.
case class ExampleCaseClassWithPrivateField(a: Int, private[hearth] val b: String)

// Example that DISTINGUISHES all three visibility scopes. `b` is class-private (`private val`),
// so it is inaccessible even at an in-hearth call site:
//   - AtCallSite (the default) and Everywhere DROP `b` -> (a)
//   - Anywhere KEEPS `b` -> (a, b) (enumeration use case)
case class ExampleCaseClassWithClassPrivateField(a: Int, private val b: String)
