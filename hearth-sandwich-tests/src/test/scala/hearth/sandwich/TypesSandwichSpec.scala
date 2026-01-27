package hearth
package sandwich

import hearth.data.Data
import hearth.typed.TypesFixtures
import hearth.sandwich.examples213.Examples213
import hearth.sandwich.examples3.Examples3

/** Sandwich tests for typed.Types focusing on case classes, AnyVals and basic flags when crossing Scala 2.13 and Scala
  * 3 compilation.
  *
  * We rely on existing fixtures from hearth-tests via [[SandwichFixtures]] and only assert on a small, stable subset of
  * the produced [[Data]].
  */
final class TypesSandwichSpec extends MacroSuite {

  private def shortNameOf(data: Data): String =
    data.asMap.get.apply("Type.shortName").asString.get

  private def isCaseClassFlag(data: Data): Boolean =
    data.asMap.get.apply("Type.isCaseClass").asBoolean.get

  private def isValFlag(data: Data): Boolean =
    data.asMap.get.apply("Type.isVal").asBoolean.get

  group("typed.Types – sandwich case classes and AnyVals") {

    group("case classes across Scala versions") {

      test("Scala 2 macros see Scala 3 case class as case class".ignoreOnScala3) {
        val flags = TypesFixtures.testFlags[Examples3.CaseClassWithDefaults]
        assert(isCaseClassFlag(flags))
      }

      test("Scala 3 macros see Scala 2 case class as case class".ignoreOnScala2_13) {
        val flags = TypesFixtures.testFlags[Examples213.CaseClassWithDefaults]
        assert(isCaseClassFlag(flags))
      }
    }

    group("AnyVals across Scala versions") {

      test("Scala 2 macros does not see Scala 3 AnyVal as value-like".ignoreOnScala3) {
        val flags = TypesFixtures.testFlags[Examples3.AnyValId]
        assert(!isValFlag(flags))
      }

      test("Scala 3 macros  see Scala 2 AnyVal as value-like".ignoreOnScala2_13) {
        val flags = TypesFixtures.testFlags[Examples213.AnyValId]
        assert(!isValFlag(flags))
      }
    }

    group("names of mirrored case classes") {

      test("short names of 2.13 and 3 case classes stay consistent") {
        val from213 = shortNameOf(TypesFixtures.testNamesPrinters[Examples213.CaseClassWithDefaults])
        val from3 = shortNameOf(TypesFixtures.testNamesPrinters[Examples3.CaseClassWithDefaults])
        from213 ==> "CaseClassWithDefaults"
        from3 ==> "CaseClassWithDefaults"
      }
    }
  }
}
