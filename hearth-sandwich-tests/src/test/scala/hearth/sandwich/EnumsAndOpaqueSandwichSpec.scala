package hearth
package sandwich

import hearth.data.Data
import hearth.sandwich.examples213.Examples213
import hearth.sandwich.examples3.Examples3
import hearth.typed.TypesFixtures

/** Sandwich tests for enums, sealed traits and opaque types. */
final class EnumsAndOpaqueSandwichSpec extends MacroSuite {

  private def childrenKeys(data: Data): Set[String] = {
    val map = data.asMap.get
    val direct = map.apply("Type.directChildren")
    direct.asMap.fold(Set.empty[String])(_.keySet)
  }

  group("typed.Types – enums and sealed traits across Scala versions") {

    test("Scala 2 macros see Scala 3 enum children".ignoreOnScala3) {
      val children = TypesFixtures.testChildren[Examples3.Color3]
      val keys = childrenKeys(children)
      assert(keys.nonEmpty)
    }

    test("Scala 3 macros see Scala 2 sealed trait children".ignoreOnScala2_13) {
      val children = TypesFixtures.testChildren[Examples213.Color213]
      val keys = childrenKeys(children)
      assert(keys.nonEmpty)
    }
  }

  group("typed.Types – opaque types visibility") {

    test("Scala 2 macros can at least obtain a short name for Scala 3 opaque type".ignoreOnScala3) {
      val names = TypesFixtures.testNamesPrinters[Examples3.OpaqueId]
      val shortName = names.asMap.get.apply("Type.shortName").asString.get
      assert(shortName.nonEmpty)
    }
  }
}
