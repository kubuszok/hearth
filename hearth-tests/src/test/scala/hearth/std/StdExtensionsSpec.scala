package hearth
package std

import hearth.data.Data

/** Macro implementation is in [[StdExtensionsFixturesImpl]] */
final class StdExtensionsSpec extends MacroSuite {

  group("trait std.StdExtensions") {

    group("class: IsCollection[A], returns preprocessed collection") {
      import StdExtensionsFixtures.testIsCollection

      test("for Scala collection") {
        // TODO
        testIsCollection(Map(1 -> "one", 2 -> "two")) <==> Data.map(
          "iteration" -> Data.list(
            Data.map(
              "key" -> Data("1"),
              "value" -> Data("one")
            ),
            Data.map(
              "key" -> Data("2"),
              "value" -> Data("two")
            )
          ),
          "building" -> Data("Map(1 -> one)")
        )
      }

      test("for Arrays") {
        // TODO
        // testIsCollection(Array(1, 2, 3)) <==> Data.map("isCollection" -> Data(true))
      }
    }

    // TODO: IsOption
    // TODO: IsEither
    // TODO: IsValueType
  }
}
