package hearth
package std

import hearth.data.Data

/** Macro implementation is in [[StdExtensionsFixturesImpl]] */
final class StdExtensionsJvmSpec extends MacroSuite {

  group("trait std.StdExtensions") {

    group("class: IsCollection[A], returns preprocessed collection") {
      import StdExtensionsFixtures.testIsCollection

      test("for Scala WeakHashMap") {
        testIsCollection(scala.collection.mutable.WeakHashMap(1 -> "one", 2 -> "two")) <==> Data.map(
          "iteration" -> Data.list(
            Data.map("key" -> Data("2"), "value" -> Data("two")),
            Data.map("key" -> Data("1"), "value" -> Data("one"))
          ),
          "building" -> Data("WeakHashMap(1 -> one)")
        )
      }
    }
  }
}
