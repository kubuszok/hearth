package hearth.demo.allfeatures

import hearth.MacroSuite

final class FastShowPrettyNamedTupleSpec extends MacroSuite {

  group("FastShowPretty") {

    group("named tuples (Scala 3.7+)") {

      test("simple named tuple") {
        val nt: (name: String, age: Int) = ("Alice", 42)
        val result = FastShowPretty.render(nt, RenderConfig.Default)
        assertEquals(
          result,
          """(
            |  name = "Alice",
            |  age = 42
            |)""".stripMargin
        )
      }

      test("compact (no indent)") {
        val nt: (name: String, age: Int) = ("Alice", 42)
        val result = FastShowPretty.render(nt, RenderConfig.Compact)
        assertEquals(
          result,
          """(
            |name = "Alice",
            |age = 42
            |)""".stripMargin
        )
      }

      test("nested named tuple with case class") {
        val nt: (person: Person, score: Int) = (Person("Bob", 25), 100)
        val result = FastShowPretty.render(nt, RenderConfig.Default)
        assertEquals(
          result,
          """(
            |  person = Person(
            |    name = "Bob",
            |    age = 25
            |  ),
            |  score = 100
            |)""".stripMargin
        )
      }
    }
  }
}
