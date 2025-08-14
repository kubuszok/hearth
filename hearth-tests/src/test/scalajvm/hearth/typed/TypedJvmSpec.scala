package hearth
package typed

import hearth.testdata.Data

/** Macro implementation is in [[TypesFixturesImpl]] */
final class TypedJvmSpec extends MacroSuite {

  group("typed.Types") {

    group("methods: Type.{simple, fcqn, plainPrint, prettyPrint}, expected behavior") {
      import TypesFixtures.testNamesPrinters

      test("for top-level classes (non-sealed)") {
        List(
          testNamesPrinters[examples.classes.ExampleJavaInterface] -> "ExampleJavaInterface",
          testNamesPrinters[examples.classes.ExampleJavaClass] -> "ExampleJavaClass"
        ).foreach { case (actual, expected) =>
          actual <==> Data(
            Map(
              "Type.shortName" -> Data(expected),
              "Type.fcqn" -> Data(s"hearth.examples.classes.$expected"),
              "Type.plainPrint" -> Data(s"hearth.examples.classes.$expected"),
              "Type.prettyPrint" -> Data(s"hearth.examples.classes.$expected")
            )
          )
        }
      }

      test("for enumerations".ignore.pending("We need to fix S2-S3 mismatch")) {
        List(
          testNamesPrinters[examples.enums.ExampleJavaEnum] -> "ExampleJavaEnum",
          testNamesPrinters[examples.enums.ExampleJavaEnumWithMethods] -> "ExampleJavaEnumWithMethods"
        ).foreach { case (actual, expected) =>
          actual <==> Data(
            Map(
              "Type.shortName" -> Data(expected),
              "Type.fcqn" -> Data(s"hearth.examples.enums.$expected"),
              "Type.plainPrint" -> Data(s"hearth.examples.enums.$expected"),
              "Type.prettyPrint" -> Data(s"hearth.examples.enums.$expected")
            )
          )
        }
      }
    }
  }
}
