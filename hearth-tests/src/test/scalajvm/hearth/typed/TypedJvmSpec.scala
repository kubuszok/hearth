package hearth
package typed

class TypedJvmSpec extends MacroSuite {

  group("typed.Types") {

    group("methods: Type.{simple, fcqn, plainPrint, prettyPrint}, expected behavior") {
      import TypesFixtures.testNamesPrinters

      test("for top-level classes (non-sealed)") {
        List(
          testNamesPrinters[examples.classes.ExampleJavaInterface] -> "ExampleJavaInterface",
          testNamesPrinters[examples.classes.ExampleJavaClass] -> "ExampleJavaClass"
        ).foreach { case (actual, expected) =>
          actual <==>
            s"""Type.shortName:   $expected
               |Type.fcqn:        hearth.examples.classes.$expected
               |Type.plainPrint:  hearth.examples.classes.$expected
               |Type.prettyPrint: hearth.examples.classes.$expected""".stripMargin
        }
      }

      test("for enumerations".ignore.pending("We need to fix S2-S3 mismatch")) {
        List(
          testNamesPrinters[examples.enums.ExampleJavaEnum] -> "ExampleJavaEnum",
          testNamesPrinters[examples.enums.ExampleJavaEnumWithMethods] -> "ExampleJavaEnumWithMethods"
        ).foreach { case (actual, expected) =>
          actual <==>
            s"""Type.shortName:   $expected
               |Type.fcqn:        hearth.examples.enums.$expected
               |Type.plainPrint:  hearth.examples.enums.$expected
               |Type.prettyPrint: hearth.examples.enums.$expected""".stripMargin
        }
      }
    }
  }
}
