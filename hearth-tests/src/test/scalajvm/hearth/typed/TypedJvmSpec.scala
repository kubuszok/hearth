package hearth
package typed

class TypedJvmSpec extends MacroSuite {

  group("typed.Types") {

    test("for classes (non-sealed)") {
      TypesFixtures.testNamesPrinters[examples.classes.ExampleJavaInterface].stripANSI ==>
        """Short:  ExampleJavaInterface
          |FCQN:   hearth.examples.classes.ExampleJavaInterface
          |Plain:  hearth.examples.classes.ExampleJavaInterface
          |Pretty: hearth.examples.classes.ExampleJavaInterface""".stripMargin
      TypesFixtures.testNamesPrinters[examples.classes.ExampleJavaClass].stripANSI ==>
        """Short:  ExampleJavaClass
          |FCQN:   hearth.examples.classes.ExampleJavaClass
          |Plain:  hearth.examples.classes.ExampleJavaClass
          |Pretty: hearth.examples.classes.ExampleJavaClass""".stripMargin
    }

    test("for enumerations") {
      TypesFixtures.testNamesPrinters[examples.enums.ExampleJavaEnum].stripANSI ==>
        """Short:  ExampleJavaEnum
          |FCQN:   hearth.examples.enums.ExampleJavaEnum
          |Plain:  hearth.examples.enums.ExampleJavaEnum
          |Pretty: hearth.examples.enums.ExampleJavaEnum""".stripMargin
      TypesFixtures.testNamesPrinters[examples.enums.ExampleJavaEnumWithMethods].stripANSI ==>
        """Short:  ExampleJavaEnumWithMethods
          |FCQN:   hearth.examples.enums.ExampleJavaEnumWithMethods
          |Plain:  hearth.examples.enums.ExampleJavaEnumWithMethods
          |Pretty: hearth.examples.enums.ExampleJavaEnumWithMethods""".stripMargin
    }
  }
}
