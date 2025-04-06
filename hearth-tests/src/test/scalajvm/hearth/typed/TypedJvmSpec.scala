package hearth
package typed

class TypedJvmSpec extends MacroSuite {

  group("typed.Types") {
    test("for enumerations") {
      TypesFixtures.testNamesPrinters[examples.enums.ExampleJavaEnum].stripANSI ==>
        s"""Short:  ExampleJavaEnum
           |FCQN:   hearth.examples.enums.ExampleJavaEnum
           |Plain:  hearth.examples.enums.ExampleJavaEnum
           |Pretty: hearth.examples.enums.ExampleJavaEnum""".stripMargin
      TypesFixtures.testNamesPrinters[examples.enums.ExampleJavaEnumWithMethods].stripANSI ==>
        s"""Short:  ExampleJavaEnumWithMethods
           |FCQN:   hearth.examples.enums.ExampleJavaEnumWithMethods
           |Plain:  hearth.examples.enums.ExampleJavaEnumWithMethods
           |Pretty: hearth.examples.enums.ExampleJavaEnumWithMethods""".stripMargin
    }
  }
}
