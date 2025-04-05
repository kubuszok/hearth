package hearth
package typed

class TypesSpec extends MacroSuite {

  test("Type names printers return consistent results cross-platform") {

    TypesFixtures.testNamesPrinters[String].stripANSI ==>
      s"""Short:  String
         |FCQN:   java.lang.String
         |Plain:  java.lang.String
         |Pretty: java.lang.String""".stripMargin
  }
}
