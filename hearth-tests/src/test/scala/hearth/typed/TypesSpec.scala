package hearth
package typed

class TypesSpec extends MacroSuite {

  group("Type names printers return consistent results cross-platform") {

    test("for primitive types") {
      TypesFixtures.testNamesPrinters[Boolean].stripANSI ==>
        s"""Short:  Boolean
           |FCQN:   scala.Boolean
           |Plain:  scala.Boolean
           |Pretty: scala.Boolean""".stripMargin
      TypesFixtures.testNamesPrinters[Byte].stripANSI ==>
        s"""Short:  Byte
           |FCQN:   scala.Byte
           |Plain:  scala.Byte
           |Pretty: scala.Byte""".stripMargin
      TypesFixtures.testNamesPrinters[Short].stripANSI ==>
        s"""Short:  Short
           |FCQN:   scala.Short
           |Plain:  scala.Short
           |Pretty: scala.Short""".stripMargin
      TypesFixtures.testNamesPrinters[Int].stripANSI ==>
        s"""Short:  Int
           |FCQN:   scala.Int
           |Plain:  scala.Int
           |Pretty: scala.Int""".stripMargin
      TypesFixtures.testNamesPrinters[Long].stripANSI ==>
        s"""Short:  Long
           |FCQN:   scala.Long
           |Plain:  scala.Long
           |Pretty: scala.Long""".stripMargin
      TypesFixtures.testNamesPrinters[Float].stripANSI ==>
        s"""Short:  Float
           |FCQN:   scala.Float
           |Plain:  scala.Float
           |Pretty: scala.Float""".stripMargin
      TypesFixtures.testNamesPrinters[Double].stripANSI ==>
        s"""Short:  Double
           |FCQN:   scala.Double
           |Plain:  scala.Double
           |Pretty: scala.Double""".stripMargin
      TypesFixtures.testNamesPrinters[Char].stripANSI ==>
        s"""Short:  Char
           |FCQN:   scala.Char
           |Plain:  scala.Char
           |Pretty: scala.Char""".stripMargin
      TypesFixtures.testNamesPrinters[Unit].stripANSI ==>
        s"""Short:  Unit
           |FCQN:   scala.Unit
           |Plain:  scala.Unit
           |Pretty: scala.Unit""".stripMargin
    }

    test("for build-in types") {
      TypesFixtures.testNamesPrinters[String].stripANSI ==>
        s"""Short:  String
           |FCQN:   java.lang.String
           |Plain:  java.lang.String
           |Pretty: java.lang.String""".stripMargin
      TypesFixtures.testNamesPrinters[Array[Int]].stripANSI ==>
        s"""Short:  Array
           |FCQN:   scala.Array
           |Plain:  scala.Array[scala.Int]
           |Pretty: scala.Array[scala.Int]""".stripMargin
    }
  }
}
