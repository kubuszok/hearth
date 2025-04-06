package hearth
package typed

class TypesSpec extends MacroSuite {

  group("typed.Types") {

    group("Type.simple, Type.fcqn, Type.plainPrint, Type.prettyPrint return consistent cross-platform results") {

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

      test("for classes (non-sealed)") {
        TypesFixtures.testNamesPrinters[examples.classes.ExampleTrait].stripANSI ==>
          s"""Short:  ExampleTrait
             |FCQN:   hearth.examples.classes.ExampleTrait
             |Plain:  hearth.examples.classes.ExampleTrait
             |Pretty: hearth.examples.classes.ExampleTrait""".stripMargin
        TypesFixtures.testNamesPrinters[examples.classes.ExampleTraitWithTypeParam[Int]].stripANSI ==>
          s"""Short:  ExampleTraitWithTypeParam
             |FCQN:   hearth.examples.classes.ExampleTraitWithTypeParam
             |Plain:  hearth.examples.classes.ExampleTraitWithTypeParam[scala.Int]
             |Pretty: hearth.examples.classes.ExampleTraitWithTypeParam[scala.Int]""".stripMargin
        TypesFixtures.testNamesPrinters[examples.classes.ExampleAbstractClass].stripANSI ==>
          s"""Short:  ExampleAbstractClass
             |FCQN:   hearth.examples.classes.ExampleAbstractClass
             |Plain:  hearth.examples.classes.ExampleAbstractClass
             |Pretty: hearth.examples.classes.ExampleAbstractClass""".stripMargin
        TypesFixtures.testNamesPrinters[examples.classes.ExampleAbstractClassWithTypeParam[Int]].stripANSI ==>
          s"""Short:  ExampleAbstractClassWithTypeParam
             |FCQN:   hearth.examples.classes.ExampleAbstractClassWithTypeParam
             |Plain:  hearth.examples.classes.ExampleAbstractClassWithTypeParam[scala.Int]
             |Pretty: hearth.examples.classes.ExampleAbstractClassWithTypeParam[scala.Int]""".stripMargin
        TypesFixtures.testNamesPrinters[examples.classes.ExampleCaseClass].stripANSI ==>
          s"""Short:  ExampleCaseClass
             |FCQN:   hearth.examples.classes.ExampleCaseClass
             |Plain:  hearth.examples.classes.ExampleCaseClass
             |Pretty: hearth.examples.classes.ExampleCaseClass""".stripMargin
        TypesFixtures.testNamesPrinters[examples.classes.ExampleCaseClassWithTypeParam[Int]].stripANSI ==>
          s"""Short:  ExampleCaseClassWithTypeParam
             |FCQN:   hearth.examples.classes.ExampleCaseClassWithTypeParam
             |Plain:  hearth.examples.classes.ExampleCaseClassWithTypeParam[scala.Int]
             |Pretty: hearth.examples.classes.ExampleCaseClassWithTypeParam[scala.Int]""".stripMargin
      }

      test("for enumerations") {
        TypesFixtures.testNamesPrinters[examples.enums.ExampleSealedTrait].stripANSI ==>
          s"""Short:  ExampleSealedTrait
             |FCQN:   hearth.examples.enums.ExampleSealedTrait
             |Plain:  hearth.examples.enums.ExampleSealedTrait
             |Pretty: hearth.examples.enums.ExampleSealedTrait""".stripMargin
        TypesFixtures.testNamesPrinters[examples.enums.ExampleSealedTraitWithTypeParam[Int]].stripANSI ==>
          s"""Short:  ExampleSealedTraitWithTypeParam
             |FCQN:   hearth.examples.enums.ExampleSealedTraitWithTypeParam
             |Plain:  hearth.examples.enums.ExampleSealedTraitWithTypeParam[scala.Int]
             |Pretty: hearth.examples.enums.ExampleSealedTraitWithTypeParam[scala.Int]""".stripMargin
        TypesFixtures.testNamesPrinters[examples.enums.ExampleSealedTraiGADT[Unit]].stripANSI ==>
          s"""Short:  ExampleSealedTraiGADT
             |FCQN:   hearth.examples.enums.ExampleSealedTraiGADT
             |Plain:  hearth.examples.enums.ExampleSealedTraiGADT[scala.Unit]
             |Pretty: hearth.examples.enums.ExampleSealedTraiGADT[scala.Unit]""".stripMargin
      }
    }
  }
}
