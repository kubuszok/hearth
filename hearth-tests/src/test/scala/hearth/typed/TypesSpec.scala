package hearth
package typed

import scala.annotation.unused

class TypesSpec extends MacroSuite {

  group("typed.Types") {

    group("Type.simple, Type.fcqn, Type.plainPrint, Type.prettyPrint return consistent cross-platform results") {

      test("for primitive types") {
        TypesFixtures.testNamesPrinters[Boolean].stripANSI ==>
          """Short:  Boolean
            |FCQN:   scala.Boolean
            |Plain:  scala.Boolean
            |Pretty: scala.Boolean""".stripMargin
        TypesFixtures.testNamesPrinters[Byte].stripANSI ==>
          """Short:  Byte
            |FCQN:   scala.Byte
            |Plain:  scala.Byte
            |Pretty: scala.Byte""".stripMargin
        TypesFixtures.testNamesPrinters[Short].stripANSI ==>
          """Short:  Short
            |FCQN:   scala.Short
            |Plain:  scala.Short
            |Pretty: scala.Short""".stripMargin
        TypesFixtures.testNamesPrinters[Int].stripANSI ==>
          """Short:  Int
            |FCQN:   scala.Int
            |Plain:  scala.Int
            |Pretty: scala.Int""".stripMargin
        TypesFixtures.testNamesPrinters[Long].stripANSI ==>
          """Short:  Long
            |FCQN:   scala.Long
            |Plain:  scala.Long
            |Pretty: scala.Long""".stripMargin
        TypesFixtures.testNamesPrinters[Float].stripANSI ==>
          """Short:  Float
            |FCQN:   scala.Float
            |Plain:  scala.Float
            |Pretty: scala.Float""".stripMargin
        TypesFixtures.testNamesPrinters[Double].stripANSI ==>
          """Short:  Double
            |FCQN:   scala.Double
            |Plain:  scala.Double
            |Pretty: scala.Double""".stripMargin
        TypesFixtures.testNamesPrinters[Char].stripANSI ==>
          """Short:  Char
            |FCQN:   scala.Char
            |Plain:  scala.Char
            |Pretty: scala.Char""".stripMargin
        TypesFixtures.testNamesPrinters[Unit].stripANSI ==>
          """Short:  Unit
            |FCQN:   scala.Unit
            |Plain:  scala.Unit
            |Pretty: scala.Unit""".stripMargin
      }

      test("for build-in types") {
        TypesFixtures.testNamesPrinters[String].stripANSI ==>
          """Short:  String
            |FCQN:   java.lang.String
            |Plain:  java.lang.String
            |Pretty: java.lang.String""".stripMargin
        TypesFixtures.testNamesPrinters[Array[Int]].stripANSI ==>
          """Short:  Array
            |FCQN:   scala.Array
            |Plain:  scala.Array[scala.Int]
            |Pretty: scala.Array[scala.Int]""".stripMargin
      }

      test("for classes (non-sealed)") {
        TypesFixtures.testNamesPrinters[examples.classes.ExampleTrait].stripANSI ==>
          """Short:  ExampleTrait
            |FCQN:   hearth.examples.classes.ExampleTrait
            |Plain:  hearth.examples.classes.ExampleTrait
            |Pretty: hearth.examples.classes.ExampleTrait""".stripMargin
        TypesFixtures.testNamesPrinters[examples.classes.ExampleTraitWithTypeParam[Int]].stripANSI ==>
          """Short:  ExampleTraitWithTypeParam
            |FCQN:   hearth.examples.classes.ExampleTraitWithTypeParam
            |Plain:  hearth.examples.classes.ExampleTraitWithTypeParam[scala.Int]
            |Pretty: hearth.examples.classes.ExampleTraitWithTypeParam[scala.Int]""".stripMargin
        TypesFixtures.testNamesPrinters[examples.classes.ExampleAbstractClass].stripANSI ==>
          """Short:  ExampleAbstractClass
            |FCQN:   hearth.examples.classes.ExampleAbstractClass
            |Plain:  hearth.examples.classes.ExampleAbstractClass
            |Pretty: hearth.examples.classes.ExampleAbstractClass""".stripMargin
        TypesFixtures.testNamesPrinters[examples.classes.ExampleAbstractClassWithTypeParam[Int]].stripANSI ==>
          """Short:  ExampleAbstractClassWithTypeParam
            |FCQN:   hearth.examples.classes.ExampleAbstractClassWithTypeParam
            |Plain:  hearth.examples.classes.ExampleAbstractClassWithTypeParam[scala.Int]
            |Pretty: hearth.examples.classes.ExampleAbstractClassWithTypeParam[scala.Int]""".stripMargin
        TypesFixtures.testNamesPrinters[examples.classes.ExampleClass].stripANSI ==>
          """Short:  ExampleClass
            |FCQN:   hearth.examples.classes.ExampleClass
            |Plain:  hearth.examples.classes.ExampleClass
            |Pretty: hearth.examples.classes.ExampleClass""".stripMargin
        TypesFixtures.testNamesPrinters[examples.classes.ExampleClassWithTypeParam[Int]].stripANSI ==>
          """Short:  ExampleClassWithTypeParam
            |FCQN:   hearth.examples.classes.ExampleClassWithTypeParam
            |Plain:  hearth.examples.classes.ExampleClassWithTypeParam[scala.Int]
            |Pretty: hearth.examples.classes.ExampleClassWithTypeParam[scala.Int]""".stripMargin
        TypesFixtures.testNamesPrinters[examples.classes.ExampleCaseClass].stripANSI ==>
          """Short:  ExampleCaseClass
            |FCQN:   hearth.examples.classes.ExampleCaseClass
            |Plain:  hearth.examples.classes.ExampleCaseClass
            |Pretty: hearth.examples.classes.ExampleCaseClass""".stripMargin
        TypesFixtures.testNamesPrinters[examples.classes.ExampleCaseClassWithTypeParam[Int]].stripANSI ==>
          """Short:  ExampleCaseClassWithTypeParam
            |FCQN:   hearth.examples.classes.ExampleCaseClassWithTypeParam
            |Plain:  hearth.examples.classes.ExampleCaseClassWithTypeParam[scala.Int]
            |Pretty: hearth.examples.classes.ExampleCaseClassWithTypeParam[scala.Int]""".stripMargin
      }

      test("for inner classes (non-sealed)") {
        @unused
        trait ExampleTrait
        @unused
        trait ExampleTraitWithTypeParam[A]

        @unused
        abstract class ExampleAbstractClass
        @unused
        abstract class ExampleAbstractClassWithTypeParam[A]

        @unused
        final class ExampleClass
        @unused
        final class ExampleClassWithTypeParam[A]

        @unused
        case class ExampleCaseClass(a: Int)
        @unused
        case class ExampleCaseClassWithTypeParam[A](a: A)

        TypesFixtures.testNamesPrinters[ExampleTrait].stripANSI ==>
          s"""Short:  ExampleTrait
             |FCQN:   hearth.typed.TypesSpec.ExampleTrait
             |Plain:  hearth.typed.TypesSpec.ExampleTrait
             |Pretty: hearth.typed.TypesSpec.ExampleTrait""".stripMargin
        TypesFixtures.testNamesPrinters[ExampleTraitWithTypeParam[Int]].stripANSI ==>
          s"""Short:  ExampleTraitWithTypeParam
             |FCQN:   hearth.typed.TypesSpec.ExampleTraitWithTypeParam
             |Plain:  hearth.typed.TypesSpec.ExampleTraitWithTypeParam[scala.Int]
             |Pretty: hearth.typed.TypesSpec.ExampleTraitWithTypeParam[scala.Int]""".stripMargin
        TypesFixtures.testNamesPrinters[ExampleAbstractClass].stripANSI ==>
          s"""Short:  ExampleAbstractClass
             |FCQN:   hearth.typed.TypesSpec.ExampleAbstractClass
             |Plain:  hearth.typed.TypesSpec.ExampleAbstractClass
             |Pretty: hearth.typed.TypesSpec.ExampleAbstractClass""".stripMargin
        TypesFixtures.testNamesPrinters[ExampleAbstractClassWithTypeParam[Int]].stripANSI ==>
          s"""Short:  ExampleAbstractClassWithTypeParam
             |FCQN:   hearth.typed.TypesSpec.ExampleAbstractClassWithTypeParam
             |Plain:  hearth.typed.TypesSpec.ExampleAbstractClassWithTypeParam[scala.Int]
             |Pretty: hearth.typed.TypesSpec.ExampleAbstractClassWithTypeParam[scala.Int]""".stripMargin
        TypesFixtures.testNamesPrinters[ExampleClass].stripANSI ==>
          s"""Short:  ExampleClass
             |FCQN:   hearth.typed.TypesSpec.ExampleClass
             |Plain:  hearth.typed.TypesSpec.ExampleClass
             |Pretty: hearth.typed.TypesSpec.ExampleClass""".stripMargin
        TypesFixtures.testNamesPrinters[ExampleClassWithTypeParam[Int]].stripANSI ==>
          s"""Short:  ExampleClassWithTypeParam
             |FCQN:   hearth.typed.TypesSpec.ExampleClassWithTypeParam
             |Plain:  hearth.typed.TypesSpec.ExampleClassWithTypeParam[scala.Int]
             |Pretty: hearth.typed.TypesSpec.ExampleClassWithTypeParam[scala.Int]""".stripMargin
        TypesFixtures.testNamesPrinters[ExampleCaseClass].stripANSI ==>
          s"""Short:  ExampleCaseClass
             |FCQN:   hearth.typed.TypesSpec.ExampleCaseClass
             |Plain:  hearth.typed.TypesSpec.ExampleCaseClass
             |Pretty: hearth.typed.TypesSpec.ExampleCaseClass""".stripMargin
        TypesFixtures.testNamesPrinters[ExampleCaseClassWithTypeParam[Int]].stripANSI ==>
          s"""Short:  ExampleCaseClassWithTypeParam
             |FCQN:   hearth.typed.TypesSpec.ExampleCaseClassWithTypeParam
             |Plain:  hearth.typed.TypesSpec.ExampleCaseClassWithTypeParam[scala.Int]
             |Pretty: hearth.typed.TypesSpec.ExampleCaseClassWithTypeParam[scala.Int]""".stripMargin
      }

      test("for enumerations") {
        // TODO: fix WeekDay.Value on Scala 2
        TypesFixtures.testNamesPrinters[examples.enums.WeekDay.type].stripANSI ==>
          s"""Short:  WeekDay
             |FCQN:   hearth.examples.enums.WeekDay
             |Plain:  hearth.examples.enums.WeekDay
             |Pretty: hearth.examples.enums.WeekDay""".stripMargin
        // TODO: fix Planet.Value on Scala 2
        TypesFixtures.testNamesPrinters[examples.enums.Planet.type].stripANSI ==>
          s"""Short:  Planet
             |FCQN:   hearth.examples.enums.Planet
             |Plain:  hearth.examples.enums.Planet
             |Pretty: hearth.examples.enums.Planet""".stripMargin
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
