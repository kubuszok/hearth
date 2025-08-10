package hearth
package typed

import hearth.testdata.Data

import scala.annotation.unused

/** Macro implementation is in [[TypesFixturesImpl]] */
class TypesSpec extends MacroSuite {

  group("trait typed.Types") {

    group("methods: Type.{simple, fcqn, plainPrint, prettyPrint}, expected behavior") {
      import TypesFixtures.testNamesPrinters

      test("for primitive types") {
        List(
          testNamesPrinters[Boolean] -> "Boolean",
          testNamesPrinters[Byte] -> "Byte",
          testNamesPrinters[Short] -> "Short",
          testNamesPrinters[Int] -> "Int",
          testNamesPrinters[Long] -> "Long",
          testNamesPrinters[Float] -> "Float",
          testNamesPrinters[Double] -> "Double",
          testNamesPrinters[Char] -> "Char",
          testNamesPrinters[Unit] -> "Unit"
        ).foreach { case (actual, expected) =>
          actual <==> Data(
            Map(
              "Type.shortName" -> Data(expected),
              "Type.fcqn" -> Data(s"scala.$expected"),
              "Type.plainPrint" -> Data(s"scala.$expected"),
              "Type.prettyPrint" -> Data(s"scala.$expected")
            )
          )
        }
      }

      test("for build-in types") {
        testNamesPrinters[String] <==> Data(
          Map(
            "Type.shortName" -> Data("String"),
            "Type.fcqn" -> Data("java.lang.String"),
            "Type.plainPrint" -> Data("java.lang.String"),
            "Type.prettyPrint" -> Data("java.lang.String")
          )
        )
        testNamesPrinters[Array[Int]] <==> Data(
          Map(
            "Type.shortName" -> Data("Array"),
            "Type.fcqn" -> Data("scala.Array"),
            "Type.plainPrint" -> Data("scala.Array[scala.Int]"),
            "Type.prettyPrint" -> Data("scala.Array[scala.Int]")
          )
        )
      }

      test("for top-level classes (non-sealed)") {
        List(
          testNamesPrinters[examples.classes.ExampleTrait] -> (("ExampleTrait", "")),
          testNamesPrinters[examples.classes.ExampleTraitWithTypeParam[Int]] -> ((
            "ExampleTraitWithTypeParam",
            "[scala.Int]"
          )),
          testNamesPrinters[examples.classes.ExampleAbstractClass] -> (("ExampleAbstractClass", "")),
          testNamesPrinters[examples.classes.ExampleAbstractClassWithTypeParam[Int]] -> ((
            "ExampleAbstractClassWithTypeParam",
            "[scala.Int]"
          )),
          testNamesPrinters[examples.classes.ExampleClass] -> (("ExampleClass", "")),
          testNamesPrinters[examples.classes.ExampleClassWithTypeParam[Int]] -> ((
            "ExampleClassWithTypeParam",
            "[scala.Int]"
          )),
          testNamesPrinters[examples.classes.ExampleCaseClass] -> (("ExampleCaseClass", "")),
          testNamesPrinters[examples.classes.ExampleCaseClassWithTypeParam[Int]] -> ((
            "ExampleCaseClassWithTypeParam",
            "[scala.Int]"
          ))
        ).foreach { case (actual, (expected, params)) =>
          actual <==> Data(
            Map(
              "Type.shortName" -> Data(expected),
              "Type.fcqn" -> Data(s"hearth.examples.classes.$expected"),
              "Type.plainPrint" -> Data(s"hearth.examples.classes.$expected$params"),
              "Type.prettyPrint" -> Data(s"hearth.examples.classes.$expected$params")
            )
          )
        }
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

        List(
          testNamesPrinters[ExampleTrait] -> (("ExampleTrait", "")),
          testNamesPrinters[ExampleTraitWithTypeParam[Int]] -> (("ExampleTraitWithTypeParam", "[scala.Int]")),
          testNamesPrinters[ExampleAbstractClass] -> (("ExampleAbstractClass", "")),
          testNamesPrinters[ExampleAbstractClassWithTypeParam[Int]] -> ((
            "ExampleAbstractClassWithTypeParam",
            "[scala.Int]"
          )),
          testNamesPrinters[ExampleClass] -> (("ExampleClass", "")),
          testNamesPrinters[ExampleClassWithTypeParam[Int]] -> (("ExampleClassWithTypeParam", "[scala.Int]")),
          testNamesPrinters[ExampleCaseClass] -> (("ExampleCaseClass", "")),
          testNamesPrinters[ExampleCaseClassWithTypeParam[Int]] -> (("ExampleCaseClassWithTypeParam", "[scala.Int]"))
        ).foreach { case (actual, (expected, params)) =>
          actual <==> Data(
            Map(
              "Type.shortName" -> Data(expected),
              "Type.fcqn" -> Data(s"hearth.typed.TypesSpec.$expected"),
              "Type.plainPrint" -> Data(s"hearth.typed.TypesSpec.$expected$params"),
              "Type.prettyPrint" -> Data(s"hearth.typed.TypesSpec.$expected$params")
            )
          )
        }
      }

      test("for enumerations") {
        List(
          // TODO: fix WeekDay.Value on Scala 2
          testNamesPrinters[examples.enums.WeekDay.type] -> (("WeekDay", "")),
          // TODO: fix Planet.Value on Scala 2
          testNamesPrinters[examples.enums.Planet.type] -> (("Planet", "")),
          testNamesPrinters[examples.enums.ExampleSealedTrait] -> (("ExampleSealedTrait", "")),
          testNamesPrinters[examples.enums.ExampleSealedTraitWithTypeParam[Int]] -> ((
            "ExampleSealedTraitWithTypeParam",
            "[scala.Int]"
          )),
          testNamesPrinters[examples.enums.ExampleSealedTraitGADT[Unit]] -> (("ExampleSealedTraitGADT", "[scala.Unit]"))
        ).foreach { case (actual, (expected, params)) =>
          actual <==> Data(
            Map(
              "Type.shortName" -> Data(expected),
              "Type.fcqn" -> Data(s"hearth.examples.enums.$expected"),
              "Type.plainPrint" -> Data(s"hearth.examples.enums.$expected$params"),
              "Type.prettyPrint" -> Data(s"hearth.examples.enums.$expected$params")
            )
          )
        }
      }
    }

    group(
      "methods: Type.{isPrimitive, isBuiltIn, isAbstract, isFinal, isClass, notBuiltInClass, isPlainOldJavaObject, isJavaBean, isSealed, isJavaEnum, isJavaEnumValue, isCase, isObject, isVal, isPublic, isAvailableHere}, expected behavior"
    ) {
      import TypesFixtures.testFlags

      test("for primitive types") {
        List(
          testFlags[Boolean],
          testFlags[Byte],
          testFlags[Short],
          testFlags[Int],
          testFlags[Long],
          testFlags[Float],
          testFlags[Double],
          testFlags[Char]
        ).foreach {
          _ <==> Data(
            Map(
              "Type.isPrimitive" -> Data(true),
              "Type.isBuiltIn" -> Data(true),
              "Type.isAbstract" -> Data(false),
              "Type.isFinal" -> Data(true),
              "Type.isClass" -> Data(true),
              "Type.notBuiltInClass" -> Data(false),
              "Type.isPlainOldJavaObject" -> Data(false),
              "Type.isJavaBean" -> Data(false),
              "Type.isSealed" -> Data(false),
              "Type.isJavaEnum" -> Data(false),
              "Type.isJavaEnumValue" -> Data(false),
              "Type.isCase" -> Data(false),
              "Type.isObject" -> Data(false),
              "Type.isVal" -> Data(false),
              "Type.isAvailable(Everywhere)" -> Data(true)
            )
          )
        }
      }

      test("for build-in types".ignore.pending("We need to fix arrays and S2-S3 mismatch")) {
        // TODO: fix isClass, isFinal
        testFlags[String] <==> Data(
          Map(
            "Type.isPrimitive" -> Data(false),
            "Type.isBuiltIn" -> Data(true),
            "Type.isAbstract" -> Data(false),
            "Type.isFinal" -> Data(false),
            "Type.isClass" -> Data(false),
            "Type.notBuiltInClass" -> Data(false),
            "Type.isPlainOldJavaObject" -> Data(false),
            "Type.isJavaBean" -> Data(false),
            "Type.isSealed" -> Data(false),
            "Type.isJavaEnum" -> Data(false),
            "Type.isJavaEnumValue" -> Data(false),
            "Type.isCase" -> Data(false),
            "Type.isObject" -> Data(false),
            "Type.isVal" -> Data(false),
            "Type.isAvailable(Everywhere)" -> Data(true)
          )
        )
        // TODO: fix isBuiltIn, isPlainOldJavaObject
        testFlags[Array[Int]] <==> Data(
          Map(
            "Type.isPrimitive" -> Data(false),
            "Type.isBuiltIn" -> Data(false),
            "Type.isAbstract" -> Data(false),
            "Type.isFinal" -> Data(true),
            "Type.isClass" -> Data(false),
            "Type.notBuiltInClass" -> Data(false),
            "Type.isPlainOldJavaObject" -> Data(false),
            "Type.isJavaBean" -> Data(false),
            "Type.isSealed" -> Data(false),
            "Type.isJavaEnum" -> Data(false),
            "Type.isJavaEnumValue" -> Data(false),
            "Type.isCase" -> Data(false),
            "Type.isObject" -> Data(false),
            "Type.isVal" -> Data(false),
            "Type.isAvailable(Everywhere)" -> Data(true)
          )
        )
      }

      test("for top-level classes (non-sealed)") {
        List(
          testFlags[examples.classes.ExampleTrait] -> ((true, false, false)),
          testFlags[examples.classes.ExampleTraitWithTypeParam[Int]] -> ((true, false, false)),
          testFlags[examples.classes.ExampleAbstractClass] -> ((true, false, false)),
          testFlags[examples.classes.ExampleAbstractClassWithTypeParam[Int]] -> ((true, false, false)),
          testFlags[examples.classes.ExampleClass] -> ((false, true, false)),
          testFlags[examples.classes.ExampleClassWithTypeParam[Int]] -> ((false, true, false)),
          testFlags[examples.classes.ExampleCaseClass] -> ((false, false, true)),
          testFlags[examples.classes.ExampleCaseClassWithTypeParam[Int]] -> ((false, false, true))
        ).foreach { case (actual, (isAbstract, isFinal, isCase)) =>
          actual <==> Data(
            Map(
              "Type.isPrimitive" -> Data(false),
              "Type.isBuiltIn" -> Data(false),
              "Type.isAbstract" -> Data(isAbstract),
              "Type.isFinal" -> Data(isFinal),
              "Type.isClass" -> Data(true),
              "Type.notBuiltInClass" -> Data(true),
              "Type.isPlainOldJavaObject" -> Data(!isAbstract),
              "Type.isJavaBean" -> Data(false),
              "Type.isSealed" -> Data(false),
              "Type.isJavaEnum" -> Data(false),
              "Type.isJavaEnumValue" -> Data(false),
              "Type.isCase" -> Data(isCase),
              "Type.isObject" -> Data(false),
              "Type.isVal" -> Data(false),
              "Type.isAvailable(Everywhere)" -> Data(true)
            )
          )
        }
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

        List(
          testFlags[ExampleTrait] -> ((true, false, false)),
          testFlags[ExampleTraitWithTypeParam[Int]] -> ((true, false, false)),
          testFlags[ExampleAbstractClass] -> ((true, false, false)),
          testFlags[ExampleAbstractClassWithTypeParam[Int]] -> ((true, false, false)),
          testFlags[ExampleClass] -> ((false, true, false)),
          testFlags[ExampleClassWithTypeParam[Int]] -> ((false, true, false)),
          testFlags[ExampleCaseClass] -> ((false, false, true)),
          testFlags[ExampleCaseClassWithTypeParam[Int]] -> ((false, false, true))
        ).foreach { case (actual, (isAbstract, isFinal, isCase)) =>
          actual <==> Data(
            Map(
              "Type.isPrimitive" -> Data(false),
              "Type.isBuiltIn" -> Data(false),
              "Type.isAbstract" -> Data(isAbstract),
              "Type.isFinal" -> Data(isFinal),
              "Type.isClass" -> Data(true),
              "Type.notBuiltInClass" -> Data(true),
              "Type.isPlainOldJavaObject" -> Data(!isAbstract),
              "Type.isJavaBean" -> Data(false),
              "Type.isSealed" -> Data(false),
              "Type.isJavaEnum" -> Data(false),
              "Type.isJavaEnumValue" -> Data(false),
              "Type.isCase" -> Data(isCase),
              "Type.isObject" -> Data(false),
              "Type.isVal" -> Data(false),
              "Type.isAvailable(Everywhere)" -> Data(true)
            )
          )
        }
      }

      test("for enumerations".ignore.pending("We need to fix S2-S3 mismatch")) {
        List(
          // TODO: fix WeekDay.Value on Scala 2
          testFlags[examples.enums.WeekDay.type] -> ((false, false, true)),
          // TODO: fix Planet.Value on Scala 2
          testFlags[examples.enums.Planet.type] -> ((false, false, true)),
          testFlags[examples.enums.ExampleSealedTrait] -> ((true, true, false)),
          testFlags[examples.enums.ExampleSealedTraitWithTypeParam[Int]] -> ((true, true, false)),
          testFlags[examples.enums.ExampleSealedTraitGADT[Unit]] -> ((true, true, false))
        ).foreach { case (actual, (isAbstract, isSealed, isObject)) =>
          actual <==> Data(
            Map(
              "Type.isPrimitive" -> Data(false),
              "Type.isBuiltIn" -> Data(false),
              "Type.isAbstract" -> Data(isAbstract),
              "Type.isFinal" -> Data(false),
              "Type.isClass" -> Data(true),
              "Type.notBuiltInClass" -> Data(true),
              "Type.isPlainOldJavaObject" -> Data(!isAbstract),
              "Type.isJavaBean" -> Data(false),
              "Type.isSealed" -> Data(isSealed),
              "Type.isJavaEnum" -> Data(false),
              "Type.isJavaEnumValue" -> Data(false),
              "Type.isCase" -> Data(false),
              "Type.isObject" -> Data(isObject),
              "Type.isVal" -> Data(false),
              "Type.isAvailable(Everywhere)" -> Data(true)
            )
          )
        }
      }
    }
  }
}
