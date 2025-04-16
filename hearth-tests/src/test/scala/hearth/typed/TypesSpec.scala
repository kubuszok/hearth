package hearth
package typed

import scala.annotation.unused

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
          actual <==>
            s"""Type.shortName:   $expected
               |Type.fcqn:        scala.$expected
               |Type.plainPrint:  scala.$expected
               |Type.prettyPrint: scala.$expected""".stripMargin
        }
      }

      test("for build-in types") {
        testNamesPrinters[String] <==>
          """Type.shortName:   String
            |Type.fcqn:        java.lang.String
            |Type.plainPrint:  java.lang.String
            |Type.prettyPrint: java.lang.String""".stripMargin
        testNamesPrinters[Array[Int]] <==>
          """Type.shortName:   Array
            |Type.fcqn:        scala.Array
            |Type.plainPrint:  scala.Array[scala.Int]
            |Type.prettyPrint: scala.Array[scala.Int]""".stripMargin
      }

      test("for top-level classes (non-sealed)") {
        List(
          testNamesPrinters[examples.classes.ExampleTrait] -> ("ExampleTrait", ""),
          testNamesPrinters[examples.classes.ExampleTraitWithTypeParam[Int]] -> (
            "ExampleTraitWithTypeParam",
            "[scala.Int]"
          ),
          testNamesPrinters[examples.classes.ExampleAbstractClass] -> ("ExampleAbstractClass", ""),
          testNamesPrinters[examples.classes.ExampleAbstractClassWithTypeParam[Int]] -> (
            "ExampleAbstractClassWithTypeParam",
            "[scala.Int]"
          ),
          testNamesPrinters[examples.classes.ExampleClass] -> ("ExampleClass", ""),
          testNamesPrinters[examples.classes.ExampleClassWithTypeParam[Int]] -> (
            "ExampleClassWithTypeParam",
            "[scala.Int]"
          ),
          testNamesPrinters[examples.classes.ExampleCaseClass] -> ("ExampleCaseClass", ""),
          testNamesPrinters[examples.classes.ExampleCaseClassWithTypeParam[Int]] -> (
            "ExampleCaseClassWithTypeParam",
            "[scala.Int]"
          )
        ).foreach { case (actual, (expected, params)) =>
          actual <==>
            s"""Type.shortName:   $expected
               |Type.fcqn:        hearth.examples.classes.$expected
               |Type.plainPrint:  hearth.examples.classes.$expected$params
               |Type.prettyPrint: hearth.examples.classes.$expected$params""".stripMargin
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
          testNamesPrinters[ExampleTrait] -> ("ExampleTrait", ""),
          testNamesPrinters[ExampleTraitWithTypeParam[Int]] -> ("ExampleTraitWithTypeParam", "[scala.Int]"),
          testNamesPrinters[ExampleAbstractClass] -> ("ExampleAbstractClass", ""),
          testNamesPrinters[ExampleAbstractClassWithTypeParam[Int]] -> (
            "ExampleAbstractClassWithTypeParam",
            "[scala.Int]"
          ),
          testNamesPrinters[ExampleClass] -> ("ExampleClass", ""),
          testNamesPrinters[ExampleClassWithTypeParam[Int]] -> ("ExampleClassWithTypeParam", "[scala.Int]"),
          testNamesPrinters[ExampleCaseClass] -> ("ExampleCaseClass", ""),
          testNamesPrinters[ExampleCaseClassWithTypeParam[Int]] -> ("ExampleCaseClassWithTypeParam", "[scala.Int]")
        ).foreach { case (actual, (expected, params)) =>
          actual <==>
            s"""Type.shortName:   $expected
               |Type.fcqn:        hearth.typed.TypesSpec.$expected
               |Type.plainPrint:  hearth.typed.TypesSpec.$expected$params
               |Type.prettyPrint: hearth.typed.TypesSpec.$expected$params""".stripMargin
        }
      }

      test("for enumerations") {
        List(
          // TODO: fix WeekDay.Value on Scala 2
          testNamesPrinters[examples.enums.WeekDay.type] -> ("WeekDay", ""),
          // TODO: fix Planet.Value on Scala 2
          testNamesPrinters[examples.enums.Planet.type] -> ("Planet", ""),
          testNamesPrinters[examples.enums.ExampleSealedTrait] -> ("ExampleSealedTrait", ""),
          testNamesPrinters[examples.enums.ExampleSealedTraitWithTypeParam[Int]] -> (
            "ExampleSealedTraitWithTypeParam",
            "[scala.Int]"
          ),
          testNamesPrinters[examples.enums.ExampleSealedTraitGADT[Unit]] -> ("ExampleSealedTraitGADT", "[scala.Unit]")
        ).foreach { case (actual, (expected, params)) =>
          actual <==>
            s"""Type.shortName:   $expected
               |Type.fcqn:        hearth.examples.enums.$expected
               |Type.plainPrint:  hearth.examples.enums.$expected$params
               |Type.prettyPrint: hearth.examples.enums.$expected$params""".stripMargin
        }
      }
    }

    group(
      "methods: Type.{isPrimitive, isBuildIn, isAbstract, isFinal, isClass, notBuildInClass, isPlainOldJavaObject, isJavaBean, isSealed, isJavaEnum, isJavaEnumValue, isCase, isObject, isVal, isPublic, isAvailableHere}, expected behavior"
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
          _ <==>
            """Type.isPrimitive: true
              |Type.isBuildIn:   true
              |
              |Type.isAbstract: false
              |Type.isFinal:    true
              |
              |Type.isClass:              true
              |Type.notBuildInClass:      false
              |Type.isPlainOldJavaObject: false
              |Type.isJavaBean:           false
              |
              |Type.isSealed:        false
              |Type.isJavaEnum:      false
              |Type.isJavaEnumValue: false
              |
              |Type.isCase:   false
              |Type.isObject: false
              |Type.isVal:    false
              |
              |Type.isPublic:        true
              |Type.isAvailableHere: true""".stripMargin
        }
      }

      test("for build-in types".ignore.pending("We need to fix arrays and S2-S3 mismatch")) {
        testFlags[String] <==>
          // TODO: fix isClass, isFinal
          """Type.isPrimitive: false
            |Type.isBuildIn:   true
            |
            |Type.isAbstract: false
            |Type.isFinal:    false
            |
            |Type.isClass:              false
            |Type.notBuildInClass:      false
            |Type.isPlainOldJavaObject: false
            |Type.isJavaBean:           false
            |
            |Type.isSealed:        false
            |Type.isJavaEnum:      false
            |Type.isJavaEnumValue: false
            |
            |Type.isCase:   false
            |Type.isObject: false
            |Type.isVal:    false
            |
            |Type.isPublic:        true
            |Type.isAvailableHere: true""".stripMargin
        // TODO: fix isBuildIn, isPlainOldJavaObject
        testFlags[Array[Int]] <==>
          """Type.isPrimitive: false
            |Type.isBuildIn:   false
            |
            |Type.isAbstract: false
            |Type.isFinal:    true
            |
            |Type.isClass:              true
            |Type.notBuildInClass:      true
            |Type.isPlainOldJavaObject: true
            |Type.isJavaBean:           false
            |
            |Type.isSealed:        false
            |Type.isJavaEnum:      false
            |Type.isJavaEnumValue: false
            |
            |Type.isCase:   false
            |Type.isObject: false
            |Type.isVal:    false
            |
            |Type.isPublic:        true
            |Type.isAvailableHere: true""".stripMargin
      }

      test("for top-level classes (non-sealed)") {
        List(
          testFlags[examples.classes.ExampleTrait] -> (true, false, false),
          testFlags[examples.classes.ExampleTraitWithTypeParam[Int]] -> (true, false, false),
          testFlags[examples.classes.ExampleAbstractClass] -> (true, false, false),
          testFlags[examples.classes.ExampleAbstractClassWithTypeParam[Int]] -> (true, false, false),
          testFlags[examples.classes.ExampleClass] -> (false, true, false),
          testFlags[examples.classes.ExampleClassWithTypeParam[Int]] -> (false, true, false),
          testFlags[examples.classes.ExampleCaseClass] -> (false, false, true),
          testFlags[examples.classes.ExampleCaseClassWithTypeParam[Int]] -> (false, false, true)
        ).foreach { case (actual, (isAbstract, isFinal, isCase)) =>
          actual <==>
            s"""Type.isPrimitive: false
               |Type.isBuildIn:   false
               |
               |Type.isAbstract: $isAbstract
               |Type.isFinal:    $isFinal
               |
               |Type.isClass:              true
               |Type.notBuildInClass:      true
               |Type.isPlainOldJavaObject: ${!isAbstract}
               |Type.isJavaBean:           false
               |
               |Type.isSealed:        false
               |Type.isJavaEnum:      false
               |Type.isJavaEnumValue: false
               |
               |Type.isCase:   $isCase
               |Type.isObject: false
               |Type.isVal:    false
               |
               |Type.isPublic:        true
               |Type.isAvailableHere: true""".stripMargin
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
          testFlags[ExampleTrait] -> (true, false, false),
          testFlags[ExampleTraitWithTypeParam[Int]] -> (true, false, false),
          testFlags[ExampleAbstractClass] -> (true, false, false),
          testFlags[ExampleAbstractClassWithTypeParam[Int]] -> (true, false, false),
          testFlags[ExampleClass] -> (false, true, false),
          testFlags[ExampleClassWithTypeParam[Int]] -> (false, true, false),
          testFlags[ExampleCaseClass] -> (false, false, true),
          testFlags[ExampleCaseClassWithTypeParam[Int]] -> (false, false, true)
        ).foreach { case (actual, (isAbstract, isFinal, isCase)) =>
          actual <==>
            s"""Type.isPrimitive: false
               |Type.isBuildIn:   false
               |
               |Type.isAbstract: $isAbstract
               |Type.isFinal:    $isFinal
               |
               |Type.isClass:              true
               |Type.notBuildInClass:      true
               |Type.isPlainOldJavaObject: ${!isAbstract}
               |Type.isJavaBean:           false
               |
               |Type.isSealed:        false
               |Type.isJavaEnum:      false
               |Type.isJavaEnumValue: false
               |
               |Type.isCase:   $isCase
               |Type.isObject: false
               |Type.isVal:    false
               |
               |Type.isPublic:        true
               |Type.isAvailableHere: true""".stripMargin
        }
      }

      test("for enumerations".ignore.pending("We need to fix S2-S3 mismatch")) {
        List(
          // TODO: fix WeekDay.Value on Scala 2
          testFlags[examples.enums.WeekDay.type] -> (false, false, true),
          // TODO: fix Planet.Value on Scala 2
          testFlags[examples.enums.Planet.type] -> (false, false, true),
          testFlags[examples.enums.ExampleSealedTrait] -> (true, true, false),
          testFlags[examples.enums.ExampleSealedTraitWithTypeParam[Int]] -> (true, true, false),
          testFlags[examples.enums.ExampleSealedTraitGADT[Unit]] -> (true, true, false)
        ).foreach { case (actual, (isAbstract, isSealed, isObject)) =>
          actual <==>
            s"""Type.isPrimitive: false
               |Type.isBuildIn:   false
               |
               |Type.isAbstract: $isAbstract
               |Type.isFinal:    false
               |
               |Type.isClass:              true
               |Type.notBuildInClass:      true
               |Type.isPlainOldJavaObject: ${!isAbstract}
               |Type.isJavaBean:           false
               |
               |Type.isSealed:        $isSealed
               |Type.isJavaEnum:      false
               |Type.isJavaEnumValue: false
               |
               |Type.isCase:   false
               |Type.isObject: $isObject
               |Type.isVal:    false
               |
               |Type.isPublic:        true
               |Type.isAvailableHere: true""".stripMargin
        }
      }
    }
  }
}
