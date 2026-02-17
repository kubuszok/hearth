package hearth
package typed

import hearth.data.Data

import scala.annotation.unused
import scala.annotation.nowarn
import scala.math.Ordering.Implicits.infixOrderingOps

/** Macro implementation is in [[TypesFixturesImpl]] */
final class TypesSpec extends MacroSuite {

  group("trait typed.Types") {

    group("type Type") {

      group("methods: Type.{simple, fqcn, plainPrint, prettyPrint}, expected behavior") {
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
            testNamesPrinters[Char] -> "Char"
          ).foreach { case (actual, expected) =>
            actual <==> Data.map(
              "Type.shortName" -> Data(expected),
              "Type.fqcn" -> Data(s"scala.$expected"),
              "Type.plainPrint" -> Data(s"scala.$expected"),
              "Type.prettyPrint" -> Data(s"scala.$expected")
            )
          }
        }

        test("for built-in types") {
          testNamesPrinters[Unit] <==> Data.map(
            "Type.shortName" -> Data("Unit"),
            "Type.fqcn" -> Data(s"scala.Unit"),
            "Type.plainPrint" -> Data(s"scala.Unit"),
            "Type.prettyPrint" -> Data(s"scala.Unit")
          )
          testNamesPrinters[String] <==> Data.map(
            "Type.shortName" -> Data("String"),
            "Type.fqcn" -> Data("java.lang.String"),
            "Type.plainPrint" -> Data("java.lang.String"),
            "Type.prettyPrint" -> Data("java.lang.String")
          )
          testNamesPrinters[Array[Int]] <==> Data.map(
            "Type.shortName" -> Data("Array"),
            "Type.fqcn" -> Data("scala.Array"),
            "Type.plainPrint" -> Data("scala.Array[scala.Int]"),
            "Type.prettyPrint" -> Data("scala.Array[scala.Int]")
          )
        }

        test("for type-system-special types") {
          testNamesPrinters[Any] <==> Data.map(
            "Type.shortName" -> Data("Any"),
            "Type.fqcn" -> Data("scala.Any"),
            "Type.plainPrint" -> Data("scala.Any"),
            "Type.prettyPrint" -> Data("scala.Any")
          )
          testNamesPrinters[AnyRef] <==> Data.map(
            "Type.shortName" -> Data("Object"),
            "Type.fqcn" -> Data("java.lang.Object"),
            "Type.plainPrint" -> Data("java.lang.Object"),
            "Type.prettyPrint" -> Data("java.lang.Object")
          )
          testNamesPrinters[AnyVal] <==> Data.map(
            "Type.shortName" -> Data("AnyVal"),
            "Type.fqcn" -> Data("scala.AnyVal"),
            "Type.plainPrint" -> Data("scala.AnyVal"),
            "Type.prettyPrint" -> Data("scala.AnyVal")
          )
          testNamesPrinters[Null] <==> Data.map(
            "Type.shortName" -> Data("Null"),
            "Type.fqcn" -> Data("scala.Null"),
            "Type.plainPrint" -> Data("scala.Null"),
            "Type.prettyPrint" -> Data("scala.Null")
          )
          testNamesPrinters[Nothing] <==> Data.map(
            "Type.shortName" -> Data("Nothing"),
            "Type.fqcn" -> Data("scala.Nothing"),
            "Type.plainPrint" -> Data("scala.Nothing"),
            "Type.prettyPrint" -> Data("scala.Nothing")
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
            actual <==> Data.map(
              "Type.shortName" -> Data(expected),
              "Type.fqcn" -> Data(s"hearth.examples.classes.$expected"),
              "Type.plainPrint" -> Data(s"hearth.examples.classes.$expected$params"),
              "Type.prettyPrint" -> Data(s"hearth.examples.classes.$expected$params")
            )
          }
        }

        test("for inner classes (non-sealed)") {
          @unused @nowarn
          trait ExampleTrait
          @unused @nowarn
          trait ExampleTraitWithTypeParam[A]

          @unused @nowarn
          abstract class ExampleAbstractClass
          @unused @nowarn
          abstract class ExampleAbstractClassWithTypeParam[A]

          @unused @nowarn
          final class ExampleClass
          @unused @nowarn
          final class ExampleClassWithTypeParam[A]

          @unused @nowarn
          case class ExampleCaseClass(a: Int)
          @unused @nowarn
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
            actual <==> Data.map(
              "Type.shortName" -> Data(expected),
              "Type.fqcn" -> Data(s"hearth.typed.TypesSpec.$expected"),
              "Type.plainPrint" -> Data(s"hearth.typed.TypesSpec.$expected$params"),
              "Type.prettyPrint" -> Data(s"hearth.typed.TypesSpec.$expected$params")
            )
          }
        }

        test("for enumerations") {
          List(
            testNamesPrinters[examples.enums.WeekDay.type] -> ((
              "WeekDay",
              "WeekDay.type",
              ""
            )),
            testNamesPrinters[examples.enums.WeekDay.Value] -> ((
              "Value",
              "WeekDay.Value",
              ""
            )),
            testNamesPrinters[examples.enums.Planet.type] -> ((
              "Planet",
              "Planet.type",
              ""
            )),
            testNamesPrinters[examples.enums.Planet.Value] -> (("Value", "Planet.Value", "")),
            testNamesPrinters[examples.enums.ExampleSealedTrait] -> (("ExampleSealedTrait", "ExampleSealedTrait", "")),
            testNamesPrinters[examples.enums.ExampleSealedTraitWithTypeParam[Int]] -> ((
              "ExampleSealedTraitWithTypeParam",
              "ExampleSealedTraitWithTypeParam",
              "[scala.Int]"
            )),
            testNamesPrinters[examples.enums.ExampleSealedTraitGADT[Unit]] -> ((
              "ExampleSealedTraitGADT",
              "ExampleSealedTraitGADT",
              "[scala.Unit]"
            ))
          ).foreach { case (actual, (shortName, longName, params)) =>
            actual <==> Data.map(
              "Type.shortName" -> Data(shortName),
              "Type.fqcn" -> Data(s"hearth.examples.enums.$longName"),
              "Type.plainPrint" -> Data(s"hearth.examples.enums.$longName$params"),
              "Type.prettyPrint" -> Data(s"hearth.examples.enums.$longName$params")
            )
          }
        }
      }

      group("methods: Type.{classOfType} expected behavior") {
        import TypesFixtures.testClassOfType

        test("for primitive types") {
          testClassOfType[Boolean] <==> Data.map("Type.classOfType" -> Data(classOf[Boolean].toString))
          testClassOfType[Byte] <==> Data.map("Type.classOfType" -> Data(classOf[Byte].toString))
          testClassOfType[Short] <==> Data.map("Type.classOfType" -> Data(classOf[Short].toString))
          testClassOfType[Int] <==> Data.map("Type.classOfType" -> Data(classOf[Int].toString))
          testClassOfType[Long] <==> Data.map("Type.classOfType" -> Data(classOf[Long].toString))
          testClassOfType[Float] <==> Data.map("Type.classOfType" -> Data(classOf[Float].toString))
          testClassOfType[Double] <==> Data.map("Type.classOfType" -> Data(classOf[Double].toString))
          testClassOfType[Char] <==> Data.map("Type.classOfType" -> Data(classOf[Char].toString))
        }

        test("for built-in types".tag(Tags.platformMismatch)) {
          testClassOfType[Unit] <==> Data.map("Type.classOfType" -> Data(classOf[Unit].toString))
          testClassOfType[String] <==> Data.map("Type.classOfType" -> Data(classOf[String].toString))
          if (!Platform.byHearth.isNative) {
            // FIXME:
            // On Native we have:
            //   classOf[Array[Int]].toString == "class scala.scalanative.runtime.IntArray"
            // instead of:
            //   classOf[Array[Int]].toString == "class [I"
            // we might need to fix it or report misalignment in behavior (Scala.js simulate JVM behavior).
            testClassOfType[Array[Int]] <==> Data.map("Type.classOfType" -> Data(classOf[Array[Int]].toString))
          }
        }

        test("for type-system-special types") {
          testClassOfType[Any] <==> Data.map("Type.classOfType" -> Data(classOf[Any].toString))
          testClassOfType[AnyRef] <==> Data.map("Type.classOfType" -> Data(classOf[AnyRef].toString))
          testClassOfType[AnyVal] <==> Data.map("Type.classOfType" -> Data(classOf[AnyVal].toString))
          testClassOfType[Null] <==> Data.map("Type.classOfType" -> Data(classOf[Null].toString))
          testClassOfType[Nothing] <==> Data.map("Type.classOfType" -> Data(classOf[Nothing].toString))
        }

        test("for top-level classes (non-sealed)") {
          testClassOfType[examples.classes.ExampleTrait] <==> Data.map(
            "Type.classOfType" -> Data(classOf[examples.classes.ExampleTrait].toString)
          )
          testClassOfType[examples.classes.ExampleTraitWithTypeParam[Int]] <==> Data.map(
            "Type.classOfType" -> Data(classOf[examples.classes.ExampleTraitWithTypeParam[Int]].toString)
          )
          testClassOfType[examples.classes.ExampleAbstractClass] <==> Data.map(
            "Type.classOfType" -> Data(classOf[examples.classes.ExampleAbstractClass].toString)
          )
          testClassOfType[examples.classes.ExampleAbstractClassWithTypeParam[Int]] <==> Data.map(
            "Type.classOfType" -> Data(classOf[examples.classes.ExampleAbstractClassWithTypeParam[Int]].toString)
          )
          testClassOfType[examples.classes.ExampleClass] <==> Data.map(
            "Type.classOfType" -> Data(classOf[examples.classes.ExampleClass].toString)
          )
          testClassOfType[examples.classes.ExampleClassWithTypeParam[Int]] <==> Data.map(
            "Type.classOfType" -> Data(classOf[examples.classes.ExampleClassWithTypeParam[Int]].toString)
          )
          testClassOfType[examples.classes.ExampleCaseClass] <==> Data.map(
            "Type.classOfType" -> Data(classOf[examples.classes.ExampleCaseClass].toString)
          )
          testClassOfType[examples.classes.ExampleCaseClassWithTypeParam[Int]] <==> Data.map(
            "Type.classOfType" -> Data(classOf[examples.classes.ExampleCaseClassWithTypeParam[Int]].toString)
          )
        }

        test(
          "for inner classes (non-sealed) should not work, since Classes are not available in the macro classpath yet"
        ) {
          @unused @nowarn
          trait ExampleTrait
          @unused @nowarn
          trait ExampleTraitWithTypeParam[A]

          @unused @nowarn
          abstract class ExampleAbstractClass
          @unused @nowarn
          abstract class ExampleAbstractClassWithTypeParam[A]

          @unused @nowarn
          final class ExampleClass
          @unused @nowarn
          final class ExampleClassWithTypeParam[A]

          @unused @nowarn
          case class ExampleCaseClass(a: Int)
          @unused @nowarn
          case class ExampleCaseClassWithTypeParam[A](a: A)

          testClassOfType[ExampleTrait] <==> Data.map("Type.classOfType" -> Data("not on classpath"))
          testClassOfType[ExampleTraitWithTypeParam[Int]] <==> Data.map("Type.classOfType" -> Data("not on classpath"))
          testClassOfType[ExampleAbstractClass] <==> Data.map("Type.classOfType" -> Data("not on classpath"))
          testClassOfType[ExampleAbstractClassWithTypeParam[Int]] <==> Data.map(
            "Type.classOfType" -> Data("not on classpath")
          )
          testClassOfType[ExampleClass] <==> Data.map("Type.classOfType" -> Data("not on classpath"))
          testClassOfType[ExampleClassWithTypeParam[Int]] <==> Data.map("Type.classOfType" -> Data("not on classpath"))
          testClassOfType[ExampleCaseClass] <==> Data.map("Type.classOfType" -> Data("not on classpath"))
          testClassOfType[ExampleCaseClassWithTypeParam[Int]] <==> Data.map(
            "Type.classOfType" -> Data("not on classpath")
          )
        }

        test("for enumerations") {
          testClassOfType[examples.enums.WeekDay.type] <==> Data.map(
            "Type.classOfType" -> Data(classOf[examples.enums.WeekDay.type].toString)
          )
          // FIXME:
          // In the runtime `examples.enums.WeekDay.Value` will be `scala.Eumeration$Value`
          // so we have to come up with a fallback that resolves a different name for the type.
          // testClassOfType[examples.enums.WeekDay.Value] <==> Data(
          //   Map("Type.classOfType" -> Data(classOf[examples.enums.WeekDay.Value].toString))
          // )
          testClassOfType[examples.enums.Planet.type] <==> Data.map(
            "Type.classOfType" -> Data(classOf[examples.enums.Planet.type].toString)
          )
          // FIXME:
          // In the runtime `examples.enums.WeekDay.Value` will be `scala.Eumeration$Value`
          // so we have to come up with a fallback that resolves a different name for the type.
          // testClassOfType[examples.enums.Planet.Value] <==> Data(
          //   Map("Type.classOfType" -> Data(classOf[examples.enums.Planet.Value].toString))
          // )
          testClassOfType[examples.enums.ExampleSealedTrait] <==> Data.map(
            "Type.classOfType" -> Data(classOf[examples.enums.ExampleSealedTrait].toString)
          )
          testClassOfType[examples.enums.ExampleSealedTraitWithTypeParam[Int]] <==> Data.map(
            "Type.classOfType" -> Data(classOf[examples.enums.ExampleSealedTraitWithTypeParam[Int]].toString)
          )
          testClassOfType[examples.enums.ExampleSealedTraitGADT[Unit]] <==> Data.map(
            "Type.classOfType" -> Data(classOf[examples.enums.ExampleSealedTraitGADT[Unit]].toString)
          )
        }
      }

      group("methods: Type.{position} expected behavior") {
        import TypesFixtures.testPosition
        // Apparently: Scala 2 does not store position for for types from previous compilation unit,
        // Scala 3 does store... something? Filename?
        val isScala3 = LanguageVersion.byHearth.isScala3
        val isScala3_8plus = isScala3 && ScalaVersion.byCompileTime >= ScalaVersion(3, 8, 0)
        val isPositionTrimmed =
          testPosition[examples.classes.ExampleTrait] == Data.map("Type.position" -> Data("classes.scala:1:1"))
        def classPosition(line: Int, column: Int): String =
          if (!isScala3) "<no position>"
          else if (isPositionTrimmed) "classes.scala:1:1"
          else s"classes.scala:$line:$column"
        def enumPosition(line: Int, column: Int): String =
          if (!isScala3) "<no position>" else if (isPositionTrimmed) "enums.scala:1:1" else s"enums.scala:$line:$column"

        test("for primitive types") {
          testPosition[Boolean] <==> Data.map(
            "Type.position" -> Data(if (isScala3_8plus) "Boolean.scala:1:1" else "<no position>")
          )
          testPosition[Byte] <==> Data.map(
            "Type.position" -> Data(if (isScala3_8plus) "Byte.scala:1:1" else "<no position>")
          )
          testPosition[Short] <==> Data.map(
            "Type.position" -> Data(if (isScala3_8plus) "Short.scala:1:1" else "<no position>")
          )
          testPosition[Int] <==> Data.map(
            "Type.position" -> Data(if (isScala3_8plus) "Int.scala:1:1" else "<no position>")
          )
          testPosition[Long] <==> Data.map(
            "Type.position" -> Data(if (isScala3_8plus) "Long.scala:1:1" else "<no position>")
          )
          testPosition[Float] <==> Data.map(
            "Type.position" -> Data(if (isScala3_8plus) "Float.scala:1:1" else "<no position>")
          )
          testPosition[Double] <==> Data.map(
            "Type.position" -> Data(if (isScala3_8plus) "Double.scala:1:1" else "<no position>")
          )
          testPosition[Char] <==> Data.map(
            "Type.position" -> Data(if (isScala3_8plus) "Char.scala:1:1" else "<no position>")
          )
        }

        test("for built-in types".tag(Tags.langVerMismatch)) {
          testPosition[Unit] <==> Data.map(
            "Type.position" -> Data(if (isScala3_8plus) "Unit.scala:1:1" else "<no position>")
          )
          testPosition[String] <==> Data.map(
            "Type.position" -> Data(if (isScala3) "Predef.scala:1:1" else "<no position>")
          )
          testPosition[Array[Int]] <==> Data.map(
            "Type.position" -> Data(if (isScala3_8plus) "Array.scala:1:1" else "<no position>")
          )
        }

        test("for type-system-special types".tag(Tags.langVerMismatch)) {
          testPosition[Any] <==> Data.map("Type.position" -> Data("<no position>"))
          testPosition[AnyRef] <==> Data.map("Type.position" -> Data("<no position>"))
          testPosition[AnyVal] <==> Data.map(
            "Type.position" -> Data(if (isScala3_8plus) "AnyVal.scala:1:1" else "<no position>")
          )
          testPosition[Null] <==> Data.map("Type.position" -> Data("<no position>"))
          testPosition[Nothing] <==> Data.map("Type.position" -> Data("<no position>"))
        }

        test("for top-level classes (non-sealed)".tag(Tags.langVerMismatch)) {
          testPosition[examples.classes.ExampleTrait] <==> Data.map(
            "Type.position" -> Data(classPosition(5, 7))
          )
          testPosition[examples.classes.ExampleTraitWithTypeParam[Int]] <==> Data.map(
            "Type.position" -> Data(classPosition(6, 7))
          )
          testPosition[examples.classes.ExampleAbstractClass] <==> Data.map(
            "Type.position" -> Data(classPosition(8, 16))
          )
          testPosition[examples.classes.ExampleAbstractClassWithTypeParam[Int]] <==> Data.map(
            "Type.position" -> Data(classPosition(9, 16))
          )
          testPosition[examples.classes.ExampleClass] <==> Data.map(
            "Type.position" -> Data(classPosition(11, 13))
          )
          testPosition[examples.classes.ExampleClassWithTypeParam[Int]] <==> Data.map(
            "Type.position" -> Data(classPosition(12, 13))
          )
          testPosition[examples.classes.ExampleCaseClass] <==> Data.map(
            "Type.position" -> Data(classPosition(14, 12))
          )
          testPosition[examples.classes.ExampleCaseClassWithTypeParam[Int]] <==> Data.map(
            "Type.position" -> Data(classPosition(15, 12))
          )
        }

        test(
          "for inner classes (non-sealed) should not work, since Classes are not available in the macro classpath yet"
        ) {
          val firstTestLine = implicitly[munit.Location].line

          @unused @nowarn
          trait ExampleTrait
          @unused @nowarn
          trait ExampleTraitWithTypeParam[A]

          @unused @nowarn
          abstract class ExampleAbstractClass
          @unused @nowarn
          abstract class ExampleAbstractClassWithTypeParam[A]

          @unused @nowarn
          final class ExampleClass
          @unused @nowarn
          final class ExampleClassWithTypeParam[A]

          @unused @nowarn
          case class ExampleCaseClass(a: Int)
          @unused @nowarn
          case class ExampleCaseClassWithTypeParam[A](a: A)

          testPosition[ExampleTrait] <==> Data.map("Type.position" -> Data(s"TypesSpec.scala:${firstTestLine + 3}:17"))
          testPosition[ExampleTraitWithTypeParam[Int]] <==> Data.map(
            "Type.position" -> Data(s"TypesSpec.scala:${firstTestLine + 5}:17")
          )
          testPosition[ExampleAbstractClass] <==> Data.map(
            "Type.position" -> Data(s"TypesSpec.scala:${firstTestLine + 8}:26")
          )
          testPosition[ExampleAbstractClassWithTypeParam[Int]] <==> Data.map(
            "Type.position" -> Data(s"TypesSpec.scala:${firstTestLine + 10}:26")
          )
          testPosition[ExampleClass] <==> Data.map("Type.position" -> Data(s"TypesSpec.scala:${firstTestLine + 13}:23"))
          testPosition[ExampleClassWithTypeParam[Int]] <==> Data.map(
            "Type.position" -> Data(s"TypesSpec.scala:${firstTestLine + 15}:23")
          )
          testPosition[ExampleCaseClass] <==> Data.map(
            "Type.position" -> Data(s"TypesSpec.scala:${firstTestLine + 18}:22")
          )
          testPosition[ExampleCaseClassWithTypeParam[Int]] <==> Data.map(
            "Type.position" -> Data(s"TypesSpec.scala:${firstTestLine + 20}:22")
          )
        }

        test("for enumerations".tag(Tags.langVerMismatch)) {
          // Apparently: Scala 2 does not store position for for types from previous compilation unit,
          // Scala 3 does store... something? Filename?
          testPosition[examples.enums.WeekDay.type] <==> Data.map(
            "Type.position" -> Data(enumPosition(5, 8))
          )
          testPosition[examples.enums.WeekDay.Value] <==> Data.map(
            "Type.position" -> Data(if (isScala3_8plus) "Enumeration.scala:1:1" else "<no position>")
          )
          testPosition[examples.enums.Planet.type] <==> Data.map(
            "Type.position" -> Data(enumPosition(10, 8))
          )
          testPosition[examples.enums.Planet.Value] <==> Data.map(
            "Type.position" -> Data(if (isScala3_8plus) "Enumeration.scala:1:1" else "<no position>")
          )
          testPosition[examples.enums.ExampleSealedTrait] <==> Data.map(
            "Type.position" -> Data(enumPosition(32, 14))
          )
          testPosition[examples.enums.ExampleSealedTraitWithTypeParam[Int]] <==> Data.map(
            "Type.position" -> Data(enumPosition(38, 14))
          )
          testPosition[examples.enums.ExampleSealedTraitGADT[Unit]] <==> Data.map(
            "Type.position" -> Data(enumPosition(44, 14))
          )
        }
      }

      group("methods: Type.{directChildren, exhaustiveChildren} expected behavior") {
        import TypesFixtures.testChildren

        test("for primitive types") {
          List(
            testChildren[Boolean],
            testChildren[Byte],
            testChildren[Short],
            testChildren[Int],
            testChildren[Long],
            testChildren[Float],
            testChildren[Double],
            testChildren[Char]
          ).foreach {
            _ <==> Data.map(
              "Type.directChildren" -> Data("<no direct children>"),
              "Type.exhaustiveChildren" -> Data("<no exhaustive children>")
            )
          }
        }

        test("for built-in types") {
          List(
            testChildren[Unit],
            testChildren[String],
            testChildren[Array[Int]]
          ).foreach {
            _ <==> Data.map(
              "Type.directChildren" -> Data("<no direct children>"),
              "Type.exhaustiveChildren" -> Data("<no exhaustive children>")
            )
          }
        }

        test("for type-system-special types") {
          List(
            testChildren[Any],
            testChildren[AnyRef],
            testChildren[AnyVal],
            testChildren[Null],
            testChildren[Nothing]
          ).foreach {
            _ <==> Data.map(
              "Type.directChildren" -> Data("<no direct children>"),
              "Type.exhaustiveChildren" -> Data("<no exhaustive children>")
            )
          }
        }

        test("for top-level classes (non-sealed)") {
          List(
            testChildren[examples.classes.ExampleTrait],
            testChildren[examples.classes.ExampleTraitWithTypeParam[Int]],
            testChildren[examples.classes.ExampleAbstractClass],
            testChildren[examples.classes.ExampleAbstractClassWithTypeParam[Int]],
            testChildren[examples.classes.ExampleClass],
            testChildren[examples.classes.ExampleClassWithTypeParam[Int]],
            testChildren[examples.classes.ExampleCaseClass],
            testChildren[examples.classes.ExampleCaseClassWithTypeParam[Int]]
          ).foreach {
            _ <==> Data.map(
              "Type.directChildren" -> Data("<no direct children>"),
              "Type.exhaustiveChildren" -> Data("<no exhaustive children>")
            )
          }
        }

        test("for inner classes (non-sealed)") {
          @unused @nowarn
          trait ExampleTrait
          @unused @nowarn
          trait ExampleTraitWithTypeParam[A]

          @unused @nowarn
          abstract class ExampleAbstractClass
          @unused @nowarn
          abstract class ExampleAbstractClassWithTypeParam[A]

          @unused @nowarn
          final class ExampleClass
          @unused @nowarn
          final class ExampleClassWithTypeParam[A]

          @unused @nowarn
          case class ExampleCaseClass(a: Int)
          @unused @nowarn
          case class ExampleCaseClassWithTypeParam[A](a: A)

          List(
            testChildren[ExampleTrait],
            testChildren[ExampleTraitWithTypeParam[Int]],
            testChildren[ExampleAbstractClass],
            testChildren[ExampleAbstractClassWithTypeParam[Int]],
            testChildren[ExampleClass],
            testChildren[ExampleClassWithTypeParam[Int]],
            testChildren[ExampleCaseClass],
            testChildren[ExampleCaseClassWithTypeParam[Int]]
          ).foreach {
            _ <==> Data.map(
              "Type.directChildren" -> Data("<no direct children>"),
              "Type.exhaustiveChildren" -> Data("<no exhaustive children>")
            )
          }
        }

        test("for enumerations") {
          // Note: The exact type strings will be determined by running the tests
          // For now, we'll use placeholders and adjust based on actual output
          testChildren[examples.enums.WeekDay.type] <==> Data.map(
            "Type.directChildren" -> Data.map(
              "Fri" -> Data("hearth.examples.enums.WeekDay.Fri.type"),
              "Mon" -> Data("hearth.examples.enums.WeekDay.Mon.type"),
              "Sat" -> Data("hearth.examples.enums.WeekDay.Sat.type"),
              "Sun" -> Data("hearth.examples.enums.WeekDay.Sun.type"),
              "Thu" -> Data("hearth.examples.enums.WeekDay.Thu.type"),
              "Tue" -> Data("hearth.examples.enums.WeekDay.Tue.type"),
              "Wed" -> Data("hearth.examples.enums.WeekDay.Wed.type")
            ),
            "Type.exhaustiveChildren" -> Data.map(
              "Fri" -> Data("hearth.examples.enums.WeekDay.Fri.type"),
              "Mon" -> Data("hearth.examples.enums.WeekDay.Mon.type"),
              "Sat" -> Data("hearth.examples.enums.WeekDay.Sat.type"),
              "Sun" -> Data("hearth.examples.enums.WeekDay.Sun.type"),
              "Thu" -> Data("hearth.examples.enums.WeekDay.Thu.type"),
              "Tue" -> Data("hearth.examples.enums.WeekDay.Tue.type"),
              "Wed" -> Data("hearth.examples.enums.WeekDay.Wed.type")
            )
          )
          testChildren[examples.enums.WeekDay.Value] <==> Data.map(
            "Type.directChildren" -> Data.map(
              "Fri" -> Data("hearth.examples.enums.WeekDay.Fri.type"),
              "Mon" -> Data("hearth.examples.enums.WeekDay.Mon.type"),
              "Sat" -> Data("hearth.examples.enums.WeekDay.Sat.type"),
              "Sun" -> Data("hearth.examples.enums.WeekDay.Sun.type"),
              "Thu" -> Data("hearth.examples.enums.WeekDay.Thu.type"),
              "Tue" -> Data("hearth.examples.enums.WeekDay.Tue.type"),
              "Wed" -> Data("hearth.examples.enums.WeekDay.Wed.type")
            ),
            "Type.exhaustiveChildren" -> Data.map(
              "Fri" -> Data("hearth.examples.enums.WeekDay.Fri.type"),
              "Mon" -> Data("hearth.examples.enums.WeekDay.Mon.type"),
              "Sat" -> Data("hearth.examples.enums.WeekDay.Sat.type"),
              "Sun" -> Data("hearth.examples.enums.WeekDay.Sun.type"),
              "Thu" -> Data("hearth.examples.enums.WeekDay.Thu.type"),
              "Tue" -> Data("hearth.examples.enums.WeekDay.Tue.type"),
              "Wed" -> Data("hearth.examples.enums.WeekDay.Wed.type")
            )
          )
          testChildren[examples.enums.Planet.type] <==> Data.map(
            "Type.directChildren" -> Data.map(
              "Earth" -> Data("hearth.examples.enums.Planet.Earth.type"),
              "Jupiter" -> Data("hearth.examples.enums.Planet.Jupiter.type"),
              "Mars" -> Data("hearth.examples.enums.Planet.Mars.type"),
              "Mercury" -> Data("hearth.examples.enums.Planet.Mercury.type"),
              "Neptune" -> Data("hearth.examples.enums.Planet.Neptune.type"),
              "Saturn" -> Data("hearth.examples.enums.Planet.Saturn.type"),
              "Uranus" -> Data("hearth.examples.enums.Planet.Uranus.type"),
              "Venus" -> Data("hearth.examples.enums.Planet.Venus.type")
            ),
            "Type.exhaustiveChildren" -> Data.map(
              "Earth" -> Data("hearth.examples.enums.Planet.Earth.type"),
              "Jupiter" -> Data("hearth.examples.enums.Planet.Jupiter.type"),
              "Mars" -> Data("hearth.examples.enums.Planet.Mars.type"),
              "Mercury" -> Data("hearth.examples.enums.Planet.Mercury.type"),
              "Neptune" -> Data("hearth.examples.enums.Planet.Neptune.type"),
              "Saturn" -> Data("hearth.examples.enums.Planet.Saturn.type"),
              "Uranus" -> Data("hearth.examples.enums.Planet.Uranus.type"),
              "Venus" -> Data("hearth.examples.enums.Planet.Venus.type")
            )
          )
          testChildren[examples.enums.Planet.Value] <==> Data.map(
            "Type.directChildren" -> Data.map(
              "Earth" -> Data("hearth.examples.enums.Planet.Earth.type"),
              "Jupiter" -> Data("hearth.examples.enums.Planet.Jupiter.type"),
              "Mars" -> Data("hearth.examples.enums.Planet.Mars.type"),
              "Mercury" -> Data("hearth.examples.enums.Planet.Mercury.type"),
              "Neptune" -> Data("hearth.examples.enums.Planet.Neptune.type"),
              "Saturn" -> Data("hearth.examples.enums.Planet.Saturn.type"),
              "Uranus" -> Data("hearth.examples.enums.Planet.Uranus.type"),
              "Venus" -> Data("hearth.examples.enums.Planet.Venus.type")
            ),
            "Type.exhaustiveChildren" -> Data.map(
              "Earth" -> Data("hearth.examples.enums.Planet.Earth.type"),
              "Jupiter" -> Data("hearth.examples.enums.Planet.Jupiter.type"),
              "Mars" -> Data("hearth.examples.enums.Planet.Mars.type"),
              "Mercury" -> Data("hearth.examples.enums.Planet.Mercury.type"),
              "Neptune" -> Data("hearth.examples.enums.Planet.Neptune.type"),
              "Saturn" -> Data("hearth.examples.enums.Planet.Saturn.type"),
              "Uranus" -> Data("hearth.examples.enums.Planet.Uranus.type"),
              "Venus" -> Data("hearth.examples.enums.Planet.Venus.type")
            )
          )
          testChildren[examples.enums.ExampleSealedTrait] <==> Data.map(
            "Type.directChildren" -> Data.map(
              "ExampleSealedTraitClass" -> Data("hearth.examples.enums.ExampleSealedTrait.ExampleSealedTraitClass"),
              "ExampleSealedTraitObject" -> Data(
                "hearth.examples.enums.ExampleSealedTrait.ExampleSealedTraitObject.type"
              )
            ),
            "Type.exhaustiveChildren" -> Data.map(
              "ExampleSealedTraitClass" -> Data("hearth.examples.enums.ExampleSealedTrait.ExampleSealedTraitClass"),
              "ExampleSealedTraitObject" -> Data(
                "hearth.examples.enums.ExampleSealedTrait.ExampleSealedTraitObject.type"
              )
            )
          )
          testChildren[examples.enums.ExampleSealedTraitWithTypeParam[Int]] <==> Data.map(
            "Type.directChildren" -> Data.map(
              "ExampleSealedTraitWithTypeParamClass" -> Data(
                "hearth.examples.enums.ExampleSealedTraitWithTypeParam.ExampleSealedTraitWithTypeParamClass[scala.Int]"
              ),
              "ExampleSealedTraitWithTypeParamObject" -> Data(
                "hearth.examples.enums.ExampleSealedTraitWithTypeParam.ExampleSealedTraitWithTypeParamObject.type"
              )
            ),
            "Type.exhaustiveChildren" -> Data.map(
              "ExampleSealedTraitWithTypeParamClass" -> Data(
                "hearth.examples.enums.ExampleSealedTraitWithTypeParam.ExampleSealedTraitWithTypeParamClass[scala.Int]"
              ),
              "ExampleSealedTraitWithTypeParamObject" -> Data(
                "hearth.examples.enums.ExampleSealedTraitWithTypeParam.ExampleSealedTraitWithTypeParamObject.type"
              )
            )
          )
          testChildren[examples.enums.ExampleSealedTraitGADT[Unit]] <==> Data.map(
            "Type.directChildren" -> Data.map(
              "ExampleSealedTraitWithTypeParamClass" -> Data(
                "hearth.examples.enums.ExampleSealedTraitGADT.ExampleSealedTraitWithTypeParamClass"
              ),
              "ExampleSealedTraitWithTypeParamObject" -> Data(
                "hearth.examples.enums.ExampleSealedTraitGADT.ExampleSealedTraitWithTypeParamObject.type"
              )
            ),
            "Type.exhaustiveChildren" -> Data.map(
              "ExampleSealedTraitWithTypeParamObject" -> Data(
                "hearth.examples.enums.ExampleSealedTraitGADT.ExampleSealedTraitWithTypeParamObject.type"
              )
            )
          )
        }
      }

      test("methods: Type.{annotations} expected behavior") {
        import TypesFixtures.testAnnotations

        test("on traits") {
          testAnnotations[examples.methods.Trait] <==> Data.map(
            "Type.annotations" -> Data.list(
              Data("hearth.examples.methods.ExampleAnnotation")
            )
          )
          testAnnotations[examples.methods.NoCompanionClass] <==> Data.map(
            "Type.annotations" -> Data.list(
              Data("hearth.examples.methods.ExampleAnnotation2")
            )
          )
        }
      }

      group(
        "methods: Type.{isPrimitive, isJvmBuiltIn, isAbstract, isFinal, isClass, isTuple, notJvmBuiltInClass, isPlainOldJavaObject, isJavaBean, isSealed, isJavaEnum, isJavaEnumValue, isCase, isObject, isVal, isCaseClass, isCaseObject, isCaseVal, isAvailableHere}, expected behavior"
      ) {
        import TypesFixtures.{testFlags, testChildrenFlags}

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
            _ <==> Data.map(
              "Type.isPrimitive" -> Data(true),
              "Type.isArray" -> Data(false),
              "Type.isIArray" -> Data(false),
              "Type.isJvmBuiltIn" -> Data(true),
              "Type.isAbstract" -> Data(false),
              "Type.isFinal" -> Data(true),
              "Type.isClass" -> Data(true),
              "Type.isTypeSystemSpecial" -> Data(false),
              "Type.isOpaqueType" -> Data(false),
              "Type.isTuple" -> Data(false),
              "Type.isNamedTuple" -> Data(false),
              "Type.notJvmBuiltInClass" -> Data(false),
              "Type.isPlainOldJavaObject" -> Data(false),
              "Type.isJavaBean" -> Data(false),
              "Type.isSealed" -> Data(false),
              "Type.isJavaEnum" -> Data(false),
              "Type.isJavaEnumValue" -> Data(false),
              "Type.isEnumeration" -> Data(false),
              "Type.isCase" -> Data(false),
              "Type.isObject" -> Data(false),
              "Type.isVal" -> Data(false),
              "Type.isCaseClass" -> Data(false),
              "Type.isCaseObject" -> Data(false),
              "Type.isCaseVal" -> Data(false),
              "Type.isAvailable(Everywhere)" -> Data(true),
              "Type.isAvailable(AtCallSite)" -> Data(true)
            )
          }
        }

        test("for built-in types") {

          testFlags[Unit] <==> Data.map(
            "Type.isPrimitive" -> Data(false),
            "Type.isArray" -> Data(false),
            "Type.isIArray" -> Data(false),
            "Type.isJvmBuiltIn" -> Data(true),
            "Type.isAbstract" -> Data(false),
            "Type.isFinal" -> Data(true),
            "Type.isClass" -> Data(true),
            "Type.isTypeSystemSpecial" -> Data(false),
            "Type.isOpaqueType" -> Data(false),
            "Type.isTuple" -> Data(false),
            "Type.isNamedTuple" -> Data(false),
            "Type.notJvmBuiltInClass" -> Data(false),
            "Type.isPlainOldJavaObject" -> Data(false),
            "Type.isJavaBean" -> Data(false),
            "Type.isSealed" -> Data(false),
            "Type.isJavaEnum" -> Data(false),
            "Type.isJavaEnumValue" -> Data(false),
            "Type.isEnumeration" -> Data(false),
            "Type.isCase" -> Data(false),
            "Type.isObject" -> Data(false),
            "Type.isVal" -> Data(false),
            "Type.isCaseClass" -> Data(false),
            "Type.isCaseObject" -> Data(false),
            "Type.isCaseVal" -> Data(false),
            "Type.isAvailable(Everywhere)" -> Data(true),
            "Type.isAvailable(AtCallSite)" -> Data(true)
          )

          testFlags[String] <==> Data.map(
            "Type.isPrimitive" -> Data(false),
            "Type.isArray" -> Data(false),
            "Type.isIArray" -> Data(false),
            "Type.isJvmBuiltIn" -> Data(true),
            "Type.isAbstract" -> Data(false),
            "Type.isFinal" -> Data(true),
            "Type.isClass" -> Data(true),
            "Type.isTypeSystemSpecial" -> Data(false),
            "Type.isOpaqueType" -> Data(false),
            "Type.isTuple" -> Data(false),
            "Type.isNamedTuple" -> Data(false),
            "Type.notJvmBuiltInClass" -> Data(false),
            "Type.isPlainOldJavaObject" -> Data(false),
            "Type.isJavaBean" -> Data(false),
            "Type.isSealed" -> Data(false),
            "Type.isJavaEnum" -> Data(false),
            "Type.isJavaEnumValue" -> Data(false),
            "Type.isEnumeration" -> Data(false),
            "Type.isCase" -> Data(false),
            "Type.isObject" -> Data(false),
            "Type.isVal" -> Data(false),
            "Type.isCaseClass" -> Data(false),
            "Type.isCaseObject" -> Data(false),
            "Type.isCaseVal" -> Data(false),
            "Type.isAvailable(Everywhere)" -> Data(true),
            "Type.isAvailable(AtCallSite)" -> Data(true)
          )

          testFlags[Array[Int]] <==> Data.map(
            "Type.isPrimitive" -> Data(false),
            "Type.isArray" -> Data(true),
            "Type.isIArray" -> Data(false),
            "Type.isJvmBuiltIn" -> Data(true),
            "Type.isAbstract" -> Data(false),
            "Type.isFinal" -> Data(true),
            "Type.isClass" -> Data(false),
            "Type.isTypeSystemSpecial" -> Data(false),
            "Type.isOpaqueType" -> Data(false),
            "Type.isTuple" -> Data(false),
            "Type.isNamedTuple" -> Data(false),
            "Type.notJvmBuiltInClass" -> Data(false),
            "Type.isPlainOldJavaObject" -> Data(false),
            "Type.isJavaBean" -> Data(false),
            "Type.isSealed" -> Data(false),
            "Type.isJavaEnum" -> Data(false),
            "Type.isJavaEnumValue" -> Data(false),
            "Type.isEnumeration" -> Data(false),
            "Type.isCase" -> Data(false),
            "Type.isObject" -> Data(false),
            "Type.isVal" -> Data(false),
            "Type.isCaseClass" -> Data(false),
            "Type.isCaseObject" -> Data(false),
            "Type.isCaseVal" -> Data(false),
            "Type.isAvailable(Everywhere)" -> Data(true),
            "Type.isAvailable(AtCallSite)" -> Data(true)
          )
        }

        test("for top-level classes (non-sealed)") {
          List(
            testFlags[examples.classes.ExampleTrait] -> ((true, false, false, false)),
            testFlags[examples.classes.ExampleTraitWithTypeParam[Int]] -> ((true, false, false, false)),
            testFlags[examples.classes.ExampleAbstractClass] -> ((true, false, false, false)),
            testFlags[examples.classes.ExampleAbstractClassWithTypeParam[Int]] -> ((true, false, false, false)),
            testFlags[examples.classes.ExampleClass] -> ((false, true, false, true)),
            testFlags[examples.classes.ExampleClassWithTypeParam[Int]] -> ((false, true, false, true)),
            testFlags[examples.classes.ExampleCaseClass] -> ((false, false, true, false)),
            testFlags[examples.classes.ExampleCaseClassWithTypeParam[Int]] -> ((false, false, true, false))
          ).foreach { case (actual, (isAbstract, isFinal, isCase, isJavaBean)) =>
            actual <==> Data.map(
              "Type.isPrimitive" -> Data(false),
              "Type.isArray" -> Data(false),
              "Type.isIArray" -> Data(false),
              "Type.isJvmBuiltIn" -> Data(false),
              "Type.isAbstract" -> Data(isAbstract),
              "Type.isFinal" -> Data(isFinal),
              "Type.isClass" -> Data(true),
              "Type.isTypeSystemSpecial" -> Data(false),
              "Type.isOpaqueType" -> Data(false),
              "Type.isTuple" -> Data(false),
              "Type.isNamedTuple" -> Data(false),
              "Type.notJvmBuiltInClass" -> Data(true),
              "Type.isPlainOldJavaObject" -> Data(!isAbstract),
              "Type.isJavaBean" -> Data(isJavaBean),
              "Type.isSealed" -> Data(false),
              "Type.isJavaEnum" -> Data(false),
              "Type.isJavaEnumValue" -> Data(false),
              "Type.isEnumeration" -> Data(false),
              "Type.isCase" -> Data(isCase),
              "Type.isObject" -> Data(false),
              "Type.isVal" -> Data(false),
              "Type.isCaseClass" -> Data(isCase),
              "Type.isCaseObject" -> Data(false),
              "Type.isCaseVal" -> Data(false),
              "Type.isAvailable(Everywhere)" -> Data(true),
              "Type.isAvailable(AtCallSite)" -> Data(true)
            )
          }
        }

        test("for inner classes (non-sealed)") {
          @unused @nowarn
          trait ExampleTrait
          @unused @nowarn
          trait ExampleTraitWithTypeParam[A]

          @unused @nowarn
          abstract class ExampleAbstractClass
          @unused @nowarn
          abstract class ExampleAbstractClassWithTypeParam[A]

          @unused @nowarn
          final class ExampleClass
          @unused @nowarn
          final class ExampleClassWithTypeParam[A]

          @unused @nowarn
          case class ExampleCaseClass(a: Int)
          @unused @nowarn
          case class ExampleCaseClassWithTypeParam[A](a: A)

          List(
            testFlags[ExampleTrait] -> ((true, false, false, false)),
            testFlags[ExampleTraitWithTypeParam[Int]] -> ((true, false, false, false)),
            testFlags[ExampleAbstractClass] -> ((true, false, false, false)),
            testFlags[ExampleAbstractClassWithTypeParam[Int]] -> ((true, false, false, false)),
            testFlags[ExampleClass] -> ((false, true, false, true)),
            testFlags[ExampleClassWithTypeParam[Int]] -> ((false, true, false, true)),
            testFlags[ExampleCaseClass] -> ((false, false, true, false)),
            testFlags[ExampleCaseClassWithTypeParam[Int]] -> ((false, false, true, false))
          ).foreach { case (actual, (isAbstract, isFinal, isCase, isJavaBean)) =>
            actual <==> Data.map(
              "Type.isPrimitive" -> Data(false),
              "Type.isArray" -> Data(false),
              "Type.isIArray" -> Data(false),
              "Type.isJvmBuiltIn" -> Data(false),
              "Type.isAbstract" -> Data(isAbstract),
              "Type.isFinal" -> Data(isFinal),
              "Type.isClass" -> Data(true),
              "Type.isTypeSystemSpecial" -> Data(false),
              "Type.isOpaqueType" -> Data(false),
              "Type.isTuple" -> Data(false),
              "Type.isNamedTuple" -> Data(false),
              "Type.notJvmBuiltInClass" -> Data(true),
              "Type.isPlainOldJavaObject" -> Data(!isAbstract),
              "Type.isJavaBean" -> Data(isJavaBean),
              "Type.isSealed" -> Data(false),
              "Type.isJavaEnum" -> Data(false),
              "Type.isJavaEnumValue" -> Data(false),
              "Type.isEnumeration" -> Data(false),
              "Type.isCase" -> Data(isCase),
              "Type.isObject" -> Data(false),
              "Type.isVal" -> Data(false),
              "Type.isCaseClass" -> Data(isCase),
              "Type.isCaseObject" -> Data(false),
              "Type.isCaseVal" -> Data(false),
              "Type.isAvailable(Everywhere)" -> Data(true),
              "Type.isAvailable(AtCallSite)" -> Data(true)
            )
          }
        }

        test("for enumerations") {
          List(
            testFlags[examples.enums.WeekDay.type] -> ((false, true, false, true, false, true)),
            testFlags[examples.enums.Planet.type] -> ((false, true, false, true, false, true)),
            testFlags[examples.enums.WeekDay.Value] -> ((false, false, false, false, false, true)),
            testFlags[examples.enums.Planet.Value] -> ((false, false, false, false, false, true)),
            testFlags[examples.enums.ExampleSealedTrait] -> ((true, false, true, false, false, false)),
            testFlags[examples.enums.ExampleSealedTraitWithTypeParam[Int]] -> ((true, false, true, false, false,
              false)),
            testFlags[examples.enums.ExampleSealedTraitGADT[Unit]] -> ((true, false, true, false, false, false))
          ).foreach { case (actual, (isAbstract, isFinal, isSealed, isObject, isJavaBean, isEnumeration)) =>
            actual <==> Data.map(
              "Type.isPrimitive" -> Data(false),
              "Type.isArray" -> Data(false),
              "Type.isIArray" -> Data(false),
              "Type.isJvmBuiltIn" -> Data(false),
              "Type.isAbstract" -> Data(isAbstract),
              "Type.isFinal" -> Data(isFinal),
              "Type.isClass" -> Data(true),
              "Type.isTypeSystemSpecial" -> Data(false),
              "Type.isOpaqueType" -> Data(false),
              "Type.isTuple" -> Data(false),
              "Type.isNamedTuple" -> Data(false),
              "Type.notJvmBuiltInClass" -> Data(true),
              "Type.isPlainOldJavaObject" -> Data(!isAbstract && !isSealed && !isEnumeration),
              "Type.isJavaBean" -> Data(isJavaBean),
              "Type.isSealed" -> Data(isSealed),
              "Type.isJavaEnum" -> Data(false),
              "Type.isJavaEnumValue" -> Data(false),
              "Type.isEnumeration" -> Data(isEnumeration),
              "Type.isCase" -> Data(false),
              "Type.isObject" -> Data(isObject),
              "Type.isVal" -> Data(false),
              "Type.isCaseClass" -> Data(false),
              "Type.isCaseObject" -> Data(false),
              "Type.isCaseVal" -> Data(false),
              "Type.isAvailable(Everywhere)" -> Data(true),
              "Type.isAvailable(AtCallSite)" -> Data(true)
            )
          }

          testChildrenFlags[examples.enums.ExampleSealedTrait] <==> Data.map(
            "ExampleSealedTraitClass" -> Data.map(
              "Type.isSealed" -> Data(false),
              "Type.isJavaEnum" -> Data(false),
              "Type.isJavaEnumValue" -> Data(false),
              "Type.isEnumeration" -> Data(false),
              "Type.isCase" -> Data(true),
              "Type.isObject" -> Data(false),
              "Type.isVal" -> Data(false),
              "Type.isCaseClass" -> Data(true),
              "Type.isCaseObject" -> Data(false),
              "Type.isCaseVal" -> Data(false),
              "Type.isAvailable(Everywhere)" -> Data(true),
              "Type.isAvailable(AtCallSite)" -> Data(true)
            ),
            "ExampleSealedTraitObject" -> Data.map(
              "Type.isSealed" -> Data(false),
              "Type.isJavaEnum" -> Data(false),
              "Type.isJavaEnumValue" -> Data(false),
              "Type.isEnumeration" -> Data(false),
              "Type.isCase" -> Data(true),
              "Type.isObject" -> Data(true),
              "Type.isVal" -> Data(false),
              "Type.isCaseClass" -> Data(false),
              "Type.isCaseObject" -> Data(true),
              "Type.isCaseVal" -> Data(false),
              "Type.isAvailable(Everywhere)" -> Data(true),
              "Type.isAvailable(AtCallSite)" -> Data(true)
            )
          )
          testChildrenFlags[examples.enums.ExampleSealedTraitWithTypeParam[Int]] <==> Data.map(
            "ExampleSealedTraitWithTypeParamClass" -> Data.map(
              "Type.isSealed" -> Data(false),
              "Type.isJavaEnum" -> Data(false),
              "Type.isJavaEnumValue" -> Data(false),
              "Type.isEnumeration" -> Data(false),
              "Type.isCase" -> Data(true),
              "Type.isObject" -> Data(false),
              "Type.isVal" -> Data(false),
              "Type.isCaseClass" -> Data(true),
              "Type.isCaseObject" -> Data(false),
              "Type.isCaseVal" -> Data(false),
              "Type.isAvailable(Everywhere)" -> Data(true),
              "Type.isAvailable(AtCallSite)" -> Data(true)
            ),
            "ExampleSealedTraitWithTypeParamObject" -> Data.map(
              "Type.isSealed" -> Data(false),
              "Type.isJavaEnum" -> Data(false),
              "Type.isJavaEnumValue" -> Data(false),
              "Type.isEnumeration" -> Data(false),
              "Type.isCase" -> Data(true),
              "Type.isObject" -> Data(true),
              "Type.isVal" -> Data(false),
              "Type.isCaseClass" -> Data(false),
              "Type.isCaseObject" -> Data(true),
              "Type.isCaseVal" -> Data(false),
              "Type.isAvailable(Everywhere)" -> Data(true),
              "Type.isAvailable(AtCallSite)" -> Data(true)
            )
          )
          testChildrenFlags[examples.enums.ExampleSealedTraitGADT[Unit]] <==> Data.map(
            "ExampleSealedTraitWithTypeParamClass" -> Data.map(
              "Type.isSealed" -> Data(false),
              "Type.isJavaEnum" -> Data(false),
              "Type.isJavaEnumValue" -> Data(false),
              "Type.isEnumeration" -> Data(false),
              "Type.isCase" -> Data(true),
              "Type.isObject" -> Data(false),
              "Type.isVal" -> Data(false),
              "Type.isCaseClass" -> Data(true),
              "Type.isCaseObject" -> Data(false),
              "Type.isCaseVal" -> Data(false),
              "Type.isAvailable(Everywhere)" -> Data(true),
              "Type.isAvailable(AtCallSite)" -> Data(true)
            ),
            "ExampleSealedTraitWithTypeParamObject" -> Data.map(
              "Type.isSealed" -> Data(false),
              "Type.isJavaEnum" -> Data(false),
              "Type.isJavaEnumValue" -> Data(false),
              "Type.isEnumeration" -> Data(false),
              "Type.isCase" -> Data(true),
              "Type.isObject" -> Data(true),
              "Type.isVal" -> Data(false),
              "Type.isCaseClass" -> Data(false),
              "Type.isCaseObject" -> Data(true),
              "Type.isCaseVal" -> Data(false),
              "Type.isAvailable(Everywhere)" -> Data(true),
              "Type.isAvailable(AtCallSite)" -> Data(true)
            )
          )
          testChildrenFlags[examples.enums.ScopeVisibility] <==> Data.map(
            "Public" -> Data.map(
              "Type.isSealed" -> Data(false),
              "Type.isJavaEnum" -> Data(false),
              "Type.isJavaEnumValue" -> Data(false),
              "Type.isEnumeration" -> Data(false),
              "Type.isCase" -> Data(true),
              "Type.isObject" -> Data(true),
              "Type.isVal" -> Data(false),
              "Type.isCaseClass" -> Data(false),
              "Type.isCaseObject" -> Data(true),
              "Type.isCaseVal" -> Data(false),
              "Type.isAvailable(Everywhere)" -> Data(true),
              "Type.isAvailable(AtCallSite)" -> Data(true)
            ),
            "HearthPrivate" -> Data.map(
              "Type.isSealed" -> Data(false),
              "Type.isJavaEnum" -> Data(false),
              "Type.isJavaEnumValue" -> Data(false),
              "Type.isEnumeration" -> Data(false),
              "Type.isCase" -> Data(true),
              "Type.isObject" -> Data(true),
              "Type.isVal" -> Data(false),
              "Type.isCaseClass" -> Data(false),
              "Type.isCaseObject" -> Data(true),
              "Type.isCaseVal" -> Data(false),
              "Type.isAvailable(Everywhere)" -> Data(false),
              "Type.isAvailable(AtCallSite)" -> Data(true)
            ),
            "HearthExamplesPrivate" -> Data.map(
              "Type.isSealed" -> Data(false),
              "Type.isJavaEnum" -> Data(false),
              "Type.isJavaEnumValue" -> Data(false),
              "Type.isEnumeration" -> Data(false),
              "Type.isCase" -> Data(true),
              "Type.isObject" -> Data(true),
              "Type.isVal" -> Data(false),
              "Type.isCaseClass" -> Data(false),
              "Type.isCaseObject" -> Data(true),
              "Type.isCaseVal" -> Data(false),
              "Type.isAvailable(Everywhere)" -> Data(false),
              "Type.isAvailable(AtCallSite)" -> Data(false)
            )
          )
        }
      }

      group("methods: <:<, =:=, expected behavior") {
        import TypesFixtures.testComparisons

        test("for primitive types") {
          testComparisons[Boolean, Boolean] <==> Data.map(
            "<:<" -> Data(true),
            "=:=" -> Data(true)
          )
          testComparisons[Boolean, true] <==> Data.map(
            "<:<" -> Data(false),
            "=:=" -> Data(false)
          )
          testComparisons[true, Boolean] <==> Data.map(
            "<:<" -> Data(true),
            "=:=" -> Data(false)
          )

          testComparisons[Byte, Byte] <==> Data.map(
            "<:<" -> Data(true),
            "=:=" -> Data(true)
          )
          @scala.annotation.unused
          type Byte1 = 1 with Byte // force treating it as Byte literal type
          testComparisons[Byte, Byte1] <==> Data.map(
            "<:<" -> Data(false),
            "=:=" -> Data(false)
          )
          testComparisons[Byte1, Byte] <==> Data.map(
            "<:<" -> Data(true),
            "=:=" -> Data(false)
          )

          testComparisons[Short, Short] <==> Data.map(
            "<:<" -> Data(true),
            "=:=" -> Data(true)
          )
          @scala.annotation.unused
          type Short1 = 1 with Short // force treating it as Short literal type
          testComparisons[Short, Short1] <==> Data.map(
            "<:<" -> Data(false),
            "=:=" -> Data(false)
          )
          testComparisons[Short1, Short] <==> Data.map(
            "<:<" -> Data(true),
            "=:=" -> Data(false)
          )

          testComparisons[Int, Int] <==> Data.map(
            "<:<" -> Data(true),
            "=:=" -> Data(true)
          )
          testComparisons[Int, 1] <==> Data.map(
            "<:<" -> Data(false),
            "=:=" -> Data(false)
          )
          testComparisons[1, Int] <==> Data.map(
            "<:<" -> Data(true),
            "=:=" -> Data(false)
          )

          testComparisons[Long, Long] <==> Data.map(
            "<:<" -> Data(true),
            "=:=" -> Data(true)
          )
          testComparisons[Long, 1L] <==> Data.map(
            "<:<" -> Data(false),
            "=:=" -> Data(false)
          )
          testComparisons[1L, Long] <==> Data.map(
            "<:<" -> Data(true),
            "=:=" -> Data(false)
          )

          testComparisons[Float, Float] <==> Data.map(
            "<:<" -> Data(true),
            "=:=" -> Data(true)
          )
          testComparisons[Float, 1.0f] <==> Data.map(
            "<:<" -> Data(false),
            "=:=" -> Data(false)
          )
          testComparisons[1.0f, Float] <==> Data.map(
            "<:<" -> Data(true),
            "=:=" -> Data(false)
          )

          testComparisons[Double, Double] <==> Data.map(
            "<:<" -> Data(true),
            "=:=" -> Data(true)
          )
          testComparisons[Double, 1.0] <==> Data.map(
            "<:<" -> Data(false),
            "=:=" -> Data(false)
          )
          testComparisons[1.0, Double] <==> Data.map(
            "<:<" -> Data(true),
            "=:=" -> Data(false)
          )

          testComparisons[Char, Char] <==> Data.map(
            "<:<" -> Data(true),
            "=:=" -> Data(true)
          )
          testComparisons[Char, 'a'] <==> Data.map(
            "<:<" -> Data(false),
            "=:=" -> Data(false)
          )
          testComparisons['a', Char] <==> Data.map(
            "<:<" -> Data(true),
            "=:=" -> Data(false)
          )
        }

        test("for built-in types") {
          testComparisons[String, String] <==> Data.map(
            "<:<" -> Data(true),
            "=:=" -> Data(true)
          )
          testComparisons[String, "a"] <==> Data.map(
            "<:<" -> Data(false),
            "=:=" -> Data(false)
          )
          testComparisons["a", String] <==> Data.map(
            "<:<" -> Data(true),
            "=:=" -> Data(false)
          )

          testComparisons[Unit, Unit] <==> Data.map(
            "<:<" -> Data(true),
            "=:=" -> Data(true)
          )

          testComparisons[Array[Int], Array[Int]] <==> Data.map(
            "<:<" -> Data(true),
            "=:=" -> Data(true)
          )
          testComparisons[Array[Int], Array[1]] <==> Data.map(
            "<:<" -> Data(false),
            "=:=" -> Data(false)
          )
          testComparisons[Array[1], Array[Int]] <==> Data.map(
            "<:<" -> Data(false),
            "=:=" -> Data(false)
          )
        }

        test("for top-level classes (non-sealed)") {
          testComparisons[examples.classes.ExampleTrait, examples.classes.ExampleTrait] <==> Data.map(
            "<:<" -> Data(true),
            "=:=" -> Data(true)
          )

          testComparisons[examples.classes.ExampleTraitWithTypeParam[Int], examples.classes.ExampleTraitWithTypeParam[
            Int
          ]] <==> Data.map(
            "<:<" -> Data(true),
            "=:=" -> Data(true)
          )
          testComparisons[examples.classes.ExampleTraitWithTypeParam[Int], examples.classes.ExampleTraitWithTypeParam[
            Float
          ]] <==> Data.map(
            "<:<" -> Data(false),
            "=:=" -> Data(false)
          )

          testComparisons[examples.classes.ExampleAbstractClass, examples.classes.ExampleAbstractClass] <==> Data.map(
            "<:<" -> Data(true),
            "=:=" -> Data(true)
          )

          testComparisons[examples.classes.ExampleAbstractClassWithTypeParam[
            Int
          ], examples.classes.ExampleAbstractClassWithTypeParam[Int]] <==> Data.map(
            "<:<" -> Data(true),
            "=:=" -> Data(true)
          )
          testComparisons[examples.classes.ExampleAbstractClassWithTypeParam[
            Int
          ], examples.classes.ExampleAbstractClassWithTypeParam[Float]] <==> Data.map(
            "<:<" -> Data(false),
            "=:=" -> Data(false)
          )

          testComparisons[examples.classes.ExampleClass, examples.classes.ExampleClass] <==> Data.map(
            "<:<" -> Data(true),
            "=:=" -> Data(true)
          )
          testComparisons[examples.classes.ExampleClass, examples.classes.ExampleClassWithTypeParam[Int]] <==> Data.map(
            "<:<" -> Data(false),
            "=:=" -> Data(false)
          )

          testComparisons[examples.classes.ExampleCaseClass, examples.classes.ExampleCaseClass] <==> Data.map(
            "<:<" -> Data(true),
            "=:=" -> Data(true)
          )
          testComparisons[examples.classes.ExampleCaseClass, examples.classes.ExampleCaseClassWithTypeParam[
            Int
          ]] <==> Data
            .map(
              "<:<" -> Data(false),
              "=:=" -> Data(false)
            )

          testComparisons[examples.classes.ExampleCaseClassWithTypeParam[
            Int
          ], examples.classes.ExampleCaseClassWithTypeParam[Int]] <==> Data.map(
            "<:<" -> Data(true),
            "=:=" -> Data(true)
          )
          testComparisons[examples.classes.ExampleCaseClassWithTypeParam[
            Int
          ], examples.classes.ExampleCaseClassWithTypeParam[Float]] <==> Data.map(
            "<:<" -> Data(false),
            "=:=" -> Data(false)
          )
        }

        test("for inner classes (non-sealed)") {
          @unused @nowarn
          trait ExampleTrait
          @unused @nowarn
          trait ExampleTraitWithTypeParam[A]

          @unused @nowarn
          abstract class ExampleAbstractClass
          @unused @nowarn
          abstract class ExampleAbstractClassWithTypeParam[A]

          @unused @nowarn
          final class ExampleClass
          @unused @nowarn
          final class ExampleClassWithTypeParam[A]

          @unused @nowarn
          case class ExampleCaseClass(a: Int)
          @unused @nowarn
          case class ExampleCaseClassWithTypeParam[A](a: A)

          testComparisons[ExampleTrait, ExampleTrait] <==> Data.map(
            "<:<" -> Data(true),
            "=:=" -> Data(true)
          )

          testComparisons[ExampleTraitWithTypeParam[Int], ExampleTraitWithTypeParam[Int]] <==> Data.map(
            "<:<" -> Data(true),
            "=:=" -> Data(true)
          )
          testComparisons[ExampleTraitWithTypeParam[Int], ExampleTraitWithTypeParam[Float]] <==> Data.map(
            "<:<" -> Data(false),
            "=:=" -> Data(false)
          )

          testComparisons[ExampleAbstractClass, ExampleAbstractClass] <==> Data.map(
            "<:<" -> Data(true),
            "=:=" -> Data(true)
          )

          testComparisons[ExampleAbstractClassWithTypeParam[Int], ExampleAbstractClassWithTypeParam[Int]] <==> Data.map(
            "<:<" -> Data(true),
            "=:=" -> Data(true)
          )
          testComparisons[ExampleAbstractClassWithTypeParam[Int], ExampleAbstractClassWithTypeParam[Float]] <==> Data
            .map(
              "<:<" -> Data(false),
              "=:=" -> Data(false)
            )

          testComparisons[ExampleClass, ExampleClass] <==> Data.map(
            "<:<" -> Data(true),
            "=:=" -> Data(true)
          )
          testComparisons[ExampleClass, ExampleClassWithTypeParam[Int]] <==> Data.map(
            "<:<" -> Data(false),
            "=:=" -> Data(false)
          )

          testComparisons[ExampleCaseClass, ExampleCaseClass] <==> Data.map(
            "<:<" -> Data(true),
            "=:=" -> Data(true)
          )
          testComparisons[ExampleCaseClass, ExampleCaseClassWithTypeParam[Int]] <==> Data
            .map(
              "<:<" -> Data(false),
              "=:=" -> Data(false)
            )

          testComparisons[ExampleCaseClassWithTypeParam[Int], ExampleCaseClassWithTypeParam[Int]] <==> Data.map(
            "<:<" -> Data(true),
            "=:=" -> Data(true)
          )
          testComparisons[ExampleCaseClassWithTypeParam[Int], ExampleCaseClassWithTypeParam[Float]] <==> Data.map(
            "<:<" -> Data(false),
            "=:=" -> Data(false)
          )
        }

        test("for enumerations") {
          testComparisons[examples.enums.WeekDay.Value, examples.enums.WeekDay.Value] <==> Data.map(
            "<:<" -> Data(true),
            "=:=" -> Data(true)
          )
          testComparisons[examples.enums.Planet.Value, examples.enums.Planet.Value] <==> Data.map(
            "<:<" -> Data(true),
            "=:=" -> Data(true)
          )
          testComparisons[examples.enums.WeekDay.Value, examples.enums.Planet.Value] <==> Data.map(
            "<:<" -> Data(false),
            "=:=" -> Data(false)
          )
        }

        test("for top-level classes (sealed)") {
          testComparisons[examples.enums.ExampleSealedTrait, examples.enums.ExampleSealedTrait] <==> Data.map(
            "<:<" -> Data(true),
            "=:=" -> Data(true)
          )
          testComparisons[
            examples.enums.ExampleSealedTrait,
            examples.enums.ExampleSealedTrait.ExampleSealedTraitClass
          ] <==> Data.map(
            "<:<" -> Data(false),
            "=:=" -> Data(false)
          )
          testComparisons[
            examples.enums.ExampleSealedTrait.ExampleSealedTraitClass,
            examples.enums.ExampleSealedTrait
          ] <==> Data.map(
            "<:<" -> Data(true),
            "=:=" -> Data(false)
          )
          testComparisons[
            examples.enums.ExampleSealedTrait,
            examples.enums.ExampleSealedTrait.ExampleSealedTraitObject.type
          ] <==> Data.map(
            "<:<" -> Data(false),
            "=:=" -> Data(false)
          )
          testComparisons[
            examples.enums.ExampleSealedTrait.ExampleSealedTraitObject.type,
            examples.enums.ExampleSealedTrait
          ] <==> Data.map(
            "<:<" -> Data(true),
            "=:=" -> Data(false)
          )

          testComparisons[
            examples.enums.ExampleSealedTraitWithTypeParam[Int],
            examples.enums.ExampleSealedTraitWithTypeParam[Int]
          ] <==> Data.map(
            "<:<" -> Data(true),
            "=:=" -> Data(true)
          )
          testComparisons[
            examples.enums.ExampleSealedTraitWithTypeParam[Int],
            examples.enums.ExampleSealedTraitWithTypeParam[Float]
          ] <==> Data.map(
            "<:<" -> Data(false),
            "=:=" -> Data(false)
          )
          testComparisons[
            examples.enums.ExampleSealedTraitWithTypeParam[Int],
            examples.enums.ExampleSealedTraitWithTypeParam.ExampleSealedTraitWithTypeParamClass[Int]
          ] <==> Data.map(
            "<:<" -> Data(false),
            "=:=" -> Data(false)
          )
          testComparisons[
            examples.enums.ExampleSealedTraitWithTypeParam.ExampleSealedTraitWithTypeParamClass[Int],
            examples.enums.ExampleSealedTraitWithTypeParam[Int]
          ] <==> Data.map(
            "<:<" -> Data(true),
            "=:=" -> Data(false)
          )
          testComparisons[
            examples.enums.ExampleSealedTraitWithTypeParam[Int],
            examples.enums.ExampleSealedTraitWithTypeParam.ExampleSealedTraitWithTypeParamObject.type
          ] <==> Data.map(
            "<:<" -> Data(false),
            "=:=" -> Data(false)
          )
          testComparisons[
            examples.enums.ExampleSealedTraitWithTypeParam.ExampleSealedTraitWithTypeParamObject.type,
            examples.enums.ExampleSealedTraitWithTypeParam[Int]
          ] <==> Data.map(
            "<:<" -> Data(true),
            "=:=" -> Data(false)
          )

          testComparisons[
            examples.enums.ExampleSealedTraitGADT[Unit],
            examples.enums.ExampleSealedTraitGADT[Unit]
          ] <==> Data.map(
            "<:<" -> Data(true),
            "=:=" -> Data(true)
          )
          testComparisons[
            examples.enums.ExampleSealedTraitGADT[Unit],
            examples.enums.ExampleSealedTraitGADT[String]
          ] <==> Data.map(
            "<:<" -> Data(false),
            "=:=" -> Data(false)
          )
          testComparisons[
            examples.enums.ExampleSealedTraitGADT[String],
            examples.enums.ExampleSealedTraitGADT.ExampleSealedTraitWithTypeParamClass
          ] <==> Data.map(
            "<:<" -> Data(false),
            "=:=" -> Data(false)
          )
          testComparisons[
            examples.enums.ExampleSealedTraitGADT.ExampleSealedTraitWithTypeParamClass,
            examples.enums.ExampleSealedTraitGADT[String]
          ] <==> Data.map(
            "<:<" -> Data(true),
            "=:=" -> Data(false)
          )
          testComparisons[
            examples.enums.ExampleSealedTraitGADT[String],
            examples.enums.ExampleSealedTraitGADT.ExampleSealedTraitWithTypeParamClass
          ] <==> Data.map(
            "<:<" -> Data(false),
            "=:=" -> Data(false)
          )
          testComparisons[
            examples.enums.ExampleSealedTraitGADT.ExampleSealedTraitWithTypeParamClass,
            examples.enums.ExampleSealedTraitGADT[String]
          ] <==> Data.map(
            "<:<" -> Data(true),
            "=:=" -> Data(false)
          )
        }
      }
    }

    group("type TypeCodec") {

      test(
        "methods TypeCodec.{toType, fromType} should allow converting between types and values for types supporting bidirectional transformation"
      ) {
        import TypesFixtures.testBidirectionalCodecs

        testBidirectionalCodecs <==> Data.map(
          "null" -> Data.map(
            "encoded" -> Data("null"),
            "decoded" -> Data("null")
          ),
          "unit" -> Data.map(
            "encoded" -> Data("()"),
            "decoded" -> Data("()")
          ),
          "boolean" -> Data.map(
            "encoded" -> Data("true"),
            "decoded" -> Data("true")
          ),
          "byte" -> Data.map(
            "encoded" -> Data("1"),
            "decoded" -> Data("1")
          ),
          "short" -> Data.map(
            "encoded" -> Data("1"),
            "decoded" -> Data("1")
          ),
          "int" -> Data.map(
            "encoded" -> Data("1"),
            "decoded" -> Data("1")
          ),
          "long" -> Data.map(
            "encoded" -> Data("1L"),
            "decoded" -> Data("1")
          ),
          "float" -> Data.map(
            "encoded" -> Data("1.0f"),
            "decoded" -> Data("1.0")
          ),
          "double" -> Data.map(
            "encoded" -> Data("1.0"),
            "decoded" -> Data("1.0")
          ),
          "char" -> Data.map(
            "encoded" -> Data("'a'"),
            "decoded" -> Data("a")
          ),
          "string" -> Data.map(
            "encoded" -> Data("\"a\""),
            "decoded" -> Data("a")
          ),
          "module" -> Data.map(
            "encoded" -> Data("scala.Predef.type"),
            "decoded" -> Data("scala.Predef$")
          ),
          "Tuple1(1)" -> Data.map(
            "encoded" -> Data("scala.Tuple1[1]"),
            "decoded" -> Data("(1)")
          ),
          "(1, a)" -> Data.map(
            "encoded" -> Data("scala.Tuple2[1, \"a\"]"),
            "decoded" -> Data("(1,a)")
          ),
          "(1, a, true)" -> Data.map(
            "encoded" -> Data("scala.Tuple3[1, \"a\", true]"),
            "decoded" -> Data("(1,a,true)")
          ),
          "(1, a, true, 2L)" -> Data.map(
            "encoded" -> Data("scala.Tuple4[1, \"a\", true, 2L]"),
            "decoded" -> Data("(1,a,true,2)")
          ),
          "(1, a, true, 2L, 3)" -> Data.map(
            "encoded" -> Data("scala.Tuple5[1, \"a\", true, 2L, 3]"),
            "decoded" -> Data("(1,a,true,2,3)")
          ),
          "(1, a, true, 2L, 3, b)" -> Data.map(
            "encoded" -> Data("scala.Tuple6[1, \"a\", true, 2L, 3, \"b\"]"),
            "decoded" -> Data("(1,a,true,2,3,b)")
          ),
          "(1, a, true, 2L, 3, b, false)" -> Data.map(
            "encoded" -> Data("scala.Tuple7[1, \"a\", true, 2L, 3, \"b\", false]"),
            "decoded" -> Data("(1,a,true,2,3,b,false)")
          ),
          "(1, a, true, 2L, 3, b, false, 4L)" -> Data.map(
            "encoded" -> Data("scala.Tuple8[1, \"a\", true, 2L, 3, \"b\", false, 4L]"),
            "decoded" -> Data("(1,a,true,2,3,b,false,4)")
          ),
          "(1, a, true, 2L, 3, b, false, 4L, 5)" -> Data.map(
            "encoded" -> Data("scala.Tuple9[1, \"a\", true, 2L, 3, \"b\", false, 4L, 5]"),
            "decoded" -> Data("(1,a,true,2,3,b,false,4,5)")
          ),
          "(1, a, true, 2L, 3, b, false, 4L, 5, c)" -> Data.map(
            "encoded" -> Data("scala.Tuple10[1, \"a\", true, 2L, 3, \"b\", false, 4L, 5, \"c\"]"),
            "decoded" -> Data("(1,a,true,2,3,b,false,4,5,c)")
          ),
          "(1, a, true, 2L, 3, b, false, 4L, 5, c, true)" -> Data.map(
            "encoded" -> Data("scala.Tuple11[1, \"a\", true, 2L, 3, \"b\", false, 4L, 5, \"c\", true]"),
            "decoded" -> Data("(1,a,true,2,3,b,false,4,5,c,true)")
          ),
          "(1, a, true, 2L, 3, b, false, 4L, 5, c, true, 6L)" -> Data.map(
            "encoded" -> Data("scala.Tuple12[1, \"a\", true, 2L, 3, \"b\", false, 4L, 5, \"c\", true, 6L]"),
            "decoded" -> Data("(1,a,true,2,3,b,false,4,5,c,true,6)")
          ),
          "(1, a, true, 2L, 3, b, false, 4L, 5, c, true, 6L, 7)" -> Data.map(
            "encoded" -> Data("scala.Tuple13[1, \"a\", true, 2L, 3, \"b\", false, 4L, 5, \"c\", true, 6L, 7]"),
            "decoded" -> Data("(1,a,true,2,3,b,false,4,5,c,true,6,7)")
          ),
          "(1, a, true, 2L, 3, b, false, 4L, 5, c, true, 6L, 7, d)" -> Data.map(
            "encoded" -> Data(
              "scala.Tuple14[1, \"a\", true, 2L, 3, \"b\", false, 4L, 5, \"c\", true, 6L, 7, \"d\"]"
            ),
            "decoded" -> Data("(1,a,true,2,3,b,false,4,5,c,true,6,7,d)")
          ),
          "(1, a, true, 2L, 3, b, false, 4L, 5, c, true, 6L, 7, d, false)" -> Data.map(
            "encoded" -> Data(
              "scala.Tuple15[1, \"a\", true, 2L, 3, \"b\", false, 4L, 5, \"c\", true, 6L, 7, \"d\", false]"
            ),
            "decoded" -> Data("(1,a,true,2,3,b,false,4,5,c,true,6,7,d,false)")
          ),
          "(1, a, true, 2L, 3, b, false, 4L, 5, c, true, 6L, 7, d, false, 8L)" -> Data.map(
            "encoded" -> Data(
              "scala.Tuple16[1, \"a\", true, 2L, 3, \"b\", false, 4L, 5, \"c\", true, 6L, 7, \"d\", false, 8L]"
            ),
            "decoded" -> Data("(1,a,true,2,3,b,false,4,5,c,true,6,7,d,false,8)")
          ),
          "(1, a, true, 2L, 3, b, false, 4L, 5, c, true, 6L, 7, d, false, 8L, 9)" -> Data.map(
            "encoded" -> Data(
              "scala.Tuple17[1, \"a\", true, 2L, 3, \"b\", false, 4L, 5, \"c\", true, 6L, 7, \"d\", false, 8L, 9]"
            ),
            "decoded" -> Data("(1,a,true,2,3,b,false,4,5,c,true,6,7,d,false,8,9)")
          ),
          "(1, a, true, 2L, 3, b, false, 4L, 5, c, true, 6L, 7, d, false, 8L, 9, e)" -> Data.map(
            "encoded" -> Data(
              "scala.Tuple18[1, \"a\", true, 2L, 3, \"b\", false, 4L, 5, \"c\", true, 6L, 7, \"d\", false, 8L, 9, \"e\"]"
            ),
            "decoded" -> Data("(1,a,true,2,3,b,false,4,5,c,true,6,7,d,false,8,9,e)")
          ),
          "(1, a, true, 2L, 3, b, false, 4L, 5, c, true, 6L, 7, d, false, 8L, 9, e, true)" -> Data.map(
            "encoded" -> Data(
              "scala.Tuple19[1, \"a\", true, 2L, 3, \"b\", false, 4L, 5, \"c\", true, 6L, 7, \"d\", false, 8L, 9, \"e\", true]"
            ),
            "decoded" -> Data("(1,a,true,2,3,b,false,4,5,c,true,6,7,d,false,8,9,e,true)")
          ),
          "(1, a, true, 2L, 3, b, false, 4L, 5, c, true, 6L, 7, d, false, 8L, 9, e, true, 10L)" -> Data.map(
            "encoded" -> Data(
              "scala.Tuple20[1, \"a\", true, 2L, 3, \"b\", false, 4L, 5, \"c\", true, 6L, 7, \"d\", false, 8L, 9, \"e\", true, 10L]"
            ),
            "decoded" -> Data("(1,a,true,2,3,b,false,4,5,c,true,6,7,d,false,8,9,e,true,10)")
          ),
          "(1, a, true, 2L, 3, b, false, 4L, 5, c, true, 6L, 7, d, false, 8L, 9, e, true, 10L, 11)" -> Data.map(
            "encoded" -> Data(
              "scala.Tuple21[1, \"a\", true, 2L, 3, \"b\", false, 4L, 5, \"c\", true, 6L, 7, \"d\", false, 8L, 9, \"e\", true, 10L, 11]"
            ),
            "decoded" -> Data("(1,a,true,2,3,b,false,4,5,c,true,6,7,d,false,8,9,e,true,10,11)")
          ),
          "(1, a, true, 2L, 3, b, false, 4L, 5, c, true, 6L, 7, d, false, 8L, 9, e, true, 10L, 11, f)" -> Data.map(
            "encoded" -> Data(
              "scala.Tuple22[1, \"a\", true, 2L, 3, \"b\", false, 4L, 5, \"c\", true, 6L, 7, \"d\", false, 8L, 9, \"e\", true, 10L, 11, \"f\"]"
            ),
            "decoded" -> Data("(1,a,true,2,3,b,false,4,5,c,true,6,7,d,false,8,9,e,true,10,11,f)")
          ),
          "Some(1)" -> Data.map(
            "encoded" -> Data("scala.Some[1]"),
            "decoded" -> Data("Some(1)")
          ),
          "None" -> Data.map(
            "encoded" -> Data("scala.None.type"),
            "decoded" -> Data("None")
          ),
          "Left(1)" -> Data.map(
            "encoded" -> Data("scala.util.Left[1, java.lang.String]"),
            "decoded" -> Data("Left(1)")
          ),
          "Right(a)" -> Data.map(
            "encoded" -> Data("scala.util.Right[scala.Int, \"a\"]"),
            "decoded" -> Data("Right(a)")
          )
        )
      }

      test(
        "methods TypeCodec.{toType, fromType} should allow converting between types and values for types supporting one-way transformation"
      ) {
        import TypesFixtures.testOneWayCodecs

        testOneWayCodecs <==> Data.map(
          """Array["a"]""" -> Data.map(
            "encoded" -> Data("scala.Array[\"a\"]")
          ),
          """Seq["a"]""" -> Data.map(
            "encoded" -> Data("scala.collection.immutable.Seq[\"a\"]")
          ),
          """List["a"]""" -> Data.map(
            "encoded" -> Data("scala.collection.immutable.List[\"a\"]")
          ),
          "Nil" -> Data.map(
            "encoded" -> Data("scala.Nil.type")
          ),
          """Vector["a"]""" -> Data.map(
            "encoded" -> Data("scala.collection.immutable.Vector[\"a\"]")
          ),
          """Map["a", "b"]""" -> Data.map(
            "encoded" -> Data("scala.collection.immutable.Map[\"a\", \"b\"]")
          ),
          """Set["a"]""" -> Data.map(
            "encoded" -> Data("scala.collection.immutable.Set[\"a\"]")
          ),
          """Option["a"]""" -> Data.map(
            "encoded" -> Data("scala.Some[\"a\"]")
          ),
          """Some["a"]""" -> Data.map(
            "encoded" -> Data("scala.Some[\"a\"]")
          ),
          "None" -> Data.map(
            "encoded" -> Data("scala.None.type")
          ),
          """Either["a", "b"]""" -> Data.map(
            "encoded" -> Data("scala.util.Left[\"a\", \"b\"]")
          ),
          """Left["a", "b"]""" -> Data.map(
            "encoded" -> Data("scala.util.Left[\"a\", \"b\"]")
          ),
          """Right["a", "b"]""" -> Data.map(
            "encoded" -> Data("scala.util.Right[\"a\", \"b\"]")
          ),
          """Class["a"]""" -> Data.map(
            "encoded" -> Data("java.lang.Class[\"a\"]")
          ),
          """ClassTag["a"]""" -> Data.map(
            "encoded" -> Data("scala.reflect.ClassTag[\"a\"]")
          ),
          """Tuple1["a"]""" -> Data.map(
            "encoded" -> Data("scala.Tuple1[\"a\"]")
          ),
          """("a", "b")""" -> Data.map(
            "encoded" -> Data("scala.Tuple2[\"a\", \"b\"]")
          ),
          """("a", "b", "a")""" -> Data.map(
            "encoded" -> Data("scala.Tuple3[\"a\", \"b\", \"a\"]")
          ),
          """("a", "b", "a", "b")""" -> Data.map(
            "encoded" -> Data("scala.Tuple4[\"a\", \"b\", \"a\", \"b\"]")
          ),
          """("a", "b", "a", "b", "a")""" -> Data.map(
            "encoded" -> Data("scala.Tuple5[\"a\", \"b\", \"a\", \"b\", \"a\"]")
          ),
          """("a", "b", "a", "b", "a", "b")""" -> Data.map(
            "encoded" -> Data("scala.Tuple6[\"a\", \"b\", \"a\", \"b\", \"a\", \"b\"]")
          ),
          """("a", "b", "a", "b", "a", "b", "a")""" -> Data.map(
            "encoded" -> Data("scala.Tuple7[\"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\"]")
          ),
          """("a", "b", "a", "b", "a", "b", "a", "b")""" -> Data.map(
            "encoded" -> Data("scala.Tuple8[\"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\", \"b\"]")
          ),
          """("a", "b", "a", "b", "a", "b", "a", "b", "a")""" -> Data.map(
            "encoded" -> Data("scala.Tuple9[\"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\"]")
          ),
          """("a", "b", "a", "b", "a", "b", "a", "b", "a", "b")""" -> Data.map(
            "encoded" -> Data(
              "scala.Tuple10[\"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\", \"b\"]"
            )
          ),
          """("a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a")""" -> Data.map(
            "encoded" -> Data(
              "scala.Tuple11[\"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\"]"
            )
          ),
          """("a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b")""" -> Data.map(
            "encoded" -> Data(
              "scala.Tuple12[\"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\", \"b\"]"
            )
          ),
          """("a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a")""" -> Data.map(
            "encoded" -> Data(
              "scala.Tuple13[\"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\"]"
            )
          ),
          """("a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b")""" -> Data.map(
            "encoded" -> Data(
              "scala.Tuple14[\"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\", \"b\"]"
            )
          ),
          """("a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a")""" -> Data.map(
            "encoded" -> Data(
              "scala.Tuple15[\"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\"]"
            )
          ),
          """("a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b")""" -> Data.map(
            "encoded" -> Data(
              "scala.Tuple16[\"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\", \"b\"]"
            )
          ),
          """("a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a")""" -> Data.map(
            "encoded" -> Data(
              "scala.Tuple17[\"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\"]"
            )
          ),
          """("a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b")""" -> Data
            .map(
              "encoded" -> Data(
                "scala.Tuple18[\"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\", \"b\"]"
              )
            ),
          """("a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a")""" -> Data
            .map(
              "encoded" -> Data(
                "scala.Tuple19[\"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\"]"
              )
            ),
          """("a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b")""" -> Data
            .map(
              "encoded" -> Data(
                "scala.Tuple20[\"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\", \"b\"]"
              )
            ),
          """("a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a")""" -> Data
            .map(
              "encoded" -> Data(
                "scala.Tuple21[\"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\"]"
              )
            ),
          """("a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b")""" -> Data
            .map(
              "encoded" -> Data(
                "scala.Tuple22[\"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\", \"b\", \"a\", \"b\"]"
              )
            )
        )
      }
    }
  }
}
