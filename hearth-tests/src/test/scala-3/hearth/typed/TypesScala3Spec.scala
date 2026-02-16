package hearth
package typed

import hearth.data.Data
import hearth.Tags

import scala.annotation.unused

/** Macro implementation is in [[TypesFixturesImpl]] */
final class TypesScala3Spec extends MacroSuite {

  group("typed.Types") {

    group("type Type") {

      group("methods: Type.{simple, fqcn, plainPrint, prettyPrint}, expected behavior") {
        import TypesFixtures.testNamesPrinters

        test("for Scala 3 enums") {
          List(
            testNamesPrinters[examples.ExampleEnum] -> (
              "ExampleEnum",
              "hearth.examples.ExampleEnum",
              "hearth.examples.ExampleEnum"
            ),
            testNamesPrinters[examples.ExampleEnumWithTypeParam[String]] -> (
              "ExampleEnumWithTypeParam",
              "hearth.examples.ExampleEnumWithTypeParam",
              "hearth.examples.ExampleEnumWithTypeParam[java.lang.String]"
            ),
            testNamesPrinters[examples.ExampleEnumGADT[String]] -> (
              "ExampleEnumGADT",
              "hearth.examples.ExampleEnumGADT",
              "hearth.examples.ExampleEnumGADT[java.lang.String]"
            )
          ).foreach { case (actual, (shortName, fqcn, fullName)) =>
            actual <==> Data(
              Map(
                "Type.shortName" -> Data(shortName),
                "Type.fqcn" -> Data(fqcn),
                "Type.plainPrint" -> Data(fullName),
                "Type.prettyPrint" -> Data(fullName)
              )
            )
          }
        }

        test("for Scala 3 enum cases") {
          List(
            testNamesPrinters[examples.ExampleEnum.ExampleEnumClass] -> (
              "ExampleEnumClass",
              "hearth.examples.ExampleEnum.ExampleEnumClass",
              "hearth.examples.ExampleEnum.ExampleEnumClass"
            ),
            testNamesPrinters[examples.ExampleEnum.ExampleEnumValue.type] -> (
              "ExampleEnumValue",
              "hearth.examples.ExampleEnum.ExampleEnumValue.type",
              "hearth.examples.ExampleEnum.ExampleEnumValue.type"
            )
          ).foreach { case (actual, (shortName, fqcn, fullName)) =>
            actual <==> Data(
              Map(
                "Type.shortName" -> Data(shortName),
                "Type.fqcn" -> Data(fqcn),
                "Type.plainPrint" -> Data(fullName),
                "Type.prettyPrint" -> Data(fullName)
              )
            )
          }
        }
      }

      group("methods: Type.{classOfType} expected behavior") {
        import TypesFixtures.testClassOfType

        test("for Scala 3 enums".tag(Tags.recompileFlaky)) {
          testClassOfType[examples.ExampleEnum] <==> Data.map(
            "Type.classOfType" -> Data(classOf[examples.ExampleEnum].toString)
          )
          testClassOfType[examples.ExampleEnumWithTypeParam[String]] <==> Data.map(
            "Type.classOfType" -> Data(classOf[examples.ExampleEnumWithTypeParam[String]].toString)
          )
          testClassOfType[examples.ExampleEnumGADT[String]] <==> Data.map(
            "Type.classOfType" -> Data(classOf[examples.ExampleEnumGADT[String]].toString)
          )
        }

        test("for Scala 3 enum cases".tag(Tags.recompileFlaky)) {
          testClassOfType[examples.ExampleEnum.ExampleEnumClass] <==> Data.map(
            "Type.classOfType" -> Data(classOf[examples.ExampleEnum.ExampleEnumClass].toString)
          )
        }
      }

      group("methods: Type.{position} expected behavior") {
        import TypesFixtures.testPosition
        val isPositionTrimmed =
          testPosition[examples.ExampleEnum] == Data.map("Type.position" -> Data("enums-s3.scala.scala:1:1"))
        def enumS3Position(line: Int, column: Int): String =
          if isPositionTrimmed then "enums-s3.scala.scala:1:1" else s"enums-s3.scala.scala:$line:$column"

        test("for Scala 3 enums".tag(Tags.langVerMismatch)) {
          testPosition[examples.ExampleEnum] <==> Data.map("Type.position" -> Data(enumS3Position(4, 6)))
          testPosition[examples.ExampleEnumWithTypeParam[String]] <==> Data.map(
            "Type.position" -> Data(enumS3Position(9, 6))
          )
          testPosition[examples.ExampleEnumGADT[String]] <==> Data.map("Type.position" -> Data(enumS3Position(14, 6)))
        }

        test("for Scala 3 enum cases".tag(Tags.langVerMismatch)) {
          testPosition[examples.ExampleEnum.ExampleEnumClass] <==> Data.map(
            "Type.position" -> Data(enumS3Position(5, 8))
          )
        }
      }

      group("methods: Type.{directChildren, exhaustiveChildren} expected behavior") {
        import TypesFixtures.testChildren

        test("for Scala 3 enums") {
          testChildren[examples.ExampleEnum] <==> Data.map(
            "Type.directChildren" -> Data.map(
              "ExampleEnumClass" -> Data("hearth.examples.ExampleEnum.ExampleEnumClass"),
              "ExampleEnumValue" -> Data("hearth.examples.ExampleEnum.ExampleEnumValue.type")
            ),
            "Type.exhaustiveChildren" -> Data.map(
              "ExampleEnumClass" -> Data("hearth.examples.ExampleEnum.ExampleEnumClass"),
              "ExampleEnumValue" -> Data("hearth.examples.ExampleEnum.ExampleEnumValue.type")
            )
          )
          testChildren[examples.ExampleEnumWithTypeParam[String]] <==> Data.map(
            "Type.directChildren" -> Data.map(
              "ExampleEnumWithTypeParamClass" -> Data(
                "hearth.examples.ExampleEnumWithTypeParam.ExampleEnumWithTypeParamClass[java.lang.String]"
              ),
              "ExampleEnumWithTypeParamValue" -> Data(
                "hearth.examples.ExampleEnumWithTypeParam.ExampleEnumWithTypeParamValue.type"
              )
            ),
            "Type.exhaustiveChildren" -> Data.map(
              "ExampleEnumWithTypeParamClass" -> Data(
                "hearth.examples.ExampleEnumWithTypeParam.ExampleEnumWithTypeParamClass[java.lang.String]"
              ),
              "ExampleEnumWithTypeParamValue" -> Data(
                "hearth.examples.ExampleEnumWithTypeParam.ExampleEnumWithTypeParamValue.type"
              )
            )
          )
          testChildren[examples.ExampleEnumGADT[String]] <==> Data.map(
            "Type.directChildren" -> Data.map(
              "ExampleEnumWithTypeParamClass" -> Data("hearth.examples.ExampleEnumGADT.ExampleEnumWithTypeParamClass"),
              "ExampleEnumWithTypeParamValue" -> Data(
                "hearth.examples.ExampleEnumGADT.ExampleEnumWithTypeParamValue.type"
              )
            ),
            "Type.exhaustiveChildren" -> Data.map(
              "ExampleEnumWithTypeParamClass" -> Data("hearth.examples.ExampleEnumGADT.ExampleEnumWithTypeParamClass")
            )
          )
          testChildren[examples.ExampleEnumGADT[Unit]] <==> Data.map(
            "Type.directChildren" -> Data.map(
              "ExampleEnumWithTypeParamClass" -> Data("hearth.examples.ExampleEnumGADT.ExampleEnumWithTypeParamClass"),
              "ExampleEnumWithTypeParamValue" -> Data(
                "hearth.examples.ExampleEnumGADT.ExampleEnumWithTypeParamValue.type"
              )
            ),
            "Type.exhaustiveChildren" -> Data.map(
              "ExampleEnumWithTypeParamValue" -> Data(
                "hearth.examples.ExampleEnumGADT.ExampleEnumWithTypeParamValue.type"
              )
            )
          )
        }

        test("for Scala 3 enum cases") {
          testChildren[examples.ExampleEnum.ExampleEnumClass] <==> Data.map(
            "Type.directChildren" -> Data("<no direct children>"),
            "Type.exhaustiveChildren" -> Data("<no exhaustive children>")
          )
        }
      }

      group(
        "methods: Type.{isPrimitive, isJvmBuiltIn, isAbstract, isFinal, isClass, isTuple, notJvmBuiltInClass, isPlainOldJavaObject, isJavaBean, isSealed, isJavaEnum, isJavaEnumValue, isCase, isObject, isVal, isCaseClass, isCaseObject, isCaseVal, isAvailableHere}, expected behavior"
      ) {
        import TypesFixtures.{testFlags, testChildrenFlags}

        test("for Scala 3 enums") {
          List(
            testFlags[examples.ExampleEnum],
            testFlags[examples.ExampleEnumWithTypeParam[String]],
            testFlags[examples.ExampleEnumGADT[String]]
          ).foreach { actual =>
            actual <==> Data.map(
              "Type.isPrimitive" -> Data(false),
              "Type.isArray" -> Data(false),
              "Type.isIArray" -> Data(false),
              "Type.isJvmBuiltIn" -> Data(false),
              "Type.isAbstract" -> Data(true),
              "Type.isFinal" -> Data(false),
              "Type.isClass" -> Data(true),
              "Type.isTypeSystemSpecial" -> Data(false),
              "Type.isOpaqueType" -> Data(false),
              "Type.isTuple" -> Data(false),
              "Type.notJvmBuiltInClass" -> Data(true),
              "Type.isPlainOldJavaObject" -> Data(false),
              "Type.isJavaBean" -> Data(false),
              "Type.isSealed" -> Data(true),
              "Type.isJavaEnum" -> Data(false),
              "Type.isJavaEnumValue" -> Data(false),
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

          testChildrenFlags[examples.ExampleEnum] <==> Data.map(
            "ExampleEnumClass" -> Data.map(
              "Type.isSealed" -> Data(false),
              "Type.isJavaEnum" -> Data(false),
              "Type.isJavaEnumValue" -> Data(false),
              "Type.isCase" -> Data(true),
              "Type.isObject" -> Data(false),
              "Type.isVal" -> Data(false),
              "Type.isCaseClass" -> Data(true),
              "Type.isCaseObject" -> Data(false),
              "Type.isCaseVal" -> Data(false),
              "Type.isAvailable(Everywhere)" -> Data(true),
              "Type.isAvailable(AtCallSite)" -> Data(true)
            ),
            "ExampleEnumValue" -> Data.map(
              "Type.isSealed" -> Data(false),
              "Type.isJavaEnum" -> Data(false),
              "Type.isJavaEnumValue" -> Data(false),
              "Type.isCase" -> Data(true),
              "Type.isObject" -> Data(false),
              "Type.isVal" -> Data(true),
              "Type.isCaseClass" -> Data(false),
              "Type.isCaseObject" -> Data(false),
              "Type.isCaseVal" -> Data(true),
              "Type.isAvailable(Everywhere)" -> Data(true),
              "Type.isAvailable(AtCallSite)" -> Data(true)
            )
          )
        }

        test("for Scala 3 enum cases") {
          testFlags[examples.ExampleEnum.ExampleEnumClass] <==> Data.map(
            "Type.isPrimitive" -> Data(false),
            "Type.isArray" -> Data(false),
            "Type.isIArray" -> Data(false),
            "Type.isJvmBuiltIn" -> Data(false),
            "Type.isAbstract" -> Data(false),
            "Type.isFinal" -> Data(true),
            "Type.isClass" -> Data(true),
            "Type.isTypeSystemSpecial" -> Data(false),
            "Type.isOpaqueType" -> Data(false),
            "Type.isTuple" -> Data(false),
            "Type.notJvmBuiltInClass" -> Data(true),
            "Type.isPlainOldJavaObject" -> Data(true),
            "Type.isJavaBean" -> Data(false),
            "Type.isSealed" -> Data(false),
            "Type.isJavaEnum" -> Data(false),
            "Type.isJavaEnumValue" -> Data(false),
            "Type.isCase" -> Data(true),
            "Type.isObject" -> Data(false),
            "Type.isVal" -> Data(false),
            "Type.isCaseClass" -> Data(true),
            "Type.isCaseObject" -> Data(false),
            "Type.isCaseVal" -> Data(false),
            "Type.isAvailable(Everywhere)" -> Data(true),
            "Type.isAvailable(AtCallSite)" -> Data(true)
          )
        }

        test("for Scala 3 opaque types") {
          import examples.opaqueid.OpaqueId
          testFlags[OpaqueId] <==> Data.map(
            "Type.isPrimitive" -> Data(false),
            "Type.isArray" -> Data(false),
            "Type.isIArray" -> Data(false),
            "Type.isJvmBuiltIn" -> Data(false),
            "Type.isAbstract" -> Data(false),
            "Type.isFinal" -> Data(false),
            "Type.isClass" -> Data(false),
            "Type.isTypeSystemSpecial" -> Data(false),
            "Type.isOpaqueType" -> Data(true),
            "Type.isTuple" -> Data(false),
            "Type.notJvmBuiltInClass" -> Data(false),
            "Type.isPlainOldJavaObject" -> Data(false),
            "Type.isJavaBean" -> Data(false),
            "Type.isSealed" -> Data(false),
            "Type.isJavaEnum" -> Data(false),
            "Type.isJavaEnumValue" -> Data(false),
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

      group("methods: Type.{isTuple, constructors, methods}, expected behavior for tuples") {
        import TypesFixtures.testFlags

        test("for small tuples (Tuple1, Tuple2, Tuple3)") {
          List(
            testFlags[Tuple1[Int]],
            testFlags[(Int, String)],
            testFlags[(Int, String, Boolean)]
          ).foreach { actual =>
            actual <==> Data.map(
              "Type.isPrimitive" -> Data(false),
              "Type.isArray" -> Data(false),
              "Type.isIArray" -> Data(false),
              "Type.isJvmBuiltIn" -> Data(false),
              "Type.isAbstract" -> Data(false),
              "Type.isFinal" -> Data(true),
              "Type.isClass" -> Data(true),
              "Type.isTypeSystemSpecial" -> Data(false),
              "Type.isOpaqueType" -> Data(false),
              "Type.isTuple" -> Data(true),
              "Type.notJvmBuiltInClass" -> Data(true),
              "Type.isPlainOldJavaObject" -> Data(true),
              "Type.isJavaBean" -> Data(false),
              "Type.isSealed" -> Data(false),
              "Type.isJavaEnum" -> Data(false),
              "Type.isJavaEnumValue" -> Data(false),
              "Type.isCase" -> Data(true),
              "Type.isObject" -> Data(false),
              "Type.isVal" -> Data(false),
              "Type.isCaseClass" -> Data(true),
              "Type.isCaseObject" -> Data(false),
              "Type.isCaseVal" -> Data(false),
              "Type.isAvailable(Everywhere)" -> Data(true),
              "Type.isAvailable(AtCallSite)" -> Data(true)
            )
          }
        }

        test("for TupleXXL (23+ elements)") {
          // 23-element tuple, represented as nested *: at the type level, TupleXXL at runtime.
          // Note: Scala 3 macro reflection sees TupleXXL types with typeSymbol = scala.Tuple (sealed abstract trait),
          // so flags like isAbstract/isSealed/isClass reflect the Tuple trait, not the *: case class.
          testFlags[
            (
                Int,
                String,
                Boolean,
                Long,
                Float,
                Double,
                Char,
                Byte,
                Short,
                Int,
                String,
                Boolean,
                Long,
                Float,
                Double,
                Char,
                Byte,
                Short,
                Int,
                String,
                Boolean,
                Int,
                String
            )
          ] <==> Data.map(
            "Type.isPrimitive" -> Data(false),
            "Type.isArray" -> Data(false),
            "Type.isIArray" -> Data(false),
            "Type.isJvmBuiltIn" -> Data(false),
            "Type.isAbstract" -> Data(true),
            "Type.isFinal" -> Data(false),
            "Type.isClass" -> Data(true),
            "Type.isTypeSystemSpecial" -> Data(false),
            "Type.isOpaqueType" -> Data(false),
            "Type.isTuple" -> Data(true),
            "Type.notJvmBuiltInClass" -> Data(true),
            "Type.isPlainOldJavaObject" -> Data(false),
            "Type.isJavaBean" -> Data(false),
            "Type.isSealed" -> Data(true),
            "Type.isJavaEnum" -> Data(false),
            "Type.isJavaEnumValue" -> Data(false),
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

        test("for IArray types") {
          testFlags[IArray[Int]] <==> Data.map(
            "Type.isPrimitive" -> Data(false),
            "Type.isArray" -> Data(false),
            "Type.isIArray" -> Data(true),
            "Type.isJvmBuiltIn" -> Data(true),
            "Type.isAbstract" -> Data(false),
            "Type.isFinal" -> Data(true),
            "Type.isClass" -> Data(false),
            "Type.isTypeSystemSpecial" -> Data(false),
            "Type.isOpaqueType" -> Data(true),
            "Type.isTuple" -> Data(false),
            "Type.notJvmBuiltInClass" -> Data(false),
            "Type.isPlainOldJavaObject" -> Data(false),
            "Type.isJavaBean" -> Data(false),
            "Type.isSealed" -> Data(false),
            "Type.isJavaEnum" -> Data(false),
            "Type.isJavaEnumValue" -> Data(false),
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

        test("for EmptyTuple") {
          // EmptyTuple is a type alias in Scala 3, so macro reflection sees it differently
          // from regular classes/objects.
          testFlags[EmptyTuple] <==> Data.map(
            "Type.isPrimitive" -> Data(false),
            "Type.isArray" -> Data(false),
            "Type.isIArray" -> Data(false),
            "Type.isJvmBuiltIn" -> Data(false),
            "Type.isAbstract" -> Data(false),
            "Type.isFinal" -> Data(false),
            "Type.isClass" -> Data(false),
            "Type.isTypeSystemSpecial" -> Data(false),
            "Type.isOpaqueType" -> Data(false),
            "Type.isTuple" -> Data(true),
            "Type.notJvmBuiltInClass" -> Data(false),
            "Type.isPlainOldJavaObject" -> Data(false),
            "Type.isJavaBean" -> Data(false),
            "Type.isSealed" -> Data(false),
            "Type.isJavaEnum" -> Data(false),
            "Type.isJavaEnumValue" -> Data(false),
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

      // TODO: <:< and =:= should behave the same way, let's test it another day
    }

    group("type TypeCodec") {

      test("methods TypeCodec.{toType} should allow converting types for TupleXXL (23+ elements) and EmptyTuple") {
        import TypesFixtures.testTupleXXLCodec

        testTupleXXLCodec <==> Data.map(
          "23-element tuple" -> Data.map(
            "encoded" -> Data(
              "scala.*:[\"a\", scala.*:[\"b\", scala.*:[\"a\", scala.*:[\"b\", scala.*:[\"a\", scala.*:[\"b\", scala.*:[\"a\", scala.*:[\"b\", scala.*:[\"a\", scala.*:[\"b\", scala.*:[\"a\", scala.*:[\"b\", scala.*:[\"a\", scala.*:[\"b\", scala.*:[\"a\", scala.*:[\"b\", scala.*:[\"a\", scala.*:[\"b\", scala.*:[\"a\", scala.*:[\"b\", scala.*:[\"a\", scala.*:[\"b\", scala.*:[\"a\", scala.Tuple$package.EmptyTuple]]]]]]]]]]]]]]]]]]]]]]]"
            )
          )
        )
      }
    }
  }
}
