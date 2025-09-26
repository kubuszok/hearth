package hearth
package typed

import hearth.data.Data
import hearth.Tags

import scala.annotation.unused

/** Macro implementation is in [[TypesFixturesImpl]] */
final class TypesScala3Spec extends MacroSuite {

  group("typed.Types") {

    group("methods: Type.{simple, fcqn, plainPrint, prettyPrint}, expected behavior") {
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
        ).foreach { case (actual, (shortName, fcqn, fullName)) =>
          actual <==> Data(
            Map(
              "Type.shortName" -> Data(shortName),
              "Type.fcqn" -> Data(fcqn),
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
            "ExampleEnum",
            "hearth.examples.ExampleEnum.ExampleEnumValue",
            "hearth.examples.ExampleEnum.ExampleEnumValue"
          )
        ).foreach { case (actual, (shortName, fcqn, fullName)) =>
          actual <==> Data(
            Map(
              "Type.shortName" -> Data(shortName),
              "Type.fcqn" -> Data(fcqn),
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

      test("for Scala 3 enums".tag(Tags.langVerMismatch)) {
        testPosition[examples.ExampleEnum] <==> Data.map(
          "Type.position" -> Data("enums-s3.scala.scala:1:1")
        )
        testPosition[examples.ExampleEnumWithTypeParam[String]] <==> Data.map(
          "Type.position" -> Data("enums-s3.scala.scala:1:1")
        )
        testPosition[examples.ExampleEnumGADT[String]] <==> Data.map(
          "Type.position" -> Data("enums-s3.scala.scala:1:1")
        )
      }

      test("for Scala 3 enum cases".tag(Tags.langVerMismatch)) {
        testPosition[examples.ExampleEnum.ExampleEnumClass] <==> Data.map(
          "Type.position" -> Data("enums-s3.scala.scala:1:1")
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
      "methods: Type.{isPrimitive, isBuiltIn, isAbstract, isFinal, isClass, notJvmBuiltInClass, isPlainOldJavaObject, isJavaBean, isSealed, isJavaEnum, isJavaEnumValue, isCase, isObject, isVal, isCaseClass, isCaseObject, isCaseVal, isAvailableHere}, expected behavior"
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
            "Type.isJvmBuiltIn" -> Data(false),
            "Type.isAbstract" -> Data(true),
            "Type.isFinal" -> Data(false),
            "Type.isClass" -> Data(true),
            "Type.isTypeSystemSpecial" -> Data(false),
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
            "Type.isAvailable(Everywhere)" -> Data(true)
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
            "Type.isCaseVal" -> Data(false)
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
            "Type.isCaseVal" -> Data(true)
          )
        )
      }

      test("for Scala 3 enum cases") {
        testFlags[examples.ExampleEnum.ExampleEnumClass] <==> Data.map(
          "Type.isPrimitive" -> Data(false),
          "Type.isArray" -> Data(false),
          "Type.isJvmBuiltIn" -> Data(false),
          "Type.isAbstract" -> Data(false),
          "Type.isFinal" -> Data(true),
          "Type.isClass" -> Data(true),
          "Type.isTypeSystemSpecial" -> Data(false),
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
          "Type.isAvailable(Everywhere)" -> Data(true)
        )
      }
    }

    // TODO: <:< and =:= should behave the same way, let's test it another day
  }
}
