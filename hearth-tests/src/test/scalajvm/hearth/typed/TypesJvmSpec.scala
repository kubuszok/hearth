package hearth
package typed

import hearth.data.Data

/** Macro implementation is in [[TypesFixturesImpl]] */
final class TypesJvmSpec extends MacroSuite {

  group("typed.Types") {

    group("type Type") {

      group("methods: Type.{simple, fqcn, plainPrint, prettyPrint}, expected behavior") {
        import TypesFixtures.testNamesPrinters

        test("for top-level Java classes") {
          List(
            testNamesPrinters[examples.classes.ExampleJavaInterface] -> "ExampleJavaInterface",
            testNamesPrinters[examples.classes.ExampleJavaClass] -> "ExampleJavaClass"
          ).foreach { case (actual, expected) =>
            actual <==> Data(
              Map(
                "Type.shortName" -> Data(expected),
                "Type.fqcn" -> Data(s"hearth.examples.classes.$expected"),
                "Type.plainPrint" -> Data(s"hearth.examples.classes.$expected"),
                "Type.prettyPrint" -> Data(s"hearth.examples.classes.$expected")
              )
            )
          }
        }

        test("for Java enumerations") {
          List(
            testNamesPrinters[examples.enums.ExampleJavaEnum] -> "ExampleJavaEnum",
            testNamesPrinters[examples.enums.ExampleJavaEnumWithMethods] -> "ExampleJavaEnumWithMethods"
          ).foreach { case (actual, expected) =>
            actual <==> Data(
              Map(
                "Type.shortName" -> Data(expected),
                "Type.fqcn" -> Data(s"hearth.examples.enums.$expected"),
                "Type.plainPrint" -> Data(s"hearth.examples.enums.$expected"),
                "Type.prettyPrint" -> Data(s"hearth.examples.enums.$expected")
              )
            )
          }
        }
      }

      group("methods: Type.{classOfType} expected behavior") {
        import TypesFixtures.testClassOfType

        test("for top-level Java classes".tag(Tags.recompileFlaky)) {
          testClassOfType[examples.classes.ExampleJavaInterface] <==> Data.map(
            "Type.classOfType" -> Data(classOf[examples.classes.ExampleJavaInterface].toString)
          )
          testClassOfType[examples.classes.ExampleJavaClass] <==> Data.map(
            "Type.classOfType" -> Data(classOf[examples.classes.ExampleJavaClass].toString)
          )
        }

        test("for Java enumerations".tag(Tags.recompileFlaky)) {
          testClassOfType[examples.enums.ExampleJavaEnum] <==> Data.map(
            "Type.classOfType" -> Data(classOf[examples.enums.ExampleJavaEnum].toString)
          )
          testClassOfType[examples.enums.ExampleJavaEnumWithMethods] <==> Data.map(
            "Type.classOfType" -> Data(classOf[examples.enums.ExampleJavaEnumWithMethods].toString)
          )
        }
      }

      group("methods: Type.{position} expected behavior") {
        import TypesFixtures.testPosition

        test("for top-level Java classes".tag(Tags.langVerMismatch)) {
          testPosition[examples.classes.ExampleJavaInterface] <==> Data.map(
            "Type.position" -> Data("<no position>")
          )
          testPosition[examples.classes.ExampleJavaClass] <==> Data.map(
            "Type.position" -> Data("<no position>")
          )
        }

        test("for Java enumerations".tag(Tags.langVerMismatch)) {
          testPosition[examples.enums.ExampleJavaEnum] <==> Data.map(
            "Type.position" -> Data("<no position>")
          )
          testPosition[examples.enums.ExampleJavaEnumWithMethods] <==> Data.map(
            "Type.position" -> Data("<no position>")
          )
        }
      }

      group("methods: Type.{directChildren, exhaustiveChildren} expected behavior") {
        import TypesFixtures.testChildren

        test("for top-level Java classes") {
          testChildren[examples.classes.ExampleJavaInterface] <==> Data.map(
            "Type.directChildren" -> Data("<no direct children>"),
            "Type.exhaustiveChildren" -> Data("<no exhaustive children>")
          )
          testChildren[examples.classes.ExampleJavaClass] <==> Data.map(
            "Type.directChildren" -> Data("<no direct children>"),
            "Type.exhaustiveChildren" -> Data("<no exhaustive children>")
          )
        }

        test("for Java enumerations") {
          testChildren[examples.enums.ExampleJavaEnum] <==> Data.map(
            "Type.directChildren" -> Data.map(
              "VALUE1" -> Data("hearth.examples.enums.ExampleJavaEnum.VALUE1.type"),
              "VALUE2" -> Data("hearth.examples.enums.ExampleJavaEnum.VALUE2.type")
            ),
            "Type.exhaustiveChildren" -> Data.map(
              "VALUE1" -> Data("hearth.examples.enums.ExampleJavaEnum.VALUE1.type"),
              "VALUE2" -> Data("hearth.examples.enums.ExampleJavaEnum.VALUE2.type")
            )
          )
          testChildren[examples.enums.ExampleJavaEnumWithMethods] <==> Data.map(
            "Type.directChildren" -> Data.map(
              "VALUE1" -> Data("hearth.examples.enums.ExampleJavaEnumWithMethods.VALUE1.type"),
              "VALUE2" -> Data("hearth.examples.enums.ExampleJavaEnumWithMethods.VALUE2.type")
            ),
            "Type.exhaustiveChildren" -> Data.map(
              "VALUE1" -> Data("hearth.examples.enums.ExampleJavaEnumWithMethods.VALUE1.type"),
              "VALUE2" -> Data("hearth.examples.enums.ExampleJavaEnumWithMethods.VALUE2.type")
            )
          )
        }
      }

      // TODO: annotations? Could we even use StaticAnnotation on Java classes? Can we detect Java annotations in macro at all?

      group(
        "methods: Type.{isPrimitive, isJvmBuiltIn, isAbstract, isFinal, isClass, isTuple, notJvmBuiltInClass, isPlainOldJavaObject, isJavaBean, isSealed, isJavaEnum, isJavaEnumValue, isCase, isObject, isVal, isCaseClass, isCaseObject, isCaseVal, isAvailableHere}, expected behavior"
      ) {
        import TypesFixtures.{testFlags, testChildrenFlags}

        test("for top-level Java classes") {
          List(
            testFlags[examples.classes.ExampleJavaInterface] -> ((true, false, false)),
            testFlags[examples.classes.ExampleJavaClass] -> ((false, false, false))
          ).foreach { case (actual, (isAbstract, isFinal, isCase)) =>
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
              "Type.notJvmBuiltInClass" -> Data(true),
              "Type.isPlainOldJavaObject" -> Data(!isAbstract),
              "Type.isJavaBean" -> Data(!isAbstract),
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

        test("for Java enumerations") {
          List(
            testFlags[examples.enums.ExampleJavaEnum],
            testFlags[examples.enums.ExampleJavaEnumWithMethods]
          ).foreach { actual =>
            actual <==> Data.map(
              "Type.isPrimitive" -> Data(false),
              "Type.isArray" -> Data(false),
              "Type.isIArray" -> Data(false),
              "Type.isJvmBuiltIn" -> Data(false),
              "Type.isAbstract" -> Data(true),
              "Type.isFinal" -> Data(true),
              "Type.isClass" -> Data(true),
              "Type.isTypeSystemSpecial" -> Data(false),
              "Type.isOpaqueType" -> Data(false),
              "Type.isTuple" -> Data(false),
              "Type.notJvmBuiltInClass" -> Data(true),
              "Type.isPlainOldJavaObject" -> Data(false),
              "Type.isJavaBean" -> Data(false),
              "Type.isSealed" -> Data(true),
              "Type.isJavaEnum" -> Data(true),
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

            testChildrenFlags[examples.enums.ExampleJavaEnum] <==> Data.map(
              "VALUE1" -> Data.map(
                "Type.isSealed" -> Data(false),
                "Type.isJavaEnum" -> Data(false),
                "Type.isJavaEnumValue" -> Data(true),
                "Type.isEnumeration" -> Data(false),
                "Type.isCase" -> Data(false),
                "Type.isObject" -> Data(false),
                "Type.isVal" -> Data(true),
                "Type.isCaseClass" -> Data(false),
                "Type.isCaseObject" -> Data(false),
                "Type.isCaseVal" -> Data(false),
                "Type.isAvailable(Everywhere)" -> Data(true),
                "Type.isAvailable(AtCallSite)" -> Data(true)
              ),
              "VALUE2" -> Data.map(
                "Type.isSealed" -> Data(false),
                "Type.isJavaEnum" -> Data(false),
                "Type.isJavaEnumValue" -> Data(true),
                "Type.isEnumeration" -> Data(false),
                "Type.isCase" -> Data(false),
                "Type.isObject" -> Data(false),
                "Type.isVal" -> Data(true),
                "Type.isCaseClass" -> Data(false),
                "Type.isCaseObject" -> Data(false),
                "Type.isCaseVal" -> Data(false),
                "Type.isAvailable(Everywhere)" -> Data(true),
                "Type.isAvailable(AtCallSite)" -> Data(true)
              )
            )
            testChildrenFlags[examples.enums.ExampleJavaEnumWithMethods] <==> Data.map(
              "VALUE1" -> Data.map(
                "Type.isSealed" -> Data(false),
                "Type.isJavaEnum" -> Data(false),
                "Type.isJavaEnumValue" -> Data(true),
                "Type.isEnumeration" -> Data(false),
                "Type.isCase" -> Data(false),
                "Type.isObject" -> Data(false),
                "Type.isVal" -> Data(true),
                "Type.isCaseClass" -> Data(false),
                "Type.isCaseObject" -> Data(false),
                "Type.isCaseVal" -> Data(false),
                "Type.isAvailable(Everywhere)" -> Data(true),
                "Type.isAvailable(AtCallSite)" -> Data(true)
              ),
              "VALUE2" -> Data.map(
                "Type.isSealed" -> Data(false),
                "Type.isJavaEnum" -> Data(false),
                "Type.isJavaEnumValue" -> Data(true),
                "Type.isEnumeration" -> Data(false),
                "Type.isCase" -> Data(false),
                "Type.isObject" -> Data(false),
                "Type.isVal" -> Data(true),
                "Type.isCaseClass" -> Data(false),
                "Type.isCaseObject" -> Data(false),
                "Type.isCaseVal" -> Data(false),
                "Type.isAvailable(Everywhere)" -> Data(true),
                "Type.isAvailable(AtCallSite)" -> Data(true)
              )
            )
          }
        }
      }

      // TODO: <:< and =:= should behave the same way, let's test it another day
    }
  }
}
