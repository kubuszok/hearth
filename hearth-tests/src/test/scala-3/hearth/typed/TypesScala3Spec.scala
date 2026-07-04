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

        test("for IArray must not throw (issue #333)") {
          // `IArray[E]` is an opaque alias recognized as a JVM built-in; `classOfType` must degrade gracefully
          // (return a class or None) rather than aborting the macro with a HearthAssertionError.
          testClassOfType[IArray[Int]] <==> Data.map(
            "Type.classOfType" -> Data("class [I")
          )
          testClassOfType[IArray[String]] <==> Data.map(
            "Type.classOfType" -> Data("class [Ljava.lang.String;")
          )
          // Element type with no resolvable runtime class (a union): the `IArrayCtor` branch answers for itself with
          // the erased array-of-Object class instead of falling through to the "unhandled built-in" assertion, which
          // used to abort with a HearthAssertionError here (issue #333). The assertion itself stays in place to catch
          // genuinely un-branched built-ins.
          testClassOfType[IArray[Int | String]] <==> Data.map(
            "Type.classOfType" -> Data("class [Ljava.lang.Object;")
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

        test("an export-created alias classifies identically to its underlying class (issue #315)") {
          // `Exported.Foo` is `export Inner.Foo`; its flags (isClass/isCase/isAbstract/...) must match `Inner.Foo`,
          // not the (flag-less) alias symbol. Before the fix isClass/isCase were false for the export alias.
          testFlags[examples.exports.Exported.Foo] <==> testFlags[examples.exports.Inner.Foo]
        }

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
              "Type.isNamedTuple" -> Data(false),
              "Type.isUnionType" -> Data(false),
              "Type.notJvmBuiltInClass" -> Data(true),
              "Type.isPlainOldJavaObject" -> Data(false),
              "Type.isJavaBean" -> Data(false),
              "Type.isSealed" -> Data(true),
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

          testChildrenFlags[examples.ExampleEnum] <==> Data.map(
            "ExampleEnumClass" -> Data.map(
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
            "ExampleEnumValue" -> Data.map(
              "Type.isSealed" -> Data(false),
              "Type.isJavaEnum" -> Data(false),
              "Type.isJavaEnumValue" -> Data(false),
              "Type.isEnumeration" -> Data(false),
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
            "Type.isNamedTuple" -> Data(false),
            "Type.isUnionType" -> Data(false),
            "Type.notJvmBuiltInClass" -> Data(true),
            "Type.isPlainOldJavaObject" -> Data(true),
            "Type.isJavaBean" -> Data(false),
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
            "Type.isNamedTuple" -> Data(false),
            "Type.isUnionType" -> Data(false),
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
              "Type.isNamedTuple" -> Data(false),
              "Type.isUnionType" -> Data(false),
              "Type.notJvmBuiltInClass" -> Data(true),
              "Type.isPlainOldJavaObject" -> Data(true),
              "Type.isJavaBean" -> Data(false),
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
            "Type.isNamedTuple" -> Data(false),
            "Type.isUnionType" -> Data(false),
            "Type.notJvmBuiltInClass" -> Data(true),
            "Type.isPlainOldJavaObject" -> Data(false),
            "Type.isJavaBean" -> Data(false),
            "Type.isSealed" -> Data(true),
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
            "Type.isNamedTuple" -> Data(false),
            "Type.isUnionType" -> Data(false),
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
            "Type.isNamedTuple" -> Data(false),
            "Type.isUnionType" -> Data(false),
            "Type.notJvmBuiltInClass" -> Data(false),
            "Type.isPlainOldJavaObject" -> Data(false),
            "Type.isJavaBean" -> Data(false),
            "Type.isSealed" -> Data(false),
            "Type.isJavaEnum" -> Data(false),
            "Type.isJavaEnumValue" -> Data(false),
            "Type.isEnumeration" -> Data(false),
            // `EmptyTuple`'s value is a `case object`, whose `Case` flag lives on the term symbol - now detected (#311).
            // The composite classifiers stay `false` because `isClass`/`isObject`/`isVal` are all `false` here.
            "Type.isCase" -> Data(true),
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

      group("methods: Type.{isUnionType, directChildren, exhaustiveChildren} for union types") {
        import TypesFixtures.testUnionMembers

        test("for non-union types") {
          testUnionMembers[String] <==> Data.map(
            "Type.isUnionType" -> Data(false),
            "Type.directChildren" -> Data("<no direct children>"),
            "Type.exhaustiveChildren" -> Data("<no exhaustive children>")
          )
          testUnionMembers[Int] <==> Data.map(
            "Type.isUnionType" -> Data(false),
            "Type.directChildren" -> Data("<no direct children>"),
            "Type.exhaustiveChildren" -> Data("<no exhaustive children>")
          )
        }

        test("for String | Int") {
          testUnionMembers[examples.unions.StringOrInt] <==> Data.map(
            "Type.isUnionType" -> Data(true),
            "Type.directChildren" -> Data.map(
              "java.lang.String" -> Data("java.lang.String"),
              "scala.Int" -> Data("scala.Int")
            ),
            "Type.exhaustiveChildren" -> Data.map(
              "java.lang.String" -> Data("java.lang.String"),
              "scala.Int" -> Data("scala.Int")
            )
          )
        }

        test("for Boolean | Double") {
          testUnionMembers[examples.unions.BooleanOrDouble] <==> Data.map(
            "Type.isUnionType" -> Data(true),
            "Type.directChildren" -> Data.map(
              "scala.Boolean" -> Data("scala.Boolean"),
              "scala.Double" -> Data("scala.Double")
            ),
            "Type.exhaustiveChildren" -> Data.map(
              "scala.Boolean" -> Data("scala.Boolean"),
              "scala.Double" -> Data("scala.Double")
            )
          )
        }

        test("for Color.Red.type | Color.Blue.type (singletons matched by value, exempt from class disjointness)") {
          testUnionMembers[examples.unions.RedOrBlue] <==> Data.map(
            "Type.isUnionType" -> Data(true),
            "Type.directChildren" -> Data.map(
              "hearth.examples.Color.Red.type" -> Data("hearth.examples.Color.Red.type"),
              "hearth.examples.Color.Blue.type" -> Data("hearth.examples.Color.Blue.type")
            ),
            "Type.exhaustiveChildren" -> Data.map(
              "hearth.examples.Color.Red.type" -> Data("hearth.examples.Color.Red.type"),
              "hearth.examples.Color.Blue.type" -> Data("hearth.examples.Color.Blue.type")
            )
          )
        }

        test("for Color.Red.type | String (mixed: singleton + class-tested member)") {
          testUnionMembers[examples.unions.RedOrString] <==> Data.map(
            "Type.isUnionType" -> Data(true),
            "Type.directChildren" -> Data.map(
              "hearth.examples.Color.Red.type" -> Data("hearth.examples.Color.Red.type"),
              "java.lang.String" -> Data("java.lang.String")
            ),
            "Type.exhaustiveChildren" -> Data.map(
              "hearth.examples.Color.Red.type" -> Data("hearth.examples.Color.Red.type"),
              "java.lang.String" -> Data("java.lang.String")
            )
          )
        }

        test("for Marker.type | Int (mixed: module singleton + class-tested member)") {
          testUnionMembers[examples.unions.MarkerOrInt] <==> Data.map(
            "Type.isUnionType" -> Data(true),
            "Type.directChildren" -> Data.map(
              "hearth.examples.unions.Marker.type" -> Data("hearth.examples.unions.Marker.type"),
              "scala.Int" -> Data("scala.Int")
            ),
            "Type.exhaustiveChildren" -> Data.map(
              "hearth.examples.unions.Marker.type" -> Data("hearth.examples.unions.Marker.type"),
              "scala.Int" -> Data("scala.Int")
            )
          )
        }

        test("for String | Int | Boolean") {
          testUnionMembers[examples.unions.StringOrIntOrBoolean] <==> Data.map(
            "Type.isUnionType" -> Data(true),
            "Type.directChildren" -> Data.map(
              "java.lang.String" -> Data("java.lang.String"),
              "scala.Int" -> Data("scala.Int"),
              "scala.Boolean" -> Data("scala.Boolean")
            ),
            "Type.exhaustiveChildren" -> Data.map(
              "java.lang.String" -> Data("java.lang.String"),
              "scala.Int" -> Data("scala.Int"),
              "scala.Boolean" -> Data("scala.Boolean")
            )
          )
        }

        test("for String | AnyRef (subtype overlap)") {
          testUnionMembers[examples.unions.StringOrAnyRef] <==> Data.map(
            "Type.isUnionType" -> Data(true),
            "Type.directChildren" -> Data("<no direct children>"),
            "Type.exhaustiveChildren" -> Data("<no exhaustive children>")
          )
        }

        test("for List[Int] | List[String] (same erasure)") {
          testUnionMembers[examples.unions.ListIntOrListString] <==> Data.map(
            "Type.isUnionType" -> Data(true),
            "Type.directChildren" -> Data("<no direct children>"),
            "Type.exhaustiveChildren" -> Data("<no exhaustive children>")
          )
        }

        test("for List[Int] | Seq[String] (related runtime classes — List <: Seq)") {
          testUnionMembers[examples.unions.ListIntOrSeqString] <==> Data.map(
            "Type.isUnionType" -> Data(true),
            "Type.directChildren" -> Data("<no direct children>"),
            "Type.exhaustiveChildren" -> Data("<no exhaustive children>")
          )
        }

        test("for Int | java.lang.Integer (same runtime class after boxing)") {
          testUnionMembers[examples.unions.IntOrJavaInteger] <==> Data.map(
            "Type.isUnionType" -> Data(true),
            "Type.directChildren" -> Data("<no direct children>"),
            "Type.exhaustiveChildren" -> Data("<no exhaustive children>")
          )
        }

        test("for Nothing | String (Nothing filters out)") {
          testUnionMembers[examples.unions.NothingOrString] <==> Data.map(
            "Type.isUnionType" -> Data(true),
            "Type.directChildren" -> Data("<no direct children>"),
            "Type.exhaustiveChildren" -> Data("<no exhaustive children>")
          )
        }

        test("for String | String (duplicate)") {
          testUnionMembers[examples.unions.StringOrString] <==> Data.map(
            "Type.isUnionType" -> Data(true),
            "Type.directChildren" -> Data("<no direct children>"),
            "Type.exhaustiveChildren" -> Data("<no exhaustive children>")
          )
        }

        test("for Int | AnyVal (subtype overlap)") {
          testUnionMembers[examples.unions.IntOrAnyVal] <==> Data.map(
            "Type.isUnionType" -> Data(true),
            "Type.directChildren" -> Data("<no direct children>"),
            "Type.exhaustiveChildren" -> Data("<no exhaustive children>")
          )
        }

        test("for OpaqueId | String (opaque with disjoint underlying class - accepted)") {
          // OpaqueId's underlying type (Long) is resolved via opaqueUnderlyingType, and Long vs String
          // runtime classes are disjoint, so the union is accepted.
          testUnionMembers[examples.unions.OpaqueIdOrString] <==> Data.map(
            "Type.isUnionType" -> Data(true),
            "Type.directChildren" -> Data.map(
              "hearth.examples.unions.OpaqueId" -> Data("hearth.examples.unions.OpaqueId"),
              "java.lang.String" -> Data("java.lang.String")
            ),
            "Type.exhaustiveChildren" -> Data.map(
              "hearth.examples.unions.OpaqueId" -> Data("hearth.examples.unions.OpaqueId"),
              "java.lang.String" -> Data("java.lang.String")
            )
          )
        }

        test("for OpaqueId | Long (opaque + its own underlying type - related classes, refused)") {
          testUnionMembers[examples.unions.OpaqueIdOrLong] <==> Data.map(
            "Type.isUnionType" -> Data(true),
            "Type.directChildren" -> Data("<no direct children>"),
            "Type.exhaustiveChildren" -> Data("<no exhaustive children>")
          )
        }

        test("for OpaqueId | OpaqueName (two opaques with disjoint underlying classes - accepted)") {
          // OpaqueId(= Long) and OpaqueName(= String) resolve to disjoint runtime classes.
          testUnionMembers[examples.unions.OpaqueIdOrOpaqueName] <==> Data.map(
            "Type.isUnionType" -> Data(true),
            "Type.directChildren" -> Data.map(
              "hearth.examples.unions.OpaqueId" -> Data("hearth.examples.unions.OpaqueId"),
              "hearth.examples.unions.OpaqueName" -> Data("hearth.examples.unions.OpaqueName")
            ),
            "Type.exhaustiveChildren" -> Data.map(
              "hearth.examples.unions.OpaqueId" -> Data("hearth.examples.unions.OpaqueId"),
              "hearth.examples.unions.OpaqueName" -> Data("hearth.examples.unions.OpaqueName")
            )
          )
        }

        test("for List[Int] | List[String] (same erasure) WITH user-provided TypeTests in scope - accepted") {
          import TypesFixtures.testChildrenNames
          import examples.unions.typetests.given

          testChildrenNames[examples.unions.ListIntOrListString] <==> Data.map(
            "scala.collection.immutable.List[scala.Int]" -> Data("List"),
            "scala.collection.immutable.List[java.lang.String]" -> Data("List")
          )
        }

        test(
          "for Color.Red.type | List[Int] | List[String] (mixed singleton + same-erasure members) WITH user-provided TypeTests in scope - accepted"
        ) {
          import TypesFixtures.testChildrenNames
          import examples.unions.typetests.given

          testChildrenNames[examples.unions.RedOrListIntOrListString] <==> Data.map(
            "hearth.examples.Color.Red.type" -> Data("Red"),
            "scala.collection.immutable.List[scala.Int]" -> Data("List"),
            "scala.collection.immutable.List[java.lang.String]" -> Data("List")
          )
        }
      }

      group("methods: Type.{opaqueUnderlyingType} expected behavior") {
        import TypesFixtures.testOpaqueUnderlyingType

        test("for simple opaque types") {
          testOpaqueUnderlyingType[examples.opaqueid.OpaqueId] <==> Data.map(
            "Type.isOpaqueType" -> Data(true),
            "Type.opaqueUnderlyingType" -> Data("scala.Long")
          )
        }

        test("for bounded opaque types (resolves the RHS, not the bound)") {
          testOpaqueUnderlyingType[examples.opaqueunderlying.Bounded] <==> Data.map(
            "Type.isOpaqueType" -> Data(true),
            "Type.opaqueUnderlyingType" -> Data("scala.Int")
          )
        }

        test("for nested opaque chains (resolves the innermost underlying type)") {
          testOpaqueUnderlyingType[examples.opaqueunderlying.Outer] <==> Data.map(
            "Type.isOpaqueType" -> Data(true),
            "Type.opaqueUnderlyingType" -> Data("scala.Int")
          )
        }

        test("for applied parameterized opaque types (type arguments are preserved)") {
          testOpaqueUnderlyingType[examples.opaqueunderlying.Wrapper[Int]] <==> Data.map(
            "Type.isOpaqueType" -> Data(true),
            "Type.opaqueUnderlyingType" -> Data("scala.collection.immutable.List[scala.Int]")
          )
        }

        test("for plain aliases to opaque types") {
          testOpaqueUnderlyingType[examples.opaqueunderlying.AliasToOpaque] <==> Data.map(
            "Type.isOpaqueType" -> Data(true),
            "Type.opaqueUnderlyingType" -> Data("scala.Long")
          )
        }

        test("for non-opaque types") {
          testOpaqueUnderlyingType[String] <==> Data.map(
            "Type.isOpaqueType" -> Data(false),
            "Type.opaqueUnderlyingType" -> Data("<no underlying type>")
          )
          testOpaqueUnderlyingType[examples.classes.ExampleCaseClass] <==> Data.map(
            "Type.isOpaqueType" -> Data(false),
            "Type.opaqueUnderlyingType" -> Data("<no underlying type>")
          )
        }
      }
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
