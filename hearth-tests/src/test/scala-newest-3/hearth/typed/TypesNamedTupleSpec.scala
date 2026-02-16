package hearth
package typed

import hearth.data.Data

/** NamedTuple tests — only compiled on Scala 3.7+ (scala-newest-3 source set).
  *
  * Macro implementation is in [[TypesFixturesImpl]]
  */
final class TypesNamedTupleSpec extends MacroSuite {

  group("typed.Types") {

    group("type Type") {

      group(
        "methods: Type.{isPrimitive, isJvmBuiltIn, isAbstract, isFinal, isClass, isTuple, isNamedTuple, notJvmBuiltInClass, isPlainOldJavaObject, isJavaBean, isSealed, isJavaEnum, isJavaEnumValue, isCase, isObject, isVal, isCaseClass, isCaseObject, isCaseVal, isAvailableHere}, expected behavior"
      ) {
        import TypesFixtures.testFlags

        test("for named tuples (Scala 3.7+)") {
          // Named tuples are NamedTuple.NamedTuple[("name", "age"), (String, Int)] at the type level.
          // NamedTuple is an opaque type alias wrapping a regular tuple, so the macro sees it as:
          // - isOpaqueType = true (it's an opaque type)
          // - isTuple = false (the opaque wrapper hides the underlying tuple)
          // - isClass = false (opaque types are not classes)
          // - isNamedTuple = true (our custom detection)
          testFlags[(name: String, age: Int)] <==> Data.map(
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
            "Type.isNamedTuple" -> Data(true),
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
    }
  }
}
