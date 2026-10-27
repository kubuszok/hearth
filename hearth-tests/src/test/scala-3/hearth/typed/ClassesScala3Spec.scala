package hearth
package typed

import hearth.data.Data

/** Macro implementation is in [[ClassesFixturesImpl]] */
final class ClassesScala3Spec extends MacroSuite {

  group("trait typed.Classes") {

    group("class: Class[A], returns preprocessed class") {
      import ClassesFixtures.testClass

      test("for Scala 3 enum") {

        testClass[examples.ExampleEnum](
          "clone",
          "equals",
          "finalize",
          "getClass",
          "hashCode",
          "notify",
          "notifyAll",
          "toString",
          "wait",
          "asInstanceOf",
          "isInstanceOf",
          "synchronized",
          "==",
          "!=",
          "eq",
          "ne",
          "##"
        ) <==> Data.map(
          "commons" -> Data.map(
            "constructors" -> Data.list(Data("()")),
            "methods" -> Data.list(
              Data.map("name" -> Data("$new"), "parameters" -> Data("(_$ordinal: scala.Int, $name: java.lang.String)")),
              Data.map("name" -> Data("ExampleEnumClass"), "parameters" -> Data("")),
              Data.map("name" -> Data("canEqual"), "parameters" -> Data("(that: scala.Any)")),
              Data.map("name" -> Data("fromOrdinal"), "parameters" -> Data("(ordinal: scala.Int)")),
              Data.map("name" -> Data("ordinal"), "parameters" -> Data("(x$0: hearth.examples.ExampleEnum)")),
              Data.map("name" -> Data("ordinal"), "parameters" -> Data("")),
              Data.map("name" -> Data("productArity"), "parameters" -> Data("")),
              Data.map("name" -> Data("productElement"), "parameters" -> Data("(n: scala.Int)")),
              Data.map("name" -> Data("productElementName"), "parameters" -> Data("(n: scala.Int)")),
              Data.map("name" -> Data("productElementNames"), "parameters" -> Data("")),
              Data.map("name" -> Data("productIterator"), "parameters" -> Data("")),
              Data.map("name" -> Data("productPrefix"), "parameters" -> Data("")),
              Data.map("name" -> Data("writeReplace"), "parameters" -> Data("()"))
            )
          ),
          "asSingleton" -> Data("<no singleton>"),
          "asNamedTuple" -> Data("<no named tuple>"),
          "asCaseClass" -> Data("<no case class>"),
          "asEnum" -> Data.map(
            "directChildren" -> Data(
              "(ExampleEnumClass: hearth.examples.ExampleEnum.ExampleEnumClass, ExampleEnumValue: hearth.examples.ExampleEnum.ExampleEnumValue.type)"
            ),
            "exhaustiveChildren" -> Data(
              "(ExampleEnumClass: hearth.examples.ExampleEnum.ExampleEnumClass, ExampleEnumValue: hearth.examples.ExampleEnum.ExampleEnumValue.type)"
            )
          ),
          "asJavaBean" -> Data("<no java bean>")
        )
      }
    }

    test("Enum[A].{matchOn and parMatchOn} should match on the Scala 3 enum") {
      import ClassesFixtures.testEnumMatchOnAndParMatchOn

      def code(input: hearth.examples.ExampleEnum) = testEnumMatchOnAndParMatchOn(input)
      code(hearth.examples.ExampleEnum.ExampleEnumClass(1)) <==>
        "sequential: subtype name: hearth.examples.ExampleEnum.ExampleEnumClass, expr: ExampleEnumClass, parallel: subtype name: hearth.examples.ExampleEnum.ExampleEnumClass, expr: ExampleEnumClass"
      code(hearth.examples.ExampleEnum.ExampleEnumValue) <==>
        "sequential: subtype name: hearth.examples.ExampleEnum.ExampleEnumValue.type, expr: ExampleEnumValue, parallel: subtype name: hearth.examples.ExampleEnum.ExampleEnumValue.type, expr: ExampleEnumValue"
    }

    test("Enum[A].{matchOn and parMatchOn} should match on the disjoint union type (String | Int)") {
      import ClassesFixtures.testEnumMatchOnAndParMatchOn

      def code(input: examples.unions.StringOrInt) = testEnumMatchOnAndParMatchOn(input)
      code("hello") <==>
        "sequential: subtype name: java.lang.String, expr: java.lang.String, parallel: subtype name: java.lang.String, expr: java.lang.String"
      code(42) <==>
        "sequential: subtype name: scala.Int, expr: scala.Int, parallel: subtype name: scala.Int, expr: scala.Int"
    }

    test("Enum[A].{matchOn and parMatchOn} should match on the 3-member disjoint union type (String | Int | Boolean)") {
      import ClassesFixtures.testEnumMatchOnAndParMatchOn

      def code(input: examples.unions.StringOrIntOrBoolean) = testEnumMatchOnAndParMatchOn(input)
      code("hello") <==>
        "sequential: subtype name: java.lang.String, expr: java.lang.String, parallel: subtype name: java.lang.String, expr: java.lang.String"
      code(42) <==>
        "sequential: subtype name: scala.Int, expr: scala.Int, parallel: subtype name: scala.Int, expr: scala.Int"
      code(true) <==>
        "sequential: subtype name: scala.Boolean, expr: scala.Boolean, parallel: subtype name: scala.Boolean, expr: scala.Boolean"
    }

    test(
      "Enum[A].{matchOn and parMatchOn} should match on the union of singleton types (Color.Red.type | Color.Blue.type)"
    ) {
      import ClassesFixtures.testEnumMatchOnAndParMatchOn

      def code(input: examples.unions.RedOrBlue) = testEnumMatchOnAndParMatchOn(input)
      code(examples.Color.Red) <==>
        "sequential: subtype name: hearth.examples.Color.Red.type, expr: hearth.examples.Color.Red.type, parallel: subtype name: hearth.examples.Color.Red.type, expr: hearth.examples.Color.Red.type"
      code(examples.Color.Blue) <==>
        "sequential: subtype name: hearth.examples.Color.Blue.type, expr: hearth.examples.Color.Blue.type, parallel: subtype name: hearth.examples.Color.Blue.type, expr: hearth.examples.Color.Blue.type"
    }

    test(
      "Enum[A].{matchOn and parMatchOn} should match on the mixed union of a singleton and a class (Color.Red.type | String)"
    ) {
      import ClassesFixtures.testEnumMatchOnAndParMatchOn

      def code(input: examples.unions.RedOrString) = testEnumMatchOnAndParMatchOn(input)
      code(examples.Color.Red) <==>
        "sequential: subtype name: hearth.examples.Color.Red.type, expr: hearth.examples.Color.Red.type, parallel: subtype name: hearth.examples.Color.Red.type, expr: hearth.examples.Color.Red.type"
      code("hello") <==>
        "sequential: subtype name: java.lang.String, expr: java.lang.String, parallel: subtype name: java.lang.String, expr: java.lang.String"
    }

    test(
      "Enum[A].{matchOn and parMatchOn} should match on the mixed union of a module singleton and a class (Marker.type | Int)"
    ) {
      import ClassesFixtures.testEnumMatchOnAndParMatchOn

      def code(input: examples.unions.MarkerOrInt) = testEnumMatchOnAndParMatchOn(input)
      code(examples.unions.Marker) <==>
        "sequential: subtype name: hearth.examples.unions.Marker.type, expr: hearth.examples.unions.Marker.type, parallel: subtype name: hearth.examples.unions.Marker.type, expr: hearth.examples.unions.Marker.type"
      code(42) <==>
        "sequential: subtype name: scala.Int, expr: scala.Int, parallel: subtype name: scala.Int, expr: scala.Int"
    }

    test(
      "Enum[A].{matchOn and parMatchOn} should match on the union with an opaque member with disjoint underlying class (OpaqueId | String)"
    ) {
      import ClassesFixtures.testEnumMatchOnAndParMatchOn

      def code(input: examples.unions.OpaqueIdOrString) = testEnumMatchOnAndParMatchOn(input)
      code(examples.unions.OpaqueId(42L)) <==>
        "sequential: subtype name: hearth.examples.unions.OpaqueId, expr: hearth.examples.unions.OpaqueId, parallel: subtype name: hearth.examples.unions.OpaqueId, expr: hearth.examples.unions.OpaqueId"
      code("hello") <==>
        "sequential: subtype name: java.lang.String, expr: java.lang.String, parallel: subtype name: java.lang.String, expr: java.lang.String"
    }

    test(
      "Enum[A].{matchOn and parMatchOn} should return <no enum> for union of an opaque member and its underlying type (OpaqueId | Long)"
    ) {
      import ClassesFixtures.testEnumMatchOnAndParMatchOn

      def code(input: examples.unions.OpaqueIdOrLong) = testEnumMatchOnAndParMatchOn(input)
      code(42L) <==> "<no enum>"
    }

    test(
      "Enum[A].{matchOn and parMatchOn} should return <no enum> for non-disjoint union type (List[Int] | List[String])"
    ) {
      import ClassesFixtures.testEnumMatchOnAndParMatchOn

      def code(input: examples.unions.ListIntOrListString) = testEnumMatchOnAndParMatchOn(input)
      code(List(1)) <==> "<no enum>"
    }

    test(
      "Enum[A].{matchOn and parMatchOn} should return <no enum> for union type with related runtime classes (List[Int] | Seq[String])"
    ) {
      import ClassesFixtures.testEnumMatchOnAndParMatchOn

      def code(input: examples.unions.ListIntOrSeqString) = testEnumMatchOnAndParMatchOn(input)
      code(List("a")) <==> "<no enum>"
    }

    test(
      "Enum[A].{matchOn and parMatchOn} should match on the same-erasure union (List[Int] | List[String]) when TypeTests are provided at the call site"
    ) {
      import ClassesFixtures.testEnumMatchOnAndParMatchOn
      import examples.unions.typetests.given

      def code(input: examples.unions.ListIntOrListString) = testEnumMatchOnAndParMatchOn(input)
      code(List(1, 2, 3)) <==>
        "sequential: subtype name: scala.collection.immutable.List[scala.Int], expr: scala.collection.immutable.List[scala.Int], parallel: subtype name: scala.collection.immutable.List[scala.Int], expr: scala.collection.immutable.List[scala.Int]"
      code(List("a", "b")) <==>
        "sequential: subtype name: scala.collection.immutable.List[java.lang.String], expr: scala.collection.immutable.List[java.lang.String], parallel: subtype name: scala.collection.immutable.List[java.lang.String], expr: scala.collection.immutable.List[java.lang.String]"
      // Empty lists are runtime-ambiguous; the TypeTest givens deterministically claim them for List[Int]
      code(Nil) <==>
        "sequential: subtype name: scala.collection.immutable.List[scala.Int], expr: scala.collection.immutable.List[scala.Int], parallel: subtype name: scala.collection.immutable.List[scala.Int], expr: scala.collection.immutable.List[scala.Int]"
    }

    test(
      "Enum[A].{matchOn and parMatchOn} should match on the mixed union (Color.Red.type | List[Int] | List[String]) when TypeTests are provided at the call site"
    ) {
      import ClassesFixtures.testEnumMatchOnAndParMatchOn
      import examples.unions.typetests.given

      def code(input: examples.unions.RedOrListIntOrListString) = testEnumMatchOnAndParMatchOn(input)
      code(examples.Color.Red) <==>
        "sequential: subtype name: hearth.examples.Color.Red.type, expr: hearth.examples.Color.Red.type, parallel: subtype name: hearth.examples.Color.Red.type, expr: hearth.examples.Color.Red.type"
      code(List(1)) <==>
        "sequential: subtype name: scala.collection.immutable.List[scala.Int], expr: scala.collection.immutable.List[scala.Int], parallel: subtype name: scala.collection.immutable.List[scala.Int], expr: scala.collection.immutable.List[scala.Int]"
      code(List("a")) <==>
        "sequential: subtype name: scala.collection.immutable.List[java.lang.String], expr: scala.collection.immutable.List[java.lang.String], parallel: subtype name: scala.collection.immutable.List[java.lang.String], expr: scala.collection.immutable.List[java.lang.String]"
    }

    test(
      "Enum.parse should name the missing TypeTest instances for the same-erasure union (List[Int] | List[String]) without givens"
    ) {
      import ClassesFixtures.testEnumParseDiagnostic

      testEnumParseDiagnostic[examples.unions.ListIntOrListString] <==>
        "scala.collection.immutable.List[scala.Int] | scala.collection.immutable.List[scala.Predef.String] is sealed/enumeration/union but has no direct children: " +
        "union member scala.collection.immutable.List[scala.Int] is not runtime-distinguishable; provide an implicit scala.reflect.TypeTest[scala.collection.immutable.List[scala.Int] | scala.collection.immutable.List[scala.Predef.String], scala.collection.immutable.List[scala.Int]], " +
        "union member scala.collection.immutable.List[java.lang.String] is not runtime-distinguishable; provide an implicit scala.reflect.TypeTest[scala.collection.immutable.List[scala.Int] | scala.collection.immutable.List[scala.Predef.String], scala.collection.immutable.List[java.lang.String]]"
    }

    test(
      "Enum.parse should accept the same-erasure union (List[Int] | List[String]) with TypeTests provided at the call site"
    ) {
      import ClassesFixtures.testEnumParseDiagnostic
      import examples.unions.typetests.given

      testEnumParseDiagnostic[examples.unions.ListIntOrListString] <==>
        "parsed with children: scala.collection.immutable.List[scala.Int], scala.collection.immutable.List[java.lang.String]"
    }

    // The member types of these unions are defined IN THIS COMPILATION UNIT, so no classfile exists at
    // macro-expansion time. A runtime `Class.forName`-style disjointness check fails (returns None → ByTypeTest →
    // union refused), so only a compiler-symbol-level check can accept/refuse them correctly.
    test(
      "Enum[A].{matchOn and parMatchOn} should match on a disjoint union of types from the SAME compilation run (regression)"
    ) {
      import ClassesFixtures.testEnumMatchOnAndParMatchOn
      import ClassesScala3Spec.LocalUnions.*

      def code(input: LocalParrot | LocalHamster) = testEnumMatchOnAndParMatchOn(input)
      code(LocalParrot("Polly", 7)) <==>
        "sequential: subtype name: hearth.typed.ClassesScala3Spec.LocalUnions.LocalParrot, expr: hearth.typed.ClassesScala3Spec.LocalUnions.LocalParrot, parallel: subtype name: hearth.typed.ClassesScala3Spec.LocalUnions.LocalParrot, expr: hearth.typed.ClassesScala3Spec.LocalUnions.LocalParrot"
      code(LocalHamster("Hammy", 4.5)) <==>
        "sequential: subtype name: hearth.typed.ClassesScala3Spec.LocalUnions.LocalHamster, expr: hearth.typed.ClassesScala3Spec.LocalUnions.LocalHamster, parallel: subtype name: hearth.typed.ClassesScala3Spec.LocalUnions.LocalHamster, expr: hearth.typed.ClassesScala3Spec.LocalUnions.LocalHamster"
    }

    test(
      "Type.directChildren should accept a disjoint union of types from the SAME compilation run (regression)"
    ) {
      import TypesFixtures.testUnionMembers
      import ClassesScala3Spec.LocalUnions.*

      testUnionMembers[LocalParrot | LocalHamster] <==> Data.map(
        "Type.isUnionType" -> Data(true),
        "Type.directChildren" -> Data.map(
          "hearth.typed.ClassesScala3Spec.LocalUnions.LocalParrot" -> Data(
            "hearth.typed.ClassesScala3Spec.LocalUnions.LocalParrot"
          ),
          "hearth.typed.ClassesScala3Spec.LocalUnions.LocalHamster" -> Data(
            "hearth.typed.ClassesScala3Spec.LocalUnions.LocalHamster"
          )
        ),
        "Type.exhaustiveChildren" -> Data.map(
          "hearth.typed.ClassesScala3Spec.LocalUnions.LocalParrot" -> Data(
            "hearth.typed.ClassesScala3Spec.LocalUnions.LocalParrot"
          ),
          "hearth.typed.ClassesScala3Spec.LocalUnions.LocalHamster" -> Data(
            "hearth.typed.ClassesScala3Spec.LocalUnions.LocalHamster"
          )
        )
      )
    }

    test(
      "Type.directChildren should still REFUSE a related-class union of SAME-compilation-run types (LocalBox[Int] | LocalBoxSub[String]) (regression)"
    ) {
      import TypesFixtures.testUnionMembers
      import ClassesScala3Spec.LocalUnions.*

      // LocalBoxSub <: LocalBox at the class level (but the MEMBERS themselves have no subtype relation, since
      // LocalBox[Int] is not <:< LocalBoxSub[String]), so symbol-level relatedness must still refuse this union.
      testUnionMembers[LocalBox[Int] | LocalBoxSub[String]] <==> Data.map(
        "Type.isUnionType" -> Data(true),
        "Type.directChildren" -> Data("<no direct children>"),
        "Type.exhaustiveChildren" -> Data("<no exhaustive children>")
      )
    }

    group("Scala 3 edge cases of class views") {
      import ClassesFixtures.{
        testCaseClassRuntimeRoundTrip,
        testCaseClassSummary,
        testClassViewKind,
        testEnumChildren,
        testEnumMatchOnRuntime,
        testSingletonRoundTrip
      }

      test("Class.apply picks the most specific view for Scala 3 enums, their cases and union types") {
        testClassViewKind[examples.ExampleEnum] <==> "Enum"
        testClassViewKind[examples.ExampleEnum.ExampleEnumValue.type] <==> "SingletonValue"
        testClassViewKind[examples.ExampleEnum.ExampleEnumClass] <==> "CaseClass"
        testClassViewKind[examples.unions.StringOrInt] <==> "Enum"
      }

      test("CaseClass for a case class with a using parameter list") {
        testCaseClassSummary[examples.classesedge.CCUsingParam] <==> Data.map(
          "primaryConstructor" -> Data("(i: scala.Int)(s: java.lang.String)"),
          "nonPrimaryConstructors" -> Data.list(),
          "caseFields" -> Data.list(Data("i: scala.Int"))
        )
        testCaseClassRuntimeRoundTrip[examples.classesedge.CCUsingParam] <==>
          "CCUsingParam(42)(hello) | CCUsingParam(42)(hello) | i=42"
      }

      test("CaseClass for a parameterized Scala 3 enum case") {
        testCaseClassRuntimeRoundTrip[examples.ExampleEnum.ExampleEnumClass] <==>
          "ExampleEnumClass(42) | ExampleEnumClass(42) | a=42"
      }

      test("SingletonValue for parameterless Scala 3 enum cases") {
        testSingletonRoundTrip[examples.ExampleEnum.ExampleEnumValue.type] <==> "ExampleEnumValue"
        testSingletonRoundTrip[examples.Color.Red.type] <==> "Red"
        testSingletonRoundTrip[examples.LowerEnum1.team.type] <==> "team"
      }

      test("Enum children of an applied generic Scala 3 enum") {
        testEnumChildren[examples.ExampleEnumWithTypeParam[Int]] <==> Data.map(
          "directChildren" -> Data.list(
            Data(
              "ExampleEnumWithTypeParamClass: hearth.examples.ExampleEnumWithTypeParam.ExampleEnumWithTypeParamClass[scala.Int]"
            ),
            Data(
              "ExampleEnumWithTypeParamValue: hearth.examples.ExampleEnumWithTypeParam.ExampleEnumWithTypeParamValue.type"
            )
          ),
          "exhaustiveChildren" -> Data.list(
            Data(
              "ExampleEnumWithTypeParamClass: hearth.examples.ExampleEnumWithTypeParam.ExampleEnumWithTypeParamClass[scala.Int]"
            ),
            Data(
              "ExampleEnumWithTypeParamValue: hearth.examples.ExampleEnumWithTypeParam.ExampleEnumWithTypeParamValue.type"
            )
          )
        )
      }

      test("Enum.{matchOn and parMatchOn} for an applied generic Scala 3 enum, evaluated at runtime") {
        def code(input: examples.ExampleEnumWithTypeParam[Int]) = testEnumMatchOnRuntime(input)
        code(examples.ExampleEnumWithTypeParam.ExampleEnumWithTypeParamClass(3)) <==>
          "hearth.examples.ExampleEnumWithTypeParam.ExampleEnumWithTypeParamClass[scala.Int]: ExampleEnumWithTypeParamClass(3) / hearth.examples.ExampleEnumWithTypeParam.ExampleEnumWithTypeParamClass[scala.Int]: ExampleEnumWithTypeParamClass(3)"
        code(examples.ExampleEnumWithTypeParam.ExampleEnumWithTypeParamValue) <==>
          "hearth.examples.ExampleEnumWithTypeParam.ExampleEnumWithTypeParamValue.type: ExampleEnumWithTypeParamValue / hearth.examples.ExampleEnumWithTypeParam.ExampleEnumWithTypeParamValue.type: ExampleEnumWithTypeParamValue"
      }

      test("Enum.{matchOn and parMatchOn} for lowercase parameterless Scala 3 enum cases, evaluated at runtime") {
        def code(input: examples.LowerEnum1) = testEnumMatchOnRuntime(input)
        code(examples.LowerEnum1.team) <==>
          "hearth.examples.LowerEnum1.team.type: team / hearth.examples.LowerEnum1.team.type: team"
        code(examples.LowerEnum1.school) <==>
          "hearth.examples.LowerEnum1.school.type: school / hearth.examples.LowerEnum1.school.type: school"
      }
    }
  }
}

object ClassesScala3Spec {

  /** Union member types defined in the SAME compilation unit as the spec that expands the macro - exercising the
    * compiler-symbol-level disjointness check (no classfile exists at macro-expansion time).
    */
  object LocalUnions {
    case class LocalParrot(name: String, vocabulary: Int)
    case class LocalHamster(name: String, wheelSize: Double)

    class LocalBox[A](val value: A)
    class LocalBoxSub[A](value: A) extends LocalBox[A](value)
  }
}
