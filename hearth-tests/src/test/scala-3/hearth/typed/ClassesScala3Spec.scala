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
  }
}
