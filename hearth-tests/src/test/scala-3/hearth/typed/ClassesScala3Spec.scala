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
      "Enum[A].{matchOn and parMatchOn} should return <no enum> for non-disjoint union type (List[Int] | List[String])"
    ) {
      import ClassesFixtures.testEnumMatchOnAndParMatchOn

      def code(input: examples.unions.ListIntOrListString) = testEnumMatchOnAndParMatchOn(input)
      code(List(1)) <==> "<no enum>"
    }
  }
}
