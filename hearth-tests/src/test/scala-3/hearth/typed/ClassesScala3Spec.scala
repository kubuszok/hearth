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
  }
}
