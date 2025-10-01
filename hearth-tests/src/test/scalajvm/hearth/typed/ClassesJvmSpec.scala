package hearth
package typed

import hearth.data.Data

/** Macro implementation is in [[ClassesFixturesImpl]] */
final class ClassesJvmSpec extends MacroSuite {

  group("trait typed.Classes") {

    group("class: Class[A], returns preprocessed class") {
      import ClassesFixtures.testClass

      test("for Java Bean") {

        // It seems some methods exist only on Scala 2.13, and some only on Scala 3.
        val methods =
          if (LanguageVersion.byHearth.isScala2_13)
            Data.list(
              Data.map("name" -> Data("getBoolean"), "parameters" -> Data("()")),
              Data.map("name" -> Data("getInt"), "parameters" -> Data("()")),
              Data.map("name" -> Data("getString"), "parameters" -> Data("()")),
              Data.map("name" -> Data("setBoolean"), "parameters" -> Data("(x$1: scala.Boolean)")),
              Data.map("name" -> Data("setInt"), "parameters" -> Data("(x$1: scala.Int)")),
              Data.map("name" -> Data("setString"), "parameters" -> Data("(x$1: java.lang.String)"))
            )
          else
            Data.list(
              Data.map("name" -> Data("getBoolean"), "parameters" -> Data("()")),
              Data.map("name" -> Data("getInt"), "parameters" -> Data("()")),
              Data.map("name" -> Data("getString"), "parameters" -> Data("()")),
              Data.map("name" -> Data("setBoolean"), "parameters" -> Data("(x$0: scala.Boolean)")),
              Data.map("name" -> Data("setInt"), "parameters" -> Data("(x$0: scala.Int)")),
              Data.map("name" -> Data("setString"), "parameters" -> Data("(x$0: java.lang.String)"))
            )

        testClass[examples.classes.ExampleJavaClass](
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
            "methods" -> methods
          ),
          "asCaseClass" -> Data("<no case class>"),
          "asEnum" -> Data("<no enum>"),
          "asJavaBean" -> Data.map(
            "defaultConstructor" -> Data("()")
          )
        )
      }

      test("for Java enum") {

        val constructors = if (LanguageVersion.byHearth.isScala2_13) Data.list() else Data.list(Data("()"))

        // It seems some methods exist only on Scala 2.13, and some only on Scala 3.
        val methods =
          if (LanguageVersion.byHearth.isScala2_13)
            Data.list(
              Data.map("name" -> Data("VALUE1"), "parameters" -> Data("")),
              Data.map("name" -> Data("VALUE2"), "parameters" -> Data("")),
              Data.map("name" -> Data("compareTo"), "parameters" -> Data("(x$1: java.lang.Object)")),
              Data
                .map("name" -> Data("compareTo"), "parameters" -> Data("(x$1: hearth.examples.enums.ExampleJavaEnum)")),
              Data.map("name" -> Data("getDeclaringClass"), "parameters" -> Data("()")),
              Data.map("name" -> Data("name"), "parameters" -> Data("()")),
              Data.map("name" -> Data("ordinal"), "parameters" -> Data("()")),
              Data.map("name" -> Data("valueOf"), "parameters" -> Data("(x$1: java.lang.String)")),
              Data.map("name" -> Data("values"), "parameters" -> Data("()"))
            )
          else
            Data.list(
              Data.map("name" -> Data("VALUE1"), "parameters" -> Data("")),
              Data.map("name" -> Data("VALUE2"), "parameters" -> Data("")),
              Data
                .map("name" -> Data("compareTo"), "parameters" -> Data("(x$0: hearth.examples.enums.ExampleJavaEnum)")),
              Data.map("name" -> Data("getDeclaringClass"), "parameters" -> Data("()")),
              Data.map("name" -> Data("name"), "parameters" -> Data("()")),
              Data.map("name" -> Data("ordinal"), "parameters" -> Data("()")),
              Data.map("name" -> Data("valueOf"), "parameters" -> Data("(x$0: java.lang.String)")),
              Data.map("name" -> Data("values"), "parameters" -> Data("()"))
            )

        testClass[examples.enums.ExampleJavaEnum](
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
            "constructors" -> constructors,
            "methods" -> methods
          ),
          "asCaseClass" -> Data("<no case class>"),
          "asEnum" -> Data.map(
            "directChildren" -> Data(
              "(VALUE1: hearth.examples.enums.ExampleJavaEnum.VALUE1.type, VALUE2: hearth.examples.enums.ExampleJavaEnum.VALUE2.type)"
            ),
            "exhaustiveChildren" -> Data(
              "(VALUE1: hearth.examples.enums.ExampleJavaEnum.VALUE1.type, VALUE2: hearth.examples.enums.ExampleJavaEnum.VALUE2.type)"
            )
          ),
          "asJavaBean" -> Data("<no java bean>")
        )
      }
    }
  }
}
