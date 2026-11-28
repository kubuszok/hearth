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
          "asSingleton" -> Data("<no singleton>"),
          "asNamedTuple" -> Data("<no named tuple>"),
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
          "##",
          "describeConstable" // JDK 12+ but it started popping out in JDK 17+(?)
        ) <==> Data.map(
          "commons" -> Data.map(
            "constructors" -> constructors,
            "methods" -> methods
          ),
          "asSingleton" -> Data("<no singleton>"),
          "asNamedTuple" -> Data("<no named tuple>"),
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

    test("Enum[A].{matchOn and parMatchOn} should match on the Java enum") {
      import ClassesFixtures.testEnumMatchOnAndParMatchOn

      def code(input: hearth.examples.enums.ExampleJavaEnum) = testEnumMatchOnAndParMatchOn(input)
      code(hearth.examples.enums.ExampleJavaEnum.VALUE1) <==>
        "sequential: subtype name: hearth.examples.enums.ExampleJavaEnum.VALUE1.type, expr: VALUE1, parallel: subtype name: hearth.examples.enums.ExampleJavaEnum.VALUE1.type, expr: VALUE1"
      code(hearth.examples.enums.ExampleJavaEnum.VALUE2) <==>
        "sequential: subtype name: hearth.examples.enums.ExampleJavaEnum.VALUE2.type, expr: VALUE2, parallel: subtype name: hearth.examples.enums.ExampleJavaEnum.VALUE2.type, expr: VALUE2"
    }

    group("Java edge cases of class views") {
      import ClassesFixtures.{
        testClassViewKind,
        testJavaBeanConstructWithoutSettersRoundTrip,
        testJavaBeanSummary,
        testSingletonRoundTrip
      }

      // Scala 2 widens a Java enum value's singleton type written in the source (`ExampleJavaEnum.VALUE1.type`) to the
      // enum before a macro sees it; only the values' types listed by `directChildren` keep the value.
      test("Class.apply picks the most specific view for Java enums, their values and Java beans") {
        testClassViewKind[examples.enums.ExampleJavaEnum] <==> "Enum"
        testClassViewKind[examples.enums.ExampleJavaEnum.VALUE1.type] <==>
          (if (LanguageVersion.byHearth.isScala2_13) "Enum" else "SingletonValue")
        testClassViewKind[examples.classes.ExampleJavaClass] <==> "JavaBean"
      }

      test("SingletonValue for Java enum values") {
        val noSingletonOnScala2 =
          (value: String) => if (LanguageVersion.byHearth.isScala2_13) "<no singleton>" else value
        testSingletonRoundTrip[examples.enums.ExampleJavaEnum.VALUE1.type] <==> noSingletonOnScala2("VALUE1")
        testSingletonRoundTrip[examples.enums.ExampleJavaEnum.VALUE2.type] <==> noSingletonOnScala2("VALUE2")
      }

      test("JavaBean getters and setters of a Java class") {
        val setterParameter = if (LanguageVersion.byHearth.isScala2_13) "x$1" else "x$0"
        testJavaBeanSummary[examples.classes.ExampleJavaClass] <==> Data.map(
          "defaultConstructor" -> Data("()"),
          "beanGetters" -> Data.list(Data("getBoolean"), Data("getClass"), Data("getInt"), Data("getString")),
          "beanSetters" -> Data.list(
            Data.map("name" -> Data("setBoolean"), "parameters" -> Data(s"($setterParameter: scala.Boolean)")),
            Data.map("name" -> Data("setInt"), "parameters" -> Data(s"($setterParameter: scala.Int)")),
            Data.map("name" -> Data("setString"), "parameters" -> Data(s"($setterParameter: java.lang.String)"))
          )
        )
      }

      test("JavaBean.constructWithoutSetters for a Java class") {
        testJavaBeanConstructWithoutSettersRoundTrip[
          examples.classes.ExampleJavaClass
        ] <==> "ExampleJavaClass(, 0, false)"
      }
    }

    test("JavaBean round-trip: construct with setters and evaluate at runtime") {
      import ClassesFixtures.testJavaBeanConstructRoundTrip

      testJavaBeanConstructRoundTrip[examples.classes.ExampleJavaClass] <==>
        "ExampleJavaClass(hello, 42, true)"
    }

    test(
      "JavaBean[A].{constructWithSetters and parConstructWithSetters} should construct an instance of the Java bean"
    ) {
      import ClassesFixtures.testJavaBeanConstructWithSettersAndParConstructWithSetters

      testJavaBeanConstructWithSettersAndParConstructWithSetters[examples.classes.ExampleJavaClass] <==>
        (if (LanguageVersion.byHearth.isScala2_13)
           """sequential:
             |{
             |  val examplejavaclass: hearth.examples.classes.ExampleJavaClass = new hearth.examples.classes.ExampleJavaClass();
             |  {
             |    examplejavaclass.setString("x$1");
             |    {
             |      examplejavaclass.setInt(0);
             |      {
             |        examplejavaclass.setBoolean(true);
             |        examplejavaclass
             |      }
             |    }
             |  }
             |}
             |parallel:
             |{
             |  val examplejavaclass: hearth.examples.classes.ExampleJavaClass = new hearth.examples.classes.ExampleJavaClass();
             |  {
             |    examplejavaclass.setString("x$1");
             |    {
             |      examplejavaclass.setInt(0);
             |      {
             |        examplejavaclass.setBoolean(true);
             |        examplejavaclass
             |      }
             |    }
             |  }
             |}""".stripMargin
         else
           """sequential:
             |{
             |  val examplejavaclass: hearth.examples.classes.ExampleJavaClass = new hearth.examples.classes.ExampleJavaClass()
             |  examplejavaclass.setString("x$0")
             |  examplejavaclass.setInt(0)
             |  examplejavaclass.setBoolean(true)
             |  examplejavaclass
             |}
             |parallel:
             |{
             |  val examplejavaclass: hearth.examples.classes.ExampleJavaClass = new hearth.examples.classes.ExampleJavaClass()
             |  examplejavaclass.setString("x$0")
             |  examplejavaclass.setInt(0)
             |  examplejavaclass.setBoolean(true)
             |  examplejavaclass
             |}""".stripMargin)
    }
    group("Enum[A] of a Java enum read from a class file") {
      import ClassesFixtures.{testChildrenInOrder, testEnumMatchOnRuntime}

      test("lists every constant, also when nothing in the compilation refers to one") {
        // Nothing in this module refers to a constant of `AccessMode`.
        testChildrenInOrder[java.nio.file.AccessMode] <==> Data.map(
          "declarationWhenAvailable" -> Data.list(Data("READ"), Data("WRITE"), Data("EXECUTE")),
          "byName" -> Data.list(Data("EXECUTE"), Data("READ"), Data("WRITE")),
          "byName (extension method)" -> Data.list(Data("EXECUTE"), Data("READ"), Data("WRITE"))
        )
      }

      test("lists the constants in declaration order, also when the compilation refers to some of them") {
        val referred = java.time.format.TextStyle.SHORT_STANDALONE
        assert(referred.ordinal == 3)
        val declared = List("FULL", "FULL_STANDALONE", "SHORT", "SHORT_STANDALONE", "NARROW", "NARROW_STANDALONE")
        testChildrenInOrder[java.time.format.TextStyle] <==> Data.map(
          "declarationWhenAvailable" -> Data(declared.map(Data(_))),
          "byName" -> Data(declared.sorted.map(Data(_))),
          "byName (extension method)" -> Data(declared.sorted.map(Data(_)))
        )
      }

      test("matches on a Java enum compiled in the same run and on one read from a class file") {
        // Passing a constant straight to the macro would let scalac fold the scrutinee and flag the other cases.
        def sameRun(value: examples.enums.ExampleJavaEnum) = testEnumMatchOnRuntime(value)
        def classFile(value: java.util.concurrent.TimeUnit) = testEnumMatchOnRuntime(value)
        sameRun(examples.enums.ExampleJavaEnum.VALUE2) <==>
          "hearth.examples.enums.ExampleJavaEnum.VALUE2.type: VALUE2 / hearth.examples.enums.ExampleJavaEnum.VALUE2.type: VALUE2"
        classFile(java.util.concurrent.TimeUnit.SECONDS) <==>
          "java.util.concurrent.TimeUnit.SECONDS.type: SECONDS / java.util.concurrent.TimeUnit.SECONDS.type: SECONDS"
      }

      test("matches on a Java enum handing out the matched value typed as the constant") {
        import ClassesFixtures.testEnumMatchOnAsChild
        def sameRun(value: examples.enums.ExampleJavaEnum) = testEnumMatchOnAsChild(value)
        def classFile(value: java.util.concurrent.TimeUnit) = testEnumMatchOnAsChild(value)
        sameRun(examples.enums.ExampleJavaEnum.VALUE2) <==> "VALUE2"
        classFile(java.util.concurrent.TimeUnit.SECONDS) <==> "SECONDS"
      }

      test("handles a Java enum nested in a Java class") {
        val declared = List("NEW", "RUNNABLE", "BLOCKED", "WAITING", "TIMED_WAITING", "TERMINATED")
        testChildrenInOrder[java.lang.Thread.State] <==> Data.map(
          "declarationWhenAvailable" -> Data(declared.map(Data(_))),
          "byName" -> Data(declared.sorted.map(Data(_))),
          "byName (extension method)" -> Data(declared.sorted.map(Data(_)))
        )
        ClassesFixtures.testSingletonExpr[java.lang.Thread.State] <==> "<no singleton>"
        def nested(value: java.lang.Thread.State) = testEnumMatchOnRuntime(value)
        nested(java.lang.Thread.State.BLOCKED) <==>
          "java.lang.Thread.State.BLOCKED.type: BLOCKED / java.lang.Thread.State.BLOCKED.type: BLOCKED"
      }
    }
  }
}
