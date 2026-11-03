package hearth
package treeprinter

final class RuntimeAwareTypePrinterSpec extends MacroSuite {

  group("runtimeAwareTypePrint") {

    group("no overrides — should match plainPrint exactly") {

      test("for Option[String]") {
        assert(
          RuntimeAwareTypePrinterFixtures.testNoOverride[Option[String]] ==
            "scala.Option[java.lang.String]"
        )
      }

      test("for simple type") {
        assert(
          RuntimeAwareTypePrinterFixtures.testNoOverride[String] ==
            "java.lang.String"
        )
      }

      test("for Map[String, Int]") {
        assert(
          RuntimeAwareTypePrinterFixtures.testNoOverride[Map[String, Int]] ==
            "scala.collection.immutable.Map[java.lang.String, scala.Int]"
        )
      }
    }

    group("with overrides") {

      test("simple type with override") {
        assert(
          RuntimeAwareTypePrinterFixtures.testWithOverride[String] ==
            "RUNTIME_STRING"
        )
      }

      test("simple type without matching override") {
        assert(
          RuntimeAwareTypePrinterFixtures.testWithOverride[Boolean] ==
            "scala.Boolean"
        )
      }

      test("Option[String] — overrides String arg") {
        assert(
          RuntimeAwareTypePrinterFixtures.testWithOverride[Option[String]] ==
            "scala.Option[RUNTIME_STRING]"
        )
      }

      test("Map[String, List[Int]] — nested overrides") {
        val actual = RuntimeAwareTypePrinterFixtures.testWithOverride[Map[String, List[Int]]]
        assertEquals(
          actual,
          "scala.collection.immutable.Map[RUNTIME_STRING, scala.collection.immutable.List[RUNTIME_INT]]"
        )
      }

      test("Option[List[String]] — deep nesting") {
        val actual = RuntimeAwareTypePrinterFixtures.testWithOverride[Option[List[String]]]
        assertEquals(actual, "scala.Option[scala.collection.immutable.List[RUNTIME_STRING]]")
      }
    }

    group("pretty print with overrides — contains ANSI escape codes") {

      test("Option[String] — has ANSI and override value") {
        val result = RuntimeAwareTypePrinterFixtures.testPrettyWithOverride[Option[String]]
        assert(result.contains("RUNTIME_STRING"))
      }
    }

    group("short print with overrides") {

      test("Option[String]") {
        assert(
          RuntimeAwareTypePrinterFixtures.testShortWithOverride[Option[String]] ==
            "Option[RUNTIME_STRING]"
        )
      }

      test("simple type without matching override") {
        assert(
          RuntimeAwareTypePrinterFixtures.testShortWithOverride[Boolean] ==
            "Boolean"
        )
      }
    }

    group("other kinds of types") {
      import hearth.examples.miscedge.*
      import RuntimeAwareTypePrinterFixtures.*

      test("tuple, function and array arguments can be overridden") {
        testWithOverride[(String, Int)] <==> "scala.Tuple2[RUNTIME_STRING, RUNTIME_INT]"
        testWithOverride[String => Int] <==> "scala.Function1[RUNTIME_STRING, RUNTIME_INT]"
        testWithOverride[Array[String]] <==> "scala.Array[RUNTIME_STRING]"
      }

      test("arguments of a user-defined generic class can be overridden") {
        testWithOverride[Box[Either[Int, String]]] <==>
          "hearth.examples.miscedge.Box[scala.util.Either[RUNTIME_INT, RUNTIME_STRING]]"
        testShortWithOverride[Box[String]] <==> "Box[RUNTIME_STRING]"
      }

      test("without overrides a type alias is printed like plainPrint prints it") {
        testNoOverride[Aliases.StringOption] <==> testPlainPrint[Aliases.StringOption]
        testNoOverride[Aliases.StringOption] <==> "scala.Option[java.lang.String]"
        testWithOverride[Aliases.StringOption] <==> "scala.Option[RUNTIME_STRING]"
      }

      test("non-applied types without matching overrides are printed like plainPrint prints them") {
        testWithOverride[Outer.Nested] <==> "hearth.examples.miscedge.Outer.Nested"
        testShortWithOverride[Outer.Nested] <==> "Nested"
      }
    }
  }
}
