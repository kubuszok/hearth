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

    group("with TypeName type class summoning") {

      implicit val stringTypeName: examples.TypeName[String] = examples.TypeName.instance[String]("MyString")
      implicit val intTypeName: examples.TypeName[Int] = examples.TypeName.instance[Int]("MyInt")

      test("simple type with TypeName instance") {
        assertEquals(
          RuntimeAwareTypePrinterFixtures.testWithTypeName[String],
          "MyString"
        )
      }

      test("Option[String] — overrides String via TypeName") {
        assertEquals(
          RuntimeAwareTypePrinterFixtures.testWithTypeName[Option[String]],
          "scala.Option[MyString]"
        )
      }

      test("Map[String, Int] — overrides both args via TypeName") {
        assertEquals(
          RuntimeAwareTypePrinterFixtures.testWithTypeName[Map[String, Int]],
          "scala.collection.immutable.Map[MyString, MyInt]"
        )
      }

      test("Boolean — no TypeName instance, falls back to plain print") {
        assertEquals(
          RuntimeAwareTypePrinterFixtures.testWithTypeName[Boolean],
          "scala.Boolean"
        )
      }
    }
  }
}
