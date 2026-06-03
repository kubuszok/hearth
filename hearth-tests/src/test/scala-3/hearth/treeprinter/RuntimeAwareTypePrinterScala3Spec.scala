package hearth
package treeprinter

final class RuntimeAwareTypePrinterScala3Spec extends MacroSuite {

  group("runtimeAwareTypePrint — union types (Scala 3 only)") {

    group("no overrides") {

      test("for String | Int") {
        assert(
          RuntimeAwareTypePrinterFixtures.testNoOverrideUnion[String | Int] ==
            "java.lang.String | scala.Int"
        )
      }

      test("for String | Int | Boolean") {
        assert(
          RuntimeAwareTypePrinterFixtures.testNoOverrideUnion[String | Int | Boolean] ==
            "java.lang.String | scala.Int | scala.Boolean"
        )
      }
    }

    group("with overrides") {

      test("String | Int — overrides String member") {
        assert(
          RuntimeAwareTypePrinterFixtures.testWithOverrideUnion[String | Int] ==
            "RUNTIME_STRING | RUNTIME_INT"
        )
      }

      test("String | Boolean — only String overridden") {
        assert(
          RuntimeAwareTypePrinterFixtures.testWithOverrideUnion[String | Boolean] ==
            "RUNTIME_STRING | scala.Boolean"
        )
      }

      test("Boolean | Double — no overrides match, falls through to plain print") {
        assert(
          RuntimeAwareTypePrinterFixtures.testWithOverrideUnion[Boolean | Double] ==
            "scala.Boolean | scala.Double"
        )
      }
    }

    group("short print with overrides") {

      test("String | Int") {
        assert(
          RuntimeAwareTypePrinterFixtures.testShortWithOverrideUnion[String | Int] ==
            "RUNTIME_STRING | Int"
        )
      }
    }
  }
}
