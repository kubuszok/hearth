package hearth
package typed

import hearth.data.Data

/** Macro implementation is in [[ExprsFixturesImpl]] */
final class ExprsJvmSpec extends MacroSuite {

  group("typed.Exprs") {

    group("type Expr") {

      test("method Expr.singletonOf should work for Java enum children via directChildren") {
        import ExprsFixtures.testChildrenSingletonOf

        // Tests the actual derivation workflow: directChildren → singletonOf for each child
        testChildrenSingletonOf[examples.enums.ExampleJavaEnum] <==> Data.map(
          "VALUE1" -> Data("found"),
          "VALUE2" -> Data("found")
        )
        testChildrenSingletonOf[examples.enums.ExampleJavaEnumWithMethods] <==> Data.map(
          "VALUE1" -> Data("found"),
          "VALUE2" -> Data("found")
        )
      }

      group("method Expr.semiEval of Java static members") {
        import ExprsFixtures.testSemiEval

        test("evaluates a qualified Java enum constant") {
          testSemiEval(java.math.RoundingMode.FLOOR) <==> Data.map(
            "status" -> Data("success"),
            "value" -> Data("FLOOR"),
            "class" -> Data("java.math.RoundingMode")
          )
          testSemiEval(examples.enums.ExampleJavaEnum.VALUE2) <==> Data.map(
            "status" -> Data("success"),
            "value" -> Data("VALUE2"),
            "class" -> Data("hearth.examples.enums.ExampleJavaEnum")
          )
        }

        test("evaluates a qualified Java enum constant nested in another expression") {
          testSemiEval(List(java.math.RoundingMode.FLOOR, java.math.RoundingMode.CEILING)) <==> Data.map(
            "status" -> Data("success"),
            "value" -> Data("List(FLOOR, CEILING)"),
            "class" -> Data("scala.collection.immutable.$colon$colon")
          )
        }

        test("evaluates a Java enum constant imported by name") {
          import java.math.RoundingMode.HALF_UP
          testSemiEval(HALF_UP) <==> Data.map(
            "status" -> Data("success"),
            "value" -> Data("HALF_UP"),
            "class" -> Data("java.math.RoundingMode")
          )
        }

        test("evaluates a qualified Java static field") {
          testSemiEval(java.io.File.separator) <==> Data.map(
            "status" -> Data("success"),
            "value" -> Data(java.io.File.separator),
            "class" -> Data("java.lang.String")
          )
        }
      }
    }
  }
}
