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
    }
  }
}
