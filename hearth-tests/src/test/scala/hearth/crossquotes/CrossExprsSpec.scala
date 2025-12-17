package hearth
package crossquotes

import hearth.data.Data

/** Macro implementation is in [[hearth.cq.CrossQuotesMacros]] (Scala 2) and [[hearth.cq.CrossQuotesPlugin]] (Scala 3).
  *
  * Fixtures are in [[CrossExprsFixturesImpl]].
  */
final class CrossExprsSpec extends MacroSuite {

  group("Cross-Quotes macro/plugin") {

    group("for Expr.quote+Expr.splice") {

      test("should handle all supported features and edge cases") {

        CrossExprsFixtures.testExprOf(4) <==> Data.map(
          "features" -> Data.map(
            "unsanitizedBlocks" -> Data("ListMap(1 -> 2)"),
            "multipleSplices" -> Data("3"),
            "generics" -> Data("4"),
            "nestedQuotesAndSplices" -> Data("42")
          ),
          "edgeCases" -> Data.map()
        )
      }
    }
  }
}
