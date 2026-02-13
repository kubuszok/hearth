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
            "genericsAndImplicitlyPassedTypes" -> Data.map(
              "viaTypeBound" -> Data("4"),
              "viaImplicitParam" -> Data("4"),
              "viaImport" -> Data("4"),
              "viaDeclaredVal" -> Data("4")
            ),
            "nestedQuotesAndSplices" -> Data("42")
          ),
          "edgeCases" -> Data.map(
            "chainingOnSplice" -> Data("<name>"),
            "implicitTypeSubstitution" -> Data.map(
              "fromTypeParamInferred" -> Data("123"),
              "fromTypeParamExplicit" -> Data("123"),
              "fromNestedImport" -> Data("key: 1, value: onekey: 2, value: two"),
              "fromSplittedNestedImport" -> Data("key: 1, value: onekey: 2, value: two"),
              "fromSameClassImplicit" -> Data("key: 1, value: onekey: 2, value: two")
            )
          )
        )
      }
    }
  }
}
