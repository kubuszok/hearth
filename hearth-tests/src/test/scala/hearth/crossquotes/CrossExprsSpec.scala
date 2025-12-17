package hearth
package crossquotes

/** Macro implementation is in [[hearth.cq.CrossQuotesMacros]] (Scala 2) and [[hearth.cq.CrossQuotesPlugin]] (Scala 3).
  *
  * Fixtures are in [[CrossExprsFixturesImpl]].
  */
final class CrossExprsSpec extends MacroSuite {

  group("Cross-Quotes macro/plugin") {

    group("for Expr.quote+Expr.splice") {

      test("should work for simple expressions") {
        CrossExprsFixtures.simpleExpr <==> "3"
      }

      test("should work for generic expressions") {
        CrossExprsFixtures.genericExpr(4) <==> "4"
      }

      test("should work for unsanitized expressions") {
        CrossExprsFixtures.unsanitizedExpr <==> "ListMap(1 -> 2)"
      }

      test("should work for nested expressions") {
        CrossExprsFixtures.nestedExpr <==> "42"
      }
    }
  }
}
