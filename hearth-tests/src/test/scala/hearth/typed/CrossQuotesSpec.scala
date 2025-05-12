package hearth
package typed

class CrossQuotesSpec extends MacroSuite{
  
  group("CrossQuotes macro/plugin") {

    group("for Expr.quote+Expr.splice") {

      test("should work for simple expressions") {
        CrossQuotesFixtures.simpleExpr <==> "3"
      }
    }
  }
}
