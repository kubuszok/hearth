package hearth
package typed

class CrossQuotesSpec extends MacroSuite {

  group("CrossQuotes macro/plugin") {

    group("for Type.of") {

      test("should work for simple types") {
        CrossQuotesFixtures.simpleType <==> "Int"
      }

      test("should work for generic types") {
        CrossQuotesFixtures.genericType[Int] <==> "List[Int]"
      }
    }

    group("for Expr.quote+Expr.splice") {

      test("should work for simple expressions") {
        CrossQuotesFixtures.simpleExpr <==> "3"
      }
    }
  }
}
