package hearth
package typed

class CrossQuotesSpec extends MacroSuite {

  group("CrossQuotes macro/plugin") {

    group("for Type.of") {

      test("should work for simple types") {
        CrossQuotesFixtures.simpleType <==> "scala.Int"
      }

      test("should work for generic types") {
        CrossQuotesFixtures.genericType[Int] <==> "scala.collection.immutable.List[scala.Int]"
      }
    }

    group("for Expr.quote+Expr.splice") {

      test("should work for simple expressions") {
        CrossQuotesFixtures.simpleExpr <==> "3"
      }
    }
  }
}
