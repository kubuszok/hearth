package hearth
package typed

import hearth.data.Data

/** Macro implementation is in [[ExprsFixturesImpl]] */
final class ExprsSpec extends MacroSuite {

  group("trait typed.Exprs") {

    group("methods: Expr.{plainPrint, prettyPrint, plainAST, prettyAST}, expected behavior") {
      import ExprsFixtures.testExprPrinters

      test("should print expression") {
        testExprPrinters("test value") <==> Data.map(
          "Expr.plainPrint" -> Data("\"test value\""),
          "Expr.prettyPrint" -> Data("\"test value\""),
          "Expr.plainAST" -> Data(
            if (LanguageVersion.byHearth.isScala3) "Inlined(None, Nil, Literal(StringConstant(\"test value\")))"
            else "Literal(Constant(\"test value\"))"
          ),
          "Expr.prettyAST" -> Data(
            if (LanguageVersion.byHearth.isScala3) "Inlined(None, Nil, Literal(StringConstant(\"test value\")))"
            else "Literal(Constant(\"test value\"))"
          )
        )
      }
    }

    group("methods: Expr.summonImplicit, expected behavior") {
      import ExprsFixtures.testExprSummoning

      test("should summon implicit of available") {
        testExprSummoning[List[Int]] <==> Data.map("Expr.summonImplicit" -> Data("<failed to summon>"))
        testExprSummoning[DummyImplicit] <==> Data.map(
          "Expr.summonImplicit" -> Data(
            if (LanguageVersion.byHearth.isScala3) "scala.DummyImplicit.dummyImplicit"
            else "scala.this.DummyImplicit.dummyImplicit"
          )
        )
      }
    }

    group("methods: Expr.upcast, expected behavior") {
      import ExprsFixtures.testExprUpcasting

      test("should upcast expression in compile time if it's possible") {
        testExprUpcasting[Any, String]("string") <==> Data.map("Expr.upcast" -> Data("<failed to upcast>"))
        testExprUpcasting[List[Int], Seq[Any]](List(1, 2, 3)) <==> Data.map(
          "Expr.upcast" -> Data(
            if (LanguageVersion.byHearth.isScala3) "scala.List.apply[scala.Int](1, 2, 3)"
            else "(scala.`package`.List.apply[Int](1, 2, 3): Seq[Any])"
          )
        )
      }
    }

    group("methods: Expr.suppressUnused, expected behavior") {
      import ExprsFixtures.testSuppressUnused

      test("should suppress unused expression") {
        val expr = 1

        testSuppressUnused(expr)
      }
    }
  }
}
