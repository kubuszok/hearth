package hearth
package typed

import hearth.data.Data

/** Macro implementation is in [[ExprsFixturesImpl]] */
final class ExprsNewestSpec extends MacroSuite {

  group("trait typed.Exprs") {

    group("methods: Expr.summonImplicitIgnoring, expected behavior") {
      import ExprsFixtures.testExprSummoningIgnoring

      test("should summon implicit of available") {
        testExprSummoningIgnoring[DummyImplicit, DummyImplicit.type]("dummyImplicit") <==> Data.map(
          "Expr.summonImplicitIgnoring" -> Data(
            "<failed to summon: No implicit value of type scala.DummyImplicit found>"
          )
        )
      }
    }
  }
}
