package hearth
package typed

import hearth.data.Data

import java.util.concurrent.TimeUnit

/** Macro implementation is in [[ExprsEdgeCasesFixturesImpl]] */
final class ExprsEdgeCasesJvmSpec extends MacroSuite {

  group("trait typed.Exprs edge cases (JVM)") {
    import ExprsEdgeCasesFixtures.*

    test("MatchCase.eqValue matches Java enum values") {
      val seconds: TimeUnit = List(TimeUnit.SECONDS).head
      testMatchCaseEqValueAgainst[TimeUnit](seconds, TimeUnit.SECONDS) <==> "matched!"
      testMatchCaseEqValueAgainst[TimeUnit](seconds, TimeUnit.MINUTES) <==> "other: SECONDS"
    }

    // ExprCodec.derived's toExpr is Expr.semiQuote, which (unlike the derived codec) reports a failure as a value
    // instead of aborting the compilation.
    test("Expr.semiQuote re-lifts the same Java enum value") {
      testSemiQuoteRoundTripAtRuntime[TimeUnit](TimeUnit.MINUTES) <==> Data.map(
        "decoded" -> Data("MINUTES"),
        "reLifted" -> Data("MINUTES")
      )
    }

    test("Expr.semiEval evaluates Java enum values") {
      testSemiEvalOutcome(TimeUnit.SECONDS) <==> Data("SECONDS (java.util.concurrent.TimeUnit)")
    }
  }
}
