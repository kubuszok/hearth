package hearth
package typed

import hearth.data.Data

/** Macro implementation is in [[ExprsEdgeCasesFixturesImpl]] */
final class ExprsEdgeCasesScala2Spec extends MacroSuite {

  group("trait typed.Exprs edge cases (Scala 2)") {

    test("VarArgs of a splatted Seq give a single Seq-typed expression, which toIterableOption reports as None") {
      import ExprsEdgeCasesFixtures.testVarArgsConversions

      val more = Seq(1, 2)
      val _ = more
      testVarArgsConversions(more: _*) <==> Data.map(
        "toIterable" -> Data(1),
        "toSeq" -> Data(1),
        "toList" -> Data(1),
        "toVector" -> Data(1),
        "to(List)" -> Data(1),
        "toIterableOption" -> Data("<None>")
      )
    }

    test("Expr.summonImplicit reports a diverging implicit search as Diverging") {
      import ExprsEdgeCasesFixtures.testSummoningOutcome

      testSummoningOutcome[hearth.examples.exprsedge.DivergingEvidence[Int]] <==> Data.map(
        "outcome" -> Data("diverging"),
        "isDefined" -> Data(false),
        "isEmpty" -> Data(true),
        "nonEmpty" -> Data(false),
        "toOption" -> Data(false),
        "get" -> Data("NoSuchElementException")
      )
    }

    test("MatchCase.typeTestMatch always fails on Scala 2, which has no TypeTest") {
      import ExprsEdgeCasesFixtures.testMatchCaseTypeTestMatch

      testMatchCaseTypeTestMatch[Any, String]("abc") <==> "<AssertionError>"
    }
  }
}
