package hearth
package typed

import hearth.data.Data

import scala.quoted.*

final private class ExprsEdgeCasesFixtures(q: Quotes) extends MacroCommonsScala3(using q), ExprsEdgeCasesFixturesImpl {

  private val IntTypeS3: Type[Int] = Type.of[Int]

  def testIArrayRoundtrip: Expr[Data] = {
    given scala.quoted.Quotes = q
    implicit val intType: Type[Int] = IntTypeS3
    val encoded = Expr(IArray[Int](1, 2, 3))
    val decoded = Expr(Expr.unapply(encoded).fold("<not decoded>")(_.mkString("IArray(", ", ", ")")))
    '{
      Data.map(
        "decoded" -> Data($decoded),
        "runtime" -> Data($encoded.mkString("IArray(", ", ", ")"))
      )
    }
  }
}

object ExprsEdgeCasesFixtures {

  inline def testIArrayRoundtrip: Data = ${ testIArrayRoundtripImpl }
  private def testIArrayRoundtripImpl(using q: Quotes): Expr[Data] = new ExprsEdgeCasesFixtures(q).testIArrayRoundtrip

  inline def testExprTpe[A](inline expr: A): Data = ${ testExprTpeImpl[A]('expr) }
  private def testExprTpeImpl[A: Type](expr: Expr[A])(using q: Quotes): Expr[Data] =
    new ExprsEdgeCasesFixtures(q).testExprTpe[A](expr)

  inline def testExistentialExpr[A](inline expr: A): Data = ${ testExistentialExprImpl[A]('expr) }
  private def testExistentialExprImpl[A: Type](expr: Expr[A])(using q: Quotes): Expr[Data] =
    new ExprsEdgeCasesFixtures(q).testExistentialExpr[A](expr)

  inline def testSummoningOutcome[A]: Data = ${ testSummoningOutcomeImpl[A] }
  private def testSummoningOutcomeImpl[A: Type](using q: Quotes): Expr[Data] =
    new ExprsEdgeCasesFixtures(q).testSummoningOutcome[A]

  inline def testExprValueInt(inline expr: Int): Data = ${ testExprValueIntImpl('expr) }
  private def testExprValueIntImpl(expr: Expr[Int])(using q: Quotes): Expr[Data] =
    new ExprsEdgeCasesFixtures(q).testExprValueInt(expr)

  inline def testExprValueString(inline expr: String): Data = ${ testExprValueStringImpl('expr) }
  private def testExprValueStringImpl(expr: Expr[String])(using q: Quotes): Expr[Data] =
    new ExprsEdgeCasesFixtures(q).testExprValueString(expr)

  inline def testSemiEvalOutcome[A](inline expr: A): Data = ${ testSemiEvalOutcomeImpl[A]('expr) }
  private def testSemiEvalOutcomeImpl[A: Type](expr: Expr[A])(using q: Quotes): Expr[Data] =
    new ExprsEdgeCasesFixtures(q).testSemiEvalOutcome[A](expr)

  inline def testSemiEvalApply[F](inline function: F, inline args: Any*): Data = ${
    testSemiEvalApplyImpl[F]('function, 'args)
  }
  private def testSemiEvalApplyImpl[F: Type](function: Expr[F], args: Expr[Seq[Any]])(using q: Quotes): Expr[Data] =
    new ExprsEdgeCasesFixtures(q).testSemiEvalApply[F](function, args)

  inline def testSemiEvalWithOverride[A](inline mode: String, inline expr: A): Data = ${
    testSemiEvalWithOverrideImpl[A]('mode, 'expr)
  }
  private def testSemiEvalWithOverrideImpl[A: Type](mode: Expr[String], expr: Expr[A])(using q: Quotes): Expr[Data] =
    new ExprsEdgeCasesFixtures(q).testSemiEvalWithOverride[A](mode, expr)

  inline def testSemiQuoteBuiltInsAndStructures: Data = ${ testSemiQuoteBuiltInsAndStructuresImpl }
  private def testSemiQuoteBuiltInsAndStructuresImpl(using q: Quotes): Expr[Data] =
    new ExprsEdgeCasesFixtures(q).testSemiQuoteBuiltInsAndStructures

  inline def testSemiQuoteWithFailingOverride: Data = ${ testSemiQuoteWithFailingOverrideImpl }
  private def testSemiQuoteWithFailingOverrideImpl(using q: Quotes): Expr[Data] =
    new ExprsEdgeCasesFixtures(q).testSemiQuoteWithFailingOverride

  inline def testBuiltInCodecEdgeCases: Data = ${ testBuiltInCodecEdgeCasesImpl }
  private def testBuiltInCodecEdgeCasesImpl(using q: Quotes): Expr[Data] =
    new ExprsEdgeCasesFixtures(q).testBuiltInCodecEdgeCases

  inline def testDataCodecRoundtrips: Data = ${ testDataCodecRoundtripsImpl }
  private def testDataCodecRoundtripsImpl(using q: Quotes): Expr[Data] =
    new ExprsEdgeCasesFixtures(q).testDataCodecRoundtrips

  inline def testExprCodecDerivedAtRuntime[A](inline expr: A): Data = ${ testExprCodecDerivedAtRuntimeImpl[A]('expr) }
  private def testExprCodecDerivedAtRuntimeImpl[A: Type](expr: Expr[A])(using q: Quotes): Expr[Data] =
    new ExprsEdgeCasesFixtures(q).testExprCodecDerivedAtRuntime[A](expr)

  inline def testSemiQuoteRoundTripAtRuntime[A](inline expr: A): Data = ${
    testSemiQuoteRoundTripAtRuntimeImpl[A]('expr)
  }
  private def testSemiQuoteRoundTripAtRuntimeImpl[A: Type](expr: Expr[A])(using q: Quotes): Expr[Data] =
    new ExprsEdgeCasesFixtures(q).testSemiQuoteRoundTripAtRuntime[A](expr)

  inline def testSingletonOfAtRuntime[A]: Data = ${ testSingletonOfAtRuntimeImpl[A] }
  private def testSingletonOfAtRuntimeImpl[A: Type](using q: Quotes): Expr[Data] =
    new ExprsEdgeCasesFixtures(q).testSingletonOfAtRuntime[A]

  inline def testVarArgsConversions[A](inline args: A*): Data = ${ testVarArgsConversionsImpl[A]('args) }
  private def testVarArgsConversionsImpl[A: Type](args: Expr[Seq[A]])(using q: Quotes): Expr[Data] =
    new ExprsEdgeCasesFixtures(q).testVarArgsConversions[A](args)

  inline def testFreshNames[A](inline expr: A): Data = ${ testFreshNamesImpl[A]('expr) }
  private def testFreshNamesImpl[A: Type](expr: Expr[A])(using q: Quotes): Expr[Data] =
    new ExprsEdgeCasesFixtures(q).testFreshNames[A](expr)

  inline def testMatchCaseEqValueAgainst[A](inline expr: A, inline expected: A): String = ${
    testMatchCaseEqValueAgainstImpl[A]('expr, 'expected)
  }
  private def testMatchCaseEqValueAgainstImpl[A: Type](expr: Expr[A], expected: Expr[A])(using
      q: Quotes
  ): Expr[String] =
    new ExprsEdgeCasesFixtures(q).testMatchCaseEqValueAgainst[A](expr, expected)

  inline def testMatchCaseTypeTestMatch[A, B](inline expr: A): String = ${ testMatchCaseTypeTestMatchImpl[A, B]('expr) }
  private def testMatchCaseTypeTestMatchImpl[A: Type, B: Type](expr: Expr[A])(using q: Quotes): Expr[String] =
    new ExprsEdgeCasesFixtures(q).testMatchCaseTypeTestMatch[A, B](expr)

  inline def testDirectStyleWithoutRunSafe: Data = ${ testDirectStyleWithoutRunSafeImpl }
  private def testDirectStyleWithoutRunSafeImpl(using q: Quotes): Expr[Data] =
    new ExprsEdgeCasesFixtures(q).testDirectStyleWithoutRunSafe

  inline def testValDefsEvaluationCount(inline kind: String, inline effect: Int, inline useIt: Boolean): Int = ${
    testValDefsEvaluationCountImpl('kind, 'effect, 'useIt)
  }
  private def testValDefsEvaluationCountImpl(kind: Expr[String], effect: Expr[Int], useIt: Expr[Boolean])(using
      q: Quotes
  ): Expr[Int] =
    new ExprsEdgeCasesFixtures(q).testValDefsEvaluationCount(kind, effect, useIt)

  inline def testValDefsSamePrefixDoNotClash: Int = ${ testValDefsSamePrefixDoNotClashImpl }
  private def testValDefsSamePrefixDoNotClashImpl(using q: Quotes): Expr[Int] =
    new ExprsEdgeCasesFixtures(q).testValDefsSamePrefixDoNotClash

  inline def testValDefBuilderRecursiveDef(inline n: Int): Int = ${ testValDefBuilderRecursiveDefImpl('n) }
  private def testValDefBuilderRecursiveDefImpl(n: Expr[Int])(using q: Quotes): Expr[Int] =
    new ExprsEdgeCasesFixtures(q).testValDefBuilderRecursiveDef(n)

  inline def testValDefBuilderMixedTypes: String = ${ testValDefBuilderMixedTypesImpl }
  private def testValDefBuilderMixedTypesImpl(using q: Quotes): Expr[String] =
    new ExprsEdgeCasesFixtures(q).testValDefBuilderMixedTypes

  inline def testValDefsCacheMutualRecursion(inline n: Int): Data = ${ testValDefsCacheMutualRecursionImpl('n) }
  private def testValDefsCacheMutualRecursionImpl(n: Expr[Int])(using q: Quotes): Expr[Data] =
    new ExprsEdgeCasesFixtures(q).testValDefsCacheMutualRecursion(n)

  inline def testValDefsCacheMLocal: Data = ${ testValDefsCacheMLocalImpl }
  private def testValDefsCacheMLocalImpl(using q: Quotes): Expr[Data] =
    new ExprsEdgeCasesFixtures(q).testValDefsCacheMLocal

  inline def testLambdaBuilderMixedTypes: String = ${ testLambdaBuilderMixedTypesImpl }
  private def testLambdaBuilderMixedTypesImpl(using q: Quotes): Expr[String] =
    new ExprsEdgeCasesFixtures(q).testLambdaBuilderMixedTypes
}
