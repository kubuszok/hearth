package hearth
package typed

import hearth.data.Data

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

final private class ExprsEdgeCasesFixtures(val c: blackbox.Context)
    extends MacroCommonsScala2
    with ExprsEdgeCasesFixturesImpl {

  def testExprTpeImpl[A: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[Data] = testExprTpe[A](expr)

  def testExistentialExprImpl[A: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[Data] = testExistentialExpr[A](expr)

  def testSummoningOutcomeImpl[A: c.WeakTypeTag]: c.Expr[Data] = testSummoningOutcome[A]

  def testExprValueIntImpl(expr: c.Expr[Int]): c.Expr[Data] = testExprValueInt(expr)

  def testExprValueStringImpl(expr: c.Expr[String]): c.Expr[Data] = testExprValueString(expr)

  def testSemiEvalOutcomeImpl[A: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[Data] = testSemiEvalOutcome[A](expr)

  def testSemiEvalApplyImpl[F: c.WeakTypeTag](function: c.Expr[F], args: c.Expr[Any]*): c.Expr[Data] =
    testSemiEvalApply[F](function, args)

  def testSemiEvalWithOverrideImpl[A: c.WeakTypeTag](mode: c.Expr[String], expr: c.Expr[A]): c.Expr[Data] =
    testSemiEvalWithOverride[A](mode, expr)

  def testSemiQuoteBuiltInsAndStructuresImpl: c.Expr[Data] = testSemiQuoteBuiltInsAndStructures

  def testSemiQuoteWithFailingOverrideImpl: c.Expr[Data] = testSemiQuoteWithFailingOverride

  def testBuiltInCodecEdgeCasesImpl: c.Expr[Data] = testBuiltInCodecEdgeCases

  def testDataCodecRoundtripsImpl: c.Expr[Data] = testDataCodecRoundtrips

  def testExprCodecDerivedAtRuntimeImpl[A: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[Data] =
    testExprCodecDerivedAtRuntime[A](expr)

  def testSemiQuoteRoundTripAtRuntimeImpl[A: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[Data] =
    testSemiQuoteRoundTripAtRuntime[A](expr)

  def testSingletonOfAtRuntimeImpl[A: c.WeakTypeTag]: c.Expr[Data] = testSingletonOfAtRuntime[A]

  def testVarArgsConversionsImpl[A: c.WeakTypeTag](args: c.Expr[A]*): c.Expr[Data] = testVarArgsConversions[A](args)

  def testFreshNamesImpl[A: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[Data] = testFreshNames[A](expr)

  def testMatchCaseEqValueAgainstImpl[A: c.WeakTypeTag](expr: c.Expr[A], expected: c.Expr[A]): c.Expr[String] =
    testMatchCaseEqValueAgainst[A](expr, expected)

  def testMatchCaseTypeTestMatchImpl[A: c.WeakTypeTag, B: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[String] =
    testMatchCaseTypeTestMatch[A, B](expr)

  def testDirectStyleWithoutRunSafeImpl: c.Expr[Data] = testDirectStyleWithoutRunSafe

  def testValDefsEvaluationCountImpl(
      kind: c.Expr[String],
      effect: c.Expr[Int],
      useIt: c.Expr[Boolean]
  ): c.Expr[Int] = testValDefsEvaluationCount(kind, effect, useIt)

  def testValDefsSamePrefixDoNotClashImpl: c.Expr[Int] = testValDefsSamePrefixDoNotClash

  def testValDefBuilderRecursiveDefImpl(n: c.Expr[Int]): c.Expr[Int] = testValDefBuilderRecursiveDef(n)

  def testValDefBuilderMixedTypesImpl: c.Expr[String] = testValDefBuilderMixedTypes

  def testValDefsCacheMutualRecursionImpl(n: c.Expr[Int]): c.Expr[Data] = testValDefsCacheMutualRecursion(n)

  def testValDefsCacheMLocalImpl: c.Expr[Data] = testValDefsCacheMLocal

  def testLambdaBuilderMixedTypesImpl: c.Expr[String] = testLambdaBuilderMixedTypes
}

object ExprsEdgeCasesFixtures {

  def testExprTpe[A](expr: A): Data = macro ExprsEdgeCasesFixtures.testExprTpeImpl[A]

  def testExistentialExpr[A](expr: A): Data = macro ExprsEdgeCasesFixtures.testExistentialExprImpl[A]

  def testSummoningOutcome[A]: Data = macro ExprsEdgeCasesFixtures.testSummoningOutcomeImpl[A]

  def testExprValueInt(expr: Int): Data = macro ExprsEdgeCasesFixtures.testExprValueIntImpl

  def testExprValueString(expr: String): Data = macro ExprsEdgeCasesFixtures.testExprValueStringImpl

  def testSemiEvalOutcome[A](expr: A): Data = macro ExprsEdgeCasesFixtures.testSemiEvalOutcomeImpl[A]

  def testSemiEvalApply[F](function: F, args: Any*): Data = macro ExprsEdgeCasesFixtures.testSemiEvalApplyImpl[F]

  def testSemiEvalWithOverride[A](mode: String, expr: A): Data =
    macro ExprsEdgeCasesFixtures.testSemiEvalWithOverrideImpl[A]

  def testSemiQuoteBuiltInsAndStructures: Data = macro ExprsEdgeCasesFixtures.testSemiQuoteBuiltInsAndStructuresImpl

  def testSemiQuoteWithFailingOverride: Data = macro ExprsEdgeCasesFixtures.testSemiQuoteWithFailingOverrideImpl

  def testBuiltInCodecEdgeCases: Data = macro ExprsEdgeCasesFixtures.testBuiltInCodecEdgeCasesImpl

  def testDataCodecRoundtrips: Data = macro ExprsEdgeCasesFixtures.testDataCodecRoundtripsImpl

  def testExprCodecDerivedAtRuntime[A](expr: A): Data =
    macro ExprsEdgeCasesFixtures.testExprCodecDerivedAtRuntimeImpl[A]

  def testSemiQuoteRoundTripAtRuntime[A](expr: A): Data =
    macro ExprsEdgeCasesFixtures.testSemiQuoteRoundTripAtRuntimeImpl[A]

  def testSingletonOfAtRuntime[A]: Data = macro ExprsEdgeCasesFixtures.testSingletonOfAtRuntimeImpl[A]

  def testVarArgsConversions[A](args: A*): Data = macro ExprsEdgeCasesFixtures.testVarArgsConversionsImpl[A]

  def testFreshNames[A](expr: A): Data = macro ExprsEdgeCasesFixtures.testFreshNamesImpl[A]

  def testMatchCaseEqValueAgainst[A](expr: A, expected: A): String =
    macro ExprsEdgeCasesFixtures.testMatchCaseEqValueAgainstImpl[A]

  def testMatchCaseTypeTestMatch[A, B](expr: A): String =
    macro ExprsEdgeCasesFixtures.testMatchCaseTypeTestMatchImpl[A, B]

  def testDirectStyleWithoutRunSafe: Data = macro ExprsEdgeCasesFixtures.testDirectStyleWithoutRunSafeImpl

  def testValDefsEvaluationCount(kind: String, effect: Int, useIt: Boolean): Int =
    macro ExprsEdgeCasesFixtures.testValDefsEvaluationCountImpl

  def testValDefsSamePrefixDoNotClash: Int = macro ExprsEdgeCasesFixtures.testValDefsSamePrefixDoNotClashImpl

  def testValDefBuilderRecursiveDef(n: Int): Int = macro ExprsEdgeCasesFixtures.testValDefBuilderRecursiveDefImpl

  def testValDefBuilderMixedTypes: String = macro ExprsEdgeCasesFixtures.testValDefBuilderMixedTypesImpl

  def testValDefsCacheMutualRecursion(n: Int): Data =
    macro ExprsEdgeCasesFixtures.testValDefsCacheMutualRecursionImpl

  def testValDefsCacheMLocal: Data = macro ExprsEdgeCasesFixtures.testValDefsCacheMLocalImpl

  def testLambdaBuilderMixedTypes: String = macro ExprsEdgeCasesFixtures.testLambdaBuilderMixedTypesImpl
}
