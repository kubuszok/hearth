package hearth
package typed

import hearth.data.Data

import scala.quoted.*

final private class ExprsFixtures(q: Quotes) extends MacroCommonsScala3(using q), ExprsFixturesImpl

object ExprsFixtures {

  // TODO: create macro annotation which would allow to do the following

  inline def testExprPrinters[A](inline expr: A): Data = ${ testExprPrintersImpl[A]('expr) }
  private def testExprPrintersImpl[A: Type](expr: Expr[A])(using q: Quotes): Expr[Data] =
    new ExprsFixtures(q).testExprPrinters[A](expr)

  inline def testExprSummoning[A]: Data = ${ testExprSummoningImpl[A] }
  private def testExprSummoningImpl[A: Type](using q: Quotes): Expr[Data] = new ExprsFixtures(q).testExprSummoning[A]

  inline def testExprSummoningIgnoring[A, Companion](inline ignoredNames: String*): Data = ${
    testExprSummoningIgnoringImpl[A, Companion]('ignoredNames)
  }
  private def testExprSummoningIgnoringImpl[A: Type, Companion: Type](ignoredNames: Expr[Seq[String]])(using
      q: Quotes
  ): Expr[Data] =
    new ExprsFixtures(q).testExprSummoningIgnoring[A, Companion](ignoredNames)

  inline def testExprUpcasting[A, B](inline expr: A): Data = ${ testExprUpcastingImpl[A, B]('expr) }
  private def testExprUpcastingImpl[A: Type, B: Type](expr: Expr[A])(using q: Quotes): Expr[Data] =
    new ExprsFixtures(q).testExprUpcasting[A, B](expr)

  inline def testSuppressUnused[A](inline expr: A): Unit = ${ testSuppressUnusedImpl[A]('expr) }
  private def testSuppressUnusedImpl[A: Type](expr: Expr[A])(using q: Quotes): Expr[Unit] =
    new ExprsFixtures(q).testSuppressUnused[A](expr)

  inline def testVarArgs[A](inline exprs: A*): Data = ${ testVarArgsImpl[A]('exprs) }
  private def testVarArgsImpl[A: Type](exprs: Expr[Seq[A]])(using q: Quotes): Expr[Data] =
    new ExprsFixtures(q).testVarArgs[A](exprs)

  inline def testMatchCaseTypeMatch[A](inline expr: A): Data = ${ testMatchCaseTypeMatchImpl[A]('expr) }
  private def testMatchCaseTypeMatchImpl[A: Type](expr: Expr[A])(using q: Quotes): Expr[Data] =
    new ExprsFixtures(q).testMatchCaseTypeMatch[A](expr)

  inline def testMatchCasePartition[A, B](inline expr: A): B = ${ testMatchCasePartitionImpl[A, B]('expr) }
  private def testMatchCasePartitionImpl[A: Type, B: Type](expr: Expr[A])(using q: Quotes): Expr[B] =
    new ExprsFixtures(q).testMatchCasePartition[A, B](expr)

  inline def testMatchCaseTraverse[A, B](inline expr: A): B = ${ testMatchCaseTraverseImpl[A, B]('expr) }
  private def testMatchCaseTraverseImpl[A: Type, B: Type](expr: Expr[A])(using q: Quotes): Expr[B] =
    new ExprsFixtures(q).testMatchCaseTraverse[A, B](expr)

  inline def testValDefsCreateAndUse: Data = ${ testValDefsCreateAndUseImpl }
  private def testValDefsCreateAndUseImpl(using q: Quotes): Expr[Data] = new ExprsFixtures(q).testValDefsCreateAndUse

  inline def testValDefsPartitionAndClose[A, B](inline expr: A): B = ${
    testValDefsPartitionAndCloseImpl[A, B]('expr)
  }
  private def testValDefsPartitionAndCloseImpl[A: Type, B: Type](expr: Expr[A])(using q: Quotes): Expr[B] =
    new ExprsFixtures(q).testValDefsPartitionAndClose[A, B](expr)

  inline def testValDefsTraverseAndClose[A, B](inline expr: A): B = ${
    testValDefsTraverseAndCloseImpl[A, B]('expr)
  }
  private def testValDefsTraverseAndCloseImpl[A: Type, B: Type](expr: Expr[A])(using q: Quotes): Expr[B] =
    new ExprsFixtures(q).testValDefsTraverseAndClose[A, B](expr)

  inline def testValDefBuilderCreateAndUse: Data = ${ testValDefBuilderCreateAndUseImpl }
  private def testValDefBuilderCreateAndUseImpl(using q: Quotes): Expr[Data] =
    new ExprsFixtures(q).testValDefBuilderCreateAndUse

  inline def testValDefBuilderPartitionAndClose[A, B](inline expr: A): B = ${
    testValDefBuilderPartitionAndCloseImpl[A, B]('expr)
  }
  private def testValDefBuilderPartitionAndCloseImpl[A: Type, B: Type](expr: Expr[A])(using q: Quotes): Expr[B] =
    new ExprsFixtures(q).testValDefBuilderPartitionAndClose[A, B](expr)

  inline def testValDefBuilderTraverseAndClose[A, B](inline expr: A): B = ${
    testValDefBuilderTraverseAndCloseImpl[A, B]('expr)
  }
  private def testValDefBuilderTraverseAndCloseImpl[A: Type, B: Type](expr: Expr[A])(using q: Quotes): Expr[B] =
    new ExprsFixtures(q).testValDefBuilderTraverseAndClose[A, B](expr)

  inline def testValDefBuilderBuildCachedAndUse: Data = ${ testValDefBuilderBuildCachedAndUseImpl }
  private def testValDefBuilderBuildCachedAndUseImpl(using q: Quotes): Expr[Data] =
    new ExprsFixtures(q).testValDefBuilderBuildCachedAndUse

  inline def testValDefBuilderBuildCachedVarGetterSetter: Data = ${ testValDefBuilderBuildCachedVarGetterSetterImpl }
  private def testValDefBuilderBuildCachedVarGetterSetterImpl(using q: Quotes): Expr[Data] =
    new ExprsFixtures(q).testValDefBuilderBuildCachedVarGetterSetter

  inline def testValDefsCacheMerge: Unit = ${ testValDefsCacheMergeImpl }
  private def testValDefsCacheMergeImpl(using q: Quotes): Expr[Unit] =
    new ExprsFixtures(q).testValDefsCacheMerge

  inline def testLambdaBuilderOfNAndBuild: Data = ${ testLambdaBuilderOfNAndBuildImpl }
  private def testLambdaBuilderOfNAndBuildImpl(using q: Quotes): Expr[Data] =
    new ExprsFixtures(q).testLambdaBuilderOfNAndBuild

  inline def testLambdaBuilderBuildWith: Data = ${ testLambdaBuilderBuildWithImpl }
  private def testLambdaBuilderBuildWithImpl(using q: Quotes): Expr[Data] =
    new ExprsFixtures(q).testLambdaBuilderBuildWith

  inline def testLambdaBuilderPartition[A](inline expr: A): Data = ${ testLambdaBuilderPartitionImpl[A]('expr) }
  private def testLambdaBuilderPartitionImpl[A: Type](expr: Expr[A])(using q: Quotes): Expr[Data] =
    new ExprsFixtures(q).testLambdaBuilderPartition[A](expr)

  inline def testLambdaBuilderTraverse[A](inline expr: A): Data = ${ testLambdaBuilderTraverseImpl[A]('expr) }
  private def testLambdaBuilderTraverseImpl[A: Type](expr: Expr[A])(using q: Quotes): Expr[Data] =
    new ExprsFixtures(q).testLambdaBuilderTraverse[A](expr)

  inline def testLambdaBuilderOf1ScopeIssue: Data = ${ testLambdaBuilderOf1ScopeIssueImpl }
  private def testLambdaBuilderOf1ScopeIssueImpl(using q: Quotes): Expr[Data] =
    new ExprsFixtures(q).testLambdaBuilderOf1ScopeIssue

  inline def testLambdaBuilderOf2ScopeIssue: Data = ${ testLambdaBuilderOf2ScopeIssueImpl }
  private def testLambdaBuilderOf2ScopeIssueImpl(using q: Quotes): Expr[Data] =
    new ExprsFixtures(q).testLambdaBuilderOf2ScopeIssue

  inline def testLambdaBuilderOf3ScopeIssue: Data = ${ testLambdaBuilderOf3ScopeIssueImpl }
  private def testLambdaBuilderOf3ScopeIssueImpl(using q: Quotes): Expr[Data] =
    new ExprsFixtures(q).testLambdaBuilderOf3ScopeIssue

  inline def testLambdaBuilderOf4ScopeIssue: Data = ${ testLambdaBuilderOf4ScopeIssueImpl }
  private def testLambdaBuilderOf4ScopeIssueImpl(using q: Quotes): Expr[Data] =
    new ExprsFixtures(q).testLambdaBuilderOf4ScopeIssue

  inline def testLambdaBuilderOf5ScopeIssue: Data = ${ testLambdaBuilderOf5ScopeIssueImpl }
  private def testLambdaBuilderOf5ScopeIssueImpl(using q: Quotes): Expr[Data] =
    new ExprsFixtures(q).testLambdaBuilderOf5ScopeIssue

  inline def testLambdaBuilderOf6ScopeIssue: Data = ${ testLambdaBuilderOf6ScopeIssueImpl }
  private def testLambdaBuilderOf6ScopeIssueImpl(using q: Quotes): Expr[Data] =
    new ExprsFixtures(q).testLambdaBuilderOf6ScopeIssue

  inline def testLambdaBuilderOf7ScopeIssue: Data = ${ testLambdaBuilderOf7ScopeIssueImpl }
  private def testLambdaBuilderOf7ScopeIssueImpl(using q: Quotes): Expr[Data] =
    new ExprsFixtures(q).testLambdaBuilderOf7ScopeIssue

  inline def testLambdaBuilderOf8ScopeIssue: Data = ${ testLambdaBuilderOf8ScopeIssueImpl }
  private def testLambdaBuilderOf8ScopeIssueImpl(using q: Quotes): Expr[Data] =
    new ExprsFixtures(q).testLambdaBuilderOf8ScopeIssue

  inline def testLambdaBuilderOf9ScopeIssue: Data = ${ testLambdaBuilderOf9ScopeIssueImpl }
  private def testLambdaBuilderOf9ScopeIssueImpl(using q: Quotes): Expr[Data] =
    new ExprsFixtures(q).testLambdaBuilderOf9ScopeIssue

  inline def testLambdaBuilderOf10ScopeIssue: Data = ${ testLambdaBuilderOf10ScopeIssueImpl }
  private def testLambdaBuilderOf10ScopeIssueImpl(using q: Quotes): Expr[Data] =
    new ExprsFixtures(q).testLambdaBuilderOf10ScopeIssue

  inline def testLambdaBuilderOf11ScopeIssue: Data = ${ testLambdaBuilderOf11ScopeIssueImpl }
  private def testLambdaBuilderOf11ScopeIssueImpl(using q: Quotes): Expr[Data] =
    new ExprsFixtures(q).testLambdaBuilderOf11ScopeIssue

  inline def testLambdaBuilderOf12ScopeIssue: Data = ${ testLambdaBuilderOf12ScopeIssueImpl }
  private def testLambdaBuilderOf12ScopeIssueImpl(using q: Quotes): Expr[Data] =
    new ExprsFixtures(q).testLambdaBuilderOf12ScopeIssue

  inline def testLambdaBuilderOf13ScopeIssue: Data = ${ testLambdaBuilderOf13ScopeIssueImpl }
  private def testLambdaBuilderOf13ScopeIssueImpl(using q: Quotes): Expr[Data] =
    new ExprsFixtures(q).testLambdaBuilderOf13ScopeIssue

  inline def testLambdaBuilderOf14ScopeIssue: Data = ${ testLambdaBuilderOf14ScopeIssueImpl }
  private def testLambdaBuilderOf14ScopeIssueImpl(using q: Quotes): Expr[Data] =
    new ExprsFixtures(q).testLambdaBuilderOf14ScopeIssue

  inline def testLambdaBuilderOf15ScopeIssue: Data = ${ testLambdaBuilderOf15ScopeIssueImpl }
  private def testLambdaBuilderOf15ScopeIssueImpl(using q: Quotes): Expr[Data] =
    new ExprsFixtures(q).testLambdaBuilderOf15ScopeIssue

  inline def testLambdaBuilderOf16ScopeIssue: Data = ${ testLambdaBuilderOf16ScopeIssueImpl }
  private def testLambdaBuilderOf16ScopeIssueImpl(using q: Quotes): Expr[Data] =
    new ExprsFixtures(q).testLambdaBuilderOf16ScopeIssue

  inline def testLambdaBuilderOf17ScopeIssue: Data = ${ testLambdaBuilderOf17ScopeIssueImpl }
  private def testLambdaBuilderOf17ScopeIssueImpl(using q: Quotes): Expr[Data] =
    new ExprsFixtures(q).testLambdaBuilderOf17ScopeIssue

  inline def testLambdaBuilderOf18ScopeIssue: Data = ${ testLambdaBuilderOf18ScopeIssueImpl }
  private def testLambdaBuilderOf18ScopeIssueImpl(using q: Quotes): Expr[Data] =
    new ExprsFixtures(q).testLambdaBuilderOf18ScopeIssue

  inline def testLambdaBuilderOf19ScopeIssue: Data = ${ testLambdaBuilderOf19ScopeIssueImpl }
  private def testLambdaBuilderOf19ScopeIssueImpl(using q: Quotes): Expr[Data] =
    new ExprsFixtures(q).testLambdaBuilderOf19ScopeIssue

  inline def testLambdaBuilderOf20ScopeIssue: Data = ${ testLambdaBuilderOf20ScopeIssueImpl }
  private def testLambdaBuilderOf20ScopeIssueImpl(using q: Quotes): Expr[Data] =
    new ExprsFixtures(q).testLambdaBuilderOf20ScopeIssue

  inline def testLambdaBuilderOf21ScopeIssue: Data = ${ testLambdaBuilderOf21ScopeIssueImpl }
  private def testLambdaBuilderOf21ScopeIssueImpl(using q: Quotes): Expr[Data] =
    new ExprsFixtures(q).testLambdaBuilderOf21ScopeIssue

  inline def testLambdaBuilderOf22ScopeIssue: Data = ${ testLambdaBuilderOf22ScopeIssueImpl }
  private def testLambdaBuilderOf22ScopeIssueImpl(using q: Quotes): Expr[Data] =
    new ExprsFixtures(q).testLambdaBuilderOf22ScopeIssue

  inline def testBidirectionalCodecs: Data = ${ testBidirectionalCodecsImpl }
  private def testBidirectionalCodecsImpl(using q: Quotes): Expr[Data] =
    new ExprsFixtures(q).testBidirectionalCodecs

  inline def testOneWayCodecs: Data = ${ testOneWayCodecsImpl }
  private def testOneWayCodecsImpl(using q: Quotes): Expr[Data] =
    new ExprsFixtures(q).testOneWayCodecs
}
