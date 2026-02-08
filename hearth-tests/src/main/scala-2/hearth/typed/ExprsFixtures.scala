package hearth
package typed

import hearth.data.Data

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

final private class ExprsFixtures(val c: blackbox.Context) extends MacroCommonsScala2 with ExprsFixturesImpl {

  // TODO: create macro annotation which would allow to do the following

  def testExprPrintersImpl[A: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[Data] = testExprPrinters[A](expr)

  def testExprSummoningImpl[A: c.WeakTypeTag]: c.Expr[Data] = testExprSummoning[A]

  def testExprSummoningIgnoringImpl[A: c.WeakTypeTag, Companion: c.WeakTypeTag](
      ignoredNames: c.Expr[String]*
  ): c.Expr[Data] = testExprSummoningIgnoring[A, Companion](ignoredNames)

  def testExprUpcastingImpl[A: c.WeakTypeTag, B: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[Data] =
    testExprUpcasting[A, B](expr)

  def testSuppressUnusedImpl[A: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[Unit] = testSuppressUnused[A](expr)

  def testVarArgsImpl[A: c.WeakTypeTag](exprs: c.Expr[A]*): c.Expr[Data] = testVarArgs[A](exprs)

  def testMatchCaseTypeMatchImpl[A: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[Data] = testMatchCaseTypeMatch[A](expr)

  def testMatchCasePartitionImpl[A: c.WeakTypeTag, B: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[B] =
    testMatchCasePartition[A, B](expr)

  def testMatchCaseTraverseImpl[A: c.WeakTypeTag, B: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[B] =
    testMatchCaseTraverse[A, B](expr)

  def testValDefsCreateAndUseImpl: c.Expr[Data] = testValDefsCreateAndUse

  def testValDefsPartitionAndCloseImpl[A: c.WeakTypeTag, B: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[B] =
    testValDefsPartitionAndClose[A, B](expr)

  def testValDefsTraverseAndCloseImpl[A: c.WeakTypeTag, B: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[B] =
    testValDefsTraverseAndClose[A, B](expr)

  def testValDefBuilderCreateAndUseImpl: c.Expr[Data] = testValDefBuilderCreateAndUse

  def testValDefBuilderPartitionAndCloseImpl[A: c.WeakTypeTag, B: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[B] =
    testValDefBuilderPartitionAndClose[A, B](expr)

  def testValDefBuilderTraverseAndCloseImpl[A: c.WeakTypeTag, B: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[B] =
    testValDefBuilderTraverseAndClose[A, B](expr)

  def testValDefBuilderBuildCachedAndUseImpl: c.Expr[Data] = testValDefBuilderBuildCachedAndUse

  def testValDefBuilderBuildCachedVarGetterSetterImpl: c.Expr[Data] = testValDefBuilderBuildCachedVarGetterSetter

  def testValDefsCacheMergeImpl: c.Expr[Unit] = testValDefsCacheMerge

  def testLambdaBuilderOfNAndBuildImpl: c.Expr[Data] = testLambdaBuilderOfNAndBuild

  def testLambdaBuilderBuildWithImpl: c.Expr[Data] = testLambdaBuilderBuildWith

  def testLambdaBuilderPartitionImpl[A: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[Data] =
    testLambdaBuilderPartition[A](expr)

  def testLambdaBuilderTraverseImpl[A: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[Data] =
    testLambdaBuilderTraverse[A](expr)

  def testLambdaBuilderOf1ScopeIssueImpl: c.Expr[Data] = testLambdaBuilderOf1ScopeIssue

  def testLambdaBuilderOf2ScopeIssueImpl: c.Expr[Data] = testLambdaBuilderOf2ScopeIssue

  def testLambdaBuilderOf3ScopeIssueImpl: c.Expr[Data] = testLambdaBuilderOf3ScopeIssue

  def testLambdaBuilderOf4ScopeIssueImpl: c.Expr[Data] = testLambdaBuilderOf4ScopeIssue
  def testLambdaBuilderOf5ScopeIssueImpl: c.Expr[Data] = testLambdaBuilderOf5ScopeIssue
  def testLambdaBuilderOf6ScopeIssueImpl: c.Expr[Data] = testLambdaBuilderOf6ScopeIssue
  def testLambdaBuilderOf7ScopeIssueImpl: c.Expr[Data] = testLambdaBuilderOf7ScopeIssue
  def testLambdaBuilderOf8ScopeIssueImpl: c.Expr[Data] = testLambdaBuilderOf8ScopeIssue
  def testLambdaBuilderOf9ScopeIssueImpl: c.Expr[Data] = testLambdaBuilderOf9ScopeIssue
  def testLambdaBuilderOf10ScopeIssueImpl: c.Expr[Data] = testLambdaBuilderOf10ScopeIssue
  def testLambdaBuilderOf11ScopeIssueImpl: c.Expr[Data] = testLambdaBuilderOf11ScopeIssue
  def testLambdaBuilderOf12ScopeIssueImpl: c.Expr[Data] = testLambdaBuilderOf12ScopeIssue
  def testLambdaBuilderOf13ScopeIssueImpl: c.Expr[Data] = testLambdaBuilderOf13ScopeIssue
  def testLambdaBuilderOf14ScopeIssueImpl: c.Expr[Data] = testLambdaBuilderOf14ScopeIssue
  def testLambdaBuilderOf15ScopeIssueImpl: c.Expr[Data] = testLambdaBuilderOf15ScopeIssue
  def testLambdaBuilderOf16ScopeIssueImpl: c.Expr[Data] = testLambdaBuilderOf16ScopeIssue
  def testLambdaBuilderOf17ScopeIssueImpl: c.Expr[Data] = testLambdaBuilderOf17ScopeIssue
  def testLambdaBuilderOf18ScopeIssueImpl: c.Expr[Data] = testLambdaBuilderOf18ScopeIssue
  def testLambdaBuilderOf19ScopeIssueImpl: c.Expr[Data] = testLambdaBuilderOf19ScopeIssue
  def testLambdaBuilderOf20ScopeIssueImpl: c.Expr[Data] = testLambdaBuilderOf20ScopeIssue
  def testLambdaBuilderOf21ScopeIssueImpl: c.Expr[Data] = testLambdaBuilderOf21ScopeIssue
  def testLambdaBuilderOf22ScopeIssueImpl: c.Expr[Data] = testLambdaBuilderOf22ScopeIssue

  def testBidirectionalCodecsImpl: c.Expr[Data] = testBidirectionalCodecs

  def testOneWayCodecsImpl: c.Expr[Data] = testOneWayCodecs
}

object ExprsFixtures {

  def testExprPrinters[A](expr: A): Data = macro ExprsFixtures.testExprPrintersImpl[A]

  def testExprSummoning[A]: Data = macro ExprsFixtures.testExprSummoningImpl[A]

  def testExprSummoningIgnoring[A, Companion](ignoredNames: String*): Data =
    macro ExprsFixtures.testExprSummoningIgnoringImpl[A, Companion]

  def testExprUpcasting[A, B](expr: A): Data = macro ExprsFixtures.testExprUpcastingImpl[A, B]

  def testSuppressUnused[A](expr: A): Unit = macro ExprsFixtures.testSuppressUnusedImpl[A]

  def testVarArgs[A](exprs: A*): Data = macro ExprsFixtures.testVarArgsImpl[A]

  def testMatchCaseTypeMatch[A](expr: A): Data = macro ExprsFixtures.testMatchCaseTypeMatchImpl[A]

  def testMatchCasePartition[A, B](expr: A): B = macro ExprsFixtures.testMatchCasePartitionImpl[A, B]

  def testMatchCaseTraverse[A, B](expr: A): B = macro ExprsFixtures.testMatchCaseTraverseImpl[A, B]

  def testValDefsCreateAndUse: Data = macro ExprsFixtures.testValDefsCreateAndUseImpl

  def testValDefsPartitionAndClose[A, B](expr: A): B = macro ExprsFixtures.testValDefsPartitionAndCloseImpl[A, B]

  def testValDefsTraverseAndClose[A, B](expr: A): B = macro ExprsFixtures.testValDefsTraverseAndCloseImpl[A, B]

  def testValDefBuilderCreateAndUse: Data = macro ExprsFixtures.testValDefBuilderCreateAndUseImpl

  def testValDefBuilderPartitionAndClose[A, B](expr: A): B =
    macro ExprsFixtures.testValDefBuilderPartitionAndCloseImpl[A, B]

  def testValDefBuilderTraverseAndClose[A, B](expr: A): B =
    macro ExprsFixtures.testValDefBuilderTraverseAndCloseImpl[A, B]

  def testValDefBuilderBuildCachedAndUse: Data = macro ExprsFixtures.testValDefBuilderBuildCachedAndUseImpl

  def testValDefBuilderBuildCachedVarGetterSetter: Data =
    macro ExprsFixtures.testValDefBuilderBuildCachedVarGetterSetterImpl

  def testValDefsCacheMerge: Unit = macro ExprsFixtures.testValDefsCacheMergeImpl

  def testLambdaBuilderOfNAndBuild: Data = macro ExprsFixtures.testLambdaBuilderOfNAndBuildImpl

  def testLambdaBuilderBuildWith: Data = macro ExprsFixtures.testLambdaBuilderBuildWithImpl

  def testLambdaBuilderPartition[A](expr: A): Data = macro ExprsFixtures.testLambdaBuilderPartitionImpl[A]

  def testLambdaBuilderTraverse[A](expr: A): Data = macro ExprsFixtures.testLambdaBuilderTraverseImpl[A]

  def testLambdaBuilderOf1ScopeIssue: Data = macro ExprsFixtures.testLambdaBuilderOf1ScopeIssueImpl

  def testLambdaBuilderOf2ScopeIssue: Data = macro ExprsFixtures.testLambdaBuilderOf2ScopeIssueImpl

  def testLambdaBuilderOf3ScopeIssue: Data = macro ExprsFixtures.testLambdaBuilderOf3ScopeIssueImpl

  def testLambdaBuilderOf4ScopeIssue: Data = macro ExprsFixtures.testLambdaBuilderOf4ScopeIssueImpl
  def testLambdaBuilderOf5ScopeIssue: Data = macro ExprsFixtures.testLambdaBuilderOf5ScopeIssueImpl
  def testLambdaBuilderOf6ScopeIssue: Data = macro ExprsFixtures.testLambdaBuilderOf6ScopeIssueImpl
  def testLambdaBuilderOf7ScopeIssue: Data = macro ExprsFixtures.testLambdaBuilderOf7ScopeIssueImpl
  def testLambdaBuilderOf8ScopeIssue: Data = macro ExprsFixtures.testLambdaBuilderOf8ScopeIssueImpl
  def testLambdaBuilderOf9ScopeIssue: Data = macro ExprsFixtures.testLambdaBuilderOf9ScopeIssueImpl
  def testLambdaBuilderOf10ScopeIssue: Data = macro ExprsFixtures.testLambdaBuilderOf10ScopeIssueImpl
  def testLambdaBuilderOf11ScopeIssue: Data = macro ExprsFixtures.testLambdaBuilderOf11ScopeIssueImpl
  def testLambdaBuilderOf12ScopeIssue: Data = macro ExprsFixtures.testLambdaBuilderOf12ScopeIssueImpl
  def testLambdaBuilderOf13ScopeIssue: Data = macro ExprsFixtures.testLambdaBuilderOf13ScopeIssueImpl
  def testLambdaBuilderOf14ScopeIssue: Data = macro ExprsFixtures.testLambdaBuilderOf14ScopeIssueImpl
  def testLambdaBuilderOf15ScopeIssue: Data = macro ExprsFixtures.testLambdaBuilderOf15ScopeIssueImpl
  def testLambdaBuilderOf16ScopeIssue: Data = macro ExprsFixtures.testLambdaBuilderOf16ScopeIssueImpl
  def testLambdaBuilderOf17ScopeIssue: Data = macro ExprsFixtures.testLambdaBuilderOf17ScopeIssueImpl
  def testLambdaBuilderOf18ScopeIssue: Data = macro ExprsFixtures.testLambdaBuilderOf18ScopeIssueImpl
  def testLambdaBuilderOf19ScopeIssue: Data = macro ExprsFixtures.testLambdaBuilderOf19ScopeIssueImpl
  def testLambdaBuilderOf20ScopeIssue: Data = macro ExprsFixtures.testLambdaBuilderOf20ScopeIssueImpl
  def testLambdaBuilderOf21ScopeIssue: Data = macro ExprsFixtures.testLambdaBuilderOf21ScopeIssueImpl
  def testLambdaBuilderOf22ScopeIssue: Data = macro ExprsFixtures.testLambdaBuilderOf22ScopeIssueImpl

  def testBidirectionalCodecs: Data = macro ExprsFixtures.testBidirectionalCodecsImpl

  def testOneWayCodecs: Data = macro ExprsFixtures.testOneWayCodecsImpl
}
