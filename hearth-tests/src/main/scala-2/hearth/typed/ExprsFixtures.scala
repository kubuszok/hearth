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

  def testValDefBuilderOfValScopeIssueImpl: c.Expr[Data] = testValDefBuilderOfValScopeIssue
  def testValDefBuilderOfVarScopeIssueImpl: c.Expr[Data] = testValDefBuilderOfVarScopeIssue
  def testValDefBuilderOfLazyScopeIssueImpl: c.Expr[Data] = testValDefBuilderOfLazyScopeIssue
  def testValDefBuilderOfDef0ScopeIssueImpl: c.Expr[Data] = testValDefBuilderOfDef0ScopeIssue
  def testValDefBuilderOfDef1ScopeIssueImpl: c.Expr[Data] = testValDefBuilderOfDef1ScopeIssue
  def testValDefBuilderOfDef2ScopeIssueImpl: c.Expr[Data] = testValDefBuilderOfDef2ScopeIssue
  def testValDefBuilderOfDef3ScopeIssueImpl: c.Expr[Data] = testValDefBuilderOfDef3ScopeIssue
  def testValDefBuilderOfDef4ScopeIssueImpl: c.Expr[Data] = testValDefBuilderOfDef4ScopeIssue
  def testValDefBuilderOfDef5ScopeIssueImpl: c.Expr[Data] = testValDefBuilderOfDef5ScopeIssue
  def testValDefBuilderOfDef6ScopeIssueImpl: c.Expr[Data] = testValDefBuilderOfDef6ScopeIssue
  def testValDefBuilderOfDef7ScopeIssueImpl: c.Expr[Data] = testValDefBuilderOfDef7ScopeIssue
  def testValDefBuilderOfDef8ScopeIssueImpl: c.Expr[Data] = testValDefBuilderOfDef8ScopeIssue
  def testValDefBuilderOfDef9ScopeIssueImpl: c.Expr[Data] = testValDefBuilderOfDef9ScopeIssue
  def testValDefBuilderOfDef10ScopeIssueImpl: c.Expr[Data] = testValDefBuilderOfDef10ScopeIssue
  def testValDefBuilderOfDef11ScopeIssueImpl: c.Expr[Data] = testValDefBuilderOfDef11ScopeIssue
  def testValDefBuilderOfDef12ScopeIssueImpl: c.Expr[Data] = testValDefBuilderOfDef12ScopeIssue
  def testValDefBuilderOfDef13ScopeIssueImpl: c.Expr[Data] = testValDefBuilderOfDef13ScopeIssue
  def testValDefBuilderOfDef14ScopeIssueImpl: c.Expr[Data] = testValDefBuilderOfDef14ScopeIssue
  def testValDefBuilderOfDef15ScopeIssueImpl: c.Expr[Data] = testValDefBuilderOfDef15ScopeIssue
  def testValDefBuilderOfDef16ScopeIssueImpl: c.Expr[Data] = testValDefBuilderOfDef16ScopeIssue
  def testValDefBuilderOfDef17ScopeIssueImpl: c.Expr[Data] = testValDefBuilderOfDef17ScopeIssue
  def testValDefBuilderOfDef18ScopeIssueImpl: c.Expr[Data] = testValDefBuilderOfDef18ScopeIssue
  def testValDefBuilderOfDef19ScopeIssueImpl: c.Expr[Data] = testValDefBuilderOfDef19ScopeIssue
  def testValDefBuilderOfDef20ScopeIssueImpl: c.Expr[Data] = testValDefBuilderOfDef20ScopeIssue
  def testValDefBuilderOfDef21ScopeIssueImpl: c.Expr[Data] = testValDefBuilderOfDef21ScopeIssue
  def testValDefBuilderOfDef22ScopeIssueImpl: c.Expr[Data] = testValDefBuilderOfDef22ScopeIssue

  def testMatchCaseDirectStyleImpl[A: c.WeakTypeTag, B: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[B] =
    testMatchCaseDirectStyle[A, B](expr)

  def testValDefsDirectStyleAndCloseImpl[A: c.WeakTypeTag, B: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[B] =
    testValDefsDirectStyleAndClose[A, B](expr)

  def testValDefBuilderDirectStyleAndCloseImpl[A: c.WeakTypeTag, B: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[B] =
    testValDefBuilderDirectStyleAndClose[A, B](expr)

  def testLambdaBuilderDirectStyleImpl[A: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[Data] =
    testLambdaBuilderDirectStyle[A](expr)

  def testBidirectionalCodecsImpl: c.Expr[Data] = testBidirectionalCodecs
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

  def testValDefBuilderOfValScopeIssue: Data = macro ExprsFixtures.testValDefBuilderOfValScopeIssueImpl
  def testValDefBuilderOfVarScopeIssue: Data = macro ExprsFixtures.testValDefBuilderOfVarScopeIssueImpl
  def testValDefBuilderOfLazyScopeIssue: Data = macro ExprsFixtures.testValDefBuilderOfLazyScopeIssueImpl
  def testValDefBuilderOfDef0ScopeIssue: Data = macro ExprsFixtures.testValDefBuilderOfDef0ScopeIssueImpl
  def testValDefBuilderOfDef1ScopeIssue: Data = macro ExprsFixtures.testValDefBuilderOfDef1ScopeIssueImpl
  def testValDefBuilderOfDef2ScopeIssue: Data = macro ExprsFixtures.testValDefBuilderOfDef2ScopeIssueImpl
  def testValDefBuilderOfDef3ScopeIssue: Data = macro ExprsFixtures.testValDefBuilderOfDef3ScopeIssueImpl
  def testValDefBuilderOfDef4ScopeIssue: Data = macro ExprsFixtures.testValDefBuilderOfDef4ScopeIssueImpl
  def testValDefBuilderOfDef5ScopeIssue: Data = macro ExprsFixtures.testValDefBuilderOfDef5ScopeIssueImpl
  def testValDefBuilderOfDef6ScopeIssue: Data = macro ExprsFixtures.testValDefBuilderOfDef6ScopeIssueImpl
  def testValDefBuilderOfDef7ScopeIssue: Data = macro ExprsFixtures.testValDefBuilderOfDef7ScopeIssueImpl
  def testValDefBuilderOfDef8ScopeIssue: Data = macro ExprsFixtures.testValDefBuilderOfDef8ScopeIssueImpl
  def testValDefBuilderOfDef9ScopeIssue: Data = macro ExprsFixtures.testValDefBuilderOfDef9ScopeIssueImpl
  def testValDefBuilderOfDef10ScopeIssue: Data = macro ExprsFixtures.testValDefBuilderOfDef10ScopeIssueImpl
  def testValDefBuilderOfDef11ScopeIssue: Data = macro ExprsFixtures.testValDefBuilderOfDef11ScopeIssueImpl
  def testValDefBuilderOfDef12ScopeIssue: Data = macro ExprsFixtures.testValDefBuilderOfDef12ScopeIssueImpl
  def testValDefBuilderOfDef13ScopeIssue: Data = macro ExprsFixtures.testValDefBuilderOfDef13ScopeIssueImpl
  def testValDefBuilderOfDef14ScopeIssue: Data = macro ExprsFixtures.testValDefBuilderOfDef14ScopeIssueImpl
  def testValDefBuilderOfDef15ScopeIssue: Data = macro ExprsFixtures.testValDefBuilderOfDef15ScopeIssueImpl
  def testValDefBuilderOfDef16ScopeIssue: Data = macro ExprsFixtures.testValDefBuilderOfDef16ScopeIssueImpl
  def testValDefBuilderOfDef17ScopeIssue: Data = macro ExprsFixtures.testValDefBuilderOfDef17ScopeIssueImpl
  def testValDefBuilderOfDef18ScopeIssue: Data = macro ExprsFixtures.testValDefBuilderOfDef18ScopeIssueImpl
  def testValDefBuilderOfDef19ScopeIssue: Data = macro ExprsFixtures.testValDefBuilderOfDef19ScopeIssueImpl
  def testValDefBuilderOfDef20ScopeIssue: Data = macro ExprsFixtures.testValDefBuilderOfDef20ScopeIssueImpl
  def testValDefBuilderOfDef21ScopeIssue: Data = macro ExprsFixtures.testValDefBuilderOfDef21ScopeIssueImpl
  def testValDefBuilderOfDef22ScopeIssue: Data = macro ExprsFixtures.testValDefBuilderOfDef22ScopeIssueImpl

  def testMatchCaseDirectStyle[A, B](expr: A): B = macro ExprsFixtures.testMatchCaseDirectStyleImpl[A, B]

  def testValDefsDirectStyleAndClose[A, B](expr: A): B =
    macro ExprsFixtures.testValDefsDirectStyleAndCloseImpl[A, B]

  def testValDefBuilderDirectStyleAndClose[A, B](expr: A): B =
    macro ExprsFixtures.testValDefBuilderDirectStyleAndCloseImpl[A, B]

  def testLambdaBuilderDirectStyle[A](expr: A): Data = macro ExprsFixtures.testLambdaBuilderDirectStyleImpl[A]

  def testBidirectionalCodecs: Data = macro ExprsFixtures.testBidirectionalCodecsImpl
}
