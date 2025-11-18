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

  def testValDefBuilderBuildCachedWithMIOImpl: c.Expr[Data] = testValDefBuilderBuildCachedWithMIO

  def testValDefBuilderBuildCachedVarGetterSetterImpl: c.Expr[Data] = testValDefBuilderBuildCachedVarGetterSetter

  def testLambdaBuilderOfNAndBuildImpl: c.Expr[Data] = testLambdaBuilderOfNAndBuild

  def testLambdaBuilderBuildWithImpl: c.Expr[Data] = testLambdaBuilderBuildWith

  def testLambdaBuilderPartitionImpl[A: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[Data] =
    testLambdaBuilderPartition[A](expr)

  def testLambdaBuilderTraverseImpl[A: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[Data] =
    testLambdaBuilderTraverse[A](expr)

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

  def testValDefBuilderBuildCachedWithMIO: Data = macro ExprsFixtures.testValDefBuilderBuildCachedWithMIOImpl

  def testValDefBuilderBuildCachedVarGetterSetter: Data =
    macro ExprsFixtures.testValDefBuilderBuildCachedVarGetterSetterImpl

  def testLambdaBuilderOfNAndBuild: Data = macro ExprsFixtures.testLambdaBuilderOfNAndBuildImpl

  def testLambdaBuilderBuildWith: Data = macro ExprsFixtures.testLambdaBuilderBuildWithImpl

  def testLambdaBuilderPartition[A](expr: A): Data = macro ExprsFixtures.testLambdaBuilderPartitionImpl[A]

  def testLambdaBuilderTraverse[A](expr: A): Data = macro ExprsFixtures.testLambdaBuilderTraverseImpl[A]

  def testBidirectionalCodecs: Data = macro ExprsFixtures.testBidirectionalCodecsImpl

  def testOneWayCodecs: Data = macro ExprsFixtures.testOneWayCodecsImpl
}
