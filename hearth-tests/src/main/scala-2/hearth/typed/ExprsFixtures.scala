package hearth
package typed

import hearth.data.Data

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

final private class ExprsFixtures(val c: blackbox.Context) extends MacroCommonsScala2 with ExprsFixturesImpl {

  // TODO: create macro annotation which would allow to do the following

  def testExprPrintersImpl[A: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[Data] = testExprPrinters[A](expr)

  def testExprSummoningImpl[A: c.WeakTypeTag]: c.Expr[Data] = testExprSummoning[A]

  def testExprUpcastingImpl[A: c.WeakTypeTag, B: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[Data] =
    testExprUpcasting[A, B](expr)

  def testSuppressUnusedImpl[A: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[Unit] = testSuppressUnused[A](expr)

  def testMatchCaseTypeMatchImpl[A: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[Data] = testMatchCaseTypeMatch[A](expr)

  def testMatchCasePartitionImpl[A: c.WeakTypeTag, B: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[B] =
    testMatchCasePartition[A, B](expr)

  def testMatchCaseTraverseImpl[A: c.WeakTypeTag, B: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[B] =
    testMatchCaseTraverse[A, B](expr)

  def testScopeCreateAndUseImpl: c.Expr[Data] = testScopeCreateAndUse
}

object ExprsFixtures {

  def testExprPrinters[A](expr: A): Data = macro ExprsFixtures.testExprPrintersImpl[A]

  def testExprSummoning[A]: Data = macro ExprsFixtures.testExprSummoningImpl[A]

  def testExprUpcasting[A, B](expr: A): Data = macro ExprsFixtures.testExprUpcastingImpl[A, B]

  def testSuppressUnused[A](expr: A): Unit = macro ExprsFixtures.testSuppressUnusedImpl[A]

  def testMatchCaseTypeMatch[A](expr: A): Data = macro ExprsFixtures.testMatchCaseTypeMatchImpl[A]

  def testMatchCasePartition[A, B](expr: A): B = macro ExprsFixtures.testMatchCasePartitionImpl[A, B]

  def testMatchCaseTraverse[A, B](expr: A): B = macro ExprsFixtures.testMatchCaseTraverseImpl[A, B]

  def testScopeCreateAndUse: Data = macro ExprsFixtures.testScopeCreateAndUseImpl
}
