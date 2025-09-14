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
}

object ExprsFixtures {

  def testExprPrinters[A](expr: A): Data = macro ExprsFixtures.testExprPrintersImpl[A]

  def testExprSummoning[A]: Data = macro ExprsFixtures.testExprSummoningImpl[A]

  def testExprUpcasting[A, B](expr: A): Data = macro ExprsFixtures.testExprUpcastingImpl[A, B]

  def testSuppressUnused[A](expr: A): Unit = macro ExprsFixtures.testSuppressUnusedImpl[A]
}
