package hearth
package typed

import hearth.data.Data

import scala.quoted.*

final private class ExprsFixtures(q: Quotes) extends MacroCommonsScala3(using q), ExprsFixturesImpl

object ExprsFixtures {

  // TODO: create macro annotation which would allow to do the following

  inline def testExprPrinters[A](inline expr: A): Data = ${ testExprPrintersImpl[A]('{ expr }) }
  private def testExprPrintersImpl[A: Type](expr: Expr[A])(using q: Quotes): Expr[Data] =
    new ExprsFixtures(q).testExprPrinters[A](expr)

  inline def testExprSummoning[A]: Data = ${ testExprSummoningImpl[A] }
  private def testExprSummoningImpl[A: Type](using q: Quotes): Expr[Data] = new ExprsFixtures(q).testExprSummoning[A]

  inline def testExprUpcasting[A, B](inline expr: A): Data = ${ testExprUpcastingImpl[A, B]('{ expr }) }
  private def testExprUpcastingImpl[A: Type, B: Type](expr: Expr[A])(using q: Quotes): Expr[Data] =
    new ExprsFixtures(q).testExprUpcasting[A, B](expr)

  inline def testSuppressUnused[A](inline expr: A): Unit = ${ testSuppressUnusedImpl[A]('{ expr }) }
  private def testSuppressUnusedImpl[A: Type](expr: Expr[A])(using q: Quotes): Expr[Unit] =
    new ExprsFixtures(q).testSuppressUnused[A](expr)

  inline def testMatchCaseTypeMatch[A](inline expr: A): Data = ${ testMatchCaseTypeMatchImpl[A]('{ expr }) }
  private def testMatchCaseTypeMatchImpl[A: Type](expr: Expr[A])(using q: Quotes): Expr[Data] =
    new ExprsFixtures(q).testMatchCaseTypeMatch[A](expr)

  inline def testMatchCasePartition[A, B](inline expr: A): B = ${ testMatchCasePartitionImpl[A, B]('{ expr }) }
  private def testMatchCasePartitionImpl[A: Type, B: Type](expr: Expr[A])(using q: Quotes): Expr[B] =
    new ExprsFixtures(q).testMatchCasePartition[A, B](expr)

  inline def testMatchCaseTraverse[A, B](inline expr: A): B = ${ testMatchCaseTraverseImpl[A, B]('{ expr }) }
  private def testMatchCaseTraverseImpl[A: Type, B: Type](expr: Expr[A])(using q: Quotes): Expr[B] =
    new ExprsFixtures(q).testMatchCaseTraverse[A, B](expr)

  inline def testScopeCreateAndUse: Data = ${ testScopeCreateAndUseImpl }
  private def testScopeCreateAndUseImpl(using q: Quotes): Expr[Data] = new ExprsFixtures(q).testScopeCreateAndUse
}
