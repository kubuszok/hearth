package hearth
package crossquotes

import hearth.data.Data

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

final private class CrossQuotesEdgeCasesFixtures(val c: blackbox.Context)
    extends MacroCommonsScala2
    with CrossQuotesEdgeCasesFixturesImpl {

  def testTypeOfEdgeCasesImpl[A: c.WeakTypeTag]: c.Expr[Data] = testTypeOfEdgeCases[A]

  def testCtorEdgeCasesImpl: c.Expr[Data] = testCtorEdgeCases

  def testBoundedCtorClaimsImpl: c.Expr[Data] = testBoundedCtorClaims

  def testPartialApplicationClaimsImpl: c.Expr[Data] = testPartialApplicationClaims

  def testQuoteEdgeCasesImpl: c.Expr[Data] = testQuoteEdgeCases

  def testQuoteGenericsImpl[A: c.WeakTypeTag](value: c.Expr[A]): c.Expr[Data] = testQuoteGenerics[A](value)
}

object CrossQuotesEdgeCasesFixtures {

  def testTypeOfEdgeCases[A]: Data = macro CrossQuotesEdgeCasesFixtures.testTypeOfEdgeCasesImpl[A]

  def testCtorEdgeCases: Data = macro CrossQuotesEdgeCasesFixtures.testCtorEdgeCasesImpl

  def testBoundedCtorClaims: Data = macro CrossQuotesEdgeCasesFixtures.testBoundedCtorClaimsImpl

  def testPartialApplicationClaims: Data = macro CrossQuotesEdgeCasesFixtures.testPartialApplicationClaimsImpl

  def testQuoteEdgeCases: Data = macro CrossQuotesEdgeCasesFixtures.testQuoteEdgeCasesImpl

  def testQuoteGenerics[A](value: A): Data = macro CrossQuotesEdgeCasesFixtures.testQuoteGenericsImpl[A]
}
