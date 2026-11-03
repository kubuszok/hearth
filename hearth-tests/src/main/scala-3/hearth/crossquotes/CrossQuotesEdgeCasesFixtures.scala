package hearth
package crossquotes

import hearth.data.Data

import scala.quoted.*

final private class CrossQuotesEdgeCasesFixtures(q: Quotes)
    extends MacroCommonsScala3(using q),
      CrossQuotesEdgeCasesFixturesImpl

object CrossQuotesEdgeCasesFixtures {

  inline def testTypeOfEdgeCases[A]: Data = ${ testTypeOfEdgeCasesImpl[A] }
  private def testTypeOfEdgeCasesImpl[A: Type](using q: Quotes): Expr[Data] =
    new CrossQuotesEdgeCasesFixtures(q).testTypeOfEdgeCases[A]

  inline def testCtorEdgeCases: Data = ${ testCtorEdgeCasesImpl }
  private def testCtorEdgeCasesImpl(using q: Quotes): Expr[Data] =
    new CrossQuotesEdgeCasesFixtures(q).testCtorEdgeCases

  inline def testBoundedCtorClaims: Data = ${ testBoundedCtorClaimsImpl }
  private def testBoundedCtorClaimsImpl(using q: Quotes): Expr[Data] =
    new CrossQuotesEdgeCasesFixtures(q).testBoundedCtorClaims

  inline def testPartialApplicationClaims: Data = ${ testPartialApplicationClaimsImpl }
  private def testPartialApplicationClaimsImpl(using q: Quotes): Expr[Data] =
    new CrossQuotesEdgeCasesFixtures(q).testPartialApplicationClaims

  inline def testQuoteEdgeCases: Data = ${ testQuoteEdgeCasesImpl }
  private def testQuoteEdgeCasesImpl(using q: Quotes): Expr[Data] =
    new CrossQuotesEdgeCasesFixtures(q).testQuoteEdgeCases

  inline def testQuoteGenerics[A](inline value: A): Data = ${ testQuoteGenericsImpl[A]('value) }
  private def testQuoteGenericsImpl[A: Type](value: Expr[A])(using q: Quotes): Expr[Data] =
    new CrossQuotesEdgeCasesFixtures(q).testQuoteGenerics[A](value)
}
