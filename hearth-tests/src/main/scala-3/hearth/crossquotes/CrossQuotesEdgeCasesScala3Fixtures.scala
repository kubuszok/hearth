package hearth
package crossquotes

import hearth.data.Data

import scala.quoted.*

final private class CrossQuotesEdgeCasesScala3Fixtures(q: Quotes)
    extends MacroCommonsScala3(using q),
      CrossQuotesEdgeCasesScala3FixturesImpl

object CrossQuotesEdgeCasesScala3Fixtures {

  inline def testOpaqueTypes: Data = ${ testOpaqueTypesImpl }
  private def testOpaqueTypesImpl(using q: Quotes): Expr[Data] =
    new CrossQuotesEdgeCasesScala3Fixtures(q).testOpaqueTypes

  inline def testQuoteSpliceTypes[A](inline value: A): Data = ${ testQuoteSpliceTypesImpl[A]('value) }
  private def testQuoteSpliceTypesImpl[A: Type](value: Expr[A])(using q: Quotes): Expr[Data] =
    new CrossQuotesEdgeCasesScala3Fixtures(q).testQuoteSpliceTypes[A](value)
}
