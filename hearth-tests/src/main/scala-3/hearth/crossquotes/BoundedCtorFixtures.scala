package hearth
package crossquotes

import hearth.data.Data

import scala.quoted.*

final private class BoundedCtorFixtures(q: Quotes) extends MacroCommonsScala3(using q), BoundedCtorFixturesImpl

object BoundedCtorFixtures {

  inline def testUpperBounded1[In]: Data = ${ testUpperBounded1Impl[In] }
  private def testUpperBounded1Impl[In: Type](using q: Quotes): Expr[Data] =
    new BoundedCtorFixtures(q).testUpperBounded1[In]

  inline def testUpperBounded2[In]: Data = ${ testUpperBounded2Impl[In] }
  private def testUpperBounded2Impl[In: Type](using q: Quotes): Expr[Data] =
    new BoundedCtorFixtures(q).testUpperBounded2[In]

  inline def testBounded1[In]: Data = ${ testBounded1Impl[In] }
  private def testBounded1Impl[In: Type](using q: Quotes): Expr[Data] =
    new BoundedCtorFixtures(q).testBounded1[In]
}
