package hearth
package crossquotes

import scala.quoted.*

final private class CrossByNameSpliceFixtures(q: Quotes)
    extends MacroCommonsScala3(using q),
      CrossByNameSpliceFixturesImpl

object CrossByNameSpliceFixtures {
  inline def testDefaultsSplicedIntoByName[A]: List[Any] = ${ testDefaultsSplicedIntoByNameImpl[A] }
  private def testDefaultsSplicedIntoByNameImpl[A: Type](using q: Quotes): Expr[List[Any]] =
    new CrossByNameSpliceFixtures(q).testDefaultsSplicedIntoByName[A]
  inline def testDefaultsSplicedIntoStrict[A]: List[Any] = ${ testDefaultsSplicedIntoStrictImpl[A] }
  private def testDefaultsSplicedIntoStrictImpl[A: Type](using q: Quotes): Expr[List[Any]] =
    new CrossByNameSpliceFixtures(q).testDefaultsSplicedIntoStrict[A]
  inline def testUpcastLiteralSplicedIntoByName: List[Any] = ${ testUpcastLiteralSplicedIntoByNameImpl }
  private def testUpcastLiteralSplicedIntoByNameImpl(using q: Quotes): Expr[List[Any]] =
    new CrossByNameSpliceFixtures(q).testUpcastLiteralSplicedIntoByName
  inline def testDefaultsWithoutUpcastIntoByName[A]: List[Any] = ${ testDefaultsWithoutUpcastIntoByNameImpl[A] }
  private def testDefaultsWithoutUpcastIntoByNameImpl[A: Type](using q: Quotes): Expr[List[Any]] =
    new CrossByNameSpliceFixtures(q).testDefaultsWithoutUpcastIntoByName[A]
}
