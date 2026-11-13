package hearth
package crossquotes

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

final private class CrossByNameSpliceFixtures(val c: blackbox.Context)
    extends MacroCommonsScala2
    with CrossByNameSpliceFixturesImpl {
  def testDefaultsSplicedIntoByNameImpl[A: c.WeakTypeTag]: c.Expr[List[Any]] = testDefaultsSplicedIntoByName[A]
  def testDefaultsSplicedIntoStrictImpl[A: c.WeakTypeTag]: c.Expr[List[Any]] = testDefaultsSplicedIntoStrict[A]
  def testUpcastLiteralSplicedIntoByNameImpl: c.Expr[List[Any]] = testUpcastLiteralSplicedIntoByName
  def testDefaultsWithoutUpcastIntoByNameImpl[A: c.WeakTypeTag]: c.Expr[List[Any]] =
    testDefaultsWithoutUpcastIntoByName[A]
}

object CrossByNameSpliceFixtures {
  def testDefaultsWithoutUpcastIntoByName[A]: List[Any] =
    macro CrossByNameSpliceFixtures.testDefaultsWithoutUpcastIntoByNameImpl[A]
  def testDefaultsSplicedIntoStrict[A]: List[Any] = macro CrossByNameSpliceFixtures.testDefaultsSplicedIntoStrictImpl[A]
  def testUpcastLiteralSplicedIntoByName: List[Any] =
    macro CrossByNameSpliceFixtures.testUpcastLiteralSplicedIntoByNameImpl
  def testDefaultsSplicedIntoByName[A]: List[Any] = macro CrossByNameSpliceFixtures.testDefaultsSplicedIntoByNameImpl[A]
}
