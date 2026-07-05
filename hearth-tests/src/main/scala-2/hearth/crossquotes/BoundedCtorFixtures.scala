package hearth
package crossquotes

import hearth.data.Data

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

final private class BoundedCtorFixtures(val c: blackbox.Context)
    extends MacroCommonsScala2
    with BoundedCtorFixturesImpl {

  def testUpperBounded1Impl[In: c.WeakTypeTag]: c.Expr[Data] = testUpperBounded1[In]
  def testUpperBounded2Impl[In: c.WeakTypeTag]: c.Expr[Data] = testUpperBounded2[In]
  def testBounded1Impl[In: c.WeakTypeTag]: c.Expr[Data] = testBounded1[In]
}

object BoundedCtorFixtures {

  def testUpperBounded1[In]: Data = macro BoundedCtorFixtures.testUpperBounded1Impl[In]
  def testUpperBounded2[In]: Data = macro BoundedCtorFixtures.testUpperBounded2Impl[In]
  def testBounded1[In]: Data = macro BoundedCtorFixtures.testBounded1Impl[In]
}
