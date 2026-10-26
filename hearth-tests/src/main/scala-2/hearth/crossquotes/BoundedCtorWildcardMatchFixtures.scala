package hearth
package crossquotes

import hearth.data.Data

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

final private class BoundedCtorWildcardMatchFixtures(val c: blackbox.Context)
    extends MacroCommonsScala2
    with BoundedCtorWildcardMatchFixturesImpl {

  def testCtor1UpperBoundedMatchImpl[In: c.WeakTypeTag]: c.Expr[Data] = testCtor1UpperBoundedMatch[In]
  def testCtor2UpperBoundedMatchImpl[In: c.WeakTypeTag]: c.Expr[Data] = testCtor2UpperBoundedMatch[In]
  def testCtor1BoundedMatchImpl[In: c.WeakTypeTag]: c.Expr[Data] = testCtor1BoundedMatch[In]
}

object BoundedCtorWildcardMatchFixtures {

  def testCtor1UpperBoundedMatch[In]: Data = macro BoundedCtorWildcardMatchFixtures.testCtor1UpperBoundedMatchImpl[In]
  def testCtor2UpperBoundedMatch[In]: Data = macro BoundedCtorWildcardMatchFixtures.testCtor2UpperBoundedMatchImpl[In]
  def testCtor1BoundedMatch[In]: Data = macro BoundedCtorWildcardMatchFixtures.testCtor1BoundedMatchImpl[In]
}
