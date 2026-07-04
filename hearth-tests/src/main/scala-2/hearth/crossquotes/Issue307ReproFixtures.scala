package hearth
package crossquotes

import hearth.data.Data

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

final private class Issue307ReproFixtures(val c: blackbox.Context)
    extends MacroCommonsScala2
    with Issue307ReproFixturesImpl {

  def testCtor1UpperBoundedMatchImpl[In: c.WeakTypeTag]: c.Expr[Data] = testCtor1UpperBoundedMatch[In]
  def testCtor2UpperBoundedMatchImpl[In: c.WeakTypeTag]: c.Expr[Data] = testCtor2UpperBoundedMatch[In]
  def testCtor1BoundedMatchImpl[In: c.WeakTypeTag]: c.Expr[Data] = testCtor1BoundedMatch[In]
}

object Issue307ReproFixtures {

  def testCtor1UpperBoundedMatch[In]: Data = macro Issue307ReproFixtures.testCtor1UpperBoundedMatchImpl[In]
  def testCtor2UpperBoundedMatch[In]: Data = macro Issue307ReproFixtures.testCtor2UpperBoundedMatchImpl[In]
  def testCtor1BoundedMatch[In]: Data = macro Issue307ReproFixtures.testCtor1BoundedMatchImpl[In]
}
