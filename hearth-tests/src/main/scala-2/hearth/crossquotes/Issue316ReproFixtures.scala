package hearth
package crossquotes

import hearth.data.Data

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

final private class Issue316ReproFixtures(val c: blackbox.Context)
    extends MacroCommonsScala2
    with Issue316ReproFixturesImpl {

  def testSiblingImplicitTypesImpl: c.Expr[Data] = testSiblingImplicitTypes
}

object Issue316ReproFixtures {

  def testSiblingImplicitTypes: Data = macro Issue316ReproFixtures.testSiblingImplicitTypesImpl
}
