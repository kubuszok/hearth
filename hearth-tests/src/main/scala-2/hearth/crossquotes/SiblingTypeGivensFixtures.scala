package hearth
package crossquotes

import hearth.data.Data

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

final private class SiblingTypeGivensFixtures(val c: blackbox.Context)
    extends MacroCommonsScala2
    with SiblingTypeGivensFixturesImpl {

  def testSiblingImplicitTypesImpl: c.Expr[Data] = testSiblingImplicitTypes
}

object SiblingTypeGivensFixtures {

  def testSiblingImplicitTypes: Data = macro SiblingTypeGivensFixtures.testSiblingImplicitTypesImpl
}
