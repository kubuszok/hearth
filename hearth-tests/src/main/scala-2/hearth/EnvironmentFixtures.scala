package hearth

import hearth.data.Data

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

final private class EnvironmentFixtures(val c: blackbox.Context)
    extends MacroCommonsScala2
    with EnvironmentFixturesImpl {

  def testPositionImpl: c.Expr[Data] = testPosition

  def testEnvironmentImpl: c.Expr[Data] = testEnvironment
}

object EnvironmentFixtures {

  def testPosition: Data = macro EnvironmentFixtures.testPositionImpl
  def testEnvironment: Data = macro EnvironmentFixtures.testEnvironmentImpl
}
