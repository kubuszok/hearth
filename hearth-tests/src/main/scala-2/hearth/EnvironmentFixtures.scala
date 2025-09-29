package hearth

import hearth.data.Data

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

final private class EnvironmentFixtures(val c: blackbox.Context)
    extends MacroCommonsScala2
    with EnvironmentFixturesImpl {

  def testPositionImpl: c.Expr[Data] = testPosition

  def testEnvironmentImpl: c.Expr[Data] = testEnvironment

  def testErrorAndAbortImpl: c.Expr[Any] = testErrorAndAbort

  def testIsExpandedAtImpl(position: c.Expr[String]): c.Expr[Boolean] = testIsExpandedAt(position)

  def testLoadingExtensionsImpl: c.Expr[Data] = testLoadingExtensions
}

object EnvironmentFixtures {

  def testPosition: Data = macro EnvironmentFixtures.testPositionImpl
  def testEnvironment: Data = macro EnvironmentFixtures.testEnvironmentImpl
  def testErrorAndAbort: Any = macro EnvironmentFixtures.testErrorAndAbortImpl
  def testIsExpandedAt(position: String): Boolean = macro EnvironmentFixtures.testIsExpandedAtImpl
  def testLoadingExtensions: Data = macro EnvironmentFixtures.testLoadingExtensionsImpl
}
