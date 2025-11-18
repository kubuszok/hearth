package hearth

import hearth.data.Data

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

final private class MioIntegrationsFixtures(val c: blackbox.Context)
    extends MacroCommonsScala2
    with MioIntegrationsFixturesImpl {

  def testValDefBuilderBuildCachedWithMIOImpl: c.Expr[Data] = testValDefBuilderBuildCachedWithMIO
}

object MioIntegrationsFixtures {

  def testValDefBuilderBuildCachedWithMIO: Data = macro MioIntegrationsFixtures.testValDefBuilderBuildCachedWithMIOImpl
}
