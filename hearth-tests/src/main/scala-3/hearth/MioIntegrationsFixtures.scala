package hearth

import hearth.data.Data

import hearth.data.Data

import scala.quoted.*

final private class MioIntegrationsFixtures(q: Quotes) extends MacroCommonsScala3(using q), MioIntegrationsFixturesImpl

object MioIntegrationsFixtures {

  // TODO: create macro annotation which would allow to do the following

  inline def testValDefBuilderBuildCachedWithMIO: Data = ${ testValDefBuilderBuildCachedWithMIOImpl }
  private def testValDefBuilderBuildCachedWithMIOImpl(using q: Quotes): Expr[Data] = new MioIntegrationsFixtures(
    q
  ).testValDefBuilderBuildCachedWithMIO

  inline def testExtensionLoadingResultToMIO: Data = ${ testExtensionLoadingResultToMIOImpl }
  private def testExtensionLoadingResultToMIOImpl(using q: Quotes): Expr[Data] = new MioIntegrationsFixtures(
    q
  ).testExtensionLoadingResultToMIO
}
