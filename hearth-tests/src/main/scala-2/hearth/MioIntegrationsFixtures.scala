package hearth

import hearth.data.Data

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

final private class MioIntegrationsFixtures(val c: blackbox.Context)
    extends MacroCommonsScala2
    with MioIntegrationsFixturesImpl {

  def testValDefBuilderBuildCachedWithMIOImpl: c.Expr[Data] = testValDefBuilderBuildCachedWithMIO
  def testExtensionLoadingResultToMIOImpl: c.Expr[Data] = testExtensionLoadingResultToMIO
  def testValDefsCacheParMap2MergeBugImpl: c.Expr[Data] = testValDefsCacheParMap2MergeBug
  def testValDefsCacheParMap2DuplicateBuildErrorImpl: c.Expr[Data] = testValDefsCacheParMap2DuplicateBuildError
}

object MioIntegrationsFixtures {

  def testValDefBuilderBuildCachedWithMIO: Data = macro MioIntegrationsFixtures.testValDefBuilderBuildCachedWithMIOImpl
  def testExtensionLoadingResultToMIO: Data = macro MioIntegrationsFixtures.testExtensionLoadingResultToMIOImpl
  def testValDefsCacheParMap2MergeBug: Data = macro MioIntegrationsFixtures.testValDefsCacheParMap2MergeBugImpl
  def testValDefsCacheParMap2DuplicateBuildError: Data =
    macro MioIntegrationsFixtures.testValDefsCacheParMap2DuplicateBuildErrorImpl
}
