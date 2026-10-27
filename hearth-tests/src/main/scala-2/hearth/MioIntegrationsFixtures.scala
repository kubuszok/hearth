package hearth

import hearth.data.Data

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

final private class MioIntegrationsFixtures(val c: blackbox.Context)
    extends MacroCommonsScala2
    with MioIntegrationsFixturesImpl {

  def testValDefBuilderBuildCachedWithMIOImpl: c.Expr[Data] = testValDefBuilderBuildCachedWithMIO
  def testExtensionLoadingResultToMIOImpl: c.Expr[Data] = testExtensionLoadingResultToMIO
  def testValDefsCacheParMap2MergeBugImpl: c.Expr[Data] = testValDefsCacheParMap2SharesEntry
  def testValDefsCacheParMap2DuplicateBuildErrorImpl: c.Expr[Data] = testValDefsCacheParMap2DuplicateBuildError
  def testRunToExprOrFailRendersFailureImpl: c.Expr[Int] = testRunToExprOrFailRendersFailure
  def testRunToExprOrFailFailsOnErrorLogImpl: c.Expr[Int] = testRunToExprOrFailFailsOnErrorLog
  def testRunToExprOrFailIgnoresErrorLogByDefaultImpl: c.Expr[Int] = testRunToExprOrFailIgnoresErrorLogByDefault
  def testRunToExprOrFailTimeoutImpl: c.Expr[Int] = testRunToExprOrFailTimeout
  def testRunToExprOrFailNestedImpl: c.Expr[Int] = testRunToExprOrFailNested
}

object MioIntegrationsFixtures {

  def testValDefBuilderBuildCachedWithMIO: Data = macro MioIntegrationsFixtures.testValDefBuilderBuildCachedWithMIOImpl
  def testExtensionLoadingResultToMIO: Data = macro MioIntegrationsFixtures.testExtensionLoadingResultToMIOImpl
  def testValDefsCacheParMap2SharesEntry: Data = macro MioIntegrationsFixtures.testValDefsCacheParMap2MergeBugImpl
  def testValDefsCacheParMap2DuplicateBuildError: Data =
    macro MioIntegrationsFixtures.testValDefsCacheParMap2DuplicateBuildErrorImpl
  def testRunToExprOrFailRendersFailure: Int = macro MioIntegrationsFixtures.testRunToExprOrFailRendersFailureImpl
  def testRunToExprOrFailFailsOnErrorLog: Int = macro MioIntegrationsFixtures.testRunToExprOrFailFailsOnErrorLogImpl
  def testRunToExprOrFailIgnoresErrorLogByDefault: Int =
    macro MioIntegrationsFixtures.testRunToExprOrFailIgnoresErrorLogByDefaultImpl
  def testRunToExprOrFailTimeout: Int = macro MioIntegrationsFixtures.testRunToExprOrFailTimeoutImpl
  def testRunToExprOrFailNested: Int = macro MioIntegrationsFixtures.testRunToExprOrFailNestedImpl
}
