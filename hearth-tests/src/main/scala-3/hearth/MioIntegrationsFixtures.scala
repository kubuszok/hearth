package hearth

import hearth.data.Data

import hearth.data.Data

import scala.quoted.*

final private class MioIntegrationsFixtures(q: Quotes) extends MacroCommonsScala3(using q), MioIntegrationsFixturesImpl

object MioIntegrationsFixtures {

  inline def testValDefBuilderBuildCachedWithMIO: Data = ${ testValDefBuilderBuildCachedWithMIOImpl }
  private def testValDefBuilderBuildCachedWithMIOImpl(using q: Quotes): Expr[Data] = new MioIntegrationsFixtures(
    q
  ).testValDefBuilderBuildCachedWithMIO

  inline def testExtensionLoadingResultToMIO: Data = ${ testExtensionLoadingResultToMIOImpl }
  private def testExtensionLoadingResultToMIOImpl(using q: Quotes): Expr[Data] = new MioIntegrationsFixtures(
    q
  ).testExtensionLoadingResultToMIO

  inline def testValDefsCacheParMap2SharesEntry: Data = ${ testValDefsCacheParMap2MergeBugImpl }
  private def testValDefsCacheParMap2MergeBugImpl(using q: Quotes): Expr[Data] = new MioIntegrationsFixtures(
    q
  ).testValDefsCacheParMap2SharesEntry

  inline def testValDefsCacheParMap2DuplicateBuildError: Data = ${
    testValDefsCacheParMap2DuplicateBuildErrorImpl
  }
  private def testValDefsCacheParMap2DuplicateBuildErrorImpl(using q: Quotes): Expr[Data] =
    new MioIntegrationsFixtures(q).testValDefsCacheParMap2DuplicateBuildError

  inline def testRunToExprOrFailRendersFailure: Int = ${ testRunToExprOrFailRendersFailureImpl }
  private def testRunToExprOrFailRendersFailureImpl(using q: Quotes): Expr[Int] = new MioIntegrationsFixtures(
    q
  ).testRunToExprOrFailRendersFailure

  inline def testRunToExprOrFailFailsOnErrorLog: Int = ${ testRunToExprOrFailFailsOnErrorLogImpl }
  private def testRunToExprOrFailFailsOnErrorLogImpl(using q: Quotes): Expr[Int] = new MioIntegrationsFixtures(
    q
  ).testRunToExprOrFailFailsOnErrorLog

  inline def testRunToExprOrFailIgnoresErrorLogByDefault: Int = ${ testRunToExprOrFailIgnoresErrorLogByDefaultImpl }
  private def testRunToExprOrFailIgnoresErrorLogByDefaultImpl(using q: Quotes): Expr[Int] = new MioIntegrationsFixtures(
    q
  ).testRunToExprOrFailIgnoresErrorLogByDefault

  inline def testRunToExprOrFailTimeout: Int = ${ testRunToExprOrFailTimeoutImpl }
  private def testRunToExprOrFailTimeoutImpl(using q: Quotes): Expr[Int] = new MioIntegrationsFixtures(
    q
  ).testRunToExprOrFailTimeout

  inline def testRunToExprOrFailNested: Int = ${ testRunToExprOrFailNestedImpl }
  private def testRunToExprOrFailNestedImpl(using q: Quotes): Expr[Int] = new MioIntegrationsFixtures(
    q
  ).testRunToExprOrFailNested
}
