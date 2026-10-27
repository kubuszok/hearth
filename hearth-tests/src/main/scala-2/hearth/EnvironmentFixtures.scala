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

  def testReportInfoAtPositionImpl[A: c.WeakTypeTag]: c.Expr[Data] = testReportInfoAtPosition[A]

  def testReportErrorAtPositionImpl: c.Expr[Data] = testReportErrorAtPosition

  def testReportErrorAndAbortAtPositionImpl: c.Expr[Any] = testReportErrorAndAbortAtPosition

  def testIsExpandedAtImpl(position: c.Expr[String]): c.Expr[Boolean] = testIsExpandedAt(position)

  def testLoadingExtensionsImpl: c.Expr[Data] = testLoadingExtensions

  def testLoadingExtensionsExcludingImpl: c.Expr[Data] = testLoadingExtensionsExcluding

  def testLoadingExtensionsWhenImpl: c.Expr[Data] = testLoadingExtensionsWhen

  def testLoadingExtensionsTwiceImpl: c.Expr[Data] = testLoadingExtensionsTwice

  def testPositionPrettyPrintLongImpl: c.Expr[Data] = testPositionPrettyPrintLong
  def testCompileTimeVersionsImpl: c.Expr[Data] = testCompileTimeVersions
  def testTypedSettingsAccessorsImpl: c.Expr[Data] = testTypedSettingsAccessors
  def testMioBenchmarkFlameGraphDirImpl: c.Expr[Data] = testMioBenchmarkFlameGraphDir
  def testReportErrorTwiceImpl: c.Expr[Data] = testReportErrorTwice
  def testReportErrorThenAbortImpl: c.Expr[Any] = testReportErrorThenAbort
  def testHandleMioTerminationReportsErrorImpl: c.Expr[Any] = testHandleMioTerminationReportsError
  def testHandleMioTerminationPassesValueImpl: c.Expr[Int] = testHandleMioTerminationPassesValue
  def testWithMioTimeoutImpl: c.Expr[Data] = testWithMioTimeout
  def testWithMioLoggingDisabledImpl: c.Expr[Data] = testWithMioLoggingDisabled
}

object EnvironmentFixtures {

  def testPosition: Data = macro EnvironmentFixtures.testPositionImpl
  def testEnvironment: Data = macro EnvironmentFixtures.testEnvironmentImpl
  def testErrorAndAbort: Any = macro EnvironmentFixtures.testErrorAndAbortImpl
  def testReportInfoAtPosition[A]: Data = macro EnvironmentFixtures.testReportInfoAtPositionImpl[A]
  def testReportErrorAtPosition: Data = macro EnvironmentFixtures.testReportErrorAtPositionImpl
  def testReportErrorAndAbortAtPosition: Any = macro EnvironmentFixtures.testReportErrorAndAbortAtPositionImpl
  def testIsExpandedAt(position: String): Boolean = macro EnvironmentFixtures.testIsExpandedAtImpl
  def testLoadingExtensions: Data = macro EnvironmentFixtures.testLoadingExtensionsImpl
  def testLoadingExtensionsExcluding: Data = macro EnvironmentFixtures.testLoadingExtensionsExcludingImpl
  def testLoadingExtensionsWhen: Data = macro EnvironmentFixtures.testLoadingExtensionsWhenImpl
  def testLoadingExtensionsTwice: Data = macro EnvironmentFixtures.testLoadingExtensionsTwiceImpl
  def testPositionPrettyPrintLong: Data = macro EnvironmentFixtures.testPositionPrettyPrintLongImpl
  def testCompileTimeVersions: Data = macro EnvironmentFixtures.testCompileTimeVersionsImpl
  def testTypedSettingsAccessors: Data = macro EnvironmentFixtures.testTypedSettingsAccessorsImpl
  def testMioBenchmarkFlameGraphDir: Data = macro EnvironmentFixtures.testMioBenchmarkFlameGraphDirImpl
  def testReportErrorTwice: Data = macro EnvironmentFixtures.testReportErrorTwiceImpl
  def testReportErrorThenAbort: Any = macro EnvironmentFixtures.testReportErrorThenAbortImpl
  def testHandleMioTerminationReportsError: Any = macro EnvironmentFixtures.testHandleMioTerminationReportsErrorImpl
  def testHandleMioTerminationPassesValue: Int = macro EnvironmentFixtures.testHandleMioTerminationPassesValueImpl
  def testWithMioTimeout: Data = macro EnvironmentFixtures.testWithMioTimeoutImpl
  def testWithMioLoggingDisabled: Data = macro EnvironmentFixtures.testWithMioLoggingDisabledImpl
}
