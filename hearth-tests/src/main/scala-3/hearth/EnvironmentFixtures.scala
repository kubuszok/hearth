package hearth

import hearth.data.Data

import scala.quoted.*

final private class EnvironmentFixtures(q: Quotes) extends MacroCommonsScala3(using q), EnvironmentFixturesImpl

object EnvironmentFixtures {

  // [hearth#176]

  inline def testPosition: Data = ${ testPositionImpl }
  private def testPositionImpl(using q: Quotes): Expr[Data] = new EnvironmentFixtures(q).testPosition

  inline def testEnvironment: Data = ${ testEnvironmentImpl }
  private def testEnvironmentImpl(using q: Quotes): Expr[Data] = new EnvironmentFixtures(q).testEnvironment

  inline def testErrorAndAbort: Any = ${ testErrorAndAbortImpl }
  private def testErrorAndAbortImpl(using q: Quotes): Expr[Any] = new EnvironmentFixtures(q).testErrorAndAbort

  inline def testIsExpandedAt(inline position: String): Boolean = ${ testIsExpandedAtImpl('position) }
  private def testIsExpandedAtImpl(position: Expr[String])(using q: Quotes): Expr[Boolean] =
    new EnvironmentFixtures(q).testIsExpandedAt(position)

  inline def testLoadingExtensions: Data = ${ testLoadingExtensionsImpl }
  private def testLoadingExtensionsImpl(using q: Quotes): Expr[Data] = new EnvironmentFixtures(q).testLoadingExtensions

  inline def testLoadingExtensionsExcluding: Data = ${ testLoadingExtensionsExcludingImpl }
  private def testLoadingExtensionsExcludingImpl(using q: Quotes): Expr[Data] = new EnvironmentFixtures(
    q
  ).testLoadingExtensionsExcluding

  inline def testLoadingExtensionsWhen: Data = ${ testLoadingExtensionsWhenImpl }
  private def testLoadingExtensionsWhenImpl(using q: Quotes): Expr[Data] = new EnvironmentFixtures(
    q
  ).testLoadingExtensionsWhen
}
