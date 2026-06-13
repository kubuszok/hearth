package hearth

import hearth.data.Data

import scala.quoted.*

final private class EnclosuresFixtures(q: Quotes) extends MacroCommonsScala3(using q), EnclosuresFixturesImpl

object EnclosuresFixtures {

  inline def testEnclosingScope: Data = ${ testEnclosingScopeImpl }
  private def testEnclosingScopeImpl(using q: Quotes): Expr[Data] = new EnclosuresFixtures(q).testEnclosingScope

  inline def testEnclosingScopeReachesPackage: Data = ${ testEnclosingScopeReachesPackageImpl }
  private def testEnclosingScopeReachesPackageImpl(using q: Quotes): Expr[Data] =
    new EnclosuresFixtures(q).testEnclosingScopeReachesPackage

  inline def testCallEnclosingHelper: Int = ${ testCallEnclosingHelperImpl }
  private def testCallEnclosingHelperImpl(using q: Quotes): Expr[Int] = new EnclosuresFixtures(
    q
  ).testCallEnclosingHelper
}
