package hearth

import hearth.data.Data

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

final private class EnclosuresFixtures(val c: blackbox.Context) extends MacroCommonsScala2 with EnclosuresFixturesImpl {

  def testEnclosingScopeImpl: c.Expr[Data] = testEnclosingScope
  def testEnclosingScopeReachesPackageImpl: c.Expr[Data] = testEnclosingScopeReachesPackage
  def testCallEnclosingHelperImpl: c.Expr[Int] = testCallEnclosingHelper
}

object EnclosuresFixtures {

  def testEnclosingScope: Data = macro EnclosuresFixtures.testEnclosingScopeImpl
  def testEnclosingScopeReachesPackage: Data = macro EnclosuresFixtures.testEnclosingScopeReachesPackageImpl
  def testCallEnclosingHelper: Int = macro EnclosuresFixtures.testCallEnclosingHelperImpl
}
