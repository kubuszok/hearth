package hearth

import hearth.data.Data

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

final private class EnclosuresFixtures(val c: blackbox.Context) extends MacroCommonsScala2 with EnclosuresFixturesImpl {

  def testEnclosingScopeImpl: c.Expr[Data] = testEnclosingScope
  def testEnclosingScopeReachesPackageImpl: c.Expr[Data] = testEnclosingScopeReachesPackage
  def testCallEnclosingHelperImpl: c.Expr[Int] = testCallEnclosingHelper
  def testEnclosingLocalValuesImpl: c.Expr[Data] = testEnclosingLocalValues
  def testSumEnclosingLocalIntsImpl: c.Expr[Int] = testSumEnclosingLocalInts
  def testEnclosingClassHasSelfTypeMemberImpl: c.Expr[Data] = testEnclosingClassHasSelfTypeMember
  def testEnclosingMethodParametersImpl: c.Expr[Data] = testEnclosingMethodParameters
  def testEnclosingMethodParameterNamesImpl: c.Expr[Data] = testEnclosingMethodParameterNames
  def testSumEnclosingMethodParametersImpl: c.Expr[Int] = testSumEnclosingMethodParameters
}

object EnclosuresFixtures {

  def testEnclosingScope: Data = macro EnclosuresFixtures.testEnclosingScopeImpl
  def testEnclosingScopeReachesPackage: Data = macro EnclosuresFixtures.testEnclosingScopeReachesPackageImpl
  def testCallEnclosingHelper: Int = macro EnclosuresFixtures.testCallEnclosingHelperImpl
  def testEnclosingLocalValues: Data = macro EnclosuresFixtures.testEnclosingLocalValuesImpl
  def testSumEnclosingLocalInts: Int = macro EnclosuresFixtures.testSumEnclosingLocalIntsImpl
  def testEnclosingClassHasSelfTypeMember: Data = macro EnclosuresFixtures.testEnclosingClassHasSelfTypeMemberImpl
  def testEnclosingMethodParameters: Data = macro EnclosuresFixtures.testEnclosingMethodParametersImpl
  def testSumEnclosingMethodParameters: Int = macro EnclosuresFixtures.testSumEnclosingMethodParametersImpl
  def testEnclosingMethodParameterNames: Data = macro EnclosuresFixtures.testEnclosingMethodParameterNamesImpl
}
