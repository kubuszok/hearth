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

  inline def testEnclosingLocalValues: Data = ${ testEnclosingLocalValuesImpl }
  private def testEnclosingLocalValuesImpl(using q: Quotes): Expr[Data] =
    new EnclosuresFixtures(q).testEnclosingLocalValues

  inline def testSumEnclosingLocalInts: Int = ${ testSumEnclosingLocalIntsImpl }
  private def testSumEnclosingLocalIntsImpl(using q: Quotes): Expr[Int] =
    new EnclosuresFixtures(q).testSumEnclosingLocalInts
}
