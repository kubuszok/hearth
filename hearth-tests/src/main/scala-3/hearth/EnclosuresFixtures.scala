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

  inline def testEnclosingClassHasSelfTypeMember: Data = ${ testEnclosingClassHasSelfTypeMemberImpl }
  private def testEnclosingClassHasSelfTypeMemberImpl(using q: Quotes): Expr[Data] =
    new EnclosuresFixtures(q).testEnclosingClassHasSelfTypeMember

  inline def testEnclosingMethodParameters: Data = ${ testEnclosingMethodParametersImpl }
  private def testEnclosingMethodParametersImpl(using q: Quotes): Expr[Data] =
    new EnclosuresFixtures(q).testEnclosingMethodParameters

  inline def testSumEnclosingMethodParameters: Int = ${ testSumEnclosingMethodParametersImpl }
  private def testSumEnclosingMethodParametersImpl(using q: Quotes): Expr[Int] =
    new EnclosuresFixtures(q).testSumEnclosingMethodParameters

  inline def testEnclosingMethodParameterNames: Data = ${ testEnclosingMethodParameterNamesImpl }
  private def testEnclosingMethodParameterNamesImpl(using q: Quotes): Expr[Data] =
    new EnclosuresFixtures(q).testEnclosingMethodParameterNames

  inline def testEnclosureDetails: Data = ${ testEnclosureDetailsImpl }
  private def testEnclosureDetailsImpl(using q: Quotes): Expr[Data] = new EnclosuresFixtures(q).testEnclosureDetails

  inline def testEnclosingValueType: String = ${ testEnclosingValueTypeImpl }
  private def testEnclosingValueTypeImpl(using q: Quotes): Expr[String] = new EnclosuresFixtures(
    q
  ).testEnclosingValueType

  inline def testEnclosurePositionsSorted: Data = ${ testEnclosurePositionsSortedImpl }
  private def testEnclosurePositionsSortedImpl(using q: Quotes): Expr[Data] = new EnclosuresFixtures(
    q
  ).testEnclosurePositionsSorted
}
