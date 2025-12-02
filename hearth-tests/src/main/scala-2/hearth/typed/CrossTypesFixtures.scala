package hearth
package typed

import hearth.data.Data

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

final private class CrossTypesFixtures(val c: blackbox.Context) extends MacroCommonsScala2 with CrossTypesFixturesImpl {

  def testTypeOfImpl[A: c.WeakTypeTag]: c.Expr[Data] = testTypeOf[A]

  def testTypeCtor1Impl[A: c.WeakTypeTag]: c.Expr[Data] = testTypeCtor1[A]

  /*
  def testTypeCtor2Impl[A: c.WeakTypeTag]: c.Expr[Data] = testTypeCtor2[A]

  def testTypeCtor3Impl[A: c.WeakTypeTag]: c.Expr[Data] = testTypeCtor3[A]

  def testTypeCtor4Impl[A: c.WeakTypeTag]: c.Expr[Data] = testTypeCtor4[A]

  def testTypeCtor5Impl[A: c.WeakTypeTag]: c.Expr[Data] = testTypeCtor5[A]

  def testTypeCtor6Impl[A: c.WeakTypeTag]: c.Expr[Data] = testTypeCtor6[A]

  def testTypeCtor7Impl[A: c.WeakTypeTag]: c.Expr[Data] = testTypeCtor7[A]

  def testTypeCtor8Impl[A: c.WeakTypeTag]: c.Expr[Data] = testTypeCtor8[A]

  def testTypeCtor9Impl[A: c.WeakTypeTag]: c.Expr[Data] = testTypeCtor9[A]

  def testTypeCtor10Impl[A: c.WeakTypeTag]: c.Expr[Data] = testTypeCtor10[A]

  def testTypeCtor11Impl[A: c.WeakTypeTag]: c.Expr[Data] = testTypeCtor11[A]

  def testTypeCtor12Impl[A: c.WeakTypeTag]: c.Expr[Data] = testTypeCtor12[A]

  def testTypeCtor13Impl[A: c.WeakTypeTag]: c.Expr[Data] = testTypeCtor13[A]

  def testTypeCtor14Impl[A: c.WeakTypeTag]: c.Expr[Data] = testTypeCtor14[A]

  def testTypeCtor15Impl[A: c.WeakTypeTag]: c.Expr[Data] = testTypeCtor15[A]

  def testTypeCtor16Impl[A: c.WeakTypeTag]: c.Expr[Data] = testTypeCtor16[A]

  def testTypeCtor17Impl[A: c.WeakTypeTag]: c.Expr[Data] = testTypeCtor17[A]

  def testTypeCtor18Impl[A: c.WeakTypeTag]: c.Expr[Data] = testTypeCtor18[A]

  def testTypeCtor19Impl[A: c.WeakTypeTag]: c.Expr[Data] = testTypeCtor19[A]

  def testTypeCtor20Impl[A: c.WeakTypeTag]: c.Expr[Data] = testTypeCtor20[A]

  def testTypeCtor21Impl[A: c.WeakTypeTag]: c.Expr[Data] = testTypeCtor21[A]

  def testTypeCtor22Impl[A: c.WeakTypeTag]: c.Expr[Data] = testTypeCtor22[A]
  */
}

object CrossTypesFixtures {

  def testTypeOf[A]: Data = macro CrossTypesFixtures.testTypeOfImpl[A]

  def testTypeCtor1[A]: Data = macro CrossTypesFixtures.testTypeCtor1Impl[A]

  /*
  def testTypeCtor2[A]: Data = macro CrossTypesFixtures.testTypeCtor2Impl[A]

  def testTypeCtor3[A]: Data = macro CrossTypesFixtures.testTypeCtor3Impl[A]

  def testTypeCtor4[A]: Data = macro CrossTypesFixtures.testTypeCtor4Impl[A]

  def testTypeCtor5[A]: Data = macro CrossTypesFixtures.testTypeCtor5Impl[A]

  def testTypeCtor6[A]: Data = macro CrossTypesFixtures.testTypeCtor6Impl[A]

  def testTypeCtor7[A]: Data = macro CrossTypesFixtures.testTypeCtor7Impl[A]

  def testTypeCtor8[A]: Data = macro CrossTypesFixtures.testTypeCtor8Impl[A]

  def testTypeCtor9[A]: Data = macro CrossTypesFixtures.testTypeCtor9Impl[A]

  def testTypeCtor10[A]: Data = macro CrossTypesFixtures.testTypeCtor10Impl[A]

  def testTypeCtor11[A]: Data = macro CrossTypesFixtures.testTypeCtor11Impl[A]

  def testTypeCtor12[A]: Data = macro CrossTypesFixtures.testTypeCtor12Impl[A]

  def testTypeCtor13[A]: Data = macro CrossTypesFixtures.testTypeCtor13Impl[A]

  def testTypeCtor14[A]: Data = macro CrossTypesFixtures.testTypeCtor14Impl[A]

  def testTypeCtor15[A]: Data = macro CrossTypesFixtures.testTypeCtor15Impl[A]

  def testTypeCtor16[A]: Data = macro CrossTypesFixtures.testTypeCtor16Impl[A]

  def testTypeCtor17[A]: Data = macro CrossTypesFixtures.testTypeCtor17Impl[A]

  def testTypeCtor18[A]: Data = macro CrossTypesFixtures.testTypeCtor18Impl[A]

  def testTypeCtor19[A]: Data = macro CrossTypesFixtures.testTypeCtor19Impl[A]

  def testTypeCtor20[A]: Data = macro CrossTypesFixtures.testTypeCtor20Impl[A]

  def testTypeCtor21[A]: Data = macro CrossTypesFixtures.testTypeCtor21Impl[A]

  def testTypeCtor22[A]: Data = macro CrossTypesFixtures.testTypeCtor22Impl[A]
  */
}
