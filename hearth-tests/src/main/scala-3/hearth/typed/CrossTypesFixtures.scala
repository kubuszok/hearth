package hearth
package typed

import hearth.data.Data

import scala.quoted.*

final private class CrossTypesFixtures(q: Quotes) extends MacroCommonsScala3(using q), CrossTypesFixturesImpl

object CrossTypesFixtures {

  // TODO: create macro annotation which would allow to do the following

  inline def testTypeOf[A]: Data = ${ testTypeOfImpl[A] }
  private def testTypeOfImpl[A: Type](using q: Quotes): Expr[Data] = new CrossTypesFixtures(q).testTypeOf[A]

  inline def testTypeCtor1[A]: Data = ${ testTypeCtor1Impl[A] }
  private def testTypeCtor1Impl[A: Type](using q: Quotes): Expr[Data] = new CrossTypesFixtures(q).testTypeCtor1[A]

  inline def testTypeCtor2[A]: Data = ${ testTypeCtor2Impl[A] }
  private def testTypeCtor2Impl[A: Type](using q: Quotes): Expr[Data] = new CrossTypesFixtures(q).testTypeCtor2[A]

  inline def testTypeCtor3[A]: Data = ${ testTypeCtor3Impl[A] }
  private def testTypeCtor3Impl[A: Type](using q: Quotes): Expr[Data] = new CrossTypesFixtures(q).testTypeCtor3[A]

  inline def testTypeCtor4[A]: Data = ${ testTypeCtor4Impl[A] }
  private def testTypeCtor4Impl[A: Type](using q: Quotes): Expr[Data] = new CrossTypesFixtures(q).testTypeCtor4[A]

  inline def testTypeCtor5[A]: Data = ${ testTypeCtor5Impl[A] }
  private def testTypeCtor5Impl[A: Type](using q: Quotes): Expr[Data] = new CrossTypesFixtures(q).testTypeCtor5[A]

  inline def testTypeCtor6[A]: Data = ${ testTypeCtor6Impl[A] }
  private def testTypeCtor6Impl[A: Type](using q: Quotes): Expr[Data] = new CrossTypesFixtures(q).testTypeCtor6[A]

  inline def testTypeCtor7[A]: Data = ${ testTypeCtor7Impl[A] }
  private def testTypeCtor7Impl[A: Type](using q: Quotes): Expr[Data] = new CrossTypesFixtures(q).testTypeCtor7[A]

  inline def testTypeCtor8[A]: Data = ${ testTypeCtor8Impl[A] }
  private def testTypeCtor8Impl[A: Type](using q: Quotes): Expr[Data] = new CrossTypesFixtures(q).testTypeCtor8[A]

  inline def testTypeCtor9[A]: Data = ${ testTypeCtor9Impl[A] }
  private def testTypeCtor9Impl[A: Type](using q: Quotes): Expr[Data] = new CrossTypesFixtures(q).testTypeCtor9[A]

  inline def testTypeCtor10[A]: Data = ${ testTypeCtor10Impl[A] }
  private def testTypeCtor10Impl[A: Type](using q: Quotes): Expr[Data] = new CrossTypesFixtures(q).testTypeCtor10[A]

  inline def testTypeCtor11[A]: Data = ${ testTypeCtor11Impl[A] }
  private def testTypeCtor11Impl[A: Type](using q: Quotes): Expr[Data] = new CrossTypesFixtures(q).testTypeCtor11[A]

  inline def testTypeCtor12[A]: Data = ${ testTypeCtor12Impl[A] }
  private def testTypeCtor12Impl[A: Type](using q: Quotes): Expr[Data] = new CrossTypesFixtures(q).testTypeCtor12[A]

  inline def testTypeCtor13[A]: Data = ${ testTypeCtor13Impl[A] }
  private def testTypeCtor13Impl[A: Type](using q: Quotes): Expr[Data] = new CrossTypesFixtures(q).testTypeCtor13[A]

  inline def testTypeCtor14[A]: Data = ${ testTypeCtor14Impl[A] }
  private def testTypeCtor14Impl[A: Type](using q: Quotes): Expr[Data] = new CrossTypesFixtures(q).testTypeCtor14[A]

  inline def testTypeCtor15[A]: Data = ${ testTypeCtor15Impl[A] }
  private def testTypeCtor15Impl[A: Type](using q: Quotes): Expr[Data] = new CrossTypesFixtures(q).testTypeCtor15[A]

  inline def testTypeCtor16[A]: Data = ${ testTypeCtor16Impl[A] }
  private def testTypeCtor16Impl[A: Type](using q: Quotes): Expr[Data] = new CrossTypesFixtures(q).testTypeCtor16[A]

  inline def testTypeCtor17[A]: Data = ${ testTypeCtor17Impl[A] }
  private def testTypeCtor17Impl[A: Type](using q: Quotes): Expr[Data] = new CrossTypesFixtures(q).testTypeCtor17[A]

  inline def testTypeCtor18[A]: Data = ${ testTypeCtor18Impl[A] }
  private def testTypeCtor18Impl[A: Type](using q: Quotes): Expr[Data] = new CrossTypesFixtures(q).testTypeCtor18[A]

  inline def testTypeCtor19[A]: Data = ${ testTypeCtor19Impl[A] }
  private def testTypeCtor19Impl[A: Type](using q: Quotes): Expr[Data] = new CrossTypesFixtures(q).testTypeCtor19[A]

  inline def testTypeCtor20[A]: Data = ${ testTypeCtor20Impl[A] }
  private def testTypeCtor20Impl[A: Type](using q: Quotes): Expr[Data] = new CrossTypesFixtures(q).testTypeCtor20[A]

  inline def testTypeCtor21[A]: Data = ${ testTypeCtor21Impl[A] }
  private def testTypeCtor21Impl[A: Type](using q: Quotes): Expr[Data] = new CrossTypesFixtures(q).testTypeCtor21[A]

  inline def testTypeCtor22[A]: Data = ${ testTypeCtor22Impl[A] }
  private def testTypeCtor22Impl[A: Type](using q: Quotes): Expr[Data] = new CrossTypesFixtures(q).testTypeCtor22[A]
}
