package hearth
package crossquotes

import hearth.data.Data

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

final private class CrossCtorInjectionFixtures(val c: blackbox.Context)
    extends MacroCommonsScala2
    with CrossCtorInjectionFixturesImpl {

  def testTypeOfWithImportedCtor1Impl[A: c.WeakTypeTag]: c.Expr[Data] = testTypeOfWithImportedCtor1[A]

  def testCtorAsUntypedImpl: c.Expr[Data] = testCtorAsUntyped

  def testCtorSetAsUntypedImpl: c.Expr[Data] = testCtorSetAsUntyped

  def testTypeOfWithCtor1HigherImpl: c.Expr[Data] = testTypeOfWithCtor1Higher

  def testTypeOfWithCtor2HigherImpl: c.Expr[Data] = testTypeOfWithCtor2Higher

  def testTypeOfWithCtor3HigherImpl: c.Expr[Data] = testTypeOfWithCtor3Higher

  def testTypeOfWithCtor4HigherImpl: c.Expr[Data] = testTypeOfWithCtor4Higher

  def testTypeOfWithCtor5HigherImpl: c.Expr[Data] = testTypeOfWithCtor5Higher

  def testTypeOfWithCtor6HigherImpl: c.Expr[Data] = testTypeOfWithCtor6Higher

  def testTypeOfWithCtor7HigherImpl: c.Expr[Data] = testTypeOfWithCtor7Higher

  def testTypeOfWithCtor8HigherImpl: c.Expr[Data] = testTypeOfWithCtor8Higher

  def testTypeOfWithCtor9HigherImpl: c.Expr[Data] = testTypeOfWithCtor9Higher

  def testTypeOfWithCtor10HigherImpl: c.Expr[Data] = testTypeOfWithCtor10Higher

  def testTypeOfWithCtor11HigherImpl: c.Expr[Data] = testTypeOfWithCtor11Higher

  def testTypeOfWithCtor12HigherImpl: c.Expr[Data] = testTypeOfWithCtor12Higher

  def testTypeOfWithCtor13HigherImpl: c.Expr[Data] = testTypeOfWithCtor13Higher

  def testTypeOfWithCtor14HigherImpl: c.Expr[Data] = testTypeOfWithCtor14Higher

  def testTypeOfWithCtor15HigherImpl: c.Expr[Data] = testTypeOfWithCtor15Higher

  def testTypeOfWithCtor16HigherImpl: c.Expr[Data] = testTypeOfWithCtor16Higher

  def testTypeOfWithCtor17HigherImpl: c.Expr[Data] = testTypeOfWithCtor17Higher

  def testTypeOfWithCtor18HigherImpl: c.Expr[Data] = testTypeOfWithCtor18Higher

  def testTypeOfWithCtor19HigherImpl: c.Expr[Data] = testTypeOfWithCtor19Higher

  def testTypeOfWithCtor20HigherImpl: c.Expr[Data] = testTypeOfWithCtor20Higher

  def testTypeOfWithCtor21HigherImpl: c.Expr[Data] = testTypeOfWithCtor21Higher

  def testTypeOfWithCtor22HigherImpl: c.Expr[Data] = testTypeOfWithCtor22Higher

  def testExprQuoteWithCtor1BodyImpl: c.Expr[Data] = testExprQuoteWithCtor1Body

  def testExprQuoteWithCtor2BodyImpl: c.Expr[Data] = testExprQuoteWithCtor2Body

  def testExprQuoteWithCtor3BodyImpl: c.Expr[Data] = testExprQuoteWithCtor3Body

  def testExprQuoteWithCtor4BodyImpl: c.Expr[Data] = testExprQuoteWithCtor4Body

  def testExprQuoteWithCtor5BodyImpl: c.Expr[Data] = testExprQuoteWithCtor5Body

  def testExprQuoteWithCtor6BodyImpl: c.Expr[Data] = testExprQuoteWithCtor6Body

  def testExprQuoteWithCtor7BodyImpl: c.Expr[Data] = testExprQuoteWithCtor7Body

  def testExprQuoteWithCtor8BodyImpl: c.Expr[Data] = testExprQuoteWithCtor8Body

  def testExprQuoteWithCtor9BodyImpl: c.Expr[Data] = testExprQuoteWithCtor9Body

  def testExprQuoteWithCtor10BodyImpl: c.Expr[Data] = testExprQuoteWithCtor10Body

  def testExprQuoteWithCtor11BodyImpl: c.Expr[Data] = testExprQuoteWithCtor11Body

  def testExprQuoteWithCtor12BodyImpl: c.Expr[Data] = testExprQuoteWithCtor12Body

  def testExprQuoteWithCtor13BodyImpl: c.Expr[Data] = testExprQuoteWithCtor13Body

  def testExprQuoteWithCtor14BodyImpl: c.Expr[Data] = testExprQuoteWithCtor14Body

  def testExprQuoteWithCtor15BodyImpl: c.Expr[Data] = testExprQuoteWithCtor15Body

  def testExprQuoteWithCtor16BodyImpl: c.Expr[Data] = testExprQuoteWithCtor16Body

  def testExprQuoteWithCtor17BodyImpl: c.Expr[Data] = testExprQuoteWithCtor17Body

  def testExprQuoteWithCtor18BodyImpl: c.Expr[Data] = testExprQuoteWithCtor18Body

  def testExprQuoteWithCtor19BodyImpl: c.Expr[Data] = testExprQuoteWithCtor19Body

  def testExprQuoteWithCtor20BodyImpl: c.Expr[Data] = testExprQuoteWithCtor20Body

  def testExprQuoteWithCtor21BodyImpl: c.Expr[Data] = testExprQuoteWithCtor21Body

  def testExprQuoteWithCtor22BodyImpl: c.Expr[Data] = testExprQuoteWithCtor22Body

  def testCtorFromUntypedImpl: c.Expr[Data] = testCtorFromUntyped

  def testCtorExtract1Impl: c.Expr[Data] = {
    val ctor: UntypedType = c.weakTypeOf[Option[Int]].typeConstructor
    val optionCtor = Type.Ctor1.fromUntyped[Option](ctor)
    verifyExtractedCtor1Option(optionCtor)
  }

  def testCtorExtract2Impl: c.Expr[Data] = {
    val ctor: UntypedType = c.weakTypeOf[Either[String, Int]].typeConstructor
    val eitherCtor = Type.Ctor2.fromUntyped[Either](ctor)
    verifyExtractedCtor2Either(eitherCtor)
  }

  def testCtorExtract3Impl: c.Expr[Data] = {
    import hearth.examples.kinds.Arity3
    val ctor: UntypedType = c.weakTypeOf[Arity3[Int, String, Boolean]].typeConstructor
    val arity3Ctor = Type.Ctor3.fromUntyped[Arity3](ctor)
    verifyExtractedCtor3Arity3(arity3Ctor)
  }

  def testCtorExtract4Impl: c.Expr[Data] = {
    import hearth.examples.kinds.Arity4
    val ctor: UntypedType = c.weakTypeOf[Arity4[Int, Int, Int, Int]].typeConstructor
    val arity4Ctor = Type.Ctor4.fromUntyped[Arity4](ctor)
    verifyExtractedCtor4Arity4(arity4Ctor)
  }

  def testCtorExtract5Impl: c.Expr[Data] = {
    import hearth.examples.kinds.Arity5
    val ctor: UntypedType = c.weakTypeOf[Arity5[Int, Int, Int, Int, Int]].typeConstructor
    val arity5Ctor = Type.Ctor5.fromUntyped[Arity5](ctor)
    verifyExtractedCtor5Arity5(arity5Ctor)
  }

  def testCtorExtract6Impl: c.Expr[Data] = {
    import hearth.examples.kinds.Arity6
    val ctor: UntypedType = c.weakTypeOf[Arity6[Int, Int, Int, Int, Int, Int]].typeConstructor
    val arity6Ctor = Type.Ctor6.fromUntyped[Arity6](ctor)
    verifyExtractedCtor6Arity6(arity6Ctor)
  }

  def testCtorExtract7Impl: c.Expr[Data] = {
    import hearth.examples.kinds.Arity7
    val ctor: UntypedType = c.weakTypeOf[Arity7[Int, Int, Int, Int, Int, Int, Int]].typeConstructor
    val arity7Ctor = Type.Ctor7.fromUntyped[Arity7](ctor)
    verifyExtractedCtor7Arity7(arity7Ctor)
  }

  def testCtorExtract8Impl: c.Expr[Data] = {
    import hearth.examples.kinds.Arity8
    val ctor: UntypedType = c.weakTypeOf[Arity8[Int, Int, Int, Int, Int, Int, Int, Int]].typeConstructor
    val arity8Ctor = Type.Ctor8.fromUntyped[Arity8](ctor)
    verifyExtractedCtor8Arity8(arity8Ctor)
  }

  def testCtorExtract9Impl: c.Expr[Data] = {
    import hearth.examples.kinds.Arity9
    val ctor: UntypedType = c.weakTypeOf[Arity9[Int, Int, Int, Int, Int, Int, Int, Int, Int]].typeConstructor
    val arity9Ctor = Type.Ctor9.fromUntyped[Arity9](ctor)
    verifyExtractedCtor9Arity9(arity9Ctor)
  }

  def testCtorExtract10Impl: c.Expr[Data] = {
    import hearth.examples.kinds.Arity10
    val ctor: UntypedType = c.weakTypeOf[Arity10[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]].typeConstructor
    val arity10Ctor = Type.Ctor10.fromUntyped[Arity10](ctor)
    verifyExtractedCtor10Arity10(arity10Ctor)
  }

  def testCtorExtract11Impl: c.Expr[Data] = {
    import hearth.examples.kinds.Arity11
    val ctor: UntypedType = c.weakTypeOf[Arity11[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]].typeConstructor
    val arity11Ctor = Type.Ctor11.fromUntyped[Arity11](ctor)
    verifyExtractedCtor11Arity11(arity11Ctor)
  }

  def testCtorExtract12Impl: c.Expr[Data] = {
    import hearth.examples.kinds.Arity12
    val ctor: UntypedType =
      c.weakTypeOf[Arity12[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]].typeConstructor
    val arity12Ctor = Type.Ctor12.fromUntyped[Arity12](ctor)
    verifyExtractedCtor12Arity12(arity12Ctor)
  }

  def testCtorExtract13Impl: c.Expr[Data] = {
    import hearth.examples.kinds.Arity13
    val ctor: UntypedType =
      c.weakTypeOf[Arity13[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]].typeConstructor
    val arity13Ctor = Type.Ctor13.fromUntyped[Arity13](ctor)
    verifyExtractedCtor13Arity13(arity13Ctor)
  }

  def testCtorExtract14Impl: c.Expr[Data] = {
    import hearth.examples.kinds.Arity14
    val ctor: UntypedType =
      c.weakTypeOf[Arity14[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]].typeConstructor
    val arity14Ctor = Type.Ctor14.fromUntyped[Arity14](ctor)
    verifyExtractedCtor14Arity14(arity14Ctor)
  }

  def testCtorExtract15Impl: c.Expr[Data] = {
    import hearth.examples.kinds.Arity15
    val ctor: UntypedType =
      c.weakTypeOf[Arity15[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]].typeConstructor
    val arity15Ctor = Type.Ctor15.fromUntyped[Arity15](ctor)
    verifyExtractedCtor15Arity15(arity15Ctor)
  }

  def testCtorExtract16Impl: c.Expr[Data] = {
    import hearth.examples.kinds.Arity16
    val ctor: UntypedType = c
      .weakTypeOf[Arity16[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]]
      .typeConstructor
    val arity16Ctor = Type.Ctor16.fromUntyped[Arity16](ctor)
    verifyExtractedCtor16Arity16(arity16Ctor)
  }

  def testCtorExtract17Impl: c.Expr[Data] = {
    import hearth.examples.kinds.Arity17
    val ctor: UntypedType = c
      .weakTypeOf[Arity17[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]]
      .typeConstructor
    val arity17Ctor = Type.Ctor17.fromUntyped[Arity17](ctor)
    verifyExtractedCtor17Arity17(arity17Ctor)
  }

  def testCtorExtract18Impl: c.Expr[Data] = {
    import hearth.examples.kinds.Arity18
    val ctor: UntypedType = c
      .weakTypeOf[Arity18[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]]
      .typeConstructor
    val arity18Ctor = Type.Ctor18.fromUntyped[Arity18](ctor)
    verifyExtractedCtor18Arity18(arity18Ctor)
  }

  def testCtorExtract19Impl: c.Expr[Data] = {
    import hearth.examples.kinds.Arity19
    val ctor: UntypedType = c
      .weakTypeOf[
        Arity19[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]
      ]
      .typeConstructor
    val arity19Ctor = Type.Ctor19.fromUntyped[Arity19](ctor)
    verifyExtractedCtor19Arity19(arity19Ctor)
  }

  def testCtorExtract20Impl: c.Expr[Data] = {
    import hearth.examples.kinds.Arity20
    val ctor: UntypedType = c
      .weakTypeOf[
        Arity20[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]
      ]
      .typeConstructor
    val arity20Ctor = Type.Ctor20.fromUntyped[Arity20](ctor)
    verifyExtractedCtor20Arity20(arity20Ctor)
  }

  def testCtorExtract21Impl: c.Expr[Data] = {
    import hearth.examples.kinds.Arity21
    val ctor: UntypedType = c
      .weakTypeOf[
        Arity21[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]
      ]
      .typeConstructor
    val arity21Ctor = Type.Ctor21.fromUntyped[Arity21](ctor)
    verifyExtractedCtor21Arity21(arity21Ctor)
  }

  def testCtorExtract22Impl: c.Expr[Data] = {
    import hearth.examples.kinds.Arity22
    val ctor: UntypedType =
      c.weakTypeOf[
        Arity22[
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int
        ]
      ].typeConstructor
    val arity22Ctor = Type.Ctor22.fromUntyped[Arity22](ctor)
    verifyExtractedCtor22Arity22(arity22Ctor)
  }

}

object CrossCtorInjectionFixtures {

  def testTypeOfWithImportedCtor1[A]: Data = macro CrossCtorInjectionFixtures.testTypeOfWithImportedCtor1Impl[A]

  def testCtorAsUntyped: Data = macro CrossCtorInjectionFixtures.testCtorAsUntypedImpl

  def testCtorSetAsUntyped: Data = macro CrossCtorInjectionFixtures.testCtorSetAsUntypedImpl

  def testTypeOfWithCtor1Higher: Data = macro CrossCtorInjectionFixtures.testTypeOfWithCtor1HigherImpl

  def testTypeOfWithCtor2Higher: Data = macro CrossCtorInjectionFixtures.testTypeOfWithCtor2HigherImpl

  def testTypeOfWithCtor3Higher: Data = macro CrossCtorInjectionFixtures.testTypeOfWithCtor3HigherImpl

  def testTypeOfWithCtor4Higher: Data = macro CrossCtorInjectionFixtures.testTypeOfWithCtor4HigherImpl

  def testTypeOfWithCtor5Higher: Data = macro CrossCtorInjectionFixtures.testTypeOfWithCtor5HigherImpl

  def testTypeOfWithCtor6Higher: Data = macro CrossCtorInjectionFixtures.testTypeOfWithCtor6HigherImpl

  def testTypeOfWithCtor7Higher: Data = macro CrossCtorInjectionFixtures.testTypeOfWithCtor7HigherImpl

  def testTypeOfWithCtor8Higher: Data = macro CrossCtorInjectionFixtures.testTypeOfWithCtor8HigherImpl

  def testTypeOfWithCtor9Higher: Data = macro CrossCtorInjectionFixtures.testTypeOfWithCtor9HigherImpl

  def testTypeOfWithCtor10Higher: Data = macro CrossCtorInjectionFixtures.testTypeOfWithCtor10HigherImpl

  def testTypeOfWithCtor11Higher: Data = macro CrossCtorInjectionFixtures.testTypeOfWithCtor11HigherImpl

  def testTypeOfWithCtor12Higher: Data = macro CrossCtorInjectionFixtures.testTypeOfWithCtor12HigherImpl

  def testTypeOfWithCtor13Higher: Data = macro CrossCtorInjectionFixtures.testTypeOfWithCtor13HigherImpl

  def testTypeOfWithCtor14Higher: Data = macro CrossCtorInjectionFixtures.testTypeOfWithCtor14HigherImpl

  def testTypeOfWithCtor15Higher: Data = macro CrossCtorInjectionFixtures.testTypeOfWithCtor15HigherImpl

  def testTypeOfWithCtor16Higher: Data = macro CrossCtorInjectionFixtures.testTypeOfWithCtor16HigherImpl

  def testTypeOfWithCtor17Higher: Data = macro CrossCtorInjectionFixtures.testTypeOfWithCtor17HigherImpl

  def testTypeOfWithCtor18Higher: Data = macro CrossCtorInjectionFixtures.testTypeOfWithCtor18HigherImpl

  def testTypeOfWithCtor19Higher: Data = macro CrossCtorInjectionFixtures.testTypeOfWithCtor19HigherImpl

  def testTypeOfWithCtor20Higher: Data = macro CrossCtorInjectionFixtures.testTypeOfWithCtor20HigherImpl

  def testTypeOfWithCtor21Higher: Data = macro CrossCtorInjectionFixtures.testTypeOfWithCtor21HigherImpl

  def testTypeOfWithCtor22Higher: Data = macro CrossCtorInjectionFixtures.testTypeOfWithCtor22HigherImpl

  def testExprQuoteWithCtor1Body: Data = macro CrossCtorInjectionFixtures.testExprQuoteWithCtor1BodyImpl

  def testExprQuoteWithCtor2Body: Data = macro CrossCtorInjectionFixtures.testExprQuoteWithCtor2BodyImpl

  def testExprQuoteWithCtor3Body: Data = macro CrossCtorInjectionFixtures.testExprQuoteWithCtor3BodyImpl

  def testExprQuoteWithCtor4Body: Data = macro CrossCtorInjectionFixtures.testExprQuoteWithCtor4BodyImpl

  def testExprQuoteWithCtor5Body: Data = macro CrossCtorInjectionFixtures.testExprQuoteWithCtor5BodyImpl

  def testExprQuoteWithCtor6Body: Data = macro CrossCtorInjectionFixtures.testExprQuoteWithCtor6BodyImpl

  def testExprQuoteWithCtor7Body: Data = macro CrossCtorInjectionFixtures.testExprQuoteWithCtor7BodyImpl

  def testExprQuoteWithCtor8Body: Data = macro CrossCtorInjectionFixtures.testExprQuoteWithCtor8BodyImpl

  def testExprQuoteWithCtor9Body: Data = macro CrossCtorInjectionFixtures.testExprQuoteWithCtor9BodyImpl

  def testExprQuoteWithCtor10Body: Data = macro CrossCtorInjectionFixtures.testExprQuoteWithCtor10BodyImpl

  def testExprQuoteWithCtor11Body: Data = macro CrossCtorInjectionFixtures.testExprQuoteWithCtor11BodyImpl

  def testExprQuoteWithCtor12Body: Data = macro CrossCtorInjectionFixtures.testExprQuoteWithCtor12BodyImpl

  def testExprQuoteWithCtor13Body: Data = macro CrossCtorInjectionFixtures.testExprQuoteWithCtor13BodyImpl

  def testExprQuoteWithCtor14Body: Data = macro CrossCtorInjectionFixtures.testExprQuoteWithCtor14BodyImpl

  def testExprQuoteWithCtor15Body: Data = macro CrossCtorInjectionFixtures.testExprQuoteWithCtor15BodyImpl

  def testExprQuoteWithCtor16Body: Data = macro CrossCtorInjectionFixtures.testExprQuoteWithCtor16BodyImpl

  def testExprQuoteWithCtor17Body: Data = macro CrossCtorInjectionFixtures.testExprQuoteWithCtor17BodyImpl

  def testExprQuoteWithCtor18Body: Data = macro CrossCtorInjectionFixtures.testExprQuoteWithCtor18BodyImpl

  def testExprQuoteWithCtor19Body: Data = macro CrossCtorInjectionFixtures.testExprQuoteWithCtor19BodyImpl

  def testExprQuoteWithCtor20Body: Data = macro CrossCtorInjectionFixtures.testExprQuoteWithCtor20BodyImpl

  def testExprQuoteWithCtor21Body: Data = macro CrossCtorInjectionFixtures.testExprQuoteWithCtor21BodyImpl

  def testExprQuoteWithCtor22Body: Data = macro CrossCtorInjectionFixtures.testExprQuoteWithCtor22BodyImpl

  def testCtorFromUntyped: Data = macro CrossCtorInjectionFixtures.testCtorFromUntypedImpl

  def testCtorExtract1: Data = macro CrossCtorInjectionFixtures.testCtorExtract1Impl

  def testCtorExtract2: Data = macro CrossCtorInjectionFixtures.testCtorExtract2Impl

  def testCtorExtract3: Data = macro CrossCtorInjectionFixtures.testCtorExtract3Impl

  def testCtorExtract4: Data = macro CrossCtorInjectionFixtures.testCtorExtract4Impl

  def testCtorExtract5: Data = macro CrossCtorInjectionFixtures.testCtorExtract5Impl

  def testCtorExtract6: Data = macro CrossCtorInjectionFixtures.testCtorExtract6Impl

  def testCtorExtract7: Data = macro CrossCtorInjectionFixtures.testCtorExtract7Impl

  def testCtorExtract8: Data = macro CrossCtorInjectionFixtures.testCtorExtract8Impl

  def testCtorExtract9: Data = macro CrossCtorInjectionFixtures.testCtorExtract9Impl

  def testCtorExtract10: Data = macro CrossCtorInjectionFixtures.testCtorExtract10Impl

  def testCtorExtract11: Data = macro CrossCtorInjectionFixtures.testCtorExtract11Impl

  def testCtorExtract12: Data = macro CrossCtorInjectionFixtures.testCtorExtract12Impl

  def testCtorExtract13: Data = macro CrossCtorInjectionFixtures.testCtorExtract13Impl

  def testCtorExtract14: Data = macro CrossCtorInjectionFixtures.testCtorExtract14Impl

  def testCtorExtract15: Data = macro CrossCtorInjectionFixtures.testCtorExtract15Impl

  def testCtorExtract16: Data = macro CrossCtorInjectionFixtures.testCtorExtract16Impl

  def testCtorExtract17: Data = macro CrossCtorInjectionFixtures.testCtorExtract17Impl

  def testCtorExtract18: Data = macro CrossCtorInjectionFixtures.testCtorExtract18Impl

  def testCtorExtract19: Data = macro CrossCtorInjectionFixtures.testCtorExtract19Impl

  def testCtorExtract20: Data = macro CrossCtorInjectionFixtures.testCtorExtract20Impl

  def testCtorExtract21: Data = macro CrossCtorInjectionFixtures.testCtorExtract21Impl

  def testCtorExtract22: Data = macro CrossCtorInjectionFixtures.testCtorExtract22Impl

}
