package hearth
package crossquotes

import hearth.data.Data

import scala.quoted.*

final private class CrossCtorInjectionFixtures(q: Quotes)
    extends MacroCommonsScala3(using q),
      CrossCtorInjectionFixturesImpl {

  def testCtorExtract1: Expr[Data] = {
    import quotes.reflect.*
    val repr = TypeRepr.of[Option[Int]]
    val ctor = repr match {
      case AppliedType(tycon, _) => tycon.asInstanceOf[UntypedType]
      case _                     => throw new Exception("Expected AppliedType for Option[Int]")
    }
    val optionCtor = Type.Ctor1.fromUntyped[Option](ctor)
    verifyExtractedCtor1Option(optionCtor)
  }

  def testCtorExtract2: Expr[Data] = {
    import quotes.reflect.*
    val repr = TypeRepr.of[Either[String, Int]]
    val ctor = repr match {
      case AppliedType(tycon, _) => tycon.asInstanceOf[UntypedType]
      case _                     => throw new Exception("Expected AppliedType for Either[String, Int]")
    }
    val eitherCtor = Type.Ctor2.fromUntyped[Either](ctor)
    verifyExtractedCtor2Either(eitherCtor)
  }

  def testCtorExtract3: Expr[Data] = {
    import quotes.reflect.*
    import hearth.examples.kinds.Arity3
    val repr = TypeRepr.of[Arity3[Int, String, Boolean]]
    val ctor = repr match {
      case AppliedType(tycon, _) => tycon.asInstanceOf[UntypedType]
      case _                     => throw new Exception("Expected AppliedType for Arity3[Int, String, Boolean]")
    }
    val arity3Ctor = Type.Ctor3.fromUntyped[Arity3](ctor)
    verifyExtractedCtor3Arity3(arity3Ctor)
  }

  def testCtorExtract4: Expr[Data] = {
    import quotes.reflect.*
    import hearth.examples.kinds.Arity4
    val repr = TypeRepr.of[Arity4[Int, Int, Int, Int]]
    val ctor = repr match {
      case AppliedType(tycon, _) => tycon.asInstanceOf[UntypedType]
      case _                     => throw new Exception("Expected AppliedType for Arity4[...]")
    }
    val arity4Ctor = Type.Ctor4.fromUntyped[Arity4](ctor)
    verifyExtractedCtor4Arity4(arity4Ctor)
  }

  def testCtorExtract5: Expr[Data] = {
    import quotes.reflect.*
    import hearth.examples.kinds.Arity5
    val repr = TypeRepr.of[Arity5[Int, Int, Int, Int, Int]]
    val ctor = repr match {
      case AppliedType(tycon, _) => tycon.asInstanceOf[UntypedType]
      case _                     => throw new Exception("Expected AppliedType for Arity5[...]")
    }
    val arity5Ctor = Type.Ctor5.fromUntyped[Arity5](ctor)
    verifyExtractedCtor5Arity5(arity5Ctor)
  }

  def testCtorExtract6: Expr[Data] = {
    import quotes.reflect.*
    import hearth.examples.kinds.Arity6
    val repr = TypeRepr.of[Arity6[Int, Int, Int, Int, Int, Int]]
    val ctor = repr match {
      case AppliedType(tycon, _) => tycon.asInstanceOf[UntypedType]
      case _                     => throw new Exception("Expected AppliedType for Arity6[...]")
    }
    val arity6Ctor = Type.Ctor6.fromUntyped[Arity6](ctor)
    verifyExtractedCtor6Arity6(arity6Ctor)
  }

  def testCtorExtract7: Expr[Data] = {
    import quotes.reflect.*
    import hearth.examples.kinds.Arity7
    val repr = TypeRepr.of[Arity7[Int, Int, Int, Int, Int, Int, Int]]
    val ctor = repr match {
      case AppliedType(tycon, _) => tycon.asInstanceOf[UntypedType]
      case _                     => throw new Exception("Expected AppliedType for Arity7[...]")
    }
    val arity7Ctor = Type.Ctor7.fromUntyped[Arity7](ctor)
    verifyExtractedCtor7Arity7(arity7Ctor)
  }

  def testCtorExtract8: Expr[Data] = {
    import quotes.reflect.*
    import hearth.examples.kinds.Arity8
    val repr = TypeRepr.of[Arity8[Int, Int, Int, Int, Int, Int, Int, Int]]
    val ctor = repr match {
      case AppliedType(tycon, _) => tycon.asInstanceOf[UntypedType]
      case _                     => throw new Exception("Expected AppliedType for Arity8[...]")
    }
    val arity8Ctor = Type.Ctor8.fromUntyped[Arity8](ctor)
    verifyExtractedCtor8Arity8(arity8Ctor)
  }

  def testCtorExtract9: Expr[Data] = {
    import quotes.reflect.*
    import hearth.examples.kinds.Arity9
    val repr = TypeRepr.of[Arity9[Int, Int, Int, Int, Int, Int, Int, Int, Int]]
    val ctor = repr match {
      case AppliedType(tycon, _) => tycon.asInstanceOf[UntypedType]
      case _                     => throw new Exception("Expected AppliedType for Arity9[...]")
    }
    val arity9Ctor = Type.Ctor9.fromUntyped[Arity9](ctor)
    verifyExtractedCtor9Arity9(arity9Ctor)
  }

  def testCtorExtract10: Expr[Data] = {
    import quotes.reflect.*
    import hearth.examples.kinds.Arity10
    val repr = TypeRepr.of[Arity10[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]]
    val ctor = repr match {
      case AppliedType(tycon, _) => tycon.asInstanceOf[UntypedType]
      case _                     => throw new Exception("Expected AppliedType for Arity10[...]")
    }
    val arity10Ctor = Type.Ctor10.fromUntyped[Arity10](ctor)
    verifyExtractedCtor10Arity10(arity10Ctor)
  }

  def testCtorExtract11: Expr[Data] = {
    import quotes.reflect.*
    import hearth.examples.kinds.Arity11
    val repr = TypeRepr.of[Arity11[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]]
    val ctor = repr match {
      case AppliedType(tycon, _) => tycon.asInstanceOf[UntypedType]
      case _                     => throw new Exception("Expected AppliedType for Arity11[...]")
    }
    val arity11Ctor = Type.Ctor11.fromUntyped[Arity11](ctor)
    verifyExtractedCtor11Arity11(arity11Ctor)
  }

  def testCtorExtract12: Expr[Data] = {
    import quotes.reflect.*
    import hearth.examples.kinds.Arity12
    val repr = TypeRepr.of[Arity12[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]]
    val ctor = repr match {
      case AppliedType(tycon, _) => tycon.asInstanceOf[UntypedType]
      case _                     => throw new Exception("Expected AppliedType for Arity12[...]")
    }
    val arity12Ctor = Type.Ctor12.fromUntyped[Arity12](ctor)
    verifyExtractedCtor12Arity12(arity12Ctor)
  }

  def testCtorExtract13: Expr[Data] = {
    import quotes.reflect.*
    import hearth.examples.kinds.Arity13
    val repr = TypeRepr.of[Arity13[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]]
    val ctor = repr match {
      case AppliedType(tycon, _) => tycon.asInstanceOf[UntypedType]
      case _                     => throw new Exception("Expected AppliedType for Arity13[...]")
    }
    val arity13Ctor = Type.Ctor13.fromUntyped[Arity13](ctor)
    verifyExtractedCtor13Arity13(arity13Ctor)
  }

  def testCtorExtract14: Expr[Data] = {
    import quotes.reflect.*
    import hearth.examples.kinds.Arity14
    val repr = TypeRepr.of[Arity14[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]]
    val ctor = repr match {
      case AppliedType(tycon, _) => tycon.asInstanceOf[UntypedType]
      case _                     => throw new Exception("Expected AppliedType for Arity14[...]")
    }
    val arity14Ctor = Type.Ctor14.fromUntyped[Arity14](ctor)
    verifyExtractedCtor14Arity14(arity14Ctor)
  }

  def testCtorExtract15: Expr[Data] = {
    import quotes.reflect.*
    import hearth.examples.kinds.Arity15
    val repr = TypeRepr.of[Arity15[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]]
    val ctor = repr match {
      case AppliedType(tycon, _) => tycon.asInstanceOf[UntypedType]
      case _                     => throw new Exception("Expected AppliedType for Arity15[...]")
    }
    val arity15Ctor = Type.Ctor15.fromUntyped[Arity15](ctor)
    verifyExtractedCtor15Arity15(arity15Ctor)
  }

  def testCtorExtract16: Expr[Data] = {
    import quotes.reflect.*
    import hearth.examples.kinds.Arity16
    val repr = TypeRepr.of[Arity16[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]]
    val ctor = repr match {
      case AppliedType(tycon, _) => tycon.asInstanceOf[UntypedType]
      case _                     => throw new Exception("Expected AppliedType for Arity16[...]")
    }
    val arity16Ctor = Type.Ctor16.fromUntyped[Arity16](ctor)
    verifyExtractedCtor16Arity16(arity16Ctor)
  }

  def testCtorExtract17: Expr[Data] = {
    import quotes.reflect.*
    import hearth.examples.kinds.Arity17
    val repr = TypeRepr.of[Arity17[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]]
    val ctor = repr match {
      case AppliedType(tycon, _) => tycon.asInstanceOf[UntypedType]
      case _                     => throw new Exception("Expected AppliedType for Arity17[...]")
    }
    val arity17Ctor = Type.Ctor17.fromUntyped[Arity17](ctor)
    verifyExtractedCtor17Arity17(arity17Ctor)
  }

  def testCtorExtract18: Expr[Data] = {
    import quotes.reflect.*
    import hearth.examples.kinds.Arity18
    val repr =
      TypeRepr.of[Arity18[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]]
    val ctor = repr match {
      case AppliedType(tycon, _) => tycon.asInstanceOf[UntypedType]
      case _                     => throw new Exception("Expected AppliedType for Arity18[...]")
    }
    val arity18Ctor = Type.Ctor18.fromUntyped[Arity18](ctor)
    verifyExtractedCtor18Arity18(arity18Ctor)
  }

  def testCtorExtract19: Expr[Data] = {
    import quotes.reflect.*
    import hearth.examples.kinds.Arity19
    val repr = TypeRepr
      .of[Arity19[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]]
    val ctor = repr match {
      case AppliedType(tycon, _) => tycon.asInstanceOf[UntypedType]
      case _                     => throw new Exception("Expected AppliedType for Arity19[...]")
    }
    val arity19Ctor = Type.Ctor19.fromUntyped[Arity19](ctor)
    verifyExtractedCtor19Arity19(arity19Ctor)
  }

  def testCtorExtract20: Expr[Data] = {
    import quotes.reflect.*
    import hearth.examples.kinds.Arity20
    val repr = TypeRepr
      .of[Arity20[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]]
    val ctor = repr match {
      case AppliedType(tycon, _) => tycon.asInstanceOf[UntypedType]
      case _                     => throw new Exception("Expected AppliedType for Arity20[...]")
    }
    val arity20Ctor = Type.Ctor20.fromUntyped[Arity20](ctor)
    verifyExtractedCtor20Arity20(arity20Ctor)
  }

  def testCtorExtract21: Expr[Data] = {
    import quotes.reflect.*
    import hearth.examples.kinds.Arity21
    val repr = TypeRepr.of[
      Arity21[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]
    ]
    val ctor = repr match {
      case AppliedType(tycon, _) => tycon.asInstanceOf[UntypedType]
      case _                     => throw new Exception("Expected AppliedType for Arity21[...]")
    }
    val arity21Ctor = Type.Ctor21.fromUntyped[Arity21](ctor)
    verifyExtractedCtor21Arity21(arity21Ctor)
  }

  def testCtorExtract22: Expr[Data] = {
    import quotes.reflect.*
    import hearth.examples.kinds.Arity22
    val repr =
      TypeRepr.of[Arity22[
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
      ]]
    val ctor = repr match {
      case AppliedType(tycon, _) => tycon.asInstanceOf[UntypedType]
      case _                     => throw new Exception("Expected AppliedType for Arity22[...]")
    }
    val arity22Ctor = Type.Ctor22.fromUntyped[Arity22](ctor)
    verifyExtractedCtor22Arity22(arity22Ctor)
  }
}

object CrossCtorInjectionFixtures {

  inline def testTypeOfWithImportedCtor1[A]: Data = ${ testTypeOfWithImportedCtor1Impl[A] }
  private def testTypeOfWithImportedCtor1Impl[A: Type](using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testTypeOfWithImportedCtor1[A]

  inline def testCtorAsUntyped: Data = ${ testCtorAsUntypedImpl }
  private def testCtorAsUntypedImpl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testCtorAsUntyped

  inline def testCtorSetAsUntyped: Data = ${ testCtorSetAsUntypedImpl }
  private def testCtorSetAsUntypedImpl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testCtorSetAsUntyped

  inline def testTypeOfWithCtor1Higher: Data = ${ testTypeOfWithCtor1HigherImpl }
  private def testTypeOfWithCtor1HigherImpl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testTypeOfWithCtor1Higher

  inline def testTypeOfWithCtor2Higher: Data = ${ testTypeOfWithCtor2HigherImpl }
  private def testTypeOfWithCtor2HigherImpl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testTypeOfWithCtor2Higher

  inline def testTypeOfWithCtor3Higher: Data = ${ testTypeOfWithCtor3HigherImpl }
  private def testTypeOfWithCtor3HigherImpl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testTypeOfWithCtor3Higher

  inline def testTypeOfWithCtor4Higher: Data = ${ testTypeOfWithCtor4HigherImpl }
  private def testTypeOfWithCtor4HigherImpl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testTypeOfWithCtor4Higher

  inline def testTypeOfWithCtor5Higher: Data = ${ testTypeOfWithCtor5HigherImpl }
  private def testTypeOfWithCtor5HigherImpl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testTypeOfWithCtor5Higher

  inline def testTypeOfWithCtor6Higher: Data = ${ testTypeOfWithCtor6HigherImpl }
  private def testTypeOfWithCtor6HigherImpl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testTypeOfWithCtor6Higher

  inline def testTypeOfWithCtor7Higher: Data = ${ testTypeOfWithCtor7HigherImpl }
  private def testTypeOfWithCtor7HigherImpl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testTypeOfWithCtor7Higher

  inline def testTypeOfWithCtor8Higher: Data = ${ testTypeOfWithCtor8HigherImpl }
  private def testTypeOfWithCtor8HigherImpl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testTypeOfWithCtor8Higher

  inline def testTypeOfWithCtor9Higher: Data = ${ testTypeOfWithCtor9HigherImpl }
  private def testTypeOfWithCtor9HigherImpl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testTypeOfWithCtor9Higher

  inline def testTypeOfWithCtor10Higher: Data = ${ testTypeOfWithCtor10HigherImpl }
  private def testTypeOfWithCtor10HigherImpl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testTypeOfWithCtor10Higher

  inline def testTypeOfWithCtor11Higher: Data = ${ testTypeOfWithCtor11HigherImpl }
  private def testTypeOfWithCtor11HigherImpl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testTypeOfWithCtor11Higher

  inline def testTypeOfWithCtor12Higher: Data = ${ testTypeOfWithCtor12HigherImpl }
  private def testTypeOfWithCtor12HigherImpl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testTypeOfWithCtor12Higher

  inline def testTypeOfWithCtor13Higher: Data = ${ testTypeOfWithCtor13HigherImpl }
  private def testTypeOfWithCtor13HigherImpl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testTypeOfWithCtor13Higher

  inline def testTypeOfWithCtor14Higher: Data = ${ testTypeOfWithCtor14HigherImpl }
  private def testTypeOfWithCtor14HigherImpl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testTypeOfWithCtor14Higher

  inline def testTypeOfWithCtor15Higher: Data = ${ testTypeOfWithCtor15HigherImpl }
  private def testTypeOfWithCtor15HigherImpl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testTypeOfWithCtor15Higher

  inline def testTypeOfWithCtor16Higher: Data = ${ testTypeOfWithCtor16HigherImpl }
  private def testTypeOfWithCtor16HigherImpl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testTypeOfWithCtor16Higher

  inline def testTypeOfWithCtor17Higher: Data = ${ testTypeOfWithCtor17HigherImpl }
  private def testTypeOfWithCtor17HigherImpl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testTypeOfWithCtor17Higher

  inline def testTypeOfWithCtor18Higher: Data = ${ testTypeOfWithCtor18HigherImpl }
  private def testTypeOfWithCtor18HigherImpl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testTypeOfWithCtor18Higher

  inline def testTypeOfWithCtor19Higher: Data = ${ testTypeOfWithCtor19HigherImpl }
  private def testTypeOfWithCtor19HigherImpl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testTypeOfWithCtor19Higher

  inline def testTypeOfWithCtor20Higher: Data = ${ testTypeOfWithCtor20HigherImpl }
  private def testTypeOfWithCtor20HigherImpl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testTypeOfWithCtor20Higher

  inline def testTypeOfWithCtor21Higher: Data = ${ testTypeOfWithCtor21HigherImpl }
  private def testTypeOfWithCtor21HigherImpl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testTypeOfWithCtor21Higher

  inline def testTypeOfWithCtor22Higher: Data = ${ testTypeOfWithCtor22HigherImpl }
  private def testTypeOfWithCtor22HigherImpl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testTypeOfWithCtor22Higher

  inline def testExprQuoteWithCtor1Body: Data = ${ testExprQuoteWithCtor1BodyImpl }
  private def testExprQuoteWithCtor1BodyImpl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testExprQuoteWithCtor1Body

  inline def testExprQuoteWithCtor2Body: Data = ${ testExprQuoteWithCtor2BodyImpl }
  private def testExprQuoteWithCtor2BodyImpl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testExprQuoteWithCtor2Body

  inline def testExprQuoteWithCtor3Body: Data = ${ testExprQuoteWithCtor3BodyImpl }
  private def testExprQuoteWithCtor3BodyImpl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testExprQuoteWithCtor3Body

  inline def testExprQuoteWithCtor4Body: Data = ${ testExprQuoteWithCtor4BodyImpl }
  private def testExprQuoteWithCtor4BodyImpl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testExprQuoteWithCtor4Body

  inline def testExprQuoteWithCtor5Body: Data = ${ testExprQuoteWithCtor5BodyImpl }
  private def testExprQuoteWithCtor5BodyImpl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testExprQuoteWithCtor5Body

  inline def testExprQuoteWithCtor6Body: Data = ${ testExprQuoteWithCtor6BodyImpl }
  private def testExprQuoteWithCtor6BodyImpl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testExprQuoteWithCtor6Body

  inline def testExprQuoteWithCtor7Body: Data = ${ testExprQuoteWithCtor7BodyImpl }
  private def testExprQuoteWithCtor7BodyImpl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testExprQuoteWithCtor7Body

  inline def testExprQuoteWithCtor8Body: Data = ${ testExprQuoteWithCtor8BodyImpl }
  private def testExprQuoteWithCtor8BodyImpl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testExprQuoteWithCtor8Body

  inline def testExprQuoteWithCtor9Body: Data = ${ testExprQuoteWithCtor9BodyImpl }
  private def testExprQuoteWithCtor9BodyImpl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testExprQuoteWithCtor9Body

  inline def testExprQuoteWithCtor10Body: Data = ${ testExprQuoteWithCtor10BodyImpl }
  private def testExprQuoteWithCtor10BodyImpl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testExprQuoteWithCtor10Body

  inline def testExprQuoteWithCtor11Body: Data = ${ testExprQuoteWithCtor11BodyImpl }
  private def testExprQuoteWithCtor11BodyImpl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testExprQuoteWithCtor11Body

  inline def testExprQuoteWithCtor12Body: Data = ${ testExprQuoteWithCtor12BodyImpl }
  private def testExprQuoteWithCtor12BodyImpl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testExprQuoteWithCtor12Body

  inline def testExprQuoteWithCtor13Body: Data = ${ testExprQuoteWithCtor13BodyImpl }
  private def testExprQuoteWithCtor13BodyImpl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testExprQuoteWithCtor13Body

  inline def testExprQuoteWithCtor14Body: Data = ${ testExprQuoteWithCtor14BodyImpl }
  private def testExprQuoteWithCtor14BodyImpl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testExprQuoteWithCtor14Body

  inline def testExprQuoteWithCtor15Body: Data = ${ testExprQuoteWithCtor15BodyImpl }
  private def testExprQuoteWithCtor15BodyImpl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testExprQuoteWithCtor15Body

  inline def testExprQuoteWithCtor16Body: Data = ${ testExprQuoteWithCtor16BodyImpl }
  private def testExprQuoteWithCtor16BodyImpl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testExprQuoteWithCtor16Body

  inline def testExprQuoteWithCtor17Body: Data = ${ testExprQuoteWithCtor17BodyImpl }
  private def testExprQuoteWithCtor17BodyImpl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testExprQuoteWithCtor17Body

  inline def testExprQuoteWithCtor18Body: Data = ${ testExprQuoteWithCtor18BodyImpl }
  private def testExprQuoteWithCtor18BodyImpl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testExprQuoteWithCtor18Body

  inline def testExprQuoteWithCtor19Body: Data = ${ testExprQuoteWithCtor19BodyImpl }
  private def testExprQuoteWithCtor19BodyImpl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testExprQuoteWithCtor19Body

  inline def testExprQuoteWithCtor20Body: Data = ${ testExprQuoteWithCtor20BodyImpl }
  private def testExprQuoteWithCtor20BodyImpl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testExprQuoteWithCtor20Body

  inline def testExprQuoteWithCtor21Body: Data = ${ testExprQuoteWithCtor21BodyImpl }
  private def testExprQuoteWithCtor21BodyImpl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testExprQuoteWithCtor21Body

  inline def testExprQuoteWithCtor22Body: Data = ${ testExprQuoteWithCtor22BodyImpl }
  private def testExprQuoteWithCtor22BodyImpl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testExprQuoteWithCtor22Body

  inline def testCtorFromUntyped: Data = ${ testCtorFromUntypedImpl }
  private def testCtorFromUntypedImpl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testCtorFromUntyped

  inline def testCtorExtract1: Data = ${ testCtorExtract1Impl }
  private def testCtorExtract1Impl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testCtorExtract1

  inline def testCtorExtract2: Data = ${ testCtorExtract2Impl }
  private def testCtorExtract2Impl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testCtorExtract2

  inline def testCtorExtract3: Data = ${ testCtorExtract3Impl }
  private def testCtorExtract3Impl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testCtorExtract3

  inline def testCtorExtract4: Data = ${ testCtorExtract4Impl }
  private def testCtorExtract4Impl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testCtorExtract4

  inline def testCtorExtract5: Data = ${ testCtorExtract5Impl }
  private def testCtorExtract5Impl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testCtorExtract5

  inline def testCtorExtract6: Data = ${ testCtorExtract6Impl }
  private def testCtorExtract6Impl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testCtorExtract6

  inline def testCtorExtract7: Data = ${ testCtorExtract7Impl }
  private def testCtorExtract7Impl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testCtorExtract7

  inline def testCtorExtract8: Data = ${ testCtorExtract8Impl }
  private def testCtorExtract8Impl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testCtorExtract8

  inline def testCtorExtract9: Data = ${ testCtorExtract9Impl }
  private def testCtorExtract9Impl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testCtorExtract9

  inline def testCtorExtract10: Data = ${ testCtorExtract10Impl }
  private def testCtorExtract10Impl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testCtorExtract10

  inline def testCtorExtract11: Data = ${ testCtorExtract11Impl }
  private def testCtorExtract11Impl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testCtorExtract11

  inline def testCtorExtract12: Data = ${ testCtorExtract12Impl }
  private def testCtorExtract12Impl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testCtorExtract12

  inline def testCtorExtract13: Data = ${ testCtorExtract13Impl }
  private def testCtorExtract13Impl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testCtorExtract13

  inline def testCtorExtract14: Data = ${ testCtorExtract14Impl }
  private def testCtorExtract14Impl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testCtorExtract14

  inline def testCtorExtract15: Data = ${ testCtorExtract15Impl }
  private def testCtorExtract15Impl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testCtorExtract15

  inline def testCtorExtract16: Data = ${ testCtorExtract16Impl }
  private def testCtorExtract16Impl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testCtorExtract16

  inline def testCtorExtract17: Data = ${ testCtorExtract17Impl }
  private def testCtorExtract17Impl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testCtorExtract17

  inline def testCtorExtract18: Data = ${ testCtorExtract18Impl }
  private def testCtorExtract18Impl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testCtorExtract18

  inline def testCtorExtract19: Data = ${ testCtorExtract19Impl }
  private def testCtorExtract19Impl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testCtorExtract19

  inline def testCtorExtract20: Data = ${ testCtorExtract20Impl }
  private def testCtorExtract20Impl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testCtorExtract20

  inline def testCtorExtract21: Data = ${ testCtorExtract21Impl }
  private def testCtorExtract21Impl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testCtorExtract21

  inline def testCtorExtract22: Data = ${ testCtorExtract22Impl }
  private def testCtorExtract22Impl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testCtorExtract22

  // ===== GENERATED CODE =====

  inline def testCtorSetApplyUnapply2to8: Data = ${ testCtorSetApplyUnapply2to8Impl }
  private def testCtorSetApplyUnapply2to8Impl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testCtorSetApplyUnapply2to8

  inline def testCtorSetApplyUnapply9to15: Data = ${ testCtorSetApplyUnapply9to15Impl }
  private def testCtorSetApplyUnapply9to15Impl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testCtorSetApplyUnapply9to15

  inline def testCtorSetApplyUnapply16to22: Data = ${ testCtorSetApplyUnapply16to22Impl }
  private def testCtorSetApplyUnapply16to22Impl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testCtorSetApplyUnapply16to22

  inline def testCtorSetMiddleAsUntyped3to7: Data = ${ testCtorSetMiddleAsUntyped3to7Impl }
  private def testCtorSetMiddleAsUntyped3to7Impl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testCtorSetMiddleAsUntyped3to7

  inline def testCtorSetMiddleAsUntyped8to12: Data = ${ testCtorSetMiddleAsUntyped8to12Impl }
  private def testCtorSetMiddleAsUntyped8to12Impl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testCtorSetMiddleAsUntyped8to12

  inline def testCtorSetMiddleAsUntyped13to17: Data = ${ testCtorSetMiddleAsUntyped13to17Impl }
  private def testCtorSetMiddleAsUntyped13to17Impl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testCtorSetMiddleAsUntyped13to17

  inline def testCtorSetMiddleAsUntyped18to22: Data = ${ testCtorSetMiddleAsUntyped18to22Impl }
  private def testCtorSetMiddleAsUntyped18to22Impl(using q: Quotes): Expr[Data] =
    new CrossCtorInjectionFixtures(q).testCtorSetMiddleAsUntyped18to22

}
