package hearth
package crossquotes

import hearth.data.Data

import scala.quoted.*

final private class CrossCtorInjectionFixtures(q: Quotes)
    extends MacroCommonsScala3(using q),
      CrossCtorInjectionFixturesImpl

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

}
