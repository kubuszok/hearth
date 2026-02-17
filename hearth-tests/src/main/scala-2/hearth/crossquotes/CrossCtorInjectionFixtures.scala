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

}
