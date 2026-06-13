package hearth
package typed

import hearth.data.Data

import scala.quoted.*

final private class ExprCodecFixtures(q: Quotes) extends MacroCommonsScala3(using q), ExprCodecFixturesImpl

object ExprCodecFixtures {

  inline def testExprCodecRoundTrip[A](inline expr: A): Data = ${
    testExprCodecRoundTripImpl[A]('expr)
  }
  private def testExprCodecRoundTripImpl[A: Type](expr: Expr[A])(using q: Quotes): Expr[Data] =
    new ExprCodecFixtures(q).testExprCodecRoundTrip[A](expr)

  inline def testSemiQuotePrimitives: Data = ${ testSemiQuotePrimitivesImpl }
  private def testSemiQuotePrimitivesImpl(using q: Quotes): Expr[Data] =
    new ExprCodecFixtures(q).testSemiQuotePrimitives

  inline def testSemiQuoteCaseClass: Data = ${ testSemiQuoteCaseClassImpl }
  private def testSemiQuoteCaseClassImpl(using q: Quotes): Expr[Data] =
    new ExprCodecFixtures(q).testSemiQuoteCaseClass

  inline def testSemiQuoteSealedChild: Data = ${ testSemiQuoteSealedChildImpl }
  private def testSemiQuoteSealedChildImpl(using q: Quotes): Expr[Data] =
    new ExprCodecFixtures(q).testSemiQuoteSealedChild

  inline def testSemiQuoteSingleton: Data = ${ testSemiQuoteSingletonImpl }
  private def testSemiQuoteSingletonImpl(using q: Quotes): Expr[Data] =
    new ExprCodecFixtures(q).testSemiQuoteSingleton

  inline def testSemiQuoteWithOverride: Data = ${ testSemiQuoteWithOverrideImpl }
  private def testSemiQuoteWithOverrideImpl(using q: Quotes): Expr[Data] =
    new ExprCodecFixtures(q).testSemiQuoteWithOverride

  inline def testSemiQuoteFailure: Data = ${ testSemiQuoteFailureImpl }
  private def testSemiQuoteFailureImpl(using q: Quotes): Expr[Data] =
    new ExprCodecFixtures(q).testSemiQuoteFailure

  inline def testBuiltInCodecExprTypes: Data = ${ testBuiltInCodecExprTypesImpl }
  private def testBuiltInCodecExprTypesImpl(using q: Quotes): Expr[Data] =
    new ExprCodecFixtures(q).testBuiltInCodecExprTypes

  inline def testDeriveToExprFailure: Data = ${ testDeriveToExprFailureImpl }
  private def testDeriveToExprFailureImpl(using q: Quotes): Expr[Data] =
    new ExprCodecFixtures(q).testDeriveToExprFailure

  inline def testDeriveFromExprFailure(inline expr: hearth.examples.expr_codecs.NotQuotable): Data = ${
    testDeriveFromExprFailureImpl('expr)
  }
  private def testDeriveFromExprFailureImpl(
      expr: Expr[hearth.examples.expr_codecs.NotQuotable]
  )(using q: Quotes): Expr[Data] =
    new ExprCodecFixtures(q).testDeriveFromExprFailure(expr)

  inline def testOverrideMapReuseForRepeatedFieldType: Data = ${ testOverrideMapReuseForRepeatedFieldTypeImpl }
  private def testOverrideMapReuseForRepeatedFieldTypeImpl(using q: Quotes): Expr[Data] =
    new ExprCodecFixtures(q).testOverrideMapReuseForRepeatedFieldType
}
