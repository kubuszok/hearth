package hearth
package typed

import hearth.data.Data

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

final private class ExprCodecFixtures(val c: blackbox.Context) extends MacroCommonsScala2 with ExprCodecFixturesImpl {

  def testExprCodecRoundTripImpl[A: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[Data] =
    testExprCodecRoundTrip[A](expr)

  def testSemiQuoteReLiftImpl[A: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[Data] =
    testSemiQuoteReLift[A](expr)

  def testSemiQuotePrimitivesImpl: c.Expr[Data] = testSemiQuotePrimitives

  def testSemiQuoteCaseClassImpl: c.Expr[Data] = testSemiQuoteCaseClass

  def testSemiQuoteSealedChildImpl: c.Expr[Data] = testSemiQuoteSealedChild

  def testSemiQuoteSingletonImpl: c.Expr[Data] = testSemiQuoteSingleton

  def testSemiQuoteWithOverrideImpl: c.Expr[Data] = testSemiQuoteWithOverride

  def testSemiQuoteFailureImpl: c.Expr[Data] = testSemiQuoteFailure

  def testBuiltInCodecExprTypesImpl: c.Expr[Data] = testBuiltInCodecExprTypes

  def testDeriveToExprFailureImpl: c.Expr[Data] = testDeriveToExprFailure

  def testDeriveFromExprFailureImpl(
      expr: c.Expr[hearth.examples.expr_codecs.NotQuotable]
  ): c.Expr[Data] = testDeriveFromExprFailure(expr)

  def testOverrideMapReuseForRepeatedFieldTypeImpl: c.Expr[Data] = testOverrideMapReuseForRepeatedFieldType
}

object ExprCodecFixtures {

  def testExprCodecRoundTrip[A](expr: A): Data =
    macro ExprCodecFixtures.testExprCodecRoundTripImpl[A]

  def testSemiQuoteReLift[A](expr: A): Data =
    macro ExprCodecFixtures.testSemiQuoteReLiftImpl[A]

  def testSemiQuotePrimitives: Data = macro ExprCodecFixtures.testSemiQuotePrimitivesImpl

  def testSemiQuoteCaseClass: Data = macro ExprCodecFixtures.testSemiQuoteCaseClassImpl

  def testSemiQuoteSealedChild: Data = macro ExprCodecFixtures.testSemiQuoteSealedChildImpl

  def testSemiQuoteSingleton: Data = macro ExprCodecFixtures.testSemiQuoteSingletonImpl

  def testSemiQuoteWithOverride: Data = macro ExprCodecFixtures.testSemiQuoteWithOverrideImpl

  def testSemiQuoteFailure: Data = macro ExprCodecFixtures.testSemiQuoteFailureImpl

  def testBuiltInCodecExprTypes: Data = macro ExprCodecFixtures.testBuiltInCodecExprTypesImpl

  def testDeriveToExprFailure: Data = macro ExprCodecFixtures.testDeriveToExprFailureImpl

  def testDeriveFromExprFailure(expr: hearth.examples.expr_codecs.NotQuotable): Data =
    macro ExprCodecFixtures.testDeriveFromExprFailureImpl

  def testOverrideMapReuseForRepeatedFieldType: Data =
    macro ExprCodecFixtures.testOverrideMapReuseForRepeatedFieldTypeImpl
}
