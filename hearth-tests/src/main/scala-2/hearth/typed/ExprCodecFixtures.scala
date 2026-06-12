package hearth
package typed

import hearth.data.Data

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

final private class ExprCodecFixtures(val c: blackbox.Context) extends MacroCommonsScala2 with ExprCodecFixturesImpl {

  def testExprCodecRoundTripImpl[A: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[Data] =
    testExprCodecRoundTrip[A](expr)

  def testSemiQuotePrimitivesImpl: c.Expr[Data] = testSemiQuotePrimitives

  def testSemiQuoteCaseClassImpl: c.Expr[Data] = testSemiQuoteCaseClass

  def testSemiQuoteSealedChildImpl: c.Expr[Data] = testSemiQuoteSealedChild

  def testSemiQuoteSingletonImpl: c.Expr[Data] = testSemiQuoteSingleton

  def testSemiQuoteWithOverrideImpl: c.Expr[Data] = testSemiQuoteWithOverride

  def testSemiQuoteFailureImpl: c.Expr[Data] = testSemiQuoteFailure
}

object ExprCodecFixtures {

  def testExprCodecRoundTrip[A](expr: A): Data =
    macro ExprCodecFixtures.testExprCodecRoundTripImpl[A]

  def testSemiQuotePrimitives: Data = macro ExprCodecFixtures.testSemiQuotePrimitivesImpl

  def testSemiQuoteCaseClass: Data = macro ExprCodecFixtures.testSemiQuoteCaseClassImpl

  def testSemiQuoteSealedChild: Data = macro ExprCodecFixtures.testSemiQuoteSealedChildImpl

  def testSemiQuoteSingleton: Data = macro ExprCodecFixtures.testSemiQuoteSingletonImpl

  def testSemiQuoteWithOverride: Data = macro ExprCodecFixtures.testSemiQuoteWithOverrideImpl

  def testSemiQuoteFailure: Data = macro ExprCodecFixtures.testSemiQuoteFailureImpl
}
