package hearth
package typed

import hearth.data.Data

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

final private class SourceTextFixtures(val c: blackbox.Context)
    extends MacroCommonsScala2
    with SourceTextFixturesImpl {

  // NoPosition has no source, so sourceCode is None.
  protected def noSourcePositionSourceCode: Option[String] = Position.sourceCode(c.universe.NoPosition)

  def testExprSourceTextImpl[A: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[Data] =
    testExprSourceText[A](expr)

  def testSyntheticSourceTextImpl: c.Expr[Data] =
    testSyntheticSourceText

  def testDestructuredSourceTextImpl[A: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[Data] =
    testDestructuredSourceText[A](expr)
}

object SourceTextFixtures {

  def testExprSourceText[A](expr: A): Data = macro SourceTextFixtures.testExprSourceTextImpl[A]

  def testSyntheticSourceText: Data = macro SourceTextFixtures.testSyntheticSourceTextImpl

  def testDestructuredSourceText[A](expr: A): Data = macro SourceTextFixtures.testDestructuredSourceTextImpl[A]
}
