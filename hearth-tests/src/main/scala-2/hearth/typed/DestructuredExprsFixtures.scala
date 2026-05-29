package hearth
package typed

import hearth.data.Data

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

final private class DestructuredExprsFixtures(val c: blackbox.Context)
    extends MacroCommonsScala2
    with DestructuredExprsFixturesImpl {

  def testParseGeneralImpl[A: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[Data] =
    testParseGeneral[A](expr)

  def testParseFieldPathImpl[A: c.WeakTypeTag, B: c.WeakTypeTag](lambda: c.Expr[A => B]): c.Expr[Data] =
    testParseFieldPath[A, B](lambda)

  def testParseLambdaImpl[A: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[Data] =
    testParseLambda[A](expr)

  def testCollectMethodCallsImpl[A: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[Data] =
    testCollectMethodCalls[A](expr)

  def testOutermostMethodCallImpl[A: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[Data] =
    testOutermostMethodCall[A](expr)

  def testParseDetailedImpl[A: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[Data] =
    testParseDetailed[A](expr)
}

object DestructuredExprsFixtures {

  def testParseGeneral[A](expr: A): Data = macro DestructuredExprsFixtures.testParseGeneralImpl[A]

  def testParseFieldPath[A, B](lambda: A => B): Data = macro DestructuredExprsFixtures.testParseFieldPathImpl[A, B]

  def testParseLambda[A](expr: A): Data = macro DestructuredExprsFixtures.testParseLambdaImpl[A]

  def testCollectMethodCalls[A](expr: A): Data = macro DestructuredExprsFixtures.testCollectMethodCallsImpl[A]

  def testOutermostMethodCall[A](expr: A): Data = macro DestructuredExprsFixtures.testOutermostMethodCallImpl[A]

  def testParseDetailed[A](expr: A): Data = macro DestructuredExprsFixtures.testParseDetailedImpl[A]
}
