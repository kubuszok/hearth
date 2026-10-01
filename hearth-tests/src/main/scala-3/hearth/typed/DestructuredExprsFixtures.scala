package hearth
package typed

import hearth.data.Data

import scala.quoted.*

final private class DestructuredExprsFixtures(q: Quotes)
    extends MacroCommonsScala3(using q),
      DestructuredExprsFixturesImpl

object DestructuredExprsFixtures {

  inline def testParseGeneral[A](inline expr: A): Data = ${ testParseGeneralImpl[A]('expr) }
  private def testParseGeneralImpl[A: Type](expr: Expr[A])(using q: Quotes): Expr[Data] =
    new DestructuredExprsFixtures(q).testParseGeneral[A](expr)

  inline def testParseFieldPath[A, B](inline lambda: A => B): Data = ${
    testParseFieldPathImpl[A, B]('lambda)
  }
  private def testParseFieldPathImpl[A: Type, B: Type](lambda: Expr[A => B])(using q: Quotes): Expr[Data] =
    new DestructuredExprsFixtures(q).testParseFieldPath[A, B](lambda)

  inline def testParseLambda[A](inline expr: A): Data = ${ testParseLambdaImpl[A]('expr) }
  private def testParseLambdaImpl[A: Type](expr: Expr[A])(using q: Quotes): Expr[Data] =
    new DestructuredExprsFixtures(q).testParseLambda[A](expr)

  inline def testCollectMethodCalls[A](inline expr: A): Data = ${ testCollectMethodCallsImpl[A]('expr) }
  private def testCollectMethodCallsImpl[A: Type](expr: Expr[A])(using q: Quotes): Expr[Data] =
    new DestructuredExprsFixtures(q).testCollectMethodCalls[A](expr)

  inline def testOutermostMethodCall[A](inline expr: A): Data = ${ testOutermostMethodCallImpl[A]('expr) }
  private def testOutermostMethodCallImpl[A: Type](expr: Expr[A])(using q: Quotes): Expr[Data] =
    new DestructuredExprsFixtures(q).testOutermostMethodCall[A](expr)

  inline def testParseDetailed[A](inline expr: A): Data = ${ testParseDetailedImpl[A]('expr) }
  private def testParseDetailedImpl[A: Type](expr: Expr[A])(using q: Quotes): Expr[Data] =
    new DestructuredExprsFixtures(q).testParseDetailed[A](expr)

  inline def testParseBindings[A](inline expr: A): Data = ${ testParseBindingsImpl[A]('expr) }
  private def testParseBindingsImpl[A: Type](expr: Expr[A])(using q: Quotes): Expr[Data] =
    new DestructuredExprsFixtures(q).testParseBindings[A](expr)

  inline def testFindReferences[A](inline expr: A): Data = ${ testFindReferencesImpl[A]('expr) }
  private def testFindReferencesImpl[A: Type](expr: Expr[A])(using q: Quotes): Expr[Data] =
    new DestructuredExprsFixtures(q).testFindReferences[A](expr)

  inline def testSkipContextualWrappers[A](inline expr: A): Data = ${ testSkipContextualWrappersImpl[A]('expr) }
  private def testSkipContextualWrappersImpl[A: Type](expr: Expr[A])(using q: Quotes): Expr[Data] =
    new DestructuredExprsFixtures(q).testSkipContextualWrappers[A](expr)

  /** The argument is passed to a context-function-typed parameter, so the compiler wraps it in a contextual lambda. */
  inline def testSkipContextualWrappersOfSelector(
      inline selector: hearth.examples.parsed_exprs.Ctx ?=> hearth.examples.parsed_exprs.Person => String
  ): Data = ${ testSkipContextualWrappersOfSelectorImpl('selector) }
  private def testSkipContextualWrappersOfSelectorImpl(
      selector: Expr[hearth.examples.parsed_exprs.Ctx ?=> hearth.examples.parsed_exprs.Person => String]
  )(using q: Quotes): Expr[Data] =
    new DestructuredExprsFixtures(q).testSkipContextualWrappers(selector)

  inline def testPlainPrints[A](inline expr: A): Data = ${ testPlainPrintsImpl[A]('expr) }
  private def testPlainPrintsImpl[A: Type](expr: Expr[A])(using q: Quotes): Expr[Data] =
    new DestructuredExprsFixtures(q).testPlainPrints[A](expr)

  inline def testReceiverChain[A](inline expr: A): Data = ${ testReceiverChainImpl[A]('expr) }
  private def testReceiverChainImpl[A: Type](expr: Expr[A])(using q: Quotes): Expr[Data] =
    new DestructuredExprsFixtures(q).testReceiverChain[A](expr)

  inline def testMarkerPath[A](inline expr: A): Data = ${ testMarkerPathImpl[A]('expr) }
  private def testMarkerPathImpl[A: Type](expr: Expr[A])(using q: Quotes): Expr[Data] =
    new DestructuredExprsFixtures(q).testMarkerPath[A](expr)
}
