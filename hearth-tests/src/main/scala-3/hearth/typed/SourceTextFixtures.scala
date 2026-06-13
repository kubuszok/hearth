package hearth
package typed

import hearth.data.Data

import scala.quoted.*

final private class SourceTextFixtures(q: Quotes)
    extends MacroCommonsScala3(using q),
      SourceTextFixturesImpl {

  import quotes.reflect.*

  // On Scala 3 every term built during macro expansion inherits the macro-expansion position, so there is no
  // positionless term. We construct a Position with a deliberately out-of-range span (start > end) so that
  // Position.sourceCode's guard returns None.
  protected def noSourcePositionSourceCode: Option[String] = {
    val sourceFile = quotes.reflect.Position.ofMacroExpansion.sourceFile
    val invalid = quotes.reflect.Position(sourceFile, sourceFile.content.fold(0)(_.length) + 10, 0)
    this.Position.sourceCode(invalid)
  }
}

object SourceTextFixtures {

  inline def testExprSourceText[A](inline expr: A): Data = ${ testExprSourceTextImpl[A]('expr) }
  private def testExprSourceTextImpl[A: Type](expr: Expr[A])(using q: Quotes): Expr[Data] =
    new SourceTextFixtures(q).testExprSourceText[A](expr)

  inline def testSyntheticSourceText: Data = ${ testSyntheticSourceTextImpl }
  private def testSyntheticSourceTextImpl(using q: Quotes): Expr[Data] =
    new SourceTextFixtures(q).testSyntheticSourceText

  inline def testDestructuredSourceText[A](inline expr: A): Data = ${ testDestructuredSourceTextImpl[A]('expr) }
  private def testDestructuredSourceTextImpl[A: Type](expr: Expr[A])(using q: Quotes): Expr[Data] =
    new SourceTextFixtures(q).testDestructuredSourceText[A](expr)
}
