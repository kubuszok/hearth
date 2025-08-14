package hearth
package typed

import hearth.data.Data

import scala.quoted.*

final private class MethodsFixtures(q: Quotes) extends MacroCommonsScala3(using q), MethodsFixturesImpl

object MethodsFixtures {

  // TODO: create macro annotation which would allow to do the following

  inline def testMethodsExtraction[A](inline excluding: String*): Data = ${
    testMethodsExtractionImpl[A]('{ excluding })
  }
  private def testMethodsExtractionImpl[A: Type](excluding: Expr[Seq[String]])(using q: Quotes): Expr[Data] =
    new MethodsFixtures(q).testMethodsExtractionS3Adapter[A](excluding)
}
