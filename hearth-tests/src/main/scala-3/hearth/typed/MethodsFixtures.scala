package hearth
package typed

import hearth.testdata.{Data, DataSupports}

import scala.quoted.*

final class MethodsFixtures(q: Quotes) extends MacroCommonsScala3(using q), DataSupports, MethodsFixturesImpl

object MethodsFixtures {

  // TODO: create macro annotation which would allow to do the following

  inline def testMethodsExtraction[A](inline excluding: String*): Data = ${
    testMethodsExtractionImpl[A]('{ excluding })
  }
  def testMethodsExtractionImpl[A: Type](excluding: Expr[Seq[String]])(using q: Quotes): Expr[Data] =
    new MethodsFixtures(q).testMethodsExtractionS3Adapter[A](excluding)
}
