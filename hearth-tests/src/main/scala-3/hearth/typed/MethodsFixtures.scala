package hearth
package typed

import hearth.testdata.{Data, DataSupports}

import scala.quoted.*

final class MethodsFixtures(q: Quotes) extends MacroCommonsScala3(using q), DataSupports, MethodsFixturesImpl

object MethodsFixtures {

  // TODO: create macro annotation which would allow to do the following

  inline def testMethodsExtraction[A]: Data = ${ testMethodsExtractionImpl[A] }
  def testMethodsExtractionImpl[A: Type](using q: Quotes): Expr[Data] = new MethodsFixtures(q).testMethodsExtraction[A]
}
