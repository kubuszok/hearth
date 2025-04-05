package hearth
package typed

import scala.quoted.*

final class TypesFixtures(q: Quotes) extends MacroCommonsScala3(using q) with TypesFixturesImpl

object TypesFixtures {

  // TODO: create macro annotation which would allow to do the following

  inline def testNamesPrinters[A]: String = ${ testNamesPrintersImpl[A] }
  def testNamesPrintersImpl[A: Type](using q: Quotes): Expr[String] = new TypesFixtures(q).testNamesPrinters[A]
}
