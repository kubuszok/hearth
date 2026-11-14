package hearth
package typed

import hearth.data.Data

import scala.quoted.*

final private class ExportAliasFixtures(q: Quotes) extends MacroCommonsScala3(using q) {

  def testSubtyping[A: Type, B: Type]: Expr[Data] = Expr(
    Data.map(
      "A <:< B" -> Data(Type[A] <:< Type[B]),
      "B <:< A" -> Data(Type[B] <:< Type[A]),
      "A =:= B" -> Data(Type[A] =:= Type[B])
    )
  )
}

object ExportAliasFixtures {

  inline def testSubtyping[A, B]: Data = ${ testSubtypingImpl[A, B] }
  private def testSubtypingImpl[A: Type, B: Type](using q: Quotes): Expr[Data] =
    new ExportAliasFixtures(q).testSubtyping[A, B]
}
