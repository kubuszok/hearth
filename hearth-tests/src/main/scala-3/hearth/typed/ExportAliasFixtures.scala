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

  def testConstructWith[A: Type, Arg: Type](arg: Expr[Arg]): Expr[Data] =
    Type[A].primaryConstructor match {
      case None              => Expr(Data("no primary constructor"))
      case Some(constructor) =>
        constructor.fold(
          onInstance = _ => Environment.reportErrorAndAbort("a constructor needs no instance"),
          onTypes = _ => Map.empty,
          onValues = _.parameters.flatten.map { case (name, _) => name -> arg.as_?? }.toMap
        ) match {
          case Right(built) =>
            given Quotes = q
            val builtExpr = built.value.asInstanceOf[scala.quoted.Expr[Any]]
            '{ Data($builtExpr.toString) }
          case Left(error) => Expr(Data(s"LEFT: ${removeAnsiColors(error)}"))
        }
    }
}

object ExportAliasFixtures {

  inline def testSubtyping[A, B]: Data = ${ testSubtypingImpl[A, B] }
  private def testSubtypingImpl[A: Type, B: Type](using q: Quotes): Expr[Data] =
    new ExportAliasFixtures(q).testSubtyping[A, B]

  inline def testConstructWith[A, Arg](inline arg: Arg): Data = ${ testConstructWithImpl[A, Arg]('arg) }
  private def testConstructWithImpl[A: Type, Arg: Type](arg: Expr[Arg])(using q: Quotes): Expr[Data] =
    new ExportAliasFixtures(q).testConstructWith[A, Arg](arg)
}
