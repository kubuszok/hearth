package hearth
package typed

import hearth.data.Data

import scala.deriving.Mirror
import scala.quoted.*

final private class EnumCaseChildrenFixtures(q: Quotes) extends MacroCommonsScala3(using q) {

  def testEnumCaseChildren[A: Type]: Expr[Data] =
    Expr(Data.map(Type[A].directChildren.toList.flatMap(_.toList).map { case (name, child) =>
      import child.Underlying as C
      name -> describe[C]
    }*))

  def testEnumCase[A: Type]: Expr[Data] = Expr(describe[A])

  // The implicits the compiler resolves for a type are what derivations built on `directChildren` rely on.
  private def describe[C: Type]: Data = {
    given Quotes = q
    import q.reflect.*
    val mirror = scala.quoted.Expr.summon[Mirror.Of[C]].map(_.asTerm.tpe) match {
      case Some(m) if m <:< TypeRepr.of[Mirror.Singleton] => "Mirror.Singleton"
      case Some(m) if m <:< TypeRepr.of[Mirror.Sum]       => "Mirror.Sum"
      case Some(_)                                        => "other Mirror"
      case None                                           => "no Mirror.Of"
    }
    Data.map(
      "Mirror.Of" -> Data(mirror),
      "ValueOf found" -> Data(scala.quoted.Expr.summon[ValueOf[C]].isDefined),
      "isSealed" -> Data(Type[C].isSealed),
      "isCaseClass" -> Data(Type[C].isCaseClass),
      "isCaseVal" -> Data(Type[C].isCaseVal)
    )
  }
}

object EnumCaseChildrenFixtures {

  inline def testEnumCaseChildren[A]: Data = ${ testEnumCaseChildrenImpl[A] }
  private def testEnumCaseChildrenImpl[A: Type](using q: Quotes): Expr[Data] =
    new EnumCaseChildrenFixtures(q).testEnumCaseChildren[A]

  inline def testEnumCase[A]: Data = ${ testEnumCaseImpl[A] }
  private def testEnumCaseImpl[A: Type](using q: Quotes): Expr[Data] =
    new EnumCaseChildrenFixtures(q).testEnumCase[A]
}
