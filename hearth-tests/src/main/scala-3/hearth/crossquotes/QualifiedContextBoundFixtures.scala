package hearth
package crossquotes

import scala.quoted.Quotes

/** A macro entry point whose context bound is written with a qualified prefix (`scala.quoted.Type`). */
object QualifiedContextBoundFixtures {

  inline def typeName[A]: String = ${ typeNameImpl[A] }

  def typeNameImpl[A: scala.quoted.Type](using q: Quotes): scala.quoted.Expr[String] =
    new QualifiedContextBoundFixtures(q).typeName[A]
}

final private class QualifiedContextBoundFixtures(q: Quotes) extends MacroCommonsScala3(using q) {

  def typeName[A: Type]: Expr[String] = Expr(Type.plainPrint[A])
}
