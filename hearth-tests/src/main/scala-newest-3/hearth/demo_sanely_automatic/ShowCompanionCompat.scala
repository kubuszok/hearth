// We've put things into a separate package and do not use:
//   package hearth
//   package demo_sanely_automatic
// here, because we want to show all the imports normal users would have to do.
package hearth.demo_sanely_automatic

import scala.quoted.*

private[demo_sanely_automatic] trait ShowCompanionCompat { this: Show.type =>

  inline given derived[A]: Show[A] = ${ ShowMacros.deriveTypeClass[A] }

  inline def show[A](value: A): String = ${ ShowMacros.deriveShowString[A]('{ value }) }
}

private[demo_sanely_automatic] class ShowMacros(q: Quotes) extends hearth.MacroCommonsScala3(using q), ShowMacrosImpl

private[demo_sanely_automatic] object ShowMacros {

  def deriveTypeClass[A: Type](using q: Quotes): Expr[Show[A]] = new ShowMacros(q).deriveTypeClass[A]

  def deriveShowString[A: Type](value: Expr[A])(using q: Quotes): Expr[String] =
    new ShowMacros(q).deriveShowString[A](value)
}
