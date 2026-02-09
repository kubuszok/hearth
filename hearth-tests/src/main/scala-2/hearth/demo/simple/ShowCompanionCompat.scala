// We've put things into a separate package and do not use:
//   package hearth
//   package demo
//   package simple
// here, because we want to show all the imports normal users would have to do.
package hearth.demo.simple

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

private[demo] trait ShowCompanionCompat { this: Show.type =>

  implicit def derived[A]: Show.AutoDerived[A] = macro ShowMacros.deriveTypeClassImpl[A]

  def show[A](value: A): String = macro ShowMacros.deriveShowStringImpl[A]
}

private[demo] class ShowMacros(val c: blackbox.Context) extends hearth.MacroCommonsScala2 with ShowMacrosImpl {

  // TODO: create macro annotation which would allow to do the following

  def deriveTypeClassImpl[A: c.WeakTypeTag]: c.Expr[Show.AutoDerived[A]] = deriveTypeClass[A]

  def deriveShowStringImpl[A: c.WeakTypeTag](value: c.Expr[A]): c.Expr[String] = deriveShowString[A](value)
}
