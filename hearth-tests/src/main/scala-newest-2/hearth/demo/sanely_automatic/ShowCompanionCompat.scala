// We've put things into a separate package and do not use:
//   package hearth
//   package demo
//   package sanely_automatic
// here, because we want to show all the imports normal users would have to do.
package hearth.demo.sanely_automatic

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

private[sanely_automatic] trait ShowCompanionCompat { this: Show.type =>

  implicit def derived[A]: Show[A] = macro ShowMacros.deriveTypeClassImpl[A]

  def show[A](value: A): String = macro ShowMacros.deriveShowStringImpl[A]
}

private[sanely_automatic] class ShowMacros(val c: blackbox.Context)
    extends hearth.MacroCommonsScala2
    with ShowMacrosImpl {

  // TODO: create macro annotation which would allow to do the following

  def deriveTypeClassImpl[A: c.WeakTypeTag]: c.Expr[Show[A]] = deriveTypeClass[A]

  def deriveShowStringImpl[A: c.WeakTypeTag](value: c.Expr[A]): c.Expr[String] = deriveShowString[A](value)
}
