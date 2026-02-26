package hearth
package typename

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

final private[typename] class TypeNameDerived(val c: blackbox.Context) extends MacroCommonsScala2 with TypeNameMacros {

  def derivedImpl[A: c.WeakTypeTag]: c.Expr[TypeName[A]] = deriveTypeName[A]
}

private[typename] trait TypeNameCompanion {

  implicit def derived[A]: TypeName[A] = macro TypeNameDerived.derivedImpl[A]
}
