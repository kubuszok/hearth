package hearth
package typename

import scala.quoted.*

final private[typename] class TypeNameDerived(q: Quotes) extends MacroCommonsScala3(using q), TypeNameMacros

private[typename] object TypeNameDerived {

  def derived[A: Type](using q: Quotes): Expr[TypeName[A]] =
    new TypeNameDerived(q).deriveTypeName[A]
}

private[typename] trait TypeNameCompanion {

  inline given derived[A]: TypeName[A] = ${ TypeNameDerived.derived[A] }
}
