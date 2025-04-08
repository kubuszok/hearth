package hearth
package untyped

trait MethodsScala2 extends Methods { this: MacroCommonsScala2 =>

  // import c.universe.*

  final override type UntypedParameter = c.Symbol

  object UntypedParameter extends UntypedParameterModule {

    override def fromTyped(param: Parameter): UntypedParameter = ???
    override def toTyped(instanceTpe: UntypedType)(untyped: UntypedParameter): Parameter = ???

    override def name(param: UntypedParameter): String = param.name.decodedName.toString

    override def annotations(param: UntypedParameter): List[UntypedExpr] = ???
  }

  final override type UntypedMethod = c.Symbol

  object UntypedMethod extends UntypedMethodModule {

    override def name(method: UntypedMethod): String = method.name.decodedName.toString
    override def position(method: UntypedMethod): Position = method.pos

    override def annotations(method: UntypedMethod): List[UntypedExpr] = ???

    override def isVal(method: UntypedMethod): Boolean = ???
    override def isVar(method: UntypedMethod): Boolean =
      method.isTerm && method.asTerm.name.toString.endsWith("_$eq")
    override def isLazy(method: UntypedMethod): Boolean = ???
    override def isDef(method: UntypedMethod): Boolean = ???
    override def isInherited(method: UntypedMethod): Boolean = ???
    override def isImplicit(method: UntypedMethod): Boolean = ???

    override def isPublic(method: UntypedMethod): Boolean = ???
    override def isAccessibleHere(method: UntypedMethod): Boolean = ???
  }
}
