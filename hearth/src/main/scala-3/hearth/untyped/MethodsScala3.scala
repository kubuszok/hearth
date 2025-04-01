package hearth
package untyped

trait MethodsScala3 extends Methods { this: MacroCommonsScala3 =>

  import quotes.*, quotes.reflect.*

  final override type UntypedParameter = Symbol

  object UntypedParameter extends UntypedParameterModule {

    override def fromTyped(param: Parameter): UntypedParameter = ???
    override def toTyped(instanceTpe: UntypedType)(untyped: UntypedParameter): Parameter = ???

    override def name(param: UntypedParameter): String = ???

    override def annotations(param: UntypedParameter): List[UntypedExpr] = ???
  }

  final override type UntypedMethod = Symbol

  object UntypedMethod extends UntypedMethodModule {

    override def name(method: UntypedMethod): String = ???

    override def annotations(method: UntypedMethod): List[UntypedExpr] = ???

    override def isVal(method: UntypedMethod): Boolean = ???
    override def isVar(method: UntypedMethod): Boolean = ???
    override def isLazy(method: UntypedMethod): Boolean = ???
    override def isDef(method: UntypedMethod): Boolean = ???
    override def isInherited(method: UntypedMethod): Boolean = ???
    override def isImplicit(method: UntypedMethod): Boolean = ???

    override def isPublic(method: UntypedMethod): Boolean = ???
    override def isAccessibleHere(method: UntypedMethod): Boolean = ???
  }

  implicit override val UntypedMethodOrdering: Ordering[UntypedMethod] = ???
}
