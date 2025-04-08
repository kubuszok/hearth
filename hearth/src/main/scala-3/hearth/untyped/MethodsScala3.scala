package hearth
package untyped

trait MethodsScala3 extends Methods { this: MacroCommonsScala3 =>

  import quotes.*, quotes.reflect.*

  final override type UntypedParameter = Symbol

  object UntypedParameter extends UntypedParameterModule {

    override def fromTyped(param: Parameter): UntypedParameter = ???
    override def toTyped(instanceTpe: UntypedType)(untyped: UntypedParameter): Parameter = ???

    override def name(param: UntypedParameter): String = param.name

    override def annotations(param: UntypedParameter): List[UntypedExpr] = ???
  }

  final override type UntypedMethod = Symbol

  object UntypedMethod extends UntypedMethodModule {

    override def name(method: UntypedMethod): String = method.name
    override def position(method: UntypedMethod): Position =
      method.pos
        // Prevent crashed in case of https://github.com/scala/scala3/issues/21672
        .filter(pos => scala.util.Try(pos.start).isSuccess)
        // TODO?
        .getOrElse(Position.current)

    override def annotations(method: UntypedMethod): List[UntypedExpr] = ???

    override def isVal(method: UntypedMethod): Boolean = ???
    override def isVar(method: UntypedMethod): Boolean =
      (method.isValDef || method.isDefDef) && method.flags.is(Flags.Mutable)
    override def isLazy(method: UntypedMethod): Boolean = ???
    override def isDef(method: UntypedMethod): Boolean = ???
    override def isInherited(method: UntypedMethod): Boolean = ???
    override def isImplicit(method: UntypedMethod): Boolean = ???

    override def isPublic(method: UntypedMethod): Boolean = ???
    override def isAccessibleHere(method: UntypedMethod): Boolean = ???
  }
}
