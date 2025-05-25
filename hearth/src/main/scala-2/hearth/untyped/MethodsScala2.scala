package hearth
package untyped

import scala.collection.immutable.ListMap

trait MethodsScala2 extends Methods { this: MacroCommonsScala2 =>

  import c.universe.*

  final class UntypedParameter private (val method: UntypedMethod, private val symbol: TermSymbol)

  object UntypedParameter extends UntypedParameterModule {

    def parse(method: UntypedMethod, symbol: Symbol): Either[String, UntypedParameter] =
      if (symbol.isTerm) Right(new UntypedParameter(method, symbol.asTerm))
      else Left(s"Expected param Symbol, got $symbol")

    override def toTyped(instanceTpe: UntypedType)(untyped: UntypedParameter): Parameter = ???

    override def name(param: UntypedParameter): String = param.symbol.name.decodedName.toString

    override def annotations(param: UntypedParameter): List[UntypedExpr] = ???

    override def isByName(param: UntypedParameter): Boolean = param.symbol.isByNameParam
    override def isImplicit(param: UntypedParameter): Boolean = param.symbol.isImplicit
  }

  final class UntypedMethod private (val symbol: MethodSymbol, private val isInherited: Boolean)

  object UntypedMethod extends UntypedMethodModule {

    def parse(symbol: Symbol, isInherited: Boolean): Either[String, UntypedMethod] =
      if (symbol.isMethod) Right(new UntypedMethod(symbol.asMethod, isInherited))
      else Left(s"Expected method Symbol, got $symbol")

    override def primaryConstructor(instanceTpe: UntypedType): Option[UntypedMethod] =
      Option(instanceTpe.typeSymbol)
        .filter(_.isClass)
        .map(_.asClass.primaryConstructor)
        .filter(_.isConstructor)
        .flatMap(UntypedMethod.parse(_, isInherited = false).toOption)
    override def constructors(instanceTpe: UntypedType): List[UntypedMethod] =
      instanceTpe.decls.filter(_.isConstructor).flatMap(UntypedMethod.parse(_, isInherited = false).toOption).toList
    override def methods(instanceTpe: UntypedType): List[UntypedMethod] = {
      val declared = instanceTpe.decls.toSet
      instanceTpe.members
        .filter(_.isMethod)
        .flatMap(s => UntypedMethod.parse(s, isInherited = !declared(s)).toOption)
        .toList
    }

    override def parametersAt(method: UntypedMethod)(instanceTpe: UntypedType): UntypedParameters =
      method.symbol
        .typeSignatureIn(instanceTpe)
        .paramLists
        .map(inner =>
          ListMap.from(inner.map { param =>
            param.name.decodedName.toString -> UntypedParameter.parse(method, param.asTerm).toOption.get
          })
        )

    override def name(method: UntypedMethod): String = method.symbol.name.decodedName.toString
    override def position(method: UntypedMethod): Position = method.symbol.pos

    override def annotations(method: UntypedMethod): List[UntypedExpr] = ???

    override def isVal(method: UntypedMethod): Boolean = method.symbol.isVal
    override def isVar(method: UntypedMethod): Boolean = method.symbol.isVar
    override def isLazy(method: UntypedMethod): Boolean = method.symbol.isLazy
    override def isDef(method: UntypedMethod): Boolean =
      !method.symbol.isVal && !method.symbol.isVar && !method.symbol.isLazy
    override def isInherited(method: UntypedMethod): Boolean = method.isInherited
    override def isImplicit(method: UntypedMethod): Boolean = method.symbol.isImplicit

    override def isAvailable(method: UntypedMethod, scope: Accessible): Boolean = scope match {
      case Everywhere => method.symbol.isPublic
      case AtCallSite => false // TODO
    }
  }
}
