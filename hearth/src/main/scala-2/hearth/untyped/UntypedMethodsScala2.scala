package hearth
package untyped

import scala.collection.immutable.ListMap

trait UntypedMethodsScala2 extends UntypedMethods { this: MacroCommonsScala2 =>

  import c.universe.*

  final class UntypedParameter private (val method: UntypedMethod, val symbol: TermSymbol, val index: Int)

  object UntypedParameter extends UntypedParameterModule {

    object platformSpecific {

      def paramName(param: Symbol): String = param.name.decodedName.toString
    }

    def parse(method: UntypedMethod, symbol: Symbol, index: Int): Either[String, UntypedParameter] =
      if (symbol.isTerm) Right(new UntypedParameter(method, symbol.asTerm, index))
      else Left(s"Expected param Symbol, got $symbol")

    override def name(param: UntypedParameter): String = platformSpecific.paramName(param.symbol)

    override def annotations(param: UntypedParameter): List[UntypedExpr] =
      param.symbol.annotations.map { ann =>
        c.untypecheck(ann.tree)
      }

    override def isByName(param: UntypedParameter): Boolean = param.symbol.isByNameParam
    override def isImplicit(param: UntypedParameter): Boolean = param.symbol.isImplicit
    override def hasDefault(param: UntypedParameter): Boolean = param.symbol.asTerm.isParamWithDefault
  }

  object UntypedParameters extends UntypedParametersModule {

    override def toTyped[Instance: Type](untyped: UntypedParameters): Parameters = {
      val instanceTpe = UntypedType.fromTyped[Instance]
      lazy val method = untyped.head.head._2.method // If params are empty it would throw... unless we don't use it.

      lazy val typesByParamName = method.symbol.asMethod
        .typeSignatureIn(instanceTpe)
        .paramLists
        .flatten
        .map(param => UntypedParameter.platformSpecific.paramName(param) -> param.typeSignatureIn(instanceTpe))
        .toMap

      untyped.map { params =>
        params.map { case (paramName, untyped) =>
          val param =
            new Parameter(asUntyped = untyped, instanceTpe = instanceTpe, parameterTpe = typesByParamName(paramName))
          paramName -> param
        }
      }
    }
  }

  final class UntypedMethod private (val symbol: MethodSymbol, private val isInherited: Boolean) {

    lazy val parameters: UntypedParameters = {
      val paramss = symbol.paramLists
      val indices = paramss.flatten.zipWithIndex.toMap
      paramss
        .map(inner =>
          ListMap.from(inner.map { param =>
            UntypedParameter.platformSpecific
              .paramName(param.asTerm) -> UntypedParameter.parse(this, param.asTerm, indices(param)).toOption.get
          })
        )
    }
  }

  object UntypedMethod extends UntypedMethodModule {

    def parse(symbol: Symbol, isInherited: Boolean): Either[String, UntypedMethod] =
      if (symbol.isMethod) Right(new UntypedMethod(symbol.asMethod, isInherited))
      else Left(s"Expected method Symbol, got $symbol")

    override def toTyped[Instance: Type](untyped: UntypedMethod): Existential[Method[Instance, *]] = {
      val Instance = UntypedType.fromTyped[Instance]
      val sym = untyped.symbol
      if (sym.isConstructor) { // TODO: or is module
        Existential[Method[Instance, *], Instance](
          Method.NoInstance[Instance](untyped, Instance, isConstructor = true): Method[Instance, Instance]
        )
      } else if (untyped.symbol.typeParams.isEmpty) {
        val returnType = untyped.symbol.typeSignatureIn(Instance).resultType.as_??
        import returnType.Underlying as Returned
        // TODO: check if the method is not having any other issues, e.g. path-dependent types (we cannot handle them like this)
        Existential[Method[Instance, *], Returned](
          Method.OfInstance[Instance, Returned](untyped, Instance): Method[Instance, Returned]
        )
      } else {
        Existential[Method[Instance, *], Nothing](
          Method.Unsupported(untyped, Instance)("Method defines type parameters")
        )
      }
    }

    override def unsafeApply(
        instanceTpe: UntypedType,
        method: UntypedMethod
    )(instance: Option[UntypedExpr], arguments: UntypedArguments, isConstructor: Boolean): UntypedExpr = {
      lazy val adaptedArguments = arguments.adaptToParams(instanceTpe, method)
      instance match {
        case None if isConstructor =>
          // new A... or new A() or new A(b1, b2), ...
          q"new $instanceTpe(...$adaptedArguments)"
        case None =>
          def valueByType(@scala.annotation.unused tpe: UntypedType): UntypedExpr = ??? // TODO: call on object
          q"${valueByType(instanceTpe)}.${method.symbol}(...$adaptedArguments)"
        case Some(instance) =>
          // instance.method, or instance.method(), or instance.method(b1, b2, ...)
          q"$instance.${method.symbol}(...$adaptedArguments)"
      }
    }

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

    override def parameters(method: UntypedMethod): UntypedParameters = method.parameters

    override def name(method: UntypedMethod): String = method.symbol.name.decodedName.toString
    override def position(method: UntypedMethod): Position = method.symbol.pos

    override def annotations(method: UntypedMethod): List[UntypedExpr] =
      method.symbol.annotations.map { ann =>
        c.untypecheck(ann.tree)
      }

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
