package hearth
package untyped

import scala.collection.immutable.ListMap

trait UntypedMethodsScala2 extends UntypedMethods { this: MacroCommonsScala2 =>

  import c.universe.*

  import UntypedMethod.platformSpecific.{positionOf, symbolName}

  final class UntypedParameter private (val method: UntypedMethod, val symbol: TermSymbol, val index: Int)
      extends UntypedParameterMethods {

    override def name: String = symbolName(symbol)
    override def position: Option[Position] = positionOf(symbol)

    override def annotations: List[UntypedExpr] =
      symbol.annotations.map { ann =>
        c.untypecheck(ann.tree)
      }

    override def isByName: Boolean = symbol.isByNameParam
    override def isImplicit: Boolean = symbol.isImplicit
    override def hasDefault: Boolean = symbol.asTerm.isParamWithDefault
  }

  object UntypedParameter extends UntypedParameterModule {

    def parse(method: UntypedMethod, symbol: Symbol, index: Int): Either[String, UntypedParameter] =
      if (symbol.isTerm) Right(new UntypedParameter(method, symbol.asTerm, index))
      else Left(s"Expected param Symbol, got $symbol")
  }

  object UntypedParameters extends UntypedParametersModule {

    override def toTyped[Instance: Type](untyped: UntypedParameters): Parameters = {
      val instanceTpe = UntypedType.fromTyped[Instance]
      lazy val method = untyped.head.head._2.method // If params are empty it would throw... unless we don't use it.

      lazy val typesByParamName = method.symbol.asMethod
        .typeSignatureIn(instanceTpe)
        .paramLists
        .flatten
        .map(param => symbolName(param) -> param.typeSignatureIn(instanceTpe))
        .toMap

      untyped.map { params =>
        params.map { case (paramName, untyped) =>
          val param =
            new Parameter(
              asUntyped = untyped,
              untypedInstanceType = instanceTpe,
              tpe = typesByParamName(paramName).as_??
            )
          paramName -> param
        }
      }
    }
  }

  final class UntypedMethod private (
      val symbol: MethodSymbol,
      val invocation: Invocation,
      val isInherited: Boolean
  ) extends UntypedMethodMethods {

    override def unsafeApply(
        instanceTpe: UntypedType
    )(instance: Option[UntypedExpr], arguments: UntypedArguments): UntypedExpr = {
      lazy val adaptedArguments = arguments.adaptToParams(instanceTpe, this)
      invocation match {
        case Invocation.Constructor =>
          // new A... or new A() or new A(b1, b2), ...
          q"new $instanceTpe(...$adaptedArguments)"
        case Invocation.OnInstance =>
          instance match {
            case None =>
              assertionFailed(s"Expected an instance for method $name that is called on an instance")
            case Some(instance) =>
              // instance.method, or instance.method(), or instance.method(b1, b2, ...)
              q"$instance.$symbol(...$adaptedArguments)"
          }
        case Invocation.OnModule(module) =>
          // module.method, or module.method(), or module.method(b1, b2, ...)
          q"$module.$symbol(...$adaptedArguments)"
      }
    }

    lazy val hasTypeParameters: Boolean = symbol.typeParams.nonEmpty

    lazy val parameters: UntypedParameters = {
      val paramss = symbol.paramLists
      val indices = paramss.flatten.zipWithIndex.toMap
      paramss
        .map(inner =>
          ListMap.from(inner.map { param =>
            symbolName(param.asTerm) -> UntypedParameter.parse(this, param.asTerm, indices(param)).toOption.get
          })
        )
    }

    lazy val name: String = symbolName(symbol)
    override def position: Option[Position] = positionOf(symbol)

    override def annotations: List[UntypedExpr] =
      symbol.annotations.map { ann =>
        c.untypecheck(ann.tree)
      }

    override def isVal: Boolean = symbol.isVal
    override def isVar: Boolean = symbol.isVar
    override def isLazy: Boolean = symbol.isLazy
    override def isDef: Boolean = !symbol.isVal && !symbol.isVar && !symbol.isLazy
    override def isImplicit: Boolean = symbol.isImplicit

    override def isAvailable(scope: Accessible): Boolean = scope match {
      case Everywhere => symbol.isPublic
      case AtCallSite => false // TODO
    }
  }

  object UntypedMethod extends UntypedMethodModule {

    object platformSpecific {

      def positionOf(symbol: Symbol): Option[Position] =
        Option(symbol.pos)
          .filter(_ != NoPosition)
          // Prevent crashed in case of https://github.com/scala/scala3/issues/21672
          .filter(pos => scala.util.Try(pos.start).isSuccess)

      def symbolName(symbol: Symbol): String = symbol.name.decodedName.toString
    }

    private def parse(isInherited: Boolean, module: Option[UntypedExpr])(
        symbol: Symbol
    ): Either[String, UntypedMethod] =
      if (symbol.isMethod)
        Right(
          new UntypedMethod(
            symbol = symbol.asMethod,
            invocation =
              if (symbol.isConstructor) Invocation.Constructor
              else module.map(Invocation.OnModule).getOrElse(Invocation.OnInstance),
            isInherited = isInherited
          )
        )
      else Left(s"Expected method Symbol, got $symbol")
    private def parseOption(isInherited: Boolean, module: Option[UntypedExpr])(symbol: Symbol): Option[UntypedMethod] =
      parse(isInherited, module)(symbol).toOption

    override def toTyped[Instance: Type](untyped: UntypedMethod): Method.Of[Instance] = {
      val Instance = UntypedType.fromTyped[Instance]
      untyped.invocation match {
        case Invocation.Constructor =>
          Existential[Method[Instance, *], Instance](
            // TODO: check if the method is not parametric if it's a module
            Method.NoInstance[Instance](untyped, Instance): Method[Instance, Instance]
          )
        case Invocation.OnModule(_) =>
          if (!untyped.hasTypeParameters) {
            val returnType = untyped.symbol.typeSignatureIn(Instance).resultType.as_??
            // TODO: check if the method is not having any other issues, e.g. path-dependent types (we cannot handle them like this)
            import returnType.Underlying as Returned
            Existential[Method[Instance, *], Returned](
              Method.NoInstance[Returned](untyped, Instance): Method[Instance, Returned]
            )
          } else {
            Existential[Method[Instance, *], Nothing](
              Method.Unsupported(untyped, Instance)("Method defines type parameters")
            )
          }
        case Invocation.OnInstance =>
          if (!untyped.hasTypeParameters) {
            val returnType = untyped.symbol.typeSignatureIn(Instance).resultType.as_??
            // TODO: check if the method is not having any other issues, e.g. path-dependent types (we cannot handle them like this)
            import returnType.Underlying as Returned
            Existential[Method[Instance, *], Returned](
              Method.OfInstance[Instance, Returned](untyped, Instance): Method[Instance, Returned]
            )
          } else {
            Existential[Method[Instance, *], Nothing](
              Method.Unsupported(untyped, Instance)("Method defines type parameters")
            )
          }
      }
    }

    override def primaryConstructor(instanceTpe: UntypedType): Option[UntypedMethod] =
      Option(instanceTpe.typeSymbol)
        .filter(_.isClass)
        .map(_.asClass.primaryConstructor)
        .filter(_.isConstructor)
        .flatMap(UntypedMethod.parseOption(isInherited = false, module = None))
    override def constructors(instanceTpe: UntypedType): List[UntypedMethod] =
      instanceTpe.decls
        .filter(_.isConstructor)
        .flatMap(UntypedMethod.parseOption(isInherited = false, module = None))
        .toList
    override def methods(instanceTpe: UntypedType): List[UntypedMethod] = {
      val declared = instanceTpe.decls.toSet
      // TODO: companion methods
      instanceTpe.members
        .filter(_.isMethod)
        .filterNot(_.isConstructor) // Constructors are handled by `primaryConstructor` and `constructors`
        .filterNot(
          _.name.decodedName.toString.contains("$default$")
        ) // Default parameters are methods, but we don't want them
        .flatMap(s => UntypedMethod.parseOption(isInherited = !declared(s), module = None)(s))
        .toList
    }

    override def enclosing: Option[UntypedMethod] = {
      @scala.annotation.tailrec
      def enclosingOf(symbol: Symbol): Option[UntypedMethod] =
        if (symbol == NoSymbol) None
        else if (symbol.isMethod) parseOption(isInherited = false, module = None)(symbol)
        else if (symbol.isClass) parseOption(isInherited = false, module = None)(symbol.asClass.primaryConstructor)
        else enclosingOf(symbol.owner)
      enclosingOf(c.internal.enclosingOwner)
    }
  }
}
