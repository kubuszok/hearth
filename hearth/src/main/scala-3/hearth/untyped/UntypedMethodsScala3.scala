package hearth
package untyped

import scala.collection.immutable.ListMap

trait UntypedMethodsScala3 extends UntypedMethods { this: MacroCommonsScala3 =>

  import quotes.*, quotes.reflect.*

  final class UntypedParameter private (val method: UntypedMethod, val symbol: Symbol, val index: Int)
      extends UntypedParameterMethods {

    override def name: String = symbol.name
    override def annotations: List[UntypedExpr] = symbol.annotations

    override def isByName: Boolean = symbol.typeRef.simplified match {
      case ByNameType(_) => true
      case _             => false
    }
    override def isImplicit: Boolean = symbol.flags.is(Flags.Private)
    override def hasDefault: Boolean = symbol.flags.is(Flags.HasDefault)
  }

  object UntypedParameter extends UntypedParameterModule {

    def parse(method: UntypedMethod, symbol: Symbol, index: Int): Either[String, UntypedParameter] =
      if symbol.flags.is(Flags.Param) then Right(new UntypedParameter(method, symbol, index))
      else Left(s"Expected param Symbol, got $symbol")
  }

  object UntypedParameters extends UntypedParametersModule {

    override def toTyped[Instance: Type](untyped: UntypedParameters): Parameters = {
      lazy val instanceTpe = UntypedType.fromTyped[Instance]
      lazy val method = untyped.head.head._2.method // If params are empty it would throw... unless we don't use it.

      // constructor methods still have to have their type parameters manually applied,
      // even if we know the exact type of their class
      lazy val appliedIfNecessary =
        if instanceTpe.typeArgs.isEmpty && method.symbol.isClassConstructor then instanceTpe.memberType(method.symbol)
        else instanceTpe.memberType(method.symbol).appliedTo(instanceTpe.typeArgs)
      lazy val typesByParamName = appliedIfNecessary match {
        // monomorphic
        case MethodType(names, types, _) => names.zip(types).toMap
        // polymorphic
        case PolyType(_, _, MethodType(names, types, AppliedType(_, typeRefs))) =>
          val typeArgumentByAlias = typeRefs.zip(instanceTpe.typeArgs).toMap
          val typeArgumentByName: Map[String, TypeRepr] =
            names
              .zip(types)
              .toMap
              .view
              .mapValues { tpe =>
                typeArgumentByAlias.getOrElse(tpe, tpe)
              }
              .toMap
          typeArgumentByName
        case AppliedType(MethodType(names, types, _), typeRefs) =>
          val typeArgumentByAlias = typeRefs.zip(instanceTpe.typeArgs).toMap
          val typeArgumentByName: Map[String, TypeRepr] =
            names
              .zip(types)
              .toMap
              .view
              .mapValues { tpe =>
                typeArgumentByAlias.getOrElse(tpe, tpe)
              }
              .toMap
          typeArgumentByName
        // unknown
        // $COVERAGE-OFF$should never happen unless we messed up
        case out =>
          throw new AssertionError(
            s"Constructor of ${Type.prettyPrint(UntypedType.toTyped[Any](instanceTpe))} has unrecognized/unsupported format of type: $out"
          )
        // $COVERAGE-ON$
      }

      untyped.map { params =>
        params.map { case (paramName, untyped) =>
          val param =
            Parameter(asUntyped = untyped, untypedInstanceType = instanceTpe, tpe = typesByParamName(paramName).as_??)
          paramName -> param
        }
      }
    }
  }

  final class UntypedMethod private (
      val symbol: Symbol,
      val invocation: Invocation,
      val isInherited: Boolean
  ) extends UntypedMethodMethods {

    override def unsafeApply(
        instanceTpe: UntypedType
    )(instance: Option[UntypedExpr], arguments: UntypedArguments): UntypedExpr = {
      type Instance
      given Instance: Type[Instance] = instanceTpe.asTyped[Instance]
      lazy val adaptedArguments = arguments.adaptToParams(instanceTpe, this)
      invocation match {
        case Invocation.Constructor =>
          // new A
          val select = New(TypeTree.of[Instance]).select(symbol)
          // new A[B1, B2, ...] vs new A
          val tree = if instanceTpe.typeArgs.nonEmpty then select.appliedToTypes(instanceTpe.typeArgs) else select
          // new A... or new A() or new A(b1, b2), ...
          tree.appliedToArgss(adaptedArguments)
        case Invocation.OnInstance =>
          instance match {
            case None =>
              assertionFailed(s"Expected an instance for method $name that is called on an instance")
            case Some(instance) =>
              // instance.method, or instance.method(), or instance.method(b1, b2, ...)
              instance.select(symbol).appliedToArgss(adaptedArguments)
          }
        case Invocation.OnModule(module) =>
          // module.method, or module.method(), or module.method(b1, b2, ...)
          module.select(symbol).appliedToArgss(adaptedArguments)
      }
    }

    lazy val parameters: UntypedParameters = {
      val paramss = symbol.paramSymss.filterNot(_.exists(_.isType))
      val indices = paramss.flatten.zipWithIndex.toMap
      paramss
        .map(inner =>
          ListMap.from(inner.map { param =>
            param.name -> UntypedParameter.parse(this, param, indices(param)).toOption.get
          })
        )
    }

    lazy val name: String = symbol.name
    override def position: Position =
      symbol.pos
        // Prevent crashed in case of https://github.com/scala/scala3/issues/21672
        .filter(pos => scala.util.Try(pos.start).isSuccess)
        // TODO?
        .getOrElse(Position.current)

    override def annotations: List[UntypedExpr] = symbol.annotations

    override def isVal: Boolean = symbol.isValDef && !symbol.flags.is(Flags.Mutable)
    override def isVar: Boolean = symbol.flags.is(Flags.Mutable)
    override def isLazy: Boolean = symbol.flags.is(Flags.Lazy)
    override def isDef: Boolean = symbol.isDefDef
    override def isImplicit: Boolean = symbol.flags.is(Flags.Implicit)

    override def isAvailable(scope: Accessible): Boolean = scope match {
      case Everywhere =>
        !symbol.flags.is(Flags.Private) && !symbol.flags.is(Flags.Protected) &&
        symbol.privateWithin.isEmpty && symbol.protectedWithin.isEmpty
      case AtCallSite => false // TODO
    }
  }

  object UntypedMethod extends UntypedMethodModule {

    def parse(isInherited: Boolean, module: Option[UntypedExpr])(symbol: Symbol): Either[String, UntypedMethod] =
      if symbol.isValDef || symbol.isDefDef then Right(
        new UntypedMethod(
          symbol = symbol,
          invocation =
            if symbol.isClassConstructor then Invocation.Constructor
            else module.map(Invocation.OnModule.apply).getOrElse(Invocation.OnInstance),
          isInherited = isInherited
        )
      )
      else Left(s"Expected method Symbol, got $symbol")
    def parseOption(isInherited: Boolean, module: Option[UntypedExpr])(symbol: Symbol): Option[UntypedMethod] =
      parse(isInherited, module)(symbol).toOption

    override def toTyped[Instance: Type](untyped: UntypedMethod): Existential[Method[Instance, *]] = {
      val Instance = UntypedType.fromTyped[Instance]
      lazy val (typeParams, valueParams) = untyped.symbol.paramSymss.partition(_.exists(_.isType))
      untyped.invocation match {
        case Invocation.Constructor | Invocation.OnModule(_) =>
          Existential[Method[Instance, *], Instance](
            Method.NoInstance[Instance](untyped, Instance): Method[Instance, Instance]
          )
        case Invocation.OnInstance =>
          if typeParams.isEmpty then {
            val returnType = Instance.memberType(untyped.symbol).widenByName match {
              case lambda: LambdaType => lambda.resType.as_??
              case out                => out.as_??
            }
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
      Option(instanceTpe.typeSymbol.primaryConstructor)
        .filterNot(_.isNoSymbol)
        .flatMap(UntypedMethod.parseOption(isInherited = false, module = None))
    override def constructors(instanceTpe: UntypedType): List[UntypedMethod] =
      instanceTpe.typeSymbol.declarations
        .filterNot(_.isNoSymbol)
        .filter(_.isClassConstructor)
        .flatMap(UntypedMethod.parseOption(isInherited = false, module = None))
    override def methods(instanceTpe: UntypedType): List[UntypedMethod] = {
      val declared = instanceTpe.typeSymbol.declaredMethods.toSet
      // TODO: companion methods
      instanceTpe.typeSymbol.methodMembers
        .filterNot(_.isNoSymbol)
        .filterNot(_.isClassConstructor)
        .flatMap(s => UntypedMethod.parseOption(isInherited = !declared(s), module = None)(s))
    }
  }
}
