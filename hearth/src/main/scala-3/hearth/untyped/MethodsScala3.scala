package hearth
package untyped

import scala.collection.immutable.ListMap

trait MethodsScala3 extends Methods { this: MacroCommonsScala3 =>

  import quotes.*, quotes.reflect.*

  final class UntypedParameter private (val method: UntypedMethod, private val symbol: Symbol)

  object UntypedParameter extends UntypedParameterModule {

    def parse(method: UntypedMethod, symbol: Symbol): Either[String, UntypedParameter] =
      if symbol.flags.is(Flags.Param) then Right(new UntypedParameter(method, symbol))
      else Left(s"Expected param Symbol, got $symbol")

    override def toTyped(instanceTpe: UntypedType)(untyped: UntypedParameter): Parameter = ???

    override def name(param: UntypedParameter): String = param.symbol.name
    override def annotations(param: UntypedParameter): List[UntypedExpr] = param.symbol.annotations

    override def isByName(param: UntypedParameter): Boolean =
      param.symbol.typeRef.simplified match {
        case ByNameType(_) => true
        case _             => false
      }
    override def isImplicit(param: UntypedParameter): Boolean = param.symbol.flags.is(Flags.Private)
  }

  final class UntypedMethod private (val symbol: Symbol, private val isInherited: Boolean)

  object UntypedMethod extends UntypedMethodModule {

    def parse(symbol: Symbol, isInherited: Boolean): Either[String, UntypedMethod] =
      if symbol.isValDef || symbol.isDefDef then Right(new UntypedMethod(symbol, isInherited))
      else Left(s"Expected method Symbol, got $symbol")

    override def toTyped[Instance: Type](untyped: UntypedMethod): Existential[Method[Instance, *]] = {
      val Instance = UntypedType.fromTyped[Instance]
      val sym = untyped.symbol
      lazy val (typeParams, valueParams) = untyped.symbol.paramSymss.partition(_.exists(_.isType))
      if sym.isClassConstructor then {
        Existential[Method[Instance, *], Instance](
          Method.NoInstance[Instance](untyped, Instance): Method[Instance, Instance]
        )
      } else if typeParams.isEmpty then {
        val returnType = Instance.memberType(sym).widenByName match {
          case lambda: LambdaType => lambda.resType.as_??
          case out                => out.as_??
        }
        import returnType.Underlying as Returned
        // TODO: check if the method is not having any other issues, e.g. path-dependent types (we cannot handle them like this)
        Existential[Method[Instance, *], Returned](
          Method.OfInstance[Instance, Returned](untyped, Instance): Method[Instance, Returned]
        )
      } else {
        Existential[Method[Instance, *], Nothing](
          Method.Unsupported(untyped, Instance, "Method defines type parameters")
        )
      }
    }

    override def primaryConstructor(instanceTpe: UntypedType): Option[UntypedMethod] =
      Option(instanceTpe.typeSymbol.primaryConstructor)
        .filterNot(_.isNoSymbol)
        .flatMap(UntypedMethod.parse(_, isInherited = false).toOption)
    override def constructors(instanceTpe: UntypedType): List[UntypedMethod] =
      instanceTpe.typeSymbol.declarations
        .filterNot(_.isNoSymbol)
        .filter(_.isClassConstructor)
        .flatMap(UntypedMethod.parse(_, isInherited = false).toOption)
    override def methods(instanceTpe: UntypedType): List[UntypedMethod] = {
      val declared = instanceTpe.typeSymbol.declaredMethods.toSet
      instanceTpe.typeSymbol.methodMembers
        .filterNot(_.isNoSymbol)
        .filterNot(_.isClassConstructor)
        .flatMap(s => UntypedMethod.parse(s, isInherited = !declared(s)).toOption)
    }

    override def parametersAt(method: UntypedMethod)(instanceTpe: UntypedType): UntypedParameters = {
      // constructor methods still have to have their type parameters manually applied,
      // even if we know the exact type of their class
      val appliedIfNecessary =
        if instanceTpe.typeArgs.isEmpty && method.symbol.isClassConstructor then instanceTpe.memberType(method.symbol)
        else instanceTpe.memberType(method.symbol).appliedTo(instanceTpe.typeArgs)
      val typesByParamName = appliedIfNecessary match {
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
      method.symbol.paramSymss
        .filterNot(_.exists(_.isType))
        .map(inner =>
          ListMap.from(inner.map { param =>
            val _ = typesByParamName // TODO: use it actually
            param.name -> UntypedParameter.parse(method, param).toOption.get // TODO
          })
        )
    }

    override def name(method: UntypedMethod): String = method.symbol.name
    override def position(method: UntypedMethod): Position =
      method.symbol.pos
        // Prevent crashed in case of https://github.com/scala/scala3/issues/21672
        .filter(pos => scala.util.Try(pos.start).isSuccess)
        // TODO?
        .getOrElse(Position.current)

    override def annotations(method: UntypedMethod): List[UntypedExpr] =
      method.symbol.annotations

    override def isVal(method: UntypedMethod): Boolean =
      method.symbol.isValDef && !method.symbol.flags.is(Flags.Mutable)
    override def isVar(method: UntypedMethod): Boolean = method.symbol.flags.is(Flags.Mutable)
    override def isLazy(method: UntypedMethod): Boolean = method.symbol.flags.is(Flags.Lazy)
    override def isDef(method: UntypedMethod): Boolean = method.symbol.isDefDef
    override def isInherited(method: UntypedMethod): Boolean = method.isInherited
    override def isImplicit(method: UntypedMethod): Boolean = method.symbol.flags.is(Flags.Implicit)

    override def isAvailable(method: UntypedMethod, scope: Accessible): Boolean = scope match {
      case Everywhere =>
        !method.symbol.flags.is(Flags.Private) && !method.symbol.flags.is(Flags.Protected) &&
        method.symbol.privateWithin.isEmpty && method.symbol.protectedWithin.isEmpty
      case AtCallSite => false // TODO
    }
  }
}
