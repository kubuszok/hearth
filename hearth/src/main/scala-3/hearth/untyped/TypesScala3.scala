package hearth
package untyped

import scala.collection.immutable.ListMap

trait TypesScala3 extends Types { this: MacroCommonsScala3 =>

  import quotes.*, quotes.reflect.*

  final override type UntypedType = TypeRepr

  object UntypedType extends UntypedTypeModule {

    override def fromTyped[A: Type]: UntypedType = TypeRepr.of[A]
    override def toTyped[A](untyped: UntypedType): Type[A] = untyped.asType.asInstanceOf[Type[A]]

    override def isPrimitive(instanceTpe: UntypedType): Boolean =
      Type.primitiveTypes.exists(tpe => instanceTpe <:< fromTyped(using tpe.Underlying))
    override def isBuildIn(instanceTpe: UntypedType): Boolean =
      Type.buildInTypes.exists(tpe => instanceTpe <:< fromTyped(using tpe.Underlying))

    override def isAbstract(instanceTpe: UntypedType): Boolean = {
      val A = instanceTpe.typeSymbol
      // We use =:= to check whether A is known to be exactly of the build-in type or is it some upper bound.
      !A.isNoSymbol && (A.flags.is(Flags.Abstract) || A.flags.is(Flags.Trait)) &&
      !Type.buildInTypes.exists(tpe => instanceTpe =:= fromTyped(using tpe.Underlying))
    }
    override def isFinal(instanceTpe: UntypedType): Boolean = {
      val A = instanceTpe.typeSymbol
      !A.isNoSymbol && A.flags.is(Flags.Final)
    }

    override def isClass(instanceTpe: UntypedType): Boolean = {
      val A = instanceTpe.typeSymbol
      !A.isNoSymbol && A.isClassDef
    }

    override def isSealed(instanceTpe: UntypedType): Boolean = {
      val A = instanceTpe.typeSymbol
      !A.isNoSymbol && A.flags.is(Flags.Sealed)
    }
    override def isJavaEnum(instanceTpe: UntypedType): Boolean =
      instanceTpe <:< fromTyped[java.lang.Enum[?]] && isAbstract(instanceTpe)
    override def isJavaEnumValue(instanceTpe: UntypedType): Boolean =
      instanceTpe <:< fromTyped[java.lang.Enum[?]] && !isAbstract(instanceTpe)
    override def isCase(instanceTpe: UntypedType): Boolean = {
      val A = instanceTpe.typeSymbol
      !A.isNoSymbol && A.flags.is(Flags.Case)
    }
    override def isObject(instanceTpe: UntypedType): Boolean = {
      val A = instanceTpe.typeSymbol
      !A.isNoSymbol && A.flags.is(Flags.Module)
    }
    override def isVal(instanceTpe: UntypedType): Boolean = {
      val A = instanceTpe.typeSymbol
      def attempt(sym: Symbol): Boolean =
        A.flags.is(Flags.Enum) && (A.flags.is(Flags.JavaStatic) || A.flags.is(Flags.StableRealizable))
      !A.isNoSymbol && (attempt(A) || attempt(instanceTpe.termSymbol))
    }

    override def isPublic(instanceTpe: UntypedType): Boolean = {
      val A = instanceTpe.typeSymbol
      !(A.flags.is(Flags.Private) || A.flags.is(Flags.PrivateLocal) || A.flags.is(Flags.Protected) ||
        A.privateWithin.isDefined || A.protectedWithin.isDefined)
    }
    override def isAvailableHere(instanceTpe: UntypedType): Boolean =
      try {
        // Try to access the type in the current context.
        // If it's not accessible, this will throw an exception.
        // TODO: test this assumption
        val A0 = instanceTpe.as_??
        import A0.Underlying as A
        val _ = '{ null.asInstanceOf[A] }
        true
      } catch {
        case _: Throwable => false
      }

    override def primaryConstructor(instanceTpe: UntypedType): Option[UntypedMethod] =
      Option(instanceTpe.typeSymbol.primaryConstructor).filterNot(_.isNoSymbol)
    override def constructors(instanceTpe: UntypedType): List[UntypedMethod] =
      instanceTpe.typeSymbol.declarations.filterNot(_.isNoSymbol).filter(_.isClassConstructor)

    override def directChildren(instanceTpe: UntypedType): Option[List[UntypedType]] =
      ??? // TODO: port enum types

    override def parameterAt(instanceTpe: UntypedType)(param: UntypedParameter): UntypedType =
      ??? // TODO: add this
    override def parametersAt(instanceTpe: UntypedType)(method: UntypedMethod): UntypedParameters = {
      // constructor methods still have to have their type parameters manually applied,
      // even if we know the exact type of their class
      val appliedIfNecessary =
        if instanceTpe.typeArgs.isEmpty && method.isClassConstructor then instanceTpe.memberType(method)
        else instanceTpe.memberType(method).appliedTo(instanceTpe.typeArgs)
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
            s"Constructor of ${Type.prettyPrint(toTyped[Any](instanceTpe))} has unrecognized/unsupported format of type: $out"
          )
        // $COVERAGE-ON$
      }
      method.paramSymss
        .filterNot(_.exists(_.isType))
        .map(inner =>
          ListMap.from(inner.map { param =>
            val _ = typesByParamName // TODO: use it actually
            param.name -> null
              .asInstanceOf[UntypedParameter] // TODO: typesByParamName(param.name) - define UntypedParameter
          })
        )
    }
    override def unsafeApplyAt(instanceTpe: UntypedType)(method: UntypedMethod): UntypedArguments => UntypedExpr =
      ??? // TODO: port ProductType constructor
    override def returnTypeAt(instanceTpe: UntypedType)(method: UntypedMethod): UntypedType =
      instanceTpe.memberType(method).widenByName match {
        case lambda: LambdaType => lambda.resType
        case out                => out
      }
  }
}
