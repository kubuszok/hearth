package hearth
package untyped

import scala.collection.immutable.ListMap

trait TypesScala3 extends Types { this: MacroCommonsScala3 =>

  import quotes.*, quotes.reflect.*

  final override type UntypedType = TypeRepr

  object UntypedType extends UntypedTypeModule {

    override def fromTyped[A: Type]: UntypedType = TypeRepr.of[A]
    override def toTyped[A](untyped: UntypedType): Type[A] = untyped.asType.asInstanceOf[Type[A]]

    override def primaryConstructor(instanceTpe: UntypedType): Option[UntypedMethod] =
      Option(instanceTpe.typeSymbol.primaryConstructor).filterNot(_.isNoSymbol)
    override def constructors(instanceTpe: UntypedType): List[UntypedMethod] =
      instanceTpe.typeSymbol.declarations.filterNot(_.isNoSymbol).filter(_.isClassConstructor)

    override def directChildren(instanceTpe: UntypedType): Option[List[UntypedType]] =
      ??? // TODO: port enum types

    override def parameterType(instanceTpe: UntypedType)(param: UntypedParameter): UntypedType =
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
