package hearth
package untyped

import scala.collection.compat.*
import scala.collection.immutable.ListMap

trait TypesScala2 extends Types { this: MacroCommonsScala2 =>

  final override type UntypedType = c.Type

  object UntypedType extends UntypedTypeModule {

    override def fromTyped[A: Type]: UntypedType = c.weakTypeOf[A]
    override def toTyped[A](untyped: UntypedType): Type[A] = c.WeakTypeTag(untyped)

    override def primaryConstructor(instanceTpe: UntypedType): Option[UntypedMethod] =
      Option(instanceTpe.typeSymbol).filter(_.isClass).map(_.asClass.primaryConstructor).filter(_.isConstructor)
    override def constructors(instanceTpe: UntypedType): List[UntypedMethod] =
      instanceTpe.decls.filter(_.isConstructor).toList

    override def directChildren(instanceTpe: UntypedType): Option[List[UntypedType]] =
      ??? // TODO: port enum types

    override def parameterAt(instanceTpe: UntypedType)(param: UntypedParameter): UntypedType =
      ??? // TODO: add this
    override def parametersAt(instanceTpe: UntypedType)(method: UntypedMethod): UntypedParameters =
      method
        .typeSignatureIn(instanceTpe)
        .paramLists
        .map(inner =>
          ListMap.from(inner.map { param =>
            param.name.decodedName.toString -> null.asInstanceOf[
              UntypedParameter
            ] // param.typeSignatureIn(instanceTpe).finalResultType - define UntypedParameter
          })
        )
    override def unsafeApplyAt(instanceTpe: UntypedType)(method: UntypedMethod): UntypedArguments => UntypedExpr =
      ??? // TODO: port ProductType constructor
    override def returnTypeAt(instanceTpe: UntypedType)(method: UntypedMethod): UntypedType =
      method.typeSignatureIn(instanceTpe).finalResultType
  }
}
