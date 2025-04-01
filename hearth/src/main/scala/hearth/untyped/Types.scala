package hearth.untyped

import hearth.MacroCommons

trait Types { this: MacroCommons =>

  /** Platform-specific untyped type representation (`c.Type` in 2, `quotes.TypeRepr` in 3) */
  type UntypedType

  val UntypedType: UntypedTypeModule
  trait UntypedTypeModule { this: UntypedType.type =>

    def fromTyped[A: Type]: UntypedType
    def toTyped[A](untyped: UntypedType): Type[A]
    final def as_??(untyped: UntypedType): ?? = toTyped[Any](untyped).as_??

    def primaryConstructor(instanceTpe: UntypedType): Option[UntypedMethod]
    def constructors(instanceTpe: UntypedType): List[UntypedMethod]

    def directChildren(instanceTpe: UntypedType): Option[List[UntypedType]]

    def parameterType(instanceTpe: UntypedType)(param: UntypedParameter): UntypedType
    def parametersAt(instanceTpe: UntypedType)(method: UntypedMethod): UntypedParameters
    def unsafeApplyAt(instanceTpe: UntypedType)(method: UntypedMethod): UntypedArguments => UntypedExpr
    def returnTypeAt(instanceTpe: UntypedType)(method: UntypedMethod): UntypedType
  }

  implicit final class UntypedTypeMethods(private val untyped: UntypedType) {

    def asTyped[A]: Type[A] = UntypedType.toTyped(untyped)
    def as_?? : ?? = UntypedType.as_??(untyped)

    def primaryConstructor: Option[UntypedMethod] = UntypedType.primaryConstructor(untyped)
    def constructors: List[UntypedMethod] = UntypedType.constructors(untyped)

    def directChildren: Option[List[UntypedType]] = UntypedType.directChildren(untyped)

    def parametersAt(method: UntypedMethod): UntypedParameters =
      UntypedType.parametersAt(untyped)(method)
    def unsafeApplyAt(method: UntypedMethod): UntypedArguments => UntypedExpr =
      UntypedType.unsafeApplyAt(untyped)(method)
    def returnTypeAt(method: UntypedMethod): UntypedType =
      UntypedType.returnTypeAt(untyped)(method)
  }
}
