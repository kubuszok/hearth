package hearth
package typed

trait MethodsScala2 extends Methods { this: MacroCommonsScala2 =>

  final class Parameter(val untyped: UntypedParameter, val instanceTpe: UntypedType)

  object Parameter extends ParameterModule {

    override def name(param: Parameter): String = UntypedParameter.name(param.untyped)

    override def paramType(param: Parameter): ?? =
      param.instanceTpe.parameter(param.untyped).as_??
    override def defaultValue(param: Parameter): Option[Expr_??] =
      UntypedExpr.defaultValue(param.instanceTpe)(param.untyped).map(_.as_??)

    override def annotations(param: Parameter): List[Expr_??] =
      UntypedParameter.annotations(param.untyped).map(_.as_??)
  }
}
