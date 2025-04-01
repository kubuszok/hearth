package hearth
package typed

trait MethodsScala2 extends Methods { this: MacroCommonsScala2 =>

  type Parameter

  object Parameter extends ParameterModule {

    override def name(param: Parameter): String = ???

    override def paramType(param: Parameter): ?? = ???
    override def defaultValue(param: Parameter): Option[Expr_??] = ???
    override def annotations(param: Parameter): List[Expr_??] = ???
  }
}
