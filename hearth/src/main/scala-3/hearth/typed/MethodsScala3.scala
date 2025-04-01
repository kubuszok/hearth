package hearth
package typed

trait MethodsScala3 extends Methods { this: MacroCommonsScala3 =>

  type Parameter

  object Parameter extends ParameterModule {

    override def name(param: Parameter): String = ???

    override def paramType(param: Parameter): ?? = ???
    override def defaultValue(param: Parameter): Option[Expr_??] = ???
    override def annotations(param: Parameter): List[Expr_??] = ???
  }
}
