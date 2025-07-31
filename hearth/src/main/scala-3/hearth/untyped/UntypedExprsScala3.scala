package hearth
package untyped

trait UntypedExprsScala3 extends UntypedExprs { this: MacroCommonsScala3 =>

  import quotes.*, quotes.reflect.*

  final override type UntypedExpr = Term

  object UntypedExpr extends UntypedExprModule {

    override def fromTyped[A](expr: Expr[A]): UntypedExpr = expr.asTerm
    override def toTyped[A: Type](untyped: UntypedExpr): Expr[A] = untyped.asExprOf[A]
    override def as_??(untyped: UntypedExpr): Expr_?? = {
      val resultType = UntypedType.as_??(untyped.tpe)
      import resultType.Underlying as Result
      toTyped[Result](untyped).as_??
    }

    override def defaultValue(instanceTpe: UntypedType)(param: UntypedParameter): Option[UntypedExpr] =
      if param.hasDefault
      then Some {
        // TODO: check if constructor, otherwise we should use `name$default$idx` on instance rather than companion!!!
        val sym = instanceTpe.typeSymbol
        val companion = sym.companionModule
        val scala2default = caseClassApplyDefaultScala2(param.index + 1)
        val scala3default = caseClassApplyDefaultScala3(param.index + 1)
        val default = (companion.declaredMethod(scala2default) ++ companion
          .declaredMethod(scala3default)).headOption.getOrElse {
          // $COVERAGE-OFF$should never happen unless we messed up
          assertionFailed(
            s"Expected that ${instanceTpe.prettyPrint}'s constructor parameter `${param.name}` would have default value: attempted `$scala2default` and `$scala3default`, found: ${companion.declaredMethods}"
          )
          // $COVERAGE-ON$
        }
        Ref(companion).select(default)
      }
      else None
  }
}
