package hearth
package untyped

import hearth.MacroCommonsScala2

trait UntypedExprsScala2 extends UntypedExprs { this: MacroCommonsScala2 =>

  import c.universe.*

  final override type UntypedExpr = Tree

  object UntypedExpr extends UntypedExprModule {

    override def fromTyped[A](expr: Expr[A]): UntypedExpr = expr.tree
    override def toTyped[A: Type](untyped: UntypedExpr): Expr[A] = c.Expr[A](untyped)
    override def as_??(untyped: UntypedExpr): Expr_?? = {
      val resultType: ?? =
        c.Expr(untyped).attemptPipe(_.actualType.finalResultType)(_.staticType.finalResultType).as_??
      import resultType.Underlying as Result
      toTyped[Result](untyped).as_??
    }

    override def defaultValue(instanceTpe: UntypedType)(param: UntypedParameter): Option[UntypedExpr] = if (
      param.hasDefault
    ) Some {
      // TODO: check if constructor, otherwise we should use `name$default$idx` on instance rather than companion!!!
      lazy val companion = companionSymbol(instanceTpe.asTyped[Any])
      val scala2default = caseClassApplyDefaultScala2(param.index + 1)
      val scala3default = caseClassApplyDefaultScala3(param.index + 1)
      val newDefault = classNewDefaultScala2(param.index + 1)
      val defaults = List(scala2default, scala3default, newDefault)
      val default = companion.typeSignature.decls
        .to(List)
        .collectFirst {
          case method if defaults.contains(method.name.decodedName.toString) => method
        }
        .getOrElse {
          // $COVERAGE-OFF$should never happen unless someone mess around with type-level representation
          assertionFailed(
            s"Expected that ${instanceTpe.prettyPrint}'s constructor parameter `${param.name}` would have default value: attempted `$scala2default`, `$scala3default` and `$newDefault`, found: ${companion.typeSignature.decls}"
          )
          // $COVERAGE-ON$
        }
      q"$companion.$default"
    }
    else None
  }

  // Borrowed from jsoniter-scala: https://github.com/plokhotnyuk/jsoniter-scala/blob/b14dbe51d3ae6752e5a9f90f1f3caf5bceb5e4b0/jsoniter-scala-macros/shared/src/main/scala/com/github/plokhotnyuk/jsoniter_scala/macros/JsonCodecMaker.scala#L462
  private def companionSymbol[A: Type]: Symbol = {
    val sym = Type[A].tpe.typeSymbol
    val comp = sym.companion
    if (comp.isModule) comp
    else {
      val ownerChainOf: Symbol => Iterator[Symbol] =
        s => Iterator.iterate(s)(_.owner).takeWhile(x => x != null && x != NoSymbol).toVector.reverseIterator
      val path = ownerChainOf(sym)
        .zipAll(ownerChainOf(c.internal.enclosingOwner), NoSymbol, NoSymbol)
        .dropWhile { case (x, y) => x == y }
        .takeWhile(_._1 != NoSymbol)
        .map(_._1.name.toTermName)
      // $COVERAGE-OFF$should never happen unless someone mess around with type-level representation
      if (path.isEmpty) assertionFailed(s"Cannot find a companion for ${Type.prettyPrint[A]}")
      else c.typecheck(path.foldLeft[Tree](Ident(path.next()))(Select(_, _)), silent = true).symbol
      // $COVERAGE-ON$
    }
  }
}
