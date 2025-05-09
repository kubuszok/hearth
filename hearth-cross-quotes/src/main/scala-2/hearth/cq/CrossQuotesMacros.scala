package hearth
package cq

import scala.reflect.macros.blackbox

class CrossQuotesMacros(val c: blackbox.Context) {

  import c.universe.{Expr as _, *}

  // Yes, I know it's cursed, but it was the only way to have something working fast.

  // TODO: freshName for ctx

  def quoteImpl[A: c.WeakTypeTag](expr: c.Expr[A]): c.Tree = {
    val result = q"""
     val ctx = CrossQuotes.ctx[scala.reflect.macros.blackbox.Context]
     import ctx.universe.Quasiquote
     ctx.Expr[${weakTypeOf[A]}](${convert(expr.tree)}).asInstanceOf[Expr[${weakTypeOf[A]}]]
     """

    println(result)
    result
  }

  private def convert(tree: c.Tree): c.Tree = {
    Unquoter.toReplace.clear()
    val extractedExprs = Unquoter.transform(tree)
    val replaced = Unquoter.toReplace.foldLeft(extractedExprs.toString) { case (result, (toReplace, replacement)) =>
      result.replaceAll(toReplace, replacement)
    }
    val quoted = "q\"\"\"" + replaced + "\"\"\""
    c.parse(quoted)
  }

  private object Unquoter extends Transformer {

    val toReplace = scala.collection.mutable.Map.empty[String, String]

    override def transform(tree: Tree): Tree = tree match {
      case Apply(
            TypeApply(
              Select(Select(This(_), TermName("Expr")), TermName("splice")),
              List(TypeTree())
            ),
            List(expr)
          ) =>
        expr match {
          case Ident(_) =>
            toReplace += (expr.toString() -> s"\\$${{$expr}.asInstanceOf[ctx.Expr[scala.Any]]}")
            expr
          case _ =>
            // Handle it recursively somehow?
            // Check what we need to put as expr to make it work?
            val stub = Ident(c.universe.internal.reificationSupport.freshTermName("stub"))
            toReplace += (stub.toString() -> s"\\$${{$expr}.asInstanceOf[ctx.Expr[scala.Any]]}")
            stub
        }
      case tree =>
        super.transform(tree)
    }
  }
}
