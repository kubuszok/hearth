package hearth
package cq

import scala.reflect.macros.blackbox

class CrossQuotesMacros(val c: blackbox.Context) {

  import c.universe.{Expr as _, *}

  private val loggingSetting = "hearth.cross-quotes.logging"

  private val loggingEnabled = c.settings
    .collect {
      case setting if setting.startsWith(loggingSetting) =>
        setting.stripPrefix(loggingSetting + "=").trim == "true"
    }
    .headOption
    .getOrElse(false)

  // Scala 3 generate prefix$macro$[n] while Scala 2 prefix[n] and we want to align the behavior
  private def freshName(prefix: String): TermName = c.universe.internal.reificationSupport.freshTermName(prefix)

  def typeOfImpl[A: c.WeakTypeTag]: c.Tree = {
    val ctx = freshName("ctx")

    val result = q"""
     val $ctx = CrossQuotes.ctx[scala.reflect.macros.blackbox.Context]
     import $ctx.universe.{Type => _, internal => _, _}
     weakTypeTag[${weakTypeOf[A]}].asInstanceOf[Type[${weakTypeOf[A]}]]
     """

    if (loggingEnabled) {
      c.echo(c.enclosingPosition, s"Type.of[${weakTypeOf[A]}] expanded to\n$result")
    }

    result
  }

  def quoteImpl[A: c.WeakTypeTag](expr: c.Expr[A]): c.Tree = {
    val ctx = freshName("ctx")

    val result = q"""
     val $ctx = CrossQuotes.ctx[scala.reflect.macros.blackbox.Context]
     import $ctx.universe.Quasiquote
     $ctx.Expr[${weakTypeOf[A]}](${convert(ctx)(expr.tree)}).asInstanceOf[Expr[${weakTypeOf[A]}]]
     """

    if (loggingEnabled) {
      c.echo(c.enclosingPosition, s"Expr.quote[${weakTypeOf[A]}]($expr) expanded to\n$result")
    }

    result
  }

  private def convert(ctx: TermName)(tree: c.Tree): c.Tree = {
    val unquoter = new Unquoter(ctx)
    val extractedExprs = unquoter.transform(tree)
    val replaced = unquoter.toReplace.foldLeft(extractedExprs.toString) { case (result, (toReplace, replacement)) =>
      result.replaceAll(toReplace, replacement)
    }
    val quoted = "q\"\"\"" + replaced + "\"\"\""
    c.parse(quoted)
  }

  private class Unquoter(ctx: TermName) extends Transformer {

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
            toReplace += (expr.toString() -> s"\\$${{$expr}.asInstanceOf[$ctx.Expr[scala.Any]]}")
            expr
          case _ =>
            // Handle it recursively somehow?
            // Check what we need to put as expr to make it work?
            val stub = Ident(freshName("stub"))
            toReplace += (stub.toString() -> s"\\$${{$expr}.asInstanceOf[$ctx.Expr[scala.Any]]}")
            stub
        }
      case tree =>
        super.transform(tree)
    }
  }
}
