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
    val termB = freshName("B")
    val typeB = TypeName(freshName("B").toString)
    val ctx = freshName("ctx")
    val convertProvidedTypesForCrossQuotes = freshName("convertProvidedTypesForCrossQuotes")

    // Replaces Type.of[A]
    // with...
    // what we see in Quasiquote
    val result = q"""
      val $ctx = CrossQuotes.ctx[scala.reflect.macros.blackbox.Context]
      import $ctx.universe.{Type => _, internal => _, _}
      implicit def $convertProvidedTypesForCrossQuotes[$typeB](implicit $termB: Type[$typeB]): $ctx.WeakTypeTag[$typeB] =
        $termB.asInstanceOf[$ctx.WeakTypeTag[$typeB]]
      _root_.hearth.fp.ignore($convertProvidedTypesForCrossQuotes[Any](_: Type[Any]))
      weakTypeTag[${weakTypeOf[A]}].asInstanceOf[Type[${weakTypeOf[A]}]]
      """

    if (loggingEnabled) {
      c.echo(
        c.enclosingPosition,
        s"""Cross-quotes Type.of expansion:
           |From: Type.of[${weakTypeOf[A]}]
           |To: $result""".stripMargin
      )
    }

    result
  }

  def quoteImpl[A: c.WeakTypeTag](expr: c.Expr[A]): c.Tree = {
    val termB = freshName("B")
    val typeB = TypeName(freshName("B").toString)
    val ctx = freshName("ctx")
    val convertProvidedTypesForCrossQuotes = freshName("convertProvidedTypesForCrossQuotes")

    // Replaces Expr.quote[A](a)
    // with...
    // what we see in Quasiquote
    val result = q"""
      val $ctx = CrossQuotes.ctx[scala.reflect.macros.blackbox.Context]
      import $ctx.universe.Quasiquote
      implicit def $convertProvidedTypesForCrossQuotes[$typeB](implicit $termB: Type[$typeB]): $ctx.WeakTypeTag[$typeB] =
        $termB.asInstanceOf[$ctx.WeakTypeTag[$typeB]]
      _root_.hearth.fp.ignore($convertProvidedTypesForCrossQuotes[Any](_: Type[Any]))
      $ctx.Expr[${weakTypeOf[A]}](${convert(ctx)(expr.tree)}).asInstanceOf[Expr[${weakTypeOf[A]}]]
      """

    if (loggingEnabled) {
      c.echo(
        c.enclosingPosition,
        s"""Cross-quotes Expr.quote expansion:
           |From: Expr.quote[${weakTypeOf[A]}]($expr)
           |To: $result""".stripMargin
      )
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
      // Replaces Expr.splice[A](a)
      // with
      // e.asInstanceOf[ctx.Expr[A]]
      case Apply(
            TypeApply(
              Select(Select(This(_), TermName("Expr")), TermName("splice")),
              List(tpe)
            ),
            List(expr)
          ) =>
        val result = convert(ctx)(expr) match {
          case Ident(_) =>
            toReplace += (expr.toString() -> s"\\$${{$expr}.asInstanceOf[$ctx.Expr[$tpe]]}")
            expr
          case _ =>
            val stub = Ident(freshName("stub"))
            toReplace += (stub.toString() -> s"\\$${{$expr}.asInstanceOf[$ctx.Expr[$tpe]]}")
            stub
        }

        if (loggingEnabled) {
          c.echo(
            c.enclosingPosition,
            s"""Cross-quotes Expr.splice expansion:
               |From: Expr.splice[$tpe]($expr)
               |To: $result""".stripMargin
          )
        }

        result

      case tree => super.transform(tree)
    }
  }
}
