package hearth
package cq

import dotty.tools.dotc.plugins.{PluginPhase, StandardPlugin}
import dotty.tools.*
import dotc.*
import dotc.ast.tpd.*
import core.Contexts.*
import core.Definitions
import core.Names.*
import core.Symbols.*
import core.Types.*
import core.StdNames.*
import core.Constants.Constant
import core.Flags.*
import dotty.tools.dotc.printing.Printer
import dotty.tools.dotc.core.Symbols
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Types
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.parsing.Parser
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.typer.Typer
import dotty.tools.dotc.ast.Trees
import dotty.tools.dotc.ast.untpd.UntypedTreeTraverser
import dotty.tools.dotc.ast.untpd.UntypedTreeMap

class CrossQuotesPlugin extends StandardPlugin {
  val name = "cross-quotes"
  val description = "Rewrites Expr.quote/splice into native quotes"

  def init(options: List[String]): List[PluginPhase] = List(new CrossQuotesPhase)
}

final class CrossQuotesPhase extends PluginPhase {
  override def runsAfter: Set[String] = Set(Parser.name)
  override def runsBefore: Set[String] = Set("typer")
  override def phaseName: String = "hearth:cross-quotes"

  override def changesMembers: Boolean = true

  override def run(using Context): Unit = {

    val isOurPatient = ctx.compilationUnit.untpdTree.show.contains("Expr.quote")

    if isOurPatient then {
      println(s"Tree: ${ctx.compilationUnit.untpdTree}")
    }
    ctx.compilationUnit.untpdTree = new UntypedTreeMap {

      private var injectingQuote: Boolean = false

      // TODO: freshName for ctx

      private def ensureQuotes(thunk: => untpd.Tree): untpd.Tree =
        if injectingQuote then thunk
        else
          try {
            injectingQuote = true

            // Create the ValDef for quotes
            val quotesName = termName("quotes")
            val quotesType =
              untpd.Select(untpd.Select(untpd.Ident(termName("scala")), termName("quoted")), typeName("Quotes"))
            val quotesValue = untpd.Select(untpd.Ident(termName("CrossQuotes")), termName("ctx"))

            val quotesDef = untpd
              .ValDef(
                quotesName,
                quotesType,
                quotesValue
              )
              .withFlags(Flags.Given)

            untpd.Block(
              List(quotesDef),
              thunk
            )
          } finally
            injectingQuote = false

      override def transform(tree: untpd.Tree)(using Context): untpd.Tree = tree match {
        case Apply(Select(Ident(expr), quote), List(innerTree)) if expr.show == "Expr" && quote.show == "quote" =>
          println(s"Found quote: $tree -> ${tree.show}")
          ensureQuotes(
            untpd.Apply(
              untpd.TypeApply(
                untpd.Select(untpd.Ident(termName("CrossQuotes")), termName("castK")),
                List(
                  untpd.Select(untpd.Select(untpd.Ident(termName("scala")), termName("quoted")), typeName("Expr")),
                  untpd.Ident(typeName("Expr"))
                )
              ),
              List(untpd.Quote(super.transform(innerTree), Nil))
            )
          )

        case Apply(Select(Ident(expr), splice), List(innerTree)) if expr.show == "Expr" && splice.show == "splice" =>
          println(s"Found splice: $tree -> ${tree.show}")
          ensureQuotes(
            untpd.Splice(
              untpd.Apply(
                untpd.TypeApply(
                  untpd.Select(untpd.Ident(termName("CrossQuotes")), termName("castK")),
                  List(
                    untpd.Ident(typeName("Expr")),
                    untpd.Select(untpd.Select(untpd.Ident(termName("scala")), termName("quoted")), typeName("Expr"))
                  )
                ),
                List(super.transform(innerTree))
              )
            )
          )

        case t => super.transform(t)
      }
    }.transform(ctx.compilationUnit.untpdTree)
    if isOurPatient then {
      println(s"Tree: ${ctx.compilationUnit.untpdTree.show}")
    }
    super.run
  }
}
