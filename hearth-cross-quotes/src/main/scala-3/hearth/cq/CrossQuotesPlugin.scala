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
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.Trees
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.ast.untpd.UntypedTreeTraverser
import dotty.tools.dotc.ast.untpd.UntypedTreeMap
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols
import dotty.tools.dotc.core.Types
import dotty.tools.dotc.parsing.Parser
import dotty.tools.dotc.printing.Printer
import dotty.tools.dotc.typer.Typer
import dotty.tools.dotc.reporting.Diagnostic

class CrossQuotesPlugin extends StandardPlugin {
  val name = "hearth.cross-quotes"
  val description = "Rewrites Expr.quote/splice into native quotes"

  def init(options: List[String]): List[PluginPhase] = {
    val loggingSetting = "logging"
    val loggingEnabled = options
      .collectFirst {
        case setting if setting.startsWith(loggingSetting) =>
          setting.stripPrefix(loggingSetting + "=").trim == "true"
      }
      .getOrElse(false)
    List(new CrossQuotesPhase(loggingEnabled))
  }
}

final class CrossQuotesPhase(loggingEnabled: Boolean) extends PluginPhase {
  override def runsAfter: Set[String] = Set(Parser.name)
  override def runsBefore: Set[String] = Set("typer")
  override def phaseName: String = "hearth:cross-quotes"

  override def changesMembers: Boolean = true

  override def run(using Context): Unit = {
    ctx.compilationUnit.untpdTree = new UntypedTreeMap {

      private var counter: Int = 0
      private var injectingQuote: Boolean = false
      private var quotesName: SimpleName = null

      private def ensureQuotes(thunk: => untpd.Tree): untpd.Tree =
        if injectingQuote then thunk
        else
          try {
            injectingQuote = true

            // Create the ValDef for quotes

            counter += 1
            quotesName = termName(s"quotes$$macro$$$counter")
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
          } finally {
            injectingQuote = false
            quotesName = null
          }

      override def transform(tree: untpd.Tree)(using Context): untpd.Tree = tree match {
        case TypeApply(Select(Ident(tp), of), List(innerTree)) if tp.show == "Type" && of.show == "of" =>
          val result = ensureQuotes(
            untpd.Apply(
              untpd.TypeApply(
                untpd.Select(untpd.Ident(termName("CrossQuotes")), termName("castK")),
                List(
                  untpd.Select(untpd.Select(untpd.Ident(termName("scala")), termName("quoted")), typeName("Type")),
                  untpd.Ident(typeName("Type"))
                )
              ),
              List(
                untpd.TypeApply(
                  untpd.Select(
                    untpd.Select(untpd.Select(untpd.Ident(termName("scala")), termName("quoted")), termName("Type")),
                    termName("of")
                  ),
                  List(super.transform(innerTree))
                )
              )
            )
          )

          if loggingEnabled then {
            ctx.reporter.report(
              new Diagnostic.Info(
                s"Type.of[${innerTree.show}] expanded to\n${result.show}",
                tree.sourcePos
              )
            )
          }

          result

        case Apply(Select(Ident(expr), quote), List(innerTree)) if expr.show == "Expr" && quote.show == "quote" =>
          val result = ensureQuotes(
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

          if loggingEnabled then {
            ctx.reporter.report(
              new Diagnostic.Info(
                s"Expr.quote(${innerTree.show}) expanded to\n${result.show}",
                tree.sourcePos
              )
            )
          }

          result

        case Apply(Select(Ident(expr), splice), List(innerTree)) if expr.show == "Expr" && splice.show == "splice" =>
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
    super.run
  }
}
