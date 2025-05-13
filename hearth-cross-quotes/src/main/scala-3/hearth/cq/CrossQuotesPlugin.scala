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

            val quotes = locally {
              quotesName = termName(s"quotes$$macro$$$counter")
              val tpe =
                untpd.Select(untpd.Select(untpd.Ident(termName("scala")), termName("quoted")), typeName("Quotes"))
              val value = untpd.Select(untpd.Ident(termName("CrossQuotes")), termName("ctx"))
              untpd.ValDef(quotesName, tpe, value).withFlags(Flags.Given)
            }

            untpd.Block(
              List(quotes),
              thunk
            )
          } finally {
            injectingQuote = false
            quotesName = null
          }

      private var givenCandidates = List.empty[untpd.ValDef]
      private var givensInjected = Set.empty[untpd.ValDef]

      private def injectGivens(thunk: => untpd.Tree): untpd.Tree = {
        val oldGivensInjected = givensInjected
        val newGivensInjected = givenCandidates.filterNot(givensInjected)
        if newGivensInjected.isEmpty then thunk
        else {
          try {
            givensInjected = givensInjected ++ newGivensInjected
            untpd.Block(newGivensInjected, thunk)
          } finally
            givensInjected = oldGivensInjected
        }
      }

      override def transform(tree: untpd.Tree)(using Context): untpd.Tree = tree match {
        // Replaces Type.of[A]
        // with
        // given quotes: scala.quoted.Quotes = CrossQuotes.ctx[scala.quoted.Quotes]
        // [manually cast every type param Type[A] to given scala.quoted.Type[A]]
        // CrossQuotes.castK[scala.quoted.Type, Type](scala.quoted.Type.of[A])
        case TypeApply(Select(Ident(tp), of), List(innerTree)) if tp.show == "Type" && of.show == "of" =>
          val result = ensureQuotes(
            injectGivens(
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
                    List(transform(innerTree))
                  )
                )
              )
            )
          )

          if loggingEnabled then {
            ctx.reporter.report(
              new Diagnostic.Info(
                s"""Cross-quotes Type.of expansion:
                   |From: ${tree.show}
                   |To: ${result.show}""".stripMargin,
                tree.sourcePos
              )
            )
          }

          result

        // Replaces Type.of[A]
        // with
        // given quotes: scala.quoted.Quotes = CrossQuotes.ctx[scala.quoted.Quotes]
        // [manually cast every type param Type[A] to given scala.quoted.Type[A]]
        // CrossQuotes.castK[scala.quoted.Type, Type](scala.quoted.Type.of[A])
        case Apply(Select(Ident(expr), quote), List(innerTree)) if expr.show == "Expr" && quote.show == "quote" =>
          val result = ensureQuotes(
            injectGivens(
              untpd.Apply(
                untpd.TypeApply(
                  untpd.Select(untpd.Ident(termName("CrossQuotes")), termName("castK")),
                  List(
                    untpd.Select(untpd.Select(untpd.Ident(termName("scala")), termName("quoted")), typeName("Expr")),
                    untpd.Ident(typeName("Expr"))
                  )
                ),
                List(untpd.Quote(transform(innerTree), Nil))
              )
            )
          )

          if loggingEnabled then {
            ctx.reporter.report(
              new Diagnostic.Info(
                s"""Cross-quotes Expr.quote expansion:
                   |From: ${tree.show}
                   |To: ${result.show}""".stripMargin,
                tree.sourcePos
              )
            )
          }

          result

        case Apply(Select(Ident(expr), splice), List(innerTree)) if expr.show == "Expr" && splice.show == "splice" =>
          val result = ensureQuotes(
            untpd.Splice(
              untpd.Apply(
                untpd.TypeApply(
                  untpd.Select(untpd.Ident(termName("CrossQuotes")), termName("castK")),
                  List(
                    untpd.Ident(typeName("Expr")),
                    untpd.Select(untpd.Select(untpd.Ident(termName("scala")), termName("quoted")), typeName("Expr"))
                  )
                ),
                List(transform(innerTree))
              )
            )
          )

          if loggingEnabled then {
            ctx.reporter.report(
              new Diagnostic.Info(
                s"""Cross-quotes Expr.quote expansion:
                   |From: ${tree.show}
                   |To: ${result.show}""".stripMargin,
                tree.sourcePos
              )
            )
          }

          result

        case dd @ DefDef(
              methodName,
              paramss,
              returnTpe,
              body: untpd.Tree
            ) =>

          val newGivenCandidates = paramss.flatten[Any].collect {
            case TypeDef(
                  name,
                  untpd.ContextBounds(_, List(AppliedTypeTree(Ident(tpe), List(Ident(name2)))))
                ) if tpe.show == "Type" && name.show == name2.show =>

              untpd
                .ValDef(
                  name = termName(s"casted${name.mangledString}"),
                  tpt = untpd.AppliedTypeTree(
                    untpd.Select(
                      untpd.Select(untpd.Ident(termName("scala")), termName("quoted")),
                      typeName("Type")
                    ),
                    List(untpd.Ident(name))
                  ),
                  rhs = untpd.TypeApply(
                    untpd.Select(
                      untpd.TypeApply(
                        untpd.Ident(termName("Type")),
                        List(untpd.Ident(name))
                      ),
                      termName("asInstanceOf")
                    ),
                    List(
                      untpd.AppliedTypeTree(
                        untpd.Select(
                          untpd.Select(untpd.Ident(termName("scala")), termName("quoted")),
                          typeName("Type")
                        ),
                        List(untpd.Ident(name))
                      )
                    )
                  )
                )
                .withFlags(Flags.Given)
          }
          val _ = newGivenCandidates

          val oldGivenCandidates = givenCandidates
          try {
            givenCandidates = oldGivenCandidates ++ newGivenCandidates
            untpd.DefDef(methodName, paramss, returnTpe, transform(body)).withMods(dd.mods)
          } finally
            givenCandidates = oldGivenCandidates

        case t => super.transform(t)
      }
    }.transform(ctx.compilationUnit.untpdTree)
    super.run
  }
}
