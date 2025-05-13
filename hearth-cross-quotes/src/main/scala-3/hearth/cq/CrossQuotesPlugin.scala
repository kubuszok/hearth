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

            // given convertProvidedTypesForCrossQuotes[A](using A: Type[A]): scala.quoted.Type[A] =
            //   A.asInstanceOf[scala.quoted.Type[A]]
            val convertProvidedTypesForCrossQuotes = locally {
              val typeB = typeName(s"B$$macro$$$counter")
              val termB = termName(s"BB$$macro$$$counter")
              val paramB = untpd
                .ValDef(
                  name = termB,
                  tpt = untpd.AppliedTypeTree(
                    // untpd.Select(
                    //   untpd.Select(untpd.Ident(termName("scala")), termName("quoted")),
                    //   typeName("Type")
                    // ),
                    untpd.Ident(typeName("Type")),
                    List(untpd.Ident(typeB))
                  ),
                  rhs = untpd.EmptyTree
                )

              val name = termName(s"convertProvidedTypesForCrossQuotes$$macro$$$counter")
              val tpe = untpd.AppliedTypeTree(
                untpd.Select(
                  untpd.Select(untpd.Ident(termName("scala")), termName("quoted")),
                  typeName("Type")
                ),
                List(untpd.Ident(typeB))
              )
              val body = untpd.TypeApply(
                untpd.Select(untpd.Ident(termB), termName("asInstanceOf")),
                List(
                  untpd.AppliedTypeTree(
                    untpd
                      .Select(untpd.Select(untpd.Ident(termName("scala")), termName("quoted")), typeName("Type")),
                    List(untpd.Ident(typeB))
                  )
                )
              )
              
              untpd
                .DefDef(
                  name = name,
                  paramss = List(
                    // Type parameter [B]
                    untpd.TypeDef(typeB, untpd.TypeBoundsTree(untpd.EmptyTree, untpd.EmptyTree)).withFlags(Flags.Param) :: Nil,
                    // Using parameter (using B: Type[B])
                    List(paramB.withFlags(Flags.Param | Flags.Given))
                  ),
                  tpt = tpe,
                  rhs = body
                )
                .withFlags(Flags.Given)
            }

            untpd.Block(
              List(quotes, convertProvidedTypesForCrossQuotes),
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

        case t =>
          if t.show.contains("convertProvidedTypesForCrossQuotes") then {
            println(s"convertProvidedTypesForCrossQuotes: ${t.show} -> $t")
            t match {
              case DefDef(
                    a,
                    List(
                      List(bb @ TypeDef(b, TypeBoundsTree(EmptyTree, EmptyTree, EmptyTree))),
                      List(cc @ ValDef(c, AppliedTypeTree(Ident(_), List(Ident(d))), EmptyTree))
                    ),
                    AppliedTypeTree(Select(Select(Ident(_), _), _), List(Ident(e))),
                    TypeApply(
                      Select(Ident(f), asInstanceOf),
                      List(AppliedTypeTree(Select(Select(Ident(_), _), _), List(Ident(g))))
                    )
                  ) =>
                    println(s"A: $a -> ${a.isTermName} ${a.isTypeName}")
                println(s"B: $b -> ${b.isTermName} ${b.isTypeName}")
                println(s"C: $c -> ${c.isTermName} ${c.isTypeName}")
                println(s"D: $d -> ${d.isTermName} ${d.isTypeName}")
                println(s"E: $e -> ${e.isTermName} ${e.isTypeName}")
                println(s"F: $f -> ${f.isTermName} ${f.isTypeName}")
                println(s"G: $g -> ${g.isTermName} ${g.isTypeName}")
                println(s"BB: ${bb} ${bb.mods} ${bb.mods.flags.flagsString}")
                println(s"CC: ${cc} ${cc.mods} ${cc.mods.flags.flagsString}")
              case _ =>
            }
          }

          super.transform(t)
      }
    }.transform(ctx.compilationUnit.untpdTree)
    super.run
  }
}
