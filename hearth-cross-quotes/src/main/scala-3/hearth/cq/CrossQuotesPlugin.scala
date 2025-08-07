package hearth
package cq

import dotty.tools.dotc.plugins.{PluginPhase, StandardPlugin}
import dotty.tools.*
import dotty.tools.dotc.*
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.ast.Trees
import dotty.tools.dotc.ast.Trees.Tree
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.ast.untpd.UntypedTreeTraverser
import dotty.tools.dotc.ast.untpd.UntypedTreeMap
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Definitions
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.Names.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.core.StdNames.*
import dotty.tools.dotc.core.Symbols
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types
import dotty.tools.dotc.parsing.Parser
import dotty.tools.dotc.printing.Printer
import dotty.tools.dotc.reporting.Diagnostic
import dotty.tools.dotc.util.SourcePosition
import dotty.tools.dotc.typer.Typer
import java.io.File as JFile
import scala.jdk.OptionConverters.*

class CrossQuotesPlugin extends StandardPlugin {
  val name = CrossQuotesSettings.crossQuotesName
  val description = "Rewrites Expr.quote/splice into native quotes"

  def init(options: List[String]): List[PluginPhase] = {
    val loggingEnabledFor = CrossQuotesSettings.parseLoggingSettingsForScala3(options)
    List(new CrossQuotesPhase(loggingEnabledFor))
  }
}

final class CrossQuotesPhase(loggingEnabledFor: Option[JFile] => Boolean) extends PluginPhase {
  override def runsAfter: Set[String] = Set(Parser.name)
  override def runsBefore: Set[String] = Set("typer")
  override def phaseName: String = "hearth:cross-quotes"

  override def changesMembers: Boolean = true

  override def run(using Context): Unit = {
    val loggingEnabled = loggingEnabledFor(ctx.compilationUnit.source.jfile.toScala)

    inline def log(inline message: String, inline src: SourcePosition): Unit = if loggingEnabled then {
      // println(s"Logging: $message at ${src.source.path}:${src.line}")
      ctx.reporter.report(new Diagnostic.Info(message, src))
    }

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
        val newGivensInjectedWithSuppression = newGivensInjected.flatMap { valdef =>
          /* Returns code:
           *   new_given
           * errors with:
           *    Unused symbol error
           */
          // List(valdef)

          /* Returns code:
           *   val _ = new_given
           * errors with:
           *    _ is already defined as value _
           * when there is more than 1 type parameter in the given
           */
          // val suppression = untpd.ValDef(termName("_"), valdef.tpt, untpd.Ident(valdef.name))
          // List(valdef, suppression)

          /* Returns code:
           *   { val _ = new_given }
           * errors with:
           *    unhandled exception while running posttyper on ...
           *    java.lang.AssertionError: NoDenotation.owner
           */
          // val suppression = untpd.Block(List.empty, untpd.ValDef(termName("_"), valdef.tpt, untpd.Ident(valdef.name)))
          // List(valdef, suppression)

          /* Returns code:
           *   hearth.fp.ignore(new_given)
           * no errors
           */
          val suppression =
            untpd.Apply(
              untpd.Select(untpd.Select(untpd.Ident(termName("hearth")), termName("fp")), termName("ignore")),
              untpd.Ident(valdef.name)
            )
          List(valdef, suppression)
        }
        if newGivensInjected.isEmpty then thunk
        else {
          try {
            givensInjected = givensInjected ++ newGivensInjected
            untpd.Block(newGivensInjectedWithSuppression, thunk)
          } finally
            givensInjected = oldGivensInjected
        }
      }

      override def transform(tree: untpd.Tree)(using Context): untpd.Tree = tree match {
        /* Replaces:
         *   Type.of[A]
         * with:
         *   given quotes: scala.quoted.Quotes = CrossQuotes.ctx[scala.quoted.Quotes]
         *   [manually cast every type param Type[A] to given scala.quoted.Type[A]]
         *   CrossQuotes.typeQuotesToCross(scala.quoted.Type.of[A])
         */
        case TypeApply(Select(Ident(tp), of), List(innerTree)) if tp.show == "Type" && of.show == "of" =>
          val result = ensureQuotes(
            injectGivens(
              untpd.Apply(
                untpd.Select(untpd.Ident(termName("CrossQuotes")), termName("typeQuotesToCross")),
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

          log(
            s"""Cross-quotes Type.of expansion:
               |From: ${tree.show}
               |To: ${result.show}""".stripMargin,
            tree.sourcePos
          )

          result

        /* Replaces:
         *   Expr.quote[A](expr)
         * with:
         *   given quotes: scala.quoted.Quotes = CrossQuotes.ctx[scala.quoted.Quotes]
         *   [manually cast every type param Type[A] to given scala.quoted.Type[A]]
         *   CrossQuotes.exprQuotesToCross('{ expr })
         */
        case Apply(Select(Ident(expr), quote), List(innerTree)) if expr.show == "Expr" && quote.show == "quote" =>
          val result = ensureQuotes(
            injectGivens(
              untpd.Apply(
                untpd.Select(untpd.Ident(termName("CrossQuotes")), termName("exprQuotesToCross")),
                List(untpd.Quote(transform(innerTree), Nil))
              )
            )
          )

          log(
            s"""Cross-quotes Expr.quote expansion:
               |From: ${tree.show}
               |To: ${result.show}""".stripMargin,
            tree.sourcePos
          )

          result

        /* Replaces:
         *   Expr.splice[A](expr)
         * with:
         *   given quotes: scala.quoted.Quotes = CrossQuotes.ctx[scala.quoted.Quotes]
         *   [manually cast every type param Type[A] to given scala.quoted.Type[A]]
         *   ${
         *     CrossQuotes.nestedCtx { // <- necessary to handle q.Nested cases
         *       CrossQuotes.exprCrossToQuotes(expr)
         *     }
         *   }
         */
        case Apply(Select(Ident(expr), splice), List(innerTree)) if expr.show == "Expr" && splice.show == "splice" =>
          val result = ensureQuotes(
            untpd.Splice(
              untpd.Apply(
                untpd.Select(untpd.Ident(termName("CrossQuotes")), termName("nestedCtx")),
                List(
                  untpd.Apply(
                    untpd.Select(untpd.Ident(termName("CrossQuotes")), termName("exprCrossToQuotes")),
                    List(transform(innerTree))
                  )
                )
              )
            )
          )

          log(
            s"""Cross-quotes Expr.splice expansion:
               |From: ${tree.show}
               |To: ${result.show}""".stripMargin,
            tree.sourcePos
          )

          result

        /* The cross-quotes code would have type bounds like:
         *  [A: Type, B: Type]
         * while Scala 3 quotes would expect:
         *  [A: scala.quoted.Type, B: scala.quoted.Type]
         * so we have to cast each type parameter to scala.quoted.Type
         */
        case dd @ DefDef(
              methodName,
              paramss,
              returnTpe,
              body: untpd.Tree
            ) =>

          val newGivenCandidates = paramss.flatten[Any].collect {
            /* If parameters is
             *   [A: Type]
             * then the name of the parameter is A and the type inside Type[_] type is also A, which we can use to distinguish such bound.
             * Then we can injects a given for A:
             *   given castedA: scala.quoted.Type[A] = Type[A].asInstanceOf[scala.quoted.Type[A]]
             */
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

          val oldGivenCandidates = givenCandidates
          try {
            givenCandidates = oldGivenCandidates ++ newGivenCandidates
            untpd.DefDef(methodName, paramss, returnTpe, transform(body)).withMods(dd.mods)
          } finally
            givenCandidates = oldGivenCandidates

        case t =>

          super.transform(t)
      }
    }.transform(ctx.compilationUnit.untpdTree)
    super.run
  }
}
