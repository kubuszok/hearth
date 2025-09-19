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

final class CrossQuotesPlugin extends StandardPlugin {
  val name = CrossQuotesSettings.crossQuotesName
  val description = "Rewrites Expr.quote/splice into native quotes"

  def init(options: List[String]): List[PluginPhase] = {
    val loggingEnabledFor = CrossQuotesSettings.parseLoggingSettingsForScala3(options)
    List(new CrossQuotesPhase(loggingEnabledFor))
  }
}

/** This plugin is responsible for rewriting Type.of/Expr.quote/Expr.splice into native quotes
  * ([[scala.quoted.Expr]]/[[scala.quoted.Type]]) in Scala 3.
  *
  *   1. The first thing that it does is to replace:
  *
  * {{{
  * Type.of[A]
  * }}}
  *
  * with:
  *
  * {{{
  * scala.quoted.Type.of[A]
  * }}}
  *
  * and:
  *
  * {{{
  * Expr.quote[A](expr)
  * }}}
  *
  * with:
  *
  * {{{
  * '{ expr }
  * }}}
  *
  * and:
  *
  * {{{
  * Expr.splice { expr }
  * }}}
  *
  * with:
  *
  * {{{
  * ${ expr }
  * }}}
  *
  * But since, both of these operations are need to use [[scala.quoted.Quotes]] we need to inject a given for it:
  *
  * {{{
  * // given is prepeded before the first ourermost Expr.quote/Expr.splice
  * given scala.quoted.Quotes = CrossQuotes.ctx
  * // the rest of the code
  * }}}
  *
  *   2. However, if there are type bounds like:
  *
  * {{{
  * [A: Type, B: Type]
  * }}}
  *
  * then we have to cast each type parameter to [[scala.quoted.Type]]:
  *
  * {{{
  * given castedA: scala.quoted.Type[A] = Type[A].asInstanceOf[scala.quoted.Type[A]]
  * given castedB: scala.quoted.Type[B] = Type[B].asInstanceOf[scala.quoted.Type[B]]
  * ...
  * }}}
  *
  *   3. Finally, if the whole expression building is decomposed into several steps, e.g.:
  *
  * {{{
  * def intToString(expr: Expr[Int]): Expr[String] = Expr.quote {
  *   Expr.splice { expr }.toString
  * }
  * Expr.quote {
  *   val a = 1
  *   Expr.splice { intToString(Expr.quote { a }) }
  * }
  * }}}
  *
  * then normally, we would have to use [[scala.quoted.Quotes.Nested]] and givens to handle that case:
  *
  * {{{
  * def intToString(expr: Expr[Int](using Quotes)): Expr[String] = '{
  *   ${ expr }.toString
  * }
  * '{
  *   val a = 1
  *   ${ intToString('{ a }) } // inside ${} we are creating a new Quotes context (q.Nested)
  * }
  * }}}
  *
  * so we need to keep trace of the current level of nested quotes and inject a given for [[scala.quoted.Quotes]] only
  * at the top level.
  *
  * {{{
  * def intToString(expr: Expr[Int]): Expr[String] =
  *   given scala.quoted.Quotes = CrossQuotes.ctx // uses current Quotes/q.Nested context
  *   '{
  *     ${ expr }.toString
  *   }
  *
  * '{
  *   val a = 1
  *   ${
  *     // inside ${} we are creating a new Quotes context (q.Nested)
  *     CrossQuotes.nestedCtx { // updates CrossQuotes.ctx
  *       intToString('{ a })
  *     } // resotres previous CrossQuotes.ctx value
  *   }
  * }
  * }}}
  */
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

      private val ctorSize = (1 to 22).map(i => s"Ctor$i" -> i).toMap
      private val isCtor = ctorSize.keySet
      private val maxUpper = untpd.Select(untpd.Ident(termName("scala")), typeName("Any"))
      private val minLower = untpd.Select(untpd.Ident(termName("scala")), typeName("Nothing"))

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
         *   Type.Ctor1.of[HKT]
         *   Type.Ctor2.of[HKT1, HKT2]
         *   ...
         *   Type.Ctor22.of[HKT1, HKT2, ..., HKT22]
         * with:
         *   given quotes: scala.quoted.Quotes = CrossQuotes.ctx[scala.quoted.Quotes]
         *   [manually cast every type param Type[A] to given scala.quoted.Type[A]]
         *   Type.Ctor1.Bounded[Nothing, Any, HKT]
         *   Type.Ctor2.Bounded[Nothing, Any, Nothing, Any, HKT1, HKT2]
         *   ...
         *   Type.Ctor22.Bounded[Nothing, Any, Nothing, Any, ..., Nothing, Any, HKT1, HKT2, ..., HKT22]
         */
        case TypeApply(prefix @ Select(Select(Ident(tp), ctor), of), List(hkt))
            if tp.show == "Type" && isCtor(ctor.show) && of.show == "of" =>
          val result = ensureQuotes(
            injectGivens(
              untpd.TypeApply(
                untpd.Select(
                  untpd.Select(
                    untpd.Select(untpd.Ident(termName("Type")), termName(ctor.show)),
                    termName("Bounded")
                  ),
                  termName("Impl")
                ),
                (1 to ctorSize(ctor.show)).flatMap(_ => List(minLower, maxUpper)).toList ++ List(transform(hkt))
              )
            )
          )

          log(
            s"""Cross-quotes Type.${ctor.show}.of expansion:
               |From: ${tree.show}
               |To: ${result.show}""".stripMargin,
            tree.sourcePos
          )

          result

        /* Replaces:
         *   Type.Ctor1.UpperBounded.of[U1, HKT]
         *   Type.Ctor2.UpperBounded.of[U1, U2, HKT]
         *   ...
         *   Type.Ctor22.UpperBounded.of[U1, U2, ..., U22, HKT]
         * with:
         *   given quotes: scala.quoted.Quotes = CrossQuotes.ctx[scala.quoted.Quotes]
         *   [manually cast every type param Type[A] to given scala.quoted.Type[A]]
         *   Type.Ctor1.Bounded[Nothing, U1, HKT]
         *   Type.Ctor2.Bounded[Nothing, U1, Nothing, U2, HKT]
         *   ...
         *   Type.Ctor22.Bounded[Nothing, U1, Nothing, U2, ..., Nothing, U22, HKT]
         */
        case TypeApply(prefix @ Select(Select(Ident(tp), ctor), of), upper :+ hkt)
            if tp.show == "Type" && isCtor(ctor.show) && of.show == "of" =>
          val result = ensureQuotes(
            injectGivens(
              untpd.TypeApply(
                untpd.Select(
                  untpd.Select(
                    untpd.Select(untpd.Ident(termName("Type")), termName(ctor.show)),
                    termName("Bounded")
                  ),
                  termName("Impl")
                ),
                upper.flatMap(u => List(minLower, u)) ++ List(transform(hkt))
              )
            )
          )

          log(
            s"""Cross-quotes Type.${ctor.show}.UpperBounded.of expansion:
               |From: ${tree.show}
               |To: ${result.show}""".stripMargin,
            tree.sourcePos
          )

          result

        /* Replaces:
         *   Type.Ctor1.Bounded.of[L1, U1, HKT]
         *   Type.Ctor2.Bounded.of[L1, U1, L2, U2, HKT]
         *   ...
         *   Type.Ctor22.Bounded.of[L1, U1, L2, U2, ..., L22, U22, HKT]
         * with:
         *   given quotes: scala.quoted.Quotes = CrossQuotes.ctx[scala.quoted.Quotes]
         *   [manually cast every type param Type[A] to given scala.quoted.Type[A]]
         *   Type.Ctor1.Bounded[L1, U1, HKT]
         *   Type.Ctor2.Bounded[L1, U1, L2, U2, HKT]
         *   ...
         *   Type.Ctor22.Bounded[L1, U1, L2, U2, ..., L22, U22, HKT]
         */
        case TypeApply(prefix @ Select(Select(Ident(tp), ctor), of), bounded :+ hkt)
            if tp.show == "Type" && isCtor(ctor.show) && of.show == "of" =>
          val result = ensureQuotes(
            injectGivens(
              untpd.TypeApply(
                untpd.Select(
                  untpd.Select(
                    untpd.Select(untpd.Ident(termName("Type")), termName(ctor.show)),
                    termName("Bounded")
                  ),
                  termName("Impl")
                ),
                bounded ++ List(transform(hkt))
              )
            )
          )

          log(
            s"""Cross-quotes Type.${ctor.show}.UpperBounded.of expansion:
               |From: ${tree.show}
               |To: ${result.show}""".stripMargin,
            tree.sourcePos
          )

          result

        /* Replaces:
         *   Expr.quote[A](thunk)
         * with:
         *   given quotes: scala.quoted.Quotes = CrossQuotes.ctx[scala.quoted.Quotes]
         *   [manually cast every type param Type[A] to given scala.quoted.Type[A]]
         *   CrossQuotes.exprQuotesToCross('{ thunk })
         */
        case Apply(Select(Ident(expr), quote), List(thunk)) if expr.show == "Expr" && quote.show == "quote" =>
          val result = ensureQuotes(
            injectGivens(
              untpd.Apply(
                untpd.Select(untpd.Ident(termName("CrossQuotes")), termName("exprQuotesToCross")),
                List(untpd.Quote(transform(thunk), Nil))
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
        case Apply(Select(Ident(expr), splice), List(thunk)) if expr.show == "Expr" && splice.show == "splice" =>
          val result = ensureQuotes(
            untpd.Splice(
              untpd.Apply(
                untpd.Select(untpd.Ident(termName("CrossQuotes")), termName("nestedCtx")),
                List(
                  untpd.Apply(
                    untpd.Select(untpd.Ident(termName("CrossQuotes")), termName("exprCrossToQuotes")),
                    List(transform(thunk))
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

        /* Looking for blocks with imports that contain Underlying.
         * For each such import we inject:
         *   given castedUnderlying: scala.quoted.Type[Underlying] =
         *     Type[Underlying].asInstanceOf[scala.quoted.Type[Underlying]]
         */
        case Block(stats, expr) if stats.collectFirst {
              case Import(_, selectors) if selectors.exists(_.imported.show == "Underlying") => ()
            }.isDefined =>

          val newGivenCandidates = stats.collect {
            case Import(expr, selectors) if selectors.exists(_.imported.show == "Underlying") =>
              selectors.collect {
                case untpd.ImportSelector(Ident(underlying), rename, _) if underlying.show.contains("Underlying") =>
                  val name = rename.match {
                    case Ident(name) => name
                    case _           => underlying
                  }.show
                  val tpe = rename match {
                    case Ident(name) => untpd.Ident(typeName(name.show))
                    case _           => untpd.Select(expr, typeName(underlying.show))
                  }

                  untpd
                    .ValDef(
                      name = termName(s"casted$name"),
                      tpt = untpd.AppliedTypeTree(
                        untpd.Select(
                          untpd.Select(untpd.Ident(termName("scala")), termName("quoted")),
                          typeName("Type")
                        ),
                        List(tpe)
                      ),
                      rhs = untpd.TypeApply(
                        untpd.Select(
                          untpd.TypeApply(
                            untpd.Ident(termName("Type")),
                            List(tpe)
                          ),
                          termName("asInstanceOf")
                        ),
                        List(
                          untpd.AppliedTypeTree(
                            untpd.Select(
                              untpd.Select(untpd.Ident(termName("scala")), termName("quoted")),
                              typeName("Type")
                            ),
                            List(tpe)
                          )
                        )
                      )
                    )
                    .withFlags(Flags.Given)
              }
          }.flatten

          val oldGivenCandidates = givenCandidates
          try {
            givenCandidates = oldGivenCandidates ++ newGivenCandidates
            untpd.Block(stats.map(transform), transform(expr))
          } finally
            givenCandidates = oldGivenCandidates

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
