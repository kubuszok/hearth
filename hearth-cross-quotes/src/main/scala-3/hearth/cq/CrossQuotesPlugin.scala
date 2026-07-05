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

  override def init(options: List[String]): List[PluginPhase] = {
    val loggingEnabled = CrossQuotesSettings.parseLoggingSettingsForScala3(options)
    List(new CrossQuotesPhase(loggingEnabled))
  }
}

/** This plugin is responsible for rewriting Type.of/Expr.quote/Expr.splice into native quotes
  * ([[scala.quoted.Expr]]/[[scala.quoted.Type]]) in Scala 3.
  *
  * ==1. Type.of[A]==
  *
  * Replaces:
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
  * ==2. Expr.quote[A](expr)==
  *
  * Replaces:
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
  * ==3. Expr.splice { expr }==
  *
  * Replaces:
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
  * Since both of these operations need [[scala.quoted.Quotes]], we inject a given for it:
  *
  * {{{
  * // given is prepended before the first outermost Expr.quote/Expr.splice
  * given scala.quoted.Quotes = CrossQuotes.ctx
  * // the rest of the code
  * }}}
  *
  * ==4. Type context bounds==
  *
  * If there are type bounds like:
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
  * ==5. Nested Quotes context management==
  *
  * If the whole expression building is decomposed into several steps, e.g.:
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
  * so we need to keep track of the current level of nested quotes and inject a given for [[scala.quoted.Quotes]] only
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
  *     } // restores previous CrossQuotes.ctx value
  *   }
  * }
  * }}}
  *
  * ==6. Local type params and HKT type constructors==
  *
  * Local type params from methods defined inside `'{ ... }` (e.g. `def helper[A]`) are handled natively by Scala 3's
  * staging system. When `Type.of[A]` is used inside `$${ ... }`, the plugin transforms it to
  * `CrossQuotes.typeQuotesToCross(scala.quoted.Type.of[A])`, and the staging system automatically provides
  * `scala.quoted.Type[A]` for type params from enclosing `'{ ... }` blocks. No workaround is needed on Scala 3 — the
  * free type + workaround method mechanism is a Scala 2-only limitation.
  *
  * For HKT type constructors (`Type.Ctor1[F]`, `Type.Ctor2[F]`, etc.), the plugin uses two mechanisms to inject
  * `given`s:
  *
  *   - `boundGivenCandidates`: inspects `[F[_]: Type.Ctor1]` context bounds and explicit `implicit` params (e.g.
  *     `implicit FC: Type.Ctor1[F]`) on enclosing method signatures.
  *   - `blockGivenCandidates`: inspects `implicit val`/`def` declarations in the enclosing block body (e.g.
  *     `implicit val FC: Type.Ctor1[Option] = ...`).
  *
  * ==Gotchas and limitations (Scala 3)==
  *
  *   - '''Self-referential implicit Type/Type.CtorN (fixed by hearth#285):'''
  *     `implicit val ConfigT: Type[Configuration] = Type.of[Configuration]` and
  *     `implicit val OptionCtor: Type.Ctor1[Option] = Type.Ctor1.of[Option]` used to inject a `given` that summoned the
  *     very value being initialized (forward-reference error in blocks, "Infinite loop in function body" or null
  *     self-assignment in class bodies). The plugin now excludes the definition being initialized (and, in blocks, any
  *     implicit val declared in a later statement) from the injected `casted` givens, so the type is materialized
  *     directly from the literal type argument. Note that `implicit val A: Type[A] = Type.of[A]` where `A` is an
  *     abstract type whose ONLY `Type` instance would be the val itself cannot work — it is now reported as a missing
  *     `Type[A]` instead of an infinite loop.
  *   - '''`Type.of[A]` requires a parameterless given:''' `scala.quoted.Type.of[A]` and `'{ ... }: Expr[A]` ignore
  *     `Type[A]` values that require implicit resolution to obtain. Only `implicit val`/parameterless `given`
  *     definitions are picked up. The plugin uses a best-effort approach to detect such cases and create local
  *     `given`s, but passing `Type[A]` as a context bound on a `def` is more reliable.
  *   - '''`val` vs `def` for `Expr` values — Quotes scope capture:''' `ensureQuotes` injects
  *     `given scala.quoted.Quotes = CrossQuotes.ctx` as a '''val''' (evaluated once). If a user stores an `Expr`
  *     produced by a Hearth utility (e.g. `LambdaBuilder.build`, `ValDefBuilder.build`) in a `val`, the `Expr` is bound
  *     to the `Quotes` scope active at that point. Later, when that `Expr` is spliced inside `Expr.quote` (which enters
  *     a nested `Quotes` via `nestedCtx`), the staging system detects a scope mismatch ("wrong staging level"). Using
  *     `def` instead re-evaluates the builder each time it is referenced, picking up the current dynamic `Quotes`
  *     context. This is especially subtle when combined with `MIO`, because `MIO` is naturally lazy and non-memoizing —
  *     wrapping `Expr`-producing code in `MIO` works correctly, but extracting the result into a `val` (e.g.
  *     `val lambda = mio.runSync`) re-introduces the scope capture problem.
  *     {{{
  *     // BAD — lambda is captured at Q0 scope:
  *     val lambda = LambdaBuilder.of1[A, B].map(...).build
  *     Expr.quote { Expr.splice(list).map(Expr.splice(lambda)) } // fails: wrong staging level
  *
  *     // GOOD — lambda is re-evaluated at the correct scope:
  *     def lambda = LambdaBuilder.of1[A, B].map(...).build
  *     Expr.quote { Expr.splice(list).map(Expr.splice(lambda)) } // works
  *     }}}
  *   - '''Mixing native quotes with Cross Quotes is undefined behavior:''' Using `scala.quoted.Type.of` directly
  *     alongside Cross Quotes' `Type.of` will likely crash the compiler. The plugin rewrites based on untyped tree
  *     patterns and cannot distinguish between Cross Quotes and native quotes.
  */
final class CrossQuotesPhase(loggingEnabled: (Option[JFile], Int, Int) => Boolean) extends PluginPhase {
  override def runsAfter: Set[String] = Set(Parser.name)
  override def runsBefore: Set[String] = Set("typer")
  override def phaseName: String = "hearth:cross-quotes"

  override def changesMembers: Boolean = true

  override def run(using Context): Unit = {

    inline def log(inline message: String, inline src: SourcePosition): Unit =
      if loggingEnabled(src.source.jfile.toScala, src.startLine + 1, src.startColumn + 1) then {
        // println(s"Logging: $message at ${src.source.path}:${src.startLine + 1}:${src.startColumn + 1}")
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
      // [hearth#285] Names of implicit val/def definitions whose RHS is currently being transformed.
      // Their `casted$name` given candidates must NOT be injected inside their own RHS — the generated given
      // would summon the very value being initialized (forward-reference error in blocks, "Infinite loop in
      // function body" / null self-assignment in class bodies). With the candidate excluded,
      // `scala.quoted.Type.of[A]` / `Type.CtorN.Bounded.Impl` materialize the type directly instead.
      private var selfDefNames = Set.empty[String]
      // [hearth#316] Names of the sibling implicit `Type[_]`/`Type.CtorN[_]` val/def definitions declared in the
      // enclosing TEMPLATE (class/trait/object body). Unlike a block, template members are mutually visible with no
      // statement ordering, so injecting one sibling's `casted$name` given into another sibling's RHS forces it
      // (via the `hearth.fp.ignore` suppression), and its RHS forces back — a mutual lazy-val init deadlock (hangs
      // on the lazy-val CountDownLatch, no diagnostic). While initializing ANY such definition's RHS we therefore
      // exclude ALL siblings (see the ValDef/DefDef cases): a concrete `Type.of[T]` RHS never legitimately needs a
      // sibling `Type` given, because concrete types are materialized directly. Abstract-type givens come from
      // context bounds / implicit params (`boundGivenCandidates`), which are NOT in this set and stay available.
      private var enclosingTemplateTypeDefNames = Set.empty[String]

      private val ctorSize = (1 to 22).map(i => s"Ctor$i" -> i).toMap
      private val isCtor = ctorSize.keySet
      private val maxUpper = untpd.Select(untpd.Ident(termName("scala")), typeName("Any"))
      private val minLower = untpd.Select(untpd.Ident(termName("scala")), typeName("Nothing"))

      // One day we'll use Symbol's annotations but for now we'll use this list.
      private val ImportedCrossTypeImplicit = Set(
        "Underlying", // from Existential
        "Returned", // from Method.NoInstance && Method.OfInstance
        "Instance", // from Method.OfInstance
        "Result", // from CtorLike
        "CtorResult", // from IsCollectionOf
        "Key", // from IsMapOf
        "Value", // from IsMapOf
        "LeftValue", // from IsEither
        "RightValue" // from IsEither
      )

      extension (dd: untpd.DefDef) {
        private def isImplicit: Boolean = dd.mods.flags.is(Flags.Implicit) || dd.mods.flags.is(Flags.Given)
      }
      extension (vd: untpd.ValDef) {
        private def isImplicit: Boolean = vd.mods.flags.is(Flags.Implicit) || vd.mods.flags.is(Flags.Given)
      }
      extension (selector: untpd.ImportSelector) {
        // Should handle all imported values defined as:
        //   @ImportedCrossTypeImplicit
        //   implicit val SomeName: Type[SomeName]
        // and used as:
        //   import someValue.SomeName // potentially SomeName as SomeOtherName
        private def isImportedCrossTypeImplicit: Boolean =
          ImportedCrossTypeImplicit(selector.imported.show)
      }

      // Test if TypeDef has a context bound like [A: Type], defending against changes across Scala versions.
      private object CtxBoundsTypeTree {

        def unapply(tree: untpd.Tree): Option[(Name, Name)] = tree match {
          // Scala 3.3 representation
          case AppliedTypeTree(Ident(tpe), List(Ident(name))) => Some((tpe, name))
          // Scala 3.7 representation
          //   untpd.ContextBoundTypeTree(Ident(tpe), name: TypeName, _: TermName)
          // using reflection to keep compiling on Scala 3.3
          case contextBoundTypeTree
              if contextBoundTypeTree.productPrefix == "ContextBoundTypeTree" &&
                contextBoundTypeTree.productArity == 3 &&
                contextBoundTypeTree.productElement(0).isInstanceOf[Ident] &&
                contextBoundTypeTree.productElement(1).isInstanceOf[TypeName] &&
                contextBoundTypeTree.productElement(2).isInstanceOf[TermName] =>
            val Ident(tpe) = contextBoundTypeTree.productElement(0).asInstanceOf[Ident]
            val name = contextBoundTypeTree.productElement(1).asInstanceOf[Name]
            Some((tpe, name))
          case _ => None
        }
      }

      // Test if TypeDef has a context bound like [F[_]: Type.Ctor1], defending against changes across Scala versions.
      private object CtxBoundsCtorTypeTree {

        def unapply(tree: untpd.Tree): Option[(Name, Name, Option[TermName])] = tree match {
          // Scala 3.3 representation: AppliedTypeTree(Select(Ident("Type"), "Ctor1"), List(Ident("F")))
          case AppliedTypeTree(Select(Ident(tpeName), ctorName), List(Ident(hktName)))
              if tpeName.show == "Type" && isCtor(ctorName.show) =>
            Some((ctorName, hktName, None))
          // Scala 3.7 representation: ContextBoundTypeTree(Select(Ident("Type"), "Ctor1"), F, evidence$N)
          // using reflection to keep compiling on Scala 3.3
          case contextBoundTypeTree
              if contextBoundTypeTree.productPrefix == "ContextBoundTypeTree" &&
                contextBoundTypeTree.productArity == 3 &&
                contextBoundTypeTree.productElement(0).isInstanceOf[Select] &&
                contextBoundTypeTree.productElement(1).isInstanceOf[TypeName] &&
                contextBoundTypeTree.productElement(2).isInstanceOf[TermName] =>
            val Select(Ident(tpeName), ctorName) =
              (contextBoundTypeTree.productElement(0).asInstanceOf[Select]: @unchecked)
            if tpeName.show == "Type" && isCtor(ctorName.show) then {
              val hktName = contextBoundTypeTree.productElement(1).asInstanceOf[Name]
              val evidenceName = contextBoundTypeTree.productElement(2).asInstanceOf[TermName]
              Some((ctorName, hktName, Some(evidenceName)))
            } else None
          case _ => None
        }
      }

      // Helper to extract Type[A] from AppliedTypeTree
      private object TypeAppliedTypeTree {
        def unapply(tree: untpd.Tree): Option[untpd.Tree] = tree match {
          case AppliedTypeTree(Ident(tpeName), List(innerType)) if tpeName.show == "Type" =>
            Some(innerType)
          case _ => None
        }
      }

      // Helper to extract HKT from Type.CtorN[HKT] (for N = 1..22)
      private object TypeCtorAppliedTypeTree {
        private val ctorNames = (1 to 22).map(i => s"Ctor$i").toSet
        def unapply(tree: untpd.Tree): Option[untpd.Tree] = tree match {
          case AppliedTypeTree(Select(Ident(tpeName), ctorName), List(hkt))
              if tpeName.show == "Type" && ctorNames(ctorName.show) =>
            Some(hkt)
          case _ => None
        }
      }

      // Test if TypeDef has a context bound like [F[_[_]]: Type.CtorK1], defending against changes across Scala versions.
      private object CtxBoundsCtorK1TypeTree {

        def unapply(tree: untpd.Tree): Option[(Name, Option[TermName])] = tree match {
          // Scala 3.3 representation: AppliedTypeTree(Select(Ident("Type"), "CtorK1"), List(Ident("F")))
          case AppliedTypeTree(Select(Ident(tpeName), ctorK1Name), List(Ident(hktName)))
              if tpeName.show == "Type" && ctorK1Name.show == "CtorK1" =>
            Some((hktName, None))
          // Scala 3.7 representation: ContextBoundTypeTree(Select(Ident("Type"), "CtorK1"), F, evidence$N)
          // using reflection to keep compiling on Scala 3.3
          case contextBoundTypeTree
              if contextBoundTypeTree.productPrefix == "ContextBoundTypeTree" &&
                contextBoundTypeTree.productArity == 3 &&
                contextBoundTypeTree.productElement(0).isInstanceOf[Select] &&
                contextBoundTypeTree.productElement(1).isInstanceOf[TypeName] &&
                contextBoundTypeTree.productElement(2).isInstanceOf[TermName] =>
            val Select(Ident(tpeName), ctorK1Name) =
              (contextBoundTypeTree.productElement(0).asInstanceOf[Select]: @unchecked)
            if tpeName.show == "Type" && ctorK1Name.show == "CtorK1" then {
              val hktName = contextBoundTypeTree.productElement(1).asInstanceOf[Name]
              val evidenceName = contextBoundTypeTree.productElement(2).asInstanceOf[TermName]
              Some((hktName, Some(evidenceName)))
            } else None
          case _ => None
        }
      }

      // Helper to extract HKT from Type.CtorK1[HKT]
      private object TypeCtorK1AppliedTypeTree {
        def unapply(tree: untpd.Tree): Option[untpd.Tree] = tree match {
          case AppliedTypeTree(Select(Ident(tpeName), ctorK1Name), List(hkt))
              if tpeName.show == "Type" && ctorK1Name.show == "CtorK1" =>
            Some(hkt)
          case _ => None
        }
      }

      // given casted$name: scala.quoted.Type[$tpe] =
      //   CrossQuotes.untypedToQuotedType($valueRef.asUntyped).asInstanceOf[scala.quoted.Type[$tpe]]
      private def makeGivenFromUntyped(name: String, tpe: untpd.Tree, valueRef: untpd.Tree): untpd.ValDef =
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
                untpd.Apply(
                  untpd.Select(untpd.Ident(termName("CrossQuotes")), termName("untypedToQuotedType")),
                  List(
                    untpd.Select(valueRef, termName("asUntyped"))
                  )
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

      // given casted$name: scala.quoted.Type[$tpe] = Type[$tpe].asInstanceOf[scala.quoted.Type[$tpe]]
      private def makeGiven(name: String, tpe: untpd.Tree): untpd.ValDef =
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

      // [hearth#285] Does an implicit val/def with this declared type produce a `casted$name` given candidate
      // (see blockGivenCandidates)? Used to exclude that candidate while transforming the definition itself.
      private def producesSelfGivenCandidate(tpt: untpd.Tree): Boolean =
        TypeAppliedTypeTree.unapply(tpt).isDefined ||
          TypeCtorAppliedTypeTree.unapply(tpt).isDefined ||
          TypeCtorK1AppliedTypeTree.unapply(tpt).isDefined

      // [hearth#316] Names of the implicit `Type[_]`/`Type.CtorN[_]` val/(parameterless) def members of a template
      // body — i.e. the members that produce mutually-visible `casted$name` given candidates and could deadlock each
      // other during initialization. See `enclosingTemplateTypeDefNames`.
      private def templateTypeDefNames(body: List[untpd.Tree]): Set[String] = body.collect {
        case vd: untpd.ValDef if vd.isImplicit && producesSelfGivenCandidate(vd.tpt) => vd.name.show
        case dd @ untpd.DefDef(_, paramss, returnTpe, _)
            if dd.isImplicit && paramss.flatten.isEmpty && producesSelfGivenCandidate(returnTpe) =>
          dd.name.show
      }.toSet

      private def blockGivenCandidates(trees: List[untpd.Tree]): List[untpd.ValDef] = trees.collect {
        // Handle imports with @ImportedCrossTypeImplicit (e.g. Underlying, Result, etc.)
        // Uses makeGivenFromUntyped which works for both Type[X] and Type.CtorN[X] values
        case Import(expr, selectors) if selectors.exists(_.isImportedCrossTypeImplicit) =>
          selectors.collect {
            case selector @ untpd.ImportSelector(Ident(underlying), rename, _)
                if selector.isImportedCrossTypeImplicit =>
              val name = rename.match {
                case Ident(name) => name
                case _           => underlying
              }.show
              val tpe = rename match {
                case Ident(name) => untpd.Ident(typeName(name.show))
                case _           => untpd.Select(expr, typeName(underlying.show))
              }
              val valueRef = rename match {
                case Ident(name) => untpd.Ident(termName(name.show))
                case _           => untpd.Select(expr, termName(underlying.show))
              }

              makeGivenFromUntyped(name, tpe, valueRef)
          }

        // Handle implicit val someName: Type[SomeType] = ... or given someName: Type[SomeType] = ...
        case vd: untpd.ValDef if vd.isImplicit =>
          TypeAppliedTypeTree
            .unapply(vd.tpt)
            .map { innerType =>
              val name = vd.name.show
              makeGiven(name, innerType)
            }
            .orElse(
              // Handle implicit val someName: Type.CtorN[HKT] = ...
              TypeCtorAppliedTypeTree.unapply(vd.tpt).map { hkt =>
                val name = vd.name.show
                makeGivenFromUntyped(name, hkt, untpd.Ident(termName(name)))
              }
            )
            .orElse(
              // Handle implicit val someName: Type.CtorK1[HKT] = ...
              TypeCtorK1AppliedTypeTree.unapply(vd.tpt).map { hkt =>
                val name = vd.name.show
                makeGivenFromUntyped(name, hkt, untpd.Ident(termName(name)))
              }
            )

        // Handle implicit def someName2: Type[SomeType2] = ... (no type parameters) or given someName2: Type[SomeType2] = ...
        case dd @ untpd.DefDef(methodName, paramss, returnTpe, _) if dd.isImplicit && paramss.flatten.isEmpty =>
          TypeAppliedTypeTree
            .unapply(returnTpe)
            .map { innerType =>
              val name = methodName.show
              makeGiven(name, innerType)
            }
            .orElse(
              // Handle implicit def someName: Type.CtorN[HKT] = ...
              TypeCtorAppliedTypeTree.unapply(returnTpe).map { hkt =>
                val name = methodName.show
                makeGivenFromUntyped(name, hkt, untpd.Ident(termName(name)))
              }
            )
            .orElse(
              // Handle implicit def someName: Type.CtorK1[HKT] = ...
              TypeCtorK1AppliedTypeTree.unapply(returnTpe).map { hkt =>
                val name = methodName.show
                makeGivenFromUntyped(name, hkt, untpd.Ident(termName(name)))
              }
            )
      }.flatten

      private def boundGivenCandidates(paramss: List[List[untpd.ValDef | untpd.TypeDef]]): List[untpd.ValDef] =
        paramss.flatten
          .flatMap {
            /* If parameters is
             *   [A: Type]
             * or
             *   [A: Ctx] // maybe there is some Ctx[A] => Type[A]
             * then the name of the parameter is A and the type inside Type[_] type is also A, which we can use to distinguish such bound.
             * Then we can injects a given for A:
             *   given castedA: scala.quoted.Type[A] = Type[A].asInstanceOf[scala.quoted.Type[A]]
             */
            case TypeDef(name, untpd.ContextBounds(_, List(CtxBoundsTypeTree(_ /*tpe*/, name2))))
                if /* tpe.show == "Type" && */ name.show == name2.show =>
              Some(makeGiven(name.mangledString, untpd.Ident(name)))

            // [F[_]: Type.Ctor1] context bound (Scala 3.7 with evidence)
            case TypeDef(name, untpd.ContextBounds(_, List(CtxBoundsCtorTypeTree(_ /*ctorName*/, name2, evidenceOpt))))
                if name.show == name2.show =>
              evidenceOpt match {
                case Some(evidenceName) =>
                  Some(makeGivenFromUntyped(name.mangledString, untpd.Ident(name), untpd.Ident(evidenceName)))
                case None =>
                  // Scala 3.3: the evidence is a desugared ValDef, handled in the ValDef case below
                  None
              }

            // [F[_[_]]: Type.CtorK1] context bound (Scala 3.7 with evidence)
            case TypeDef(name, untpd.ContextBounds(_, List(CtxBoundsCtorK1TypeTree(name2, evidenceOpt))))
                if name.show == name2.show =>
              evidenceOpt match {
                case Some(evidenceName) =>
                  Some(makeGivenFromUntyped(name.mangledString, untpd.Ident(name), untpd.Ident(evidenceName)))
                case None =>
                  None
              }

            /* If parameters is
             *   [A](implicit a: Type[A])
             * or
             *   [A](using a: Type[A])
             * then we extract the inner type A from Type[A] and inject a given for it:
             *   given castedA: scala.quoted.Type[A] = Type[A].asInstanceOf[scala.quoted.Type[A]]
             */
            case vd: untpd.ValDef if vd.isImplicit =>
              TypeAppliedTypeTree
                .unapply(vd.tpt)
                .map { innerType =>
                  // Use the parameter name for the given name, but reference the inner type
                  val paramName = vd.name.show
                  makeGiven(paramName, innerType)
                }
                .orElse(
                  // Handle (implicit FC: Type.CtorN[F]) parameter
                  TypeCtorAppliedTypeTree.unapply(vd.tpt).map { hkt =>
                    val name = vd.name.show
                    makeGivenFromUntyped(name, hkt, untpd.Ident(termName(name)))
                  }
                )
                .orElse(
                  // Handle (implicit FK: Type.CtorK1[F]) parameter
                  TypeCtorK1AppliedTypeTree.unapply(vd.tpt).map { hkt =>
                    val name = vd.name.show
                    makeGivenFromUntyped(name, hkt, untpd.Ident(termName(name)))
                  }
                )

            case _ => None
          }

      // [hearth#285] Drops candidates that would reference the definition whose RHS is being transformed.
      private def withoutSelfReferences(candidates: List[untpd.ValDef]): List[untpd.ValDef] =
        if selfDefNames.isEmpty then candidates
        else candidates.filterNot(vd => selfDefNames.exists(name => vd.name.show == s"casted$name"))

      private def injectGivens(thunk: => untpd.Tree): untpd.Tree = {
        val oldGivensInjected = givensInjected
        val newGivensInjected = withoutSelfReferences(givenCandidates.filterNot(givensInjected))
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

      private def replaceTypeOf(tree: untpd.Tree, innerTree: untpd.Tree, name: String): untpd.Tree = {
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
          s"""Cross-quotes $name expansion:
             |From: ${tree.show}
             |To: ${result.show}""".stripMargin,
          tree.sourcePos
        )

        result
      }

      private def replaceTypeCtorOf(
          tree: untpd.Tree,
          ctor: Name,
          hkt: untpd.Tree,
          bounds: List[untpd.Tree]
      ): untpd.Tree = {
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
              bounds ++ List(transform(hkt))
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
      }

      private def replaceExprQuote(tree: untpd.Tree, thunk: untpd.Tree): untpd.Tree = {
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
      }

      private def replaceExprSplice(tree: untpd.Tree, thunk: untpd.Tree): untpd.Tree = {
        val result = ensureQuotes(
          injectGivens(
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
        )

        log(
          s"""Cross-quotes Expr.splice expansion:
             |From: ${tree.show}
             |To: ${result.show}""".stripMargin,
          tree.sourcePos
        )

        result
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
          replaceTypeOf(tree, innerTree, "Type.of")

        /* Replaces:
         *   Type.CtorK1.of[HKT]
         * with:
         *   given quotes: scala.quoted.Quotes = CrossQuotes.ctx[scala.quoted.Quotes]
         *   [manually cast every type param Type[A] to given scala.quoted.Type[A]]
         *   Type.CtorK1.Impl[HKT]
         */
        case TypeApply(Select(Select(Ident(tp), ctorK1), of), List(hkt))
            if tp.show == "Type" && ctorK1.show == "CtorK1" && of.show == "of" =>
          val result = ensureQuotes(
            injectGivens(
              untpd.TypeApply(
                untpd.Select(
                  untpd.Select(untpd.Ident(termName("Type")), termName("CtorK1")),
                  termName("Impl")
                ),
                List(transform(hkt))
              )
            )
          )

          log(
            s"""Cross-quotes Type.CtorK1.of expansion:
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
         *   Type.Ctor1.Bounded.Impl[Nothing, Any, HKT]
         *   Type.Ctor2.Bounded.Impl[Nothing, Any, Nothing, Any, HKT1, HKT2]
         *   ...
         *   Type.Ctor22.Bounded.Impl[Nothing, Any, Nothing, Any, ..., Nothing, Any, HKT1, HKT2, ..., HKT22]
         */
        case TypeApply(prefix @ Select(Select(Ident(tp), ctor), of), List(hkt))
            if tp.show == "Type" && isCtor(ctor.show) && of.show == "of" =>
          replaceTypeCtorOf(tree, ctor, hkt, (1 to ctorSize(ctor.show)).flatMap(_ => List(minLower, maxUpper)).toList)

        /* Replaces:
         *   Type.Ctor1.UpperBounded.of[U1, HKT]
         *   Type.Ctor2.UpperBounded.of[U1, U2, HKT]
         *   ...
         *   Type.Ctor22.UpperBounded.of[U1, U2, ..., U22, HKT]
         * with:
         *   given quotes: scala.quoted.Quotes = CrossQuotes.ctx[scala.quoted.Quotes]
         *   [manually cast every type param Type[A] to given scala.quoted.Type[A]]
         *   Type.Ctor1.Bounded.Impl[Nothing, U1, HKT]
         *   Type.Ctor2.Bounded.Impl[Nothing, U1, Nothing, U2, HKT]
         *   ...
         *   Type.Ctor22.Bounded.Impl[Nothing, U1, Nothing, U2, ..., Nothing, U22, HKT]
         */
        // [hearth#344] `UpperBounded.of` is a 3-select (`Type.CtorN.UpperBounded.of`), not the 2-select `.of` above.
        case TypeApply(Select(Select(Select(Ident(tp), ctor), upperBounded), of), upper :+ hkt)
            if tp.show == "Type" && isCtor(ctor.show) && upperBounded.show == "UpperBounded" && of.show == "of" =>
          replaceTypeCtorOf(tree, ctor, hkt, upper.flatMap(u => List(minLower, u)))

        /* Replaces:
         *   Type.Ctor1.Bounded.of[L1, U1, HKT]
         *   Type.Ctor2.Bounded.of[L1, U1, L2, U2, HKT]
         *   ...
         *   Type.Ctor22.Bounded.of[L1, U1, L2, U2, ..., L22, U22, HKT]
         * with:
         *   given quotes: scala.quoted.Quotes = CrossQuotes.ctx[scala.quoted.Quotes]
         *   [manually cast every type param Type[A] to given scala.quoted.Type[A]]
         *   Type.Ctor1.Bounded.Impl[L1, U1, HKT]
         *   Type.Ctor2.Bounded.Impl[L1, U1, L2, U2, HKT]
         *   ...
         *   Type.Ctor22.Bounded.Impl[L1, U1, L2, U2, ..., L22, U22, HKT]
         */
        // [hearth#344] `Bounded.of` is a 3-select (`Type.CtorN.Bounded.of`), not the 2-select `.of` above.
        case TypeApply(Select(Select(Select(Ident(tp), ctor), boundedSel), of), bounded :+ hkt)
            if tp.show == "Type" && isCtor(ctor.show) && boundedSel.show == "Bounded" && of.show == "of" =>
          replaceTypeCtorOf(tree, ctor, hkt, bounded)

        /* Replaces:
         *   Expr.quote[A](thunk) // with explicit type parameter
         * with:
         *   given quotes: scala.quoted.Quotes = CrossQuotes.ctx[scala.quoted.Quotes]
         *   [manually cast every type param Type[A] to given scala.quoted.Type[A]]
         *   CrossQuotes.exprQuotesToCross('{ thunk })
         */
        case Apply(TypeApply(Select(Ident(expr), quote), _), List(thunk))
            if expr.show == "Expr" && quote.show == "quote" =>
          replaceExprQuote(tree, thunk)

        /* Replaces:
         *   Expr.quote(thunk) // with inferred type
         * with:
         *   given quotes: scala.quoted.Quotes = CrossQuotes.ctx[scala.quoted.Quotes]
         *   [manually cast every type param Type[A] to given scala.quoted.Type[A]]
         *   CrossQuotes.exprQuotesToCross('{ thunk })
         */
        case Apply(Select(Ident(expr), quote), List(thunk)) if expr.show == "Expr" && quote.show == "quote" =>
          replaceExprQuote(tree, thunk)

        /* Replaces:
         *   Expr.splice[A](expr) // with explicit type parameter
         * with:
         *   given quotes: scala.quoted.Quotes = CrossQuotes.ctx[scala.quoted.Quotes]
         *   [manually cast every type param Type[A] to given scala.quoted.Type[A]]
         *   ${
         *     CrossQuotes.nestedCtx { // <- necessary to handle q.Nested cases
         *       CrossQuotes.exprCrossToQuotes(expr)
         *     }
         *   }
         */
        case Apply(TypeApply(Select(Ident(expr), splice), _), List(thunk))
            if expr.show == "Expr" && splice.show == "splice" =>
          replaceExprSplice(tree, thunk)

        /* Replaces:
         *   Expr.splice(expr) // with inferred type
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
          replaceExprSplice(tree, thunk)

        /* [hearth#285] An implicit val/given whose declared type is Type[A]/Type.CtorN[HKT]/Type.CtorK1[HKT] is
         * itself a given candidate (see blockGivenCandidates). While transforming ITS OWN definition (in
         * particular its RHS, e.g. `implicit val ConfigT: Type[Configuration] = Type.of[Configuration]`), that
         * candidate must not be injected — the generated given would summon the value being initialized.
         */
        case vd: untpd.ValDef if vd.isImplicit && producesSelfGivenCandidate(vd.tpt) =>
          val oldSelfDefNames = selfDefNames
          try {
            // [hearth#316] Exclude self AND every sibling template-level implicit Type member: injecting a sibling's
            // casted given here would force it (and it would force back) — a mutual lazy-val init deadlock.
            selfDefNames = oldSelfDefNames + vd.name.show ++ enclosingTemplateTypeDefNames
            super.transform(vd)
          } finally
            selfDefNames = oldSelfDefNames

        /* Looking for blocks with imports that contain Underlying, or implicit val/def/given returning Type[A].
         * For each such import we inject:
         *   given castedUnderlying: scala.quoted.Type[Underlying] =
         *     Type[Underlying].asInstanceOf[scala.quoted.Type[Underlying]]
         * For each implicit val/def/given returning Type[A] we inject:
         *   given casted$name: scala.quoted.Type[A] = Type[A].asInstanceOf[scala.quoted.Type[A]]
         */
        case block: Block =>
          val oldGivenCandidates = givenCandidates
          try {
            // Imports and (implicit) defs can legally be forward-referenced from earlier statements in a block,
            // so their candidates are visible to every expansion in the block.
            givenCandidates = oldGivenCandidates ++ blockGivenCandidates(
              block.stats.filterNot(_.isInstanceOf[untpd.ValDef])
            )
            // [hearth#285] Candidates from implicit vals become visible only AFTER their defining statement —
            // injecting them into earlier expansions would generate an illegal forward reference to the val.
            val newStats = block.stats.map { stat =>
              val transformed = transform(stat)
              stat match {
                case vd: untpd.ValDef => givenCandidates = givenCandidates ++ blockGivenCandidates(vd :: Nil)
                case _                => ()
              }
              transformed
            }
            val newExpr = transform(block.expr)
            cpy.Block(block)(newStats, newExpr)
          } finally
            givenCandidates = oldGivenCandidates

        /* Looking for classes' bodies with imports that contain Underlying, or implicit val/def/given returning Type[A].
         * For each such import we inject:
         *   given castedUnderlying: scala.quoted.Type[Underlying] =
         *     Type[Underlying].asInstanceOf[scala.quoted.Type[Underlying]]
         * For each implicit val/def/given returning Type[A] we inject:
         *   given casted$name: scala.quoted.Type[A] = Type[A].asInstanceOf[scala.quoted.Type[A]]
         */
        case tpl: Template =>
          val newGivenCandidates = blockGivenCandidates(tpl.body)
          val oldGivenCandidates = givenCandidates
          val oldEnclosingTemplateTypeDefNames = enclosingTemplateTypeDefNames
          try {
            givenCandidates = oldGivenCandidates ++ newGivenCandidates
            // [hearth#316] Record this template's mutually-visible implicit Type members so that each one's RHS can
            // exclude its siblings (avoiding the mutual lazy-val init deadlock). Templates nest, so accumulate.
            enclosingTemplateTypeDefNames = oldEnclosingTemplateTypeDefNames ++ templateTypeDefNames(tpl.body)
            super.transform(tpl)
          } finally {
            givenCandidates = oldGivenCandidates
            enclosingTemplateTypeDefNames = oldEnclosingTemplateTypeDefNames
          }

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

          val newGivenCandidates = boundGivenCandidates(paramss)

          // Uncomment to debug missing givens:
          // if loggingEnabled && newGivenCandidates.isEmpty && dd.toString.contains("Type") then {
          //   println(s"""missed givens for:
          //              |$dd
          //              |paramss:
          //              |${paramss.flatten.mkString("\n")}
          //              |""".stripMargin)
          // }
          // when found, handle new cases in CtxBoundsTypeTree.

          // [hearth#285] A parameterless implicit def returning Type[A]/Type.CtorN[HKT]/Type.CtorK1[HKT] is itself
          // a given candidate — exclude it while transforming its own body (calling it there would recurse forever).
          val selfName =
            if dd.isImplicit && paramss.flatten.isEmpty && producesSelfGivenCandidate(returnTpe)
            then Some(methodName.show)
            else None

          val oldGivenCandidates = givenCandidates
          val oldSelfDefNames = selfDefNames
          try {
            givenCandidates = oldGivenCandidates ++ newGivenCandidates
            // [hearth#316] For an implicit parameterless Type def (selfName defined), also exclude sibling
            // template-level implicit Type members to avoid the mutual init deadlock. Regular methods keep all
            // sibling givens available (their bodies legitimately summon sibling Type[_]).
            selfDefNames =
              oldSelfDefNames ++ selfName ++ (if selfName.isDefined then enclosingTemplateTypeDefNames else Set.empty)
            // untpd.DefDef(methodName, paramss, returnTpe, transform(body)).withMods(dd.mods)
            super.transform(dd)
          } finally {
            givenCandidates = oldGivenCandidates
            selfDefNames = oldSelfDefNames
          }

        case t =>
          super.transform(t)
      }
    }.transform(ctx.compilationUnit.untpdTree)
    super.run
  }
}
