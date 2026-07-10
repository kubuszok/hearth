package hearth

trait EnvironmentsScala3 extends Environments { this: MacroCommonsScala3 =>

  import quotes.*, quotes.reflect.*

  final override type Position = quotes.reflect.Position

  object Position extends PositionModule {

    override def current: Position = quotes.reflect.Position.ofMacroExpansion

    override def file(pos: Position): Option[java.nio.file.Path] = pos.sourceFile.getJPath
    override def offset(pos: Position): Int = pos.start
    override def line(pos: Position): Int = pos.startLine + 1 // for some reason, the line number is 0-based in Scala 3
    override def column(pos: Position): Int = pos.startColumn + 1 // same for the column number

    // We slice the SourceFile content by [start, end) rather than calling `pos.sourceCode` directly, because
    // `pos.sourceCode` (a quotes.reflect extension) would clash with our own `PositionMethods.sourceCode` implicit
    // class (in scope here) and resolve to it, causing infinite recursion.
    override def sourceCode(pos: Position): Option[String] = scala.util
      .Try {
        pos.sourceFile.content.flatMap { content =>
          val start = pos.start
          val end = pos.end
          if start >= 0 && start <= end && end <= content.length then Some(content.substring(start, end))
          else None
        }
      }
      .toOption
      .flatten
  }

  override lazy val enclosingScope: hearth.fp.data.NonEmptyVector[Enclosure] = {
    // NOTE: we intentionally do NOT call `pos.sourceFile` here. Owner symbols in user code do not carry `.sig`
    // positions, so the plain `symbol.pos` is sufficient.
    def positionOf(symbol: Symbol): Option[Position] = symbol.pos

    // `Symbol.tree` is @experimental on Scala 3.3.x. We reach it through Java reflection on `quotes.reflect`
    // (`SymbolMethods.tree(symbol)`), the same bypass `UntypedTypesScala3` uses for `TypeReprMethods`.
    def symbolTree(symbol: Symbol): Option[Tree] = scala.util.Try {
      val q = quotes
      val reflectModule = q.getClass.getMethod("reflect").invoke(q)
      val symbolMethods = reflectModule.getClass.getMethod("SymbolMethods").invoke(reflectModule)
      val method = symbolMethods.getClass.getMethod("tree", classOf[Object])
      method.invoke(symbolMethods, symbol).asInstanceOf[Tree]
    }.toOption

    // Local `val`s declared inside the immediately-enclosing method body that are in scope at the macro-expansion
    // point (declared textually BEFORE the macro call). This is the literal macwire case.
    //
    // EMPIRICAL FINDING (Scala 3.3.x / 3.8.x): `Symbol.tree` DOES return a `DefDef` for the enclosing method during
    // its own expansion, BUT that `DefDef.rhs` is `None` - the method body is not yet available while the method is
    // being compiled. So the code below correctly yields `Nil` on Scala 3 today. It is written defensively (rather
    // than hard-coding `Nil`) so it will start working automatically should a future Scala version expose the body.
    // See `Enclosure.LocalValue` scaladoc for the platform asymmetry. Scala 2 (`c.enclosingMethod`) does work.
    def localValuesOf(methodSymbol: Symbol): List[Enclosure.LocalValue] = scala.util
      .Try {
        val macroStart = quotes.reflect.Position.ofMacroExpansion.start
        symbolTree(methodSymbol) match {
          case Some(dd: DefDef) =>
            val stmts: List[Statement] = dd.rhs match {
              case Some(Block(s, _)) => s
              case _                 => Nil
            }
            stmts.flatMap {
              case vd: ValDef =>
                // Only definitions that end before the macro call are in scope at the call site.
                val endsBefore = scala.util.Try(vd.pos.end <= macroStart).getOrElse(false)
                if !endsBefore then None
                else
                  scala.util.Try {
                    val tpe = vd.tpt.tpe
                    val ev = UntypedType.as_??(tpe)
                    val ref = UntypedExpr.as_??(Ref(vd.symbol): Term, tpe)
                    Enclosure.LocalValue(vd.name.stripSuffix("$").trim, ev, ref)
                  }.toOption
              case _ => None
            }
          case _ => Nil
        }
      }
      .toOption
      .getOrElse(Nil)

    def decodedName(symbol: Symbol): String = symbol.name.stripSuffix("$").trim

    // Owners we never want to surface to the user (compiler-internal wrappers).
    def isHiddenOwner(symbol: Symbol): Boolean = {
      val n = symbol.name
      n == "<root>" || n == "<empty>" || n == "<none>" ||
      // The `inline def`/`${ }` splice expansion glue is a synthetic owner literally named "macro".
      n == "macro" ||
      // synthetic anonymous functions / by-name wrappers
      n.startsWith("$anonfun") || n.contains("$anon") ||
      // macro-expansion synthetic method wrappers (the `inline def` expansion glue)
      (symbol.isDefDef && symbol.flags.is(Flags.Synthetic) && symbol.flags.is(Flags.Macro))
    }

    def toEnclosure(symbol: Symbol): Option[Enclosure] =
      if symbol.isNoSymbol || isHiddenOwner(symbol) then None
      else if symbol.isPackageDef then Some(
        Enclosure.Package(decodedName(symbol), Some(symbol.fullName), positionOf(symbol))
      )
      else if symbol.isClassDef then {
        // A package object surfaces as a ClassDef whose name ends with `package$` - treat as Package.
        if symbol.flags.is(Flags.Package) then Some(
          Enclosure.Package(decodedName(symbol), Some(symbol.fullName), positionOf(symbol))
        )
        else {
          val tpe = symbol.typeRef
          val ev = UntypedType.as_??(tpe)
          import ev.Underlying
          val members = UntypedMethod.methods(tpe).map(_.asTyped[ev.Underlying])
          if symbol.flags.is(Flags.Module) then {
            // object/module: stable path, members callable via module Ref. On Scala 3 the owner is the module CLASS,
            // whose `companionModule` is the object's term (val) symbol that `Ref` can reference.
            val moduleSym = {
              val cm = symbol.companionModule
              if cm.isNoSymbol then symbol else cm
            }
            val refTerm: Term = Ref(moduleSym)
            Some(
              Enclosure.Object(
                decodedName(symbol),
                Some(symbol.fullName),
                positionOf(symbol),
                ev,
                members,
                UntypedExpr.as_??(refTerm)
              )
            )
          } else {
            // class/trait: `this` reference only for the immediately-enclosing class.
            val thisRef =
              scala.util.Try(UntypedExpr.as_??(This(symbol): Term)).toOption
            Some(
              Enclosure.Class(
                decodedName(symbol),
                Some(symbol.fullName),
                positionOf(symbol),
                ev,
                members,
                thisRef
              )
            )
          }
        }
      } else if symbol.isValDef then {
        val tpe = symbol.termRef.widen
        Some(Enclosure.Value(decodedName(symbol), Some(symbol.fullName), positionOf(symbol), UntypedType.as_??(tpe)))
      } else if symbol.isDefDef then
      // Result type is intentionally None: a non-experimental, cross-platform-consistent way to read a def's result
      // type from an owner Symbol alone is not available on Scala 3 (`Symbol.info` is @experimental), so we keep the
      // method enclosure's `tpe` None on both platforms for identical output.
      Some(Enclosure.Method(decodedName(symbol), Some(symbol.fullName), positionOf(symbol), None, Nil))
      else None

    // `This(symbol)` is only sound for the immediately-enclosing class - blank it out for outer ones.
    var seenImmediateClass = false
    // `localValues` is populated only for the immediately-enclosing method (the macwire case); outer methods keep Nil.
    var seenImmediateMethod = false
    val enclosures = scala.collection.mutable.ArrayBuffer.empty[Enclosure]
    var current = Symbol.spliceOwner
    while !current.isNoSymbol do {
      toEnclosure(current).foreach {
        case c: Enclosure.Class =>
          if seenImmediateClass then enclosures += c.copy(thisRef = None)
          else { enclosures += c; seenImmediateClass = true }
        case m: Enclosure.Method =>
          if seenImmediateMethod then enclosures += m
          else { enclosures += m.copy(localValues = localValuesOf(current)); seenImmediateMethod = true }
        case other => enclosures += other
      }
      current = current.maybeOwner
    }

    hearth.fp.data.NonEmptyVector
      .fromVector(enclosures.toVector)
      .getOrElse(
        hearth.fp.data.NonEmptyVector.one(Enclosure.Package("<root>", Some("<root>"), None))
      )
  }

  object Environment extends EnvironmentModule {

    override lazy val currentScalaVersion: ScalaVersion = ScalaVersion.byScalaLibrary(quotes)

    override lazy val XMacroSettings: List[String] = {
      // workaround to contain @experimental from polluting the whole codebase
      val info = quotes.reflect.CompilationInfo
      info.getClass.getMethod("XmacroSettings").invoke(info).asInstanceOf[List[String]]
    }

    override def reportInfo(msg: String): Unit = report.info(msg, currentPosition)
    override def reportInfo(msg: String, position: Position): Unit = report.info(msg, position)
    override def reportWarn(msg: String): Unit = report.warning(msg, currentPosition)
    override def reportWarn(msg: String, position: Position): Unit = report.warning(msg, position)
    override def reportError(msg: String): Unit = report.error(msg, currentPosition)
    override def reportError(msg: String, position: Position): Unit = report.error(msg, position)
    override def reportErrorAndAbort(msg: String): Nothing = report.errorAndAbort(msg, currentPosition)
    override def reportErrorAndAbort(msg: String, position: Position): Nothing = report.errorAndAbort(msg, position)
  }

  object CrossQuotes extends CrossQuotesModule {

    currentCtx = quotes

    override protected def macroEntryCtx: scala.quoted.Quotes = quotes

    override def ctx[CastAs]: CastAs = currentCtx.asInstanceOf[CastAs]

    override private[hearth] def withMacroEntryContext[A](thunk: => A): A = withMacroEntryCtx(thunk)

    override private[hearth] def macroEntryContextKey: AnyRef = macroEntryCtx
  }
}
