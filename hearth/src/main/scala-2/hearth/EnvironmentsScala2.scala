package hearth

trait EnvironmentsScala2 extends Environments { this: MacroCommonsScala2 =>

  final override type Position = c.universe.Position

  object Position extends PositionModule {
    override def current: Position = c.enclosingPosition

    override def file(pos: Position): Option[java.nio.file.Path] =
      scala.util.Try(new java.io.File(pos.source.path).toPath).toOption
    override def offset(pos: Position): Int = pos.start - macroPositionCorrection(pos)
    override def line(pos: Position): Int = pos.line
    override def column(pos: Position): Int = pos.column - macroPositionCorrection(pos)

    // NOTE: slicing uses the RAW pos.start/pos.end (not macroPositionCorrection, which only adjusts reporting
    // offsets) so we extract the exact source characters that the position spans.
    override def sourceCode(pos: Position): Option[String] = scala.util
      .Try {
        if (pos == c.universe.NoPosition) None
        else {
          val content = pos.source.content
          if (pos.start >= 0 && pos.start <= pos.end && pos.end <= content.length)
            Some(new String(content.slice(pos.start, pos.end)))
          else None
        }
      }
      .toOption
      .flatten

    /** So, apparently when we are expanding a macro, the position of the macro expansion is computed differently for
      * macro-methods that have no parameter lists and for those that have:
      *
      * {{{
      * MyType.noParamMacro
      * //     ^ macro expansion is counted from this place
      * MyType.nullaryMacro()
      * //                 ^ macro expansion is counted from this place
      * MyType.paramMacro(arg)
      * //               ^ macro expansion is counted from this place
      * }}}
      *
      * It means that the column/offset has to be adjusted by the macro-method's name length.
      */
    private def macroPositionCorrection(pos: Position) =
      if (pos.source == current.source && pos.start == current.start) macroPossitionCorrectonValue else 0
    private lazy val macroPossitionCorrectonValue: Int = {
      val currentMacro = c.macroApplication.symbol.asMethod
      if (currentMacro.paramLists.isEmpty) 0
      else currentMacro.name.decodedName.toString.length
    }
  }

  override lazy val enclosingScope: hearth.fp.data.NonEmptyVector[Enclosure] = {
    import c.universe.{Position as _, *}

    def positionOf(symbol: Symbol): Option[Position] =
      Option(symbol.pos)
        .filter(_ != NoPosition)
        .filter(pos => scala.util.Try(pos.start).isSuccess)

    def decodedName(symbol: Symbol): String = symbol.name.decodedName.toString.trim

    // Local `val`s declared inside the immediately-enclosing method body that are in scope at the macro-expansion
    // point (declared textually BEFORE the macro call). This is the literal macwire case. `c.enclosingMethod` is
    // deprecated but is the only way on Scala 2 to reach the enclosing `DefDef` body tree.
    @scala.annotation.nowarn("cat=deprecation")
    def localValuesOf(methodSymbol: Symbol): List[Enclosure.LocalValue] = scala.util
      .Try {
        val macroStart = c.macroApplication.pos.start
        c.enclosingMethod match {
          case dd: DefDef if dd.symbol == methodSymbol =>
            val stmts: List[Tree] = dd.rhs match {
              case Block(s, _) => s
              case _           => Nil
            }
            stmts.flatMap {
              case vd: ValDef =>
                val endsBefore = scala.util.Try(vd.pos.end <= macroStart).getOrElse(false)
                if (!endsBefore) None
                else
                  scala.util.Try {
                    val sym = vd.symbol
                    val tpe: UntypedType =
                      if (sym != null && sym != NoSymbol && sym.typeSignature != NoType) sym.typeSignature
                      else vd.tpt.tpe
                    val ev = UntypedType.as_??(tpe)
                    val refTree: c.Tree = c.universe.internal.gen.mkAttributedRef(sym)
                    Enclosure.LocalValue(decodedName(sym), ev, UntypedExpr.as_??(refTree, tpe))
                  }.toOption
              case _ => None
            }
          case _ => Nil
        }
      }
      .toOption
      .getOrElse(Nil)

    def isHiddenOwner(symbol: Symbol): Boolean = {
      val n = decodedName(symbol)
      n == "<root>" || n == "<empty>" || n == "<none>" ||
      n.startsWith("$anonfun") || n.contains("$anon") ||
      // synthetic accessor/macro glue methods
      (symbol.isMethod && symbol.isSynthetic)
    }

    def toEnclosure(symbol: Symbol): Option[Enclosure] =
      if (symbol == NoSymbol || isHiddenOwner(symbol)) None
      else if (symbol.isPackage || symbol.isPackageClass)
        Some(Enclosure.Package(decodedName(symbol), Some(symbol.fullName), positionOf(symbol)))
      else if (symbol.isModule || symbol.isModuleClass) {
        val moduleSym = if (symbol.isModuleClass) symbol.asClass.module else symbol
        val tpe: UntypedType = symbol.asClass.toType
        val ev = UntypedType.as_??(tpe)
        import ev.Underlying
        val members = UntypedMethod.methods(tpe).map(_.asTyped[ev.Underlying])
        val refTree: c.Tree = c.universe.internal.gen.mkAttributedRef(moduleSym)
        Some(
          Enclosure.Object(
            decodedName(symbol),
            Some(symbol.fullName),
            positionOf(symbol),
            ev,
            members,
            UntypedExpr.as_??(refTree)
          )
        )
      } else if (symbol.isClass) {
        val classTpe: UntypedType = symbol.asClass.toType
        // A trait/class may declare a self-type requirement (`this: Provider =>`); its members are visible on `this`
        // but absent from `toType`. Fold the self-type in (as an intersection) so the enclosing scope exposes them —
        // wiring/DI draws candidates from `Enclosure.Class.members`. `typeOfThis` equals `toType` when there is no
        // self-type, so non-self-typed classes are unaffected.
        // `typeOfThis` (the type of `this`, including any self-type requirement) is only on the internal Symbol API.
        val selfTpe: UntypedType =
          symbol.asInstanceOf[scala.reflect.internal.Symbols#Symbol].typeOfThis.asInstanceOf[c.universe.Type]
        val tpe: UntypedType =
          if (selfTpe <:< classTpe) selfTpe
          else c.internal.intersectionType(List(classTpe, selfTpe))
        val ev = UntypedType.as_??(tpe)
        import ev.Underlying
        val members = UntypedMethod.methods(tpe).map(_.asTyped[ev.Underlying])
        val thisRef = scala.util.Try(UntypedExpr.as_??(c.universe.internal.gen.mkAttributedThis(symbol))).toOption
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
      } else if (symbol.isTerm && (symbol.asTerm.isVal || symbol.asTerm.isVar)) {
        val tpe: UntypedType = symbol.typeSignature
        Some(Enclosure.Value(decodedName(symbol), Some(symbol.fullName), positionOf(symbol), UntypedType.as_??(tpe)))
      } else if (symbol.isMethod)
        // Result type kept None for cross-platform parity with Scala 3 (where `Symbol.info` is @experimental).
        Some(Enclosure.Method(decodedName(symbol), Some(symbol.fullName), positionOf(symbol), None, Nil))
      else None

    var seenImmediateClass = false
    // `localValues` is populated only for the immediately-enclosing method (the macwire case); outer methods keep Nil.
    var seenImmediateMethod = false
    val enclosures = scala.collection.mutable.ArrayBuffer.empty[Enclosure]
    var current: Symbol = c.internal.enclosingOwner
    while (current != NoSymbol) {
      toEnclosure(current).foreach {
        case cls: Enclosure.Class =>
          if (seenImmediateClass) enclosures += cls.copy(thisRef = None)
          else { enclosures += cls; seenImmediateClass = true }
        case m: Enclosure.Method =>
          if (seenImmediateMethod) enclosures += m
          else { enclosures += m.copy(localValues = localValuesOf(current)); seenImmediateMethod = true }
        case other => enclosures += other
      }
      current = current.owner
    }

    hearth.fp.data.NonEmptyVector
      .fromVector(enclosures.toVector)
      .getOrElse(
        hearth.fp.data.NonEmptyVector.one(Enclosure.Package("<root>", Some("<root>"), None))
      )
  }

  object Environment extends EnvironmentModule {

    override lazy val currentScalaVersion: ScalaVersion = ScalaVersion.byScalaLibrary(c)

    override lazy val XMacroSettings: List[String] = c.settings

    override def reportInfo(msg: String): Unit = c.echo(c.enclosingPosition, msg)
    override def reportInfo(msg: String, position: Position): Unit = c.echo(position, msg)
    override def reportWarn(msg: String): Unit = c.warning(c.enclosingPosition, msg)
    override def reportWarn(msg: String, position: Position): Unit = c.warning(position, msg)
    override def reportError(msg: String): Unit = c.error(c.enclosingPosition, msg)
    override def reportError(msg: String, position: Position): Unit = c.error(position, msg)
    override def reportErrorAndAbort(msg: String): Nothing = c.abort(c.enclosingPosition, msg)
    override def reportErrorAndAbort(msg: String, position: Position): Nothing = c.abort(position, msg)
  }

  /** Do not use this module, it exists only to be used by Scala 2 Cross-Quotes macros and Scala 3 Cross-Quotes compiler
    * plugin.
    *
    * Here live dragons.
    */
  object CrossQuotes extends CrossQuotesModule {

    override def ctx[CastAs]: CastAs = c.asInstanceOf[CastAs]

    // Scala 2 has a single macro Context: there is no splice scoping, so pinning the entry context is a no-op.
    override private[hearth] def withMacroEntryContext[A](thunk: => A): A = thunk

    override private[hearth] def macroEntryContextKey: AnyRef = c
  }
}
