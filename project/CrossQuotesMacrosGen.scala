/** Generates the CrossQuotesMacrosCtorMethods trait for Scala 2.
  *
  * The generated trait uses a self-type `this: CrossQuotesMacros =>` to access
  * all utility methods from the hand-written macro bundle class.
  *
  * It defines:
  *   - `typeCtor*Body` / `typeCtor*FromUntypedBody` — arity-specific methods (1..22)
  *
  * The hand-written CrossQuotesMacros class mixes in this trait and defines thin
  * forwarder methods (`typeCtor*Impl` / `typeCtor*FromUntypedImpl`) that delegate
  * to the corresponding `*Body` methods. Macro bundle validation in Scala 2 requires
  * the `*Impl` entry points to be defined directly on the class, but the actual
  * implementations can live in the mixed-in trait.
  *
  * Each arity method decomposes its quasiquote into multiple local `def`s that each
  * build a portion of the anonymous class body. This distributes the bytecode
  * across several JVM methods, avoiding "Method too large" errors at high
  * arities (n >= 20).
  *
  * Uses [[ArityGen]] for shared naming helpers (paramName, lower, upper, etc.).
  *
  * ==Hygiene: fully-qualify every type/term used inside the emitted quasiquotes==
  *
  * Scala 2 quasiquotes are NOT hygienic. Every tree we build here is spliced back
  * into the user's macro-expansion site and typechecked in THEIR scope, so any bare
  * identifier (`List`, `Seq`, `Option`, `None`, `Any`, ...) can be captured by a
  * user definition that happens to share the name. This bites in practice because
  * users routinely stash `Type.CtorN.of[...]` results in vals named after the type
  * they hold:
  *
  * {{{
  *   val List: Type.Ctor1[List] = Type.Ctor1.of[List] // shadows scala.List
  *   val Map:  Type.Ctor2[Map]  = Type.Ctor2.of[Map]  // shadows scala.Map/Seq
  * }}}
  *
  * When the emitted `unapply` used a bare `List(...)` / `Seq(...)` extractor, it
  * resolved to the user's `Type.CtorN` value instead of the collection companion,
  * producing baffling `Nothing <:??<: Any` type-mismatch errors deep inside the
  * generated code (kubuszok/hearth#300).
  *
  * RULE: anything with a stable full path MUST be written as `_root_.scala.<X>` (or
  * `_root_.pkg.<X>`) in the emitted strings — never bare. Path-dependent members of
  * the macro bundle (`Type`, `CrossQuotes`, `<:??<:`, `as_<:??<:`) resolve through
  * the enclosing `this` and cannot be `_root_`-qualified; those stay bare by
  * necessity, but every `scala.*` reference here is fully qualified on purpose.
  * This was originally sanitized and got lost across rewrites — keep it sanitized.
  */
object CrossQuotesMacrosGen {

  private val numberWords: Map[Int, String] = Map(
    1  -> "one",   2  -> "two",    3  -> "three",   4  -> "four",    5  -> "five",
    6  -> "six",   7  -> "seven",  8  -> "eight",   9  -> "nine",    10 -> "ten",
    11 -> "eleven", 12 -> "twelve", 13 -> "thirteen", 14 -> "fourteen", 15 -> "fifteen",
    16 -> "sixteen", 17 -> "seventeen", 18 -> "eighteen", 19 -> "nineteen", 20 -> "twenty",
    21 -> "twenty-one", 22 -> "twenty-two"
  )

  /** Triple-quote for use inside s-interpolated arity method builders. */
  private val tq = "\"\"\""

  def generate(maxArity: Int = 22): String = {
    val sb = new StringBuilder
    sb ++= traitHeader
    sb ++= "\n"
    for (n <- 1 to maxArity) { sb ++= typeCtorNBody(n); sb ++= "\n" }
    for (n <- 1 to maxArity) { sb ++= typeCtorNFromUntypedBody(n); sb ++= "\n" }
    sb ++= traitFooter
    sb.toString
  }

  // ---------------------------------------------------------------------------
  // Trait header / footer
  // ---------------------------------------------------------------------------

  private val traitHeader: String =
    s"""package hearth
       |package cq
       |
       |// AUTO-GENERATED — DO NOT EDIT
       |// $$COVERAGE-OFF$$
       |// format: off
       |trait CrossQuotesMacrosCtorMethods { this: CrossQuotesMacros =>
       |
       |  import c.universe.{Expr => _, _}
       |""".stripMargin

  private val traitFooter: String = "}\n"

  // ---------------------------------------------------------------------------
  // typeCtorNBody (arity-based methods)
  // ---------------------------------------------------------------------------

  private def typeCtorNBody(n: Int): String = {
    val cn = ArityGen.ctorName(n)
    val sb = new StringBuilder

    // Comment block
    sb ++= s"  /* Replaces:\n"
    sb ++= s"   *   Type.$cn.of[HKT]\n"
    sb ++= s"   *   Type.$cn.UpperBounded.of[${upperBoundedOfArgs(n)}, HKT]\n"
    sb ++= s"   *   Type.$cn.Bounded.of[${boundedOfArgs(n)}, HKT]\n"
    sb ++= s"   * with:\n"
    sb ++= s"   *   what we see in Quasiquote\n"
    sb ++= s"   */\n"

    // Method signature
    sb ++= s"  // format: off\n"
    sb ++= s"  protected def typeCtor${n}Body[${ArityGen.boundsPairs(n)}, HKT[${ArityGen.hktSlots(n)}]](implicit\n"
    sb ++= s"  // format: on\n"

    // Implicit parameters
    for (i <- 1 to n) {
      sb ++= s"      ${ArityGen.lower(i)}: c.WeakTypeTag[${ArityGen.lower(i)}],\n"
      sb ++= s"      ${ArityGen.upper(i)}: c.WeakTypeTag[${ArityGen.upper(i)}],\n"
    }
    sb ++= s"      HKTE: c.WeakTypeTag[HKT[${ArityGen.questionMarks(n)}]]\n"
    sb ++= s"  ): c.Tree = try {\n"

    // Assert
    sb ++= s"""    assert(HKTE.tpe.typeParams.size == $n, "HKT must have exactly ${numberWords(n)} type parameter${if (n > 1) "s" else ""}")\n"""
    sb ++= s"\n"

    // Val declarations
    sb ++= s"    val HKT = HKTE.tpe.typeConstructor\n"
    for (i <- 0 until n) {
      val p = ArityGen.paramName(i)
      sb ++= s"""    val $p = freshTypeName("$p") // ${i + 1}\n"""
    }

    // appliedHKT
    val boundsArgs = (1 to n).flatMap(i => Seq(s"$$${ArityGen.lower(i)}", s"$$${ArityGen.upper(i)}")).mkString(", ")
    val paramArgs = (0 until n).map(i => s"$$${ArityGen.paramName(i)}").mkString(", ")
    sb ++= s"""    val appliedHKT = tq"Type.$cn.Apply[$boundsArgs, $$HKT, $paramArgs]"\n"""
    sb ++= s"\n"

    // Fresh names
    sb ++= s"""    val ctx = freshName("ctx")\n"""
    sb ++= s"""    val convertProvidedTypesForCrossQuotes = freshName("convertProvidedTypesForCrossQuotes")\n"""
    sb ++= s"""    val termInner = freshName("Inner")\n"""
    sb ++= s"""    val typeInner = TypeName(freshName("Inner").toString)\n"""
    sb ++= s"\n"

    // --- Decomposed quasiquote builders ---

    // def buildPreamble(): builds ctx val, convert def, import, HKT val
    sb ++= s"    def buildPreamble(): List[c.Tree] = {\n"
    val stubTypeArgs = (1 to n).flatMap(i => Seq(s"$$${ArityGen.lower(i)}", s"$$${ArityGen.upper(i)}")).mkString(", ")
    sb ++= s"""      val ctxVal = q${tq}private val $$ctx = CrossQuotes.ctx[_root_.scala.reflect.macros.blackbox.Context]${tq}\n"""
    sb ++= s"""      val convertDef = q${tq}private implicit def $$convertProvidedTypesForCrossQuotes[$$typeInner](implicit $$termInner: Type[$$typeInner]): $$ctx.WeakTypeTag[$$typeInner] = $$termInner.asInstanceOf[$$ctx.WeakTypeTag[$$typeInner]]${tq}\n"""
    sb ++= s"""      val ctxImport = q${tq}import $$ctx.universe.{ TypeRef, TypeRefTag, ExistentialType, ExistentialTypeTag }${tq}\n"""
    sb ++= s"""      val hktVal = q${tq}private val HKT = $$ctx.weakTypeTag[Type.$cn.Stub[$stubTypeArgs, $$HKT]].tpe.typeArgs.last${tq}\n"""
    sb ++= s"      List(ctxVal, convertDef, ctxImport, hktVal)\n"
    sb ++= s"    }\n"
    sb ++= s"\n"

    // def buildApplyDef(): builds the apply method (uses appliedType to avoid TypeCreator issues with aliased form)
    // Uses context bounds + convertProvidedTypesForCrossQuotes to access the evidence
    val applyParams = (0 until n).map { i =>
      val p = ArityGen.paramName(i)
      val j = i + 1
      s"$$${p} >: $$${ArityGen.lower(j)} <: $$${ArityGen.upper(j)}: Type"
    }.mkString(", ")
    val appliedTypeArgs = (0 until n).map { i =>
      val p = ArityGen.paramName(i)
      s"$$convertProvidedTypesForCrossQuotes[$$${p}].tpe"
    }.mkString(", ")
    sb ++= s"    def buildApplyDef(): c.Tree =\n"
    sb ++= s"""      q${tq}\n"""
    sb ++= s"      def apply[$applyParams]: Type[$$appliedHKT] =\n"
    sb ++= s"        $$ctx.WeakTypeTag($$ctx.universe.appliedType(HKT,\n"
    sb ++= s"          _root_.scala.List($appliedTypeArgs)\n"
    sb ++= s"        ).dealias).asInstanceOf[Type[$$appliedHKT]]\n"
    sb ++= s"      $tq\n"
    sb ++= s"\n"

    // def buildUnapplyDefs(): builds matchResult and unapply
    val matchResultParams = (1 to n).map(i => s"tp$i: $$ctx.Type").mkString(", ")
    val matchResultReturn = matchResultReturnType(n)
    val unapplyReturn = unapplyOptionType(n, isFromUntyped = false)
    sb ++= s"    def buildUnapplyDefs(): List[c.Tree] = {\n"
    sb ++= s"      val matchResultDef = q${tq}\n"
    sb ++= s"      private def matchResult($matchResultParams): $matchResultReturn =\n"
    sb ++= matchResultBody(n, 8)
    sb ++= s"      $tq\n"
    sb ++= s"      val unapplyDef = q${tq}\n"
    sb ++= s"      def unapply[$$${ArityGen.paramName(0)}](A: Type[$$${ArityGen.paramName(0)}]): $unapplyReturn = {\n"
    sb ++= s"        val A0 = A.asInstanceOf[$$ctx.WeakTypeTag[$$${ArityGen.paramName(0)}]].tpe\n"
    sb ++= s"        A0.dealias.widen.baseType(HKT.typeSymbol) match {\n"
    val tpList = (1 to n).map(i => s"tp$i").mkString(", ")
    sb ++= s"          case TypeRef(_, _, _root_.scala.List($tpList)) =>\n"
    sb ++= s"            matchResult($tpList)\n"
    // [hearth#307] A WILDCARD type argument (`HKT[?]`, e.g. a `SomeFlag[?]` DSL member) makes `baseType` return a
    // PACKED `ExistentialType(_, TypeRef(...))` instead of a bare `TypeRef`, so the case above misses. Unwrap it here
    // (still `baseType`-symbol-based, so prefix-representation-independent) rather than falling through to the
    // structural-`==` fallback, which additionally misses when the constructor's prefix is captured via an alias.
    sb ++= s"          case ExistentialType(_, TypeRef(_, _, _root_.scala.List($tpList))) =>\n"
    sb ++= s"            matchResult($tpList)\n"
    sb ++= s"          case _ =>\n"
    sb ++= s"            if (A0.typeConstructor == HKT && A0.typeArgs.size == $n) {\n"
    sb ++= s"              val _root_.scala.Seq($tpList) = A0.typeArgs\n"
    sb ++= s"              matchResult($tpList)\n"
    sb ++= s"            }\n"
    sb ++= s"            else _root_.scala.None\n"
    sb ++= s"        }\n"
    sb ++= s"      }\n"
    sb ++= s"      $tq\n"
    sb ++= s"      List(matchResultDef, unapplyDef)\n"
    sb ++= s"    }\n"
    sb ++= s"\n"

    // def buildTree(): composes the parts into the final anonymous class
    val boundedTypeArgs = (1 to n).flatMap(i => Seq(s"$$${ArityGen.lower(i)}", s"$$${ArityGen.upper(i)}")).mkString(", ")
    sb ++= s"    def buildTree(): c.Tree = {\n"
    sb ++= s"      val stats = buildPreamble() ++ List(buildApplyDef()) ++ buildUnapplyDefs()\n"
    sb ++= s"""      q${tq}new Type.$cn.Bounded[$boundedTypeArgs, $$HKT] { ..$$stats }${tq}\n"""
    sb ++= s"    }\n"
    sb ++= s"\n"

    // Call buildTree() and typecheck
    sb ++= s"    val unchecked = buildTree()\n"
    sb ++= s"    val result = suppressWarnings(c.typecheck(unchecked))\n"
    sb ++= s"\n"

    // Local def doLog() to further reduce main method's bytecode
    sb ++= doLogDef(n, isFromUntyped = false)
    sb ++= s"\n"
    sb ++= s"    doLog()\n"
    sb ++= s"\n"
    sb ++= s"    result\n"
    sb ++= s"  } catch { case e: Throwable => reportIssue(e) }\n"

    sb.toString
  }

  // ---------------------------------------------------------------------------
  // typeCtorNFromUntypedBody
  // ---------------------------------------------------------------------------

  private def typeCtorNFromUntypedBody(n: Int): String = {
    val cn = ArityGen.ctorName(n)
    val sb = new StringBuilder

    // Comment block
    sb ++= s"  /* Creates:\n"
    sb ++= s"   *   Type.$cn.fromUntyped[HKT](untyped)\n"
    sb ++= s"   */\n"

    // Method signature
    sb ++= s"  // format: off\n"
    sb ++= s"  protected def typeCtor${n}FromUntypedBody[${ArityGen.boundsPairs(n)}, HKT[${ArityGen.hktSlots(n)}]](untyped: c.Expr[Any])(implicit\n"
    sb ++= s"  // format: on\n"

    // Implicit parameters
    for (i <- 1 to n) {
      sb ++= s"      ${ArityGen.lower(i)}: c.WeakTypeTag[${ArityGen.lower(i)}],\n"
      sb ++= s"      ${ArityGen.upper(i)}: c.WeakTypeTag[${ArityGen.upper(i)}],\n"
    }
    sb ++= s"      HKTE: c.WeakTypeTag[HKT[${ArityGen.questionMarks(n)}]]\n"
    sb ++= s"  ): c.Tree = try {\n"

    // Val declarations (no assert for fromUntyped)
    sb ++= s"    val HKT = HKTE.tpe.typeConstructor\n"
    for (i <- 0 until n) {
      val p = ArityGen.paramName(i)
      sb ++= s"""    val $p = freshTypeName("$p") // ${i + 1}\n"""
    }

    // appliedHKT
    val boundsArgs = (1 to n).flatMap(i => Seq(s"$$${ArityGen.lower(i)}", s"$$${ArityGen.upper(i)}")).mkString(", ")
    val paramArgs = (0 until n).map(i => s"$$${ArityGen.paramName(i)}").mkString(", ")
    sb ++= s"""    val appliedHKT = tq"Type.$cn.Apply[$boundsArgs, $$HKT, $paramArgs]"\n"""
    sb ++= s"\n"

    // Fresh names (no typeValue for fromUntyped)
    sb ++= s"""    val ctx = freshName("ctx")\n"""
    sb ++= s"""    val convertProvidedTypesForCrossQuotes = freshName("convertProvidedTypesForCrossQuotes")\n"""
    sb ++= s"""    val termInner = freshName("Inner")\n"""
    sb ++= s"""    val typeInner = TypeName(freshName("Inner").toString)\n"""
    sb ++= s"\n"

    // --- Decomposed quasiquote builders ---

    // def buildPreamble(): builds ctx val, convert def, import, HKT val (fromUntyped version)
    sb ++= s"    def buildPreamble(): List[c.Tree] = {\n"
    sb ++= s"""      val ctxVal = q${tq}private val $$ctx = CrossQuotes.ctx[_root_.scala.reflect.macros.blackbox.Context]${tq}\n"""
    sb ++= s"""      val convertDef = q${tq}private implicit def $$convertProvidedTypesForCrossQuotes[$$typeInner](implicit $$termInner: Type[$$typeInner]): $$ctx.WeakTypeTag[$$typeInner] = $$termInner.asInstanceOf[$$ctx.WeakTypeTag[$$typeInner]]${tq}\n"""
    sb ++= s"""      val ctxImport = q${tq}import $$ctx.universe.{ TypeRef, TypeRefTag, ExistentialType, ExistentialTypeTag }${tq}\n"""
    sb ++= s"""      val hktVal = q${tq}private val HKT = $${untyped.tree}.asInstanceOf[$$ctx.Type]${tq}\n"""
    sb ++= s"      List(ctxVal, convertDef, ctxImport, hktVal)\n"
    sb ++= s"    }\n"
    sb ++= s"\n"

    // def buildApplyDef(): builds the apply method (fromUntyped uses appliedType)
    // Uses context bounds + convertProvidedTypesForCrossQuotes to access the evidence
    val applyParams = (0 until n).map { i =>
      val p = ArityGen.paramName(i)
      val j = i + 1
      s"$$${p} >: $$${ArityGen.lower(j)} <: $$${ArityGen.upper(j)}: Type"
    }.mkString(", ")
    val appliedTypeArgs = (0 until n).map { i =>
      val p = ArityGen.paramName(i)
      s"$$convertProvidedTypesForCrossQuotes[$$${p}].tpe"
    }.mkString(", ")
    sb ++= s"    def buildApplyDef(): c.Tree =\n"
    sb ++= s"""      q${tq}\n"""
    sb ++= s"      def apply[$applyParams]: Type[$$appliedHKT] =\n"
    sb ++= s"        $$ctx.WeakTypeTag($$ctx.universe.appliedType(HKT,\n"
    sb ++= s"          _root_.scala.List($appliedTypeArgs)\n"
    sb ++= s"        ).dealias).asInstanceOf[Type[$$appliedHKT]]\n"
    sb ++= s"      $tq\n"
    sb ++= s"\n"

    // def buildUnapplyDefs(): builds matchResult and unapply
    val matchResultParams = (1 to n).map(i => s"tp$i: $$ctx.Type").mkString(", ")
    val matchResultReturn = matchResultReturnType(n)
    val unapplyReturn = unapplyOptionType(n, isFromUntyped = true)
    sb ++= s"    def buildUnapplyDefs(): List[c.Tree] = {\n"
    sb ++= s"      val matchResultDef = q${tq}\n"
    sb ++= s"      private def matchResult($matchResultParams): $matchResultReturn =\n"
    sb ++= matchResultBody(n, 8)
    sb ++= s"      $tq\n"
    sb ++= s"      val unapplyDef = q${tq}\n"
    sb ++= s"      def unapply[$$${ArityGen.paramName(0)}](A: Type[$$${ArityGen.paramName(0)}]): $unapplyReturn = {\n"
    sb ++= s"        val A0 = A.asInstanceOf[$$ctx.WeakTypeTag[$$${ArityGen.paramName(0)}]].tpe\n"
    sb ++= s"        A0.dealias.widen.baseType(HKT.typeSymbol) match {\n"
    val tpList = (1 to n).map(i => s"tp$i").mkString(", ")
    sb ++= s"          case TypeRef(_, _, _root_.scala.List($tpList)) =>\n"
    sb ++= s"            matchResult($tpList)\n"
    // [hearth#307] A WILDCARD type argument (`HKT[?]`, e.g. a `SomeFlag[?]` DSL member) makes `baseType` return a
    // PACKED `ExistentialType(_, TypeRef(...))` instead of a bare `TypeRef`, so the case above misses. Unwrap it here
    // (still `baseType`-symbol-based, so prefix-representation-independent) rather than falling through to the
    // structural-`==` fallback, which additionally misses when the constructor's prefix is captured via an alias.
    sb ++= s"          case ExistentialType(_, TypeRef(_, _, _root_.scala.List($tpList))) =>\n"
    sb ++= s"            matchResult($tpList)\n"
    sb ++= s"          case _ =>\n"
    sb ++= s"            if (A0.typeConstructor == HKT && A0.typeArgs.size == $n) {\n"
    sb ++= s"              val _root_.scala.Seq($tpList) = A0.typeArgs\n"
    sb ++= s"              matchResult($tpList)\n"
    sb ++= s"            }\n"
    sb ++= s"            else _root_.scala.None\n"
    sb ++= s"        }\n"
    sb ++= s"      }\n"
    sb ++= s"      $tq\n"
    sb ++= s"      List(matchResultDef, unapplyDef)\n"
    sb ++= s"    }\n"
    sb ++= s"\n"

    // def buildTree(): composes the parts into the final anonymous class
    val boundedTypeArgs = (1 to n).flatMap(i => Seq(s"$$${ArityGen.lower(i)}", s"$$${ArityGen.upper(i)}")).mkString(", ")
    sb ++= s"    def buildTree(): c.Tree = {\n"
    sb ++= s"      val stats = buildPreamble() ++ List(buildApplyDef()) ++ buildUnapplyDefs()\n"
    sb ++= s"""      q${tq}new Type.$cn.Bounded[$boundedTypeArgs, $$HKT] { ..$$stats }${tq}\n"""
    sb ++= s"    }\n"
    sb ++= s"\n"

    // Call buildTree() and typecheck
    sb ++= s"    val unchecked = buildTree()\n"
    sb ++= s"    val result = suppressWarnings(c.typecheck(unchecked))\n"
    sb ++= s"\n"

    // Local def doLog() to further reduce main method's bytecode
    sb ++= doLogDef(n, isFromUntyped = true)
    sb ++= s"\n"
    sb ++= s"    doLog()\n"
    sb ++= s"\n"
    sb ++= s"    result\n"
    sb ++= s"  } catch { case e: Throwable => reportIssue(e) }\n"

    sb.toString
  }

  // ---------------------------------------------------------------------------
  // Shared helpers
  // ---------------------------------------------------------------------------

  /** "U1, HKT" or "U1, U2, HKT" etc. */
  private def upperBoundedOfArgs(n: Int): String =
    (1 to n).map(i => ArityGen.upper(i)).mkString(", ")

  /** "L1, U1, HKT" or "L1, U1, L2, U2, HKT" etc. */
  private def boundedOfArgs(n: Int): String =
    (1 to n).map(i => s"${ArityGen.lower(i)}, ${ArityGen.upper(i)}").mkString(", ")

  /** matchResult return type: always `_root_.scala.Some[(bounds)]` */
  private def matchResultReturnType(n: Int): String = {
    val boundsInner = (1 to n).map(i => s"$$${ArityGen.lower(i)} <:??<: $$${ArityGen.upper(i)}").mkString(", ")
    s"_root_.scala.Some[($boundsInner)]"
  }

  /** unapply Option return type.
    *
    * For regular typeCtorNBody:
    *   - Ctor1: `Option[$$L1 <:??<: $$U1]` (no tuple parens)
    *   - Ctor2+: `Option[($$L1 <:??<: $$U1, ...)]`
    *
    * For fromUntyped:
    *   - Always: `Option[($$L1 <:??<: $$U1, ...)]` (with parens even for arity 1)
    */
  private def unapplyOptionType(n: Int, isFromUntyped: Boolean): String = {
    val bounds = (1 to n).map(i => s"$$${ArityGen.lower(i)} <:??<: $$${ArityGen.upper(i)}").mkString(", ")
    // `_root_.scala.Option` — see the hygiene note in this object's Scaladoc.
    if (n == 1 && !isFromUntyped) s"_root_.scala.Option[$bounds]"
    else s"_root_.scala.Option[($bounds)]"
  }

  /** matchResult body: the `_root_.scala.Some(...)` expression.
    *
    * For Ctor1: `_root_.scala.Some(expr)` (no tuple)
    * For Ctor2+: `_root_.scala.Some((\n  expr1,\n  expr2\n))`
    */
  private def matchResultBody(n: Int, baseIndent: Int): String = {
    val indent = " " * baseIndent
    val exprIndent = " " * (baseIndent + 6) // aligning with inner Some(
    // Cast to `Type[U_i]` (the UPPER bound), not `Type[scala.Any]`: `as_<:??<:[L, U]` on a `Type[A]` requires `U >: A`,
    // so a `Type[scala.Any]` receiver would force `U >: Any` and reject any non-`Any` upper bound. For an unbounded
    // ctor `U_i` is `Any`, so this is unchanged there; for a bounded ctor it makes `U >: U` hold trivially. See #307.
    //
    // [hearth#307] `dealias` the extracted argument but do NOT `widen` it: widening decays a literal singleton
    // (`ConstantType`, e.g. the `"fieldName"` in `Path.Select["fieldName", _]`) to `java.lang.String`, and a
    // term singleton (`x.type`) to its widened type — an extractor must hand back the argument as written.
    if (n == 1) {
      val expr = s"$$ctx.WeakTypeTag(tp1.dealias).asInstanceOf[Type[$$${ArityGen.upper(1)}]].as_<:??<:[$$${ArityGen.lower(1)}, $$${ArityGen.upper(1)}]"
      s"${indent}_root_.scala.Some($expr)\n"
    } else {
      val sb = new StringBuilder
      sb ++= s"${indent}_root_.scala.Some((\n"
      for (i <- 1 to n) {
        val expr = s"$$ctx.WeakTypeTag(tp$i.dealias).asInstanceOf[Type[$$${ArityGen.upper(i)}]].as_<:??<:[$$${ArityGen.lower(i)}, $$${ArityGen.upper(i)}]"
        if (i < n) sb ++= s"$exprIndent$expr,\n"
        else sb ++= s"$exprIndent$expr\n"
      }
      sb ++= s"${indent}    ))\n"
      sb.toString
    }
  }

  /** Generate the local `def doLog()` block for either typeCtorNBody or typeCtorNFromUntypedBody.
    *
    * This is a local def (not inlined) to keep the main method's bytecode smaller.
    */
  private def doLogDef(n: Int, isFromUntyped: Boolean): String = {
    val cn = ArityGen.ctorName(n)
    val label = if (isFromUntyped) s"Type.$cn.fromUntyped" else s"Type.$cn.of"

    // Build the pp arguments for From line: ${pp(L1.tpe)}, ${pp(U1.tpe)}, ..., ${pp(HKT)}
    val ppArgs = (1 to n).flatMap { i =>
      Seq(s"$${pp(${ArityGen.lower(i)}.tpe)}", s"$${pp(${ArityGen.upper(i)}.tpe)}")
    } :+ "${pp(HKT)}"

    val ppArgsStr = ppArgs.mkString(", ")

    val sb = new StringBuilder
    sb ++= s"    def doLog(): Unit = log(\n"
    sb ++= s"""      s${tq}Cross-quotes $${paintExclDot(Console.BLUE)("$label")} expansion:\n"""
    sb ++= s"""         |From: $${paintExclDot(Console.BLUE)("$label")}[$ppArgsStr]\n"""
    sb ++= s"""         |To: $${indent(pp(result))}${tq}.stripMargin\n"""
    sb ++= s"    )\n"

    sb.toString
  }
}
