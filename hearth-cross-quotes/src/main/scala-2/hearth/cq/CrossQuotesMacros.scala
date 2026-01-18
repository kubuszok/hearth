package hearth
package cq

import java.util.regex.{Matcher, Pattern}
import hearth.treeprinter.*
import scala.reflect.macros.blackbox
import scala.util.chaining.*

/** These macros are responsible for rewriting Expr.quote/Expr.splice into native quotes
  * ([[scala.reflect.macros.blackbox.Context.Expr]]/[[scala.reflect.macros.blackbox.Context.Type]]) in Scala 2.
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
  * val ctx: blackbox.Context = CrossQuotes.ctx
  * import ctx.universe.{Type => _, internal => _, _}
  * implicit def convertProvidedTypesForCrossQuotes[B](implicit B: Type[B]): ctx.WeakTypeTag[B] =
  *   B.asInstanceOf[ctx.WeakTypeTag[B]]
  * _root_.hearth.fp.ignore(convertProvidedTypesForCrossQuotes[Any](_: Type[Any])) // supress warnings
  * weakTypeTag[A].asInstanceOf[Type[A}]]
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
  * c.Expr[A](
  *   q"""
  *   expr
  *   """
  * ).asInstanceOf[Expr[A]]
  * }}}
  *
  * and
  *
  * {{{
  * Expr.splice[A](expr)
  * }}}
  *
  * with:
  *
  * {{{
  * ${ expr.asInstanceOf[c.Expr[A]] } // interpolation within q""
  * }}}
  *
  * It uses intermediate fresh name to create a stub and replace it with the actual expression unsing Regex.
  *
  * Same for: Type.Ctor1.of[HKT], Type.Ctor2.of[HKT], Type.Ctor3.of[HKT], ..., Type.Ctor22.of[HKT].
  *
  * The challenges here are:
  *   - `expr.toString` and `showCode(expr)` does not always print the correct Scala code:
  *     - `def <init>` is correct AST... but parser does not understand it, similarly many `<synthethic>` methods
  *       (`expr.toString` prints it `showCode(expr)` has it fixed)
  *     - `final class $anon` + `new $anon()` could break the interpolation (and looks odd in the printed code)
  *   - applied type parameter [A] makes sense outside the code... in the AST it's just an abstract type that doesn't
  *     make sense
  *
  * So, we have to:
  *
  *   - use `showCodePretty` to print the code - it fixes most of the issues
  *   - escape the remaining `$`s
  *   - carefully replace the type parameter [A] with something that makes sense - actually, the type that is
  *     represented by `Type[A]` - this is the most fragile part of the whole macro.
  *
  * The result while not perfect, should be robust enough for most cases.
  */
final class CrossQuotesMacros(val c: blackbox.Context) extends ShowCodePrettyScala2 {

  import c.universe.{Expr as _, *}

  private def reportIssue(throwable: Throwable): Nothing =
    c.abort(
      c.enclosingPosition,
      s"""Unexpected error ${throwable.getClass.getName} at ${c.enclosingPosition.source.path}:${c.enclosingPosition.line}:${c.enclosingPosition.column}:
         |
         |${indent(throwable.getMessage)}
         |
         |from:
         |${throwable.getStackTrace.map(item => s"  at: $item").mkString("\n")}
         |
         |Please, report an issue at https://github.com/MateuszKubuszok/hearth/issues
         |""".stripMargin
    )

  private val loggingEnabled = CrossQuotesSettings.parseLoggingSettingsForScala2(c.settings)(
    scala.util.Try(c.enclosingPosition.source.file.file).toOption,
    c.enclosingPosition.line,
    c.enclosingPosition.column
  )

  private def log(message: => String): Unit =
    if (loggingEnabled) {
      // println(s"Logging: $message at ${c.enclosingPosition.source.path}:${c.enclosingPosition.line}")
      c.echo(c.enclosingPosition, message)
    }

  // Scala 3 generate prefix$macro$[n] while Scala 2 prefix[n] and we want to align the behavior
  private def freshName(prefix: String): TermName = c.universe.internal.reificationSupport.freshTermName(prefix)
  private def freshTypeName(prefix: String): TypeName = c.universe.internal.reificationSupport.freshTypeName(prefix)

  private def paint(color: String)(text: String): String =
    text.split("\n").map(line => s"$color$line${Console.RESET}").mkString("\n")

  private def paintExclDot(color: String)(text: String): String =
    text.split("[.]").map(segment => s"$color$segment${Console.RESET}").mkString(".")

  private def indent(text: String): String =
    text.split("\n").map(line => s"  $line").mkString("\n")

  private def suppressWarnings(result: c.Tree): c.Tree =
    q"""
    $result : @_root_.scala.annotation.nowarn
    """

  // One day we'll use Symbol's annotations but for now we'll use this list.
  private val ImportedCrossTypeImplicit = Set(
    "Underlying", // from Existential
    "Returned", // from Method.NoInstance && Method.OfInstance
    "Instance", // from Method.OfInstance
    // "Result", // from PossibleSmartCtor (when implicit Type.CtorN will be supported)
    "PossibleSmartResult", // from IsCollectionOf
    "Key", // from IsMapOf
    "Value", // from IsMapOf
    "LeftValue", // from IsEither
    "RightValue" // from IsEither
  )
  implicit private class ImportedCrossTypeImplicitOps(private val name: Name) {
    // Should handle all imported values defined as:
    //   @ImportedCrossTypeImplicit
    //   implicit val SomeName: Type[SomeName]
    // and used as:
    //   import someValue.SomeName // potentially SomeName as SomeOtherName
    def isImportedCrossTypeImplicit: Boolean = ImportedCrossTypeImplicit(name.decodedName.toString)
  }

  /* Replaces:
   *   Type.of[A]
   * with:
   *   what we see in Quasiquote
   */
  def typeOfImpl[A: c.WeakTypeTag]: c.Tree = try {
    val ctx = freshName("ctx")
    val typeValue = TypeWithUnderlyingInjected(ctx, tq"${weakTypeOf[A]}")
    val convertProvidedTypesForCrossQuotes = freshName("convertProvidedTypesForCrossQuotes")
    val termInner = freshName("Inner")
    val typeInner = freshTypeName("Inner")

    // TODO: This: import _root_.scala.reflect.api.{Mirror, TypeCreator, Universe}
    // is kind of a hack, because apparently these types were not sanitized during printing.
    // We should fix the printing, so that we don't have to introduce this import.
    val unchecked = q"""
    val $ctx = CrossQuotes.ctx[_root_.scala.reflect.macros.blackbox.Context]
    import _root_.scala.reflect.api.{Mirror, TypeCreator, Universe}
    import $ctx.universe.{Mirror => _,Type => _, internal => _, _}
    implicit def $convertProvidedTypesForCrossQuotes[$typeInner](implicit $termInner: Type[$typeInner]): $ctx.WeakTypeTag[$typeInner] =
      $termInner.asInstanceOf[$ctx.WeakTypeTag[$typeInner]]
    _root_.hearth.fp.ignore($convertProvidedTypesForCrossQuotes[Any](_: Type[Any]))
    $typeValue
    """

    val result = suppressWarnings(c.typecheck(unchecked))

    log(
      s"""Cross-quotes ${paintExclDot(Console.BLUE)("Type.of")} expansion:
         |From: ${paintExclDot(Console.BLUE)("Type.of")}[${weakTypeOf[A]}]
         |To: ${indent(showCodePretty(result, SyntaxHighlight.ANSI))}""".stripMargin
    )

    result
  } catch { case e: Throwable => reportIssue(e) }

  /* Replaces:
   *   Type.Ctor1.of[HKT]
   *   Type.Ctor1.UpperBounded.of[U1, HKT]
   *   Type.Ctor1.Bounded.of[L1, U1, HKT]
   * with:
   *   what we see in Quasiquote
   */
  // format: off
  def typeCtor1Impl[L1, U1 >: L1, HKT[_ >: L1 <: U1]](implicit
  // format: on
      L1: c.WeakTypeTag[L1],
      U1: c.WeakTypeTag[U1],
      HKTE: c.WeakTypeTag[HKT[?]]
  ): c.Tree = try {
    assert(HKTE.tpe.typeParams.size == 1, "HKT must have exactly one type parameter")

    val HKT = HKTE.tpe.typeConstructor
    val A = freshTypeName("A") // 1
    val appliedHKT = tq"Type.Ctor1.Apply[$L1, $U1, $HKT, $A]"

    val ctx = freshName("ctx")
    val typeValue = TypeWithUnderlyingInjected(ctx, appliedHKT, A)
    val convertProvidedTypesForCrossQuotes = freshName("convertProvidedTypesForCrossQuotes")
    val termInner = freshName("Inner")
    val typeInner = TypeName(freshName("Inner").toString)

    val unchecked = q"""
    new Type.Ctor1.Bounded[$L1, $U1, $HKT] {
      private val $ctx = CrossQuotes.ctx[_root_.scala.reflect.macros.blackbox.Context]
      private implicit def $convertProvidedTypesForCrossQuotes[$typeInner](implicit $termInner: Type[$typeInner]): $ctx.WeakTypeTag[$typeInner] =
        $termInner.asInstanceOf[$ctx.WeakTypeTag[$typeInner]]
      import $ctx.universe.{Type => _, internal => _, _}

      private val HKT = $ctx.weakTypeTag[Type.Ctor1.Stub[$L1, $U1, $HKT]].tpe.typeArgs.last

      def apply[$A >: $L1 <: $U1: Type]: Type[$appliedHKT] =
        $typeValue

      def unapply[$A](A: Type[$A]): Option[$L1 <:??<: $U1] = {
        val A0 = A.asInstanceOf[$ctx.WeakTypeTag[$A]].tpe
        A0.dealias.widen.baseType(HKT.typeSymbol) match {
          case TypeRef(_, _, List(tp1)) =>
            _root_.scala.Some($ctx.WeakTypeTag(tp1.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L1, $U1])
          case _ =>
            if (A0.typeConstructor == HKT && A0.typeArgs.size == 1) {
              val Seq(tp1) = A0.typeArgs
              _root_.scala.Some($ctx.WeakTypeTag(tp1.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L1, $U1])
            }
            else None
        }
      }
    }
    """

    val result = suppressWarnings(c.typecheck(unchecked))

    log(
      s"""Cross-quotes ${paintExclDot(Console.BLUE)("Type.Ctor1.of")} expansion:
         |From: ${paintExclDot(Console.BLUE)("Type.Ctor1.of")}[${L1.tpe}, ${U1.tpe}, $HKT]
         |To: ${indent(showCodePretty(result, SyntaxHighlight.ANSI))}""".stripMargin
    )

    result
  } catch { case e: Throwable => reportIssue(e) }

  /* Replaces:
   *   Type.Ctor2.of[HKT]
   *   Type.Ctor2.UpperBounded.of[U1, U2, HKT]
   *   Type.Ctor2.Bounded.of[L1, U1, L2, U2, HKT]
   * with:
   *   what we see in Quasiquote
   */
  // format: off
  def typeCtor2Impl[L1, U1 >: L1, L2, U2 >: L2, HKT[_ >: L1 <: U1, _ >: L2 <: U2]](implicit
  // format: on
      L1: c.WeakTypeTag[L1],
      U1: c.WeakTypeTag[U1],
      L2: c.WeakTypeTag[L2],
      U2: c.WeakTypeTag[U2],
      HKTE: c.WeakTypeTag[HKT[?, ?]]
  ): c.Tree = try {
    assert(HKTE.tpe.typeParams.size == 2, "HKT must have exactly two type parameters")

    val HKT = HKTE.tpe.typeConstructor
    val A = freshTypeName("A") // 1
    val B = freshTypeName("B") // 2
    val appliedHKT = tq"Type.Ctor2.Apply[$L1, $U1, $L2, $U2, $HKT, $A, $B]"

    val ctx = freshName("ctx")
    val typeValue = TypeWithUnderlyingInjected(ctx, appliedHKT, A, B)
    val convertProvidedTypesForCrossQuotes = freshName("convertProvidedTypesForCrossQuotes")
    val termInner = freshName("Inner")
    val typeInner = TypeName(freshName("Inner").toString)

    val unchecked = q"""
    new Type.Ctor2.Bounded[$L1, $U1, $L2, $U2, $HKT] {
      private val $ctx = CrossQuotes.ctx[_root_.scala.reflect.macros.blackbox.Context]
      private implicit def $convertProvidedTypesForCrossQuotes[$typeInner](implicit $termInner: Type[$typeInner]): $ctx.WeakTypeTag[$typeInner] =
        $termInner.asInstanceOf[$ctx.WeakTypeTag[$typeInner]]
      import $ctx.universe.{Type => _, internal => _, _}

      private val HKT = $ctx.weakTypeTag[Type.Ctor2.Stub[$L1, $U1, $L2, $U2, $HKT]].tpe.typeArgs.last

      def apply[$A >: $L1 <: $U1: Type, $B >: $L2 <: $U2: Type]: Type[$appliedHKT] = 
        $typeValue

      def unapply[$A](A: Type[$A]): Option[($L1 <:??<: $U1, $L2 <:??<: $U2)] = {
        val A0 = A.asInstanceOf[$ctx.WeakTypeTag[$A]].tpe
        A0.dealias.widen.baseType(HKT.typeSymbol) match {
          case TypeRef(_, _, List(tp1, tp2)) =>
            _root_.scala.Some(
              (
                $ctx.WeakTypeTag(tp1.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L1, $U1],
                $ctx.WeakTypeTag(tp2.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L2, $U2]
              )
            )
          case _ =>
            if (A0.typeConstructor == HKT && A0.typeArgs.size == 2) {
              val Seq(tp1, tp2) = A0.typeArgs
              _root_.scala.Some(
                ($ctx.WeakTypeTag(tp1.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L1, $U1],
                $ctx.WeakTypeTag(tp2.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L2, $U2])
              )
            }
            else None
        }
      }
    }
    """

    val result = suppressWarnings(c.typecheck(unchecked))

    log(
      s"""Cross-quotes ${paintExclDot(Console.BLUE)("Type.Ctor2.of")} expansion:
         |From: ${paintExclDot(Console.BLUE)("Type.Ctor2.of")}[${L1.tpe}, ${U1.tpe}, ${L2.tpe}, ${U2.tpe}, $HKT]
         |To: ${indent(showCodePretty(result, SyntaxHighlight.ANSI))}""".stripMargin
    )

    result
  } catch { case e: Throwable => reportIssue(e) }

  /* Replaces:
   *   Type.Ctor3.of[HKT]
   *   Type.Ctor3.UpperBounded.of[U1, U2, U3, HKT]
   *   Type.Ctor3.Bounded.of[L1, U1, L2, U2, L3, U3, HKT]
   * with:
   *   what we see in Quasiquote
   */
  // format: off
  def typeCtor3Impl[L1, U1 >: L1, L2, U2 >: L2, L3, U3 >: L3, HKT[_ >: L1 <: U1, _ >: L2 <: U2, _ >: L3 <: U3]](implicit
  // format: on
      L1: c.WeakTypeTag[L1],
      U1: c.WeakTypeTag[U1],
      L2: c.WeakTypeTag[L2],
      U2: c.WeakTypeTag[U2],
      L3: c.WeakTypeTag[L3],
      U3: c.WeakTypeTag[U3],
      HKTE: c.WeakTypeTag[HKT[?, ?, ?]]
  ): c.Tree = try {
    assert(HKTE.tpe.typeParams.size == 3, "HKT must have exactly three type parameters")

    val HKT = HKTE.tpe.typeConstructor
    val A = freshTypeName("A")
    val B = freshTypeName("B")
    val C = freshTypeName("C")
    val appliedHKT = tq"Type.Ctor3.Apply[$L1, $U1, $L2, $U2, $L3, $U3, $HKT, $A, $B, $C]"

    val ctx = freshName("ctx")
    val typeValue = TypeWithUnderlyingInjected(ctx, appliedHKT, A, B, C)
    val convertProvidedTypesForCrossQuotes = freshName("convertProvidedTypesForCrossQuotes")
    val termInner = freshName("Inner")
    val typeInner = TypeName(freshName("Inner").toString)

    val unchecked = q"""
    new Type.Ctor3.Bounded[$L1, $U1, $L2, $U2, $L3, $U3, $HKT] {
      private val $ctx = CrossQuotes.ctx[_root_.scala.reflect.macros.blackbox.Context]
      private implicit def $convertProvidedTypesForCrossQuotes[$typeInner](implicit $termInner: Type[$typeInner]): $ctx.WeakTypeTag[$typeInner] =
        $termInner.asInstanceOf[$ctx.WeakTypeTag[$typeInner]]
      import $ctx.universe.{Type => _, internal => _, _}

      private val HKT = $ctx.weakTypeTag[Type.Ctor3.Stub[$L1, $U1, $L2, $U2, $L3, $U3, $HKT]].tpe.typeArgs.last

      def apply[$A >: $L1 <: $U1: Type, $B >: $L2 <: $U2: Type, $C >: $L3 <: $U3: Type]: Type[$appliedHKT] = 
        $typeValue  

      def unapply[$A](A: Type[$A]): Option[($L1 <:??<: $U1, $L2 <:??<: $U2, $L3 <:??<: $U3)] = {
        val A0 = A.asInstanceOf[$ctx.WeakTypeTag[$A]].tpe
        A0.dealias.widen.baseType(HKT.typeSymbol) match {
          case TypeRef(_, _, List(tp1, tp2, tp3)) =>
            _root_.scala.Some(
              (
                $ctx.WeakTypeTag(tp1.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L1, $U1],
                $ctx.WeakTypeTag(tp2.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L2, $U2],
                $ctx.WeakTypeTag(tp3.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L3, $U3]
              )
            )
          case _ =>
            if (A0.typeConstructor == HKT && A0.typeArgs.size == 3) {
              val Seq(tp1, tp2, tp3) = A0.typeArgs
              _root_.scala.Some(
                (
                  $ctx.WeakTypeTag(tp1.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L1, $U1],
                  $ctx.WeakTypeTag(tp2.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L2, $U2],
                  $ctx.WeakTypeTag(tp3.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L3, $U3]
                )
              )
            }
            else None
        }
      }
    }
    """

    val result = suppressWarnings(c.typecheck(unchecked))

    log(
      s"""Cross-quotes ${paintExclDot(Console.BLUE)("Type.Ctor3.of")} expansion:
         |From: ${paintExclDot(Console.BLUE)(
          "Type.Ctor3.of"
        )}[${L1.tpe}, ${U1.tpe}, ${L2.tpe}, ${U2.tpe}, ${L3.tpe}, ${U3.tpe}, $HKT]
         |To: ${indent(showCodePretty(result, SyntaxHighlight.ANSI))}""".stripMargin
    )

    result
  } catch { case e: Throwable => reportIssue(e) }

  /* Replaces:
   *   Type.Ctor4.of[HKT]
   *   Type.Ctor4.UpperBounded.of[U1, U2, U3, U4, HKT]
   *   Type.Ctor4.Bounded.of[L1, U1, L2, U2, L3, U3, L4, U4, HKT]
   * with:
   *   what we see in Quasiquote
   */
  // format: off
    def typeCtor4Impl[L1, U1 >: L1, L2, U2 >: L2, L3, U3 >: L3, L4, U4 >: L4, HKT[_ >: L1 <: U1, _ >: L2 <: U2, _ >: L3 <: U3, _ >: L4 <: U4]](implicit
  // format: on
      L1: c.WeakTypeTag[L1],
      U1: c.WeakTypeTag[U1],
      L2: c.WeakTypeTag[L2],
      U2: c.WeakTypeTag[U2],
      L3: c.WeakTypeTag[L3],
      U3: c.WeakTypeTag[U3],
      L4: c.WeakTypeTag[L4],
      U4: c.WeakTypeTag[U4],
      HKTE: c.WeakTypeTag[HKT[?, ?, ?, ?]]
  ): c.Tree = try {
    assert(HKTE.tpe.typeParams.size == 4, "HKT must have exactly four type parameters")

    val HKT = HKTE.tpe.typeConstructor
    val A = freshTypeName("A") // 1
    val B = freshTypeName("B") // 2
    val C = freshTypeName("C") // 3
    val D = freshTypeName("D") // 4
    val appliedHKT = tq"Type.Ctor4.Apply[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $HKT, $A, $B, $C, $D]"

    val ctx = freshName("ctx")
    val typeValue = TypeWithUnderlyingInjected(ctx, appliedHKT, A, B, C, D)
    val convertProvidedTypesForCrossQuotes = freshName("convertProvidedTypesForCrossQuotes")
    val termInner = freshName("Inner")
    val typeInner = TypeName(freshName("Inner").toString)

    val unchecked = q"""
    new Type.Ctor4.Bounded[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $HKT] {
      private val $ctx = CrossQuotes.ctx[_root_.scala.reflect.macros.blackbox.Context]
      private implicit def $convertProvidedTypesForCrossQuotes[$typeInner](implicit $termInner: Type[$typeInner]): $ctx.WeakTypeTag[$typeInner] =
        $termInner.asInstanceOf[$ctx.WeakTypeTag[$typeInner]]
      import $ctx.universe.{Type => _, internal => _, _}

      private val HKT = $ctx.weakTypeTag[Type.Ctor4.Stub[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $HKT]].tpe.typeArgs.last

      def apply[$A >: $L1 <: $U1: Type, $B >: $L2 <: $U2: Type, $C >: $L3 <: $U3: Type, $D >: $L4 <: $U4: Type]: Type[$appliedHKT] = 
        $typeValue

      def unapply[$A](A: Type[$A]): Option[($L1 <:??<: $U1, $L2 <:??<: $U2, $L3 <:??<: $U3, $L4 <:??<: $U4)] = {
        val A0 = A.asInstanceOf[$ctx.WeakTypeTag[$A]].tpe
        A0.dealias.widen.baseType(HKT.typeSymbol) match {
          case TypeRef(_, _, List(tp1, tp2, tp3, tp4)) =>
            _root_.scala.Some(
              (
                $ctx.WeakTypeTag(tp1.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L1, $U1],
                $ctx.WeakTypeTag(tp2.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L2, $U2],
                $ctx.WeakTypeTag(tp3.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L3, $U3],
                $ctx.WeakTypeTag(tp4.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L4, $U4]
              )
            )
          case _ =>
            if (A0.typeConstructor == HKT && A0.typeArgs.size == 4) {
              val Seq(tp1, tp2, tp3, tp4) = A0.typeArgs
              _root_.scala.Some(
                (
                  $ctx.WeakTypeTag(tp1.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L1, $U1],
                  $ctx.WeakTypeTag(tp2.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L2, $U2],
                  $ctx.WeakTypeTag(tp3.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L3, $U3],
                  $ctx.WeakTypeTag(tp4.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L4, $U4]
                )
              )
            }
            else None
        }
      }
    }
    """

    val result = suppressWarnings(c.typecheck(unchecked))

    log(
      s"""Cross-quotes ${paintExclDot(Console.BLUE)("Type.Ctor4.of")} expansion:
         |From: ${paintExclDot(Console.BLUE)(
          "Type.Ctor4.of"
        )}[${L1.tpe}, ${U1.tpe}, ${L2.tpe}, ${U2.tpe}, ${L3.tpe}, ${U3.tpe}, ${L4.tpe}, ${U4.tpe}, $HKT]
         |To: ${indent(showCodePretty(result, SyntaxHighlight.ANSI))}""".stripMargin
    )

    result
  } catch { case e: Throwable => reportIssue(e) }

  /* Replaces:
   *   Type.Ctor5.of[HKT]
   *   Type.Ctor5.UpperBounded.of[U1, U2, U3, U4, U5, HKT]
   *   Type.Ctor5.Bounded.of[L1, U1, L2, U2, L3, U3, L4, U4, L5, U5, HKT]
   * with:
   *   what we see in Quasiquote
   */
  // format: off
  def typeCtor5Impl[L1, U1 >: L1, L2, U2 >: L2, L3, U3 >: L3, L4, U4 >: L4, L5, U5 >: L5, HKT[_ >: L1 <: U1, _ >: L2 <: U2, _ >: L3 <: U3, _ >: L4 <: U4, _ >: L5 <: U5]](implicit
  // format: on
      L1: c.WeakTypeTag[L1],
      U1: c.WeakTypeTag[U1],
      L2: c.WeakTypeTag[L2],
      U2: c.WeakTypeTag[U2],
      L3: c.WeakTypeTag[L3],
      U3: c.WeakTypeTag[U3],
      L4: c.WeakTypeTag[L4],
      U4: c.WeakTypeTag[U4],
      L5: c.WeakTypeTag[L5],
      U5: c.WeakTypeTag[U5],
      HKTE: c.WeakTypeTag[HKT[?, ?, ?, ?, ?]]
  ): c.Tree = try {
    assert(HKTE.tpe.typeParams.size == 5, "HKT must have exactly five type parameters")

    val HKT = HKTE.tpe.typeConstructor
    val A = freshTypeName("A") // 1
    val B = freshTypeName("B") // 2
    val C = freshTypeName("C") // 3
    val D = freshTypeName("D") // 4
    val E = freshTypeName("E") // 5
    val appliedHKT = tq"Type.Ctor5.Apply[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $HKT, $A, $B, $C, $D, $E]"

    val ctx = freshName("ctx")
    val typeValue = TypeWithUnderlyingInjected(ctx, appliedHKT, A, B, C, D, E)
    val convertProvidedTypesForCrossQuotes = freshName("convertProvidedTypesForCrossQuotes")
    val termInner = freshName("Inner")
    val typeInner = TypeName(freshName("Inner").toString)

    val unchecked = q"""
    new Type.Ctor5.Bounded[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $HKT] {
      private val $ctx = CrossQuotes.ctx[_root_.scala.reflect.macros.blackbox.Context]
      private implicit def $convertProvidedTypesForCrossQuotes[$typeInner](implicit $termInner: Type[$typeInner]): $ctx.WeakTypeTag[$typeInner] =
        $termInner.asInstanceOf[$ctx.WeakTypeTag[$typeInner]]
      import $ctx.universe.{Type => _, internal => _, _}

      private val HKT = $ctx.weakTypeTag[Type.Ctor5.Stub[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $HKT]].tpe.typeArgs.last

      def apply[$A >: $L1 <: $U1: Type, $B >: $L2 <: $U2: Type, $C >: $L3 <: $U3: Type, $D >: $L4 <: $U4: Type, $E >: $L5 <: $U5: Type]: Type[$appliedHKT] = 
        $typeValue

      def unapply[$A](A: Type[$A]): Option[($L1 <:??<: $U1, $L2 <:??<: $U2, $L3 <:??<: $U3, $L4 <:??<: $U4, $L5 <:??<: $U5)] = {
        val A0 = A.asInstanceOf[$ctx.WeakTypeTag[$A]].tpe
        A0.dealias.widen.baseType(HKT.typeSymbol) match {
          case TypeRef(_, _, List(tp1, tp2, tp3, tp4, tp5)) =>
            _root_.scala.Some(
              (
                $ctx.WeakTypeTag(tp1.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L1, $U1],
                $ctx.WeakTypeTag(tp2.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L2, $U2],
                $ctx.WeakTypeTag(tp3.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L3, $U3],
                $ctx.WeakTypeTag(tp4.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L4, $U4],
                $ctx.WeakTypeTag(tp5.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L5, $U5]
              )
            )
          case _ =>
            if (A0.typeConstructor == HKT && A0.typeArgs.size == 5) {
              val Seq(tp1, tp2, tp3, tp4, tp5) = A0.typeArgs
              _root_.scala.Some(
                (
                  $ctx.WeakTypeTag(tp1.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L1, $U1],
                  $ctx.WeakTypeTag(tp2.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L2, $U2],
                  $ctx.WeakTypeTag(tp3.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L3, $U3],
                  $ctx.WeakTypeTag(tp4.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L4, $U4],
                  $ctx.WeakTypeTag(tp5.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L5, $U5]
                )
              )
            }
            else None
        }
      }
    }
    """

    val result = suppressWarnings(c.typecheck(unchecked))

    log(
      s"""Cross-quotes ${paintExclDot(Console.BLUE)("Type.Ctor5.of")} expansion:
         |From: ${paintExclDot(Console.BLUE)(
          "Type.Ctor5.of"
        )}[${L1.tpe}, ${U1.tpe}, ${L2.tpe}, ${U2.tpe}, ${L3.tpe}, ${U3.tpe}, ${L4.tpe}, ${U4.tpe}, ${L5.tpe}, ${U5.tpe}, $HKT]
         |To: ${indent(showCodePretty(result, SyntaxHighlight.ANSI))}""".stripMargin
    )

    result
  } catch { case e: Throwable => reportIssue(e) }

  /* Replaces:
   *   Type.Ctor6.of[HKT]
   *   Type.Ctor6.UpperBounded.of[U1, U2, U3, U4, U5, U6, HKT]
   *   Type.Ctor6.Bounded.of[L1, U1, L2, U2, L3, U3, L4, U4, L5, U5, L6, U6, HKT]
   * with:
   *   what we see in Quasiquote
   */
  // format: off
  def typeCtor6Impl[L1, U1 >: L1, L2, U2 >: L2, L3, U3 >: L3, L4, U4 >: L4, L5, U5 >: L5, L6, U6 >: L6, HKT[_ >: L1 <: U1, _ >: L2 <: U2, _ >: L3 <: U3, _ >: L4 <: U4, _ >: L5 <: U5, _ >: L6 <: U6]](implicit
  // format: on
      L1: c.WeakTypeTag[L1],
      U1: c.WeakTypeTag[U1],
      L2: c.WeakTypeTag[L2],
      U2: c.WeakTypeTag[U2],
      L3: c.WeakTypeTag[L3],
      U3: c.WeakTypeTag[U3],
      L4: c.WeakTypeTag[L4],
      U4: c.WeakTypeTag[U4],
      L5: c.WeakTypeTag[L5],
      U5: c.WeakTypeTag[U5],
      L6: c.WeakTypeTag[L6],
      U6: c.WeakTypeTag[U6],
      HKTE: c.WeakTypeTag[HKT[?, ?, ?, ?, ?, ?]]
  ): c.Tree = try {
    assert(HKTE.tpe.typeParams.size == 6, "HKT must have exactly six type parameters")

    val HKT = HKTE.tpe.typeConstructor
    val A = freshTypeName("A") // 1
    val B = freshTypeName("B") // 2
    val C = freshTypeName("C") // 3
    val D = freshTypeName("D") // 4
    val E = freshTypeName("E") // 5
    val F = freshTypeName("F") // 6
    val appliedHKT =
      tq"Type.Ctor6.Apply[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $HKT, $A, $B, $C, $D, $E, $F]"

    val ctx = freshName("ctx")
    val typeValue = TypeWithUnderlyingInjected(ctx, appliedHKT, A, B, C, D, E, F)
    val convertProvidedTypesForCrossQuotes = freshName("convertProvidedTypesForCrossQuotes")
    val termInner = freshName("Inner")
    val typeInner = TypeName(freshName("Inner").toString)

    val unchecked = q"""
    new Type.Ctor6.Bounded[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $HKT] {
      private val $ctx = CrossQuotes.ctx[_root_.scala.reflect.macros.blackbox.Context]
      private implicit def $convertProvidedTypesForCrossQuotes[$typeInner](implicit $termInner: Type[$typeInner]): $ctx.WeakTypeTag[$typeInner] =
        $termInner.asInstanceOf[$ctx.WeakTypeTag[$typeInner]]
      import $ctx.universe.{Type => _, internal => _, _}

      private val HKT = $ctx.weakTypeTag[Type.Ctor6.Stub[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $HKT]].tpe.typeArgs.last

      def apply[$A >: $L1 <: $U1: Type, $B >: $L2 <: $U2: Type, $C >: $L3 <: $U3: Type, $D >: $L4 <: $U4: Type, $E >: $L5 <: $U5: Type, $F >: $L6 <: $U6: Type]: Type[$appliedHKT] = 
        $typeValue

      def unapply[$A](A: Type[$A]): Option[($L1 <:??<: $U1, $L2 <:??<: $U2, $L3 <:??<: $U3, $L4 <:??<: $U4, $L5 <:??<: $U5, $L6 <:??<: $U6)] = {
        val A0 = A.asInstanceOf[$ctx.WeakTypeTag[$A]].tpe
        A0.dealias.widen.baseType(HKT.typeSymbol) match {
          case TypeRef(_, _, List(tp1, tp2, tp3, tp4, tp5, tp6)) =>
            _root_.scala.Some(
              (
                $ctx.WeakTypeTag(tp1.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L1, $U1],
                $ctx.WeakTypeTag(tp2.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L2, $U2],
                $ctx.WeakTypeTag(tp3.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L3, $U3],
                $ctx.WeakTypeTag(tp4.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L4, $U4],
                $ctx.WeakTypeTag(tp5.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L5, $U5],
                $ctx.WeakTypeTag(tp6.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L6, $U6]
              )
            )
          case _ =>
            if (A0.typeConstructor == HKT && A0.typeArgs.size == 6) {
              val Seq(tp1, tp2, tp3, tp4, tp5, tp6) = A0.typeArgs
              _root_.scala.Some(
                (
                  $ctx.WeakTypeTag(tp1.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L1, $U1],
                  $ctx.WeakTypeTag(tp2.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L2, $U2],
                  $ctx.WeakTypeTag(tp3.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L3, $U3],
                  $ctx.WeakTypeTag(tp4.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L4, $U4],
                  $ctx.WeakTypeTag(tp5.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L5, $U5],
                  $ctx.WeakTypeTag(tp6.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L6, $U6]
                )
              )
            }
            else None
        }
      }
    }
    """

    val result = suppressWarnings(c.typecheck(unchecked))

    log(
      s"""Cross-quotes ${paintExclDot(Console.BLUE)("Type.Ctor6.of")} expansion:
         |From: ${paintExclDot(Console.BLUE)(
          "Type.Ctor6.of"
        )}[${L1.tpe}, ${U1.tpe}, ${L2.tpe}, ${U2.tpe}, ${L3.tpe}, ${U3.tpe}, ${L4.tpe}, ${U4.tpe}, ${L5.tpe}, ${U5.tpe}, ${L6.tpe}, ${U6.tpe}, $HKT]
         |To: ${indent(showCodePretty(result, SyntaxHighlight.ANSI))}""".stripMargin
    )

    result
  } catch { case e: Throwable => reportIssue(e) }

  /* Replaces:
   *   Type.Ctor7.of[HKT]
   *   Type.Ctor7.UpperBounded.of[U1, U2, U3, U4, U5, U6, U7, HKT]
   *   Type.Ctor7.Bounded.of[L1, U1, L2, U2, L3, U3, L4, U4, L5, U5, L6, U6, L7, U7, HKT]
   * with:
   *   what we see in Quasiquote
   */
  // format: off
  def typeCtor7Impl[L1, U1 >: L1, L2, U2 >: L2, L3, U3 >: L3, L4, U4 >: L4, L5, U5 >: L5, L6, U6 >: L6, L7, U7 >: L7, HKT[_ >: L1 <: U1, _ >: L2 <: U2, _ >: L3 <: U3, _ >: L4 <: U4, _ >: L5 <: U5, _ >: L6 <: U6, _ >: L7 <: U7]](implicit
  // format: on
      L1: c.WeakTypeTag[L1],
      U1: c.WeakTypeTag[U1],
      L2: c.WeakTypeTag[L2],
      U2: c.WeakTypeTag[U2],
      L3: c.WeakTypeTag[L3],
      U3: c.WeakTypeTag[U3],
      L4: c.WeakTypeTag[L4],
      U4: c.WeakTypeTag[U4],
      L5: c.WeakTypeTag[L5],
      U5: c.WeakTypeTag[U5],
      L6: c.WeakTypeTag[L6],
      U6: c.WeakTypeTag[U6],
      L7: c.WeakTypeTag[L7],
      U7: c.WeakTypeTag[U7],
      HKTE: c.WeakTypeTag[HKT[?, ?, ?, ?, ?, ?, ?]]
  ): c.Tree = try {
    assert(HKTE.tpe.typeParams.size == 7, "HKT must have exactly seven type parameters")

    val HKT = HKTE.tpe.typeConstructor
    val A = freshTypeName("A") // 1
    val B = freshTypeName("B") // 2
    val C = freshTypeName("C") // 3
    val D = freshTypeName("D") // 4
    val E = freshTypeName("E") // 5
    val F = freshTypeName("F") // 6
    val G = freshTypeName("G") // 7
    val appliedHKT =
      tq"Type.Ctor7.Apply[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $L7, $U7, $HKT, $A, $B, $C, $D, $E, $F, $G]"

    val ctx = freshName("ctx")
    val typeValue = TypeWithUnderlyingInjected(ctx, appliedHKT, A, B, C, D, E, F, G)
    val convertProvidedTypesForCrossQuotes = freshName("convertProvidedTypesForCrossQuotes")
    val termInner = freshName("Inner")
    val typeInner = TypeName(freshName("Inner").toString)

    val unchecked = q"""
    new Type.Ctor7.Bounded[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $L7, $U7, $HKT] {
      private val $ctx = CrossQuotes.ctx[_root_.scala.reflect.macros.blackbox.Context]
      private implicit def $convertProvidedTypesForCrossQuotes[$typeInner](implicit $termInner: Type[$typeInner]): $ctx.WeakTypeTag[$typeInner] =
        $termInner.asInstanceOf[$ctx.WeakTypeTag[$typeInner]]
      import $ctx.universe.{Type => _, internal => _, _}

      private val HKT = $ctx.weakTypeTag[Type.Ctor7.Stub[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $L7, $U7, $HKT]].tpe.typeArgs.last

      def apply[$A >: $L1 <: $U1: Type, $B >: $L2 <: $U2: Type, $C >: $L3 <: $U3: Type, $D >: $L4 <: $U4: Type, $E >: $L5 <: $U5: Type, $F >: $L6 <: $U6: Type, $G >: $L7 <: $U7: Type]: Type[$appliedHKT] = 
        $typeValue

      def unapply[$A](A: Type[$A]): Option[($L1 <:??<: $U1, $L2 <:??<: $U2, $L3 <:??<: $U3, $L4 <:??<: $U4, $L5 <:??<: $U5, $L6 <:??<: $U6, $L7 <:??<: $U7)] = {
        val A0 = A.asInstanceOf[$ctx.WeakTypeTag[$A]].tpe
        A0.dealias.widen.baseType(HKT.typeSymbol) match {
          case TypeRef(_, _, List(tp1, tp2, tp3, tp4, tp5, tp6, tp7)) =>
            _root_.scala.Some(
              (
                $ctx.WeakTypeTag(tp1.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L1, $U1],
                $ctx.WeakTypeTag(tp2.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L2, $U2],
                $ctx.WeakTypeTag(tp3.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L3, $U3],
                $ctx.WeakTypeTag(tp4.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L4, $U4],
                $ctx.WeakTypeTag(tp5.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L5, $U5],
                $ctx.WeakTypeTag(tp6.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L6, $U6],
                $ctx.WeakTypeTag(tp7.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L7, $U7]
              )
            )
          case _ =>
            if (A0.typeConstructor == HKT && A0.typeArgs.size == 7) {
              val Seq(tp1, tp2, tp3, tp4, tp5, tp6, tp7) = A0.typeArgs
              _root_.scala.Some(
                (
                  $ctx.WeakTypeTag(tp1.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L1, $U1],
                  $ctx.WeakTypeTag(tp2.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L2, $U2],
                  $ctx.WeakTypeTag(tp3.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L3, $U3],
                  $ctx.WeakTypeTag(tp4.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L4, $U4],
                  $ctx.WeakTypeTag(tp5.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L5, $U5],
                  $ctx.WeakTypeTag(tp6.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L6, $U6],
                  $ctx.WeakTypeTag(tp7.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L7, $U7]
                )
              )
            }
            else None
        }
      }
    }
    """

    val result = suppressWarnings(c.typecheck(unchecked))

    log(
      s"""Cross-quotes ${paintExclDot(Console.BLUE)("Type.Ctor7.of")} expansion:
         |From: ${paintExclDot(Console.BLUE)(
          "Type.Ctor7.of"
        )}[${L1.tpe}, ${U1.tpe}, ${L2.tpe}, ${U2.tpe}, ${L3.tpe}, ${U3.tpe}, ${L4.tpe}, ${U4.tpe}, ${L5.tpe}, ${U5.tpe}, ${L6.tpe}, ${U6.tpe}, ${L7.tpe}, ${U7.tpe}, $HKT]
         |To: ${indent(showCodePretty(result, SyntaxHighlight.ANSI))}""".stripMargin
    )

    result
  } catch { case e: Throwable => reportIssue(e) }

  /* Replaces:
   *   Type.Ctor8.of[HKT]
   *   Type.Ctor8.UpperBounded.of[U1, U2, U3, U4, U5, U6, U7, U8, HKT]
   *   Type.Ctor8.Bounded.of[L1, U1, L2, U2, L3, U3, L4, U4, L5, U5, L6, U6, L7, U7, L8, U8, HKT]
   * with:
   *   what we see in Quasiquote
   */
  // format: off
  def typeCtor8Impl[L1, U1 >: L1, L2, U2 >: L2, L3, U3 >: L3, L4, U4 >: L4, L5, U5 >: L5, L6, U6 >: L6, L7, U7 >: L7, L8, U8 >: L8, HKT[_ >: L1 <: U1, _ >: L2 <: U2, _ >: L3 <: U3, _ >: L4 <: U4, _ >: L5 <: U5, _ >: L6 <: U6, _ >: L7 <: U7, _ >: L8 <: U8]](implicit
  // format: on
      L1: c.WeakTypeTag[L1],
      U1: c.WeakTypeTag[U1],
      L2: c.WeakTypeTag[L2],
      U2: c.WeakTypeTag[U2],
      L3: c.WeakTypeTag[L3],
      U3: c.WeakTypeTag[U3],
      L4: c.WeakTypeTag[L4],
      U4: c.WeakTypeTag[U4],
      L5: c.WeakTypeTag[L5],
      U5: c.WeakTypeTag[U5],
      L6: c.WeakTypeTag[L6],
      U6: c.WeakTypeTag[U6],
      L7: c.WeakTypeTag[L7],
      U7: c.WeakTypeTag[U7],
      L8: c.WeakTypeTag[L8],
      U8: c.WeakTypeTag[U8],
      HKTE: c.WeakTypeTag[HKT[?, ?, ?, ?, ?, ?, ?, ?]]
  ): c.Tree = try {
    assert(HKTE.tpe.typeParams.size == 8, "HKT must have exactly eight type parameters")

    val HKT = HKTE.tpe.typeConstructor
    val A = freshTypeName("A") // 1
    val B = freshTypeName("B") // 2
    val C = freshTypeName("C") // 3
    val D = freshTypeName("D") // 4
    val E = freshTypeName("E") // 5
    val F = freshTypeName("F") // 6
    val G = freshTypeName("G") // 7
    val H = freshTypeName("H") // 8
    val appliedHKT =
      tq"Type.Ctor8.Apply[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $L7, $U7, $L8, $U8, $HKT, $A, $B, $C, $D, $E, $F, $G, $H]"

    val ctx = freshName("ctx")
    val typeValue = TypeWithUnderlyingInjected(ctx, appliedHKT, A, B, C, D, E, F, G, H)
    val convertProvidedTypesForCrossQuotes = freshName("convertProvidedTypesForCrossQuotes")
    val termInner = freshName("Inner")
    val typeInner = TypeName(freshName("Inner").toString)

    val unchecked = q"""
    new Type.Ctor8.Bounded[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $L7, $U7, $L8, $U8, $HKT] {
      private val $ctx = CrossQuotes.ctx[_root_.scala.reflect.macros.blackbox.Context]
      private implicit def $convertProvidedTypesForCrossQuotes[$typeInner](implicit $termInner: Type[$typeInner]): $ctx.WeakTypeTag[$typeInner] =
        $termInner.asInstanceOf[$ctx.WeakTypeTag[$typeInner]]
      import $ctx.universe.{Type => _, internal => _, _}

      private val HKT = $ctx.weakTypeTag[Type.Ctor8.Stub[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $L7, $U7, $L8, $U8, $HKT]].tpe.typeArgs.last

      def apply[$A >: $L1 <: $U1: Type, $B >: $L2 <: $U2: Type, $C >: $L3 <: $U3: Type, $D >: $L4 <: $U4: Type, $E >: $L5 <: $U5: Type, $F >: $L6 <: $U6: Type, $G >: $L7 <: $U7: Type, $H >: $L8 <: $U8: Type]: Type[$appliedHKT] = 
        $typeValue

      def unapply[$A](A: Type[$A]): Option[($L1 <:??<: $U1, $L2 <:??<: $U2, $L3 <:??<: $U3, $L4 <:??<: $U4, $L5 <:??<: $U5, $L6 <:??<: $U6, $L7 <:??<: $U7, $L8 <:??<: $U8)] = {
        val A0 = A.asInstanceOf[$ctx.WeakTypeTag[$A]].tpe
        A0.dealias.widen.baseType(HKT.typeSymbol) match {
          case TypeRef(_, _, List(tp1, tp2, tp3, tp4, tp5, tp6, tp7, tp8)) =>
            _root_.scala.Some(
              (
                $ctx.WeakTypeTag(tp1.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L1, $U1],
                $ctx.WeakTypeTag(tp2.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L2, $U2],
                $ctx.WeakTypeTag(tp3.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L3, $U3],
                $ctx.WeakTypeTag(tp4.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L4, $U4],
                $ctx.WeakTypeTag(tp5.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L5, $U5],
                $ctx.WeakTypeTag(tp6.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L6, $U6],
                $ctx.WeakTypeTag(tp7.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L7, $U7],
                $ctx.WeakTypeTag(tp8.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L8, $U8]
              )
            )
          case _ =>
            if (A0.typeConstructor == HKT && A0.typeArgs.size == 8) {
              val Seq(tp1, tp2, tp3, tp4, tp5, tp6, tp7, tp8) = A0.typeArgs
              _root_.scala.Some(
                (
                  $ctx.WeakTypeTag(tp1.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L1, $U1],
                  $ctx.WeakTypeTag(tp2.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L2, $U2],
                  $ctx.WeakTypeTag(tp3.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L3, $U3],
                  $ctx.WeakTypeTag(tp4.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L4, $U4],
                  $ctx.WeakTypeTag(tp5.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L5, $U5],
                  $ctx.WeakTypeTag(tp6.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L6, $U6],
                  $ctx.WeakTypeTag(tp7.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L7, $U7],
                  $ctx.WeakTypeTag(tp8.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L8, $U8]
                )
              )
            }
            else None
        }
      }
    }
    """

    val result = suppressWarnings(c.typecheck(unchecked))

    log(
      s"""Cross-quotes ${paintExclDot(Console.BLUE)("Type.Ctor8.of")} expansion:
         |From: ${paintExclDot(Console.BLUE)(
          "Type.Ctor8.of"
        )}[${L1.tpe}, ${U1.tpe}, ${L2.tpe}, ${U2.tpe}, ${L3.tpe}, ${U3.tpe}, ${L4.tpe}, ${U4.tpe}, ${L5.tpe}, ${U5.tpe}, ${L6.tpe}, ${U6.tpe}, ${L7.tpe}, ${U7.tpe}, ${L8.tpe}, ${U8.tpe}, $HKT]
         |To: ${indent(showCodePretty(result, SyntaxHighlight.ANSI))}""".stripMargin
    )

    result
  } catch { case e: Throwable => reportIssue(e) }

  /* Replaces:
   *   Type.Ctor9.of[HKT]
   *   Type.Ctor9.UpperBounded.of[U1, U2, U3, U4, U5, U6, U7, U8, U9, HKT]
   *   Type.Ctor9.Bounded.of[L1, U1, L2, U2, L3, U3, L4, U4, L5, U5, L6, U6, L7, U7, L8, U8, L9, U9, HKT]
   * with:
   *   what we see in Quasiquote
   */
  // format: off
  def typeCtor9Impl[L1, U1 >: L1, L2, U2 >: L2, L3, U3 >: L3, L4, U4 >: L4, L5, U5 >: L5, L6, U6 >: L6, L7, U7 >: L7, L8, U8 >: L8, L9, U9 >: L9, HKT[_ >: L1 <: U1, _ >: L2 <: U2, _ >: L3 <: U3, _ >: L4 <: U4, _ >: L5 <: U5, _ >: L6 <: U6, _ >: L7 <: U7, _ >: L8 <: U8, _ >: L9 <: U9]](implicit
  // format: on
      L1: c.WeakTypeTag[L1],
      U1: c.WeakTypeTag[U1],
      L2: c.WeakTypeTag[L2],
      U2: c.WeakTypeTag[U2],
      L3: c.WeakTypeTag[L3],
      U3: c.WeakTypeTag[U3],
      L4: c.WeakTypeTag[L4],
      U4: c.WeakTypeTag[U4],
      L5: c.WeakTypeTag[L5],
      U5: c.WeakTypeTag[U5],
      L6: c.WeakTypeTag[L6],
      U6: c.WeakTypeTag[U6],
      L7: c.WeakTypeTag[L7],
      U7: c.WeakTypeTag[U7],
      L8: c.WeakTypeTag[L8],
      U8: c.WeakTypeTag[U8],
      L9: c.WeakTypeTag[L9],
      U9: c.WeakTypeTag[U9],
      HKTE: c.WeakTypeTag[HKT[?, ?, ?, ?, ?, ?, ?, ?, ?]]
  ): c.Tree = try {
    assert(HKTE.tpe.typeParams.size == 9, "HKT must have exactly nine type parameters")

    val HKT = HKTE.tpe.typeConstructor
    val A = freshTypeName("A") // 1
    val B = freshTypeName("B") // 2
    val C = freshTypeName("C") // 3
    val D = freshTypeName("D") // 4
    val E = freshTypeName("E") // 5
    val F = freshTypeName("F") // 6
    val G = freshTypeName("G") // 7
    val H = freshTypeName("H") // 8
    val I = freshTypeName("I") // 9
    val appliedHKT =
      tq"Type.Ctor9.Apply[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $L7, $U7, $L8, $U8, $L9, $U9, $HKT, $A, $B, $C, $D, $E, $F, $G, $H, $I]"

    val ctx = freshName("ctx")
    val typeValue = TypeWithUnderlyingInjected(ctx, appliedHKT, A, B, C, D, E, F, G, H, I)
    val convertProvidedTypesForCrossQuotes = freshName("convertProvidedTypesForCrossQuotes")
    val termInner = freshName("Inner")
    val typeInner = TypeName(freshName("Inner").toString)

    val unchecked = q"""
    new Type.Ctor9.Bounded[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $L7, $U7, $L8, $U8, $L9, $U9, $HKT] {
      private val $ctx = CrossQuotes.ctx[_root_.scala.reflect.macros.blackbox.Context]
      private implicit def $convertProvidedTypesForCrossQuotes[$typeInner](implicit $termInner: Type[$typeInner]): $ctx.WeakTypeTag[$typeInner] =
        $termInner.asInstanceOf[$ctx.WeakTypeTag[$typeInner]]
      import $ctx.universe.{Type => _, internal => _, _}

      private val HKT = $ctx.weakTypeTag[Type.Ctor9.Stub[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $L7, $U7, $L8, $U8, $L9, $U9, $HKT]].tpe.typeArgs.last

      def apply[$A >: $L1 <: $U1: Type, $B >: $L2 <: $U2: Type, $C >: $L3 <: $U3: Type, $D >: $L4 <: $U4: Type, $E >: $L5 <: $U5: Type, $F >: $L6 <: $U6: Type, $G >: $L7 <: $U7: Type, $H >: $L8 <: $U8: Type, $I >: $L9 <: $U9: Type]: Type[$appliedHKT] = 
        $typeValue

      def unapply[$A](A: Type[$A]): Option[($L1 <:??<: $U1, $L2 <:??<: $U2, $L3 <:??<: $U3, $L4 <:??<: $U4, $L5 <:??<: $U5, $L6 <:??<: $U6, $L7 <:??<: $U7, $L8 <:??<: $U8, $L9 <:??<: $U9)] = {
        val A0 = A.asInstanceOf[$ctx.WeakTypeTag[$A]].tpe
        A0.dealias.widen.baseType(HKT.typeSymbol) match {
          case TypeRef(_, _, List(tp1, tp2, tp3, tp4, tp5, tp6, tp7, tp8, tp9)) =>
            _root_.scala.Some(
              (
                $ctx.WeakTypeTag(tp1.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L1, $U1],
                $ctx.WeakTypeTag(tp2.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L2, $U2],
                $ctx.WeakTypeTag(tp3.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L3, $U3],
                $ctx.WeakTypeTag(tp4.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L4, $U4],
                $ctx.WeakTypeTag(tp5.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L5, $U5],
                $ctx.WeakTypeTag(tp6.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L6, $U6],
                $ctx.WeakTypeTag(tp7.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L7, $U7],
                $ctx.WeakTypeTag(tp8.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L8, $U8],
                $ctx.WeakTypeTag(tp9.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L9, $U9]
              )
            )
          case _ =>
            if (A0.typeConstructor == HKT && A0.typeArgs.size == 9) {
              val Seq(tp1, tp2, tp3, tp4, tp5, tp6, tp7, tp8, tp9) = A0.typeArgs
              _root_.scala.Some(
                (
                  $ctx.WeakTypeTag(tp1.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L1, $U1],
                  $ctx.WeakTypeTag(tp2.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L2, $U2],
                  $ctx.WeakTypeTag(tp3.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L3, $U3],
                  $ctx.WeakTypeTag(tp4.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L4, $U4],
                  $ctx.WeakTypeTag(tp5.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L5, $U5],
                  $ctx.WeakTypeTag(tp6.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L6, $U6],
                  $ctx.WeakTypeTag(tp7.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L7, $U7],
                  $ctx.WeakTypeTag(tp8.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L8, $U8],
                  $ctx.WeakTypeTag(tp9.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L9, $U9]
                )
              )
            }
            else None
        }
      }
    }
    """

    val result = suppressWarnings(c.typecheck(unchecked))

    log(
      s"""Cross-quotes ${paintExclDot(Console.BLUE)("Type.Ctor9.of")} expansion:
         |From: ${paintExclDot(Console.BLUE)(
          "Type.Ctor9.of"
        )}[${L1.tpe}, ${U1.tpe}, ${L2.tpe}, ${U2.tpe}, ${L3.tpe}, ${U3.tpe}, ${L4.tpe}, ${U4.tpe}, ${L5.tpe}, ${U5.tpe}, ${L6.tpe}, ${U6.tpe}, ${L7.tpe}, ${U7.tpe}, ${L8.tpe}, ${U8.tpe}, ${L9.tpe}, ${U9.tpe}, $HKT]
         |To: ${indent(showCodePretty(result, SyntaxHighlight.ANSI))}""".stripMargin
    )

    result
  } catch { case e: Throwable => reportIssue(e) }

  /* Replaces:
   *   Type.Ctor10.of[HKT]
   *   Type.Ctor10.UpperBounded.of[U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, HKT]
   *   Type.Ctor10.Bounded.of[L1, U1, L2, U2, L3, U3, L4, U4, L5, U5, L6, U6, L7, U7, L8, U8, L9, U9, L10, U10, HKT]
   * with:
   *   what we see in Quasiquote
   */
  // format: off
    def typeCtor10Impl[L1, U1 >: L1, L2, U2 >: L2, L3, U3 >: L3, L4, U4 >: L4, L5, U5 >: L5, L6, U6 >: L6, L7, U7 >: L7, L8, U8 >: L8, L9, U9 >: L9, L10, U10 >: L10, HKT[_ >: L1 <: U1, _ >: L2 <: U2, _ >: L3 <: U3, _ >: L4 <: U4, _ >: L5 <: U5, _ >: L6 <: U6, _ >: L7 <: U7, _ >: L8 <: U8, _ >: L9 <: U9, _ >: L10 <: U10]](implicit
  // format: on
      L1: c.WeakTypeTag[L1],
      U1: c.WeakTypeTag[U1],
      L2: c.WeakTypeTag[L2],
      U2: c.WeakTypeTag[U2],
      L3: c.WeakTypeTag[L3],
      U3: c.WeakTypeTag[U3],
      L4: c.WeakTypeTag[L4],
      U4: c.WeakTypeTag[U4],
      L5: c.WeakTypeTag[L5],
      U5: c.WeakTypeTag[U5],
      L6: c.WeakTypeTag[L6],
      U6: c.WeakTypeTag[U6],
      L7: c.WeakTypeTag[L7],
      U7: c.WeakTypeTag[U7],
      L8: c.WeakTypeTag[L8],
      U8: c.WeakTypeTag[U8],
      L9: c.WeakTypeTag[L9],
      U9: c.WeakTypeTag[U9],
      L10: c.WeakTypeTag[L10],
      U10: c.WeakTypeTag[U10],
      HKTE: c.WeakTypeTag[HKT[?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]
  ): c.Tree = try {
    assert(HKTE.tpe.typeParams.size == 10, "HKT must have exactly ten type parameters")

    val HKT = HKTE.tpe.typeConstructor
    val A = freshTypeName("A") // 1
    val B = freshTypeName("B") // 2
    val C = freshTypeName("C") // 3
    val D = freshTypeName("D") // 4
    val E = freshTypeName("E") // 5
    val F = freshTypeName("F") // 6
    val G = freshTypeName("G") // 7
    val H = freshTypeName("H") // 8
    val I = freshTypeName("I") // 9
    val J = freshTypeName("J") // 10
    val appliedHKT =
      tq"Type.Ctor10.Apply[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $L7, $U7, $L8, $U8, $L9, $U9, $L10, $U10, $HKT, $A, $B, $C, $D, $E, $F, $G, $H, $I, $J]"

    val ctx = freshName("ctx")
    val typeValue = TypeWithUnderlyingInjected(ctx, appliedHKT, A, B, C, D, E, F, G, H, I, J)
    val convertProvidedTypesForCrossQuotes = freshName("convertProvidedTypesForCrossQuotes")
    val termInner = freshName("Inner")
    val typeInner = TypeName(freshName("Inner").toString)

    val unchecked = q"""
    new Type.Ctor10.Bounded[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $L7, $U7, $L8, $U8, $L9, $U9, $L10, $U10, $HKT] {
      private val $ctx = CrossQuotes.ctx[_root_.scala.reflect.macros.blackbox.Context]
      private implicit def $convertProvidedTypesForCrossQuotes[$typeInner](implicit $termInner: Type[$typeInner]): $ctx.WeakTypeTag[$typeInner] =
        $termInner.asInstanceOf[$ctx.WeakTypeTag[$typeInner]]
      import $ctx.universe.{Type => _, internal => _, _}

      private val HKT = $ctx.weakTypeTag[Type.Ctor10.Stub[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $L7, $U7, $L8, $U8, $L9, $U9, $L10, $U10, $HKT]].tpe.typeArgs.last

      def apply[$A >: $L1 <: $U1: Type, $B >: $L2 <: $U2: Type, $C >: $L3 <: $U3: Type, $D >: $L4 <: $U4: Type, $E >: $L5 <: $U5: Type, $F >: $L6 <: $U6: Type, $G >: $L7 <: $U7: Type, $H >: $L8 <: $U8: Type, $I >: $L9 <: $U9: Type, $J >: $L10 <: $U10: Type]: Type[$appliedHKT] = 
        $typeValue

      def unapply[$A](A: Type[$A]): Option[($L1 <:??<: $U1, $L2 <:??<: $U2, $L3 <:??<: $U3, $L4 <:??<: $U4, $L5 <:??<: $U5, $L6 <:??<: $U6, $L7 <:??<: $U7, $L8 <:??<: $U8, $L9 <:??<: $U9, $L10 <:??<: $U10)] = {
        val A0 = A.asInstanceOf[$ctx.WeakTypeTag[$A]].tpe
        A0.dealias.widen.baseType(HKT.typeSymbol) match {
          case TypeRef(_, _, List(tp1, tp2, tp3, tp4, tp5, tp6, tp7, tp8, tp9, tp10)) =>
            _root_.scala.Some(
              (
                $ctx.WeakTypeTag(tp1.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L1, $U1],
                $ctx.WeakTypeTag(tp2.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L2, $U2],
                $ctx.WeakTypeTag(tp3.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L3, $U3],
                $ctx.WeakTypeTag(tp4.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L4, $U4],
                $ctx.WeakTypeTag(tp5.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L5, $U5],
                $ctx.WeakTypeTag(tp6.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L6, $U6],
                $ctx.WeakTypeTag(tp7.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L7, $U7],
                $ctx.WeakTypeTag(tp8.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L8, $U8],
                $ctx.WeakTypeTag(tp9.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L9, $U9],
                $ctx.WeakTypeTag(tp10.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L10, $U10]
              )
            )
          case _ =>
            if (A0.typeConstructor == HKT && A0.typeArgs.size == 10) {
              val Seq(tp1, tp2, tp3, tp4, tp5, tp6, tp7, tp8, tp9, tp10) = A0.typeArgs
              _root_.scala.Some(
                (
                  $ctx.WeakTypeTag(tp1.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L1, $U1],
                  $ctx.WeakTypeTag(tp2.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L2, $U2],
                  $ctx.WeakTypeTag(tp3.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L3, $U3],
                  $ctx.WeakTypeTag(tp4.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L4, $U4],
                  $ctx.WeakTypeTag(tp5.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L5, $U5],
                  $ctx.WeakTypeTag(tp6.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L6, $U6],
                  $ctx.WeakTypeTag(tp7.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L7, $U7],
                  $ctx.WeakTypeTag(tp8.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L8, $U8],
                  $ctx.WeakTypeTag(tp9.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L9, $U9],
                  $ctx.WeakTypeTag(tp10.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L10, $U10]
                )
              )
            }
            else None
        }
      }
    }
    """

    val result = suppressWarnings(c.typecheck(unchecked))

    log(
      s"""Cross-quotes ${paintExclDot(Console.BLUE)("Type.Ctor10.of")} expansion:
         |From: ${paintExclDot(Console.BLUE)(
          "Type.Ctor10.of"
        )}[${L1.tpe}, ${U1.tpe}, ${L2.tpe}, ${U2.tpe}, ${L3.tpe}, ${U3.tpe}, ${L4.tpe}, ${U4.tpe}, ${L5.tpe}, ${U5.tpe}, ${L6.tpe}, ${U6.tpe}, ${L7.tpe}, ${U7.tpe}, ${L8.tpe}, ${U8.tpe}, ${L9.tpe}, ${U9.tpe}, ${L10.tpe}, ${U10.tpe}, $HKT]
         |To: ${indent(showCodePretty(result, SyntaxHighlight.ANSI))}""".stripMargin
    )

    result
  } catch { case e: Throwable => reportIssue(e) }

  /* Replaces:
   *   Type.Ctor11.of[HKT]
   *   Type.Ctor11.UpperBounded.of[U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, HKT]
   *   Type.Ctor11.Bounded.of[L1, U1, L2, U2, L3, U3, L4, U4, L5, U5, L6, U6, L7, U7, L8, U8, L9, U9, L10, U10, L11, U11, HKT]
   * with:
   *   what we see in Quasiquote
   */
  // format: off
  def typeCtor11Impl[L1, U1 >: L1, L2, U2 >: L2, L3, U3 >: L3, L4, U4 >: L4, L5, U5 >: L5, L6, U6 >: L6, L7, U7 >: L7, L8, U8 >: L8, L9, U9 >: L9, L10, U10 >: L10, L11, U11 >: L11, HKT[_ >: L1 <: U1, _ >: L2 <: U2, _ >: L3 <: U3, _ >: L4 <: U4, _ >: L5 <: U5, _ >: L6 <: U6, _ >: L7 <: U7, _ >: L8 <: U8, _ >: L9 <: U9, _ >: L10 <: U10, _ >: L11 <: U11]](implicit
  // format: on
      L1: c.WeakTypeTag[L1],
      U1: c.WeakTypeTag[U1],
      L2: c.WeakTypeTag[L2],
      U2: c.WeakTypeTag[U2],
      L3: c.WeakTypeTag[L3],
      U3: c.WeakTypeTag[U3],
      L4: c.WeakTypeTag[L4],
      U4: c.WeakTypeTag[U4],
      L5: c.WeakTypeTag[L5],
      U5: c.WeakTypeTag[U5],
      L6: c.WeakTypeTag[L6],
      U6: c.WeakTypeTag[U6],
      L7: c.WeakTypeTag[L7],
      U7: c.WeakTypeTag[U7],
      L8: c.WeakTypeTag[L8],
      U8: c.WeakTypeTag[U8],
      L9: c.WeakTypeTag[L9],
      U9: c.WeakTypeTag[U9],
      L10: c.WeakTypeTag[L10],
      U10: c.WeakTypeTag[U10],
      L11: c.WeakTypeTag[L11],
      U11: c.WeakTypeTag[U11],
      HKTE: c.WeakTypeTag[HKT[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]
  ): c.Tree = try {
    assert(HKTE.tpe.typeParams.size == 11, "HKT must have exactly eleven type parameters")

    val HKT = HKTE.tpe.typeConstructor
    val A = freshTypeName("A") // 1
    val B = freshTypeName("B") // 2
    val C = freshTypeName("C") // 3
    val D = freshTypeName("D") // 4
    val E = freshTypeName("E") // 5
    val F = freshTypeName("F") // 6
    val G = freshTypeName("G") // 7
    val H = freshTypeName("H") // 8
    val I = freshTypeName("I") // 9
    val J = freshTypeName("J") // 10
    val K = freshTypeName("K") // 11
    val appliedHKT =
      tq"Type.Ctor11.Apply[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $L7, $U7, $L8, $U8, $L9, $U9, $L10, $U10, $L11, $U11, $HKT, $A, $B, $C, $D, $E, $F, $G, $H, $I, $J, $K]"

    val ctx = freshName("ctx")
    val typeValue = TypeWithUnderlyingInjected(ctx, appliedHKT, A, B, C, D, E, F, G, H, I, J, K)
    val convertProvidedTypesForCrossQuotes = freshName("convertProvidedTypesForCrossQuotes")
    val termInner = freshName("Inner")
    val typeInner = TypeName(freshName("Inner").toString)

    val unchecked = q"""
    new Type.Ctor11.Bounded[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $L7, $U7, $L8, $U8, $L9, $U9, $L10, $U10, $L11, $U11, $HKT] {
      private val $ctx = CrossQuotes.ctx[_root_.scala.reflect.macros.blackbox.Context]
      private implicit def $convertProvidedTypesForCrossQuotes[$typeInner](implicit $termInner: Type[$typeInner]): $ctx.WeakTypeTag[$typeInner] =
        $termInner.asInstanceOf[$ctx.WeakTypeTag[$typeInner]]
      import $ctx.universe.{Type => _, internal => _, _}

      private val HKT = $ctx.weakTypeTag[Type.Ctor11.Stub[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $L7, $U7, $L8, $U8, $L9, $U9, $L10, $U10, $L11, $U11, $HKT]].tpe.typeArgs.last

      def apply[$A >: $L1 <: $U1: Type, $B >: $L2 <: $U2: Type, $C >: $L3 <: $U3: Type, $D >: $L4 <: $U4: Type, $E >: $L5 <: $U5: Type, $F >: $L6 <: $U6: Type, $G >: $L7 <: $U7: Type, $H >: $L8 <: $U8: Type, $I >: $L9 <: $U9: Type, $J >: $L10 <: $U10: Type, $K >: $L11 <: $U11: Type]: Type[$appliedHKT] = 
        $typeValue

      def unapply[$A](A: Type[$A]): Option[($L1 <:??<: $U1, $L2 <:??<: $U2, $L3 <:??<: $U3, $L4 <:??<: $U4, $L5 <:??<: $U5, $L6 <:??<: $U6, $L7 <:??<: $U7, $L8 <:??<: $U8, $L9 <:??<: $U9, $L10 <:??<: $U10, $L11 <:??<: $U11)] = {
        val A0 = A.asInstanceOf[$ctx.WeakTypeTag[$A]].tpe
        A0.dealias.widen.baseType(HKT.typeSymbol) match {
          case TypeRef(_, _, List(tp1, tp2, tp3, tp4, tp5, tp6, tp7, tp8, tp9, tp10, tp11)) =>
            _root_.scala.Some(
              (
                $ctx.WeakTypeTag(tp1.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L1, $U1],
                $ctx.WeakTypeTag(tp2.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L2, $U2],
                $ctx.WeakTypeTag(tp3.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L3, $U3],
                $ctx.WeakTypeTag(tp4.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L4, $U4],
                $ctx.WeakTypeTag(tp5.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L5, $U5],
                $ctx.WeakTypeTag(tp6.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L6, $U6],
                $ctx.WeakTypeTag(tp7.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L7, $U7],
                $ctx.WeakTypeTag(tp8.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L8, $U8],
                $ctx.WeakTypeTag(tp9.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L9, $U9],
                $ctx.WeakTypeTag(tp10.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L10, $U10],
                $ctx.WeakTypeTag(tp11.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L11, $U11]
              )
            )
          case _ =>
            if (A0.typeConstructor == HKT && A0.typeArgs.size == 11) {
              val Seq(tp1, tp2, tp3, tp4, tp5, tp6, tp7, tp8, tp9, tp10, tp11) = A0.typeArgs
              _root_.scala.Some(
                (
                  $ctx.WeakTypeTag(tp1.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L1, $U1],
                  $ctx.WeakTypeTag(tp2.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L2, $U2],
                  $ctx.WeakTypeTag(tp3.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L3, $U3],
                  $ctx.WeakTypeTag(tp4.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L4, $U4],
                  $ctx.WeakTypeTag(tp5.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L5, $U5],
                  $ctx.WeakTypeTag(tp6.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L6, $U6],
                  $ctx.WeakTypeTag(tp7.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L7, $U7],
                  $ctx.WeakTypeTag(tp8.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L8, $U8],
                  $ctx.WeakTypeTag(tp9.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L9, $U9],
                  $ctx.WeakTypeTag(tp10.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L10, $U10],
                  $ctx.WeakTypeTag(tp11.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L11, $U11]
                )
              )
            }
            else None
        }
      }
    }
    """

    val result = suppressWarnings(c.typecheck(unchecked))

    log(
      s"""Cross-quotes ${paintExclDot(Console.BLUE)("Type.Ctor11.of")} expansion:
         |From: ${paintExclDot(Console.BLUE)(
          "Type.Ctor11.of"
        )}[${L1.tpe}, ${U1.tpe}, ${L2.tpe}, ${U2.tpe}, ${L3.tpe}, ${U3.tpe}, ${L4.tpe}, ${U4.tpe}, ${L5.tpe}, ${U5.tpe}, ${L6.tpe}, ${U6.tpe}, ${L7.tpe}, ${U7.tpe}, ${L8.tpe}, ${U8.tpe}, ${L9.tpe}, ${U9.tpe}, ${L10.tpe}, ${U10.tpe}, ${L11.tpe}, ${U11.tpe}, $HKT]
         |To: ${indent(showCodePretty(result, SyntaxHighlight.ANSI))}""".stripMargin
    )

    result
  } catch { case e: Throwable => reportIssue(e) }

  /* Replaces:
   *   Type.Ctor12.of[HKT]
   *   Type.Ctor12.UpperBounded.of[U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12, HKT]
   *   Type.Ctor12.Bounded.of[L1, U1, L2, U2, L3, U3, L4, U4, L5, U5, L6, U6, L7, U7, L8, U8, L9, U9, L10, U10, L11, U11, L12, U12, HKT]
   * with:
   *   what we see in Quasiquote
   */
  // format: off
  def typeCtor12Impl[L1, U1 >: L1, L2, U2 >: L2, L3, U3 >: L3, L4, U4 >: L4, L5, U5 >: L5, L6, U6 >: L6, L7, U7 >: L7, L8, U8 >: L8, L9, U9 >: L9, L10, U10 >: L10, L11, U11 >: L11, L12, U12 >: L12, HKT[_ >: L1 <: U1, _ >: L2 <: U2, _ >: L3 <: U3, _ >: L4 <: U4, _ >: L5 <: U5, _ >: L6 <: U6, _ >: L7 <: U7, _ >: L8 <: U8, _ >: L9 <: U9, _ >: L10 <: U10, _ >: L11 <: U11, _ >: L12 <: U12]](implicit
  // format: on
      L1: c.WeakTypeTag[L1],
      U1: c.WeakTypeTag[U1],
      L2: c.WeakTypeTag[L2],
      U2: c.WeakTypeTag[U2],
      L3: c.WeakTypeTag[L3],
      U3: c.WeakTypeTag[U3],
      L4: c.WeakTypeTag[L4],
      U4: c.WeakTypeTag[U4],
      L5: c.WeakTypeTag[L5],
      U5: c.WeakTypeTag[U5],
      L6: c.WeakTypeTag[L6],
      U6: c.WeakTypeTag[U6],
      L7: c.WeakTypeTag[L7],
      U7: c.WeakTypeTag[U7],
      L8: c.WeakTypeTag[L8],
      U8: c.WeakTypeTag[U8],
      L9: c.WeakTypeTag[L9],
      U9: c.WeakTypeTag[U9],
      L10: c.WeakTypeTag[L10],
      U10: c.WeakTypeTag[U10],
      L11: c.WeakTypeTag[L11],
      U11: c.WeakTypeTag[U11],
      L12: c.WeakTypeTag[L12],
      U12: c.WeakTypeTag[U12],
      HKTE: c.WeakTypeTag[HKT[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]
  ): c.Tree = try {
    assert(HKTE.tpe.typeParams.size == 12, "HKT must have exactly twelve type parameters")

    val HKT = HKTE.tpe.typeConstructor
    val A = freshTypeName("A") // 1
    val B = freshTypeName("B") // 2
    val C = freshTypeName("C") // 3
    val D = freshTypeName("D") // 4
    val E = freshTypeName("E") // 5
    val F = freshTypeName("F") // 6
    val G = freshTypeName("G") // 7
    val H = freshTypeName("H") // 8
    val I = freshTypeName("I") // 9
    val J = freshTypeName("J") // 10
    val K = freshTypeName("K") // 11
    val L = freshTypeName("L") // 12
    val appliedHKT =
      tq"Type.Ctor12.Apply[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $L7, $U7, $L8, $U8, $L9, $U9, $L10, $U10, $L11, $U11, $L12, $U12, $HKT, $A, $B, $C, $D, $E, $F, $G, $H, $I, $J, $K, $L]"

    val ctx = freshName("ctx")
    val typeValue = TypeWithUnderlyingInjected(ctx, appliedHKT, A, B, C, D, E, F, G, H, I, J, K, L)
    val convertProvidedTypesForCrossQuotes = freshName("convertProvidedTypesForCrossQuotes")
    val termInner = freshName("Inner")
    val typeInner = TypeName(freshName("Inner").toString)

    val unchecked = q"""
    new Type.Ctor12.Bounded[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $L7, $U7, $L8, $U8, $L9, $U9, $L10, $U10, $L11, $U11, $L12, $U12, $HKT] {
      private val $ctx = CrossQuotes.ctx[_root_.scala.reflect.macros.blackbox.Context]
      private implicit def $convertProvidedTypesForCrossQuotes[$typeInner](implicit $termInner: Type[$typeInner]): $ctx.WeakTypeTag[$typeInner] =
        $termInner.asInstanceOf[$ctx.WeakTypeTag[$typeInner]]
      import $ctx.universe.{Type => _, internal => _, _}

      private val HKT = $ctx.weakTypeTag[Type.Ctor12.Stub[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $L7, $U7, $L8, $U8, $L9, $U9, $L10, $U10, $L11, $U11, $L12, $U12, $HKT]].tpe.typeArgs.last

      def apply[$A >: $L1 <: $U1: Type, $B >: $L2 <: $U2: Type, $C >: $L3 <: $U3: Type, $D >: $L4 <: $U4: Type, $E >: $L5 <: $U5: Type, $F >: $L6 <: $U6: Type, $G >: $L7 <: $U7: Type, $H >: $L8 <: $U8: Type, $I >: $L9 <: $U9: Type, $J >: $L10 <: $U10: Type, $K >: $L11 <: $U11: Type, $L >: $L12 <: $U12: Type]: Type[$appliedHKT] = 
        $typeValue

      def unapply[$A](A: Type[$A]): Option[($L1 <:??<: $U1, $L2 <:??<: $U2, $L3 <:??<: $U3, $L4 <:??<: $U4, $L5 <:??<: $U5, $L6 <:??<: $U6, $L7 <:??<: $U7, $L8 <:??<: $U8, $L9 <:??<: $U9, $L10 <:??<: $U10, $L11 <:??<: $U11, $L12 <:??<: $U12)] = {
        val A0 = A.asInstanceOf[$ctx.WeakTypeTag[$A]].tpe
        A0.dealias.widen.baseType(HKT.typeSymbol) match {
          case TypeRef(_, _, List(tp1, tp2, tp3, tp4, tp5, tp6, tp7, tp8, tp9, tp10, tp11, tp12)) =>
            _root_.scala.Some(
              (
                $ctx.WeakTypeTag(tp1.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L1, $U1],
                $ctx.WeakTypeTag(tp2.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L2, $U2],
                $ctx.WeakTypeTag(tp3.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L3, $U3],
                $ctx.WeakTypeTag(tp4.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L4, $U4],
                $ctx.WeakTypeTag(tp5.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L5, $U5],
                $ctx.WeakTypeTag(tp6.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L6, $U6],
                $ctx.WeakTypeTag(tp7.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L7, $U7],
                $ctx.WeakTypeTag(tp8.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L8, $U8],
                $ctx.WeakTypeTag(tp9.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L9, $U9],
                $ctx.WeakTypeTag(tp10.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L10, $U10],
                $ctx.WeakTypeTag(tp11.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L11, $U11],
                $ctx.WeakTypeTag(tp12.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L12, $U12]
              )
            )
          case _ =>
            if (A0.typeConstructor == HKT && A0.typeArgs.size == 12) {
              val Seq(tp1, tp2, tp3, tp4, tp5, tp6, tp7, tp8, tp9, tp10, tp11, tp12) = A0.typeArgs
              _root_.scala.Some(
                (
                  $ctx.WeakTypeTag(tp1.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L1, $U1],
                  $ctx.WeakTypeTag(tp2.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L2, $U2],
                  $ctx.WeakTypeTag(tp3.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L3, $U3],
                  $ctx.WeakTypeTag(tp4.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L4, $U4],
                  $ctx.WeakTypeTag(tp5.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L5, $U5],
                  $ctx.WeakTypeTag(tp6.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L6, $U6],
                  $ctx.WeakTypeTag(tp7.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L7, $U7],
                  $ctx.WeakTypeTag(tp8.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L8, $U8],
                  $ctx.WeakTypeTag(tp9.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L9, $U9],
                  $ctx.WeakTypeTag(tp10.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L10, $U10],
                  $ctx.WeakTypeTag(tp11.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L11, $U11],
                  $ctx.WeakTypeTag(tp12.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L12, $U12]
                )
              )
            }
            else None
        }
      }
    }
    """

    val result = suppressWarnings(c.typecheck(unchecked))

    log(
      s"""Cross-quotes ${paintExclDot(Console.BLUE)("Type.Ctor12.of")} expansion:
         |From: ${paintExclDot(Console.BLUE)(
          "Type.Ctor12.of"
        )}[${L1.tpe}, ${U1.tpe}, ${L2.tpe}, ${U2.tpe}, ${L3.tpe}, ${U3.tpe}, ${L4.tpe}, ${U4.tpe}, ${L5.tpe}, ${U5.tpe}, ${L6.tpe}, ${U6.tpe}, ${L7.tpe}, ${U7.tpe}, ${L8.tpe}, ${U8.tpe}, ${L9.tpe}, ${U9.tpe}, ${L10.tpe}, ${U10.tpe}, ${L11.tpe}, ${U11.tpe}, ${L12.tpe}, ${U12.tpe}, $HKT]
         |To: ${indent(showCodePretty(result, SyntaxHighlight.ANSI))}""".stripMargin
    )

    result
  } catch { case e: Throwable => reportIssue(e) }

  /* Replaces:
   *   Type.Ctor13.of[HKT]
   *   Type.Ctor13.UpperBounded.of[U1, ... U13, HKT]
   *   Type.Ctor13.Bounded.of[L1, U1, ... L13, U13, HKT]
   * with:
   *   what we see in Quasiquote
   */
  // format: off
  def typeCtor13Impl[L1, U1 >: L1, L2, U2 >: L2, L3, U3 >: L3, L4, U4 >: L4, L5, U5 >: L5, L6, U6 >: L6, L7, U7 >: L7, L8, U8 >: L8, L9, U9 >: L9, L10, U10 >: L10, L11, U11 >: L11, L12, U12 >: L12, L13, U13 >: L13, HKT[_ >: L1 <: U1, _ >: L2 <: U2, _ >: L3 <: U3, _ >: L4 <: U4, _ >: L5 <: U5, _ >: L6 <: U6, _ >: L7 <: U7, _ >: L8 <: U8, _ >: L9 <: U9, _ >: L10 <: U10, _ >: L11 <: U11, _ >: L12 <: U12, _ >: L13 <: U13]](implicit
  // format: on
      L1: c.WeakTypeTag[L1],
      U1: c.WeakTypeTag[U1],
      L2: c.WeakTypeTag[L2],
      U2: c.WeakTypeTag[U2],
      L3: c.WeakTypeTag[L3],
      U3: c.WeakTypeTag[U3],
      L4: c.WeakTypeTag[L4],
      U4: c.WeakTypeTag[U4],
      L5: c.WeakTypeTag[L5],
      U5: c.WeakTypeTag[U5],
      L6: c.WeakTypeTag[L6],
      U6: c.WeakTypeTag[U6],
      L7: c.WeakTypeTag[L7],
      U7: c.WeakTypeTag[U7],
      L8: c.WeakTypeTag[L8],
      U8: c.WeakTypeTag[U8],
      L9: c.WeakTypeTag[L9],
      U9: c.WeakTypeTag[U9],
      L10: c.WeakTypeTag[L10],
      U10: c.WeakTypeTag[U10],
      L11: c.WeakTypeTag[L11],
      U11: c.WeakTypeTag[U11],
      L12: c.WeakTypeTag[L12],
      U12: c.WeakTypeTag[U12],
      L13: c.WeakTypeTag[L13],
      U13: c.WeakTypeTag[U13],
      HKTE: c.WeakTypeTag[HKT[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]
  ): c.Tree = try {
    assert(HKTE.tpe.typeParams.size == 13, "HKT must have exactly thirteen type parameters")

    val HKT = HKTE.tpe.typeConstructor
    val A = freshTypeName("A") // 1
    val B = freshTypeName("B") // 2
    val C = freshTypeName("C") // 3
    val D = freshTypeName("D") // 4
    val E = freshTypeName("E") // 5
    val F = freshTypeName("F") // 6
    val G = freshTypeName("G") // 7
    val H = freshTypeName("H") // 8
    val I = freshTypeName("I") // 9
    val J = freshTypeName("J") // 10
    val K = freshTypeName("K") // 11
    val L = freshTypeName("L") // 12
    val M = freshTypeName("M") // 13
    val appliedHKT =
      tq"Type.Ctor13.Apply[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $L7, $U7, $L8, $U8, $L9, $U9, $L10, $U10, $L11, $U11, $L12, $U12, $L13, $U13, $HKT, $A, $B, $C, $D, $E, $F, $G, $H, $I, $J, $K, $L, $M]"

    val ctx = freshName("ctx")
    val typeValue = TypeWithUnderlyingInjected(ctx, appliedHKT, A, B, C, D, E, F, G, H, I, J, K, L, M)
    val convertProvidedTypesForCrossQuotes = freshName("convertProvidedTypesForCrossQuotes")
    val termInner = freshName("Inner")
    val typeInner = TypeName(freshName("Inner").toString)

    val unchecked = q"""
    new Type.Ctor13.Bounded[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $L7, $U7, $L8, $U8, $L9, $U9, $L10, $U10, $L11, $U11, $L12, $U12, $L13, $U13, $HKT] {
      private val $ctx = CrossQuotes.ctx[_root_.scala.reflect.macros.blackbox.Context]
      private implicit def $convertProvidedTypesForCrossQuotes[$typeInner](implicit $termInner: Type[$typeInner]): $ctx.WeakTypeTag[$typeInner] =
        $termInner.asInstanceOf[$ctx.WeakTypeTag[$typeInner]]
      import $ctx.universe.{Type => _, internal => _, _}

      private val HKT = $ctx.weakTypeTag[Type.Ctor13.Stub[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $L7, $U7, $L8, $U8, $L9, $U9, $L10, $U10, $L11, $U11, $L12, $U12, $L13, $U13, $HKT]].tpe.typeArgs.last

      def apply[$A >: $L1 <: $U1: Type, $B >: $L2 <: $U2: Type, $C >: $L3 <: $U3: Type, $D >: $L4 <: $U4: Type, $E >: $L5 <: $U5: Type, $F >: $L6 <: $U6: Type, $G >: $L7 <: $U7: Type, $H >: $L8 <: $U8: Type, $I >: $L9 <: $U9: Type, $J >: $L10 <: $U10: Type, $K >: $L11 <: $U11: Type, $L >: $L12 <: $U12: Type, $M >: $L13 <: $U13: Type]: Type[$appliedHKT] = 
        $typeValue

      def unapply[$A](A: Type[$A]): Option[($L1 <:??<: $U1, $L2 <:??<: $U2, $L3 <:??<: $U3, $L4 <:??<: $U4, $L5 <:??<: $U5, $L6 <:??<: $U6, $L7 <:??<: $U7, $L8 <:??<: $U8, $L9 <:??<: $U9, $L10 <:??<: $U10, $L11 <:??<: $U11, $L12 <:??<: $U12, $L13 <:??<: $U13)] = {
        val A0 = A.asInstanceOf[$ctx.WeakTypeTag[$A]].tpe
        A0.dealias.widen.baseType(HKT.typeSymbol) match {
          case TypeRef(_, _, List(tp1, tp2, tp3, tp4, tp5, tp6, tp7, tp8, tp9, tp10, tp11, tp12, tp13)) =>
            _root_.scala.Some(
              (
                $ctx.WeakTypeTag(tp1.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L1, $U1],
                $ctx.WeakTypeTag(tp2.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L2, $U2],
                $ctx.WeakTypeTag(tp3.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L3, $U3],
                $ctx.WeakTypeTag(tp4.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L4, $U4],
                $ctx.WeakTypeTag(tp5.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L5, $U5],
                $ctx.WeakTypeTag(tp6.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L6, $U6],
                $ctx.WeakTypeTag(tp7.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L7, $U7],
                $ctx.WeakTypeTag(tp8.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L8, $U8],
                $ctx.WeakTypeTag(tp9.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L9, $U9],
                $ctx.WeakTypeTag(tp10.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L10, $U10],
                $ctx.WeakTypeTag(tp11.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L11, $U11],
                $ctx.WeakTypeTag(tp12.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L12, $U12],
                $ctx.WeakTypeTag(tp13.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L13, $U13]
              )
            )
          case _ =>
            if (A0.typeConstructor == HKT && A0.typeArgs.size == 13) {
              val Seq(tp1, tp2, tp3, tp4, tp5, tp6, tp7, tp8, tp9, tp10, tp11, tp12, tp13) = A0.typeArgs
              _root_.scala.Some(
                (
                  $ctx.WeakTypeTag(tp1.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L1, $U1],
                  $ctx.WeakTypeTag(tp2.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L2, $U2],
                  $ctx.WeakTypeTag(tp3.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L3, $U3],
                  $ctx.WeakTypeTag(tp4.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L4, $U4],
                  $ctx.WeakTypeTag(tp5.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L5, $U5],
                  $ctx.WeakTypeTag(tp6.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L6, $U6],
                  $ctx.WeakTypeTag(tp7.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L7, $U7],
                  $ctx.WeakTypeTag(tp8.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L8, $U8],
                  $ctx.WeakTypeTag(tp9.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L9, $U9],
                  $ctx.WeakTypeTag(tp10.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L10, $U10],
                  $ctx.WeakTypeTag(tp11.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L11, $U11],
                  $ctx.WeakTypeTag(tp12.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L12, $U12],
                  $ctx.WeakTypeTag(tp13.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L13, $U13]
                )
              )
            }
            else None
        }
      }
    }
    """

    val result = suppressWarnings(c.typecheck(unchecked))

    log(
      s"""Cross-quotes ${paintExclDot(Console.BLUE)("Type.Ctor13.of")} expansion:
         |From: ${paintExclDot(Console.BLUE)(
          "Type.Ctor13.of"
        )}[${L1.tpe}, ${U1.tpe}, ${L2.tpe}, ${U2.tpe}, ${L3.tpe}, ${U3.tpe}, ${L4.tpe}, ${U4.tpe}, ${L5.tpe}, ${U5.tpe}, ${L6.tpe}, ${U6.tpe}, ${L7.tpe}, ${U7.tpe}, ${L8.tpe}, ${U8.tpe}, ${L9.tpe}, ${U9.tpe}, ${L10.tpe}, ${U10.tpe}, ${L11.tpe}, ${U11.tpe}, ${L12.tpe}, ${U12.tpe}, ${L13.tpe}, ${U13.tpe}, $HKT]
         |To: ${indent(showCodePretty(result, SyntaxHighlight.ANSI))}""".stripMargin
    )

    result
  } catch { case e: Throwable => reportIssue(e) }

  /* Replaces:
   *   Type.Ctor14.of[HKT]
   *   ...
   */
  // format: off
  def typeCtor14Impl[L1, U1 >: L1, L2, U2 >: L2, L3, U3 >: L3, L4, U4 >: L4, L5, U5 >: L5, L6, U6 >: L6, L7, U7 >: L7, L8, U8 >: L8, L9, U9 >: L9, L10, U10 >: L10, L11, U11 >: L11, L12, U12 >: L12, L13, U13 >: L13, L14, U14 >: L14, HKT[_ >: L1 <: U1, _ >: L2 <: U2, _ >: L3 <: U3, _ >: L4 <: U4, _ >: L5 <: U5, _ >: L6 <: U6, _ >: L7 <: U7, _ >: L8 <: U8, _ >: L9 <: U9, _ >: L10 <: U10, _ >: L11 <: U11, _ >: L12 <: U12, _ >: L13 <: U13, _ >: L14 <: U14]](implicit
  // format: on
      L1: c.WeakTypeTag[L1],
      U1: c.WeakTypeTag[U1],
      L2: c.WeakTypeTag[L2],
      U2: c.WeakTypeTag[U2],
      L3: c.WeakTypeTag[L3],
      U3: c.WeakTypeTag[U3],
      L4: c.WeakTypeTag[L4],
      U4: c.WeakTypeTag[U4],
      L5: c.WeakTypeTag[L5],
      U5: c.WeakTypeTag[U5],
      L6: c.WeakTypeTag[L6],
      U6: c.WeakTypeTag[U6],
      L7: c.WeakTypeTag[L7],
      U7: c.WeakTypeTag[U7],
      L8: c.WeakTypeTag[L8],
      U8: c.WeakTypeTag[U8],
      L9: c.WeakTypeTag[L9],
      U9: c.WeakTypeTag[U9],
      L10: c.WeakTypeTag[L10],
      U10: c.WeakTypeTag[U10],
      L11: c.WeakTypeTag[L11],
      U11: c.WeakTypeTag[U11],
      L12: c.WeakTypeTag[L12],
      U12: c.WeakTypeTag[U12],
      L13: c.WeakTypeTag[L13],
      U13: c.WeakTypeTag[U13],
      L14: c.WeakTypeTag[L14],
      U14: c.WeakTypeTag[U14],
      HKTE: c.WeakTypeTag[HKT[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]
  ): c.Tree = try {
    assert(HKTE.tpe.typeParams.size == 14, "HKT must have exactly fourteen type parameters")

    val HKT = HKTE.tpe.typeConstructor
    val A = freshTypeName("A") // 1
    val B = freshTypeName("B") // 2
    val C = freshTypeName("C") // 3
    val D = freshTypeName("D") // 4
    val E = freshTypeName("E") // 5
    val F = freshTypeName("F") // 6
    val G = freshTypeName("G") // 7
    val H = freshTypeName("H") // 8
    val I = freshTypeName("I") // 9
    val J = freshTypeName("J") // 10
    val K = freshTypeName("K") // 11
    val L = freshTypeName("L") // 12
    val M = freshTypeName("M") // 13
    val N = freshTypeName("N") // 14
    val appliedHKT =
      tq"Type.Ctor14.Apply[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $L7, $U7, $L8, $U8, $L9, $U9, $L10, $U10, $L11, $U11, $L12, $U12, $L13, $U13, $L14, $U14, $HKT, $A, $B, $C, $D, $E, $F, $G, $H, $I, $J, $K, $L, $M, $N]"

    val ctx = freshName("ctx")
    val typeValue = TypeWithUnderlyingInjected(ctx, appliedHKT, A, B, C, D, E, F, G, H, I, J, K, L, M, N)
    val convertProvidedTypesForCrossQuotes = freshName("convertProvidedTypesForCrossQuotes")
    val termInner = freshName("Inner")
    val typeInner = TypeName(freshName("Inner").toString)

    val unchecked = q"""
    new Type.Ctor14.Bounded[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $L7, $U7, $L8, $U8, $L9, $U9, $L10, $U10, $L11, $U11, $L12, $U12, $L13, $U13, $L14, $U14, $HKT] {
      private val $ctx = CrossQuotes.ctx[_root_.scala.reflect.macros.blackbox.Context]
      private implicit def $convertProvidedTypesForCrossQuotes[$typeInner](implicit $termInner: Type[$typeInner]): $ctx.WeakTypeTag[$typeInner] =
        $termInner.asInstanceOf[$ctx.WeakTypeTag[$typeInner]]
      import $ctx.universe.{Type => _, internal => _, _}

      private val HKT = $ctx.weakTypeTag[Type.Ctor14.Stub[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $L7, $U7, $L8, $U8, $L9, $U9, $L10, $U10, $L11, $U11, $L12, $U12, $L13, $U13, $L14, $U14, $HKT]].tpe.typeArgs.last

      def apply[$A >: $L1 <: $U1: Type, $B >: $L2 <: $U2: Type, $C >: $L3 <: $U3: Type, $D >: $L4 <: $U4: Type, $E >: $L5 <: $U5: Type, $F >: $L6 <: $U6: Type, $G >: $L7 <: $U7: Type, $H >: $L8 <: $U8: Type, $I >: $L9 <: $U9: Type, $J >: $L10 <: $U10: Type, $K >: $L11 <: $U11: Type, $L >: $L12 <: $U12: Type, $M >: $L13 <: $U13: Type, $N >: $L14 <: $U14: Type]: Type[$appliedHKT] = 
        $typeValue

      def unapply[$A](A: Type[$A]): Option[($L1 <:??<: $U1, $L2 <:??<: $U2, $L3 <:??<: $U3, $L4 <:??<: $U4, $L5 <:??<: $U5, $L6 <:??<: $U6, $L7 <:??<: $U7, $L8 <:??<: $U8, $L9 <:??<: $U9, $L10 <:??<: $U10, $L11 <:??<: $U11, $L12 <:??<: $U12, $L13 <:??<: $U13, $L14 <:??<: $U14)] = {
        val A0 = A.asInstanceOf[$ctx.WeakTypeTag[$A]].tpe
        A0.dealias.widen.baseType(HKT.typeSymbol) match {
          case TypeRef(_, _, List(tp1, tp2, tp3, tp4, tp5, tp6, tp7, tp8, tp9, tp10, tp11, tp12, tp13, tp14)) =>
            _root_.scala.Some(
              (
                $ctx.WeakTypeTag(tp1.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L1, $U1],
                $ctx.WeakTypeTag(tp2.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L2, $U2],
                $ctx.WeakTypeTag(tp3.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L3, $U3],
                $ctx.WeakTypeTag(tp4.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L4, $U4],
                $ctx.WeakTypeTag(tp5.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L5, $U5],
                $ctx.WeakTypeTag(tp6.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L6, $U6],
                $ctx.WeakTypeTag(tp7.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L7, $U7],
                $ctx.WeakTypeTag(tp8.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L8, $U8],
                $ctx.WeakTypeTag(tp9.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L9, $U9],
                $ctx.WeakTypeTag(tp10.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L10, $U10],
                $ctx.WeakTypeTag(tp11.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L11, $U11],
                $ctx.WeakTypeTag(tp12.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L12, $U12],
                $ctx.WeakTypeTag(tp13.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L13, $U13],
                $ctx.WeakTypeTag(tp14.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L14, $U14]
              )
            )
          case _ =>
            if (A0.typeConstructor == HKT && A0.typeArgs.size == 14) {
              val Seq(tp1, tp2, tp3, tp4, tp5, tp6, tp7, tp8, tp9, tp10, tp11, tp12, tp13, tp14) = A0.typeArgs
              _root_.scala.Some(
                (
                  $ctx.WeakTypeTag(tp1.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L1, $U1],
                  $ctx.WeakTypeTag(tp2.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L2, $U2],
                  $ctx.WeakTypeTag(tp3.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L3, $U3],
                  $ctx.WeakTypeTag(tp4.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L4, $U4],
                  $ctx.WeakTypeTag(tp5.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L5, $U5],
                  $ctx.WeakTypeTag(tp6.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L6, $U6],
                  $ctx.WeakTypeTag(tp7.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L7, $U7],
                  $ctx.WeakTypeTag(tp8.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L8, $U8],
                  $ctx.WeakTypeTag(tp9.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L9, $U9],
                  $ctx.WeakTypeTag(tp10.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L10, $U10],
                  $ctx.WeakTypeTag(tp11.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L11, $U11],
                  $ctx.WeakTypeTag(tp12.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L12, $U12],
                  $ctx.WeakTypeTag(tp13.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L13, $U13],
                  $ctx.WeakTypeTag(tp14.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L14, $U14]
                )
              )
            }
            else None
        }
      }
    }
    """

    val result = suppressWarnings(c.typecheck(unchecked))

    log(
      s"""Cross-quotes ${paintExclDot(Console.BLUE)("Type.Ctor14.of")} expansion:
         |From: ${paintExclDot(Console.BLUE)(
          "Type.Ctor14.of"
        )}[${L1.tpe}, ${U1.tpe}, ${L2.tpe}, ${U2.tpe}, ${L3.tpe}, ${U3.tpe}, ${L4.tpe}, ${U4.tpe}, ${L5.tpe}, ${U5.tpe}, ${L6.tpe}, ${U6.tpe}, ${L7.tpe}, ${U7.tpe}, ${L8.tpe}, ${U8.tpe}, ${L9.tpe}, ${U9.tpe}, ${L10.tpe}, ${U10.tpe}, ${L11.tpe}, ${U11.tpe}, ${L12.tpe}, ${U12.tpe}, ${L13.tpe}, ${U13.tpe}, ${L14.tpe}, ${U14.tpe}, $HKT]
         |To: ${indent(showCodePretty(result, SyntaxHighlight.ANSI))}""".stripMargin
    )

    result
  } catch { case e: Throwable => reportIssue(e) }

  /* Replaces:
   *   Type.Ctor15.of[HKT]
   */
  // format: off
  def typeCtor15Impl[L1, U1 >: L1, L2, U2 >: L2, L3, U3 >: L3, L4, U4 >: L4, L5, U5 >: L5, L6, U6 >: L6, L7, U7 >: L7, L8, U8 >: L8, L9, U9 >: L9, L10, U10 >: L10, L11, U11 >: L11, L12, U12 >: L12, L13, U13 >: L13, L14, U14 >: L14, L15, U15 >: L15, HKT[_ >: L1 <: U1, _ >: L2 <: U2, _ >: L3 <: U3, _ >: L4 <: U4, _ >: L5 <: U5, _ >: L6 <: U6, _ >: L7 <: U7, _ >: L8 <: U8, _ >: L9 <: U9, _ >: L10 <: U10, _ >: L11 <: U11, _ >: L12 <: U12, _ >: L13 <: U13, _ >: L14 <: U14, _ >: L15 <: U15]](implicit
  // format: on
      L1: c.WeakTypeTag[L1],
      U1: c.WeakTypeTag[U1],
      L2: c.WeakTypeTag[L2],
      U2: c.WeakTypeTag[U2],
      L3: c.WeakTypeTag[L3],
      U3: c.WeakTypeTag[U3],
      L4: c.WeakTypeTag[L4],
      U4: c.WeakTypeTag[U4],
      L5: c.WeakTypeTag[L5],
      U5: c.WeakTypeTag[U5],
      L6: c.WeakTypeTag[L6],
      U6: c.WeakTypeTag[U6],
      L7: c.WeakTypeTag[L7],
      U7: c.WeakTypeTag[U7],
      L8: c.WeakTypeTag[L8],
      U8: c.WeakTypeTag[U8],
      L9: c.WeakTypeTag[L9],
      U9: c.WeakTypeTag[U9],
      L10: c.WeakTypeTag[L10],
      U10: c.WeakTypeTag[U10],
      L11: c.WeakTypeTag[L11],
      U11: c.WeakTypeTag[U11],
      L12: c.WeakTypeTag[L12],
      U12: c.WeakTypeTag[U12],
      L13: c.WeakTypeTag[L13],
      U13: c.WeakTypeTag[U13],
      L14: c.WeakTypeTag[L14],
      U14: c.WeakTypeTag[U14],
      L15: c.WeakTypeTag[L15],
      U15: c.WeakTypeTag[U15],
      HKTE: c.WeakTypeTag[HKT[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]
  ): c.Tree = try {
    assert(HKTE.tpe.typeParams.size == 15, "HKT must have exactly fifteen type parameters")

    val HKT = HKTE.tpe.typeConstructor
    val A = freshTypeName("A") // 1
    val B = freshTypeName("B") // 2
    val C = freshTypeName("C") // 3
    val D = freshTypeName("D") // 4
    val E = freshTypeName("E") // 5
    val F = freshTypeName("F") // 6
    val G = freshTypeName("G") // 7
    val H = freshTypeName("H") // 8
    val I = freshTypeName("I") // 9
    val J = freshTypeName("J") // 10
    val K = freshTypeName("K") // 11
    val L = freshTypeName("L") // 12
    val M = freshTypeName("M") // 13
    val N = freshTypeName("N") // 14
    val O = freshTypeName("O") // 15
    val appliedHKT =
      tq"Type.Ctor15.Apply[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $L7, $U7, $L8, $U8, $L9, $U9, $L10, $U10, $L11, $U11, $L12, $U12, $L13, $U13, $L14, $U14, $L15, $U15, $HKT, $A, $B, $C, $D, $E, $F, $G, $H, $I, $J, $K, $L, $M, $N, $O]"

    val ctx = freshName("ctx")
    val typeValue = TypeWithUnderlyingInjected(ctx, appliedHKT, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)
    val convertProvidedTypesForCrossQuotes = freshName("convertProvidedTypesForCrossQuotes")
    val termInner = freshName("Inner")
    val typeInner = TypeName(freshName("Inner").toString)

    val unchecked = q"""
    new Type.Ctor15.Bounded[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $L7, $U7, $L8, $U8, $L9, $U9, $L10, $U10, $L11, $U11, $L12, $U12, $L13, $U13, $L14, $U14, $L15, $U15, $HKT] {
      private val $ctx = CrossQuotes.ctx[_root_.scala.reflect.macros.blackbox.Context]
      private implicit def $convertProvidedTypesForCrossQuotes[$typeInner](implicit $termInner: Type[$typeInner]): $ctx.WeakTypeTag[$typeInner] =
        $termInner.asInstanceOf[$ctx.WeakTypeTag[$typeInner]]
      import $ctx.universe.{Type => _, internal => _, _}

      private val HKT = $ctx.weakTypeTag[Type.Ctor15.Stub[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $L7, $U7, $L8, $U8, $L9, $U9, $L10, $U10, $L11, $U11, $L12, $U12, $L13, $U13, $L14, $U14, $L15, $U15, $HKT]].tpe.typeArgs.last

      def apply[$A >: $L1 <: $U1: Type, $B >: $L2 <: $U2: Type, $C >: $L3 <: $U3: Type, $D >: $L4 <: $U4: Type, $E >: $L5 <: $U5: Type, $F >: $L6 <: $U6: Type, $G >: $L7 <: $U7: Type, $H >: $L8 <: $U8: Type, $I >: $L9 <: $U9: Type, $J >: $L10 <: $U10: Type, $K >: $L11 <: $U11: Type, $L >: $L12 <: $U12: Type, $M >: $L13 <: $U13: Type, $N >: $L14 <: $U14: Type, $O >: $L15 <: $U15: Type]: Type[$appliedHKT] = 
        $typeValue

      def unapply[$A](A: Type[$A]): Option[($L1 <:??<: $U1, $L2 <:??<: $U2, $L3 <:??<: $U3, $L4 <:??<: $U4, $L5 <:??<: $U5, $L6 <:??<: $U6, $L7 <:??<: $U7, $L8 <:??<: $U8, $L9 <:??<: $U9, $L10 <:??<: $U10, $L11 <:??<: $U11, $L12 <:??<: $U12, $L13 <:??<: $U13, $L14 <:??<: $U14, $L15 <:??<: $U15)] = {
        val A0 = A.asInstanceOf[$ctx.WeakTypeTag[$A]].tpe
        A0.dealias.widen.baseType(HKT.typeSymbol) match {
          case TypeRef(_, _, List(tp1, tp2, tp3, tp4, tp5, tp6, tp7, tp8, tp9, tp10, tp11, tp12, tp13, tp14, tp15)) =>
            _root_.scala.Some(
              (
                $ctx.WeakTypeTag(tp1.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L1, $U1],
                $ctx.WeakTypeTag(tp2.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L2, $U2],
                $ctx.WeakTypeTag(tp3.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L3, $U3],
                $ctx.WeakTypeTag(tp4.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L4, $U4],
                $ctx.WeakTypeTag(tp5.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L5, $U5],
                $ctx.WeakTypeTag(tp6.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L6, $U6],
                $ctx.WeakTypeTag(tp7.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L7, $U7],
                $ctx.WeakTypeTag(tp8.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L8, $U8],
                $ctx.WeakTypeTag(tp9.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L9, $U9],
                $ctx.WeakTypeTag(tp10.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L10, $U10],
                $ctx.WeakTypeTag(tp11.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L11, $U11],
                $ctx.WeakTypeTag(tp12.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L12, $U12],
                $ctx.WeakTypeTag(tp13.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L13, $U13],
                $ctx.WeakTypeTag(tp14.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L14, $U14],
                $ctx.WeakTypeTag(tp15.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L15, $U15]
              )
            )
          case _ =>
            if (A0.typeConstructor == HKT && A0.typeArgs.size == 15) {
              val Seq(tp1, tp2, tp3, tp4, tp5, tp6, tp7, tp8, tp9, tp10, tp11, tp12, tp13, tp14, tp15) = A0.typeArgs
              _root_.scala.Some(
                (
                  $ctx.WeakTypeTag(tp1.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L1, $U1],
                  $ctx.WeakTypeTag(tp2.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L2, $U2],
                  $ctx.WeakTypeTag(tp3.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L3, $U3],
                  $ctx.WeakTypeTag(tp4.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L4, $U4],
                  $ctx.WeakTypeTag(tp5.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L5, $U5],
                  $ctx.WeakTypeTag(tp6.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L6, $U6],
                  $ctx.WeakTypeTag(tp7.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L7, $U7],
                  $ctx.WeakTypeTag(tp8.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L8, $U8],
                  $ctx.WeakTypeTag(tp9.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L9, $U9],
                  $ctx.WeakTypeTag(tp10.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L10, $U10],
                  $ctx.WeakTypeTag(tp11.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L11, $U11],
                  $ctx.WeakTypeTag(tp12.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L12, $U12],
                  $ctx.WeakTypeTag(tp13.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L13, $U13],
                  $ctx.WeakTypeTag(tp14.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L14, $U14],
                  $ctx.WeakTypeTag(tp15.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L15, $U15]
                )
              )
            }
            else None
        }
      }
    }
    """

    val result = suppressWarnings(c.typecheck(unchecked))

    log(
      s"""Cross-quotes ${paintExclDot(Console.BLUE)("Type.Ctor15.of")} expansion:
         |From: ${paintExclDot(Console.BLUE)(
          "Type.Ctor15.of"
        )}[${L1.tpe}, ${U1.tpe}, ${L2.tpe}, ${U2.tpe}, ${L3.tpe}, ${U3.tpe}, ${L4.tpe}, ${U4.tpe}, ${L5.tpe}, ${U5.tpe}, ${L6.tpe}, ${U6.tpe}, ${L7.tpe}, ${U7.tpe}, ${L8.tpe}, ${U8.tpe}, ${L9.tpe}, ${U9.tpe}, ${L10.tpe}, ${U10.tpe}, ${L11.tpe}, ${U11.tpe}, ${L12.tpe}, ${U12.tpe}, ${L13.tpe}, ${U13.tpe}, ${L14.tpe}, ${U14.tpe}, ${L15.tpe}, ${U15.tpe}, $HKT]
         |To: ${indent(showCodePretty(result, SyntaxHighlight.ANSI))}""".stripMargin
    )

    result
  } catch { case e: Throwable => reportIssue(e) }

  /* Replaces:
   *   Type.Ctor16.of[HKT]
   */
  // format: off
  def typeCtor16Impl[L1, U1 >: L1, L2, U2 >: L2, L3, U3 >: L3, L4, U4 >: L4, L5, U5 >: L5, L6, U6 >: L6, L7, U7 >: L7, L8, U8 >: L8, L9, U9 >: L9, L10, U10 >: L10, L11, U11 >: L11, L12, U12 >: L12, L13, U13 >: L13, L14, U14 >: L14, L15, U15 >: L15, L16, U16 >: L16, HKT[_ >: L1 <: U1, _ >: L2 <: U2, _ >: L3 <: U3, _ >: L4 <: U4, _ >: L5 <: U5, _ >: L6 <: U6, _ >: L7 <: U7, _ >: L8 <: U8, _ >: L9 <: U9, _ >: L10 <: U10, _ >: L11 <: U11, _ >: L12 <: U12, _ >: L13 <: U13, _ >: L14 <: U14, _ >: L15 <: U15, _ >: L16 <: U16]](implicit
  // format: on
      L1: c.WeakTypeTag[L1],
      U1: c.WeakTypeTag[U1],
      L2: c.WeakTypeTag[L2],
      U2: c.WeakTypeTag[U2],
      L3: c.WeakTypeTag[L3],
      U3: c.WeakTypeTag[U3],
      L4: c.WeakTypeTag[L4],
      U4: c.WeakTypeTag[U4],
      L5: c.WeakTypeTag[L5],
      U5: c.WeakTypeTag[U5],
      L6: c.WeakTypeTag[L6],
      U6: c.WeakTypeTag[U6],
      L7: c.WeakTypeTag[L7],
      U7: c.WeakTypeTag[U7],
      L8: c.WeakTypeTag[L8],
      U8: c.WeakTypeTag[U8],
      L9: c.WeakTypeTag[L9],
      U9: c.WeakTypeTag[U9],
      L10: c.WeakTypeTag[L10],
      U10: c.WeakTypeTag[U10],
      L11: c.WeakTypeTag[L11],
      U11: c.WeakTypeTag[U11],
      L12: c.WeakTypeTag[L12],
      U12: c.WeakTypeTag[U12],
      L13: c.WeakTypeTag[L13],
      U13: c.WeakTypeTag[U13],
      L14: c.WeakTypeTag[L14],
      U14: c.WeakTypeTag[U14],
      L15: c.WeakTypeTag[L15],
      U15: c.WeakTypeTag[U15],
      L16: c.WeakTypeTag[L16],
      U16: c.WeakTypeTag[U16],
      HKTE: c.WeakTypeTag[HKT[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]
  ): c.Tree = try {
    assert(HKTE.tpe.typeParams.size == 16, "HKT must have exactly sixteen type parameters")

    val HKT = HKTE.tpe.typeConstructor
    val A = freshTypeName("A") // 1
    val B = freshTypeName("B") // 2
    val C = freshTypeName("C") // 3
    val D = freshTypeName("D") // 4
    val E = freshTypeName("E") // 5
    val F = freshTypeName("F") // 6
    val G = freshTypeName("G") // 7
    val H = freshTypeName("H") // 8
    val I = freshTypeName("I") // 9
    val J = freshTypeName("J") // 10
    val K = freshTypeName("K") // 11
    val L = freshTypeName("L") // 12
    val M = freshTypeName("M") // 13
    val N = freshTypeName("N") // 14
    val O = freshTypeName("O") // 15
    val P = freshTypeName("P") // 16
    val appliedHKT =
      tq"Type.Ctor16.Apply[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $L7, $U7, $L8, $U8, $L9, $U9, $L10, $U10, $L11, $U11, $L12, $U12, $L13, $U13, $L14, $U14, $L15, $U15, $L16, $U16, $HKT, $A, $B, $C, $D, $E, $F, $G, $H, $I, $J, $K, $L, $M, $N, $O, $P]"

    val ctx = freshName("ctx")
    val typeValue = TypeWithUnderlyingInjected(ctx, appliedHKT, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)
    val convertProvidedTypesForCrossQuotes = freshName("convertProvidedTypesForCrossQuotes")
    val termInner = freshName("Inner")
    val typeInner = TypeName(freshName("Inner").toString)

    val unchecked = q"""
    new Type.Ctor16.Bounded[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $L7, $U7, $L8, $U8, $L9, $U9, $L10, $U10, $L11, $U11, $L12, $U12, $L13, $U13, $L14, $U14, $L15, $U15, $L16, $U16, $HKT] {
      private val $ctx = CrossQuotes.ctx[_root_.scala.reflect.macros.blackbox.Context]
      private implicit def $convertProvidedTypesForCrossQuotes[$typeInner](implicit $termInner: Type[$typeInner]): $ctx.WeakTypeTag[$typeInner] =
        $termInner.asInstanceOf[$ctx.WeakTypeTag[$typeInner]]
      import $ctx.universe.{Type => _, internal => _, _}

      private val HKT = $ctx.weakTypeTag[Type.Ctor16.Stub[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $L7, $U7, $L8, $U8, $L9, $U9, $L10, $U10, $L11, $U11, $L12, $U12, $L13, $U13, $L14, $U14, $L15, $U15, $L16, $U16, $HKT]].tpe.typeArgs.last

      def apply[$A >: $L1 <: $U1: Type, $B >: $L2 <: $U2: Type, $C >: $L3 <: $U3: Type, $D >: $L4 <: $U4: Type, $E >: $L5 <: $U5: Type, $F >: $L6 <: $U6: Type, $G >: $L7 <: $U7: Type, $H >: $L8 <: $U8: Type, $I >: $L9 <: $U9: Type, $J >: $L10 <: $U10: Type, $K >: $L11 <: $U11: Type, $L >: $L12 <: $U12: Type, $M >: $L13 <: $U13: Type, $N >: $L14 <: $U14: Type, $O >: $L15 <: $U15: Type, $P >: $L16 <: $U16: Type]: Type[$appliedHKT] = 
        $typeValue

      def unapply[$A](A: Type[$A]): Option[($L1 <:??<: $U1, $L2 <:??<: $U2, $L3 <:??<: $U3, $L4 <:??<: $U4, $L5 <:??<: $U5, $L6 <:??<: $U6, $L7 <:??<: $U7, $L8 <:??<: $U8, $L9 <:??<: $U9, $L10 <:??<: $U10, $L11 <:??<: $U11, $L12 <:??<: $U12, $L13 <:??<: $U13, $L14 <:??<: $U14, $L15 <:??<: $U15, $L16 <:??<: $U16)] = {
        val A0 = A.asInstanceOf[$ctx.WeakTypeTag[$A]].tpe
        A0.dealias.widen.baseType(HKT.typeSymbol) match {
          case TypeRef(_, _, List(tp1, tp2, tp3, tp4, tp5, tp6, tp7, tp8, tp9, tp10, tp11, tp12, tp13, tp14, tp15, tp16)) =>
            _root_.scala.Some(
              (
                $ctx.WeakTypeTag(tp1.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L1, $U1],
                $ctx.WeakTypeTag(tp2.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L2, $U2],
                $ctx.WeakTypeTag(tp3.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L3, $U3],
                $ctx.WeakTypeTag(tp4.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L4, $U4],
                $ctx.WeakTypeTag(tp5.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L5, $U5],
                $ctx.WeakTypeTag(tp6.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L6, $U6],
                $ctx.WeakTypeTag(tp7.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L7, $U7],
                $ctx.WeakTypeTag(tp8.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L8, $U8],
                $ctx.WeakTypeTag(tp9.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L9, $U9],
                $ctx.WeakTypeTag(tp10.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L10, $U10],
                $ctx.WeakTypeTag(tp11.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L11, $U11],
                $ctx.WeakTypeTag(tp12.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L12, $U12],
                $ctx.WeakTypeTag(tp13.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L13, $U13],
                $ctx.WeakTypeTag(tp14.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L14, $U14],
                $ctx.WeakTypeTag(tp15.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L15, $U15],
                $ctx.WeakTypeTag(tp16.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L16, $U16]
              )
            )
          case _ =>
            if (A0.typeConstructor == HKT && A0.typeArgs.size == 16) {
              val Seq(tp1, tp2, tp3, tp4, tp5, tp6, tp7, tp8, tp9, tp10, tp11, tp12, tp13, tp14, tp15, tp16) = A0.typeArgs
              _root_.scala.Some(
                (
                  $ctx.WeakTypeTag(tp1.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L1, $U1],
                  $ctx.WeakTypeTag(tp2.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L2, $U2],
                  $ctx.WeakTypeTag(tp3.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L3, $U3],
                  $ctx.WeakTypeTag(tp4.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L4, $U4],
                  $ctx.WeakTypeTag(tp5.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L5, $U5],
                  $ctx.WeakTypeTag(tp6.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L6, $U6],
                  $ctx.WeakTypeTag(tp7.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L7, $U7],
                  $ctx.WeakTypeTag(tp8.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L8, $U8],
                  $ctx.WeakTypeTag(tp9.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L9, $U9],
                  $ctx.WeakTypeTag(tp10.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L10, $U10],
                  $ctx.WeakTypeTag(tp11.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L11, $U11],
                  $ctx.WeakTypeTag(tp12.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L12, $U12],
                  $ctx.WeakTypeTag(tp13.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L13, $U13],
                  $ctx.WeakTypeTag(tp14.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L14, $U14],
                  $ctx.WeakTypeTag(tp15.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L15, $U15],
                  $ctx.WeakTypeTag(tp16.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L16, $U16]
                )
              )
            }
            else None
        }
      }
    }
    """

    val result = suppressWarnings(c.typecheck(unchecked))

    log(
      s"""Cross-quotes ${paintExclDot(Console.BLUE)("Type.Ctor16.of")} expansion:
         |From: ${paintExclDot(Console.BLUE)(
          "Type.Ctor16.of"
        )}[${L1.tpe}, ${U1.tpe}, ${L2.tpe}, ${U2.tpe}, ${L3.tpe}, ${U3.tpe}, ${L4.tpe}, ${U4.tpe}, ${L5.tpe}, ${U5.tpe}, ${L6.tpe}, ${U6.tpe}, ${L7.tpe}, ${U7.tpe}, ${L8.tpe}, ${U8.tpe}, ${L9.tpe}, ${U9.tpe}, ${L10.tpe}, ${U10.tpe}, ${L11.tpe}, ${U11.tpe}, ${L12.tpe}, ${U12.tpe}, ${L13.tpe}, ${U13.tpe}, ${L14.tpe}, ${U14.tpe}, ${L15.tpe}, ${U15.tpe}, ${L16.tpe}, ${U16.tpe}, $HKT]
         |To: ${indent(showCodePretty(result, SyntaxHighlight.ANSI))}""".stripMargin
    )

    result
  } catch { case e: Throwable => reportIssue(e) }

  /* Replaces:
   *   Type.Ctor17.of[HKT]
   */
  // format: off
  def typeCtor17Impl[L1, U1 >: L1, L2, U2 >: L2, L3, U3 >: L3, L4, U4 >: L4, L5, U5 >: L5, L6, U6 >: L6, L7, U7 >: L7, L8, U8 >: L8, L9, U9 >: L9, L10, U10 >: L10, L11, U11 >: L11, L12, U12 >: L12, L13, U13 >: L13, L14, U14 >: L14, L15, U15 >: L15, L16, U16 >: L16, L17, U17 >: L17, HKT[_ >: L1 <: U1, _ >: L2 <: U2, _ >: L3 <: U3, _ >: L4 <: U4, _ >: L5 <: U5, _ >: L6 <: U6, _ >: L7 <: U7, _ >: L8 <: U8, _ >: L9 <: U9, _ >: L10 <: U10, _ >: L11 <: U11, _ >: L12 <: U12, _ >: L13 <: U13, _ >: L14 <: U14, _ >: L15 <: U15, _ >: L16 <: U16, _ >: L17 <: U17]](implicit
  // format: on
      L1: c.WeakTypeTag[L1],
      U1: c.WeakTypeTag[U1],
      L2: c.WeakTypeTag[L2],
      U2: c.WeakTypeTag[U2],
      L3: c.WeakTypeTag[L3],
      U3: c.WeakTypeTag[U3],
      L4: c.WeakTypeTag[L4],
      U4: c.WeakTypeTag[U4],
      L5: c.WeakTypeTag[L5],
      U5: c.WeakTypeTag[U5],
      L6: c.WeakTypeTag[L6],
      U6: c.WeakTypeTag[U6],
      L7: c.WeakTypeTag[L7],
      U7: c.WeakTypeTag[U7],
      L8: c.WeakTypeTag[L8],
      U8: c.WeakTypeTag[U8],
      L9: c.WeakTypeTag[L9],
      U9: c.WeakTypeTag[U9],
      L10: c.WeakTypeTag[L10],
      U10: c.WeakTypeTag[U10],
      L11: c.WeakTypeTag[L11],
      U11: c.WeakTypeTag[U11],
      L12: c.WeakTypeTag[L12],
      U12: c.WeakTypeTag[U12],
      L13: c.WeakTypeTag[L13],
      U13: c.WeakTypeTag[U13],
      L14: c.WeakTypeTag[L14],
      U14: c.WeakTypeTag[U14],
      L15: c.WeakTypeTag[L15],
      U15: c.WeakTypeTag[U15],
      L16: c.WeakTypeTag[L16],
      U16: c.WeakTypeTag[U16],
      L17: c.WeakTypeTag[L17],
      U17: c.WeakTypeTag[U17],
      HKTE: c.WeakTypeTag[HKT[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]
  ): c.Tree = try {
    assert(HKTE.tpe.typeParams.size == 17, "HKT must have exactly seventeen type parameters")

    val HKT = HKTE.tpe.typeConstructor
    val A = freshTypeName("A") // 1
    val B = freshTypeName("B") // 2
    val C = freshTypeName("C") // 3
    val D = freshTypeName("D") // 4
    val E = freshTypeName("E") // 5
    val F = freshTypeName("F") // 6
    val G = freshTypeName("G") // 7
    val H = freshTypeName("H") // 8
    val I = freshTypeName("I") // 9
    val J = freshTypeName("J") // 10
    val K = freshTypeName("K") // 11
    val L = freshTypeName("L") // 12
    val M = freshTypeName("M") // 13
    val N = freshTypeName("N") // 14
    val O = freshTypeName("O") // 15
    val P = freshTypeName("P") // 16
    val Q = freshTypeName("Q") // 17
    val appliedHKT =
      tq"Type.Ctor17.Apply[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $L7, $U7, $L8, $U8, $L9, $U9, $L10, $U10, $L11, $U11, $L12, $U12, $L13, $U13, $L14, $U14, $L15, $U15, $L16, $U16, $L17, $U17, $HKT, $A, $B, $C, $D, $E, $F, $G, $H, $I, $J, $K, $L, $M, $N, $O, $P, $Q]"

    val ctx = freshName("ctx")
    val typeValue = TypeWithUnderlyingInjected(ctx, appliedHKT, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)
    val convertProvidedTypesForCrossQuotes = freshName("convertProvidedTypesForCrossQuotes")
    val termInner = freshName("Inner")
    val typeInner = TypeName(freshName("Inner").toString)

    val unchecked = q"""
    new Type.Ctor17.Bounded[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $L7, $U7, $L8, $U8, $L9, $U9, $L10, $U10, $L11, $U11, $L12, $U12, $L13, $U13, $L14, $U14, $L15, $U15, $L16, $U16, $L17, $U17, $HKT] {
      private val $ctx = CrossQuotes.ctx[_root_.scala.reflect.macros.blackbox.Context]
      private implicit def $convertProvidedTypesForCrossQuotes[$typeInner](implicit $termInner: Type[$typeInner]): $ctx.WeakTypeTag[$typeInner] =
        $termInner.asInstanceOf[$ctx.WeakTypeTag[$typeInner]]
      import $ctx.universe.{Type => _, internal => _, _}

      private val HKT = $ctx.weakTypeTag[Type.Ctor17.Stub[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $L7, $U7, $L8, $U8, $L9, $U9, $L10, $U10, $L11, $U11, $L12, $U12, $L13, $U13, $L14, $U14, $L15, $U15, $L16, $U16, $L17, $U17, $HKT]].tpe.typeArgs.last

      def apply[$A >: $L1 <: $U1: Type, $B >: $L2 <: $U2: Type, $C >: $L3 <: $U3: Type, $D >: $L4 <: $U4: Type, $E >: $L5 <: $U5: Type, $F >: $L6 <: $U6: Type, $G >: $L7 <: $U7: Type, $H >: $L8 <: $U8: Type, $I >: $L9 <: $U9: Type, $J >: $L10 <: $U10: Type, $K >: $L11 <: $U11: Type, $L >: $L12 <: $U12: Type, $M >: $L13 <: $U13: Type, $N >: $L14 <: $U14: Type, $O >: $L15 <: $U15: Type, $P >: $L16 <: $U16: Type, $Q >: $L17 <: $U17: Type]: Type[$appliedHKT] = 
        $typeValue

      def unapply[$A](A: Type[$A]): Option[($L1 <:??<: $U1, $L2 <:??<: $U2, $L3 <:??<: $U3, $L4 <:??<: $U4, $L5 <:??<: $U5, $L6 <:??<: $U6, $L7 <:??<: $U7, $L8 <:??<: $U8, $L9 <:??<: $U9, $L10 <:??<: $U10, $L11 <:??<: $U11, $L12 <:??<: $U12, $L13 <:??<: $U13, $L14 <:??<: $U14, $L15 <:??<: $U15, $L16 <:??<: $U16, $L17 <:??<: $U17)] = {
        val A0 = A.asInstanceOf[$ctx.WeakTypeTag[$A]].tpe
        A0.dealias.widen.baseType(HKT.typeSymbol) match {
          case TypeRef(_, _, List(tp1, tp2, tp3, tp4, tp5, tp6, tp7, tp8, tp9, tp10, tp11, tp12, tp13, tp14, tp15, tp16, tp17)) =>
            _root_.scala.Some(
              (
                $ctx.WeakTypeTag(tp1.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L1, $U1],
                $ctx.WeakTypeTag(tp2.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L2, $U2],
                $ctx.WeakTypeTag(tp3.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L3, $U3],
                $ctx.WeakTypeTag(tp4.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L4, $U4],
                $ctx.WeakTypeTag(tp5.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L5, $U5],
                $ctx.WeakTypeTag(tp6.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L6, $U6],
                $ctx.WeakTypeTag(tp7.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L7, $U7],
                $ctx.WeakTypeTag(tp8.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L8, $U8],
                $ctx.WeakTypeTag(tp9.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L9, $U9],
                $ctx.WeakTypeTag(tp10.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L10, $U10],
                $ctx.WeakTypeTag(tp11.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L11, $U11],
                $ctx.WeakTypeTag(tp12.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L12, $U12],
                $ctx.WeakTypeTag(tp13.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L13, $U13],
                $ctx.WeakTypeTag(tp14.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L14, $U14],
                $ctx.WeakTypeTag(tp15.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L15, $U15],
                $ctx.WeakTypeTag(tp16.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L16, $U16],
                $ctx.WeakTypeTag(tp17.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L17, $U17]
              )
            )
          case _ =>
            if (A0.typeConstructor == HKT && A0.typeArgs.size == 17) {
              val Seq(tp1, tp2, tp3, tp4, tp5, tp6, tp7, tp8, tp9, tp10, tp11, tp12, tp13, tp14, tp15, tp16, tp17) = A0.typeArgs
              _root_.scala.Some(
                (
                  $ctx.WeakTypeTag(tp1.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L1, $U1],
                  $ctx.WeakTypeTag(tp2.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L2, $U2],
                  $ctx.WeakTypeTag(tp3.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L3, $U3],
                  $ctx.WeakTypeTag(tp4.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L4, $U4],
                  $ctx.WeakTypeTag(tp5.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L5, $U5],
                  $ctx.WeakTypeTag(tp6.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L6, $U6],
                  $ctx.WeakTypeTag(tp7.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L7, $U7],
                  $ctx.WeakTypeTag(tp8.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L8, $U8],
                  $ctx.WeakTypeTag(tp9.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L9, $U9],
                  $ctx.WeakTypeTag(tp10.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L10, $U10],
                  $ctx.WeakTypeTag(tp11.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L11, $U11],
                  $ctx.WeakTypeTag(tp12.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L12, $U12],
                  $ctx.WeakTypeTag(tp13.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L13, $U13],
                  $ctx.WeakTypeTag(tp14.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L14, $U14],
                  $ctx.WeakTypeTag(tp15.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L15, $U15],
                  $ctx.WeakTypeTag(tp16.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L16, $U16],
                  $ctx.WeakTypeTag(tp17.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L17, $U17]
                )
              )
            }
            else None
        }
      }
    }
    """

    val result = suppressWarnings(c.typecheck(unchecked))

    log(
      s"""Cross-quotes ${paintExclDot(Console.BLUE)("Type.Ctor17.of")} expansion:
         |From: ${paintExclDot(Console.BLUE)(
          "Type.Ctor17.of"
        )}[${L1.tpe}, ${U1.tpe}, ${L2.tpe}, ${U2.tpe}, ${L3.tpe}, ${U3.tpe}, ${L4.tpe}, ${U4.tpe}, ${L5.tpe}, ${U5.tpe}, ${L6.tpe}, ${U6.tpe}, ${L7.tpe}, ${U7.tpe}, ${L8.tpe}, ${U8.tpe}, ${L9.tpe}, ${U9.tpe}, ${L10.tpe}, ${U10.tpe}, ${L11.tpe}, ${U11.tpe}, ${L12.tpe}, ${U12.tpe}, ${L13.tpe}, ${U13.tpe}, ${L14.tpe}, ${U14.tpe}, ${L15.tpe}, ${U15.tpe}, ${L16.tpe}, ${U16.tpe}, ${L17.tpe}, ${U17.tpe}, $HKT]
         |To: ${indent(showCodePretty(result, SyntaxHighlight.ANSI))}""".stripMargin
    )

    result
  } catch { case e: Throwable => reportIssue(e) }

  /* Replaces:
   *   Type.Ctor18.of[HKT]
   */
  // format: off
  def typeCtor18Impl[L1, U1 >: L1, L2, U2 >: L2, L3, U3 >: L3, L4, U4 >: L4, L5, U5 >: L5, L6, U6 >: L6, L7, U7 >: L7, L8, U8 >: L8, L9, U9 >: L9, L10, U10 >: L10, L11, U11 >: L11, L12, U12 >: L12, L13, U13 >: L13, L14, U14 >: L14, L15, U15 >: L15, L16, U16 >: L16, L17, U17 >: L17, L18, U18 >: L18, HKT[_ >: L1 <: U1, _ >: L2 <: U2, _ >: L3 <: U3, _ >: L4 <: U4, _ >: L5 <: U5, _ >: L6 <: U6, _ >: L7 <: U7, _ >: L8 <: U8, _ >: L9 <: U9, _ >: L10 <: U10, _ >: L11 <: U11, _ >: L12 <: U12, _ >: L13 <: U13, _ >: L14 <: U14, _ >: L15 <: U15, _ >: L16 <: U16, _ >: L17 <: U17, _ >: L18 <: U18]](implicit
  // format: on
      L1: c.WeakTypeTag[L1],
      U1: c.WeakTypeTag[U1],
      L2: c.WeakTypeTag[L2],
      U2: c.WeakTypeTag[U2],
      L3: c.WeakTypeTag[L3],
      U3: c.WeakTypeTag[U3],
      L4: c.WeakTypeTag[L4],
      U4: c.WeakTypeTag[U4],
      L5: c.WeakTypeTag[L5],
      U5: c.WeakTypeTag[U5],
      L6: c.WeakTypeTag[L6],
      U6: c.WeakTypeTag[U6],
      L7: c.WeakTypeTag[L7],
      U7: c.WeakTypeTag[U7],
      L8: c.WeakTypeTag[L8],
      U8: c.WeakTypeTag[U8],
      L9: c.WeakTypeTag[L9],
      U9: c.WeakTypeTag[U9],
      L10: c.WeakTypeTag[L10],
      U10: c.WeakTypeTag[U10],
      L11: c.WeakTypeTag[L11],
      U11: c.WeakTypeTag[U11],
      L12: c.WeakTypeTag[L12],
      U12: c.WeakTypeTag[U12],
      L13: c.WeakTypeTag[L13],
      U13: c.WeakTypeTag[U13],
      L14: c.WeakTypeTag[L14],
      U14: c.WeakTypeTag[U14],
      L15: c.WeakTypeTag[L15],
      U15: c.WeakTypeTag[U15],
      L16: c.WeakTypeTag[L16],
      U16: c.WeakTypeTag[U16],
      L17: c.WeakTypeTag[L17],
      U17: c.WeakTypeTag[U17],
      L18: c.WeakTypeTag[L18],
      U18: c.WeakTypeTag[U18],
      HKTE: c.WeakTypeTag[HKT[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]
  ): c.Tree = try {
    assert(HKTE.tpe.typeParams.size == 18, "HKT must have exactly eighteen type parameters")

    val HKT = HKTE.tpe.typeConstructor
    val A = freshTypeName("A") // 1
    val B = freshTypeName("B") // 2
    val C = freshTypeName("C") // 3
    val D = freshTypeName("D") // 4
    val E = freshTypeName("E") // 5
    val F = freshTypeName("F") // 6
    val G = freshTypeName("G") // 7
    val H = freshTypeName("H") // 8
    val I = freshTypeName("I") // 9
    val J = freshTypeName("J") // 10
    val K = freshTypeName("K") // 11
    val L = freshTypeName("L") // 12
    val M = freshTypeName("M") // 13
    val N = freshTypeName("N") // 14
    val O = freshTypeName("O") // 15
    val P = freshTypeName("P") // 16
    val Q = freshTypeName("Q") // 17
    val R = freshTypeName("R") // 18
    val appliedHKT =
      tq"Type.Ctor18.Apply[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $L7, $U7, $L8, $U8, $L9, $U9, $L10, $U10, $L11, $U11, $L12, $U12, $L13, $U13, $L14, $U14, $L15, $U15, $L16, $U16, $L17, $U17, $L18, $U18, $HKT, $A, $B, $C, $D, $E, $F, $G, $H, $I, $J, $K, $L, $M, $N, $O, $P, $Q, $R]"

    val ctx = freshName("ctx")
    val typeValue = TypeWithUnderlyingInjected(ctx, appliedHKT, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)
    val convertProvidedTypesForCrossQuotes = freshName("convertProvidedTypesForCrossQuotes")
    val termInner = freshName("Inner")
    val typeInner = TypeName(freshName("Inner").toString)

    val unchecked = q"""
    new Type.Ctor18.Bounded[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $L7, $U7, $L8, $U8, $L9, $U9, $L10, $U10, $L11, $U11, $L12, $U12, $L13, $U13, $L14, $U14, $L15, $U15, $L16, $U16, $L17, $U17, $L18, $U18, $HKT] {
      private val $ctx = CrossQuotes.ctx[_root_.scala.reflect.macros.blackbox.Context]
      private implicit def $convertProvidedTypesForCrossQuotes[$typeInner](implicit $termInner: Type[$typeInner]): $ctx.WeakTypeTag[$typeInner] =
        $termInner.asInstanceOf[$ctx.WeakTypeTag[$typeInner]]
      import $ctx.universe.{Type => _, internal => _, _}

      private val HKT = $ctx.weakTypeTag[Type.Ctor18.Stub[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $L7, $U7, $L8, $U8, $L9, $U9, $L10, $U10, $L11, $U11, $L12, $U12, $L13, $U13, $L14, $U14, $L15, $U15, $L16, $U16, $L17, $U17, $L18, $U18, $HKT]].tpe.typeArgs.last

      def apply[$A >: $L1 <: $U1: Type, $B >: $L2 <: $U2: Type, $C >: $L3 <: $U3: Type, $D >: $L4 <: $U4: Type, $E >: $L5 <: $U5: Type, $F >: $L6 <: $U6: Type, $G >: $L7 <: $U7: Type, $H >: $L8 <: $U8: Type, $I >: $L9 <: $U9: Type, $J >: $L10 <: $U10: Type, $K >: $L11 <: $U11: Type, $L >: $L12 <: $U12: Type, $M >: $L13 <: $U13: Type, $N >: $L14 <: $U14: Type, $O >: $L15 <: $U15: Type, $P >: $L16 <: $U16: Type, $Q >: $L17 <: $U17: Type, $R >: $L18 <: $U18: Type]: Type[$appliedHKT] = 
        $typeValue

      def unapply[$A](A: Type[$A]): Option[($L1 <:??<: $U1, $L2 <:??<: $U2, $L3 <:??<: $U3, $L4 <:??<: $U4, $L5 <:??<: $U5, $L6 <:??<: $U6, $L7 <:??<: $U7, $L8 <:??<: $U8, $L9 <:??<: $U9, $L10 <:??<: $U10, $L11 <:??<: $U11, $L12 <:??<: $U12, $L13 <:??<: $U13, $L14 <:??<: $U14, $L15 <:??<: $U15, $L16 <:??<: $U16, $L17 <:??<: $U17, $L18 <:??<: $U18)] = {
        val A0 = A.asInstanceOf[$ctx.WeakTypeTag[$A]].tpe
        A0.dealias.widen.baseType(HKT.typeSymbol) match {
          case TypeRef(_, _, List(tp1, tp2, tp3, tp4, tp5, tp6, tp7, tp8, tp9, tp10, tp11, tp12, tp13, tp14, tp15, tp16, tp17, tp18)) =>
            _root_.scala.Some(
              (
                $ctx.WeakTypeTag(tp1.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L1, $U1],
                $ctx.WeakTypeTag(tp2.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L2, $U2],
                $ctx.WeakTypeTag(tp3.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L3, $U3],
                $ctx.WeakTypeTag(tp4.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L4, $U4],
                $ctx.WeakTypeTag(tp5.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L5, $U5],
                $ctx.WeakTypeTag(tp6.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L6, $U6],
                $ctx.WeakTypeTag(tp7.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L7, $U7],
                $ctx.WeakTypeTag(tp8.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L8, $U8],
                $ctx.WeakTypeTag(tp9.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L9, $U9],
                $ctx.WeakTypeTag(tp10.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L10, $U10],
                $ctx.WeakTypeTag(tp11.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L11, $U11],
                $ctx.WeakTypeTag(tp12.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L12, $U12],
                $ctx.WeakTypeTag(tp13.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L13, $U13],
                $ctx.WeakTypeTag(tp14.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L14, $U14],
                $ctx.WeakTypeTag(tp15.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L15, $U15],
                $ctx.WeakTypeTag(tp16.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L16, $U16],
                $ctx.WeakTypeTag(tp17.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L17, $U17],
                $ctx.WeakTypeTag(tp18.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L18, $U18]
              )
            )
          case _ =>
            if (A0.typeConstructor == HKT && A0.typeArgs.size == 18) {
              val Seq(tp1, tp2, tp3, tp4, tp5, tp6, tp7, tp8, tp9, tp10, tp11, tp12, tp13, tp14, tp15, tp16, tp17, tp18) = A0.typeArgs
              _root_.scala.Some(
                (
                  $ctx.WeakTypeTag(tp1.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L1, $U1],
                  $ctx.WeakTypeTag(tp2.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L2, $U2],
                  $ctx.WeakTypeTag(tp3.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L3, $U3],
                  $ctx.WeakTypeTag(tp4.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L4, $U4],
                  $ctx.WeakTypeTag(tp5.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L5, $U5],
                  $ctx.WeakTypeTag(tp6.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L6, $U6],
                  $ctx.WeakTypeTag(tp7.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L7, $U7],
                  $ctx.WeakTypeTag(tp8.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L8, $U8],
                  $ctx.WeakTypeTag(tp9.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L9, $U9],
                  $ctx.WeakTypeTag(tp10.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L10, $U10],
                  $ctx.WeakTypeTag(tp11.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L11, $U11],
                  $ctx.WeakTypeTag(tp12.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L12, $U12],
                  $ctx.WeakTypeTag(tp13.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L13, $U13],
                  $ctx.WeakTypeTag(tp14.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L14, $U14],
                  $ctx.WeakTypeTag(tp15.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L15, $U15],
                  $ctx.WeakTypeTag(tp16.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L16, $U16],
                  $ctx.WeakTypeTag(tp17.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L17, $U17],
                  $ctx.WeakTypeTag(tp18.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L18, $U18]
                )
              )
            }
            else None
        }
      }
    }
    """

    val result = suppressWarnings(c.typecheck(unchecked))

    log(
      s"""Cross-quotes ${paintExclDot(Console.BLUE)("Type.Ctor18.of")} expansion:
         |From: ${paintExclDot(Console.BLUE)(
          "Type.Ctor18.of"
        )}[${L1.tpe}, ${U1.tpe}, ${L2.tpe}, ${U2.tpe}, ${L3.tpe}, ${U3.tpe}, ${L4.tpe}, ${U4.tpe}, ${L5.tpe}, ${U5.tpe}, ${L6.tpe}, ${U6.tpe}, ${L7.tpe}, ${U7.tpe}, ${L8.tpe}, ${U8.tpe}, ${L9.tpe}, ${U9.tpe}, ${L10.tpe}, ${U10.tpe}, ${L11.tpe}, ${U11.tpe}, ${L12.tpe}, ${U12.tpe}, ${L13.tpe}, ${U13.tpe}, ${L14.tpe}, ${U14.tpe}, ${L15.tpe}, ${U15.tpe}, ${L16.tpe}, ${U16.tpe}, ${L17.tpe}, ${U17.tpe}, ${L18.tpe}, ${U18.tpe}, $HKT]
         |To: ${indent(showCodePretty(result, SyntaxHighlight.ANSI))}""".stripMargin
    )

    result
  } catch { case e: Throwable => reportIssue(e) }

  /* Replaces:
   *   Type.Ctor19.of[HKT]
   */
  // format: off
  def typeCtor19Impl[L1, U1 >: L1, L2, U2 >: L2, L3, U3 >: L3, L4, U4 >: L4, L5, U5 >: L5, L6, U6 >: L6, L7, U7 >: L7, L8, U8 >: L8, L9, U9 >: L9, L10, U10 >: L10, L11, U11 >: L11, L12, U12 >: L12, L13, U13 >: L13, L14, U14 >: L14, L15, U15 >: L15, L16, U16 >: L16, L17, U17 >: L17, L18, U18 >: L18, L19, U19 >: L19, HKT[_ >: L1 <: U1, _ >: L2 <: U2, _ >: L3 <: U3, _ >: L4 <: U4, _ >: L5 <: U5, _ >: L6 <: U6, _ >: L7 <: U7, _ >: L8 <: U8, _ >: L9 <: U9, _ >: L10 <: U10, _ >: L11 <: U11, _ >: L12 <: U12, _ >: L13 <: U13, _ >: L14 <: U14, _ >: L15 <: U15, _ >: L16 <: U16, _ >: L17 <: U17, _ >: L18 <: U18, _ >: L19 <: U19]](implicit
  // format: on
      L1: c.WeakTypeTag[L1],
      U1: c.WeakTypeTag[U1],
      L2: c.WeakTypeTag[L2],
      U2: c.WeakTypeTag[U2],
      L3: c.WeakTypeTag[L3],
      U3: c.WeakTypeTag[U3],
      L4: c.WeakTypeTag[L4],
      U4: c.WeakTypeTag[U4],
      L5: c.WeakTypeTag[L5],
      U5: c.WeakTypeTag[U5],
      L6: c.WeakTypeTag[L6],
      U6: c.WeakTypeTag[U6],
      L7: c.WeakTypeTag[L7],
      U7: c.WeakTypeTag[U7],
      L8: c.WeakTypeTag[L8],
      U8: c.WeakTypeTag[U8],
      L9: c.WeakTypeTag[L9],
      U9: c.WeakTypeTag[U9],
      L10: c.WeakTypeTag[L10],
      U10: c.WeakTypeTag[U10],
      L11: c.WeakTypeTag[L11],
      U11: c.WeakTypeTag[U11],
      L12: c.WeakTypeTag[L12],
      U12: c.WeakTypeTag[U12],
      L13: c.WeakTypeTag[L13],
      U13: c.WeakTypeTag[U13],
      L14: c.WeakTypeTag[L14],
      U14: c.WeakTypeTag[U14],
      L15: c.WeakTypeTag[L15],
      U15: c.WeakTypeTag[U15],
      L16: c.WeakTypeTag[L16],
      U16: c.WeakTypeTag[U16],
      L17: c.WeakTypeTag[L17],
      U17: c.WeakTypeTag[U17],
      L18: c.WeakTypeTag[L18],
      U18: c.WeakTypeTag[U18],
      L19: c.WeakTypeTag[L19],
      U19: c.WeakTypeTag[U19],
      HKTE: c.WeakTypeTag[HKT[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]
  ): c.Tree = try {
    assert(HKTE.tpe.typeParams.size == 19, "HKT must have exactly nineteen type parameters")

    val HKT = HKTE.tpe.typeConstructor
    val A = freshTypeName("A") // 1
    val B = freshTypeName("B") // 2
    val C = freshTypeName("C") // 3
    val D = freshTypeName("D") // 4
    val E = freshTypeName("E") // 5
    val F = freshTypeName("F") // 6
    val G = freshTypeName("G") // 7
    val H = freshTypeName("H") // 8
    val I = freshTypeName("I") // 9
    val J = freshTypeName("J") // 10
    val K = freshTypeName("K") // 11
    val L = freshTypeName("L") // 12
    val M = freshTypeName("M") // 13
    val N = freshTypeName("N") // 14
    val O = freshTypeName("O") // 15
    val P = freshTypeName("P") // 16
    val Q = freshTypeName("Q") // 17
    val R = freshTypeName("R") // 18
    val S = freshTypeName("S") // 19
    val appliedHKT =
      tq"Type.Ctor19.Apply[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $L7, $U7, $L8, $U8, $L9, $U9, $L10, $U10, $L11, $U11, $L12, $U12, $L13, $U13, $L14, $U14, $L15, $U15, $L16, $U16, $L17, $U17, $L18, $U18, $L19, $U19, $HKT, $A, $B, $C, $D, $E, $F, $G, $H, $I, $J, $K, $L, $M, $N, $O, $P, $Q, $R, $S]"

    val ctx = freshName("ctx")
    val typeValue =
      TypeWithUnderlyingInjected(ctx, appliedHKT, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)
    val convertProvidedTypesForCrossQuotes = freshName("convertProvidedTypesForCrossQuotes")
    val termInner = freshName("Inner")
    val typeInner = TypeName(freshName("Inner").toString)

    val unchecked = q"""
    new Type.Ctor19.Bounded[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $L7, $U7, $L8, $U8, $L9, $U9, $L10, $U10, $L11, $U11, $L12, $U12, $L13, $U13, $L14, $U14, $L15, $U15, $L16, $U16, $L17, $U17, $L18, $U18, $L19, $U19, $HKT] {
      private val $ctx = CrossQuotes.ctx[_root_.scala.reflect.macros.blackbox.Context]
      private implicit def $convertProvidedTypesForCrossQuotes[$typeInner](implicit $termInner: Type[$typeInner]): $ctx.WeakTypeTag[$typeInner] =
        $termInner.asInstanceOf[$ctx.WeakTypeTag[$typeInner]]
      import $ctx.universe.{Type => _, internal => _, _}

      private val HKT = $ctx.weakTypeTag[Type.Ctor19.Stub[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $L7, $U7, $L8, $U8, $L9, $U9, $L10, $U10, $L11, $U11, $L12, $U12, $L13, $U13, $L14, $U14, $L15, $U15, $L16, $U16, $L17, $U17, $L18, $U18, $L19, $U19, $HKT]].tpe.typeArgs.last

      def apply[$A >: $L1 <: $U1: Type, $B >: $L2 <: $U2: Type, $C >: $L3 <: $U3: Type, $D >: $L4 <: $U4: Type, $E >: $L5 <: $U5: Type, $F >: $L6 <: $U6: Type, $G >: $L7 <: $U7: Type, $H >: $L8 <: $U8: Type, $I >: $L9 <: $U9: Type, $J >: $L10 <: $U10: Type, $K >: $L11 <: $U11: Type, $L >: $L12 <: $U12: Type, $M >: $L13 <: $U13: Type, $N >: $L14 <: $U14: Type, $O >: $L15 <: $U15: Type, $P >: $L16 <: $U16: Type, $Q >: $L17 <: $U17: Type, $R >: $L18 <: $U18: Type, $S >: $L19 <: $U19: Type]: Type[$appliedHKT] = 
        $typeValue

      def unapply[$A](A: Type[$A]): Option[($L1 <:??<: $U1, $L2 <:??<: $U2, $L3 <:??<: $U3, $L4 <:??<: $U4, $L5 <:??<: $U5, $L6 <:??<: $U6, $L7 <:??<: $U7, $L8 <:??<: $U8, $L9 <:??<: $U9, $L10 <:??<: $U10, $L11 <:??<: $U11, $L12 <:??<: $U12, $L13 <:??<: $U13, $L14 <:??<: $U14, $L15 <:??<: $U15, $L16 <:??<: $U16, $L17 <:??<: $U17, $L18 <:??<: $U18, $L19 <:??<: $U19)] = {
        val A0 = A.asInstanceOf[$ctx.WeakTypeTag[$A]].tpe
        A0.dealias.widen.baseType(HKT.typeSymbol) match {
          case TypeRef(_, _, List(tp1, tp2, tp3, tp4, tp5, tp6, tp7, tp8, tp9, tp10, tp11, tp12, tp13, tp14, tp15, tp16, tp17, tp18, tp19)) =>
            _root_.scala.Some(
              (
                $ctx.WeakTypeTag(tp1.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L1, $U1],
                $ctx.WeakTypeTag(tp2.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L2, $U2],
                $ctx.WeakTypeTag(tp3.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L3, $U3],
                $ctx.WeakTypeTag(tp4.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L4, $U4],
                $ctx.WeakTypeTag(tp5.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L5, $U5],
                $ctx.WeakTypeTag(tp6.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L6, $U6],
                $ctx.WeakTypeTag(tp7.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L7, $U7],
                $ctx.WeakTypeTag(tp8.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L8, $U8],
                $ctx.WeakTypeTag(tp9.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L9, $U9],
                $ctx.WeakTypeTag(tp10.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L10, $U10],
                $ctx.WeakTypeTag(tp11.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L11, $U11],
                $ctx.WeakTypeTag(tp12.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L12, $U12],
                $ctx.WeakTypeTag(tp13.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L13, $U13],
                $ctx.WeakTypeTag(tp14.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L14, $U14],
                $ctx.WeakTypeTag(tp15.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L15, $U15],
                $ctx.WeakTypeTag(tp16.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L16, $U16],
                $ctx.WeakTypeTag(tp17.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L17, $U17],
                $ctx.WeakTypeTag(tp18.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L18, $U18],
                $ctx.WeakTypeTag(tp19.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L19, $U19]
              )
            )
          case _ =>
            if (A0.typeConstructor == HKT && A0.typeArgs.size == 19) {
              val Seq(tp1, tp2, tp3, tp4, tp5, tp6, tp7, tp8, tp9, tp10, tp11, tp12, tp13, tp14, tp15, tp16, tp17, tp18, tp19) = A0.typeArgs
              _root_.scala.Some(
                (
                  $ctx.WeakTypeTag(tp1.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L1, $U1],
                  $ctx.WeakTypeTag(tp2.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L2, $U2],
                  $ctx.WeakTypeTag(tp3.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L3, $U3],
                  $ctx.WeakTypeTag(tp4.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L4, $U4],
                  $ctx.WeakTypeTag(tp5.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L5, $U5],
                  $ctx.WeakTypeTag(tp6.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L6, $U6],
                  $ctx.WeakTypeTag(tp7.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L7, $U7],
                  $ctx.WeakTypeTag(tp8.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L8, $U8],
                  $ctx.WeakTypeTag(tp9.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L9, $U9],
                  $ctx.WeakTypeTag(tp10.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L10, $U10],
                  $ctx.WeakTypeTag(tp11.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L11, $U11],
                  $ctx.WeakTypeTag(tp12.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L12, $U12],
                  $ctx.WeakTypeTag(tp13.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L13, $U13],
                  $ctx.WeakTypeTag(tp14.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L14, $U14],
                  $ctx.WeakTypeTag(tp15.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L15, $U15],
                  $ctx.WeakTypeTag(tp16.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L16, $U16],
                  $ctx.WeakTypeTag(tp17.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L17, $U17],
                  $ctx.WeakTypeTag(tp18.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L18, $U18],
                  $ctx.WeakTypeTag(tp19.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L19, $U19]
                )
              )
            }
            else None
        }
      }
    }
    """

    val result = suppressWarnings(c.typecheck(unchecked))

    log(
      s"""Cross-quotes ${paintExclDot(Console.BLUE)("Type.Ctor19.of")} expansion:
         |From: ${paintExclDot(Console.BLUE)(
          "Type.Ctor19.of"
        )}[${L1.tpe}, ${U1.tpe}, ${L2.tpe}, ${U2.tpe}, ${L3.tpe}, ${U3.tpe}, ${L4.tpe}, ${U4.tpe}, ${L5.tpe}, ${U5.tpe}, ${L6.tpe}, ${U6.tpe}, ${L7.tpe}, ${U7.tpe}, ${L8.tpe}, ${U8.tpe}, ${L9.tpe}, ${U9.tpe}, ${L10.tpe}, ${U10.tpe}, ${L11.tpe}, ${U11.tpe}, ${L12.tpe}, ${U12.tpe}, ${L13.tpe}, ${U13.tpe}, ${L14.tpe}, ${U14.tpe}, ${L15.tpe}, ${U15.tpe}, ${L16.tpe}, ${U16.tpe}, ${L17.tpe}, ${U17.tpe}, ${L18.tpe}, ${U18.tpe}, ${L19.tpe}, ${U19.tpe}, $HKT]
         |To: ${indent(showCodePretty(result, SyntaxHighlight.ANSI))}""".stripMargin
    )

    result
  } catch { case e: Throwable => reportIssue(e) }

  /* Replaces:
   *   Type.Ctor20.of[HKT]
   */
  // format: off
  def typeCtor20Impl[L1, U1 >: L1, L2, U2 >: L2, L3, U3 >: L3, L4, U4 >: L4, L5, U5 >: L5, L6, U6 >: L6, L7, U7 >: L7, L8, U8 >: L8, L9, U9 >: L9, L10, U10 >: L10, L11, U11 >: L11, L12, U12 >: L12, L13, U13 >: L13, L14, U14 >: L14, L15, U15 >: L15, L16, U16 >: L16, L17, U17 >: L17, L18, U18 >: L18, L19, U19 >: L19, L20, U20 >: L20, HKT[_ >: L1 <: U1, _ >: L2 <: U2, _ >: L3 <: U3, _ >: L4 <: U4, _ >: L5 <: U5, _ >: L6 <: U6, _ >: L7 <: U7, _ >: L8 <: U8, _ >: L9 <: U9, _ >: L10 <: U10, _ >: L11 <: U11, _ >: L12 <: U12, _ >: L13 <: U13, _ >: L14 <: U14, _ >: L15 <: U15, _ >: L16 <: U16, _ >: L17 <: U17, _ >: L18 <: U18, _ >: L19 <: U19, _ >: L20 <: U20]](implicit
  // format: on
      L1: c.WeakTypeTag[L1],
      U1: c.WeakTypeTag[U1],
      L2: c.WeakTypeTag[L2],
      U2: c.WeakTypeTag[U2],
      L3: c.WeakTypeTag[L3],
      U3: c.WeakTypeTag[U3],
      L4: c.WeakTypeTag[L4],
      U4: c.WeakTypeTag[U4],
      L5: c.WeakTypeTag[L5],
      U5: c.WeakTypeTag[U5],
      L6: c.WeakTypeTag[L6],
      U6: c.WeakTypeTag[U6],
      L7: c.WeakTypeTag[L7],
      U7: c.WeakTypeTag[U7],
      L8: c.WeakTypeTag[L8],
      U8: c.WeakTypeTag[U8],
      L9: c.WeakTypeTag[L9],
      U9: c.WeakTypeTag[U9],
      L10: c.WeakTypeTag[L10],
      U10: c.WeakTypeTag[U10],
      L11: c.WeakTypeTag[L11],
      U11: c.WeakTypeTag[U11],
      L12: c.WeakTypeTag[L12],
      U12: c.WeakTypeTag[U12],
      L13: c.WeakTypeTag[L13],
      U13: c.WeakTypeTag[U13],
      L14: c.WeakTypeTag[L14],
      U14: c.WeakTypeTag[U14],
      L15: c.WeakTypeTag[L15],
      U15: c.WeakTypeTag[U15],
      L16: c.WeakTypeTag[L16],
      U16: c.WeakTypeTag[U16],
      L17: c.WeakTypeTag[L17],
      U17: c.WeakTypeTag[U17],
      L18: c.WeakTypeTag[L18],
      U18: c.WeakTypeTag[U18],
      L19: c.WeakTypeTag[L19],
      U19: c.WeakTypeTag[U19],
      L20: c.WeakTypeTag[L20],
      U20: c.WeakTypeTag[U20],
      HKTE: c.WeakTypeTag[HKT[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]
  ): c.Tree = try {
    assert(HKTE.tpe.typeParams.size == 20, "HKT must have exactly twenty type parameters")

    val HKT = HKTE.tpe.typeConstructor
    val A = freshTypeName("A") // 1
    val B = freshTypeName("B") // 2
    val C = freshTypeName("C") // 3
    val D = freshTypeName("D") // 4
    val E = freshTypeName("E") // 5
    val F = freshTypeName("F") // 6
    val G = freshTypeName("G") // 7
    val H = freshTypeName("H") // 8
    val I = freshTypeName("I") // 9
    val J = freshTypeName("J") // 10
    val K = freshTypeName("K") // 11
    val L = freshTypeName("L") // 12
    val M = freshTypeName("M") // 13
    val N = freshTypeName("N") // 14
    val O = freshTypeName("O") // 15
    val P = freshTypeName("P") // 16
    val Q = freshTypeName("Q") // 17
    val R = freshTypeName("R") // 18
    val S = freshTypeName("S") // 19
    val T = freshTypeName("T") // 20
    val appliedHKT =
      tq"Type.Ctor20.Apply[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $L7, $U7, $L8, $U8, $L9, $U9, $L10, $U10, $L11, $U11, $L12, $U12, $L13, $U13, $L14, $U14, $L15, $U15, $L16, $U16, $L17, $U17, $L18, $U18, $L19, $U19, $L20, $U20, $HKT, $A, $B, $C, $D, $E, $F, $G, $H, $I, $J, $K, $L, $M, $N, $O, $P, $Q, $R, $S, $T]"

    val ctx = freshName("ctx")
    val typeValue =
      TypeWithUnderlyingInjected(ctx, appliedHKT, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)
    val convertProvidedTypesForCrossQuotes = freshName("convertProvidedTypesForCrossQuotes")
    val termInner = freshName("Inner")
    val typeInner = TypeName(freshName("Inner").toString)

    val unchecked = q"""
    new Type.Ctor20.Bounded[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $L7, $U7, $L8, $U8, $L9, $U9, $L10, $U10, $L11, $U11, $L12, $U12, $L13, $U13, $L14, $U14, $L15, $U15, $L16, $U16, $L17, $U17, $L18, $U18, $L19, $U19, $L20, $U20, $HKT] {
      private val $ctx = CrossQuotes.ctx[_root_.scala.reflect.macros.blackbox.Context]
      private implicit def $convertProvidedTypesForCrossQuotes[$typeInner](implicit $termInner: Type[$typeInner]): $ctx.WeakTypeTag[$typeInner] =
        $termInner.asInstanceOf[$ctx.WeakTypeTag[$typeInner]]
      import $ctx.universe.{Type => _, internal => _, _}

      private val HKT = $ctx.weakTypeTag[Type.Ctor20.Stub[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $L7, $U7, $L8, $U8, $L9, $U9, $L10, $U10, $L11, $U11, $L12, $U12, $L13, $U13, $L14, $U14, $L15, $U15, $L16, $U16, $L17, $U17, $L18, $U18, $L19, $U19, $L20, $U20, $HKT]].tpe.typeArgs.last

      def apply[$A >: $L1 <: $U1: Type, $B >: $L2 <: $U2: Type, $C >: $L3 <: $U3: Type, $D >: $L4 <: $U4: Type, $E >: $L5 <: $U5: Type, $F >: $L6 <: $U6: Type, $G >: $L7 <: $U7: Type, $H >: $L8 <: $U8: Type, $I >: $L9 <: $U9: Type, $J >: $L10 <: $U10: Type, $K >: $L11 <: $U11: Type, $L >: $L12 <: $U12: Type, $M >: $L13 <: $U13: Type, $N >: $L14 <: $U14: Type, $O >: $L15 <: $U15: Type, $P >: $L16 <: $U16: Type, $Q >: $L17 <: $U17: Type, $R >: $L18 <: $U18: Type, $S >: $L19 <: $U19: Type, $T >: $L20 <: $U20: Type]: Type[$appliedHKT] = 
        $typeValue

      def unapply[$A](A: Type[$A]): Option[($L1 <:??<: $U1, $L2 <:??<: $U2, $L3 <:??<: $U3, $L4 <:??<: $U4, $L5 <:??<: $U5, $L6 <:??<: $U6, $L7 <:??<: $U7, $L8 <:??<: $U8, $L9 <:??<: $U9, $L10 <:??<: $U10, $L11 <:??<: $U11, $L12 <:??<: $U12, $L13 <:??<: $U13, $L14 <:??<: $U14, $L15 <:??<: $U15, $L16 <:??<: $U16, $L17 <:??<: $U17, $L18 <:??<: $U18, $L19 <:??<: $U19, $L20 <:??<: $U20)] = {
        val A0 = A.asInstanceOf[$ctx.WeakTypeTag[$A]].tpe
        A0.dealias.widen.baseType(HKT.typeSymbol) match {
          case TypeRef(_, _, List(tp1, tp2, tp3, tp4, tp5, tp6, tp7, tp8, tp9, tp10, tp11, tp12, tp13, tp14, tp15, tp16, tp17, tp18, tp19, tp20)) =>
            _root_.scala.Some(
              (
                $ctx.WeakTypeTag(tp1.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L1, $U1],
                $ctx.WeakTypeTag(tp2.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L2, $U2],
                $ctx.WeakTypeTag(tp3.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L3, $U3],
                $ctx.WeakTypeTag(tp4.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L4, $U4],
                $ctx.WeakTypeTag(tp5.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L5, $U5],
                $ctx.WeakTypeTag(tp6.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L6, $U6],
                $ctx.WeakTypeTag(tp7.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L7, $U7],
                $ctx.WeakTypeTag(tp8.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L8, $U8],
                $ctx.WeakTypeTag(tp9.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L9, $U9],
                $ctx.WeakTypeTag(tp10.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L10, $U10],
                $ctx.WeakTypeTag(tp11.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L11, $U11],
                $ctx.WeakTypeTag(tp12.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L12, $U12],
                $ctx.WeakTypeTag(tp13.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L13, $U13],
                $ctx.WeakTypeTag(tp14.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L14, $U14],
                $ctx.WeakTypeTag(tp15.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L15, $U15],
                $ctx.WeakTypeTag(tp16.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L16, $U16],
                $ctx.WeakTypeTag(tp17.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L17, $U17],
                $ctx.WeakTypeTag(tp18.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L18, $U18],
                $ctx.WeakTypeTag(tp19.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L19, $U19],
                $ctx.WeakTypeTag(tp20.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L20, $U20]
              )
            )
          case _ =>
            if (A0.typeConstructor == HKT && A0.typeArgs.size == 20) {
              val Seq(tp1, tp2, tp3, tp4, tp5, tp6, tp7, tp8, tp9, tp10, tp11, tp12, tp13, tp14, tp15, tp16, tp17, tp18, tp19, tp20) = A0.typeArgs
              _root_.scala.Some(
                (
                  $ctx.WeakTypeTag(tp1.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L1, $U1],
                  $ctx.WeakTypeTag(tp2.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L2, $U2],
                  $ctx.WeakTypeTag(tp3.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L3, $U3],
                  $ctx.WeakTypeTag(tp4.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L4, $U4],
                  $ctx.WeakTypeTag(tp5.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L5, $U5],
                  $ctx.WeakTypeTag(tp6.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L6, $U6],
                  $ctx.WeakTypeTag(tp7.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L7, $U7],
                  $ctx.WeakTypeTag(tp8.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L8, $U8],
                  $ctx.WeakTypeTag(tp9.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L9, $U9],
                  $ctx.WeakTypeTag(tp10.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L10, $U10],
                  $ctx.WeakTypeTag(tp11.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L11, $U11],
                  $ctx.WeakTypeTag(tp12.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L12, $U12],
                  $ctx.WeakTypeTag(tp13.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L13, $U13],
                  $ctx.WeakTypeTag(tp14.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L14, $U14],
                  $ctx.WeakTypeTag(tp15.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L15, $U15],
                  $ctx.WeakTypeTag(tp16.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L16, $U16],
                  $ctx.WeakTypeTag(tp17.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L17, $U17],
                  $ctx.WeakTypeTag(tp18.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L18, $U18],
                  $ctx.WeakTypeTag(tp19.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L19, $U19],
                  $ctx.WeakTypeTag(tp20.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L20, $U20]
                )
              )
            }
            else None
        }
      }
    }
    """

    val result = suppressWarnings(c.typecheck(unchecked))

    log(
      s"""Cross-quotes ${paintExclDot(Console.BLUE)("Type.Ctor20.of")} expansion:
         |From: ${paintExclDot(Console.BLUE)(
          "Type.Ctor20.of"
        )}[${L1.tpe}, ${U1.tpe}, ${L2.tpe}, ${U2.tpe}, ${L3.tpe}, ${U3.tpe}, ${L4.tpe}, ${U4.tpe}, ${L5.tpe}, ${U5.tpe}, ${L6.tpe}, ${U6.tpe}, ${L7.tpe}, ${U7.tpe}, ${L8.tpe}, ${U8.tpe}, ${L9.tpe}, ${U9.tpe}, ${L10.tpe}, ${U10.tpe}, ${L11.tpe}, ${U11.tpe}, ${L12.tpe}, ${U12.tpe}, ${L13.tpe}, ${U13.tpe}, ${L14.tpe}, ${U14.tpe}, ${L15.tpe}, ${U15.tpe}, ${L16.tpe}, ${U16.tpe}, ${L17.tpe}, ${U17.tpe}, ${L18.tpe}, ${U18.tpe}, ${L19.tpe}, ${U19.tpe}, ${L20.tpe}, ${U20.tpe}, $HKT]
         |To: ${indent(showCodePretty(result, SyntaxHighlight.ANSI))}""".stripMargin
    )

    result
  } catch { case e: Throwable => reportIssue(e) }

  /* Replaces:
   *   Type.Ctor21.of[HKT]
   */
  // format: off
  def typeCtor21Impl[L1, U1 >: L1, L2, U2 >: L2, L3, U3 >: L3, L4, U4 >: L4, L5, U5 >: L5, L6, U6 >: L6, L7, U7 >: L7, L8, U8 >: L8, L9, U9 >: L9, L10, U10 >: L10, L11, U11 >: L11, L12, U12 >: L12, L13, U13 >: L13, L14, U14 >: L14, L15, U15 >: L15, L16, U16 >: L16, L17, U17 >: L17, L18, U18 >: L18, L19, U19 >: L19, L20, U20 >: L20, L21, U21 >: L21, HKT[_ >: L1 <: U1, _ >: L2 <: U2, _ >: L3 <: U3, _ >: L4 <: U4, _ >: L5 <: U5, _ >: L6 <: U6, _ >: L7 <: U7, _ >: L8 <: U8, _ >: L9 <: U9, _ >: L10 <: U10, _ >: L11 <: U11, _ >: L12 <: U12, _ >: L13 <: U13, _ >: L14 <: U14, _ >: L15 <: U15, _ >: L16 <: U16, _ >: L17 <: U17, _ >: L18 <: U18, _ >: L19 <: U19, _ >: L20 <: U20, _ >: L21 <: U21]](implicit
  // format: on
      L1: c.WeakTypeTag[L1],
      U1: c.WeakTypeTag[U1],
      L2: c.WeakTypeTag[L2],
      U2: c.WeakTypeTag[U2],
      L3: c.WeakTypeTag[L3],
      U3: c.WeakTypeTag[U3],
      L4: c.WeakTypeTag[L4],
      U4: c.WeakTypeTag[U4],
      L5: c.WeakTypeTag[L5],
      U5: c.WeakTypeTag[U5],
      L6: c.WeakTypeTag[L6],
      U6: c.WeakTypeTag[U6],
      L7: c.WeakTypeTag[L7],
      U7: c.WeakTypeTag[U7],
      L8: c.WeakTypeTag[L8],
      U8: c.WeakTypeTag[U8],
      L9: c.WeakTypeTag[L9],
      U9: c.WeakTypeTag[U9],
      L10: c.WeakTypeTag[L10],
      U10: c.WeakTypeTag[U10],
      L11: c.WeakTypeTag[L11],
      U11: c.WeakTypeTag[U11],
      L12: c.WeakTypeTag[L12],
      U12: c.WeakTypeTag[U12],
      L13: c.WeakTypeTag[L13],
      U13: c.WeakTypeTag[U13],
      L14: c.WeakTypeTag[L14],
      U14: c.WeakTypeTag[U14],
      L15: c.WeakTypeTag[L15],
      U15: c.WeakTypeTag[U15],
      L16: c.WeakTypeTag[L16],
      U16: c.WeakTypeTag[U16],
      L17: c.WeakTypeTag[L17],
      U17: c.WeakTypeTag[U17],
      L18: c.WeakTypeTag[L18],
      U18: c.WeakTypeTag[U18],
      L19: c.WeakTypeTag[L19],
      U19: c.WeakTypeTag[U19],
      L20: c.WeakTypeTag[L20],
      U20: c.WeakTypeTag[U20],
      L21: c.WeakTypeTag[L21],
      U21: c.WeakTypeTag[U21],
      HKTE: c.WeakTypeTag[HKT[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]
  ): c.Tree = try {
    assert(HKTE.tpe.typeParams.size == 21, "HKT must have exactly twenty-one type parameters")

    val HKT = HKTE.tpe.typeConstructor
    val A = freshTypeName("A") // 1
    val B = freshTypeName("B") // 2
    val C = freshTypeName("C") // 3
    val D = freshTypeName("D") // 4
    val E = freshTypeName("E") // 5
    val F = freshTypeName("F") // 6
    val G = freshTypeName("G") // 7
    val H = freshTypeName("H") // 8
    val I = freshTypeName("I") // 9
    val J = freshTypeName("J") // 10
    val K = freshTypeName("K") // 11
    val L = freshTypeName("L") // 12
    val M = freshTypeName("M") // 13
    val N = freshTypeName("N") // 14
    val O = freshTypeName("O") // 15
    val P = freshTypeName("P") // 16
    val Q = freshTypeName("Q") // 17
    val R = freshTypeName("R") // 18
    val S = freshTypeName("S") // 19
    val T = freshTypeName("T") // 20
    val U = freshTypeName("U") // 21
    val appliedHKT =
      tq"Type.Ctor21.Apply[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $L7, $U7, $L8, $U8, $L9, $U9, $L10, $U10, $L11, $U11, $L12, $U12, $L13, $U13, $L14, $U14, $L15, $U15, $L16, $U16, $L17, $U17, $L18, $U18, $L19, $U19, $L20, $U20, $L21, $U21, $HKT, $A, $B, $C, $D, $E, $F, $G, $H, $I, $J, $K, $L, $M, $N, $O, $P, $Q, $R, $S, $T, $U]"

    val ctx = freshName("ctx")
    val typeValue =
      TypeWithUnderlyingInjected(ctx, appliedHKT, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)
    val convertProvidedTypesForCrossQuotes = freshName("convertProvidedTypesForCrossQuotes")
    val termInner = freshName("Inner")
    val typeInner = TypeName(freshName("Inner").toString)

    val unchecked = q"""
    new Type.Ctor21.Bounded[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $L7, $U7, $L8, $U8, $L9, $U9, $L10, $U10, $L11, $U11, $L12, $U12, $L13, $U13, $L14, $U14, $L15, $U15, $L16, $U16, $L17, $U17, $L18, $U18, $L19, $U19, $L20, $U20, $L21, $U21, $HKT] {
      private val $ctx = CrossQuotes.ctx[_root_.scala.reflect.macros.blackbox.Context]
      private implicit def $convertProvidedTypesForCrossQuotes[$typeInner](implicit $termInner: Type[$typeInner]): $ctx.WeakTypeTag[$typeInner] =
        $termInner.asInstanceOf[$ctx.WeakTypeTag[$typeInner]]
      import $ctx.universe.{Type => _, internal => _, _}

      private val HKT = $ctx.weakTypeTag[Type.Ctor21.Stub[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $L7, $U7, $L8, $U8, $L9, $U9, $L10, $U10, $L11, $U11, $L12, $U12, $L13, $U13, $L14, $U14, $L15, $U15, $L16, $U16, $L17, $U17, $L18, $U18, $L19, $U19, $L20, $U20, $L21, $U21, $HKT]].tpe.typeArgs.last

      def apply[$A >: $L1 <: $U1: Type, $B >: $L2 <: $U2: Type, $C >: $L3 <: $U3: Type, $D >: $L4 <: $U4: Type, $E >: $L5 <: $U5: Type, $F >: $L6 <: $U6: Type, $G >: $L7 <: $U7: Type, $H >: $L8 <: $U8: Type, $I >: $L9 <: $U9: Type, $J >: $L10 <: $U10: Type, $K >: $L11 <: $U11: Type, $L >: $L12 <: $U12: Type, $M >: $L13 <: $U13: Type, $N >: $L14 <: $U14: Type, $O >: $L15 <: $U15: Type, $P >: $L16 <: $U16: Type, $Q >: $L17 <: $U17: Type, $R >: $L18 <: $U18: Type, $S >: $L19 <: $U19: Type, $T >: $L20 <: $U20: Type, $U >: $L21 <: $U21: Type]: Type[$appliedHKT] = 
        $typeValue

      def unapply[$A](A: Type[$A]): Option[($L1 <:??<: $U1, $L2 <:??<: $U2, $L3 <:??<: $U3, $L4 <:??<: $U4, $L5 <:??<: $U5, $L6 <:??<: $U6, $L7 <:??<: $U7, $L8 <:??<: $U8, $L9 <:??<: $U9, $L10 <:??<: $U10, $L11 <:??<: $U11, $L12 <:??<: $U12, $L13 <:??<: $U13, $L14 <:??<: $U14, $L15 <:??<: $U15, $L16 <:??<: $U16, $L17 <:??<: $U17, $L18 <:??<: $U18, $L19 <:??<: $U19, $L20 <:??<: $U20, $L21 <:??<: $U21)] = {
        val A0 = A.asInstanceOf[$ctx.WeakTypeTag[$A]].tpe
        A0.dealias.widen.baseType(HKT.typeSymbol) match {
          case TypeRef(_, _, List(tp1, tp2, tp3, tp4, tp5, tp6, tp7, tp8, tp9, tp10, tp11, tp12, tp13, tp14, tp15, tp16, tp17, tp18, tp19, tp20, tp21)) =>
            _root_.scala.Some(
              (
                $ctx.WeakTypeTag(tp1.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L1, $U1],
                $ctx.WeakTypeTag(tp2.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L2, $U2],
                $ctx.WeakTypeTag(tp3.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L3, $U3],
                $ctx.WeakTypeTag(tp4.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L4, $U4],
                $ctx.WeakTypeTag(tp5.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L5, $U5],
                $ctx.WeakTypeTag(tp6.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L6, $U6],
                $ctx.WeakTypeTag(tp7.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L7, $U7],
                $ctx.WeakTypeTag(tp8.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L8, $U8],
                $ctx.WeakTypeTag(tp9.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L9, $U9],
                $ctx.WeakTypeTag(tp10.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L10, $U10],
                $ctx.WeakTypeTag(tp11.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L11, $U11],
                $ctx.WeakTypeTag(tp12.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L12, $U12],
                $ctx.WeakTypeTag(tp13.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L13, $U13],
                $ctx.WeakTypeTag(tp14.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L14, $U14],
                $ctx.WeakTypeTag(tp15.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L15, $U15],
                $ctx.WeakTypeTag(tp16.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L16, $U16],
                $ctx.WeakTypeTag(tp17.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L17, $U17],
                $ctx.WeakTypeTag(tp18.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L18, $U18],
                $ctx.WeakTypeTag(tp19.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L19, $U19],
                $ctx.WeakTypeTag(tp20.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L20, $U20],
                $ctx.WeakTypeTag(tp21.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L21, $U21]
              )
            )
          case _ =>
            if (A0.typeConstructor == HKT && A0.typeArgs.size == 21) {
              val Seq(tp1, tp2, tp3, tp4, tp5, tp6, tp7, tp8, tp9, tp10, tp11, tp12, tp13, tp14, tp15, tp16, tp17, tp18, tp19, tp20, tp21) = A0.typeArgs
              _root_.scala.Some(
                (
                  $ctx.WeakTypeTag(tp1.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L1, $U1],
                  $ctx.WeakTypeTag(tp2.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L2, $U2],
                  $ctx.WeakTypeTag(tp3.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L3, $U3],
                  $ctx.WeakTypeTag(tp4.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L4, $U4],
                  $ctx.WeakTypeTag(tp5.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L5, $U5],
                  $ctx.WeakTypeTag(tp6.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L6, $U6],
                  $ctx.WeakTypeTag(tp7.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L7, $U7],
                  $ctx.WeakTypeTag(tp8.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L8, $U8],
                  $ctx.WeakTypeTag(tp9.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L9, $U9],
                  $ctx.WeakTypeTag(tp10.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L10, $U10],
                  $ctx.WeakTypeTag(tp11.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L11, $U11],
                  $ctx.WeakTypeTag(tp12.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L12, $U12],
                  $ctx.WeakTypeTag(tp13.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L13, $U13],
                  $ctx.WeakTypeTag(tp14.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L14, $U14],
                  $ctx.WeakTypeTag(tp15.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L15, $U15],
                  $ctx.WeakTypeTag(tp16.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L16, $U16],
                  $ctx.WeakTypeTag(tp17.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L17, $U17],
                  $ctx.WeakTypeTag(tp18.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L18, $U18],
                  $ctx.WeakTypeTag(tp19.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L19, $U19],
                  $ctx.WeakTypeTag(tp20.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L20, $U20],
                  $ctx.WeakTypeTag(tp21.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L21, $U21]
                )
              )
            }
            else None
        }
      }
    }
    """

    val result = suppressWarnings(c.typecheck(unchecked))

    log(
      s"""Cross-quotes ${paintExclDot(Console.BLUE)("Type.Ctor21.of")} expansion:
         |From: ${paintExclDot(Console.BLUE)(
          "Type.Ctor21.of"
        )}[${L1.tpe}, ${U1.tpe}, ${L2.tpe}, ${U2.tpe}, ${L3.tpe}, ${U3.tpe}, ${L4.tpe}, ${U4.tpe}, ${L5.tpe}, ${U5.tpe}, ${L6.tpe}, ${U6.tpe}, ${L7.tpe}, ${U7.tpe}, ${L8.tpe}, ${U8.tpe}, ${L9.tpe}, ${U9.tpe}, ${L10.tpe}, ${U10.tpe}, ${L11.tpe}, ${U11.tpe}, ${L12.tpe}, ${U12.tpe}, ${L13.tpe}, ${U13.tpe}, ${L14.tpe}, ${U14.tpe}, ${L15.tpe}, ${U15.tpe}, ${L16.tpe}, ${U16.tpe}, ${L17.tpe}, ${U17.tpe}, ${L18.tpe}, ${U18.tpe}, ${L19.tpe}, ${U19.tpe}, ${L20.tpe}, ${U20.tpe}, ${L21.tpe}, ${U21.tpe}, $HKT]
         |To: ${indent(showCodePretty(result, SyntaxHighlight.ANSI))}""".stripMargin
    )

    result
  } catch { case e: Throwable => reportIssue(e) }

  /* Replaces:
   *   Type.Ctor22.of[HKT]
   */
  // format: off
  def typeCtor22Impl[L1, U1 >: L1, L2, U2 >: L2, L3, U3 >: L3, L4, U4 >: L4, L5, U5 >: L5, L6, U6 >: L6, L7, U7 >: L7, L8, U8 >: L8, L9, U9 >: L9, L10, U10 >: L10, L11, U11 >: L11, L12, U12 >: L12, L13, U13 >: L13, L14, U14 >: L14, L15, U15 >: L15, L16, U16 >: L16, L17, U17 >: L17, L18, U18 >: L18, L19, U19 >: L19, L20, U20 >: L20, L21, U21 >: L21, L22, U22 >: L22, HKT[_ >: L1 <: U1, _ >: L2 <: U2, _ >: L3 <: U3, _ >: L4 <: U4, _ >: L5 <: U5, _ >: L6 <: U6, _ >: L7 <: U7, _ >: L8 <: U8, _ >: L9 <: U9, _ >: L10 <: U10, _ >: L11 <: U11, _ >: L12 <: U12, _ >: L13 <: U13, _ >: L14 <: U14, _ >: L15 <: U15, _ >: L16 <: U16, _ >: L17 <: U17, _ >: L18 <: U18, _ >: L19 <: U19, _ >: L20 <: U20, _ >: L21 <: U21, _ >: L22 <: U22]](implicit
  // format: on
      L1: c.WeakTypeTag[L1],
      U1: c.WeakTypeTag[U1],
      L2: c.WeakTypeTag[L2],
      U2: c.WeakTypeTag[U2],
      L3: c.WeakTypeTag[L3],
      U3: c.WeakTypeTag[U3],
      L4: c.WeakTypeTag[L4],
      U4: c.WeakTypeTag[U4],
      L5: c.WeakTypeTag[L5],
      U5: c.WeakTypeTag[U5],
      L6: c.WeakTypeTag[L6],
      U6: c.WeakTypeTag[U6],
      L7: c.WeakTypeTag[L7],
      U7: c.WeakTypeTag[U7],
      L8: c.WeakTypeTag[L8],
      U8: c.WeakTypeTag[U8],
      L9: c.WeakTypeTag[L9],
      U9: c.WeakTypeTag[U9],
      L10: c.WeakTypeTag[L10],
      U10: c.WeakTypeTag[U10],
      L11: c.WeakTypeTag[L11],
      U11: c.WeakTypeTag[U11],
      L12: c.WeakTypeTag[L12],
      U12: c.WeakTypeTag[U12],
      L13: c.WeakTypeTag[L13],
      U13: c.WeakTypeTag[U13],
      L14: c.WeakTypeTag[L14],
      U14: c.WeakTypeTag[U14],
      L15: c.WeakTypeTag[L15],
      U15: c.WeakTypeTag[U15],
      L16: c.WeakTypeTag[L16],
      U16: c.WeakTypeTag[U16],
      L17: c.WeakTypeTag[L17],
      U17: c.WeakTypeTag[U17],
      L18: c.WeakTypeTag[L18],
      U18: c.WeakTypeTag[U18],
      L19: c.WeakTypeTag[L19],
      U19: c.WeakTypeTag[U19],
      L20: c.WeakTypeTag[L20],
      U20: c.WeakTypeTag[U20],
      L21: c.WeakTypeTag[L21],
      U21: c.WeakTypeTag[U21],
      L22: c.WeakTypeTag[L22],
      U22: c.WeakTypeTag[U22],
      HKTE: c.WeakTypeTag[HKT[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]
  ): c.Tree = try {
    assert(HKTE.tpe.typeParams.size == 22, "HKT must have exactly twenty-two type parameters")

    val HKT = HKTE.tpe.typeConstructor
    val A = freshTypeName("A") // 1
    val B = freshTypeName("B") // 2
    val C = freshTypeName("C") // 3
    val D = freshTypeName("D") // 4
    val E = freshTypeName("E") // 5
    val F = freshTypeName("F") // 6
    val G = freshTypeName("G") // 7
    val H = freshTypeName("H") // 8
    val I = freshTypeName("I") // 9
    val J = freshTypeName("J") // 10
    val K = freshTypeName("K") // 11
    val L = freshTypeName("L") // 12
    val M = freshTypeName("M") // 13
    val N = freshTypeName("N") // 14
    val O = freshTypeName("O") // 15
    val P = freshTypeName("P") // 16
    val Q = freshTypeName("Q") // 17
    val R = freshTypeName("R") // 18
    val S = freshTypeName("S") // 19
    val T = freshTypeName("T") // 20
    val U = freshTypeName("U") // 21
    val V = freshTypeName("V") // 22
    val appliedHKT =
      tq"Type.Ctor22.Apply[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $L7, $U7, $L8, $U8, $L9, $U9, $L10, $U10, $L11, $U11, $L12, $U12, $L13, $U13, $L14, $U14, $L15, $U15, $L16, $U16, $L17, $U17, $L18, $U18, $L19, $U19, $L20, $U20, $L21, $U21, $L22, $U22, $HKT, $A, $B, $C, $D, $E, $F, $G, $H, $I, $J, $K, $L, $M, $N, $O, $P, $Q, $R, $S, $T, $U, $V]"

    val ctx = freshName("ctx")
    val typeValue =
      TypeWithUnderlyingInjected(ctx, appliedHKT, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)
    val convertProvidedTypesForCrossQuotes = freshName("convertProvidedTypesForCrossQuotes")
    val termInner = freshName("Inner")
    val typeInner = TypeName(freshName("Inner").toString)

    val unchecked = q"""
    new Type.Ctor22.Bounded[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $L7, $U7, $L8, $U8, $L9, $U9, $L10, $U10, $L11, $U11, $L12, $U12, $L13, $U13, $L14, $U14, $L15, $U15, $L16, $U16, $L17, $U17, $L18, $U18, $L19, $U19, $L20, $U20, $L21, $U21, $L22, $U22, $HKT] {
      private val $ctx = CrossQuotes.ctx[_root_.scala.reflect.macros.blackbox.Context]
      private implicit def $convertProvidedTypesForCrossQuotes[$typeInner](implicit $termInner: Type[$typeInner]): $ctx.WeakTypeTag[$typeInner] =
        $termInner.asInstanceOf[$ctx.WeakTypeTag[$typeInner]]
      import $ctx.universe.{Type => _, internal => _, _}

      private val HKT = $ctx.weakTypeTag[Type.Ctor22.Stub[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $L7, $U7, $L8, $U8, $L9, $U9, $L10, $U10, $L11, $U11, $L12, $U12, $L13, $U13, $L14, $U14, $L15, $U15, $L16, $U16, $L17, $U17, $L18, $U18, $L19, $U19, $L20, $U20, $L21, $U21, $L22, $U22, $HKT]].tpe.typeArgs.last

      def apply[$A >: $L1 <: $U1: Type, $B >: $L2 <: $U2: Type, $C >: $L3 <: $U3: Type, $D >: $L4 <: $U4: Type, $E >: $L5 <: $U5: Type, $F >: $L6 <: $U6: Type, $G >: $L7 <: $U7: Type, $H >: $L8 <: $U8: Type, $I >: $L9 <: $U9: Type, $J >: $L10 <: $U10: Type, $K >: $L11 <: $U11: Type, $L >: $L12 <: $U12: Type, $M >: $L13 <: $U13: Type, $N >: $L14 <: $U14: Type, $O >: $L15 <: $U15: Type, $P >: $L16 <: $U16: Type, $Q >: $L17 <: $U17: Type, $R >: $L18 <: $U18: Type, $S >: $L19 <: $U19: Type, $T >: $L20 <: $U20: Type, $U >: $L21 <: $U21: Type, $V >: $L22 <: $U22: Type]: Type[$appliedHKT] = 
        $typeValue

      def unapply[$A](A: Type[$A]): Option[($L1 <:??<: $U1, $L2 <:??<: $U2, $L3 <:??<: $U3, $L4 <:??<: $U4, $L5 <:??<: $U5, $L6 <:??<: $U6, $L7 <:??<: $U7, $L8 <:??<: $U8, $L9 <:??<: $U9, $L10 <:??<: $U10, $L11 <:??<: $U11, $L12 <:??<: $U12, $L13 <:??<: $U13, $L14 <:??<: $U14, $L15 <:??<: $U15, $L16 <:??<: $U16, $L17 <:??<: $U17, $L18 <:??<: $U18, $L19 <:??<: $U19, $L20 <:??<: $U20, $L21 <:??<: $U21, $L22 <:??<: $U22)] = {
        val A0 = A.asInstanceOf[$ctx.WeakTypeTag[$A]].tpe
        A0.dealias.widen.baseType(HKT.typeSymbol) match {
          case TypeRef(_, _, List(tp1, tp2, tp3, tp4, tp5, tp6, tp7, tp8, tp9, tp10, tp11, tp12, tp13, tp14, tp15, tp16, tp17, tp18, tp19, tp20, tp21, tp22)) =>
            _root_.scala.Some(
              (
                $ctx.WeakTypeTag(tp1.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L1, $U1],
                $ctx.WeakTypeTag(tp2.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L2, $U2],
                $ctx.WeakTypeTag(tp3.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L3, $U3],
                $ctx.WeakTypeTag(tp4.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L4, $U4],
                $ctx.WeakTypeTag(tp5.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L5, $U5],
                $ctx.WeakTypeTag(tp6.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L6, $U6],
                $ctx.WeakTypeTag(tp7.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L7, $U7],
                $ctx.WeakTypeTag(tp8.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L8, $U8],
                $ctx.WeakTypeTag(tp9.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L9, $U9],
                $ctx.WeakTypeTag(tp10.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L10, $U10],
                $ctx.WeakTypeTag(tp11.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L11, $U11],
                $ctx.WeakTypeTag(tp12.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L12, $U12],
                $ctx.WeakTypeTag(tp13.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L13, $U13],
                $ctx.WeakTypeTag(tp14.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L14, $U14],
                $ctx.WeakTypeTag(tp15.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L15, $U15],
                $ctx.WeakTypeTag(tp16.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L16, $U16],
                $ctx.WeakTypeTag(tp17.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L17, $U17],
                $ctx.WeakTypeTag(tp18.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L18, $U18],
                $ctx.WeakTypeTag(tp19.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L19, $U19],
                $ctx.WeakTypeTag(tp20.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L20, $U20],
                $ctx.WeakTypeTag(tp21.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L21, $U21],
                $ctx.WeakTypeTag(tp22.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L22, $U22]
              )
            )
          case _ =>
            if (A0.typeConstructor == HKT && A0.typeArgs.size == 22) {
              val Seq(tp1, tp2, tp3, tp4, tp5, tp6, tp7, tp8, tp9, tp10, tp11, tp12, tp13, tp14, tp15, tp16, tp17, tp18, tp19, tp20, tp21, tp22) = A0.typeArgs
              _root_.scala.Some(
                (
                  $ctx.WeakTypeTag(tp1.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L1, $U1],
                  $ctx.WeakTypeTag(tp2.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L2, $U2],
                  $ctx.WeakTypeTag(tp3.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L3, $U3],
                  $ctx.WeakTypeTag(tp4.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L4, $U4],
                  $ctx.WeakTypeTag(tp5.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L5, $U5],
                  $ctx.WeakTypeTag(tp6.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L6, $U6],
                  $ctx.WeakTypeTag(tp7.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L7, $U7],
                  $ctx.WeakTypeTag(tp8.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L8, $U8],
                  $ctx.WeakTypeTag(tp9.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L9, $U9],
                  $ctx.WeakTypeTag(tp10.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L10, $U10],
                  $ctx.WeakTypeTag(tp11.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L11, $U11],
                  $ctx.WeakTypeTag(tp12.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L12, $U12],
                  $ctx.WeakTypeTag(tp13.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L13, $U13],
                  $ctx.WeakTypeTag(tp14.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L14, $U14],
                  $ctx.WeakTypeTag(tp15.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L15, $U15],
                  $ctx.WeakTypeTag(tp16.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L16, $U16],
                  $ctx.WeakTypeTag(tp17.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L17, $U17],
                  $ctx.WeakTypeTag(tp18.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L18, $U18],
                  $ctx.WeakTypeTag(tp19.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L19, $U19],
                  $ctx.WeakTypeTag(tp20.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L20, $U20],
                  $ctx.WeakTypeTag(tp21.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L21, $U21],
                  $ctx.WeakTypeTag(tp22.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L22, $U22]
                )
              )
            }
            else None
        }
      }
    }
    """

    val result = suppressWarnings(c.typecheck(unchecked))

    log(
      s"""Cross-quotes ${paintExclDot(Console.BLUE)("Type.Ctor22.of")} expansion:
         |From: ${paintExclDot(Console.BLUE)(
          "Type.Ctor22.of"
        )}[${L1.tpe}, ${U1.tpe}, ${L2.tpe}, ${U2.tpe}, ${L3.tpe}, ${U3.tpe}, ${L4.tpe}, ${U4.tpe}, ${L5.tpe}, ${U5.tpe}, ${L6.tpe}, ${U6.tpe}, ${L7.tpe}, ${U7.tpe}, ${L8.tpe}, ${U8.tpe}, ${L9.tpe}, ${U9.tpe}, ${L10.tpe}, ${U10.tpe}, ${L11.tpe}, ${U11.tpe}, ${L12.tpe}, ${U12.tpe}, ${L13.tpe}, ${U13.tpe}, ${L14.tpe}, ${U14.tpe}, ${L15.tpe}, ${U15.tpe}, ${L16.tpe}, ${U16.tpe}, ${L17.tpe}, ${U17.tpe}, ${L18.tpe}, ${U18.tpe}, ${L19.tpe}, ${U19.tpe}, ${L20.tpe}, ${U20.tpe}, ${L21.tpe}, ${U21.tpe}, ${L22.tpe}, ${U22.tpe}, $HKT]
         |To: ${indent(showCodePretty(result, SyntaxHighlight.ANSI))}""".stripMargin
    )

    result
  } catch { case e: Throwable => reportIssue(e) }

  /* Replaces:
   *   Expr.quote[A](a)
   * with:
   *   what we see in Quasiquote
   */
  def quoteImpl[A: c.WeakTypeTag](expr: c.Expr[A]): c.Tree = try {
    val ctx = freshName("ctx")
    val convertProvidedTypesForCrossQuotes = freshName("convertProvidedTypesForCrossQuotes")
    val termInner = freshName("Inner")
    val typeInner = TypeName(freshName("Inner").toString)
    val wtt = freshName("wtt")

    val A = weakTypeOf[A]
    val typeA =
      try
        c.inferImplicitValue(
          pt = c.typecheck(tq"Type[$A]", mode = c.TYPEmode).tpe,
          silent = false,
          withMacrosDisabled = true
        )
      catch {
        // If there is no such implicit, we have to create it.
        case e: Throwable if e.getClass.getName == "scala.reflect.macros.TypecheckException" =>
          if (loggingEnabled) {
            println(s"""No implicit Type[$A] found in the scope, creating one with Type.of[$A]""")
          }
          val resolved = typeOfImpl[A]
          if (loggingEnabled) {
            println(s"""Created Type[$A] with: ${showCodePretty(resolved, SyntaxHighlight.ANSI)}""")
          }
          resolved
      }
    val quasiquote = convert(ctx)(expr.tree)

    val unchecked = q"""
    val $wtt = $typeA
    val $ctx = CrossQuotes.ctx[_root_.scala.reflect.macros.blackbox.Context]
    import $ctx.universe.Quasiquote
    @_root_.scala.annotation.nowarn
    implicit def $convertProvidedTypesForCrossQuotes[$typeInner](implicit $termInner: Type[$typeInner]): $ctx.WeakTypeTag[$typeInner] =
      $termInner.asInstanceOf[$ctx.WeakTypeTag[$typeInner]]
    _root_.hearth.fp.ignore($convertProvidedTypesForCrossQuotes[Any](_: Type[Any]))
    $ctx.Expr[$A]($quasiquote)($wtt.asInstanceOf[$ctx.WeakTypeTag[$A]]).asInstanceOf[Expr[$A]]
    """

    // purposefuly not typechecking, because it would fail with: unexpected error: Position.point on NoPosition
    val result = suppressWarnings(unchecked)

    log(
      s"""Cross-quotes ${paintExclDot(Console.BLUE)("Expr.quote")} expansion:
         |From: ${paintExclDot(Console.BLUE)("Expr.quote")}[${weakTypeOf[A]}]($expr)
         |To: ${showCodePretty(result, SyntaxHighlight.ANSI)}""".stripMargin
    )

    result
  } catch { case e: Throwable => reportIssue(e) }

  // Type utilities

  /* If implicit was imported from tpe.Underlying or defined as implicit val/def/given,
   * it will be ignored by the c.weakTypeTag[Tpe] - so tpe.Underlying that appears inside Tpe will be resolved
   * to WeakTypeTag of the local variable, not of the type represented by tpe.Underlying.
   * 
   * Only WeakTypeTags corresponding to type parameters' of encloding def are considered, which is why we need to rewrite the tree into:

   * {{{
   * def $workaround[A1, B, ...](implicit A1: $ctx.WeakTypeTag[A1], B: $ctx.WeakTypeTag[B], ...) = $ctx.weakTypeTag[...] // use A1, B2, ...
   * $workaround[A, B, ...](A.asInstanceOf[$ctx.WeakTypeTag[A]], B.asInstanceOf[$ctx.WeakTypeTag[B]], ...)
   * }}}
   * 
   * and only then WeakTypeTag could be correctly resolved.
   */
  private def TypeWithUnderlyingInjected(ctx: TermName, weakTypeTagOf: c.Tree, excluding: TypeName*): c.Tree = {
    lazy val noInjectResult = q"""$ctx.weakTypeTag[$weakTypeTagOf].asInstanceOf[Type[$weakTypeTagOf]]"""
    lazy val ctx2 = freshName("ctx2")

    val candidates: List[(Type, String)] = importedUnderlyingTypes
    val excludingSet: Set[String] = excluding.view.map(_.decodedName.toString).toSet
    val importedUnderlying = candidates.view
      .filterNot { case (_, renamed) => excludingSet(renamed) }
      .flatMap { case (tpeToReplace, renamed) =>
        try {
          val typeValue: Tree =
            c.inferImplicitValue(c.typecheck(tq"Type[$tpeToReplace]", mode = c.TYPEmode).tpe, silent = false)
          val weakTypeTagValue = q"""$typeValue.asInstanceOf[$ctx2.WeakTypeTag[$tpeToReplace]]"""

          val paramName = freshTypeName(renamed)
          val paramDef: TypeDef = q"type $paramName"
          val paramVal: ValDef = q"val ${freshName(renamed)}: $ctx2.WeakTypeTag[$paramName]"

          Iterable((paramDef, paramVal, tpeToReplace, weakTypeTagValue))
        } catch {
          // If there is no such implicit, we aren't creating a workaround for it.
          case e: Throwable if e.getClass.getName == "scala.reflect.macros.TypecheckException" => Iterable.empty
          // If there are other errors, we should probably report them.
        }
      }
      .toSeq

    if (importedUnderlying.isEmpty) {
      noInjectResult
    } else {
      var skipWorkaround = true

      val workaround: TermName = freshName("workaround")
      val paramDefs: Seq[TypeDef] = importedUnderlying.map(_._1)
      val paramVals: Seq[ValDef] = importedUnderlying.map(_._2)
      val tpesToReplace: Seq[Type] = importedUnderlying.map(_._3)
      val weakTypeTagValues: Seq[Tree] = importedUnderlying.map(_._4)

      // If macros were sane, this would be almost enough.
      //
      // We would just need to apply the fix to `weakTypeTagOf` in `$ctx2.weakTypeTag[$weakTypeTagOf].asInstanceOf[Type[$weakTypeTagOf]]`.
      //
      // So, why I put `null` instead of `$ctx2.weakTypeTag[$weakTypeTagOf]`? As a matter of the fact, why I created new $ctx2?
      val uncheckedResultPrototype = q"""
      val $ctx2 = CrossQuotes.ctx[_root_.scala.reflect.macros.blackbox.Context]
      import $ctx2.universe.{Type => _, internal => _, _}
      def $workaround[..$paramDefs](implicit ..$paramVals): Type[$weakTypeTagOf] = null.asInstanceOf[Type[$weakTypeTagOf]]
      $workaround[..$tpesToReplace](..$weakTypeTagValues)
      """

      // To fix the `weakTypeTagOf` tree, we have to have c.Type or Symbol or each type parameter, so that we could replace params with them.
      //
      // And this requires typechecking.
      //
      // So, we have to create a stub, that typechecks (ctx would not, since it's defined outside of that snippet), that we will modify later on.
      def fixWorkaroundBody(patchBody: DefDef => c.Tree): c.Tree = {
        val Block(List(ctxVal, ctxImport, unpatchedDef: DefDef), callDef) = c.typecheck(uncheckedResultPrototype)
        val patchedDef = treeCopy.DefDef(
          unpatchedDef,
          unpatchedDef.mods,
          unpatchedDef.name,
          unpatchedDef.tparams,
          unpatchedDef.vparamss,
          unpatchedDef.tpt,
          patchBody(unpatchedDef)
        )
        if (skipWorkaround) noInjectResult
        // We also have to c.untypecheck the result, because otherwise we're getting errors from mixing typed and untyped trees.
        // Literally, NullPoinerExceptions, from Symbols on typechecking:
        //   Cannot invoke "scala.reflect.internal.Types$Type.typeSymbol()" because "tp" is null
        // So we have to untypecheck the result, to avoid this problem.
        else c.untypecheck(Block(List(ctxVal, ctxImport, patchedDef), callDef))
      }

      fixWorkaroundBody { unpatchedDef =>
        // Replaces the Tree with the Type representation that could miss some value.Underlying,
        // with Tree using Types from type parameters - because they would get picked up. SMH.
        object typePatcher extends Transformer {

          // To obtain these we had to typecheck the Tree above, because we need them to be able to recursively replace
          // e.g. somevalue.Underlying with SomeValueTypeParameter (substituteTypes and substituteSymbols seem to not work
          // and only replacing the top level Type in TypeTree is not enough).
          private val paramTpes = unpatchedDef.symbol.asMethod.typeParams.map(_.asType.toType)
          def patchTpe(tpe: Type): Type = tpe.map { oldType =>
            val index = tpesToReplace.indexWhere(oldType =:= _)
            if (index < 0) oldType
            else {
              skipWorkaround = false
              paramTpes(index)
            }
          }

          // We need to reify the type as a Tree, because otherwise it won't survive the c.untypecheck(result)
          // (and we need to untypecheck the result, because otherwise we're getting errors from mixing typed and untyped trees).
          //
          // If we skipped this... then e.g.:
          //   weakTypeTag[Option[B1]] // B1 is type paremeter
          // on c.untypecheck becomes
          //   weakTypeTag
          // which on later (re)typechecking becomes
          //   weakTypeTag[B1] // ??
          // which is not what we want. We need to preserve the type during untypechecking... which requires reifying it as a Tree.
          private def pkgRef(sym: Symbol): Tree = {
            // sym is a package *class* symbol
            val parts = sym.fullName.split('.')
            parts.foldLeft[Tree](Ident(termNames.ROOTPKG)) { case (acc, part) =>
              Select(acc, TermName(part))
            }
          }
          def typeToTree(t: Type): Tree = t.dealias match {
            case TypeRef(pre, sym, args) =>
              val base: Tree = pre match {
                // unqualified simple type (e.g. List, MyType)
                case NoPrefix => Ident(sym.name.toTypeName)
                // *** THIS IS THE IMPORTANT BIT ***
                // Option[B1] often has pre = ThisType(scala.package)
                case ThisType(pkg) if pkg.isPackageClass => Select(pkgRef(pkg), sym.name.toTypeName)
                // other prefixes (path-dependent types etc.)
                case _ => Select(typeToTree(pre), sym.name.toTypeName)
              }
              if (args.isEmpty) base
              else AppliedTypeTree(base, args.map(typeToTree))
            case SingleType(pre, sym) =>
              pre match {
                case ThisType(pkg) if pkg.isPackageClass => Select(pkgRef(pkg), sym.name.toTermName)
                case _                                   => Select(typeToTree(pre), sym.name.toTermName)
              }
            case ThisType(sym) if sym.isPackageClass => pkgRef(sym)
            case ThisType(sym)                       => This(sym.name.toTypeName)
            case ConstantType(c)                     => Literal(c)
            case AnnotatedType(_, underlying)        => typeToTree(underlying)
            case ExistentialType(_, underlying)      => typeToTree(underlying)
            case other                               => TypeTree(other) // fallback
          }

          override def transform(tree: Tree): Tree = tree match {
            case tpeTree: TypeTree if tpeTree.tpe != null =>
              val patchedType = patchTpe(tpeTree.tpe)
              if (patchedType =:= tpeTree.tpe) tpeTree
              else typeToTree(patchedType)
            case _ => super.transform(tree)
          }
        }

        q"""$ctx2.weakTypeTag[${typePatcher.transform(weakTypeTagOf)}].asInstanceOf[Type[$weakTypeTagOf]]"""
      }
    }
  }

  /** Collects names imported from *.Underlying patterns in the macro expansion's enclosings.
    *
    * Looks for imports like:
    *   - import someName.Underlying
    *   - import someName.{Underlying => SomeName}
    *   - import a.{Underlying => A}, b.{Underlying => B}
    *
    * Traverses the enclosing method/class trees to find import statements that come before the macro expansion point.
    */
  private def importedUnderlyingTypes: List[(Type, String)] = try {
    val importedNames = scala.collection.mutable.ListBuffer[(Type, String)]()
    val expansionPos = c.enclosingPosition

    def isBeforeExpansionPoint(pos: Position): Boolean =
      pos != NoPosition &&
        (pos.line < expansionPos.line ||
          (pos.line == expansionPos.line && pos.column < expansionPos.column))

    // Helper to extract names from import selectors that match *.Underlying pattern
    def extractNamesFromImport(importTree: Import): Unit =
      // Check if import comes before macro expansion
      if (isBeforeExpansionPoint(importTree.pos)) {
        // Check if the import expression is something.Underlying
        val found = importTree.selectors
          .collect {
            case ImportSelector(name, _, rename, _) if name.isImportedCrossTypeImplicit =>
              val toReplace = scala.util.Try {
                c.typecheck(tq"${importTree.expr}.${name.toTypeName}", mode = c.TYPEmode).tpe
              }.toOption
              toReplace -> Option(rename).getOrElse(name).decodedName.toString
          }
          .collect { case (Some(toReplace), name) =>
            (toReplace, name)
          }

        importedNames ++= found
      }

    // Traverse tree to find Import statements
    object finder extends Traverser {
      override def traverse(tree: Tree): Unit = tree match {
        case importTree: Import =>
          extractNamesFromImport(importTree)
          super.traverse(tree)
        case _ =>
          super.traverse(tree)
      }
    }

    // Check enclosing method
    @scala.annotation.nowarn // No replacement and it's not going anywhere
    val enclosingMethod = c.enclosingMethod
    if (enclosingMethod != EmptyTree) {
      finder.traverse(enclosingMethod)
    }

    // Check enclosing class
    @scala.annotation.nowarn // No replacement and it's not going anywhere
    val enclosingClass = c.enclosingClass
    if (enclosingClass != EmptyTree) {
      finder.traverse(enclosingClass)
    }

    // We do NOT deduplacate here, because we want to keep the order of imports,
    // AND we may want to check e.g. if there are actual implicits for these,
    // before we deduplicate them.
    importedNames.toList
  } catch { case _: Throwable => Nil }

  // Expr utilities

  private def convert(ctx: TermName)(tree: c.Tree): c.Tree = {
    // 1. Replace certain trees with stubs,
    // 2. then print the tree,
    // 3. put it into quasiquotes and escape `$`
    // 4. replace stubs with their actual replacements
    // 5. and parse it back into Tree
    // It is _so much easier_ than properly modifying the tree.
    val spliceReplacer = new SpliceReplacer(ctx)
    val implicitTypeReferenceReplacer = new ImplicitTypeReferenceReplacer(ctx)
    val putIntoQuasiquote: String => String = str =>
      "q\"\"\"" + str.view.map {
        // Escape $ - we have to do it before replacing stubs, because we might have accidentaly removed some actual string interpolation
        case '$' => "$$"
        case c   => c
      }.mkString + "\"\"\""
    tree
      // Replace Expr.splice and Types that have implicit Type[T] values with stubs:
      // - there will be no nested Expr.quotes - if there is one, the nested one would expand a macro, so splice would contain only the result
      // - the content of Expr.splice would NOT require adjustment, because it would appear inside ${ ... } where tree rewriting is undesired
      // - that's why we start with replacing Expr.splice stubs first, and ONLY THEN we replace type parameter stubs
      .tap { source =>
        if (loggingEnabled) {
          println("Checkpoint 0")
        }
      }
      .pipe(spliceReplacer.transform)
      .tap { source =>
        if (loggingEnabled) {
          println("Checkpoint 1")
        }
      }
      .pipe(implicitTypeReferenceReplacer.transform)
      // Convert to String - we are assuming that showCodePretty produces correct Scala code (that cannot be said of toString or showCode)
      .tap { source =>
        if (loggingEnabled) {
          println("Checkpoint 2")
          println(
            s"""Stubbed source:
               |${indent(showCodePretty(source, SyntaxHighlight.ANSI))}
               |""".stripMargin
          )
        }
      }
      .pipe(showCodePretty(_, SyntaxHighlight.plain))
      // Put into quasiquote - we have to do it before replacing stubs, because we might have accidentaly removed some actual string interpolation
      .pipe(putIntoQuasiquote)
      // Replace stubs with their values:
      // - Expr.splice replacement should NOT BE EDITED, which is why we replace type stubs first
      // - only then we are replacing Expr.splice stubs
      .pipe(implicitTypeReferenceReplacer.replaceStubsInSource)
      .pipe(spliceReplacer.replaceStubsInSource)
      // Nested Expr.splice generate `final val rassoc$1 = ...; /* use rassoc$1 */` but final val is not valid in Block so we are removing it..
      // We should probably fix this in the better-printers, but for now it's OK.
      .pipe(_.replaceAll("final val rassoc", "val rassoc"))
      // Validate that there are no stubs left
      .pipe(implicitTypeReferenceReplacer.validateNoStubsLeft)
      .pipe(spliceReplacer.validateNoStubsLeft)
      // Parse back to Tree
      .tap { source =>
        if (loggingEnabled) {
          println(
            s"""Parsing source:
               |${indent(paint(Console.BLUE)(source))}
               |""".stripMargin
          )
        }
      }
      .pipe(c.parse(_))
      .tap { tree =>
        if (loggingEnabled) {
          println(s"""Parsed source:
                     |${indent(showCodePretty(tree, SyntaxHighlight.ANSI))}
                     |""".stripMargin)
        }
      }
  }

  /** Handles type parameters and imported value.Underlying inside Expr.quote: Expr.quote { def foo[A] = ... // use A },
    * as we as type parameters and imported value.Underlying inside Expr.splice: that require Type[A] from the outside.
    */
  private class ImplicitTypeReferenceReplacer(ctx: TermName) extends Transformer {

    def newTypeRef(pre: Type, sym: Symbol, args: List[Type]): Type = c.internal.typeRef(pre, sym, args)

    case class Cache(stub: TypeName, symbol: Symbol, weakTypeTag: Tree) {

      def ident: Ident = Ident(stub)
      def typeRef: Type = c.internal.typeRef(NoPrefix, symbol, List())
    }
    object Cache {
      def forTypeName(name: String): Option[Cache] = try {
        val parsed = c.typecheck(c.parse(s"Type[$name]"))
        val stub = freshTypeName("TypeStub")
        @scala.annotation.nowarn
        val symbol = {
          val originalSymbol = parsed.tpe match {
            case TypeRef(_, _, List(TypeRef(_, nameSymbol, List()))) => nameSymbol
          }
          c.universe.internal.newTypeSymbol(
            owner = originalSymbol.owner,
            name = stub,
            pos = originalSymbol.pos,
            flags = c.internal.flags(originalSymbol)
          )
        }
        val ttag = c.parse(s"$parsed.asInstanceOf[$ctx.WeakTypeTag[$name]]")
        _root_.scala.Some(Cache(stub, symbol, ttag))
      } catch {
        // case e: scala.tools.reflect.ToolBoxError =>
        //   println(s"Error: $e")
        //   None
        case _: Exception => None
      }
    }

    object AbstractTypes {
      private val caches = scala.collection.mutable.Map.empty[String, Option[Cache]]

      def find(name: String): Option[Cache] = caches.getOrElseUpdate(name, Cache.forTypeName(name))
      def find(name: Name): Option[Cache] = find(name.toString)
      def find(symbol: Symbol): Option[Cache] = find(symbol.name.toString)

      def toReplace = caches.view.collect { case (_, Some(Cache(stub, _, weakTypeTag))) =>
        val printedWeakTypeTag = showCodePretty(weakTypeTag, SyntaxHighlight.plain)
        Pattern.quote(stub.toString) -> Matcher.quoteReplacement(s"$${ $printedWeakTypeTag }")
      }.toMap
      def toCheck = caches.view.collect { case (name, Some(Cache(stub, _, _))) =>
        name -> stub.toString
      }.toMap
    }

    object ImportedTypes {
      private val cachesAliases = scala.collection.mutable.Map.empty[String, Cache]
      private val caches = importedUnderlyingTypes.flatMap { case (tpe, name) =>
        def byName = Cache.forTypeName(name)
        def byType = try
          Cache.forTypeName(showCodePretty(tpe, SyntaxHighlight.plain))
        catch {
          case _: Throwable => None
        }
        byType.orElse(byName).map { cache =>
          cachesAliases += (name -> cache)
          cachesAliases += (s"$name.type" -> cache)
          cachesAliases += (showCodePretty(tpe, SyntaxHighlight.plain) -> cache)
          tpe -> cache
        }
      }.toMap

      // It's riddiculous but:
      //  - we have both: someValue.Underlying (type Underlying = ...)
      //  - and: someValue.Underlying.type (val Underlying = ...)
      // and we have to address that.
      private def isImportedType(imported: Type, tpe: Type): Boolean =
        imported =:= tpe || s"$imported.type" == tpe.toString

      def find(tpe: Type): Option[Cache] = caches.collectFirst {
        case (imported, cache) if isImportedType(imported, tpe) => cache
      }
      def find(name: String): Option[Cache] = cachesAliases.get(name)

      def toReplace = cachesAliases.view.collect { case (_, Cache(stub, _, weakTypeTag)) =>
        val printedWeakTypeTag = showCodePretty(weakTypeTag, SyntaxHighlight.plain)
        Pattern.quote(stub.toString) -> Matcher.quoteReplacement(s"$${ $printedWeakTypeTag }")
      }.toMap
      def toCheck = caches.view.collect { case (name, Cache(stub, _, _)) =>
        name -> stub.toString
      }.toMap
    }

    override def transform(tree: Tree): Tree = tree match {
      // Transform references like someValue.Underlying to Ident(ImportStub).
      case s: Select =>
        // While it looks perfectly reasonable to test `ImportedTypes.find(s.tpe)` as well,
        // if we have e.g. an expression of type `(key.Underlying, value.Underlying)` (key being scala.Int) we'd get e.g. `scala.Int`
        // instead of the `key.Underlying` expression value.
        ImportedTypes
          .find(showCodePretty(s, SyntaxHighlight.plain))
          .map(_.ident)
          .map { stub =>
            if (loggingEnabled) {
              println(s"""Replaced Select with stub:
                         |In:  ${showCodePretty(tree, SyntaxHighlight.ANSI)}
                         |     ${showRawPretty(tree, SyntaxHighlight.ANSI)}
                         |Out: ${showCodePretty(stub, SyntaxHighlight.ANSI)}
                         |     ${showRawPretty(stub, SyntaxHighlight.ANSI)}
                         |""".stripMargin)
            }
            stub
          }
          .getOrElse(super.transform(s))
      // Transform references like TypeParameter to Ident(TypeStub).
      case Ident(name) =>
        AbstractTypes
          .find(name)
          .map { cache =>
            val stub = cache.ident
            if (loggingEnabled) {
              println(s"""Replaced Ident with stub:
                         |In:  ${showCodePretty(tree, SyntaxHighlight.ANSI)}
                         |     ${showRawPretty(tree, SyntaxHighlight.ANSI)}
                         |Out: ${showCodePretty(stub, SyntaxHighlight.ANSI)}
                         |     ${showRawPretty(stub, SyntaxHighlight.ANSI)}
                         |""".stripMargin)
            }
            stub
          }
          .getOrElse(Ident(name))
      // Transform references like EITHER TypeParameter OR someType.Underlying to Ident(TypeStub).
      case tt: TypeTree =>
        try
          if (tt.original != null) {
            val updated = transform(tt.original)
            if (updated == tt.original) tt.original
            else {
              if (loggingEnabled) {
                println(s"""Stubbed TypeTree original:
                           |In:  ${showCodePretty(tree, SyntaxHighlight.ANSI)}
                           |     ${showRawPretty(tree, SyntaxHighlight.ANSI)}
                           |Out: ${showCodePretty(updated, SyntaxHighlight.ANSI)}
                           |     ${showRawPretty(updated, SyntaxHighlight.ANSI)}
                           |""".stripMargin)
              }
              updated
            }
          } else {
            val updated = transformType(tt.tpe)
            if (updated == tt.tpe) tt
            else {
              val newTypeTree = TypeTree(updated)
              if (loggingEnabled) {
                println(s"""Stubbed TypeTree:
                           |In:  ${showCodePretty(tree, SyntaxHighlight.ANSI)}
                           |     ${showRawPretty(tree, SyntaxHighlight.ANSI)}
                           |Out: ${showCodePretty(newTypeTree, SyntaxHighlight.ANSI)}
                           |     ${showRawPretty(newTypeTree, SyntaxHighlight.ANSI)}
                           |""".stripMargin)
              }
              newTypeTree
            }
          }
        catch {
          // We ignore CyclicReference errors, because they are expected and harmless.
          // E.g. referemce to $anon class that will be erased on printing.
          case e: Throwable if e.getClass.getName == "scala.reflect.internal.Symbols$CyclicReference" => tt
        }
      case tree => super.transform(tree)
    }

    private def transformType(tpe: Type): Type = {
      // Makes sure to use exactly the same cache key for .Underlying, even if we have some aliases etc
      val importedType = ImportedTypes.find(tpe).map(_.typeRef).getOrElse(tpe)
      tpe match {
        case _ if importedType ne tpe       => importedType
        case TypeRef(NoPrefix, sym, List()) =>
          AbstractTypes.find(sym).map(_.typeRef).getOrElse(tpe)
        case TypeRef(pre, sym, args) =>
          newTypeRef(pre, sym, args.map(transformType))
        // TODO: maybe also transform other types
        case _ => tpe
      }
    }

    def replaceStubsInSource(source: String): String =
      (AbstractTypes.toReplace.view ++ ImportedTypes.toReplace.view).foldLeft(source) {
        case (result, (pattern, replacement)) => result.replaceAll(pattern, replacement)
      }

    def validateNoStubsLeft(source: String): String = {
      val remainingStubs = (AbstractTypes.toCheck.view ++ ImportedTypes.toCheck.view).collect {
        case (name, stub) if source.contains(stub) => name
      }
      assert(
        remainingStubs.isEmpty,
        s"Type parameter/imported type stubs left: ${remainingStubs.mkString(", ")} in:\n$source"
      )
      source
    }
  }

  /** Handles type parameters inside Expr.splice: Expr.splice[A](a)
    */
  private class SpliceReplacer(ctx: TermName) extends Transformer {

    object Splices {
      private val caches = scala.collection.mutable.Map.empty[String, String]

      def store(expr: Tree, tpe: Tree): Tree = {
        // This code can contain $ which has to be escaped before printing, also we should use better printers for all such things.
        val printedExpr = showCodePretty(expr, SyntaxHighlight.plain)
        val printedTpe = showCodePretty(tpe, SyntaxHighlight.plain)

        val stub = Ident(freshName("expressionStub"))
        caches += (stub.toString() -> s"$${ {$printedExpr}.asInstanceOf[$ctx.Expr[$printedTpe]] }")
        stub
      }

      def toReplace = caches.view.collect { case (stub, expr) =>
        Pattern.quote(stub.toString) -> Matcher.quoteReplacement(expr)
      }.toMap
      def toCheck = caches.keys.toSet
    }

    override def transform(tree: Tree): Tree = tree match {
      /* Replaces:
       *   Expr.splice[A](a)
       * with:
       *   ${{a}.asInstanceOf[ctx.Expr[A]]}
       */
      case Apply(
            TypeApply(
              Select(Select(This(_) | Ident(_), TermName("Expr")), TermName("splice")),
              List(tpe)
            ),
            List(expr)
          ) =>
        val result = Splices.store(expr, tpe)

        log(
          s"""Cross-quotes ${paintExclDot(Console.BLUE)("Expr.splice")} expansion:
             |From: ${paintExclDot(Console.BLUE)("Expr.splice")}(${showCodePretty(expr, SyntaxHighlight.ANSI)})
             |To: ${indent(showCodePretty(result, SyntaxHighlight.ANSI))}""".stripMargin
        )

        result

      case tree => super.transform(tree)
    }

    def replaceStubsInSource(source: String): String = Splices.toReplace.foldLeft(source) {
      case (result, (pattern, replacement)) =>
        result.replaceAll(pattern, replacement)
    }

    def validateNoStubsLeft(source: String): String = {
      val stubs = Splices.toCheck.filter(source.contains)
      assert(stubs.isEmpty, s"Expr stubs left: ${stubs.mkString(", ")} in:\n$source")
      source
    }
  }
}
