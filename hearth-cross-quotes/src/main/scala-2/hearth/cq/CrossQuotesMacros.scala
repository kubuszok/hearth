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
final class CrossQuotesMacros(val c: blackbox.Context) extends ShowCodePrettyScala2 with CrossQuotesMacrosCtorMethods {

  import c.universe.{Expr as _, *}

  protected def reportIssue(throwable: Throwable): Nothing =
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

  protected def log(message: => String): Unit =
    if (loggingEnabled) {
      // println(s"Logging: $message at ${c.enclosingPosition.source.path}:${c.enclosingPosition.line}")
      c.echo(c.enclosingPosition, message)
    }

  protected def pp(any: Any): String = {
    val result = showCodePretty(any, SyntaxHighlight.ANSI)
    // Sometimes the code is empty, not sure why, but for the debugging purposes we need a fallback.
    if (result.nonEmpty) result else any.toString
  }

  private def renderCode(any: Any): String = {
    val result = showCodePretty(any, SyntaxHighlight.plain)
    // If we are rendering code to actually use it and rely on it, we cannot get empty results!!!
    assert(
      result.trim.nonEmpty,
      s"Expected non-empty result for `$any`, unhandle AST:\n${showRawPretty(any, SyntaxHighlight.ANSI)}"
    )
    result
  }

  // We're adding `_` to the prefix to make sure that if there was a trailing digit, it would not affect the order of stubs.
  protected def freshName(prefix: String): TermName =
    c.universe.internal.reificationSupport.freshTermName(prefix + "_")
  protected def freshTypeName(prefix: String): TypeName =
    c.universe.internal.reificationSupport.freshTypeName(prefix + "_")

  private def paint(color: String)(text: String): String =
    text.split("\n").map(line => s"$color$line${Console.RESET}").mkString("\n")

  protected def paintExclDot(color: String)(text: String): String =
    text.split("[.]").map(segment => s"$color$segment${Console.RESET}").mkString(".")

  protected def indent(text: String): String =
    text.split("\n").map(line => s"  $line").mkString("\n")

  protected def suppressWarnings(result: c.Tree): c.Tree =
    q"""
    $result : @_root_.scala.annotation.nowarn
    """

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
    import $ctx.universe.{ TypeRef, TypeRefTag }
    implicit def $convertProvidedTypesForCrossQuotes[$typeInner](implicit $termInner: Type[$typeInner]): $ctx.WeakTypeTag[$typeInner] =
      $termInner.asInstanceOf[$ctx.WeakTypeTag[$typeInner]]
    _root_.hearth.fp.ignore($convertProvidedTypesForCrossQuotes[Any](_: Type[Any]))
    $typeValue
    """

    val result = suppressWarnings(c.typecheck(unchecked))

    log(
      s"""Cross-quotes ${paintExclDot(Console.BLUE)("Type.of")} expansion:
         |From: ${paintExclDot(Console.BLUE)("Type.of")}[${pp(weakTypeOf[A])}]
         |To: ${indent(pp(result))}""".stripMargin
    )

    result
  } catch { case e: Throwable => reportIssue(e) }

  // typeCtor*Impl and typeCtor*FromUntypedImpl forward to implementations in CrossQuotesMacrosCtorMethods.
  // The implementations are generated by project/CrossQuotesMacrosGen.scala.

  // format: off
  def typeCtor1Impl[L1, U1 >: L1, HKT[_ >: L1 <: U1]](implicit L1: c.WeakTypeTag[L1], U1: c.WeakTypeTag[U1], HKTE: c.WeakTypeTag[HKT[?]]): c.Tree = typeCtor1Body[L1, U1, HKT]
  def typeCtor2Impl[L1, U1 >: L1, L2, U2 >: L2, HKT[_ >: L1 <: U1, _ >: L2 <: U2]](implicit L1: c.WeakTypeTag[L1], U1: c.WeakTypeTag[U1], L2: c.WeakTypeTag[L2], U2: c.WeakTypeTag[U2], HKTE: c.WeakTypeTag[HKT[?, ?]]): c.Tree = typeCtor2Body[L1, U1, L2, U2, HKT]
  def typeCtor3Impl[L1, U1 >: L1, L2, U2 >: L2, L3, U3 >: L3, HKT[_ >: L1 <: U1, _ >: L2 <: U2, _ >: L3 <: U3]](implicit L1: c.WeakTypeTag[L1], U1: c.WeakTypeTag[U1], L2: c.WeakTypeTag[L2], U2: c.WeakTypeTag[U2], L3: c.WeakTypeTag[L3], U3: c.WeakTypeTag[U3], HKTE: c.WeakTypeTag[HKT[?, ?, ?]]): c.Tree = typeCtor3Body[L1, U1, L2, U2, L3, U3, HKT]
  def typeCtor4Impl[L1, U1 >: L1, L2, U2 >: L2, L3, U3 >: L3, L4, U4 >: L4, HKT[_ >: L1 <: U1, _ >: L2 <: U2, _ >: L3 <: U3, _ >: L4 <: U4]](implicit L1: c.WeakTypeTag[L1], U1: c.WeakTypeTag[U1], L2: c.WeakTypeTag[L2], U2: c.WeakTypeTag[U2], L3: c.WeakTypeTag[L3], U3: c.WeakTypeTag[U3], L4: c.WeakTypeTag[L4], U4: c.WeakTypeTag[U4], HKTE: c.WeakTypeTag[HKT[?, ?, ?, ?]]): c.Tree = typeCtor4Body[L1, U1, L2, U2, L3, U3, L4, U4, HKT]
  def typeCtor5Impl[L1, U1 >: L1, L2, U2 >: L2, L3, U3 >: L3, L4, U4 >: L4, L5, U5 >: L5, HKT[_ >: L1 <: U1, _ >: L2 <: U2, _ >: L3 <: U3, _ >: L4 <: U4, _ >: L5 <: U5]](implicit L1: c.WeakTypeTag[L1], U1: c.WeakTypeTag[U1], L2: c.WeakTypeTag[L2], U2: c.WeakTypeTag[U2], L3: c.WeakTypeTag[L3], U3: c.WeakTypeTag[U3], L4: c.WeakTypeTag[L4], U4: c.WeakTypeTag[U4], L5: c.WeakTypeTag[L5], U5: c.WeakTypeTag[U5], HKTE: c.WeakTypeTag[HKT[?, ?, ?, ?, ?]]): c.Tree = typeCtor5Body[L1, U1, L2, U2, L3, U3, L4, U4, L5, U5, HKT]
  def typeCtor6Impl[L1, U1 >: L1, L2, U2 >: L2, L3, U3 >: L3, L4, U4 >: L4, L5, U5 >: L5, L6, U6 >: L6, HKT[_ >: L1 <: U1, _ >: L2 <: U2, _ >: L3 <: U3, _ >: L4 <: U4, _ >: L5 <: U5, _ >: L6 <: U6]](implicit L1: c.WeakTypeTag[L1], U1: c.WeakTypeTag[U1], L2: c.WeakTypeTag[L2], U2: c.WeakTypeTag[U2], L3: c.WeakTypeTag[L3], U3: c.WeakTypeTag[U3], L4: c.WeakTypeTag[L4], U4: c.WeakTypeTag[U4], L5: c.WeakTypeTag[L5], U5: c.WeakTypeTag[U5], L6: c.WeakTypeTag[L6], U6: c.WeakTypeTag[U6], HKTE: c.WeakTypeTag[HKT[?, ?, ?, ?, ?, ?]]): c.Tree = typeCtor6Body[L1, U1, L2, U2, L3, U3, L4, U4, L5, U5, L6, U6, HKT]
  def typeCtor7Impl[L1, U1 >: L1, L2, U2 >: L2, L3, U3 >: L3, L4, U4 >: L4, L5, U5 >: L5, L6, U6 >: L6, L7, U7 >: L7, HKT[_ >: L1 <: U1, _ >: L2 <: U2, _ >: L3 <: U3, _ >: L4 <: U4, _ >: L5 <: U5, _ >: L6 <: U6, _ >: L7 <: U7]](implicit L1: c.WeakTypeTag[L1], U1: c.WeakTypeTag[U1], L2: c.WeakTypeTag[L2], U2: c.WeakTypeTag[U2], L3: c.WeakTypeTag[L3], U3: c.WeakTypeTag[U3], L4: c.WeakTypeTag[L4], U4: c.WeakTypeTag[U4], L5: c.WeakTypeTag[L5], U5: c.WeakTypeTag[U5], L6: c.WeakTypeTag[L6], U6: c.WeakTypeTag[U6], L7: c.WeakTypeTag[L7], U7: c.WeakTypeTag[U7], HKTE: c.WeakTypeTag[HKT[?, ?, ?, ?, ?, ?, ?]]): c.Tree = typeCtor7Body[L1, U1, L2, U2, L3, U3, L4, U4, L5, U5, L6, U6, L7, U7, HKT]
  def typeCtor8Impl[L1, U1 >: L1, L2, U2 >: L2, L3, U3 >: L3, L4, U4 >: L4, L5, U5 >: L5, L6, U6 >: L6, L7, U7 >: L7, L8, U8 >: L8, HKT[_ >: L1 <: U1, _ >: L2 <: U2, _ >: L3 <: U3, _ >: L4 <: U4, _ >: L5 <: U5, _ >: L6 <: U6, _ >: L7 <: U7, _ >: L8 <: U8]](implicit L1: c.WeakTypeTag[L1], U1: c.WeakTypeTag[U1], L2: c.WeakTypeTag[L2], U2: c.WeakTypeTag[U2], L3: c.WeakTypeTag[L3], U3: c.WeakTypeTag[U3], L4: c.WeakTypeTag[L4], U4: c.WeakTypeTag[U4], L5: c.WeakTypeTag[L5], U5: c.WeakTypeTag[U5], L6: c.WeakTypeTag[L6], U6: c.WeakTypeTag[U6], L7: c.WeakTypeTag[L7], U7: c.WeakTypeTag[U7], L8: c.WeakTypeTag[L8], U8: c.WeakTypeTag[U8], HKTE: c.WeakTypeTag[HKT[?, ?, ?, ?, ?, ?, ?, ?]]): c.Tree = typeCtor8Body[L1, U1, L2, U2, L3, U3, L4, U4, L5, U5, L6, U6, L7, U7, L8, U8, HKT]
  def typeCtor9Impl[L1, U1 >: L1, L2, U2 >: L2, L3, U3 >: L3, L4, U4 >: L4, L5, U5 >: L5, L6, U6 >: L6, L7, U7 >: L7, L8, U8 >: L8, L9, U9 >: L9, HKT[_ >: L1 <: U1, _ >: L2 <: U2, _ >: L3 <: U3, _ >: L4 <: U4, _ >: L5 <: U5, _ >: L6 <: U6, _ >: L7 <: U7, _ >: L8 <: U8, _ >: L9 <: U9]](implicit L1: c.WeakTypeTag[L1], U1: c.WeakTypeTag[U1], L2: c.WeakTypeTag[L2], U2: c.WeakTypeTag[U2], L3: c.WeakTypeTag[L3], U3: c.WeakTypeTag[U3], L4: c.WeakTypeTag[L4], U4: c.WeakTypeTag[U4], L5: c.WeakTypeTag[L5], U5: c.WeakTypeTag[U5], L6: c.WeakTypeTag[L6], U6: c.WeakTypeTag[U6], L7: c.WeakTypeTag[L7], U7: c.WeakTypeTag[U7], L8: c.WeakTypeTag[L8], U8: c.WeakTypeTag[U8], L9: c.WeakTypeTag[L9], U9: c.WeakTypeTag[U9], HKTE: c.WeakTypeTag[HKT[?, ?, ?, ?, ?, ?, ?, ?, ?]]): c.Tree = typeCtor9Body[L1, U1, L2, U2, L3, U3, L4, U4, L5, U5, L6, U6, L7, U7, L8, U8, L9, U9, HKT]
  def typeCtor10Impl[L1, U1 >: L1, L2, U2 >: L2, L3, U3 >: L3, L4, U4 >: L4, L5, U5 >: L5, L6, U6 >: L6, L7, U7 >: L7, L8, U8 >: L8, L9, U9 >: L9, L10, U10 >: L10, HKT[_ >: L1 <: U1, _ >: L2 <: U2, _ >: L3 <: U3, _ >: L4 <: U4, _ >: L5 <: U5, _ >: L6 <: U6, _ >: L7 <: U7, _ >: L8 <: U8, _ >: L9 <: U9, _ >: L10 <: U10]](implicit L1: c.WeakTypeTag[L1], U1: c.WeakTypeTag[U1], L2: c.WeakTypeTag[L2], U2: c.WeakTypeTag[U2], L3: c.WeakTypeTag[L3], U3: c.WeakTypeTag[U3], L4: c.WeakTypeTag[L4], U4: c.WeakTypeTag[U4], L5: c.WeakTypeTag[L5], U5: c.WeakTypeTag[U5], L6: c.WeakTypeTag[L6], U6: c.WeakTypeTag[U6], L7: c.WeakTypeTag[L7], U7: c.WeakTypeTag[U7], L8: c.WeakTypeTag[L8], U8: c.WeakTypeTag[U8], L9: c.WeakTypeTag[L9], U9: c.WeakTypeTag[U9], L10: c.WeakTypeTag[L10], U10: c.WeakTypeTag[U10], HKTE: c.WeakTypeTag[HKT[?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]): c.Tree = typeCtor10Body[L1, U1, L2, U2, L3, U3, L4, U4, L5, U5, L6, U6, L7, U7, L8, U8, L9, U9, L10, U10, HKT]
  def typeCtor11Impl[L1, U1 >: L1, L2, U2 >: L2, L3, U3 >: L3, L4, U4 >: L4, L5, U5 >: L5, L6, U6 >: L6, L7, U7 >: L7, L8, U8 >: L8, L9, U9 >: L9, L10, U10 >: L10, L11, U11 >: L11, HKT[_ >: L1 <: U1, _ >: L2 <: U2, _ >: L3 <: U3, _ >: L4 <: U4, _ >: L5 <: U5, _ >: L6 <: U6, _ >: L7 <: U7, _ >: L8 <: U8, _ >: L9 <: U9, _ >: L10 <: U10, _ >: L11 <: U11]](implicit L1: c.WeakTypeTag[L1], U1: c.WeakTypeTag[U1], L2: c.WeakTypeTag[L2], U2: c.WeakTypeTag[U2], L3: c.WeakTypeTag[L3], U3: c.WeakTypeTag[U3], L4: c.WeakTypeTag[L4], U4: c.WeakTypeTag[U4], L5: c.WeakTypeTag[L5], U5: c.WeakTypeTag[U5], L6: c.WeakTypeTag[L6], U6: c.WeakTypeTag[U6], L7: c.WeakTypeTag[L7], U7: c.WeakTypeTag[U7], L8: c.WeakTypeTag[L8], U8: c.WeakTypeTag[U8], L9: c.WeakTypeTag[L9], U9: c.WeakTypeTag[U9], L10: c.WeakTypeTag[L10], U10: c.WeakTypeTag[U10], L11: c.WeakTypeTag[L11], U11: c.WeakTypeTag[U11], HKTE: c.WeakTypeTag[HKT[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]): c.Tree = typeCtor11Body[L1, U1, L2, U2, L3, U3, L4, U4, L5, U5, L6, U6, L7, U7, L8, U8, L9, U9, L10, U10, L11, U11, HKT]
  def typeCtor12Impl[L1, U1 >: L1, L2, U2 >: L2, L3, U3 >: L3, L4, U4 >: L4, L5, U5 >: L5, L6, U6 >: L6, L7, U7 >: L7, L8, U8 >: L8, L9, U9 >: L9, L10, U10 >: L10, L11, U11 >: L11, L12, U12 >: L12, HKT[_ >: L1 <: U1, _ >: L2 <: U2, _ >: L3 <: U3, _ >: L4 <: U4, _ >: L5 <: U5, _ >: L6 <: U6, _ >: L7 <: U7, _ >: L8 <: U8, _ >: L9 <: U9, _ >: L10 <: U10, _ >: L11 <: U11, _ >: L12 <: U12]](implicit L1: c.WeakTypeTag[L1], U1: c.WeakTypeTag[U1], L2: c.WeakTypeTag[L2], U2: c.WeakTypeTag[U2], L3: c.WeakTypeTag[L3], U3: c.WeakTypeTag[U3], L4: c.WeakTypeTag[L4], U4: c.WeakTypeTag[U4], L5: c.WeakTypeTag[L5], U5: c.WeakTypeTag[U5], L6: c.WeakTypeTag[L6], U6: c.WeakTypeTag[U6], L7: c.WeakTypeTag[L7], U7: c.WeakTypeTag[U7], L8: c.WeakTypeTag[L8], U8: c.WeakTypeTag[U8], L9: c.WeakTypeTag[L9], U9: c.WeakTypeTag[U9], L10: c.WeakTypeTag[L10], U10: c.WeakTypeTag[U10], L11: c.WeakTypeTag[L11], U11: c.WeakTypeTag[U11], L12: c.WeakTypeTag[L12], U12: c.WeakTypeTag[U12], HKTE: c.WeakTypeTag[HKT[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]): c.Tree = typeCtor12Body[L1, U1, L2, U2, L3, U3, L4, U4, L5, U5, L6, U6, L7, U7, L8, U8, L9, U9, L10, U10, L11, U11, L12, U12, HKT]
  def typeCtor13Impl[L1, U1 >: L1, L2, U2 >: L2, L3, U3 >: L3, L4, U4 >: L4, L5, U5 >: L5, L6, U6 >: L6, L7, U7 >: L7, L8, U8 >: L8, L9, U9 >: L9, L10, U10 >: L10, L11, U11 >: L11, L12, U12 >: L12, L13, U13 >: L13, HKT[_ >: L1 <: U1, _ >: L2 <: U2, _ >: L3 <: U3, _ >: L4 <: U4, _ >: L5 <: U5, _ >: L6 <: U6, _ >: L7 <: U7, _ >: L8 <: U8, _ >: L9 <: U9, _ >: L10 <: U10, _ >: L11 <: U11, _ >: L12 <: U12, _ >: L13 <: U13]](implicit L1: c.WeakTypeTag[L1], U1: c.WeakTypeTag[U1], L2: c.WeakTypeTag[L2], U2: c.WeakTypeTag[U2], L3: c.WeakTypeTag[L3], U3: c.WeakTypeTag[U3], L4: c.WeakTypeTag[L4], U4: c.WeakTypeTag[U4], L5: c.WeakTypeTag[L5], U5: c.WeakTypeTag[U5], L6: c.WeakTypeTag[L6], U6: c.WeakTypeTag[U6], L7: c.WeakTypeTag[L7], U7: c.WeakTypeTag[U7], L8: c.WeakTypeTag[L8], U8: c.WeakTypeTag[U8], L9: c.WeakTypeTag[L9], U9: c.WeakTypeTag[U9], L10: c.WeakTypeTag[L10], U10: c.WeakTypeTag[U10], L11: c.WeakTypeTag[L11], U11: c.WeakTypeTag[U11], L12: c.WeakTypeTag[L12], U12: c.WeakTypeTag[U12], L13: c.WeakTypeTag[L13], U13: c.WeakTypeTag[U13], HKTE: c.WeakTypeTag[HKT[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]): c.Tree = typeCtor13Body[L1, U1, L2, U2, L3, U3, L4, U4, L5, U5, L6, U6, L7, U7, L8, U8, L9, U9, L10, U10, L11, U11, L12, U12, L13, U13, HKT]
  def typeCtor14Impl[L1, U1 >: L1, L2, U2 >: L2, L3, U3 >: L3, L4, U4 >: L4, L5, U5 >: L5, L6, U6 >: L6, L7, U7 >: L7, L8, U8 >: L8, L9, U9 >: L9, L10, U10 >: L10, L11, U11 >: L11, L12, U12 >: L12, L13, U13 >: L13, L14, U14 >: L14, HKT[_ >: L1 <: U1, _ >: L2 <: U2, _ >: L3 <: U3, _ >: L4 <: U4, _ >: L5 <: U5, _ >: L6 <: U6, _ >: L7 <: U7, _ >: L8 <: U8, _ >: L9 <: U9, _ >: L10 <: U10, _ >: L11 <: U11, _ >: L12 <: U12, _ >: L13 <: U13, _ >: L14 <: U14]](implicit L1: c.WeakTypeTag[L1], U1: c.WeakTypeTag[U1], L2: c.WeakTypeTag[L2], U2: c.WeakTypeTag[U2], L3: c.WeakTypeTag[L3], U3: c.WeakTypeTag[U3], L4: c.WeakTypeTag[L4], U4: c.WeakTypeTag[U4], L5: c.WeakTypeTag[L5], U5: c.WeakTypeTag[U5], L6: c.WeakTypeTag[L6], U6: c.WeakTypeTag[U6], L7: c.WeakTypeTag[L7], U7: c.WeakTypeTag[U7], L8: c.WeakTypeTag[L8], U8: c.WeakTypeTag[U8], L9: c.WeakTypeTag[L9], U9: c.WeakTypeTag[U9], L10: c.WeakTypeTag[L10], U10: c.WeakTypeTag[U10], L11: c.WeakTypeTag[L11], U11: c.WeakTypeTag[U11], L12: c.WeakTypeTag[L12], U12: c.WeakTypeTag[U12], L13: c.WeakTypeTag[L13], U13: c.WeakTypeTag[U13], L14: c.WeakTypeTag[L14], U14: c.WeakTypeTag[U14], HKTE: c.WeakTypeTag[HKT[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]): c.Tree = typeCtor14Body[L1, U1, L2, U2, L3, U3, L4, U4, L5, U5, L6, U6, L7, U7, L8, U8, L9, U9, L10, U10, L11, U11, L12, U12, L13, U13, L14, U14, HKT]
  def typeCtor15Impl[L1, U1 >: L1, L2, U2 >: L2, L3, U3 >: L3, L4, U4 >: L4, L5, U5 >: L5, L6, U6 >: L6, L7, U7 >: L7, L8, U8 >: L8, L9, U9 >: L9, L10, U10 >: L10, L11, U11 >: L11, L12, U12 >: L12, L13, U13 >: L13, L14, U14 >: L14, L15, U15 >: L15, HKT[_ >: L1 <: U1, _ >: L2 <: U2, _ >: L3 <: U3, _ >: L4 <: U4, _ >: L5 <: U5, _ >: L6 <: U6, _ >: L7 <: U7, _ >: L8 <: U8, _ >: L9 <: U9, _ >: L10 <: U10, _ >: L11 <: U11, _ >: L12 <: U12, _ >: L13 <: U13, _ >: L14 <: U14, _ >: L15 <: U15]](implicit L1: c.WeakTypeTag[L1], U1: c.WeakTypeTag[U1], L2: c.WeakTypeTag[L2], U2: c.WeakTypeTag[U2], L3: c.WeakTypeTag[L3], U3: c.WeakTypeTag[U3], L4: c.WeakTypeTag[L4], U4: c.WeakTypeTag[U4], L5: c.WeakTypeTag[L5], U5: c.WeakTypeTag[U5], L6: c.WeakTypeTag[L6], U6: c.WeakTypeTag[U6], L7: c.WeakTypeTag[L7], U7: c.WeakTypeTag[U7], L8: c.WeakTypeTag[L8], U8: c.WeakTypeTag[U8], L9: c.WeakTypeTag[L9], U9: c.WeakTypeTag[U9], L10: c.WeakTypeTag[L10], U10: c.WeakTypeTag[U10], L11: c.WeakTypeTag[L11], U11: c.WeakTypeTag[U11], L12: c.WeakTypeTag[L12], U12: c.WeakTypeTag[U12], L13: c.WeakTypeTag[L13], U13: c.WeakTypeTag[U13], L14: c.WeakTypeTag[L14], U14: c.WeakTypeTag[U14], L15: c.WeakTypeTag[L15], U15: c.WeakTypeTag[U15], HKTE: c.WeakTypeTag[HKT[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]): c.Tree = typeCtor15Body[L1, U1, L2, U2, L3, U3, L4, U4, L5, U5, L6, U6, L7, U7, L8, U8, L9, U9, L10, U10, L11, U11, L12, U12, L13, U13, L14, U14, L15, U15, HKT]
  def typeCtor16Impl[L1, U1 >: L1, L2, U2 >: L2, L3, U3 >: L3, L4, U4 >: L4, L5, U5 >: L5, L6, U6 >: L6, L7, U7 >: L7, L8, U8 >: L8, L9, U9 >: L9, L10, U10 >: L10, L11, U11 >: L11, L12, U12 >: L12, L13, U13 >: L13, L14, U14 >: L14, L15, U15 >: L15, L16, U16 >: L16, HKT[_ >: L1 <: U1, _ >: L2 <: U2, _ >: L3 <: U3, _ >: L4 <: U4, _ >: L5 <: U5, _ >: L6 <: U6, _ >: L7 <: U7, _ >: L8 <: U8, _ >: L9 <: U9, _ >: L10 <: U10, _ >: L11 <: U11, _ >: L12 <: U12, _ >: L13 <: U13, _ >: L14 <: U14, _ >: L15 <: U15, _ >: L16 <: U16]](implicit L1: c.WeakTypeTag[L1], U1: c.WeakTypeTag[U1], L2: c.WeakTypeTag[L2], U2: c.WeakTypeTag[U2], L3: c.WeakTypeTag[L3], U3: c.WeakTypeTag[U3], L4: c.WeakTypeTag[L4], U4: c.WeakTypeTag[U4], L5: c.WeakTypeTag[L5], U5: c.WeakTypeTag[U5], L6: c.WeakTypeTag[L6], U6: c.WeakTypeTag[U6], L7: c.WeakTypeTag[L7], U7: c.WeakTypeTag[U7], L8: c.WeakTypeTag[L8], U8: c.WeakTypeTag[U8], L9: c.WeakTypeTag[L9], U9: c.WeakTypeTag[U9], L10: c.WeakTypeTag[L10], U10: c.WeakTypeTag[U10], L11: c.WeakTypeTag[L11], U11: c.WeakTypeTag[U11], L12: c.WeakTypeTag[L12], U12: c.WeakTypeTag[U12], L13: c.WeakTypeTag[L13], U13: c.WeakTypeTag[U13], L14: c.WeakTypeTag[L14], U14: c.WeakTypeTag[U14], L15: c.WeakTypeTag[L15], U15: c.WeakTypeTag[U15], L16: c.WeakTypeTag[L16], U16: c.WeakTypeTag[U16], HKTE: c.WeakTypeTag[HKT[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]): c.Tree = typeCtor16Body[L1, U1, L2, U2, L3, U3, L4, U4, L5, U5, L6, U6, L7, U7, L8, U8, L9, U9, L10, U10, L11, U11, L12, U12, L13, U13, L14, U14, L15, U15, L16, U16, HKT]
  def typeCtor17Impl[L1, U1 >: L1, L2, U2 >: L2, L3, U3 >: L3, L4, U4 >: L4, L5, U5 >: L5, L6, U6 >: L6, L7, U7 >: L7, L8, U8 >: L8, L9, U9 >: L9, L10, U10 >: L10, L11, U11 >: L11, L12, U12 >: L12, L13, U13 >: L13, L14, U14 >: L14, L15, U15 >: L15, L16, U16 >: L16, L17, U17 >: L17, HKT[_ >: L1 <: U1, _ >: L2 <: U2, _ >: L3 <: U3, _ >: L4 <: U4, _ >: L5 <: U5, _ >: L6 <: U6, _ >: L7 <: U7, _ >: L8 <: U8, _ >: L9 <: U9, _ >: L10 <: U10, _ >: L11 <: U11, _ >: L12 <: U12, _ >: L13 <: U13, _ >: L14 <: U14, _ >: L15 <: U15, _ >: L16 <: U16, _ >: L17 <: U17]](implicit L1: c.WeakTypeTag[L1], U1: c.WeakTypeTag[U1], L2: c.WeakTypeTag[L2], U2: c.WeakTypeTag[U2], L3: c.WeakTypeTag[L3], U3: c.WeakTypeTag[U3], L4: c.WeakTypeTag[L4], U4: c.WeakTypeTag[U4], L5: c.WeakTypeTag[L5], U5: c.WeakTypeTag[U5], L6: c.WeakTypeTag[L6], U6: c.WeakTypeTag[U6], L7: c.WeakTypeTag[L7], U7: c.WeakTypeTag[U7], L8: c.WeakTypeTag[L8], U8: c.WeakTypeTag[U8], L9: c.WeakTypeTag[L9], U9: c.WeakTypeTag[U9], L10: c.WeakTypeTag[L10], U10: c.WeakTypeTag[U10], L11: c.WeakTypeTag[L11], U11: c.WeakTypeTag[U11], L12: c.WeakTypeTag[L12], U12: c.WeakTypeTag[U12], L13: c.WeakTypeTag[L13], U13: c.WeakTypeTag[U13], L14: c.WeakTypeTag[L14], U14: c.WeakTypeTag[U14], L15: c.WeakTypeTag[L15], U15: c.WeakTypeTag[U15], L16: c.WeakTypeTag[L16], U16: c.WeakTypeTag[U16], L17: c.WeakTypeTag[L17], U17: c.WeakTypeTag[U17], HKTE: c.WeakTypeTag[HKT[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]): c.Tree = typeCtor17Body[L1, U1, L2, U2, L3, U3, L4, U4, L5, U5, L6, U6, L7, U7, L8, U8, L9, U9, L10, U10, L11, U11, L12, U12, L13, U13, L14, U14, L15, U15, L16, U16, L17, U17, HKT]
  def typeCtor18Impl[L1, U1 >: L1, L2, U2 >: L2, L3, U3 >: L3, L4, U4 >: L4, L5, U5 >: L5, L6, U6 >: L6, L7, U7 >: L7, L8, U8 >: L8, L9, U9 >: L9, L10, U10 >: L10, L11, U11 >: L11, L12, U12 >: L12, L13, U13 >: L13, L14, U14 >: L14, L15, U15 >: L15, L16, U16 >: L16, L17, U17 >: L17, L18, U18 >: L18, HKT[_ >: L1 <: U1, _ >: L2 <: U2, _ >: L3 <: U3, _ >: L4 <: U4, _ >: L5 <: U5, _ >: L6 <: U6, _ >: L7 <: U7, _ >: L8 <: U8, _ >: L9 <: U9, _ >: L10 <: U10, _ >: L11 <: U11, _ >: L12 <: U12, _ >: L13 <: U13, _ >: L14 <: U14, _ >: L15 <: U15, _ >: L16 <: U16, _ >: L17 <: U17, _ >: L18 <: U18]](implicit L1: c.WeakTypeTag[L1], U1: c.WeakTypeTag[U1], L2: c.WeakTypeTag[L2], U2: c.WeakTypeTag[U2], L3: c.WeakTypeTag[L3], U3: c.WeakTypeTag[U3], L4: c.WeakTypeTag[L4], U4: c.WeakTypeTag[U4], L5: c.WeakTypeTag[L5], U5: c.WeakTypeTag[U5], L6: c.WeakTypeTag[L6], U6: c.WeakTypeTag[U6], L7: c.WeakTypeTag[L7], U7: c.WeakTypeTag[U7], L8: c.WeakTypeTag[L8], U8: c.WeakTypeTag[U8], L9: c.WeakTypeTag[L9], U9: c.WeakTypeTag[U9], L10: c.WeakTypeTag[L10], U10: c.WeakTypeTag[U10], L11: c.WeakTypeTag[L11], U11: c.WeakTypeTag[U11], L12: c.WeakTypeTag[L12], U12: c.WeakTypeTag[U12], L13: c.WeakTypeTag[L13], U13: c.WeakTypeTag[U13], L14: c.WeakTypeTag[L14], U14: c.WeakTypeTag[U14], L15: c.WeakTypeTag[L15], U15: c.WeakTypeTag[U15], L16: c.WeakTypeTag[L16], U16: c.WeakTypeTag[U16], L17: c.WeakTypeTag[L17], U17: c.WeakTypeTag[U17], L18: c.WeakTypeTag[L18], U18: c.WeakTypeTag[U18], HKTE: c.WeakTypeTag[HKT[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]): c.Tree = typeCtor18Body[L1, U1, L2, U2, L3, U3, L4, U4, L5, U5, L6, U6, L7, U7, L8, U8, L9, U9, L10, U10, L11, U11, L12, U12, L13, U13, L14, U14, L15, U15, L16, U16, L17, U17, L18, U18, HKT]
  def typeCtor19Impl[L1, U1 >: L1, L2, U2 >: L2, L3, U3 >: L3, L4, U4 >: L4, L5, U5 >: L5, L6, U6 >: L6, L7, U7 >: L7, L8, U8 >: L8, L9, U9 >: L9, L10, U10 >: L10, L11, U11 >: L11, L12, U12 >: L12, L13, U13 >: L13, L14, U14 >: L14, L15, U15 >: L15, L16, U16 >: L16, L17, U17 >: L17, L18, U18 >: L18, L19, U19 >: L19, HKT[_ >: L1 <: U1, _ >: L2 <: U2, _ >: L3 <: U3, _ >: L4 <: U4, _ >: L5 <: U5, _ >: L6 <: U6, _ >: L7 <: U7, _ >: L8 <: U8, _ >: L9 <: U9, _ >: L10 <: U10, _ >: L11 <: U11, _ >: L12 <: U12, _ >: L13 <: U13, _ >: L14 <: U14, _ >: L15 <: U15, _ >: L16 <: U16, _ >: L17 <: U17, _ >: L18 <: U18, _ >: L19 <: U19]](implicit L1: c.WeakTypeTag[L1], U1: c.WeakTypeTag[U1], L2: c.WeakTypeTag[L2], U2: c.WeakTypeTag[U2], L3: c.WeakTypeTag[L3], U3: c.WeakTypeTag[U3], L4: c.WeakTypeTag[L4], U4: c.WeakTypeTag[U4], L5: c.WeakTypeTag[L5], U5: c.WeakTypeTag[U5], L6: c.WeakTypeTag[L6], U6: c.WeakTypeTag[U6], L7: c.WeakTypeTag[L7], U7: c.WeakTypeTag[U7], L8: c.WeakTypeTag[L8], U8: c.WeakTypeTag[U8], L9: c.WeakTypeTag[L9], U9: c.WeakTypeTag[U9], L10: c.WeakTypeTag[L10], U10: c.WeakTypeTag[U10], L11: c.WeakTypeTag[L11], U11: c.WeakTypeTag[U11], L12: c.WeakTypeTag[L12], U12: c.WeakTypeTag[U12], L13: c.WeakTypeTag[L13], U13: c.WeakTypeTag[U13], L14: c.WeakTypeTag[L14], U14: c.WeakTypeTag[U14], L15: c.WeakTypeTag[L15], U15: c.WeakTypeTag[U15], L16: c.WeakTypeTag[L16], U16: c.WeakTypeTag[U16], L17: c.WeakTypeTag[L17], U17: c.WeakTypeTag[U17], L18: c.WeakTypeTag[L18], U18: c.WeakTypeTag[U18], L19: c.WeakTypeTag[L19], U19: c.WeakTypeTag[U19], HKTE: c.WeakTypeTag[HKT[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]): c.Tree = typeCtor19Body[L1, U1, L2, U2, L3, U3, L4, U4, L5, U5, L6, U6, L7, U7, L8, U8, L9, U9, L10, U10, L11, U11, L12, U12, L13, U13, L14, U14, L15, U15, L16, U16, L17, U17, L18, U18, L19, U19, HKT]
  def typeCtor20Impl[L1, U1 >: L1, L2, U2 >: L2, L3, U3 >: L3, L4, U4 >: L4, L5, U5 >: L5, L6, U6 >: L6, L7, U7 >: L7, L8, U8 >: L8, L9, U9 >: L9, L10, U10 >: L10, L11, U11 >: L11, L12, U12 >: L12, L13, U13 >: L13, L14, U14 >: L14, L15, U15 >: L15, L16, U16 >: L16, L17, U17 >: L17, L18, U18 >: L18, L19, U19 >: L19, L20, U20 >: L20, HKT[_ >: L1 <: U1, _ >: L2 <: U2, _ >: L3 <: U3, _ >: L4 <: U4, _ >: L5 <: U5, _ >: L6 <: U6, _ >: L7 <: U7, _ >: L8 <: U8, _ >: L9 <: U9, _ >: L10 <: U10, _ >: L11 <: U11, _ >: L12 <: U12, _ >: L13 <: U13, _ >: L14 <: U14, _ >: L15 <: U15, _ >: L16 <: U16, _ >: L17 <: U17, _ >: L18 <: U18, _ >: L19 <: U19, _ >: L20 <: U20]](implicit L1: c.WeakTypeTag[L1], U1: c.WeakTypeTag[U1], L2: c.WeakTypeTag[L2], U2: c.WeakTypeTag[U2], L3: c.WeakTypeTag[L3], U3: c.WeakTypeTag[U3], L4: c.WeakTypeTag[L4], U4: c.WeakTypeTag[U4], L5: c.WeakTypeTag[L5], U5: c.WeakTypeTag[U5], L6: c.WeakTypeTag[L6], U6: c.WeakTypeTag[U6], L7: c.WeakTypeTag[L7], U7: c.WeakTypeTag[U7], L8: c.WeakTypeTag[L8], U8: c.WeakTypeTag[U8], L9: c.WeakTypeTag[L9], U9: c.WeakTypeTag[U9], L10: c.WeakTypeTag[L10], U10: c.WeakTypeTag[U10], L11: c.WeakTypeTag[L11], U11: c.WeakTypeTag[U11], L12: c.WeakTypeTag[L12], U12: c.WeakTypeTag[U12], L13: c.WeakTypeTag[L13], U13: c.WeakTypeTag[U13], L14: c.WeakTypeTag[L14], U14: c.WeakTypeTag[U14], L15: c.WeakTypeTag[L15], U15: c.WeakTypeTag[U15], L16: c.WeakTypeTag[L16], U16: c.WeakTypeTag[U16], L17: c.WeakTypeTag[L17], U17: c.WeakTypeTag[U17], L18: c.WeakTypeTag[L18], U18: c.WeakTypeTag[U18], L19: c.WeakTypeTag[L19], U19: c.WeakTypeTag[U19], L20: c.WeakTypeTag[L20], U20: c.WeakTypeTag[U20], HKTE: c.WeakTypeTag[HKT[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]): c.Tree = typeCtor20Body[L1, U1, L2, U2, L3, U3, L4, U4, L5, U5, L6, U6, L7, U7, L8, U8, L9, U9, L10, U10, L11, U11, L12, U12, L13, U13, L14, U14, L15, U15, L16, U16, L17, U17, L18, U18, L19, U19, L20, U20, HKT]
  def typeCtor21Impl[L1, U1 >: L1, L2, U2 >: L2, L3, U3 >: L3, L4, U4 >: L4, L5, U5 >: L5, L6, U6 >: L6, L7, U7 >: L7, L8, U8 >: L8, L9, U9 >: L9, L10, U10 >: L10, L11, U11 >: L11, L12, U12 >: L12, L13, U13 >: L13, L14, U14 >: L14, L15, U15 >: L15, L16, U16 >: L16, L17, U17 >: L17, L18, U18 >: L18, L19, U19 >: L19, L20, U20 >: L20, L21, U21 >: L21, HKT[_ >: L1 <: U1, _ >: L2 <: U2, _ >: L3 <: U3, _ >: L4 <: U4, _ >: L5 <: U5, _ >: L6 <: U6, _ >: L7 <: U7, _ >: L8 <: U8, _ >: L9 <: U9, _ >: L10 <: U10, _ >: L11 <: U11, _ >: L12 <: U12, _ >: L13 <: U13, _ >: L14 <: U14, _ >: L15 <: U15, _ >: L16 <: U16, _ >: L17 <: U17, _ >: L18 <: U18, _ >: L19 <: U19, _ >: L20 <: U20, _ >: L21 <: U21]](implicit L1: c.WeakTypeTag[L1], U1: c.WeakTypeTag[U1], L2: c.WeakTypeTag[L2], U2: c.WeakTypeTag[U2], L3: c.WeakTypeTag[L3], U3: c.WeakTypeTag[U3], L4: c.WeakTypeTag[L4], U4: c.WeakTypeTag[U4], L5: c.WeakTypeTag[L5], U5: c.WeakTypeTag[U5], L6: c.WeakTypeTag[L6], U6: c.WeakTypeTag[U6], L7: c.WeakTypeTag[L7], U7: c.WeakTypeTag[U7], L8: c.WeakTypeTag[L8], U8: c.WeakTypeTag[U8], L9: c.WeakTypeTag[L9], U9: c.WeakTypeTag[U9], L10: c.WeakTypeTag[L10], U10: c.WeakTypeTag[U10], L11: c.WeakTypeTag[L11], U11: c.WeakTypeTag[U11], L12: c.WeakTypeTag[L12], U12: c.WeakTypeTag[U12], L13: c.WeakTypeTag[L13], U13: c.WeakTypeTag[U13], L14: c.WeakTypeTag[L14], U14: c.WeakTypeTag[U14], L15: c.WeakTypeTag[L15], U15: c.WeakTypeTag[U15], L16: c.WeakTypeTag[L16], U16: c.WeakTypeTag[U16], L17: c.WeakTypeTag[L17], U17: c.WeakTypeTag[U17], L18: c.WeakTypeTag[L18], U18: c.WeakTypeTag[U18], L19: c.WeakTypeTag[L19], U19: c.WeakTypeTag[U19], L20: c.WeakTypeTag[L20], U20: c.WeakTypeTag[U20], L21: c.WeakTypeTag[L21], U21: c.WeakTypeTag[U21], HKTE: c.WeakTypeTag[HKT[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]): c.Tree = typeCtor21Body[L1, U1, L2, U2, L3, U3, L4, U4, L5, U5, L6, U6, L7, U7, L8, U8, L9, U9, L10, U10, L11, U11, L12, U12, L13, U13, L14, U14, L15, U15, L16, U16, L17, U17, L18, U18, L19, U19, L20, U20, L21, U21, HKT]
  def typeCtor22Impl[L1, U1 >: L1, L2, U2 >: L2, L3, U3 >: L3, L4, U4 >: L4, L5, U5 >: L5, L6, U6 >: L6, L7, U7 >: L7, L8, U8 >: L8, L9, U9 >: L9, L10, U10 >: L10, L11, U11 >: L11, L12, U12 >: L12, L13, U13 >: L13, L14, U14 >: L14, L15, U15 >: L15, L16, U16 >: L16, L17, U17 >: L17, L18, U18 >: L18, L19, U19 >: L19, L20, U20 >: L20, L21, U21 >: L21, L22, U22 >: L22, HKT[_ >: L1 <: U1, _ >: L2 <: U2, _ >: L3 <: U3, _ >: L4 <: U4, _ >: L5 <: U5, _ >: L6 <: U6, _ >: L7 <: U7, _ >: L8 <: U8, _ >: L9 <: U9, _ >: L10 <: U10, _ >: L11 <: U11, _ >: L12 <: U12, _ >: L13 <: U13, _ >: L14 <: U14, _ >: L15 <: U15, _ >: L16 <: U16, _ >: L17 <: U17, _ >: L18 <: U18, _ >: L19 <: U19, _ >: L20 <: U20, _ >: L21 <: U21, _ >: L22 <: U22]](implicit L1: c.WeakTypeTag[L1], U1: c.WeakTypeTag[U1], L2: c.WeakTypeTag[L2], U2: c.WeakTypeTag[U2], L3: c.WeakTypeTag[L3], U3: c.WeakTypeTag[U3], L4: c.WeakTypeTag[L4], U4: c.WeakTypeTag[U4], L5: c.WeakTypeTag[L5], U5: c.WeakTypeTag[U5], L6: c.WeakTypeTag[L6], U6: c.WeakTypeTag[U6], L7: c.WeakTypeTag[L7], U7: c.WeakTypeTag[U7], L8: c.WeakTypeTag[L8], U8: c.WeakTypeTag[U8], L9: c.WeakTypeTag[L9], U9: c.WeakTypeTag[U9], L10: c.WeakTypeTag[L10], U10: c.WeakTypeTag[U10], L11: c.WeakTypeTag[L11], U11: c.WeakTypeTag[U11], L12: c.WeakTypeTag[L12], U12: c.WeakTypeTag[U12], L13: c.WeakTypeTag[L13], U13: c.WeakTypeTag[U13], L14: c.WeakTypeTag[L14], U14: c.WeakTypeTag[U14], L15: c.WeakTypeTag[L15], U15: c.WeakTypeTag[U15], L16: c.WeakTypeTag[L16], U16: c.WeakTypeTag[U16], L17: c.WeakTypeTag[L17], U17: c.WeakTypeTag[U17], L18: c.WeakTypeTag[L18], U18: c.WeakTypeTag[U18], L19: c.WeakTypeTag[L19], U19: c.WeakTypeTag[U19], L20: c.WeakTypeTag[L20], U20: c.WeakTypeTag[U20], L21: c.WeakTypeTag[L21], U21: c.WeakTypeTag[U21], L22: c.WeakTypeTag[L22], U22: c.WeakTypeTag[U22], HKTE: c.WeakTypeTag[HKT[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]): c.Tree = typeCtor22Body[L1, U1, L2, U2, L3, U3, L4, U4, L5, U5, L6, U6, L7, U7, L8, U8, L9, U9, L10, U10, L11, U11, L12, U12, L13, U13, L14, U14, L15, U15, L16, U16, L17, U17, L18, U18, L19, U19, L20, U20, L21, U21, L22, U22, HKT]

  def typeCtor1FromUntypedImpl[L1, U1 >: L1, HKT[_ >: L1 <: U1]](untyped: c.Expr[Any])(implicit L1: c.WeakTypeTag[L1], U1: c.WeakTypeTag[U1], HKTE: c.WeakTypeTag[HKT[?]]): c.Tree = typeCtor1FromUntypedBody[L1, U1, HKT](untyped)
  def typeCtor2FromUntypedImpl[L1, U1 >: L1, L2, U2 >: L2, HKT[_ >: L1 <: U1, _ >: L2 <: U2]](untyped: c.Expr[Any])(implicit L1: c.WeakTypeTag[L1], U1: c.WeakTypeTag[U1], L2: c.WeakTypeTag[L2], U2: c.WeakTypeTag[U2], HKTE: c.WeakTypeTag[HKT[?, ?]]): c.Tree = typeCtor2FromUntypedBody[L1, U1, L2, U2, HKT](untyped)
  def typeCtor3FromUntypedImpl[L1, U1 >: L1, L2, U2 >: L2, L3, U3 >: L3, HKT[_ >: L1 <: U1, _ >: L2 <: U2, _ >: L3 <: U3]](untyped: c.Expr[Any])(implicit L1: c.WeakTypeTag[L1], U1: c.WeakTypeTag[U1], L2: c.WeakTypeTag[L2], U2: c.WeakTypeTag[U2], L3: c.WeakTypeTag[L3], U3: c.WeakTypeTag[U3], HKTE: c.WeakTypeTag[HKT[?, ?, ?]]): c.Tree = typeCtor3FromUntypedBody[L1, U1, L2, U2, L3, U3, HKT](untyped)
  def typeCtor4FromUntypedImpl[L1, U1 >: L1, L2, U2 >: L2, L3, U3 >: L3, L4, U4 >: L4, HKT[_ >: L1 <: U1, _ >: L2 <: U2, _ >: L3 <: U3, _ >: L4 <: U4]](untyped: c.Expr[Any])(implicit L1: c.WeakTypeTag[L1], U1: c.WeakTypeTag[U1], L2: c.WeakTypeTag[L2], U2: c.WeakTypeTag[U2], L3: c.WeakTypeTag[L3], U3: c.WeakTypeTag[U3], L4: c.WeakTypeTag[L4], U4: c.WeakTypeTag[U4], HKTE: c.WeakTypeTag[HKT[?, ?, ?, ?]]): c.Tree = typeCtor4FromUntypedBody[L1, U1, L2, U2, L3, U3, L4, U4, HKT](untyped)
  def typeCtor5FromUntypedImpl[L1, U1 >: L1, L2, U2 >: L2, L3, U3 >: L3, L4, U4 >: L4, L5, U5 >: L5, HKT[_ >: L1 <: U1, _ >: L2 <: U2, _ >: L3 <: U3, _ >: L4 <: U4, _ >: L5 <: U5]](untyped: c.Expr[Any])(implicit L1: c.WeakTypeTag[L1], U1: c.WeakTypeTag[U1], L2: c.WeakTypeTag[L2], U2: c.WeakTypeTag[U2], L3: c.WeakTypeTag[L3], U3: c.WeakTypeTag[U3], L4: c.WeakTypeTag[L4], U4: c.WeakTypeTag[U4], L5: c.WeakTypeTag[L5], U5: c.WeakTypeTag[U5], HKTE: c.WeakTypeTag[HKT[?, ?, ?, ?, ?]]): c.Tree = typeCtor5FromUntypedBody[L1, U1, L2, U2, L3, U3, L4, U4, L5, U5, HKT](untyped)
  def typeCtor6FromUntypedImpl[L1, U1 >: L1, L2, U2 >: L2, L3, U3 >: L3, L4, U4 >: L4, L5, U5 >: L5, L6, U6 >: L6, HKT[_ >: L1 <: U1, _ >: L2 <: U2, _ >: L3 <: U3, _ >: L4 <: U4, _ >: L5 <: U5, _ >: L6 <: U6]](untyped: c.Expr[Any])(implicit L1: c.WeakTypeTag[L1], U1: c.WeakTypeTag[U1], L2: c.WeakTypeTag[L2], U2: c.WeakTypeTag[U2], L3: c.WeakTypeTag[L3], U3: c.WeakTypeTag[U3], L4: c.WeakTypeTag[L4], U4: c.WeakTypeTag[U4], L5: c.WeakTypeTag[L5], U5: c.WeakTypeTag[U5], L6: c.WeakTypeTag[L6], U6: c.WeakTypeTag[U6], HKTE: c.WeakTypeTag[HKT[?, ?, ?, ?, ?, ?]]): c.Tree = typeCtor6FromUntypedBody[L1, U1, L2, U2, L3, U3, L4, U4, L5, U5, L6, U6, HKT](untyped)
  def typeCtor7FromUntypedImpl[L1, U1 >: L1, L2, U2 >: L2, L3, U3 >: L3, L4, U4 >: L4, L5, U5 >: L5, L6, U6 >: L6, L7, U7 >: L7, HKT[_ >: L1 <: U1, _ >: L2 <: U2, _ >: L3 <: U3, _ >: L4 <: U4, _ >: L5 <: U5, _ >: L6 <: U6, _ >: L7 <: U7]](untyped: c.Expr[Any])(implicit L1: c.WeakTypeTag[L1], U1: c.WeakTypeTag[U1], L2: c.WeakTypeTag[L2], U2: c.WeakTypeTag[U2], L3: c.WeakTypeTag[L3], U3: c.WeakTypeTag[U3], L4: c.WeakTypeTag[L4], U4: c.WeakTypeTag[U4], L5: c.WeakTypeTag[L5], U5: c.WeakTypeTag[U5], L6: c.WeakTypeTag[L6], U6: c.WeakTypeTag[U6], L7: c.WeakTypeTag[L7], U7: c.WeakTypeTag[U7], HKTE: c.WeakTypeTag[HKT[?, ?, ?, ?, ?, ?, ?]]): c.Tree = typeCtor7FromUntypedBody[L1, U1, L2, U2, L3, U3, L4, U4, L5, U5, L6, U6, L7, U7, HKT](untyped)
  def typeCtor8FromUntypedImpl[L1, U1 >: L1, L2, U2 >: L2, L3, U3 >: L3, L4, U4 >: L4, L5, U5 >: L5, L6, U6 >: L6, L7, U7 >: L7, L8, U8 >: L8, HKT[_ >: L1 <: U1, _ >: L2 <: U2, _ >: L3 <: U3, _ >: L4 <: U4, _ >: L5 <: U5, _ >: L6 <: U6, _ >: L7 <: U7, _ >: L8 <: U8]](untyped: c.Expr[Any])(implicit L1: c.WeakTypeTag[L1], U1: c.WeakTypeTag[U1], L2: c.WeakTypeTag[L2], U2: c.WeakTypeTag[U2], L3: c.WeakTypeTag[L3], U3: c.WeakTypeTag[U3], L4: c.WeakTypeTag[L4], U4: c.WeakTypeTag[U4], L5: c.WeakTypeTag[L5], U5: c.WeakTypeTag[U5], L6: c.WeakTypeTag[L6], U6: c.WeakTypeTag[U6], L7: c.WeakTypeTag[L7], U7: c.WeakTypeTag[U7], L8: c.WeakTypeTag[L8], U8: c.WeakTypeTag[U8], HKTE: c.WeakTypeTag[HKT[?, ?, ?, ?, ?, ?, ?, ?]]): c.Tree = typeCtor8FromUntypedBody[L1, U1, L2, U2, L3, U3, L4, U4, L5, U5, L6, U6, L7, U7, L8, U8, HKT](untyped)
  def typeCtor9FromUntypedImpl[L1, U1 >: L1, L2, U2 >: L2, L3, U3 >: L3, L4, U4 >: L4, L5, U5 >: L5, L6, U6 >: L6, L7, U7 >: L7, L8, U8 >: L8, L9, U9 >: L9, HKT[_ >: L1 <: U1, _ >: L2 <: U2, _ >: L3 <: U3, _ >: L4 <: U4, _ >: L5 <: U5, _ >: L6 <: U6, _ >: L7 <: U7, _ >: L8 <: U8, _ >: L9 <: U9]](untyped: c.Expr[Any])(implicit L1: c.WeakTypeTag[L1], U1: c.WeakTypeTag[U1], L2: c.WeakTypeTag[L2], U2: c.WeakTypeTag[U2], L3: c.WeakTypeTag[L3], U3: c.WeakTypeTag[U3], L4: c.WeakTypeTag[L4], U4: c.WeakTypeTag[U4], L5: c.WeakTypeTag[L5], U5: c.WeakTypeTag[U5], L6: c.WeakTypeTag[L6], U6: c.WeakTypeTag[U6], L7: c.WeakTypeTag[L7], U7: c.WeakTypeTag[U7], L8: c.WeakTypeTag[L8], U8: c.WeakTypeTag[U8], L9: c.WeakTypeTag[L9], U9: c.WeakTypeTag[U9], HKTE: c.WeakTypeTag[HKT[?, ?, ?, ?, ?, ?, ?, ?, ?]]): c.Tree = typeCtor9FromUntypedBody[L1, U1, L2, U2, L3, U3, L4, U4, L5, U5, L6, U6, L7, U7, L8, U8, L9, U9, HKT](untyped)
  def typeCtor10FromUntypedImpl[L1, U1 >: L1, L2, U2 >: L2, L3, U3 >: L3, L4, U4 >: L4, L5, U5 >: L5, L6, U6 >: L6, L7, U7 >: L7, L8, U8 >: L8, L9, U9 >: L9, L10, U10 >: L10, HKT[_ >: L1 <: U1, _ >: L2 <: U2, _ >: L3 <: U3, _ >: L4 <: U4, _ >: L5 <: U5, _ >: L6 <: U6, _ >: L7 <: U7, _ >: L8 <: U8, _ >: L9 <: U9, _ >: L10 <: U10]](untyped: c.Expr[Any])(implicit L1: c.WeakTypeTag[L1], U1: c.WeakTypeTag[U1], L2: c.WeakTypeTag[L2], U2: c.WeakTypeTag[U2], L3: c.WeakTypeTag[L3], U3: c.WeakTypeTag[U3], L4: c.WeakTypeTag[L4], U4: c.WeakTypeTag[U4], L5: c.WeakTypeTag[L5], U5: c.WeakTypeTag[U5], L6: c.WeakTypeTag[L6], U6: c.WeakTypeTag[U6], L7: c.WeakTypeTag[L7], U7: c.WeakTypeTag[U7], L8: c.WeakTypeTag[L8], U8: c.WeakTypeTag[U8], L9: c.WeakTypeTag[L9], U9: c.WeakTypeTag[U9], L10: c.WeakTypeTag[L10], U10: c.WeakTypeTag[U10], HKTE: c.WeakTypeTag[HKT[?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]): c.Tree = typeCtor10FromUntypedBody[L1, U1, L2, U2, L3, U3, L4, U4, L5, U5, L6, U6, L7, U7, L8, U8, L9, U9, L10, U10, HKT](untyped)
  def typeCtor11FromUntypedImpl[L1, U1 >: L1, L2, U2 >: L2, L3, U3 >: L3, L4, U4 >: L4, L5, U5 >: L5, L6, U6 >: L6, L7, U7 >: L7, L8, U8 >: L8, L9, U9 >: L9, L10, U10 >: L10, L11, U11 >: L11, HKT[_ >: L1 <: U1, _ >: L2 <: U2, _ >: L3 <: U3, _ >: L4 <: U4, _ >: L5 <: U5, _ >: L6 <: U6, _ >: L7 <: U7, _ >: L8 <: U8, _ >: L9 <: U9, _ >: L10 <: U10, _ >: L11 <: U11]](untyped: c.Expr[Any])(implicit L1: c.WeakTypeTag[L1], U1: c.WeakTypeTag[U1], L2: c.WeakTypeTag[L2], U2: c.WeakTypeTag[U2], L3: c.WeakTypeTag[L3], U3: c.WeakTypeTag[U3], L4: c.WeakTypeTag[L4], U4: c.WeakTypeTag[U4], L5: c.WeakTypeTag[L5], U5: c.WeakTypeTag[U5], L6: c.WeakTypeTag[L6], U6: c.WeakTypeTag[U6], L7: c.WeakTypeTag[L7], U7: c.WeakTypeTag[U7], L8: c.WeakTypeTag[L8], U8: c.WeakTypeTag[U8], L9: c.WeakTypeTag[L9], U9: c.WeakTypeTag[U9], L10: c.WeakTypeTag[L10], U10: c.WeakTypeTag[U10], L11: c.WeakTypeTag[L11], U11: c.WeakTypeTag[U11], HKTE: c.WeakTypeTag[HKT[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]): c.Tree = typeCtor11FromUntypedBody[L1, U1, L2, U2, L3, U3, L4, U4, L5, U5, L6, U6, L7, U7, L8, U8, L9, U9, L10, U10, L11, U11, HKT](untyped)
  def typeCtor12FromUntypedImpl[L1, U1 >: L1, L2, U2 >: L2, L3, U3 >: L3, L4, U4 >: L4, L5, U5 >: L5, L6, U6 >: L6, L7, U7 >: L7, L8, U8 >: L8, L9, U9 >: L9, L10, U10 >: L10, L11, U11 >: L11, L12, U12 >: L12, HKT[_ >: L1 <: U1, _ >: L2 <: U2, _ >: L3 <: U3, _ >: L4 <: U4, _ >: L5 <: U5, _ >: L6 <: U6, _ >: L7 <: U7, _ >: L8 <: U8, _ >: L9 <: U9, _ >: L10 <: U10, _ >: L11 <: U11, _ >: L12 <: U12]](untyped: c.Expr[Any])(implicit L1: c.WeakTypeTag[L1], U1: c.WeakTypeTag[U1], L2: c.WeakTypeTag[L2], U2: c.WeakTypeTag[U2], L3: c.WeakTypeTag[L3], U3: c.WeakTypeTag[U3], L4: c.WeakTypeTag[L4], U4: c.WeakTypeTag[U4], L5: c.WeakTypeTag[L5], U5: c.WeakTypeTag[U5], L6: c.WeakTypeTag[L6], U6: c.WeakTypeTag[U6], L7: c.WeakTypeTag[L7], U7: c.WeakTypeTag[U7], L8: c.WeakTypeTag[L8], U8: c.WeakTypeTag[U8], L9: c.WeakTypeTag[L9], U9: c.WeakTypeTag[U9], L10: c.WeakTypeTag[L10], U10: c.WeakTypeTag[U10], L11: c.WeakTypeTag[L11], U11: c.WeakTypeTag[U11], L12: c.WeakTypeTag[L12], U12: c.WeakTypeTag[U12], HKTE: c.WeakTypeTag[HKT[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]): c.Tree = typeCtor12FromUntypedBody[L1, U1, L2, U2, L3, U3, L4, U4, L5, U5, L6, U6, L7, U7, L8, U8, L9, U9, L10, U10, L11, U11, L12, U12, HKT](untyped)
  def typeCtor13FromUntypedImpl[L1, U1 >: L1, L2, U2 >: L2, L3, U3 >: L3, L4, U4 >: L4, L5, U5 >: L5, L6, U6 >: L6, L7, U7 >: L7, L8, U8 >: L8, L9, U9 >: L9, L10, U10 >: L10, L11, U11 >: L11, L12, U12 >: L12, L13, U13 >: L13, HKT[_ >: L1 <: U1, _ >: L2 <: U2, _ >: L3 <: U3, _ >: L4 <: U4, _ >: L5 <: U5, _ >: L6 <: U6, _ >: L7 <: U7, _ >: L8 <: U8, _ >: L9 <: U9, _ >: L10 <: U10, _ >: L11 <: U11, _ >: L12 <: U12, _ >: L13 <: U13]](untyped: c.Expr[Any])(implicit L1: c.WeakTypeTag[L1], U1: c.WeakTypeTag[U1], L2: c.WeakTypeTag[L2], U2: c.WeakTypeTag[U2], L3: c.WeakTypeTag[L3], U3: c.WeakTypeTag[U3], L4: c.WeakTypeTag[L4], U4: c.WeakTypeTag[U4], L5: c.WeakTypeTag[L5], U5: c.WeakTypeTag[U5], L6: c.WeakTypeTag[L6], U6: c.WeakTypeTag[U6], L7: c.WeakTypeTag[L7], U7: c.WeakTypeTag[U7], L8: c.WeakTypeTag[L8], U8: c.WeakTypeTag[U8], L9: c.WeakTypeTag[L9], U9: c.WeakTypeTag[U9], L10: c.WeakTypeTag[L10], U10: c.WeakTypeTag[U10], L11: c.WeakTypeTag[L11], U11: c.WeakTypeTag[U11], L12: c.WeakTypeTag[L12], U12: c.WeakTypeTag[U12], L13: c.WeakTypeTag[L13], U13: c.WeakTypeTag[U13], HKTE: c.WeakTypeTag[HKT[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]): c.Tree = typeCtor13FromUntypedBody[L1, U1, L2, U2, L3, U3, L4, U4, L5, U5, L6, U6, L7, U7, L8, U8, L9, U9, L10, U10, L11, U11, L12, U12, L13, U13, HKT](untyped)
  def typeCtor14FromUntypedImpl[L1, U1 >: L1, L2, U2 >: L2, L3, U3 >: L3, L4, U4 >: L4, L5, U5 >: L5, L6, U6 >: L6, L7, U7 >: L7, L8, U8 >: L8, L9, U9 >: L9, L10, U10 >: L10, L11, U11 >: L11, L12, U12 >: L12, L13, U13 >: L13, L14, U14 >: L14, HKT[_ >: L1 <: U1, _ >: L2 <: U2, _ >: L3 <: U3, _ >: L4 <: U4, _ >: L5 <: U5, _ >: L6 <: U6, _ >: L7 <: U7, _ >: L8 <: U8, _ >: L9 <: U9, _ >: L10 <: U10, _ >: L11 <: U11, _ >: L12 <: U12, _ >: L13 <: U13, _ >: L14 <: U14]](untyped: c.Expr[Any])(implicit L1: c.WeakTypeTag[L1], U1: c.WeakTypeTag[U1], L2: c.WeakTypeTag[L2], U2: c.WeakTypeTag[U2], L3: c.WeakTypeTag[L3], U3: c.WeakTypeTag[U3], L4: c.WeakTypeTag[L4], U4: c.WeakTypeTag[U4], L5: c.WeakTypeTag[L5], U5: c.WeakTypeTag[U5], L6: c.WeakTypeTag[L6], U6: c.WeakTypeTag[U6], L7: c.WeakTypeTag[L7], U7: c.WeakTypeTag[U7], L8: c.WeakTypeTag[L8], U8: c.WeakTypeTag[U8], L9: c.WeakTypeTag[L9], U9: c.WeakTypeTag[U9], L10: c.WeakTypeTag[L10], U10: c.WeakTypeTag[U10], L11: c.WeakTypeTag[L11], U11: c.WeakTypeTag[U11], L12: c.WeakTypeTag[L12], U12: c.WeakTypeTag[U12], L13: c.WeakTypeTag[L13], U13: c.WeakTypeTag[U13], L14: c.WeakTypeTag[L14], U14: c.WeakTypeTag[U14], HKTE: c.WeakTypeTag[HKT[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]): c.Tree = typeCtor14FromUntypedBody[L1, U1, L2, U2, L3, U3, L4, U4, L5, U5, L6, U6, L7, U7, L8, U8, L9, U9, L10, U10, L11, U11, L12, U12, L13, U13, L14, U14, HKT](untyped)
  def typeCtor15FromUntypedImpl[L1, U1 >: L1, L2, U2 >: L2, L3, U3 >: L3, L4, U4 >: L4, L5, U5 >: L5, L6, U6 >: L6, L7, U7 >: L7, L8, U8 >: L8, L9, U9 >: L9, L10, U10 >: L10, L11, U11 >: L11, L12, U12 >: L12, L13, U13 >: L13, L14, U14 >: L14, L15, U15 >: L15, HKT[_ >: L1 <: U1, _ >: L2 <: U2, _ >: L3 <: U3, _ >: L4 <: U4, _ >: L5 <: U5, _ >: L6 <: U6, _ >: L7 <: U7, _ >: L8 <: U8, _ >: L9 <: U9, _ >: L10 <: U10, _ >: L11 <: U11, _ >: L12 <: U12, _ >: L13 <: U13, _ >: L14 <: U14, _ >: L15 <: U15]](untyped: c.Expr[Any])(implicit L1: c.WeakTypeTag[L1], U1: c.WeakTypeTag[U1], L2: c.WeakTypeTag[L2], U2: c.WeakTypeTag[U2], L3: c.WeakTypeTag[L3], U3: c.WeakTypeTag[U3], L4: c.WeakTypeTag[L4], U4: c.WeakTypeTag[U4], L5: c.WeakTypeTag[L5], U5: c.WeakTypeTag[U5], L6: c.WeakTypeTag[L6], U6: c.WeakTypeTag[U6], L7: c.WeakTypeTag[L7], U7: c.WeakTypeTag[U7], L8: c.WeakTypeTag[L8], U8: c.WeakTypeTag[U8], L9: c.WeakTypeTag[L9], U9: c.WeakTypeTag[U9], L10: c.WeakTypeTag[L10], U10: c.WeakTypeTag[U10], L11: c.WeakTypeTag[L11], U11: c.WeakTypeTag[U11], L12: c.WeakTypeTag[L12], U12: c.WeakTypeTag[U12], L13: c.WeakTypeTag[L13], U13: c.WeakTypeTag[U13], L14: c.WeakTypeTag[L14], U14: c.WeakTypeTag[U14], L15: c.WeakTypeTag[L15], U15: c.WeakTypeTag[U15], HKTE: c.WeakTypeTag[HKT[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]): c.Tree = typeCtor15FromUntypedBody[L1, U1, L2, U2, L3, U3, L4, U4, L5, U5, L6, U6, L7, U7, L8, U8, L9, U9, L10, U10, L11, U11, L12, U12, L13, U13, L14, U14, L15, U15, HKT](untyped)
  def typeCtor16FromUntypedImpl[L1, U1 >: L1, L2, U2 >: L2, L3, U3 >: L3, L4, U4 >: L4, L5, U5 >: L5, L6, U6 >: L6, L7, U7 >: L7, L8, U8 >: L8, L9, U9 >: L9, L10, U10 >: L10, L11, U11 >: L11, L12, U12 >: L12, L13, U13 >: L13, L14, U14 >: L14, L15, U15 >: L15, L16, U16 >: L16, HKT[_ >: L1 <: U1, _ >: L2 <: U2, _ >: L3 <: U3, _ >: L4 <: U4, _ >: L5 <: U5, _ >: L6 <: U6, _ >: L7 <: U7, _ >: L8 <: U8, _ >: L9 <: U9, _ >: L10 <: U10, _ >: L11 <: U11, _ >: L12 <: U12, _ >: L13 <: U13, _ >: L14 <: U14, _ >: L15 <: U15, _ >: L16 <: U16]](untyped: c.Expr[Any])(implicit L1: c.WeakTypeTag[L1], U1: c.WeakTypeTag[U1], L2: c.WeakTypeTag[L2], U2: c.WeakTypeTag[U2], L3: c.WeakTypeTag[L3], U3: c.WeakTypeTag[U3], L4: c.WeakTypeTag[L4], U4: c.WeakTypeTag[U4], L5: c.WeakTypeTag[L5], U5: c.WeakTypeTag[U5], L6: c.WeakTypeTag[L6], U6: c.WeakTypeTag[U6], L7: c.WeakTypeTag[L7], U7: c.WeakTypeTag[U7], L8: c.WeakTypeTag[L8], U8: c.WeakTypeTag[U8], L9: c.WeakTypeTag[L9], U9: c.WeakTypeTag[U9], L10: c.WeakTypeTag[L10], U10: c.WeakTypeTag[U10], L11: c.WeakTypeTag[L11], U11: c.WeakTypeTag[U11], L12: c.WeakTypeTag[L12], U12: c.WeakTypeTag[U12], L13: c.WeakTypeTag[L13], U13: c.WeakTypeTag[U13], L14: c.WeakTypeTag[L14], U14: c.WeakTypeTag[U14], L15: c.WeakTypeTag[L15], U15: c.WeakTypeTag[U15], L16: c.WeakTypeTag[L16], U16: c.WeakTypeTag[U16], HKTE: c.WeakTypeTag[HKT[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]): c.Tree = typeCtor16FromUntypedBody[L1, U1, L2, U2, L3, U3, L4, U4, L5, U5, L6, U6, L7, U7, L8, U8, L9, U9, L10, U10, L11, U11, L12, U12, L13, U13, L14, U14, L15, U15, L16, U16, HKT](untyped)
  def typeCtor17FromUntypedImpl[L1, U1 >: L1, L2, U2 >: L2, L3, U3 >: L3, L4, U4 >: L4, L5, U5 >: L5, L6, U6 >: L6, L7, U7 >: L7, L8, U8 >: L8, L9, U9 >: L9, L10, U10 >: L10, L11, U11 >: L11, L12, U12 >: L12, L13, U13 >: L13, L14, U14 >: L14, L15, U15 >: L15, L16, U16 >: L16, L17, U17 >: L17, HKT[_ >: L1 <: U1, _ >: L2 <: U2, _ >: L3 <: U3, _ >: L4 <: U4, _ >: L5 <: U5, _ >: L6 <: U6, _ >: L7 <: U7, _ >: L8 <: U8, _ >: L9 <: U9, _ >: L10 <: U10, _ >: L11 <: U11, _ >: L12 <: U12, _ >: L13 <: U13, _ >: L14 <: U14, _ >: L15 <: U15, _ >: L16 <: U16, _ >: L17 <: U17]](untyped: c.Expr[Any])(implicit L1: c.WeakTypeTag[L1], U1: c.WeakTypeTag[U1], L2: c.WeakTypeTag[L2], U2: c.WeakTypeTag[U2], L3: c.WeakTypeTag[L3], U3: c.WeakTypeTag[U3], L4: c.WeakTypeTag[L4], U4: c.WeakTypeTag[U4], L5: c.WeakTypeTag[L5], U5: c.WeakTypeTag[U5], L6: c.WeakTypeTag[L6], U6: c.WeakTypeTag[U6], L7: c.WeakTypeTag[L7], U7: c.WeakTypeTag[U7], L8: c.WeakTypeTag[L8], U8: c.WeakTypeTag[U8], L9: c.WeakTypeTag[L9], U9: c.WeakTypeTag[U9], L10: c.WeakTypeTag[L10], U10: c.WeakTypeTag[U10], L11: c.WeakTypeTag[L11], U11: c.WeakTypeTag[U11], L12: c.WeakTypeTag[L12], U12: c.WeakTypeTag[U12], L13: c.WeakTypeTag[L13], U13: c.WeakTypeTag[U13], L14: c.WeakTypeTag[L14], U14: c.WeakTypeTag[U14], L15: c.WeakTypeTag[L15], U15: c.WeakTypeTag[U15], L16: c.WeakTypeTag[L16], U16: c.WeakTypeTag[U16], L17: c.WeakTypeTag[L17], U17: c.WeakTypeTag[U17], HKTE: c.WeakTypeTag[HKT[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]): c.Tree = typeCtor17FromUntypedBody[L1, U1, L2, U2, L3, U3, L4, U4, L5, U5, L6, U6, L7, U7, L8, U8, L9, U9, L10, U10, L11, U11, L12, U12, L13, U13, L14, U14, L15, U15, L16, U16, L17, U17, HKT](untyped)
  def typeCtor18FromUntypedImpl[L1, U1 >: L1, L2, U2 >: L2, L3, U3 >: L3, L4, U4 >: L4, L5, U5 >: L5, L6, U6 >: L6, L7, U7 >: L7, L8, U8 >: L8, L9, U9 >: L9, L10, U10 >: L10, L11, U11 >: L11, L12, U12 >: L12, L13, U13 >: L13, L14, U14 >: L14, L15, U15 >: L15, L16, U16 >: L16, L17, U17 >: L17, L18, U18 >: L18, HKT[_ >: L1 <: U1, _ >: L2 <: U2, _ >: L3 <: U3, _ >: L4 <: U4, _ >: L5 <: U5, _ >: L6 <: U6, _ >: L7 <: U7, _ >: L8 <: U8, _ >: L9 <: U9, _ >: L10 <: U10, _ >: L11 <: U11, _ >: L12 <: U12, _ >: L13 <: U13, _ >: L14 <: U14, _ >: L15 <: U15, _ >: L16 <: U16, _ >: L17 <: U17, _ >: L18 <: U18]](untyped: c.Expr[Any])(implicit L1: c.WeakTypeTag[L1], U1: c.WeakTypeTag[U1], L2: c.WeakTypeTag[L2], U2: c.WeakTypeTag[U2], L3: c.WeakTypeTag[L3], U3: c.WeakTypeTag[U3], L4: c.WeakTypeTag[L4], U4: c.WeakTypeTag[U4], L5: c.WeakTypeTag[L5], U5: c.WeakTypeTag[U5], L6: c.WeakTypeTag[L6], U6: c.WeakTypeTag[U6], L7: c.WeakTypeTag[L7], U7: c.WeakTypeTag[U7], L8: c.WeakTypeTag[L8], U8: c.WeakTypeTag[U8], L9: c.WeakTypeTag[L9], U9: c.WeakTypeTag[U9], L10: c.WeakTypeTag[L10], U10: c.WeakTypeTag[U10], L11: c.WeakTypeTag[L11], U11: c.WeakTypeTag[U11], L12: c.WeakTypeTag[L12], U12: c.WeakTypeTag[U12], L13: c.WeakTypeTag[L13], U13: c.WeakTypeTag[U13], L14: c.WeakTypeTag[L14], U14: c.WeakTypeTag[U14], L15: c.WeakTypeTag[L15], U15: c.WeakTypeTag[U15], L16: c.WeakTypeTag[L16], U16: c.WeakTypeTag[U16], L17: c.WeakTypeTag[L17], U17: c.WeakTypeTag[U17], L18: c.WeakTypeTag[L18], U18: c.WeakTypeTag[U18], HKTE: c.WeakTypeTag[HKT[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]): c.Tree = typeCtor18FromUntypedBody[L1, U1, L2, U2, L3, U3, L4, U4, L5, U5, L6, U6, L7, U7, L8, U8, L9, U9, L10, U10, L11, U11, L12, U12, L13, U13, L14, U14, L15, U15, L16, U16, L17, U17, L18, U18, HKT](untyped)
  def typeCtor19FromUntypedImpl[L1, U1 >: L1, L2, U2 >: L2, L3, U3 >: L3, L4, U4 >: L4, L5, U5 >: L5, L6, U6 >: L6, L7, U7 >: L7, L8, U8 >: L8, L9, U9 >: L9, L10, U10 >: L10, L11, U11 >: L11, L12, U12 >: L12, L13, U13 >: L13, L14, U14 >: L14, L15, U15 >: L15, L16, U16 >: L16, L17, U17 >: L17, L18, U18 >: L18, L19, U19 >: L19, HKT[_ >: L1 <: U1, _ >: L2 <: U2, _ >: L3 <: U3, _ >: L4 <: U4, _ >: L5 <: U5, _ >: L6 <: U6, _ >: L7 <: U7, _ >: L8 <: U8, _ >: L9 <: U9, _ >: L10 <: U10, _ >: L11 <: U11, _ >: L12 <: U12, _ >: L13 <: U13, _ >: L14 <: U14, _ >: L15 <: U15, _ >: L16 <: U16, _ >: L17 <: U17, _ >: L18 <: U18, _ >: L19 <: U19]](untyped: c.Expr[Any])(implicit L1: c.WeakTypeTag[L1], U1: c.WeakTypeTag[U1], L2: c.WeakTypeTag[L2], U2: c.WeakTypeTag[U2], L3: c.WeakTypeTag[L3], U3: c.WeakTypeTag[U3], L4: c.WeakTypeTag[L4], U4: c.WeakTypeTag[U4], L5: c.WeakTypeTag[L5], U5: c.WeakTypeTag[U5], L6: c.WeakTypeTag[L6], U6: c.WeakTypeTag[U6], L7: c.WeakTypeTag[L7], U7: c.WeakTypeTag[U7], L8: c.WeakTypeTag[L8], U8: c.WeakTypeTag[U8], L9: c.WeakTypeTag[L9], U9: c.WeakTypeTag[U9], L10: c.WeakTypeTag[L10], U10: c.WeakTypeTag[U10], L11: c.WeakTypeTag[L11], U11: c.WeakTypeTag[U11], L12: c.WeakTypeTag[L12], U12: c.WeakTypeTag[U12], L13: c.WeakTypeTag[L13], U13: c.WeakTypeTag[U13], L14: c.WeakTypeTag[L14], U14: c.WeakTypeTag[U14], L15: c.WeakTypeTag[L15], U15: c.WeakTypeTag[U15], L16: c.WeakTypeTag[L16], U16: c.WeakTypeTag[U16], L17: c.WeakTypeTag[L17], U17: c.WeakTypeTag[U17], L18: c.WeakTypeTag[L18], U18: c.WeakTypeTag[U18], L19: c.WeakTypeTag[L19], U19: c.WeakTypeTag[U19], HKTE: c.WeakTypeTag[HKT[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]): c.Tree = typeCtor19FromUntypedBody[L1, U1, L2, U2, L3, U3, L4, U4, L5, U5, L6, U6, L7, U7, L8, U8, L9, U9, L10, U10, L11, U11, L12, U12, L13, U13, L14, U14, L15, U15, L16, U16, L17, U17, L18, U18, L19, U19, HKT](untyped)
  def typeCtor20FromUntypedImpl[L1, U1 >: L1, L2, U2 >: L2, L3, U3 >: L3, L4, U4 >: L4, L5, U5 >: L5, L6, U6 >: L6, L7, U7 >: L7, L8, U8 >: L8, L9, U9 >: L9, L10, U10 >: L10, L11, U11 >: L11, L12, U12 >: L12, L13, U13 >: L13, L14, U14 >: L14, L15, U15 >: L15, L16, U16 >: L16, L17, U17 >: L17, L18, U18 >: L18, L19, U19 >: L19, L20, U20 >: L20, HKT[_ >: L1 <: U1, _ >: L2 <: U2, _ >: L3 <: U3, _ >: L4 <: U4, _ >: L5 <: U5, _ >: L6 <: U6, _ >: L7 <: U7, _ >: L8 <: U8, _ >: L9 <: U9, _ >: L10 <: U10, _ >: L11 <: U11, _ >: L12 <: U12, _ >: L13 <: U13, _ >: L14 <: U14, _ >: L15 <: U15, _ >: L16 <: U16, _ >: L17 <: U17, _ >: L18 <: U18, _ >: L19 <: U19, _ >: L20 <: U20]](untyped: c.Expr[Any])(implicit L1: c.WeakTypeTag[L1], U1: c.WeakTypeTag[U1], L2: c.WeakTypeTag[L2], U2: c.WeakTypeTag[U2], L3: c.WeakTypeTag[L3], U3: c.WeakTypeTag[U3], L4: c.WeakTypeTag[L4], U4: c.WeakTypeTag[U4], L5: c.WeakTypeTag[L5], U5: c.WeakTypeTag[U5], L6: c.WeakTypeTag[L6], U6: c.WeakTypeTag[U6], L7: c.WeakTypeTag[L7], U7: c.WeakTypeTag[U7], L8: c.WeakTypeTag[L8], U8: c.WeakTypeTag[U8], L9: c.WeakTypeTag[L9], U9: c.WeakTypeTag[U9], L10: c.WeakTypeTag[L10], U10: c.WeakTypeTag[U10], L11: c.WeakTypeTag[L11], U11: c.WeakTypeTag[U11], L12: c.WeakTypeTag[L12], U12: c.WeakTypeTag[U12], L13: c.WeakTypeTag[L13], U13: c.WeakTypeTag[U13], L14: c.WeakTypeTag[L14], U14: c.WeakTypeTag[U14], L15: c.WeakTypeTag[L15], U15: c.WeakTypeTag[U15], L16: c.WeakTypeTag[L16], U16: c.WeakTypeTag[U16], L17: c.WeakTypeTag[L17], U17: c.WeakTypeTag[U17], L18: c.WeakTypeTag[L18], U18: c.WeakTypeTag[U18], L19: c.WeakTypeTag[L19], U19: c.WeakTypeTag[U19], L20: c.WeakTypeTag[L20], U20: c.WeakTypeTag[U20], HKTE: c.WeakTypeTag[HKT[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]): c.Tree = typeCtor20FromUntypedBody[L1, U1, L2, U2, L3, U3, L4, U4, L5, U5, L6, U6, L7, U7, L8, U8, L9, U9, L10, U10, L11, U11, L12, U12, L13, U13, L14, U14, L15, U15, L16, U16, L17, U17, L18, U18, L19, U19, L20, U20, HKT](untyped)
  def typeCtor21FromUntypedImpl[L1, U1 >: L1, L2, U2 >: L2, L3, U3 >: L3, L4, U4 >: L4, L5, U5 >: L5, L6, U6 >: L6, L7, U7 >: L7, L8, U8 >: L8, L9, U9 >: L9, L10, U10 >: L10, L11, U11 >: L11, L12, U12 >: L12, L13, U13 >: L13, L14, U14 >: L14, L15, U15 >: L15, L16, U16 >: L16, L17, U17 >: L17, L18, U18 >: L18, L19, U19 >: L19, L20, U20 >: L20, L21, U21 >: L21, HKT[_ >: L1 <: U1, _ >: L2 <: U2, _ >: L3 <: U3, _ >: L4 <: U4, _ >: L5 <: U5, _ >: L6 <: U6, _ >: L7 <: U7, _ >: L8 <: U8, _ >: L9 <: U9, _ >: L10 <: U10, _ >: L11 <: U11, _ >: L12 <: U12, _ >: L13 <: U13, _ >: L14 <: U14, _ >: L15 <: U15, _ >: L16 <: U16, _ >: L17 <: U17, _ >: L18 <: U18, _ >: L19 <: U19, _ >: L20 <: U20, _ >: L21 <: U21]](untyped: c.Expr[Any])(implicit L1: c.WeakTypeTag[L1], U1: c.WeakTypeTag[U1], L2: c.WeakTypeTag[L2], U2: c.WeakTypeTag[U2], L3: c.WeakTypeTag[L3], U3: c.WeakTypeTag[U3], L4: c.WeakTypeTag[L4], U4: c.WeakTypeTag[U4], L5: c.WeakTypeTag[L5], U5: c.WeakTypeTag[U5], L6: c.WeakTypeTag[L6], U6: c.WeakTypeTag[U6], L7: c.WeakTypeTag[L7], U7: c.WeakTypeTag[U7], L8: c.WeakTypeTag[L8], U8: c.WeakTypeTag[U8], L9: c.WeakTypeTag[L9], U9: c.WeakTypeTag[U9], L10: c.WeakTypeTag[L10], U10: c.WeakTypeTag[U10], L11: c.WeakTypeTag[L11], U11: c.WeakTypeTag[U11], L12: c.WeakTypeTag[L12], U12: c.WeakTypeTag[U12], L13: c.WeakTypeTag[L13], U13: c.WeakTypeTag[U13], L14: c.WeakTypeTag[L14], U14: c.WeakTypeTag[U14], L15: c.WeakTypeTag[L15], U15: c.WeakTypeTag[U15], L16: c.WeakTypeTag[L16], U16: c.WeakTypeTag[U16], L17: c.WeakTypeTag[L17], U17: c.WeakTypeTag[U17], L18: c.WeakTypeTag[L18], U18: c.WeakTypeTag[U18], L19: c.WeakTypeTag[L19], U19: c.WeakTypeTag[U19], L20: c.WeakTypeTag[L20], U20: c.WeakTypeTag[U20], L21: c.WeakTypeTag[L21], U21: c.WeakTypeTag[U21], HKTE: c.WeakTypeTag[HKT[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]): c.Tree = typeCtor21FromUntypedBody[L1, U1, L2, U2, L3, U3, L4, U4, L5, U5, L6, U6, L7, U7, L8, U8, L9, U9, L10, U10, L11, U11, L12, U12, L13, U13, L14, U14, L15, U15, L16, U16, L17, U17, L18, U18, L19, U19, L20, U20, L21, U21, HKT](untyped)
  def typeCtor22FromUntypedImpl[L1, U1 >: L1, L2, U2 >: L2, L3, U3 >: L3, L4, U4 >: L4, L5, U5 >: L5, L6, U6 >: L6, L7, U7 >: L7, L8, U8 >: L8, L9, U9 >: L9, L10, U10 >: L10, L11, U11 >: L11, L12, U12 >: L12, L13, U13 >: L13, L14, U14 >: L14, L15, U15 >: L15, L16, U16 >: L16, L17, U17 >: L17, L18, U18 >: L18, L19, U19 >: L19, L20, U20 >: L20, L21, U21 >: L21, L22, U22 >: L22, HKT[_ >: L1 <: U1, _ >: L2 <: U2, _ >: L3 <: U3, _ >: L4 <: U4, _ >: L5 <: U5, _ >: L6 <: U6, _ >: L7 <: U7, _ >: L8 <: U8, _ >: L9 <: U9, _ >: L10 <: U10, _ >: L11 <: U11, _ >: L12 <: U12, _ >: L13 <: U13, _ >: L14 <: U14, _ >: L15 <: U15, _ >: L16 <: U16, _ >: L17 <: U17, _ >: L18 <: U18, _ >: L19 <: U19, _ >: L20 <: U20, _ >: L21 <: U21, _ >: L22 <: U22]](untyped: c.Expr[Any])(implicit L1: c.WeakTypeTag[L1], U1: c.WeakTypeTag[U1], L2: c.WeakTypeTag[L2], U2: c.WeakTypeTag[U2], L3: c.WeakTypeTag[L3], U3: c.WeakTypeTag[U3], L4: c.WeakTypeTag[L4], U4: c.WeakTypeTag[U4], L5: c.WeakTypeTag[L5], U5: c.WeakTypeTag[U5], L6: c.WeakTypeTag[L6], U6: c.WeakTypeTag[U6], L7: c.WeakTypeTag[L7], U7: c.WeakTypeTag[U7], L8: c.WeakTypeTag[L8], U8: c.WeakTypeTag[U8], L9: c.WeakTypeTag[L9], U9: c.WeakTypeTag[U9], L10: c.WeakTypeTag[L10], U10: c.WeakTypeTag[U10], L11: c.WeakTypeTag[L11], U11: c.WeakTypeTag[U11], L12: c.WeakTypeTag[L12], U12: c.WeakTypeTag[U12], L13: c.WeakTypeTag[L13], U13: c.WeakTypeTag[U13], L14: c.WeakTypeTag[L14], U14: c.WeakTypeTag[U14], L15: c.WeakTypeTag[L15], U15: c.WeakTypeTag[U15], L16: c.WeakTypeTag[L16], U16: c.WeakTypeTag[U16], L17: c.WeakTypeTag[L17], U17: c.WeakTypeTag[U17], L18: c.WeakTypeTag[L18], U18: c.WeakTypeTag[U18], L19: c.WeakTypeTag[L19], U19: c.WeakTypeTag[U19], L20: c.WeakTypeTag[L20], U20: c.WeakTypeTag[U20], L21: c.WeakTypeTag[L21], U21: c.WeakTypeTag[U21], L22: c.WeakTypeTag[L22], U22: c.WeakTypeTag[U22], HKTE: c.WeakTypeTag[HKT[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]): c.Tree = typeCtor22FromUntypedBody[L1, U1, L2, U2, L3, U3, L4, U4, L5, U5, L6, U6, L7, U7, L8, U8, L9, U9, L10, U10, L11, U11, L12, U12, L13, U13, L14, U14, L15, U15, L16, U16, L17, U17, L18, U18, L19, U19, L20, U20, L21, U21, L22, U22, HKT](untyped)
  // format: on

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
            println(s"""Created Type[$A] with: ${pp(resolved)}""")
          }
          resolved
      }
    val quasiquote = convert(ctx)(expr.tree)

    // TODO: generate freshTerm for quasiquotes and then {Quasiquote => $FreshTerm}
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

    // purposefully not typechecking, because it would fail with: unexpected error: Position.point on NoPosition
    val result = suppressWarnings(unchecked)

    log(
      s"""Cross-quotes ${paintExclDot(Console.BLUE)("Expr.quote")} expansion:
         |From: ${paintExclDot(Console.BLUE)("Expr.quote")}[${pp(weakTypeOf[A])}](${pp(expr)})
         |To: ${pp(result)}""".stripMargin
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
  protected def TypeWithUnderlyingInjected(ctx: TermName, weakTypeTagOf: c.Tree, excluding: TypeName*): c.Tree = {
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
      import $ctx2.universe.{ TypeRef, TypeRefTag }
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

  /** Collects implicit vals with ImportedCrossTypeImplicit names from the enclosing class (issue #168).
    *
    * These are vals like `implicit val Key: Type[Key]` defined in the same class/trait as the Expr.quote call. Unlike
    * `importedUnderlyingTypes`, these are NOT fed into `TypeWithUnderlyingInjected` (which handles `Type.of[A]`
    * resolution) because same-class type members with `ThisType` prefixes cause issues there. Instead, these are only
    * used by `ImplicitTypeReferenceReplacer` for type substitution in `Expr.quote`.
    */
  private def sameClassImplicitTypes: List[(Type, String)] = try {
    val result = scala.collection.mutable.ListBuffer[(Type, String)]()

    @scala.annotation.nowarn // No replacement and it's not going anywhere
    val enclosingClass = c.enclosingClass
    if (enclosingClass != EmptyTree) {
      object implicitValFinder extends Traverser {
        override def traverse(tree: Tree): Unit = tree match {
          case vd: ValDef
              if vd.mods.hasFlag(Flag.IMPLICIT) &&
                vd.name.isImportedCrossTypeImplicit &&
                vd.symbol != null =>
            // Extract the inner type from Type[X] - the symbol's info is Type[X], we want X.
            // symbol.info is e.g. TypeRef(_, Type, List(TypeRef(ThisType($anon), Key, List())))
            scala.util
              .Try {
                vd.symbol.info match {
                  case TypeRef(_, _, List(innerType)) => innerType
                }
              }
              .toOption
              .foreach { tpeToReplace =>
                result += (tpeToReplace -> vd.name.decodedName.toString)
              }
          case _ => super.traverse(tree)
        }
      }
      implicitValFinder.traverse(enclosingClass)
    }

    result.toList
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
               |${indent(pp(source))}
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
                     |${indent(pp(tree))}
                     |""".stripMargin)
        }
      }
  }

  /** When we replace stubs, we should be careful to e.g. not replace Something$1 before Something$10, or we will end up
    * with a broken code. To avoid this, we sort stubs in descending order of their suffixes.
    */
  private val stubSuffixDescending: Ordering[String] = Ordering.by { (string: String) =>
    val lastNonDigit = {
      var i = string.length - 1
      while (i >= 0 && string.charAt(i).isDigit) i -= 1
      i
    }
    val prefix = string.substring(0, lastNonDigit + 1)
    val suffix = string.substring(lastNonDigit + 1).toIntOption.getOrElse(0)
    (prefix, suffix)
  }.reverse

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
        val printedWeakTypeTag = renderCode(weakTypeTag)
        val stubString = stub.toString
        (stubString, Pattern.quote(stubString), Matcher.quoteReplacement(s"$${ $printedWeakTypeTag }"))
      }
      def toCheck = caches.view.collect { case (name, Some(Cache(stub, _, _))) =>
        name -> stub.toString
      }.toMap
    }

    object ImportedTypes {
      private val cachesAliases = scala.collection.mutable.Map.empty[String, Cache]
      // Combine imported types with same-class implicit vals (issue #168).
      // Same-class implicit vals are only used here (not in TypeWithUnderlyingInjected) because
      // their ThisType prefixes cause issues in the WeakTypeTag workaround mechanism.
      private val caches = (importedUnderlyingTypes ++ sameClassImplicitTypes).flatMap { case (tpe, name) =>
        def byName = Cache.forTypeName(name)
        def byType = try
          Cache.forTypeName(renderCode(tpe))
        catch {
          case _: Throwable => None
        }
        byType.orElse(byName).map { cache =>
          cachesAliases += (name -> cache)
          cachesAliases += (s"$name.type" -> cache)
          cachesAliases += (renderCode(tpe) -> cache)
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
        val printedWeakTypeTag = renderCode(weakTypeTag)
        val stubString = stub.toString
        (stubString, Pattern.quote(stubString), Matcher.quoteReplacement(s"$${ $printedWeakTypeTag }"))
      }
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
          .find(renderCode(s))
          .map(_.ident)
          .map { stub =>
            if (loggingEnabled) {
              println(s"""Replaced Select with stub:
                         |In:  ${pp(tree)}
                         |     ${showRawPretty(tree, SyntaxHighlight.ANSI)}
                         |Out: ${pp(stub)}
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
                         |In:  ${pp(tree)}
                         |     ${showRawPretty(tree, SyntaxHighlight.ANSI)}
                         |Out: ${pp(stub)}
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
                           |In:  ${pp(tree)}
                           |     ${showRawPretty(tree, SyntaxHighlight.ANSI)}
                           |Out: ${pp(updated)}
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
                           |In:  ${pp(tree)}
                           |     ${showRawPretty(tree, SyntaxHighlight.ANSI)}
                           |Out: ${pp(newTypeTree)}
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
      (AbstractTypes.toReplace ++ ImportedTypes.toReplace).toVector
        .sortBy(_._1)(using stubSuffixDescending)
        .foldLeft(source) { case (result, (_, pattern, replacement)) =>
          result.replaceAll(pattern, replacement)
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
        val printedExpr = renderCode(expr)
        val printedTpe = renderCode(tpe)

        val stub = Ident(freshName("expressionStub"))
        caches += (stub.toString() -> s"$${ {$printedExpr}.asInstanceOf[$ctx.Expr[$printedTpe]] }")
        stub
      }

      def storeVarArgs(expr: Tree, tpe: Tree): Tree = {
        val printedExpr = renderCode(expr)
        val printedTpe = renderCode(tpe)

        val stub = Ident(freshName("expressionStub"))
        caches += (stub.toString() ->
          s"..$${ {$printedExpr}.asInstanceOf[$ctx.Expr[$printedTpe]].tree.children.tail }")
        stub
      }

      def toReplace = caches.view.collect { case (stub, expr) =>
        val stubString = stub.toString
        (stubString, Pattern.quote(stubString), Matcher.quoteReplacement(expr))
      }
      def toCheck = caches.keys.toSet
    }

    private val WildcardStar = typeNames.WILDCARD_STAR

    override def transform(tree: Tree): Tree = tree match {
      /* Replaces:
       *   Expr.splice[Seq[A]](a): _*
       * with:
       *   ..${{a}.asInstanceOf[ctx.Expr[Seq[A]]].tree.children.tail}
       * This avoids the redundant Seq(...): _* wrapping when using VarArgs.
       */
      case Typed(
            Apply(
              TypeApply(
                Select(Select(This(_) | Ident(_), TermName("Expr")), TermName("splice")),
                List(tpe)
              ),
              List(expr)
            ),
            Ident(WildcardStar)
          ) =>
        val result = Splices.storeVarArgs(expr, tpe)

        log(
          s"""Cross-quotes ${paintExclDot(Console.BLUE)("Expr.splice")} VarArgs expansion:
             |From: ${paintExclDot(Console.BLUE)("Expr.splice")}(${pp(expr)})*
             |To: ${indent(pp(result))}""".stripMargin
        )

        result

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
             |From: ${paintExclDot(Console.BLUE)("Expr.splice")}(${pp(expr)})
             |To: ${indent(pp(result))}""".stripMargin
        )

        result

      case tree => super.transform(tree)
    }

    def replaceStubsInSource(source: String): String =
      Splices.toReplace.toVector.sortBy(_._1)(using stubSuffixDescending).foldLeft(source) {
        case (result, (_, pattern, replacement)) =>
          result.replaceAll(pattern, replacement)
      }

    def validateNoStubsLeft(source: String): String = {
      val stubs = Splices.toCheck.filter(source.contains)
      assert(stubs.isEmpty, s"Expr stubs left: ${stubs.mkString(", ")} in:\n$source")
      source
    }
  }
}
