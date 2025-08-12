package hearth
package cq

import java.util.regex.{Matcher, Pattern}
import scala.reflect.macros.blackbox
import scala.util.chaining.*

/** This macro is responsible for rewriting Expr.quote/Expr.splice into native quotes
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
  *   2. If there is an anonymous class, it fixes the troublesome parts of the printed Quasiquote:
  *
  * {{{
  * final class $anon$1 extends Something {
  *   def <init>(): $anon$1 = {
  *     anonymous.super.<init>()
  *     ()
  *   }
  *
  *   ...
  * }
  * new $anon$1()
  * }}}
  *
  * is replaced with:
  *
  * {{{
  * final class anonymous extends Something {
  *   // empty constructor is removed from the string representation
  * }
  * new anonymous()
  * }}}
  *
  *   3. The last, hardest part is that:
  *
  * {{{
  * new TypeClass[A] {
  *
  *   def someMethod(a: A): A = a
  * }
  * }}}
  *
  * does NOT parse. If we print it, we would get:
  *
  * {{{
  * final class $anon extends AnyRef with TypeClass[A] {
  *   def <init>(): $anon = {
  *     anonymous.super.<init>()
  *     ()
  *   }
  *
  *   def someMethod(a: A): A = a
  * }
  * new $anon()
  * }}}
  *
  * The issues here are:
  *   - def <init> is correct AST... but parser does not understand it, even though the same code just printed it
  *   - $anon break the interpolation
  *   - applied type parameter [A] makes sense outside the code... in the AST it's just an abstract type that doesn't
  *     make sense
  *
  * So, we have to:
  *
  *   - remove the empty constructor
  *   - rename $anon to something else (without `$`)
  *   - carefully replace the type parameter [A] with something that makes sense - actually, the type that is
  *     represented by `Type[A]`
  *
  * It's the most fragile part of the whole macro. But for the Proof-of-Concept, it's good enough.
  */
class CrossQuotesMacros(val c: blackbox.Context) {

  import c.universe.{Expr as _, *}

  private def reportIssue(message: String): Nothing =
    c.abort(
      c.enclosingPosition,
      s"""Unexpected error:
         |
         |  $message
         |
         |Please, report an issue at https://github.com/MateuszKubuszok/hearth/issues
         |""".stripMargin
    )

  private val loggingEnabled =
    CrossQuotesSettings.parseLoggingSettingsForScala2(c.settings)(
      scala.util.Try(c.enclosingPosition.source.file.file).toOption
    )

  private def log(message: => String): Unit =
    if (loggingEnabled) {
      // println(s"Logging: $message at ${c.enclosingPosition.source.path}:${c.enclosingPosition.line}")
      c.echo(c.enclosingPosition, message)
    }

  // Scala 3 generate prefix$macro$[n] while Scala 2 prefix[n] and we want to align the behavior
  private def freshName(prefix: String): TermName = c.universe.internal.reificationSupport.freshTermName(prefix)

  private val anonumousClassPrefix = Pattern.quote("$anon")
  private val anonymousClassReplacement = "anonymous"

  private val anonymousClassConstructor =
    "def <init>\\(\\): <.+> = \\{\n(\\s+anonymous.super.<init>)\\(\\);\n\\s+\\(\\)\n\\s+\\};"

  private def typeCtorFrom[A](tag: c.WeakTypeTag[A]) = tag.tpe.typeConstructor
  private def applyToCtor(ctor: Type, args: TypeName*) =
    AppliedTypeTree(c.internal.gen.mkAttributedRef(ctor.typeSymbol.asType), args.map(Ident(_)).toList)

  private def paint(color: String)(text: String): String =
    text.split("\n").map(line => s"$color$line${Console.RESET}").mkString("\n")

  /* Replaces:
   *   Type.of[A]
   * with:
   *   what we see in Quasiquote
   */
  def typeOfImpl[A: c.WeakTypeTag]: c.Tree = try {
    val ctx = freshName("ctx")
    val convertProvidedTypesForCrossQuotes = freshName("convertProvidedTypesForCrossQuotes")
    val termInner = freshName("Inner")
    val typeInner = TypeName(freshName("Inner").toString)

    val result = q"""
      val $ctx = CrossQuotes.ctx[_root_.scala.reflect.macros.blackbox.Context]
      import $ctx.universe.{Type => _, internal => _, _}
      implicit def $convertProvidedTypesForCrossQuotes[$typeInner](implicit $termInner: Type[$typeInner]): $ctx.WeakTypeTag[$typeInner] =
        $termInner.asInstanceOf[$ctx.WeakTypeTag[$typeInner]]
      _root_.hearth.fp.ignore($convertProvidedTypesForCrossQuotes[Any](_: Type[Any]))
      $ctx.weakTypeTag[${weakTypeOf[A]}].asInstanceOf[Type[${weakTypeOf[A]}]]
      """

    log(
      s"""Cross-quotes ${Console.BLUE}Type.of${Console.RESET} expansion:
         |From: ${paint(Console.BLUE)(s"Type.of[${weakTypeOf[A]}]")}
         |To: ${paint(Console.BLUE)(result.toString)}""".stripMargin
    )

    result
  } catch { case e: Throwable => reportIssue(e.getMessage) }

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

    val HKT = typeCtorFrom(HKTE)
    val appliedHKT = applyToCtor(HKT, TypeName("A"))

    val fullName = HKT.typeSymbol.fullName

    val ctx = freshName("ctx")
    val convertProvidedTypesForCrossQuotes = freshName("convertProvidedTypesForCrossQuotes")
    val termInner = freshName("Inner")
    val typeInner = TypeName(freshName("Inner").toString)

    val unchecked = q"""
    new Type.Ctor1.Bounded[$L1, $U1, $HKT] {
      private val $ctx = CrossQuotes.ctx[_root_.scala.reflect.macros.blackbox.Context]
      private implicit def $convertProvidedTypesForCrossQuotes[$typeInner](implicit $termInner: Type[$typeInner]): $ctx.WeakTypeTag[$typeInner] =
        $termInner.asInstanceOf[$ctx.WeakTypeTag[$typeInner]]
      import $ctx.universe.{Type => _, internal => _, _}

      private val HKT = $ctx.mirror.staticClass($fullName)

      def apply[A >: $L1 <: $U1: Type]: Type[$appliedHKT] =
        $ctx.weakTypeTag[$appliedHKT].asInstanceOf[Type[$appliedHKT]]

      def unapply[A](A: Type[A]): Option[$L1 <:??<: $U1] =
        A.asInstanceOf[$ctx.WeakTypeTag[A]].tpe.dealias.widen.baseType(HKT) match {
          case TypeRef(_, _, List(tp1)) =>
            Some($ctx.WeakTypeTag(tp1.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L1, $U1])
          case _ => None
        }
    }
    """

    val result = c.typecheck(unchecked)

    log(
      s"""Cross-quotes ${Console.BLUE}Type.Ctor1.of${Console.RESET} expansion:
         |From: ${Console.BLUE}Type.Ctor1.of[${L1.tpe}, ${U1.tpe}, $HKT]${Console.RESET}
         |To: ${result.toString
          .split("\n")
          .map(line => s"${Console.BLUE}$line${Console.RESET}")
          .mkString("\n")}""".stripMargin
    )

    result
  } catch { case e: Throwable => reportIssue(e.getMessage) }

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

    val HKT = typeCtorFrom(HKTE)
    val appliedHKT = applyToCtor(HKT, TypeName("A"), TypeName("B"))

    val fullName = HKT.typeSymbol.fullName

    val ctx = freshName("ctx")
    val convertProvidedTypesForCrossQuotes = freshName("convertProvidedTypesForCrossQuotes")
    val termInner = freshName("Inner")
    val typeInner = TypeName(freshName("Inner").toString)

    val unchecked = q"""
    new Type.Ctor2.Bounded[$L1, $U1, $L2, $U2, $HKT] {
      private val $ctx = CrossQuotes.ctx[_root_.scala.reflect.macros.blackbox.Context]
      private implicit def $convertProvidedTypesForCrossQuotes[$typeInner](implicit $termInner: Type[$typeInner]): $ctx.WeakTypeTag[$typeInner] =
        $termInner.asInstanceOf[$ctx.WeakTypeTag[$typeInner]]
      import $ctx.universe.{Type => _, internal => _, _}

      private val HKT = $ctx.mirror.staticClass($fullName)

      def apply[A >: $L1 <: $U1, B >: $L2 <: $U2: Type]: Type[$appliedHKT] = 
        $ctx.weakTypeTag[$appliedHKT].asInstanceOf[Type[$appliedHKT]]

      def unapply[A](A: Type[A]): Option[($L1 <:??<: $U1, $L2 <:??<: $U2)] =
        A.asInstanceOf[$ctx.WeakTypeTag[A]].tpe.dealias.widen.baseType(HKT) match {
          case TypeRef(_, _, List(tp1, tp2)) =>
            Some(
              (
                $ctx.WeakTypeTag(tp1.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L1, $U1],
                $ctx.WeakTypeTag(tp2.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L2, $U2]
              )
            )
          case _ => None
        }
    }
    """

    val result = c.typecheck(unchecked)

    log(
      s"""Cross-quotes ${Console.BLUE}Type.Ctor2.of${Console.RESET} expansion:
         |From: ${paint(Console.BLUE)(s"Type.Ctor2.of[${L1.tpe}, ${U1.tpe}, ${L2.tpe}, ${U2.tpe}, $HKT]")}
         |To: ${paint(Console.BLUE)(result.toString)}""".stripMargin
    )

    result
  } catch { case e: Throwable => reportIssue(e.getMessage) }

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

    val HKT = typeCtorFrom(HKTE)
    val appliedHKT = applyToCtor(HKT, TypeName("A"), TypeName("B"))

    val fullName = HKT.typeSymbol.fullName

    val ctx = freshName("ctx")
    val convertProvidedTypesForCrossQuotes = freshName("convertProvidedTypesForCrossQuotes")
    val termInner = freshName("Inner")
    val typeInner = TypeName(freshName("Inner").toString)

    val unchecked = q"""
    new Type.Ctor3.Bounded[$L1, $U1, $L2, $U2, $L3, $U3, $HKT] {
      private val $ctx = CrossQuotes.ctx[_root_.scala.reflect.macros.blackbox.Context]
      private implicit def $convertProvidedTypesForCrossQuotes[$typeInner](implicit $termInner: Type[$typeInner]): $ctx.WeakTypeTag[$typeInner] =
        $termInner.asInstanceOf[$ctx.WeakTypeTag[$typeInner]]
      import $ctx.universe.{Type => _, internal => _, _}

      private val HKT = $ctx.mirror.staticClass($fullName)

      def apply[A >: $L1 <: $U1, B >: $L2 <: $U2, C >: $L3 <: $U3: Type]: Type[$appliedHKT] = 
        $ctx.weakTypeTag[$appliedHKT].asInstanceOf[Type[$appliedHKT]]

      def unapply[A](A: Type[A]): Option[($L1 <:??<: $U1, $L2 <:??<: $U2, $L3 <:??<: $U3)] =
        A.asInstanceOf[$ctx.WeakTypeTag[A]].tpe.dealias.widen.baseType(HKT) match {
          case TypeRef(_, _, List(tp1, tp2, tp3)) =>
            Some(
              (
                $ctx.WeakTypeTag(tp1.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L1, $U1],
                $ctx.WeakTypeTag(tp2.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L2, $U2],
                $ctx.WeakTypeTag(tp3.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L3, $U3]
              )
            )
          case _ => None
        }
    }
    """

    val result = c.typecheck(unchecked)

    log(
      s"""Cross-quotes ${Console.BLUE}Type.Ctor3.of${Console.RESET} expansion:
         |From: ${paint(Console.BLUE)(
          s"Type.Ctor3.of[${L1.tpe}, ${U1.tpe}, ${L2.tpe}, ${U2.tpe}, ${L3.tpe}, ${U3.tpe}, $HKT]"
        )}
         |To: ${paint(Console.BLUE)(result.toString)}""".stripMargin
    )

    result
  } catch { case e: Throwable => reportIssue(e.getMessage) }

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

    val HKT = typeCtorFrom(HKTE)
    val appliedHKT = applyToCtor(HKT, TypeName("A"), TypeName("B"))

    val fullName = HKT.typeSymbol.fullName

    val ctx = freshName("ctx")
    val convertProvidedTypesForCrossQuotes = freshName("convertProvidedTypesForCrossQuotes")
    val termInner = freshName("Inner")
    val typeInner = TypeName(freshName("Inner").toString)

    val unchecked = q"""
    new Type.Ctor4.Bounded[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $HKT] {
      private val $ctx = CrossQuotes.ctx[_root_.scala.reflect.macros.blackbox.Context]
      private implicit def $convertProvidedTypesForCrossQuotes[$typeInner](implicit $termInner: Type[$typeInner]): $ctx.WeakTypeTag[$typeInner] =
        $termInner.asInstanceOf[$ctx.WeakTypeTag[$typeInner]]
      import $ctx.universe.{Type => _, internal => _, _}

      private val HKT = $ctx.mirror.staticClass($fullName)

      def apply[A >: $L1 <: $U1, B >: $L2 <: $U2, C >: $L3 <: $U3: Type]: Type[$appliedHKT] = 
        $ctx.weakTypeTag[$appliedHKT].asInstanceOf[Type[$appliedHKT]]

      def unapply[A](A: Type[A]): Option[($L1 <:??<: $U1, $L2 <:??<: $U2, $L3 <:??<: $U3, $L4 <:??<: $U4)] =
        A.asInstanceOf[$ctx.WeakTypeTag[A]].tpe.dealias.widen.baseType(HKT) match {
          case TypeRef(_, _, List(tp1, tp2, tp3, tp4)) =>
            Some(
              (
                $ctx.WeakTypeTag(tp1.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L1, $U1],
                $ctx.WeakTypeTag(tp2.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L2, $U2],
                $ctx.WeakTypeTag(tp3.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L3, $U3],
                $ctx.WeakTypeTag(tp4.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L4, $U4]
              )
            )
          case _ => None
        }
    }
    """

    val result = c.typecheck(unchecked)

    log(
      s"""Cross-quotes ${Console.BLUE}Type.Ctor4.of${Console.RESET} expansion:
         |From: ${paint(Console.BLUE)(
          s"Type.Ctor4.of[${L1.tpe}, ${U1.tpe}, ${L2.tpe}, ${U2.tpe}, ${L3.tpe}, ${U3.tpe}, ${L4.tpe}, ${U4.tpe}, $HKT]"
        )}
         |To: ${paint(Console.BLUE)(result.toString)}""".stripMargin
    )

    result
  } catch { case e: Throwable => reportIssue(e.getMessage) }

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

    val HKT = typeCtorFrom(HKTE)
    val appliedHKT = applyToCtor(HKT, TypeName("A"), TypeName("B"))

    val fullName = HKT.typeSymbol.fullName

    val ctx = freshName("ctx")
    val convertProvidedTypesForCrossQuotes = freshName("convertProvidedTypesForCrossQuotes")
    val termInner = freshName("Inner")
    val typeInner = TypeName(freshName("Inner").toString)

    val unchecked = q"""
    new Type.Ctor5.Bounded[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $HKT] {
      private val $ctx = CrossQuotes.ctx[_root_.scala.reflect.macros.blackbox.Context]
      private implicit def $convertProvidedTypesForCrossQuotes[$typeInner](implicit $termInner: Type[$typeInner]): $ctx.WeakTypeTag[$typeInner] =
        $termInner.asInstanceOf[$ctx.WeakTypeTag[$typeInner]]
      import $ctx.universe.{Type => _, internal => _, _}

      private val HKT = $ctx.mirror.staticClass($fullName)

      def apply[A >: $L1 <: $U1, B >: $L2 <: $U2, C >: $L3 <: $U3: Type]: Type[$appliedHKT] = 
        $ctx.weakTypeTag[$appliedHKT].asInstanceOf[Type[$appliedHKT]]

      def unapply[A](A: Type[A]): Option[($L1 <:??<: $U1, $L2 <:??<: $U2, $L3 <:??<: $U3, $L4 <:??<: $U4, $L5 <:??<: $U5)] =
        A.asInstanceOf[$ctx.WeakTypeTag[A]].tpe.dealias.widen.baseType(HKT) match {
          case TypeRef(_, _, List(tp1, tp2, tp3, tp4, tp5)) =>
            Some(
              (
                $ctx.WeakTypeTag(tp1.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L1, $U1],
                $ctx.WeakTypeTag(tp2.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L2, $U2],
                $ctx.WeakTypeTag(tp3.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L3, $U3],
                $ctx.WeakTypeTag(tp4.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L4, $U4],
                $ctx.WeakTypeTag(tp5.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L5, $U5]
              )
            )
          case _ => None
        }
    }
    """

    val result = c.typecheck(unchecked)

    log(
      s"""Cross-quotes ${Console.BLUE}Type.Ctor5.of${Console.RESET} expansion:
         |From: ${paint(Console.BLUE)(
          s"Type.Ctor5.of[${L1.tpe}, ${U1.tpe}, ${L2.tpe}, ${U2.tpe}, ${L3.tpe}, ${U3.tpe}, ${L4.tpe}, ${U4.tpe}, ${L5.tpe}, ${U5.tpe}, $HKT]"
        )}
         |To: ${paint(Console.BLUE)(result.toString)}""".stripMargin
    )

    result
  } catch { case e: Throwable => reportIssue(e.getMessage) }

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

    val HKT = typeCtorFrom(HKTE)
    val appliedHKT = applyToCtor(HKT, TypeName("A"), TypeName("B"))

    val fullName = HKT.typeSymbol.fullName

    val ctx = freshName("ctx")
    val convertProvidedTypesForCrossQuotes = freshName("convertProvidedTypesForCrossQuotes")
    val termInner = freshName("Inner")
    val typeInner = TypeName(freshName("Inner").toString)

    val unchecked = q"""
    new Type.Ctor6.Bounded[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $HKT] {
      private val $ctx = CrossQuotes.ctx[_root_.scala.reflect.macros.blackbox.Context]
      private implicit def $convertProvidedTypesForCrossQuotes[$typeInner](implicit $termInner: Type[$typeInner]): $ctx.WeakTypeTag[$typeInner] =
        $termInner.asInstanceOf[$ctx.WeakTypeTag[$typeInner]]
      import $ctx.universe.{Type => _, internal => _, _}

      private val HKT = $ctx.mirror.staticClass($fullName)

      def apply[A >: $L1 <: $U1, B >: $L2 <: $U2, C >: $L3 <: $U3: Type]: Type[$appliedHKT] = 
        $ctx.weakTypeTag[$appliedHKT].asInstanceOf[Type[$appliedHKT]]

      def unapply[A](A: Type[A]): Option[($L1 <:??<: $U1, $L2 <:??<: $U2, $L3 <:??<: $U3, $L4 <:??<: $U4, $L5 <:??<: $U5, $L6 <:??<: $U6)] =
        A.asInstanceOf[$ctx.WeakTypeTag[A]].tpe.dealias.widen.baseType(HKT) match {
          case TypeRef(_, _, List(tp1, tp2, tp3, tp4, tp5, tp6)) =>
            Some(
              (
                $ctx.WeakTypeTag(tp1.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L1, $U1],
                $ctx.WeakTypeTag(tp2.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L2, $U2],
                $ctx.WeakTypeTag(tp3.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L3, $U3],
                $ctx.WeakTypeTag(tp4.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L4, $U4],
                $ctx.WeakTypeTag(tp5.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L5, $U5],
                $ctx.WeakTypeTag(tp6.dealias.widen).asInstanceOf[Type[scala.Any]].as_<:??<:[$L6, $U6]
              )
            )
          case _ => None
        }
    }
    """

    val result = c.typecheck(unchecked)

    log(
      s"""Cross-quotes ${Console.BLUE}Type.Ctor6.of${Console.RESET} expansion:
         |From: ${paint(Console.BLUE)(
          s"Type.Ctor6.of[${L1.tpe}, ${U1.tpe}, ${L2.tpe}, ${U2.tpe}, ${L3.tpe}, ${U3.tpe}, ${L4.tpe}, ${U4.tpe}, ${L5.tpe}, ${U5.tpe}, ${L6.tpe}, ${U6.tpe}, $HKT]"
        )}
         |To: ${paint(Console.BLUE)(result.toString)}""".stripMargin
    )

    result
  } catch { case e: Throwable => reportIssue(e.getMessage) }

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

    val HKT = typeCtorFrom(HKTE)
    val appliedHKT = applyToCtor(HKT, TypeName("A"), TypeName("B"))

    val fullName = HKT.typeSymbol.fullName

    val ctx = freshName("ctx")
    val convertProvidedTypesForCrossQuotes = freshName("convertProvidedTypesForCrossQuotes")
    val termInner = freshName("Inner")
    val typeInner = TypeName(freshName("Inner").toString)

    val unchecked = q"""
    new Type.Ctor7.Bounded[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $L7, $U7, $HKT] {
      private val $ctx = CrossQuotes.ctx[_root_.scala.reflect.macros.blackbox.Context]
      private implicit def $convertProvidedTypesForCrossQuotes[$typeInner](implicit $termInner: Type[$typeInner]): $ctx.WeakTypeTag[$typeInner] =
        $termInner.asInstanceOf[$ctx.WeakTypeTag[$typeInner]]
      import $ctx.universe.{Type => _, internal => _, _}

      private val HKT = $ctx.mirror.staticClass($fullName)

      def apply[A >: $L1 <: $U1, B >: $L2 <: $U2, C >: $L3 <: $U3: Type]: Type[$appliedHKT] = 
        $ctx.weakTypeTag[$appliedHKT].asInstanceOf[Type[$appliedHKT]]

      def unapply[A](A: Type[A]): Option[($L1 <:??<: $U1, $L2 <:??<: $U2, $L3 <:??<: $U3, $L4 <:??<: $U4, $L5 <:??<: $U5, $L6 <:??<: $U6, $L7 <:??<: $U7)] =
        A.asInstanceOf[$ctx.WeakTypeTag[A]].tpe.dealias.widen.baseType(HKT) match {
          case TypeRef(_, _, List(tp1, tp2, tp3, tp4, tp5, tp6, tp7)) =>
            Some(
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
          case _ => None
        }
    }
    """

    val result = c.typecheck(unchecked)

    log(
      s"""Cross-quotes ${Console.BLUE}Type.Ctor7.of${Console.RESET} expansion:
         |From: ${paint(Console.BLUE)(
          s"Type.Ctor7.of[${L1.tpe}, ${U1.tpe}, ${L2.tpe}, ${U2.tpe}, ${L3.tpe}, ${U3.tpe}, ${L4.tpe}, ${U4.tpe}, ${L5.tpe}, ${U5.tpe}, ${L6.tpe}, ${U6.tpe}, ${L7.tpe}, ${U7.tpe}, $HKT]"
        )}
         |To: ${paint(Console.BLUE)(result.toString)}""".stripMargin
    )

    result
  } catch { case e: Throwable => reportIssue(e.getMessage) }

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

    val HKT = typeCtorFrom(HKTE)
    val appliedHKT = applyToCtor(HKT, TypeName("A"), TypeName("B"))

    val fullName = HKT.typeSymbol.fullName

    val ctx = freshName("ctx")
    val convertProvidedTypesForCrossQuotes = freshName("convertProvidedTypesForCrossQuotes")
    val termInner = freshName("Inner")
    val typeInner = TypeName(freshName("Inner").toString)

    val unchecked = q"""
    new Type.Ctor8.Bounded[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $L7, $U7, $L8, $U8, $HKT] {
      private val $ctx = CrossQuotes.ctx[_root_.scala.reflect.macros.blackbox.Context]
      private implicit def $convertProvidedTypesForCrossQuotes[$typeInner](implicit $termInner: Type[$typeInner]): $ctx.WeakTypeTag[$typeInner] =
        $termInner.asInstanceOf[$ctx.WeakTypeTag[$typeInner]]
      import $ctx.universe.{Type => _, internal => _, _}

      private val HKT = $ctx.mirror.staticClass($fullName)

      def apply[A >: $L1 <: $U1, B >: $L2 <: $U2, C >: $L3 <: $U3: Type]: Type[$appliedHKT] = 
        $ctx.weakTypeTag[$appliedHKT].asInstanceOf[Type[$appliedHKT]]

      def unapply[A](A: Type[A]): Option[($L1 <:??<: $U1, $L2 <:??<: $U2, $L3 <:??<: $U3, $L4 <:??<: $U4, $L5 <:??<: $U5, $L6 <:??<: $U6, $L7 <:??<: $U7, $L8 <:??<: $U8)] =
        A.asInstanceOf[$ctx.WeakTypeTag[A]].tpe.dealias.widen.baseType(HKT) match {
          case TypeRef(_, _, List(tp1, tp2, tp3, tp4, tp5, tp6, tp7, tp8)) =>
            Some(
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
          case _ => None
        }
    }
    """

    val result = c.typecheck(unchecked)

    log(
      s"""Cross-quotes ${Console.BLUE}Type.Ctor8.of${Console.RESET} expansion:
         |From: ${paint(Console.BLUE)(
          s"Type.Ctor8.of[${L1.tpe}, ${U1.tpe}, ${L2.tpe}, ${U2.tpe}, ${L3.tpe}, ${U3.tpe}, ${L4.tpe}, ${U4.tpe}, ${L5.tpe}, ${U5.tpe}, ${L6.tpe}, ${U6.tpe}, ${L7.tpe}, ${U7.tpe}, ${L8.tpe}, ${U8.tpe}, $HKT]"
        )}
         |To: ${paint(Console.BLUE)(result.toString)}""".stripMargin
    )

    result
  } catch { case e: Throwable => reportIssue(e.getMessage) }

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

    val HKT = typeCtorFrom(HKTE)
    val appliedHKT = applyToCtor(HKT, TypeName("A"), TypeName("B"))

    val fullName = HKT.typeSymbol.fullName

    val ctx = freshName("ctx")
    val convertProvidedTypesForCrossQuotes = freshName("convertProvidedTypesForCrossQuotes")
    val termInner = freshName("Inner")
    val typeInner = TypeName(freshName("Inner").toString)

    val unchecked = q"""
    new Type.Ctor9.Bounded[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $L7, $U7, $L8, $U8, $L9, $U9, $HKT] {
      private val $ctx = CrossQuotes.ctx[_root_.scala.reflect.macros.blackbox.Context]
      private implicit def $convertProvidedTypesForCrossQuotes[$typeInner](implicit $termInner: Type[$typeInner]): $ctx.WeakTypeTag[$typeInner] =
        $termInner.asInstanceOf[$ctx.WeakTypeTag[$typeInner]]
      import $ctx.universe.{Type => _, internal => _, _}

      private val HKT = $ctx.mirror.staticClass($fullName)

      def apply[A >: $L1 <: $U1, B >: $L2 <: $U2, C >: $L3 <: $U3: Type]: Type[$appliedHKT] = 
        $ctx.weakTypeTag[$appliedHKT].asInstanceOf[Type[$appliedHKT]]

      def unapply[A](A: Type[A]): Option[($L1 <:??<: $U1, $L2 <:??<: $U2, $L3 <:??<: $U3, $L4 <:??<: $U4, $L5 <:??<: $U5, $L6 <:??<: $U6, $L7 <:??<: $U7, $L8 <:??<: $U8)] =
        A.asInstanceOf[$ctx.WeakTypeTag[A]].tpe.dealias.widen.baseType(HKT) match {
          case TypeRef(_, _, List(tp1, tp2, tp3, tp4, tp5, tp6, tp7, tp8, tp9)) =>
            Some(
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
          case _ => None
        }
    }
    """

    val result = c.typecheck(unchecked)

    log(
      s"""Cross-quotes ${Console.BLUE}Type.Ctor9.of${Console.RESET} expansion:
         |From: ${paint(Console.BLUE)(
          s"Type.Ctor9.of[${L1.tpe}, ${U1.tpe}, ${L2.tpe}, ${U2.tpe}, ${L3.tpe}, ${U3.tpe}, ${L4.tpe}, ${U4.tpe}, ${L5.tpe}, ${U5.tpe}, ${L6.tpe}, ${U6.tpe}, ${L7.tpe}, ${U7.tpe}, ${L8.tpe}, ${U8.tpe}, ${L9.tpe}, ${U9.tpe}, $HKT]"
        )}
         |To: ${paint(Console.BLUE)(result.toString)}""".stripMargin
    )

    result
  } catch { case e: Throwable => reportIssue(e.getMessage) }

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

    val HKT = typeCtorFrom(HKTE)
    val appliedHKT = applyToCtor(HKT, TypeName("A"), TypeName("B"))

    val fullName = HKT.typeSymbol.fullName

    val ctx = freshName("ctx")
    val convertProvidedTypesForCrossQuotes = freshName("convertProvidedTypesForCrossQuotes")
    val termInner = freshName("Inner")
    val typeInner = TypeName(freshName("Inner").toString)

    val unchecked = q"""
    new Type.Ctor10.Bounded[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $L7, $U7, $L8, $U8, $L9, $U9, $L10, $U10, $HKT] {
      private val $ctx = CrossQuotes.ctx[_root_.scala.reflect.macros.blackbox.Context]
      private implicit def $convertProvidedTypesForCrossQuotes[$typeInner](implicit $termInner: Type[$typeInner]): $ctx.WeakTypeTag[$typeInner] =
        $termInner.asInstanceOf[$ctx.WeakTypeTag[$typeInner]]
      import $ctx.universe.{Type => _, internal => _, _}

      private val HKT = $ctx.mirror.staticClass($fullName)

      def apply[A >: $L1 <: $U1, B >: $L2 <: $U2, C >: $L3 <: $U3: Type]: Type[$appliedHKT] = 
        $ctx.weakTypeTag[$appliedHKT].asInstanceOf[Type[$appliedHKT]]

      def unapply[A](A: Type[A]): Option[($L1 <:??<: $U1, $L2 <:??<: $U2, $L3 <:??<: $U3, $L4 <:??<: $U4, $L5 <:??<: $U5, $L6 <:??<: $U6, $L7 <:??<: $U7, $L8 <:??<: $U8)] =
        A.asInstanceOf[$ctx.WeakTypeTag[A]].tpe.dealias.widen.baseType(HKT) match {
          case TypeRef(_, _, List(tp1, tp2, tp3, tp4, tp5, tp6, tp7, tp8, tp9, tp10)) =>
            Some(
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
          case _ => None
        }
    }
    """

    val result = c.typecheck(unchecked)

    log(
      s"""Cross-quotes ${Console.BLUE}Type.Ctor10.of${Console.RESET} expansion:
         |From: ${paint(Console.BLUE)(
          s"Type.Ctor10.of[${L1.tpe}, ${U1.tpe}, ${L2.tpe}, ${U2.tpe}, ${L3.tpe}, ${U3.tpe}, ${L4.tpe}, ${U4.tpe}, ${L5.tpe}, ${U5.tpe}, ${L6.tpe}, ${U6.tpe}, ${L7.tpe}, ${U7.tpe}, ${L8.tpe}, ${U8.tpe}, ${L9.tpe}, ${U9.tpe}, ${L10.tpe}, ${U10.tpe}, $HKT]"
        )}
         |To: ${paint(Console.BLUE)(result.toString)}""".stripMargin
    )

    result
  } catch { case e: Throwable => reportIssue(e.getMessage) }

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

    val HKT = typeCtorFrom(HKTE)
    val appliedHKT = applyToCtor(HKT, TypeName("A"), TypeName("B"))

    val fullName = HKT.typeSymbol.fullName

    val ctx = freshName("ctx")
    val convertProvidedTypesForCrossQuotes = freshName("convertProvidedTypesForCrossQuotes")
    val termInner = freshName("Inner")
    val typeInner = TypeName(freshName("Inner").toString)

    val unchecked = q"""
    new Type.Ctor11.Bounded[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $L7, $U7, $L8, $U8, $L9, $U9, $L10, $U10, $L11, $U11, $HKT] {
      private val $ctx = CrossQuotes.ctx[_root_.scala.reflect.macros.blackbox.Context]
      private implicit def $convertProvidedTypesForCrossQuotes[$typeInner](implicit $termInner: Type[$typeInner]): $ctx.WeakTypeTag[$typeInner] =
        $termInner.asInstanceOf[$ctx.WeakTypeTag[$typeInner]]
      import $ctx.universe.{Type => _, internal => _, _}

      private val HKT = $ctx.mirror.staticClass($fullName)

      def apply[A >: $L1 <: $U1, B >: $L2 <: $U2, C >: $L3 <: $U3: Type]: Type[$appliedHKT] = 
        $ctx.weakTypeTag[$appliedHKT].asInstanceOf[Type[$appliedHKT]]

      def unapply[A](A: Type[A]): Option[($L1 <:??<: $U1, $L2 <:??<: $U2, $L3 <:??<: $U3, $L4 <:??<: $U4, $L5 <:??<: $U5, $L6 <:??<: $U6, $L7 <:??<: $U7, $L8 <:??<: $U8, $L9 <:??<: $U9, $L10 <:??<: $U10, $L11 <:??<: $U11)] =
        A.asInstanceOf[$ctx.WeakTypeTag[A]].tpe.dealias.widen.baseType(HKT) match {
          case TypeRef(_, _, List(tp1, tp2, tp3, tp4, tp5, tp6, tp7, tp8, tp9, tp10, tp11)) =>
            Some(
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
          case _ => None
        }
    }
    """

    val result = c.typecheck(unchecked)

    log(
      s"""Cross-quotes ${Console.BLUE}Type.Ctor11.of${Console.RESET} expansion:
         |From: ${paint(Console.BLUE)(
          s"Type.Ctor11.of[${L1.tpe}, ${U1.tpe}, ${L2.tpe}, ${U2.tpe}, ${L3.tpe}, ${U3.tpe}, ${L4.tpe}, ${U4.tpe}, ${L5.tpe}, ${U5.tpe}, ${L6.tpe}, ${U6.tpe}, ${L7.tpe}, ${U7.tpe}, ${L8.tpe}, ${U8.tpe}, ${L9.tpe}, ${U9.tpe}, ${L10.tpe}, ${U10.tpe}, ${L11.tpe}, ${U11.tpe}, $HKT]"
        )}
         |To: ${paint(Console.BLUE)(result.toString)}""".stripMargin
    )

    result
  } catch { case e: Throwable => reportIssue(e.getMessage) }

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

    val HKT = typeCtorFrom(HKTE)
    val appliedHKT = applyToCtor(HKT, TypeName("A"), TypeName("B"))

    val fullName = HKT.typeSymbol.fullName

    val ctx = freshName("ctx")
    val convertProvidedTypesForCrossQuotes = freshName("convertProvidedTypesForCrossQuotes")
    val termInner = freshName("Inner")
    val typeInner = TypeName(freshName("Inner").toString)

    val unchecked = q"""
    new Type.Ctor12.Bounded[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $L7, $U7, $L8, $U8, $L9, $U9, $L10, $U10, $L11, $U11, $L12, $U12, $HKT] {
      private val $ctx = CrossQuotes.ctx[_root_.scala.reflect.macros.blackbox.Context]
      private implicit def $convertProvidedTypesForCrossQuotes[$typeInner](implicit $termInner: Type[$typeInner]): $ctx.WeakTypeTag[$typeInner] =
        $termInner.asInstanceOf[$ctx.WeakTypeTag[$typeInner]]
      import $ctx.universe.{Type => _, internal => _, _}

      private val HKT = $ctx.mirror.staticClass($fullName)

      def apply[A >: $L1 <: $U1, B >: $L2 <: $U2, C >: $L3 <: $U3: Type]: Type[$appliedHKT] = 
        $ctx.weakTypeTag[$appliedHKT].asInstanceOf[Type[$appliedHKT]]

      def unapply[A](A: Type[A]): Option[($L1 <:??<: $U1, $L2 <:??<: $U2, $L3 <:??<: $U3, $L4 <:??<: $U4, $L5 <:??<: $U5, $L6 <:??<: $U6, $L7 <:??<: $U7, $L8 <:??<: $U8, $L9 <:??<: $U9, $L10 <:??<: $U10, $L11 <:??<: $U11, $L12 <:??<: $U12)] =
        A.asInstanceOf[$ctx.WeakTypeTag[A]].tpe.dealias.widen.baseType(HKT) match {
          case TypeRef(_, _, List(tp1, tp2, tp3, tp4, tp5, tp6, tp7, tp8, tp9, tp10, tp11, tp12)) =>
            Some(
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
          case _ => None
        }
    }
    """

    val result = c.typecheck(unchecked)

    log(
      s"""Cross-quotes ${Console.BLUE}Type.Ctor12.of${Console.RESET} expansion:
         |From: ${paint(Console.BLUE)(
          s"Type.Ctor12.of[${L1.tpe}, ${U1.tpe}, ${L2.tpe}, ${U2.tpe}, ${L3.tpe}, ${U3.tpe}, ${L4.tpe}, ${U4.tpe}, ${L5.tpe}, ${U5.tpe}, ${L6.tpe}, ${U6.tpe}, ${L7.tpe}, ${U7.tpe}, ${L8.tpe}, ${U8.tpe}, ${L9.tpe}, ${U9.tpe}, ${L10.tpe}, ${U10.tpe}, ${L11.tpe}, ${U11.tpe}, ${L12.tpe}, ${U12.tpe}, $HKT]"
        )}
         |To: ${paint(Console.BLUE)(result.toString)}""".stripMargin
    )

    result
  } catch { case e: Throwable => reportIssue(e.getMessage) }

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

    val HKT = typeCtorFrom(HKTE)
    val appliedHKT = applyToCtor(HKT, TypeName("A"), TypeName("B"))

    val fullName = HKT.typeSymbol.fullName

    val ctx = freshName("ctx")
    val convertProvidedTypesForCrossQuotes = freshName("convertProvidedTypesForCrossQuotes")
    val termInner = freshName("Inner")
    val typeInner = TypeName(freshName("Inner").toString)

    val unchecked = q"""
    new Type.Ctor13.Bounded[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $L7, $U7, $L8, $U8, $L9, $U9, $L10, $U10, $L11, $U11, $L12, $U12, $L13, $U13, $HKT] {
      private val $ctx = CrossQuotes.ctx[_root_.scala.reflect.macros.blackbox.Context]
      private implicit def $convertProvidedTypesForCrossQuotes[$typeInner](implicit $termInner: Type[$typeInner]): $ctx.WeakTypeTag[$typeInner] =
        $termInner.asInstanceOf[$ctx.WeakTypeTag[$typeInner]]
      import $ctx.universe.{Type => _, internal => _, _}

      private val HKT = $ctx.mirror.staticClass($fullName)

      def apply[A >: $L1 <: $U1, B >: $L2 <: $U2, C >: $L3 <: $U3: Type]: Type[$appliedHKT] = 
        $ctx.weakTypeTag[$appliedHKT].asInstanceOf[Type[$appliedHKT]]

      def unapply[A](A: Type[A]): Option[($L1 <:??<: $U1, $L2 <:??<: $U2, $L3 <:??<: $U3, $L4 <:??<: $U4, $L5 <:??<: $U5, $L6 <:??<: $U6, $L7 <:??<: $U7, $L8 <:??<: $U8, $L9 <:??<: $U9, $L10 <:??<: $U10, $L11 <:??<: $U11, $L12 <:??<: $U12, $L13 <:??<: $U13)] =
        A.asInstanceOf[$ctx.WeakTypeTag[A]].tpe.dealias.widen.baseType(HKT) match {
          case TypeRef(_, _, List(tp1, tp2, tp3, tp4, tp5, tp6, tp7, tp8, tp9, tp10, tp11, tp12, tp13)) =>
            Some(
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
          case _ => None
        }
    }
    """

    val result = c.typecheck(unchecked)

    log(
      s"""Cross-quotes ${Console.BLUE}Type.Ctor13.of${Console.RESET} expansion:
         |From: ${paint(Console.BLUE)(
          s"Type.Ctor13.of[${L1.tpe}, ${U1.tpe}, ${L2.tpe}, ${U2.tpe}, ${L3.tpe}, ${U3.tpe}, ${L4.tpe}, ${U4.tpe}, ${L5.tpe}, ${U5.tpe}, ${L6.tpe}, ${U6.tpe}, ${L7.tpe}, ${U7.tpe}, ${L8.tpe}, ${U8.tpe}, ${L9.tpe}, ${U9.tpe}, ${L10.tpe}, ${U10.tpe}, ${L11.tpe}, ${U11.tpe}, ${L12.tpe}, ${U12.tpe}, ${L13.tpe}, ${U13.tpe}, $HKT]"
        )}
         |To: ${paint(Console.BLUE)(result.toString)}""".stripMargin
    )

    result
  } catch { case e: Throwable => reportIssue(e.getMessage) }

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

    val HKT = typeCtorFrom(HKTE)
    val appliedHKT = applyToCtor(HKT, TypeName("A"), TypeName("B"))

    val fullName = HKT.typeSymbol.fullName

    val ctx = freshName("ctx")
    val convertProvidedTypesForCrossQuotes = freshName("convertProvidedTypesForCrossQuotes")
    val termInner = freshName("Inner")
    val typeInner = TypeName(freshName("Inner").toString)

    val unchecked = q"""
    new Type.Ctor14.Bounded[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $L7, $U7, $L8, $U8, $L9, $U9, $L10, $U10, $L11, $U11, $L12, $U12, $L13, $U13, $L14, $U14, $HKT] {
      private val $ctx = CrossQuotes.ctx[_root_.scala.reflect.macros.blackbox.Context]
      private implicit def $convertProvidedTypesForCrossQuotes[$typeInner](implicit $termInner: Type[$typeInner]): $ctx.WeakTypeTag[$typeInner] =
        $termInner.asInstanceOf[$ctx.WeakTypeTag[$typeInner]]
      import $ctx.universe.{Type => _, internal => _, _}

      private val HKT = $ctx.mirror.staticClass($fullName)

      def apply[A >: $L1 <: $U1, B >: $L2 <: $U2, C >: $L3 <: $U3: Type]: Type[$appliedHKT] = 
        $ctx.weakTypeTag[$appliedHKT].asInstanceOf[Type[$appliedHKT]]

      def unapply[A](A: Type[A]): Option[($L1 <:??<: $U1, $L2 <:??<: $U2, $L3 <:??<: $U3, $L4 <:??<: $U4, $L5 <:??<: $U5, $L6 <:??<: $U6, $L7 <:??<: $U7, $L8 <:??<: $U8, $L9 <:??<: $U9, $L10 <:??<: $U10, $L11 <:??<: $U11, $L12 <:??<: $U12, $L13 <:??<: $U13, $L14 <:??<: $U14)] =
        A.asInstanceOf[$ctx.WeakTypeTag[A]].tpe.dealias.widen.baseType(HKT) match {
          case TypeRef(_, _, List(tp1, tp2, tp3, tp4, tp5, tp6, tp7, tp8, tp9, tp10, tp11, tp12, tp13, tp14)) =>
            Some(
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
          case _ => None
        }
    }
    """

    val result = c.typecheck(unchecked)

    log(
      s"""Cross-quotes ${Console.BLUE}Type.Ctor14.of${Console.RESET} expansion:
         |From: ${paint(Console.BLUE)(
          s"Type.Ctor14.of[${L1.tpe}, ${U1.tpe}, ${L2.tpe}, ${U2.tpe}, ${L3.tpe}, ${U3.tpe}, ${L4.tpe}, ${U4.tpe}, ${L5.tpe}, ${U5.tpe}, ${L6.tpe}, ${U6.tpe}, ${L7.tpe}, ${U7.tpe}, ${L8.tpe}, ${U8.tpe}, ${L9.tpe}, ${U9.tpe}, ${L10.tpe}, ${U10.tpe}, ${L11.tpe}, ${U11.tpe}, ${L12.tpe}, ${U12.tpe}, ${L13.tpe}, ${U13.tpe}, ${L14.tpe}, ${U14.tpe}, $HKT]"
        )}
         |To: ${paint(Console.BLUE)(result.toString)}""".stripMargin
    )

    result
  } catch { case e: Throwable => reportIssue(e.getMessage) }

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

    val HKT = typeCtorFrom(HKTE)
    val appliedHKT = applyToCtor(HKT, TypeName("A"), TypeName("B"))

    val fullName = HKT.typeSymbol.fullName

    val ctx = freshName("ctx")
    val convertProvidedTypesForCrossQuotes = freshName("convertProvidedTypesForCrossQuotes")
    val termInner = freshName("Inner")
    val typeInner = TypeName(freshName("Inner").toString)

    val unchecked = q"""
    new Type.Ctor15.Bounded[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $L7, $U7, $L8, $U8, $L9, $U9, $L10, $U10, $L11, $U11, $L12, $U12, $L13, $U13, $L14, $U14, $L15, $U15, $HKT] {
      private val $ctx = CrossQuotes.ctx[_root_.scala.reflect.macros.blackbox.Context]
      private implicit def $convertProvidedTypesForCrossQuotes[$typeInner](implicit $termInner: Type[$typeInner]): $ctx.WeakTypeTag[$typeInner] =
        $termInner.asInstanceOf[$ctx.WeakTypeTag[$typeInner]]
      import $ctx.universe.{Type => _, internal => _, _}

      private val HKT = $ctx.mirror.staticClass($fullName)

      def apply[A >: $L1 <: $U1, B >: $L2 <: $U2, C >: $L3 <: $U3: Type]: Type[$appliedHKT] = 
        $ctx.weakTypeTag[$appliedHKT].asInstanceOf[Type[$appliedHKT]]

      def unapply[A](A: Type[A]): Option[($L1 <:??<: $U1, $L2 <:??<: $U2, $L3 <:??<: $U3, $L4 <:??<: $U4, $L5 <:??<: $U5, $L6 <:??<: $U6, $L7 <:??<: $U7, $L8 <:??<: $U8, $L9 <:??<: $U9, $L10 <:??<: $U10, $L11 <:??<: $U11, $L12 <:??<: $U12, $L13 <:??<: $U13, $L14 <:??<: $U14, $L15 <:??<: $U15)] =
        A.asInstanceOf[$ctx.WeakTypeTag[A]].tpe.dealias.widen.baseType(HKT) match {
          case TypeRef(_, _, List(tp1, tp2, tp3, tp4, tp5, tp6, tp7, tp8, tp9, tp10, tp11, tp12, tp13, tp14, tp15)) =>
            Some(
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
          case _ => None
        }
    }
    """

    val result = c.typecheck(unchecked)

    log(
      s"""Cross-quotes ${Console.BLUE}Type.Ctor15.of${Console.RESET} expansion:
         |From: ${paint(Console.BLUE)(
          s"Type.Ctor15.of[${L1.tpe}, ${U1.tpe}, ${L2.tpe}, ${U2.tpe}, ${L3.tpe}, ${U3.tpe}, ${L4.tpe}, ${U4.tpe}, ${L5.tpe}, ${U5.tpe}, ${L6.tpe}, ${U6.tpe}, ${L7.tpe}, ${U7.tpe}, ${L8.tpe}, ${U8.tpe}, ${L9.tpe}, ${U9.tpe}, ${L10.tpe}, ${U10.tpe}, ${L11.tpe}, ${U11.tpe}, ${L12.tpe}, ${U12.tpe}, ${L13.tpe}, ${U13.tpe}, ${L14.tpe}, ${U14.tpe}, ${L15.tpe}, ${U15.tpe}, $HKT]"
        )}
         |To: ${paint(Console.BLUE)(result.toString)}""".stripMargin
    )

    result
  } catch { case e: Throwable => reportIssue(e.getMessage) }

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

    val HKT = typeCtorFrom(HKTE)
    val appliedHKT = applyToCtor(HKT, TypeName("A"), TypeName("B"))

    val fullName = HKT.typeSymbol.fullName

    val ctx = freshName("ctx")
    val convertProvidedTypesForCrossQuotes = freshName("convertProvidedTypesForCrossQuotes")
    val termInner = freshName("Inner")
    val typeInner = TypeName(freshName("Inner").toString)

    val unchecked = q"""
    new Type.Ctor16.Bounded[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $L7, $U7, $L8, $U8, $L9, $U9, $L10, $U10, $L11, $U11, $L12, $U12, $L13, $U13, $L14, $U14, $L15, $U15, $L16, $U16, $HKT] {
      private val $ctx = CrossQuotes.ctx[_root_.scala.reflect.macros.blackbox.Context]
      private implicit def $convertProvidedTypesForCrossQuotes[$typeInner](implicit $termInner: Type[$typeInner]): $ctx.WeakTypeTag[$typeInner] =
        $termInner.asInstanceOf[$ctx.WeakTypeTag[$typeInner]]
      import $ctx.universe.{Type => _, internal => _, _}

      private val HKT = $ctx.mirror.staticClass($fullName)

      def apply[A >: $L1 <: $U1, B >: $L2 <: $U2, C >: $L3 <: $U3: Type]: Type[$appliedHKT] = 
        $ctx.weakTypeTag[$appliedHKT].asInstanceOf[Type[$appliedHKT]]

      def unapply[A](A: Type[A]): Option[($L1 <:??<: $U1, $L2 <:??<: $U2, $L3 <:??<: $U3, $L4 <:??<: $U4, $L5 <:??<: $U5, $L6 <:??<: $U6, $L7 <:??<: $U7, $L8 <:??<: $U8, $L9 <:??<: $U9, $L10 <:??<: $U10, $L11 <:??<: $U11, $L12 <:??<: $U12, $L13 <:??<: $U13, $L14 <:??<: $U14, $L15 <:??<: $U15, $L16 <:??<: $U16)] =
        A.asInstanceOf[$ctx.WeakTypeTag[A]].tpe.dealias.widen.baseType(HKT) match {
          case TypeRef(_, _, List(tp1, tp2, tp3, tp4, tp5, tp6, tp7, tp8, tp9, tp10, tp11, tp12, tp13, tp14, tp15, tp16)) =>
            Some(
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
          case _ => None
        }
    }
    """

    val result = c.typecheck(unchecked)

    log(
      s"""Cross-quotes ${Console.BLUE}Type.Ctor16.of${Console.RESET} expansion:
         |From: ${paint(Console.BLUE)(
          s"Type.Ctor16.of[${L1.tpe}, ${U1.tpe}, ${L2.tpe}, ${U2.tpe}, ${L3.tpe}, ${U3.tpe}, ${L4.tpe}, ${U4.tpe}, ${L5.tpe}, ${U5.tpe}, ${L6.tpe}, ${U6.tpe}, ${L7.tpe}, ${U7.tpe}, ${L8.tpe}, ${U8.tpe}, ${L9.tpe}, ${U9.tpe}, ${L10.tpe}, ${U10.tpe}, ${L11.tpe}, ${U11.tpe}, ${L12.tpe}, ${U12.tpe}, ${L13.tpe}, ${U13.tpe}, ${L14.tpe}, ${U14.tpe}, ${L15.tpe}, ${U15.tpe}, ${L16.tpe}, ${U16.tpe}, $HKT]"
        )}
         |To: ${paint(Console.BLUE)(result.toString)}""".stripMargin
    )

    result
  } catch { case e: Throwable => reportIssue(e.getMessage) }

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

    val HKT = typeCtorFrom(HKTE)
    val appliedHKT = applyToCtor(HKT, TypeName("A"), TypeName("B"))

    val fullName = HKT.typeSymbol.fullName

    val ctx = freshName("ctx")
    val convertProvidedTypesForCrossQuotes = freshName("convertProvidedTypesForCrossQuotes")
    val termInner = freshName("Inner")
    val typeInner = TypeName(freshName("Inner").toString)

    val unchecked = q"""
    new Type.Ctor17.Bounded[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $L7, $U7, $L8, $U8, $L9, $U9, $L10, $U10, $L11, $U11, $L12, $U12, $L13, $U13, $L14, $U14, $L15, $U15, $L16, $U16, $L17, $U17, $HKT] {
      private val $ctx = CrossQuotes.ctx[_root_.scala.reflect.macros.blackbox.Context]
      private implicit def $convertProvidedTypesForCrossQuotes[$typeInner](implicit $termInner: Type[$typeInner]): $ctx.WeakTypeTag[$typeInner] =
        $termInner.asInstanceOf[$ctx.WeakTypeTag[$typeInner]]
      import $ctx.universe.{Type => _, internal => _, _}

      private val HKT = $ctx.mirror.staticClass($fullName)

      def apply[A >: $L1 <: $U1, B >: $L2 <: $U2, C >: $L3 <: $U3: Type]: Type[$appliedHKT] = 
        $ctx.weakTypeTag[$appliedHKT].asInstanceOf[Type[$appliedHKT]]

      def unapply[A](A: Type[A]): Option[($L1 <:??<: $U1, $L2 <:??<: $U2, $L3 <:??<: $U3, $L4 <:??<: $U4, $L5 <:??<: $U5, $L6 <:??<: $U6, $L7 <:??<: $U7, $L8 <:??<: $U8, $L9 <:??<: $U9, $L10 <:??<: $U10, $L11 <:??<: $U11, $L12 <:??<: $U12, $L13 <:??<: $U13, $L14 <:??<: $U14, $L15 <:??<: $U15, $L16 <:??<: $U16, $L17 <:??<: $U17)] =
        A.asInstanceOf[$ctx.WeakTypeTag[A]].tpe.dealias.widen.baseType(HKT) match {
          case TypeRef(_, _, List(tp1, tp2, tp3, tp4, tp5, tp6, tp7, tp8, tp9, tp10, tp11, tp12, tp13, tp14, tp15, tp16, tp17)) =>
            Some(
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
          case _ => None
        }
    }
    """

    val result = c.typecheck(unchecked)

    log(
      s"""Cross-quotes ${Console.BLUE}Type.Ctor17.of${Console.RESET} expansion:
         |From: ${paint(Console.BLUE)(
          s"Type.Ctor17.of[${L1.tpe}, ${U1.tpe}, ${L2.tpe}, ${U2.tpe}, ${L3.tpe}, ${U3.tpe}, ${L4.tpe}, ${U4.tpe}, ${L5.tpe}, ${U5.tpe}, ${L6.tpe}, ${U6.tpe}, ${L7.tpe}, ${U7.tpe}, ${L8.tpe}, ${U8.tpe}, ${L9.tpe}, ${U9.tpe}, ${L10.tpe}, ${U10.tpe}, ${L11.tpe}, ${U11.tpe}, ${L12.tpe}, ${U12.tpe}, ${L13.tpe}, ${U13.tpe}, ${L14.tpe}, ${U14.tpe}, ${L15.tpe}, ${U15.tpe}, ${L16.tpe}, ${U16.tpe}, ${L17.tpe}, ${U17.tpe}, $HKT]"
        )}
         |To: ${paint(Console.BLUE)(result.toString)}""".stripMargin
    )

    result
  } catch { case e: Throwable => reportIssue(e.getMessage) }

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

    val HKT = typeCtorFrom(HKTE)
    val appliedHKT = applyToCtor(HKT, TypeName("A"), TypeName("B"))

    val fullName = HKT.typeSymbol.fullName

    val ctx = freshName("ctx")
    val convertProvidedTypesForCrossQuotes = freshName("convertProvidedTypesForCrossQuotes")
    val termInner = freshName("Inner")
    val typeInner = TypeName(freshName("Inner").toString)

    val unchecked = q"""
    new Type.Ctor18.Bounded[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $L7, $U7, $L8, $U8, $L9, $U9, $L10, $U10, $L11, $U11, $L12, $U12, $L13, $U13, $L14, $U14, $L15, $U15, $L16, $U16, $L17, $U17, $L18, $U18, $HKT] {
      private val $ctx = CrossQuotes.ctx[_root_.scala.reflect.macros.blackbox.Context]
      private implicit def $convertProvidedTypesForCrossQuotes[$typeInner](implicit $termInner: Type[$typeInner]): $ctx.WeakTypeTag[$typeInner] =
        $termInner.asInstanceOf[$ctx.WeakTypeTag[$typeInner]]
      import $ctx.universe.{Type => _, internal => _, _}

      private val HKT = $ctx.mirror.staticClass($fullName)

      def apply[A >: $L1 <: $U1, B >: $L2 <: $U2, C >: $L3 <: $U3: Type]: Type[$appliedHKT] = 
        $ctx.weakTypeTag[$appliedHKT].asInstanceOf[Type[$appliedHKT]]

      def unapply[A](A: Type[A]): Option[($L1 <:??<: $U1, $L2 <:??<: $U2, $L3 <:??<: $U3, $L4 <:??<: $U4, $L5 <:??<: $U5, $L6 <:??<: $U6, $L7 <:??<: $U7, $L8 <:??<: $U8, $L9 <:??<: $U9, $L10 <:??<: $U10, $L11 <:??<: $U11, $L12 <:??<: $U12, $L13 <:??<: $U13, $L14 <:??<: $U14, $L15 <:??<: $U15, $L16 <:??<: $U16, $L17 <:??<: $U17, $L18 <:??<: $U18)] =
        A.asInstanceOf[$ctx.WeakTypeTag[A]].tpe.dealias.widen.baseType(HKT) match {
          case TypeRef(_, _, List(tp1, tp2, tp3, tp4, tp5, tp6, tp7, tp8, tp9, tp10, tp11, tp12, tp13, tp14, tp15, tp16, tp17, tp18)) =>
            Some(
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
          case _ => None
        }
    }
    """

    val result = c.typecheck(unchecked)

    log(
      s"""Cross-quotes ${Console.BLUE}Type.Ctor18.of${Console.RESET} expansion:
         |From: ${paint(Console.BLUE)(
          s"Type.Ctor18.of[${L1.tpe}, ${U1.tpe}, ${L2.tpe}, ${U2.tpe}, ${L3.tpe}, ${U3.tpe}, ${L4.tpe}, ${U4.tpe}, ${L5.tpe}, ${U5.tpe}, ${L6.tpe}, ${U6.tpe}, ${L7.tpe}, ${U7.tpe}, ${L8.tpe}, ${U8.tpe}, ${L9.tpe}, ${U9.tpe}, ${L10.tpe}, ${U10.tpe}, ${L11.tpe}, ${U11.tpe}, ${L12.tpe}, ${U12.tpe}, ${L13.tpe}, ${U13.tpe}, ${L14.tpe}, ${U14.tpe}, ${L15.tpe}, ${U15.tpe}, ${L16.tpe}, ${U16.tpe}, ${L17.tpe}, ${U17.tpe}, ${L18.tpe}, ${U18.tpe}, $HKT]"
        )}
         |To: ${paint(Console.BLUE)(result.toString)}""".stripMargin
    )

    result
  } catch { case e: Throwable => reportIssue(e.getMessage) }

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

    val HKT = typeCtorFrom(HKTE)
    val appliedHKT = applyToCtor(HKT, TypeName("A"), TypeName("B"))

    val fullName = HKT.typeSymbol.fullName

    val ctx = freshName("ctx")
    val convertProvidedTypesForCrossQuotes = freshName("convertProvidedTypesForCrossQuotes")
    val termInner = freshName("Inner")
    val typeInner = TypeName(freshName("Inner").toString)

    val unchecked = q"""
    new Type.Ctor19.Bounded[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $L7, $U7, $L8, $U8, $L9, $U9, $L10, $U10, $L11, $U11, $L12, $U12, $L13, $U13, $L14, $U14, $L15, $U15, $L16, $U16, $L17, $U17, $L18, $U18, $L19, $U19, $HKT] {
      private val $ctx = CrossQuotes.ctx[_root_.scala.reflect.macros.blackbox.Context]
      private implicit def $convertProvidedTypesForCrossQuotes[$typeInner](implicit $termInner: Type[$typeInner]): $ctx.WeakTypeTag[$typeInner] =
        $termInner.asInstanceOf[$ctx.WeakTypeTag[$typeInner]]
      import $ctx.universe.{Type => _, internal => _, _}

      private val HKT = $ctx.mirror.staticClass($fullName)

      def apply[A >: $L1 <: $U1, B >: $L2 <: $U2, C >: $L3 <: $U3: Type]: Type[$appliedHKT] = 
        $ctx.weakTypeTag[$appliedHKT].asInstanceOf[Type[$appliedHKT]]

      def unapply[A](A: Type[A]): Option[($L1 <:??<: $U1, $L2 <:??<: $U2, $L3 <:??<: $U3, $L4 <:??<: $U4, $L5 <:??<: $U5, $L6 <:??<: $U6, $L7 <:??<: $U7, $L8 <:??<: $U8, $L9 <:??<: $U9, $L10 <:??<: $U10, $L11 <:??<: $U11, $L12 <:??<: $U12, $L13 <:??<: $U13, $L14 <:??<: $U14, $L15 <:??<: $U15, $L16 <:??<: $U16, $L17 <:??<: $U17, $L18 <:??<: $U18, $L19 <:??<: $U19)] =
        A.asInstanceOf[$ctx.WeakTypeTag[A]].tpe.dealias.widen.baseType(HKT) match {
          case TypeRef(_, _, List(tp1, tp2, tp3, tp4, tp5, tp6, tp7, tp8, tp9, tp10, tp11, tp12, tp13, tp14, tp15, tp16, tp17, tp18, tp19)) =>
            Some(
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
          case _ => None
        }
    }
    """

    val result = c.typecheck(unchecked)

    log(
      s"""Cross-quotes ${Console.BLUE}Type.Ctor19.of${Console.RESET} expansion:
         |From: ${paint(Console.BLUE)(
          s"Type.Ctor19.of[${L1.tpe}, ${U1.tpe}, ${L2.tpe}, ${U2.tpe}, ${L3.tpe}, ${U3.tpe}, ${L4.tpe}, ${U4.tpe}, ${L5.tpe}, ${U5.tpe}, ${L6.tpe}, ${U6.tpe}, ${L7.tpe}, ${U7.tpe}, ${L8.tpe}, ${U8.tpe}, ${L9.tpe}, ${U9.tpe}, ${L10.tpe}, ${U10.tpe}, ${L11.tpe}, ${U11.tpe}, ${L12.tpe}, ${U12.tpe}, ${L13.tpe}, ${U13.tpe}, ${L14.tpe}, ${U14.tpe}, ${L15.tpe}, ${U15.tpe}, ${L16.tpe}, ${U16.tpe}, ${L17.tpe}, ${U17.tpe}, ${L18.tpe}, ${U18.tpe}, ${L19.tpe}, ${U19.tpe}, $HKT]"
        )}
         |To: ${paint(Console.BLUE)(result.toString)}""".stripMargin
    )

    result
  } catch { case e: Throwable => reportIssue(e.getMessage) }

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

    val HKT = typeCtorFrom(HKTE)
    val appliedHKT = applyToCtor(HKT, TypeName("A"), TypeName("B"))

    val fullName = HKT.typeSymbol.fullName

    val ctx = freshName("ctx")
    val convertProvidedTypesForCrossQuotes = freshName("convertProvidedTypesForCrossQuotes")
    val termInner = freshName("Inner")
    val typeInner = TypeName(freshName("Inner").toString)

    val unchecked = q"""
    new Type.Ctor20.Bounded[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $L7, $U7, $L8, $U8, $L9, $U9, $L10, $U10, $L11, $U11, $L12, $U12, $L13, $U13, $L14, $U14, $L15, $U15, $L16, $U16, $L17, $U17, $L18, $U18, $L19, $U19, $L20, $U20, $HKT] {
      private val $ctx = CrossQuotes.ctx[_root_.scala.reflect.macros.blackbox.Context]
      private implicit def $convertProvidedTypesForCrossQuotes[$typeInner](implicit $termInner: Type[$typeInner]): $ctx.WeakTypeTag[$typeInner] =
        $termInner.asInstanceOf[$ctx.WeakTypeTag[$typeInner]]
      import $ctx.universe.{Type => _, internal => _, _}

      private val HKT = $ctx.mirror.staticClass($fullName)

      def apply[A >: $L1 <: $U1, B >: $L2 <: $U2, C >: $L3 <: $U3: Type]: Type[$appliedHKT] = 
        $ctx.weakTypeTag[$appliedHKT].asInstanceOf[Type[$appliedHKT]]

      def unapply[A](A: Type[A]): Option[($L1 <:??<: $U1, $L2 <:??<: $U2, $L3 <:??<: $U3, $L4 <:??<: $U4, $L5 <:??<: $U5, $L6 <:??<: $U6, $L7 <:??<: $U7, $L8 <:??<: $U8, $L9 <:??<: $U9, $L10 <:??<: $U10, $L11 <:??<: $U11, $L12 <:??<: $U12, $L13 <:??<: $U13, $L14 <:??<: $U14, $L15 <:??<: $U15, $L16 <:??<: $U16, $L17 <:??<: $U17, $L18 <:??<: $U18, $L19 <:??<: $U19, $L20 <:??<: $U20)] =
        A.asInstanceOf[$ctx.WeakTypeTag[A]].tpe.dealias.widen.baseType(HKT) match {
          case TypeRef(_, _, List(tp1, tp2, tp3, tp4, tp5, tp6, tp7, tp8, tp9, tp10, tp11, tp12, tp13, tp14, tp15, tp16, tp17, tp18, tp19, tp20)) =>
            Some(
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
          case _ => None
        }
    }
    """

    val result = c.typecheck(unchecked)

    log(
      s"""Cross-quotes ${Console.BLUE}Type.Ctor20.of${Console.RESET} expansion:
         |From: ${paint(Console.BLUE)(
          s"Type.Ctor20.of[${L1.tpe}, ${U1.tpe}, ${L2.tpe}, ${U2.tpe}, ${L3.tpe}, ${U3.tpe}, ${L4.tpe}, ${U4.tpe}, ${L5.tpe}, ${U5.tpe}, ${L6.tpe}, ${U6.tpe}, ${L7.tpe}, ${U7.tpe}, ${L8.tpe}, ${U8.tpe}, ${L9.tpe}, ${U9.tpe}, ${L10.tpe}, ${U10.tpe}, ${L11.tpe}, ${U11.tpe}, ${L12.tpe}, ${U12.tpe}, ${L13.tpe}, ${U13.tpe}, ${L14.tpe}, ${U14.tpe}, ${L15.tpe}, ${U15.tpe}, ${L16.tpe}, ${U16.tpe}, ${L17.tpe}, ${U17.tpe}, ${L18.tpe}, ${U18.tpe}, ${L19.tpe}, ${U19.tpe}, ${L20.tpe}, ${U20.tpe}, $HKT]"
        )}
         |To: ${paint(Console.BLUE)(result.toString)}""".stripMargin
    )

    result
  } catch { case e: Throwable => reportIssue(e.getMessage) }

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

    val HKT = typeCtorFrom(HKTE)
    val appliedHKT = applyToCtor(HKT, TypeName("A"), TypeName("B"))

    val fullName = HKT.typeSymbol.fullName

    val ctx = freshName("ctx")
    val convertProvidedTypesForCrossQuotes = freshName("convertProvidedTypesForCrossQuotes")
    val termInner = freshName("Inner")
    val typeInner = TypeName(freshName("Inner").toString)

    val unchecked = q"""
    new Type.Ctor21.Bounded[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $L7, $U7, $L8, $U8, $L9, $U9, $L10, $U10, $L11, $U11, $L12, $U12, $L13, $U13, $L14, $U14, $L15, $U15, $L16, $U16, $L17, $U17, $L18, $U18, $L19, $U19, $L20, $U20, $L21, $U21, $HKT] {
      private val $ctx = CrossQuotes.ctx[_root_.scala.reflect.macros.blackbox.Context]
      private implicit def $convertProvidedTypesForCrossQuotes[$typeInner](implicit $termInner: Type[$typeInner]): $ctx.WeakTypeTag[$typeInner] =
        $termInner.asInstanceOf[$ctx.WeakTypeTag[$typeInner]]
      import $ctx.universe.{Type => _, internal => _, _}

      private val HKT = $ctx.mirror.staticClass($fullName)

      def apply[A >: $L1 <: $U1, B >: $L2 <: $U2, C >: $L3 <: $U3: Type]: Type[$appliedHKT] = 
        $ctx.weakTypeTag[$appliedHKT].asInstanceOf[Type[$appliedHKT]]

      def unapply[A](A: Type[A]): Option[($L1 <:??<: $U1, $L2 <:??<: $U2, $L3 <:??<: $U3, $L4 <:??<: $U4, $L5 <:??<: $U5, $L6 <:??<: $U6, $L7 <:??<: $U7, $L8 <:??<: $U8, $L9 <:??<: $U9, $L10 <:??<: $U10, $L11 <:??<: $U11, $L12 <:??<: $U12, $L13 <:??<: $U13, $L14 <:??<: $U14, $L15 <:??<: $U15, $L16 <:??<: $U16, $L17 <:??<: $U17, $L18 <:??<: $U18, $L19 <:??<: $U19, $L20 <:??<: $U20, $L21 <:??<: $U21)] =
        A.asInstanceOf[$ctx.WeakTypeTag[A]].tpe.dealias.widen.baseType(HKT) match {
          case TypeRef(_, _, List(tp1, tp2, tp3, tp4, tp5, tp6, tp7, tp8, tp9, tp10, tp11, tp12, tp13, tp14, tp15, tp16, tp17, tp18, tp19, tp20, tp21)) =>
            Some(
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
          case _ => None
        }
    }
    """

    val result = c.typecheck(unchecked)

    log(
      s"""Cross-quotes ${Console.BLUE}Type.Ctor21.of${Console.RESET} expansion:
         |From: ${paint(Console.BLUE)(
          s"Type.Ctor21.of[${L1.tpe}, ${U1.tpe}, ${L2.tpe}, ${U2.tpe}, ${L3.tpe}, ${U3.tpe}, ${L4.tpe}, ${U4.tpe}, ${L5.tpe}, ${U5.tpe}, ${L6.tpe}, ${U6.tpe}, ${L7.tpe}, ${U7.tpe}, ${L8.tpe}, ${U8.tpe}, ${L9.tpe}, ${U9.tpe}, ${L10.tpe}, ${U10.tpe}, ${L11.tpe}, ${U11.tpe}, ${L12.tpe}, ${U12.tpe}, ${L13.tpe}, ${U13.tpe}, ${L14.tpe}, ${U14.tpe}, ${L15.tpe}, ${U15.tpe}, ${L16.tpe}, ${U16.tpe}, ${L17.tpe}, ${U17.tpe}, ${L18.tpe}, ${U18.tpe}, ${L19.tpe}, ${U19.tpe}, ${L20.tpe}, ${U20.tpe}, ${L21.tpe}, ${U21.tpe}, $HKT]"
        )}
         |To: ${paint(Console.BLUE)(result.toString)}""".stripMargin
    )

    result
  } catch { case e: Throwable => reportIssue(e.getMessage) }

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

    val HKT = typeCtorFrom(HKTE)
    val appliedHKT = applyToCtor(HKT, TypeName("A"), TypeName("B"))

    val fullName = HKT.typeSymbol.fullName

    val ctx = freshName("ctx")
    val convertProvidedTypesForCrossQuotes = freshName("convertProvidedTypesForCrossQuotes")
    val termInner = freshName("Inner")
    val typeInner = TypeName(freshName("Inner").toString)

    val unchecked = q"""
    new Type.Ctor22.Bounded[$L1, $U1, $L2, $U2, $L3, $U3, $L4, $U4, $L5, $U5, $L6, $U6, $L7, $U7, $L8, $U8, $L9, $U9, $L10, $U10, $L11, $U11, $L12, $U12, $L13, $U13, $L14, $U14, $L15, $U15, $L16, $U16, $L17, $U17, $L18, $U18, $L19, $U19, $L20, $U20, $L21, $U21, $L22, $U22, $HKT] {
      private val $ctx = CrossQuotes.ctx[_root_.scala.reflect.macros.blackbox.Context]
      private implicit def $convertProvidedTypesForCrossQuotes[$typeInner](implicit $termInner: Type[$typeInner]): $ctx.WeakTypeTag[$typeInner] =
        $termInner.asInstanceOf[$ctx.WeakTypeTag[$typeInner]]
      import $ctx.universe.{Type => _, internal => _, _}

      private val HKT = $ctx.mirror.staticClass($fullName)

      def apply[A >: $L1 <: $U1, B >: $L2 <: $U2, C >: $L3 <: $U3: Type]: Type[$appliedHKT] = 
        $ctx.weakTypeTag[$appliedHKT].asInstanceOf[Type[$appliedHKT]]

      def unapply[A](A: Type[A]): Option[($L1 <:??<: $U1, $L2 <:??<: $U2, $L3 <:??<: $U3, $L4 <:??<: $U4, $L5 <:??<: $U5, $L6 <:??<: $U6, $L7 <:??<: $U7, $L8 <:??<: $U8, $L9 <:??<: $U9, $L10 <:??<: $U10, $L11 <:??<: $U11, $L12 <:??<: $U12, $L13 <:??<: $U13, $L14 <:??<: $U14, $L15 <:??<: $U15, $L16 <:??<: $U16, $L17 <:??<: $U17, $L18 <:??<: $U18, $L19 <:??<: $U19, $L20 <:??<: $U20, $L21 <:??<: $U21, $L22 <:??<: $U22)] =
        A.asInstanceOf[$ctx.WeakTypeTag[A]].tpe.dealias.widen.baseType(HKT) match {
          case TypeRef(_, _, List(tp1, tp2, tp3, tp4, tp5, tp6, tp7, tp8, tp9, tp10, tp11, tp12, tp13, tp14, tp15, tp16, tp17, tp18, tp19, tp20, tp21, tp22)) =>
            Some(
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
          case _ => None
        }
    }
    """

    val result = c.typecheck(unchecked)

    log(
      s"""Cross-quotes ${Console.BLUE}Type.Ctor22.of${Console.RESET} expansion:
         |From: ${paint(Console.BLUE)(
          s"Type.Ctor22.of[${L1.tpe}, ${U1.tpe}, ${L2.tpe}, ${U2.tpe}, ${L3.tpe}, ${U3.tpe}, ${L4.tpe}, ${U4.tpe}, ${L5.tpe}, ${U5.tpe}, ${L6.tpe}, ${U6.tpe}, ${L7.tpe}, ${U7.tpe}, ${L8.tpe}, ${U8.tpe}, ${L9.tpe}, ${U9.tpe}, ${L10.tpe}, ${U10.tpe}, ${L11.tpe}, ${U11.tpe}, ${L12.tpe}, ${U12.tpe}, ${L13.tpe}, ${U13.tpe}, ${L14.tpe}, ${U14.tpe}, ${L15.tpe}, ${U15.tpe}, ${L16.tpe}, ${U16.tpe}, ${L17.tpe}, ${U17.tpe}, ${L18.tpe}, ${U18.tpe}, ${L19.tpe}, ${U19.tpe}, ${L20.tpe}, ${U20.tpe}, ${L21.tpe}, ${U21.tpe}, ${L22.tpe}, ${U22.tpe}, $HKT]"
        )}
         |To: ${paint(Console.BLUE)(result.toString)}""".stripMargin
    )

    result
  } catch { case e: Throwable => reportIssue(e.getMessage) }

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

    val result = q"""
      val $ctx = CrossQuotes.ctx[_root_.scala.reflect.macros.blackbox.Context]
      import $ctx.universe.Quasiquote
      implicit def $convertProvidedTypesForCrossQuotes[$typeInner](implicit $termInner: Type[$typeInner]): $ctx.WeakTypeTag[$typeInner] =
        $termInner.asInstanceOf[$ctx.WeakTypeTag[$typeInner]]
      _root_.hearth.fp.ignore($convertProvidedTypesForCrossQuotes[Any](_: Type[Any]))
      $ctx.Expr[${weakTypeOf[A]}](${convert(ctx)(expr.tree)}).asInstanceOf[Expr[${weakTypeOf[A]}]]
      """

    log(
      s"""Cross-quotes ${Console.BLUE}Expr.quote${Console.RESET} expansion:
         |From: ${paint(Console.BLUE)(s"Expr.quote[${weakTypeOf[A]}]($expr)")}
         |To: ${paint(Console.BLUE)(result.toString)}""".stripMargin
    )

    result
  } catch { case e: Throwable => reportIssue(e.getMessage) }

  private def convert(ctx: TermName)(tree: c.Tree): c.Tree = {
    // 1. Replace certain trees with stubs,
    // 2. then print the tree,
    // 3. replace stubs with their actual replacements
    // 3. and parse it back into Tree
    // It is _so much easier_ than properly modifying the tree.
    val abstractTypeReferenceReplacer = new AbstractTypeReferenceReplacer(ctx)
    val spliceReplacer = new SpliceReplacer(ctx)
    tree
      // replace some Exprs and Types with stubs
      .pipe(spliceReplacer.transform)
      .pipe(abstractTypeReferenceReplacer.transform)
      // Convert to String
      .pipe(_.toString)
      .tap { source =>
        if (loggingEnabled) {
          println(s"""Stubbed source:
                     |${paint(Console.BLUE)(source)}
                     |""".stripMargin)
        }
      }
      // Replace stubs with their values
      .pipe(abstractTypeReferenceReplacer.replaceStubsInSource)
      .pipe(spliceReplacer.replaceStubsInSource)
      // Remove parts of anonymous classes that breaks the parser
      .pipe(sourceWithFixedAnonymousClasses)
      // Validate that there are no stubs left
      .pipe(abstractTypeReferenceReplacer.validateNoStubsLeft)
      .pipe(spliceReplacer.validateNoStubsLeft)
      // Parse back to Tree
      .pipe(parse)
  }

  private def sourceWithFixedAnonymousClasses(source: String): String =
    source.replaceAll(anonumousClassPrefix, anonymousClassReplacement).replaceAll(anonymousClassConstructor, "")

  private def parse(quoteContent: String): c.Tree = try {
    val quoted = "q\"\"\"" + quoteContent + "\"\"\""
    val expr = c.parse(quoted)
    // log(s"Quasiquote: $quoted")
    expr
  } catch {
    // If quasiquotes contains anonymous variable e.g. x$1, it might look like a string interpolation,
    // but it's not. So we try to escape the $ and parse again.
    case e: scala.reflect.macros.ParseException if e.msg.contains("invalid string interpolation") =>
      val escaped = quoteContent.view.map {
        case '$' => "$$"
        case c   => c
      }.mkString
      parse(escaped)
    // case e: scala.reflect.macros.ParseException =>
    //   println(s"Error: $e, at:\n$quoteContent")
    //   throw e
  }

  /** Handles type parameters inside Expr.quote: Expr.quote { def foo[A] = ... // use A }, as we as type parameters
    * inside Expr.splice: that require Type[A] from the outside.
    */
  private class AbstractTypeReferenceReplacer(ctx: TermName) extends Transformer {

    private var insideTypeParametricCrap = false

    // In theory: we should always be able to check if there is Type[A], when it is
    // convert it to c.WeakTypeTag[A] and use it in quasiquotes.
    // But in practice, some code fails to compile, I don't know why.
    // So for the time being we are enabling/disabling it using trial-and-error, currently:
    //  - it is enabled inside def/class with type parameters (defined inside Expr.quote)
    //  - it is enabled for single-letter types
    //  - it is disabled for everything else
    // we should fix this, but for now it's OK.
    private def shouldTryUsingImplicitType(tree: Tree): Boolean =
      insideTypeParametricCrap || tree.toString.length == 1

    private case class Cache(stub: TypeName, symbol: Symbol, weakTypeTag: Tree)

    private val abstractTypes = scala.collection.mutable.Map.empty[String, Option[Cache]]

    private def attemptToFindAbstractType(name: Name): Name =
      attemptToFindAbstractType(name.toString).map(_.stub).getOrElse(name)

    private def attemptToFindAbstractType(symbol: Symbol): Symbol =
      attemptToFindAbstractType(symbol.name.toString).map(_.symbol).getOrElse(symbol)

    private def attemptToFindAbstractType(name: String): Option[Cache] = abstractTypes.getOrElseUpdate(
      name,
      try {
        val parsed = c.typecheck(c.parse(s"Type[$name]"))
        val stub = c.universe.internal.reificationSupport.freshTypeName("TypeParameterStub")
        @scala.annotation.nowarn
        val symbol = {
          val originalSymbol = parsed.tpe match {
            // TypeRef(ThisType(hearth.demo.ShowMacrosImpl), TypeName("Type"), List(TypeRef(NoPrefix, TypeName("A"), List())))
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
        Some(Cache(stub, symbol, ttag))
      } catch {
        // case e: scala.tools.reflect.ToolBoxError =>
        //   println(s"Error: $e")
        //   None
        case _: Exception => None
      }
    )

    override def transform(tree: Tree): Tree = tree match {
      // find classes that have type parameters
      case cd @ ClassDef(_, _, _, Template(parents, _, _))
          if !insideTypeParametricCrap && parents.exists(_.tpe.typeSymbol.asType.typeParams.nonEmpty) =>
        val oldTypeParametricCrap = insideTypeParametricCrap
        try {
          insideTypeParametricCrap = true
          super.transform(cd)
        } finally
          insideTypeParametricCrap = oldTypeParametricCrap
      // find methods that have type parameters
      case dd: DefDef if !insideTypeParametricCrap && dd.tparams.nonEmpty =>
        val oldTypeParametricCrap = insideTypeParametricCrap
        try {
          insideTypeParametricCrap = true
          super.transform(dd)
        } finally
          insideTypeParametricCrap = oldTypeParametricCrap
      // if we're in either of the above, we need to transform the type parameters
      case AppliedTypeTree(tpt, args) if shouldTryUsingImplicitType(tree) =>
        AppliedTypeTree(transform(tpt), args.map(transform))
      case Ident(name) if shouldTryUsingImplicitType(tree) =>
        Ident(attemptToFindAbstractType(name))
      case tt: TypeTree if shouldTryUsingImplicitType(tree) =>
        if (tt.original != null) {
          val updated = transform(tt.original)
          if (updated == tt.original) tt.original
          else updated
        } else {
          TypeTree(transformType(tt.tpe))
        }
      case tree =>
        val transformed = super.transform(tree)
        if (loggingEnabled) {
          println(s"""Transforming tree:
                     |In:  ${paint(Console.BLUE)(tree.toString)}
                     |     ${paint(Console.BLUE)(showRaw(tree))}
                     |Out: ${paint(Console.BLUE)(transformed.toString)}
                     |     ${paint(Console.BLUE)(showRaw(transformed))}
                     |crap: $insideTypeParametricCrap
                     |""".stripMargin)
        }
        transformed
    }

    private def transformType(tpe: Type): Type = tpe match {
      case TypeRef(pre, sym, args) =>
        c.internal.typeRef(transformType(pre), attemptToFindAbstractType(sym), args.map(transformType))
      case tpe => tpe
    }

    def replaceStubsInSource(source: String): String = abstractTypes.view.values.flatten.foldLeft(source) {
      case (result, Cache(stub, _, weakTypeTag)) =>
        result.replaceAll(Pattern.quote(stub.toString), Matcher.quoteReplacement(s"$${ $weakTypeTag }"))
    }

    def validateNoStubsLeft(source: String): String = {
      val stubs = abstractTypes.collect {
        case (name, Some(Cache(stub, _, _))) if source.contains(stub.toString) => name
      }
      assert(stubs.isEmpty, s"Type parameter stubs left: ${stubs.mkString(", ")} in:\n$source")
      source
    }
  }

  /** Handles type parameters inside Expr.splice: Expr.splice[A](a)
    */
  private class SpliceReplacer(ctx: TermName) extends Transformer {

    private val toReplace = scala.collection.mutable.Map.empty[String, String]

    override def transform(tree: Tree): Tree = tree match {
      /* Replaces:
       *   Expr.splice[A](a)
       * with:
       *   ${{a}.asInstanceOf[ctx.Expr[A]]}
       */
      case Apply(
            TypeApply(
              Select(Select(This(_), TermName("Expr")), TermName("splice")),
              List(tpe)
            ),
            List(expr)
          ) =>
        val result = convert(ctx)(expr) match {
          case Ident(_) =>
            toReplace += (expr.toString() -> s"$${ {$expr}.asInstanceOf[$ctx.Expr[$tpe]] }")
            expr
          case _ =>
            val stub = Ident(freshName("expressionStub"))
            toReplace += (stub.toString() -> s"$${ {$expr}.asInstanceOf[$ctx.Expr[$tpe]] }")
            stub
        }

        log(
          s"""Cross-quotes ${Console.BLUE}Expr.splice${Console.RESET} expansion:
             |From: ${paint(Console.BLUE)(s"Expr.splice[$tpe]($expr)")}
             |To: ${paint(Console.BLUE)(result.toString)}""".stripMargin
        )

        result

      case tree => super.transform(tree)
    }

    def replaceStubsInSource(source: String): String = toReplace.foldLeft(source) { case (result, (stub, expr)) =>
      result.replaceAll(Pattern.quote(stub.toString), Matcher.quoteReplacement(expr))
    }

    def validateNoStubsLeft(source: String): String = {
      val stubs = toReplace.keys.filter(source.contains)
      assert(stubs.isEmpty, s"Expr stubs left: ${stubs.mkString(", ")} in:\n$source")
      source
    }
  }
}
