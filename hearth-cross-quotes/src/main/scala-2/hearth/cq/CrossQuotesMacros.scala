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

  /* Replaces:
   *   Type.of[A]
   * with:
   *   what we see in Quasiquote
   */
  def typeOfImpl[A: c.WeakTypeTag]: c.Tree = {
    val termB = freshName("B")
    val typeB = TypeName(freshName("B").toString)
    val ctx = freshName("ctx")
    val convertProvidedTypesForCrossQuotes = freshName("convertProvidedTypesForCrossQuotes")

    val result = q"""
      val $ctx = CrossQuotes.ctx[scala.reflect.macros.blackbox.Context]
      import $ctx.universe.{Type => _, internal => _, _}
      implicit def $convertProvidedTypesForCrossQuotes[$typeB](implicit $termB: Type[$typeB]): $ctx.WeakTypeTag[$typeB] =
        $termB.asInstanceOf[$ctx.WeakTypeTag[$typeB]]
      _root_.hearth.fp.ignore($convertProvidedTypesForCrossQuotes[Any](_: Type[Any]))
      weakTypeTag[${weakTypeOf[A]}].asInstanceOf[Type[${weakTypeOf[A]}]]
      """

    log(
      s"""Cross-quotes Type.of expansion:
         |From: Type.of[${weakTypeOf[A]}]
         |To: $result""".stripMargin
    )

    result
  }

  /* Replaces:
   *   Expr.quote[A](a)
   * with:
   *   what we see in Quasiquote
   */
  def quoteImpl[A: c.WeakTypeTag](expr: c.Expr[A]): c.Tree = {
    val termB = freshName("B")
    val typeB = TypeName(freshName("B").toString)
    val ctx = freshName("ctx")
    val convertProvidedTypesForCrossQuotes = freshName("convertProvidedTypesForCrossQuotes")

    val result = q"""
      val $ctx = CrossQuotes.ctx[scala.reflect.macros.blackbox.Context]
      import $ctx.universe.Quasiquote
      implicit def $convertProvidedTypesForCrossQuotes[$typeB](implicit $termB: Type[$typeB]): $ctx.WeakTypeTag[$typeB] =
        $termB.asInstanceOf[$ctx.WeakTypeTag[$typeB]]
      _root_.hearth.fp.ignore($convertProvidedTypesForCrossQuotes[Any](_: Type[Any]))
      $ctx.Expr[${weakTypeOf[A]}](${convert(ctx)(expr.tree)}).asInstanceOf[Expr[${weakTypeOf[A]}]]
      """

    log(
      s"""Cross-quotes Expr.quote expansion:
         |From: Expr.quote[${weakTypeOf[A]}]($expr)
         |To: $result""".stripMargin
    )

    result
  }

  private def convert(ctx: TermName)(tree: c.Tree): c.Tree = {
    val abstractTypeReplacer = new AbstractTypeReplacer(ctx)
    val unquoter = new Unquoter(ctx)
    tree
      // replace some Exprs and Types with stubs
      .pipe(unquoter.transform)
      .pipe(abstractTypeReplacer.transform)
      // Convert to String
      .pipe(_.toString)
      // Replace stubs with their values
      .pipe(abstractTypeReplacer.replaceStubsInSource)
      .pipe(unquoter.replaceStubsInSource)
      // Remove parts of anonymous classes that breaks the parser
      .pipe(sourceWithFixedAnonymousClasses)
      // Validate that there are no stubs left
      .pipe(abstractTypeReplacer.validateNoStubsLeft)
      .pipe(unquoter.validateNoStubsLeft)
      // Parse back to Tree
      .pipe(parse)
  }

  private def sourceWithFixedAnonymousClasses(source: String): String =
    source.replaceAll(anonumousClassPrefix, anonymousClassReplacement).replaceAll(anonymousClassConstructor, "")

  private def parse(quoteContent: String): c.Tree = try {
    val quoted = "q\"\"\"" + quoteContent + "\"\"\""
    val expr = c.parse(quoted)
    log(s"Quasiquote: $quoted")
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

  private class AbstractTypeReplacer(ctx: TermName) extends Transformer {

    private var insideTypeParametricCrap = false

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
      case cd @ ClassDef(_, _, _, Template(parents, _, _))
          if !insideTypeParametricCrap && parents.exists(_.tpe.typeSymbol.asType.typeParams.nonEmpty) =>
        val oldTypeParametricCrap = insideTypeParametricCrap
        try {
          insideTypeParametricCrap = true
          super.transform(cd)
        } finally
          insideTypeParametricCrap = oldTypeParametricCrap
      case AppliedTypeTree(tpt, args) if insideTypeParametricCrap =>
        AppliedTypeTree(transform(tpt), args.map(transform))
      case Ident(name) if insideTypeParametricCrap  => Ident(attemptToFindAbstractType(name))
      case tt: TypeTree if insideTypeParametricCrap =>
        if (tt.original != null) {
          val updated = transform(tt.original)
          if (updated == tt.original) tt.original
          else updated
        } else {
          TypeTree(transformType(tt.tpe))
        }
      case tree => super.transform(tree)
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

  private class Unquoter(ctx: TermName) extends Transformer {

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
          s"""Cross-quotes Expr.splice expansion:
             |From: Expr.splice[$tpe]($expr)
             |To: $result""".stripMargin
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
