package hearth
package treeprinter

import java.lang.reflect.InvocationTargetException
import scala.reflect.ClassTag

trait ShowCodePrettyScala3 {
  val quotes: scala.quoted.Quotes

  import quotes.*, quotes.reflect.*

  /** Printer that formats the tree with indentation.
    *
    * @since 0.2.0
    */
  lazy val FormattedTreeStructure: Printer[Tree] =
    implementationDetails.HighlightedTreePrinter(SyntaxHighlight.plain)

  /** Printer that formats the tree with indentation and ANSI colors.
    *
    * @since 0.2.0
    */
  lazy val FormattedTreeStructureAnsi: Printer[Tree] =
    implementationDetails.HighlightedTreePrinter(SyntaxHighlight.ANSI)

  /** Adding new val/lazy val/var to the trait is breaking backward compatibility in the bytecode (SIC!).
    *
    * Therefore we should avoid that when possible - for private fields, `private` modifier does NOT help, since the
    * compiler still needs to do some magic to allow the val to refer to other vals which can be overridden.
    *
    * Instead we can create a private object - adding it, would break backward compatibility, but if it already exists,
    * and it's not available to the user, it's fine to add things there when needed.
    */
  private object implementationDetails {

    Constants.initSettings {
      // workaround to contain @experimental from polluting the whole codebase
      val info = quotes.reflect.CompilationInfo
      info.getClass.getMethod("XmacroSettings").invoke(info).asInstanceOf[List[String]]
    }
    import Constants.*

    final class HighlightedTreePrinter(syntaxHighlight: SyntaxHighlight) extends Printer[Tree] {

      import syntaxHighlight.*

      def show(tree: Tree): String =
        try {
          val sb = new StringBuilder()
          sb.show(tree, indentLevel = 0, lastTree = tree)
          sb.toString()
        } catch {
          // $COVERAGE-OFF$
          case StringBuildingTerminated(sb) =>
            val warningLength = stringBuilderLimitWarning.length
            val truncated =
              if sb.length <= (stringBuilderHardLimit - warningLength) then sb
              else sb.take(stringBuilderHardLimit - warningLength)
            truncated.append(stringBuilderLimitWarning).toString
          // $COVERAGE-ON$
        }

      // Workaround for the fact, that the Tree hierarchy would change betwen versions - but it should always work
      // via reflection.
      object ReflectModule {

        def unapply(arg: Any): Option[(String, Product | String)] =
          if arg == null then None else attemptUnapply(arg)

        private def attemptUnapply(arg: Any): Option[(String, Product | String)] = unapplies.view
          .map { (name, unapply) =>
            unapply(arg).map((name, _))
          }
          .collectFirst { case Some(value) => value }

        private val unapplies =
          (for {
            reflectMethod <- reflect.getClass.getDeclaredMethods().view
            if reflectMethod.getParameterCount == 0 && reflectMethod.getReturnType.getSimpleName.endsWith("Module")
            module = reflectMethod.invoke(reflect)
            moduleMethod <- module.getClass
              .getDeclaredMethods()
              .view
              .find { moduleMethod =>
                moduleMethod.getParameterCount == 1 && moduleMethod.getName == "unapply"
              }
              .view
          } yield {
            val name = reflectMethod.getName
            val unapply: Any => Option[Product | String] = (arg: Any) =>
              if arg == null then None
              else
                try
                  moduleMethod.invoke(module, arg).match {
                    case b: Boolean     => Some(Tuple())
                    case s: String      => Some(s)
                    case opt: Option[?] => opt.asInstanceOf[Option[Product | String]]
                    case p: Product     => Some(p)
                  }
                catch {
                  // Wrong argument for the unapply method - usually if the type hierarchies are completely different.
                  case _: IllegalArgumentException => None
                  // Same as above, but when type hierarchy is the same, but the actual type is different.
                  case e: InvocationTargetException if e.getCause.isInstanceOf[ClassCastException] => None
                  case e: InvocationTargetException if e.getCause.isInstanceOf[MatchError]         => None
                  case e: Throwable => if failOnUnsupportedTree then throw e else None
                }
            (name, unapply)
          }).toList
      }

      // Handles highlighting of literals and their formatting.
      private object HighlightedLiteral {

        def apply(value: Null | Boolean | Byte | Short | Int | Long | Float | Double | Char | String): String =
          value match {
            case null                                       => highlightLiteral(escapedStringValue(null))
            case _: (Boolean | Byte | Short | Int | Double) => highlightLiteral(value.toString)
            case float: Float                               => highlightLiteral(float.toString + "f")
            case raw: (Char | Long)                         => highlightLiteral(escapedStringValue(raw))
            case raw: String                                => highlightString(escapedStringValue(raw))
          }

        def unapply(value: Any): Option[String] = value match {
          case null                                           => Some(apply(null))
          case value: (Boolean | Byte | Short | Int | Double) => Some(apply(value))
          case float: Float                                   => Some(apply(float))
          case raw: (Char | Long)                             => Some(apply(raw))
          case raw: String                                    => Some(apply(raw))
          case _                                              => None
        }
      }

      // Workaround for the fact, that we can't check if a type is a tree at runtime.
      private object IsSymbol {

        def unapply(symbol: Any): Option[Symbol] =
          try
            Some {
              val casted = symbol.asInstanceOf[Symbol]
              val _ = casted.flags // should throw if not a symbol
              casted
            }
          catch {
            case _: Throwable => None
          }
      }

      // Workaround for the fact, that we can't check if a type is a tree at runtime.
      private object IsTree {

        def unapply(tree: Any): Option[Tree] =
          try
            Some {
              val casted = tree.asInstanceOf[Tree]
              val _ = casted.isExpr // should throw if not a tree
              casted
            }
          catch {
            case _: Throwable => None
          }
      }

      // Workaround for the fact, that we can't check if a type is a tree at runtime.
      private object IsTypeTree {

        def unapply(tree: Any): Option[TypeTree] =
          try
            Some {
              val casted = tree.asInstanceOf[TypeTree]
              assert(casted.symbol.isType)
              val _ = casted.tpe // should throw if not a type tree
              casted
            }
          catch {
            case _: Throwable => None
          }
      }

      extension (sb: StringBuilder) {

        private def appendIndent(indentLevel: Int): StringBuilder = sb.append("  " * indentLevel)

        @scala.annotation.nowarn
        private def show(what: Any, indentLevel: Int, lastTree: Tree): Unit = what match {
          // To prevent "UTF8 string too large" errors, we limit the size of the StringBuilder.
          case _ if sb.length > stringBuilderSoftLimit =>
            // To test the whole tree, we can clear the StringBuilder and continue printing.
            // The result makes no sense, but we can look for exceptions on unhandled cases.
            // $COVERAGE-OFF$
            if areWeInTests then {
              sb.clear()
              show(what, indentLevel, lastTree)
            } else throw StringBuildingTerminated(sb)
          // $COVERAGE-ON$

          // Explicitly handled cases - reflection fails to deal with them correctly.
          // And we almost always print them as String literals anyway.
          case name if name.getClass.getName.startsWith("dotty.tools.dotc.core.Names$") =>
            appendIndent(indentLevel).append(HighlightedLiteral(name.toString))

          // List and Option has to be handled before ReflectModule, since they are also products.
          case None           => appendIndent(indentLevel).append(highlightTypeDef("None"))
          case opt: Option[?] => showProduct(opt.productPrefix, opt.iterator, indentLevel, lastTree)
          case Nil            => appendIndent(indentLevel).append(highlightTypeDef("Nil"))
          case lst: List[?]   => showProduct("List", lst.iterator, indentLevel, lastTree)

          // We cannot print code that would build the symbols - but we can add comments what are they for.
          case IsSymbol(symbol) =>
            appendIndent(indentLevel)
              .append(highlightTripleQs)
              .append(" /* Symbol of ")
              .append(highlightTypeDef(symbol.fullName))
              .append(" */")
          // We do-NOT-want to handle that, at all (possible recursion and shit)

          case IsTypeTree(typeTree) =>
            appendIndent(indentLevel).append(highlightTypeDef(typeTree.show(using Printer.TreeStructure)))

          // Handle all Tree elements that can be handled with TypeName.unapply(tree).
          case ReflectModule(name, product) =>
            def newLastTree = product match {
              case IsTree(tree) => tree
              case _            => lastTree
            }
            product match {
              case p: Product => showProduct(name, p.productIterator, indentLevel, newLastTree)
              case s: String  => appendIndent(indentLevel).append(HighlightedLiteral(s))
            }

          // Handle all literals.
          case HighlightedLiteral(literal) => appendIndent(indentLevel).append(literal)

          // Unhandled cases are reported as unsupported trees, when such setting is enabled.
          case _ if failOnUnsupportedTree => unsupportedTree(lastTree, what)
          // Unhandled cases are rendered as is, when it's not enabled.
          case IsTree(tree) =>
            appendIndent(indentLevel).append(highlightTypeDef(tree.show(using Printer.TreeStructure)))
          case _ => appendIndent(indentLevel).append(highlightTypeDef(what.toString))
        }

        @scala.annotation.nowarn
        private def showProduct(name: String, it: Iterator[Any], indentLevel: Int, lastTree: Tree): Unit = {
          val _ = appendIndent(indentLevel).append(highlightTypeDef(name)).append("(")

          if name == "Literal" then {
            val _ = sb.append("\n")
            val Literal(value) = lastTree: @unchecked
            value match {
              case BooleanConstant(value) =>
                showProduct("BooleanConstant", Iterator.single(value), indentLevel + 1, lastTree)
              case ByteConstant(value) => showProduct("ByteConstant", Iterator.single(value), indentLevel + 1, lastTree)
              case ShortConstant(value) =>
                showProduct("ShortConstant", Iterator.single(value), indentLevel + 1, lastTree)
              case IntConstant(value)  => showProduct("IntConstant", Iterator.single(value), indentLevel + 1, lastTree)
              case LongConstant(value) => showProduct("LongConstant", Iterator.single(value), indentLevel + 1, lastTree)
              case FloatConstant(value) =>
                showProduct("FloatConstant", Iterator.single(value), indentLevel + 1, lastTree)
              case DoubleConstant(value) =>
                showProduct("DoubleConstant", Iterator.single(value), indentLevel + 1, lastTree)
              case CharConstant(value) => showProduct("CharConstant", Iterator.single(value), indentLevel + 1, lastTree)
              case StringConstant(value) =>
                showProduct("StringConstant", Iterator.single(value), indentLevel + 1, lastTree)
              case UnitConstant()         => showProduct("UnitConstant", Iterator.empty, indentLevel + 1, lastTree)
              case NullConstant()         => showProduct("NullConstant", Iterator.empty, indentLevel + 1, lastTree)
              case ClassOfConstant(value) =>
                showProduct("ClassOfConstant", Iterator.single(value), indentLevel + 1, lastTree)
              // Unhandled cases are reported as unsupported trees, when such setting is enabled.
              case _ if failOnUnsupportedTree => unsupportedTree(lastTree, lastTree)
              // Unhandled cases are rendered as is, when it's not enabled.
              case IsTree(tree) =>
                appendIndent(indentLevel).append(highlightTypeDef(tree.show(using Printer.TreeStructure)))
              case _ => appendIndent(indentLevel).append(highlightTypeDef(value.toString))
            }
            val _ = sb.append("\n").appendIndent(indentLevel)
          } else if it.nonEmpty then {
            val _ = sb.append("\n")
            while it.hasNext do {
              val nextWhat = it.next()
              val nextLastTree = nextWhat match {
                case IsTree(tree) => tree
                case _            => lastTree
              }
              show(nextWhat, indentLevel + 1, nextLastTree)
              if it.hasNext then {
                val _ = sb.append(",\n")
              }
            }
            val _ = sb.append("\n").appendIndent(indentLevel)
          }

          val _ = sb.append(")")
        }
      }

      private def escapedStringValue(raw: Null | String | Char | Long): String = {
        import java.lang.StringBuilder
        def requiresFormat(c: Char): Boolean =
          c match {
            case '\b' | '\t' | '\n' | '\f' | '\r' | '"' | '\'' | '\\' => true
            case c                                                    => c.isControl
          }
        def escapedChar(b: StringBuilder, c: Char): Unit = {
          def quadNibble(b: StringBuilder, x: Int, i: Int): Unit =
            if i < 4 then {
              quadNibble(b, x >> 4, i + 1)
              val n = x & 0xf
              val c = if n < 10 then '0' + n else 'A' + (n - 10)
              val _ = b.append(c.toChar)
            }
          val replace = c match {
            case '\b' => "\\b"
            case '\t' => "\\t"
            case '\n' => "\\n"
            case '\f' => "\\f"
            case '\r' => "\\r"
            case '"'  => "\\\""
            case '\'' => "\\\'"
            case '\\' => "\\\\"
            case c    =>
              if c.isControl then {
                b.append("\\u")
                quadNibble(b, c.toInt, 0)
              } else b.append(c)
              return
          }
          val _ = b.append(replace)
        }
        def escape(text: String) = {
          def mustBuild: Boolean = {
            var i = 0
            while i < text.length do {
              if requiresFormat(text.charAt(i)) then return true
              i += 1
            }
            false
          }
          if mustBuild then {
            val b = new StringBuilder(text.length + 16).append('"')
            var i = 0
            while i < text.length do {
              escapedChar(b, text.charAt(i))
              i += 1
            }
            b.append('"').toString
          } else "\"" + text + "\""
        }
        raw match {
          case null      => "null"
          case s: String => escape(s)
          // case clazz: ClassTag[?] => "classOf[" + signature(clazz.tpe) + "]"
          case c: Char =>
            if requiresFormat(c) then {
              val b = new StringBuilder().append('\'')
              escapedChar(b, c)
              b.append('\'').toString
            } else "'" + c + "'"
          case l: Long => l.toString() + "L"
        }
      }

      // $COVERAGE-OFF$should only be triggered by error we don't know about
      private def unsupportedTree(tree: Tree, what: Any): Nothing = throw new UnsupportedOperationException(
        s"""Unsupported tree for `showCodePretty`:
           |
           |${Option(tree).map(_.show(using Printer.TreeAnsiCode)).filter(_.nonEmpty).getOrElse("<no tree ?>")}
           |
           |constructed as:
           |
           |${tree.show(using Printer.TreeStructure)}
           |
           |failed at:
           |
           |${what.toString()} : ${what.getClass.getName}
           |
           |(Code in this error message rendered using `Printer.TreeStructure` instead of `HighlightedTreePrinter`)
           |
           |Please, report an issue at https://github.com/MateuszKubuszok/hearth/issues
           |""".stripMargin
      )
      // $COVERAGE-ON$
    }
  }
}
