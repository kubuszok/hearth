package hearth
package treeprinter

import scala.reflect.ClassTag

trait ShowCodePrettyScala3 {
  val quotes: scala.quoted.Quotes

  import quotes.*, quotes.reflect.*

  lazy val FormattedTreeStructure: Printer[Tree] =
    implementationDetails.HighlightedTreePrinter(SyntaxHighlight.plain)
  lazy val FormattedTreeStructureAnsi: Printer[Tree] =
    implementationDetails.HighlightedTreePrinter(SyntaxHighlight.ANSI)

  /** Adding new val/lazy val/var to the trait is breaking backward compatibility in the bytecode (SIC!).
    *
    * Therefore we should avoid that when possible - for private fields, `private` modifier does NOT help, since the
    * compiler still needs to do some magic to allow the val to refer to other vals which can be overridden.
    *
    * Instead we can create a private object - adding it, would break backward compatibility, but if it already exist,
    * and it's not available to the user, it's fine to add things there when needed.
    */
  private object implementationDetails {

    lazy val failOnUnsupportedTree: Boolean = {
      // workaround to contain @experimental from polluting the whole codebase
      val info = quotes.reflect.CompilationInfo
      val settings = info.getClass.getMethod("XmacroSettings").invoke(info).asInstanceOf[List[String]]
      val defaultValue = true
      settings
        .collectFirst { case s"hearth.betterPrintersShouldFailOnUnsupportedTree=${value}" =>
          scala.util.Try(value.trim.toBoolean).getOrElse(defaultValue)
        }
        .getOrElse(defaultValue)
    }

    final class HighlightedTreePrinter(syntaxHighlight: SyntaxHighlight) extends Printer[Tree] {

      import syntaxHighlight.*

      def show(tree: Tree): String = {
        val sb = new StringBuilder()
        sb.show(tree, indentLevel = 0, lastTree = tree)
        sb.toString()
      }

      object ReflectModule {

        def unapply(arg: Any): Option[(String, Product)] = unapplies.view
          .map { (name, unapply) =>
            unapply(arg).map((name, _))
          }
          .collectFirst { case Some(value) => value }

        private val unapplies = reflect.getClass
          .getDeclaredMethods()
          .view
          .flatMap { reflectMethod =>
            Option(reflectMethod)
              .filter(_.getParameterCount == 0)
              .filter(_.getReturnType.getSimpleName.endsWith("Module"))
              .map(_.invoke(reflect))
              .flatMap { module =>
                module.getClass
                  .getDeclaredMethods()
                  .find { moduleMethod =>
                    moduleMethod.getParameterCount == 1 && moduleMethod.getName == "unapply"
                  }
                  .map { moduleMethod =>
                    val unapply: Any => Option[Product] = (arg: Any) =>
                      try
                        moduleMethod.invoke(module, arg).match {
                          case opt: Option[?] => opt.asInstanceOf[Option[Product]]
                          case p: Product     => Some(p)
                        }
                      catch {
                        case _: Throwable =>
                          // case e: Throwable =>
                          //  println(s"failed to unapply $arg to ${moduleMethod.getName}: ${e.getMessage}")
                          None
                      }
                    unapply
                  }
              }
              .map { unapply =>
                val name = reflectMethod.getName
                (name, unapply)
              }
          }
          .toList
      }

      // Workaround for the fact that we can't check if a type is a tree at runtime.
      private object IsTree {

        def unapply(tree: Any): Option[Tree] =
          scala.util.Try {
            val casted = tree.asInstanceOf[Tree]
            val _ = casted.isExpr // should throw if not a tree
            casted
          }.toOption
      }

      extension (sb: StringBuilder) {

        private def appendIndent(indentLevel: Int): StringBuilder = sb.append("  " * indentLevel)

        @scala.annotation.nowarn
        private def show(what: Any, indentLevel: Int, lastTree: Tree): Unit = what match {
          // List and Option has to be handled before ReflectModule, since they are also products.
          case None           => appendIndent(indentLevel).append(highlightTypeDef("None"))
          case opt: Option[?] => showProduct(opt.productPrefix, opt.iterator, indentLevel, lastTree)
          case Nil            => appendIndent(indentLevel).append(highlightTypeDef("Nil"))
          case lst: List[?]   => showProduct("List", lst.iterator, indentLevel, lastTree)
          case ReflectModule(name, product @ IsTree(tree)) =>
            showProduct(name, product.productIterator, indentLevel, tree)
          case ReflectModule(name, product) => showProduct(name, product.productIterator, indentLevel, lastTree)
          // TODO" handle flags
          // Primitives have to be formatted.
          case null => appendIndent(indentLevel).append(highlightLiteral(escapedStringValue(null)))
          case _: (Boolean | Byte | Short | Int | Double) =>
            appendIndent(indentLevel).append(highlightLiteral(what.toString))
          case float: Float       => appendIndent(indentLevel).append(highlightLiteral(float.toString + "f"))
          case raw: (Char | Long) => appendIndent(indentLevel).append(highlightString(escapedStringValue(raw)))
          case raw: String        => appendIndent(indentLevel).append(highlightLiteral(escapedStringValue(raw)))
          // TODO: Class[?]
          // case cl: Class[?] => appendIndent(indentLevel).append(highlightTypeDef(cl.tpe.show(using Printer.TypeReprCode)))
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
           |${what.toString()}
           |
           |(Code in this error message rendered using `Printer.TreeStructure` instead of `HighlightedTreePrinter`)
           |
           |Please, report an issue at https://github.com/MateuszKubuszok/hearth/issues
           |""".stripMargin
      )
    }
  }
}
