# Better Printers

Built-in tree/type printers in macros have their limitations:

 - Scala 2's `showCode` and `showRaw`: have no syntax highlighting and no line breaks and indentations, that makes reading code easier
 - Scala 3's `Printer.TreeAnsiCode` and `Printer.TypeReprAnsiCode` are fixing that, but `Printer.TreeStructure` has no support
   for syntax highlighting

This module addresses these shortcomings by providing improved printers with syntax highlighting and proper formatting for both Scala 2 and Scala 3.

!!! note "Scala 2 vs Scala 3 Differences"
    The implementations for Scala 2 and Scala 3 are **platform-specific** and use different underlying APIs:
    
    - **Scala 2**: Uses `showCodePretty` and `showRawPretty` methods that extend Scala 2's `CodePrinter` and `RawTreePrinter`
    - **Scala 3**: Uses `FormattedTreeStructure` and `FormattedTreeStructureAnsi` printers that extend Scala 3's `Printer[Tree]`
    
    While both provide similar functionality (syntax highlighting and formatting), they **do not aim for identical output** due to fundamental differences in how Scala 2 and Scala 3 represent ASTs. The output format will differ between platforms, but both provide improved readability over the built-in printers.

## Basic Utilities integrations

If you are using `MacroCommons` and its:

 - `Expr.prettyPrint(expr)`/`expr.prettyPrint`
 - `Expr.plainPrint(expr)`/`expr.plainPrint`
 - `Expr.prettyAST(expr)`/`expr.prettyAST`
 - `Expr.plainAST(expr)`/`expr.plainAST`
 - `Type.prettyPrint[A]`/`Type[A].prettyPrint`
 - `Type.plainPrint[A]`/`Type[A].plainPrint`

then you are using Better Printers already. You don't need to additionally add them to your project.

## Installation

[![Hearth Better Printers JVM versions](https://index.scala-lang.org/MateuszKubuszok/hearth/hearth-better-printers/latest.svg?platform=jvm)](https://central.sonatype.com/search?q=hearth&namespace=com.kubuszok&name=hearth-better-printers_3) <br>

!!! notice "Installation is only necessary if you want to use Better Printers without the core Hearth library."

!!! example "[sbt](https://www.scala-sbt.org/)"

    ```scala
    libraryDependencies += "com.kubuszok" %% "hearth-better-printers" % "{{ hearth_version() }}"
    ```

!!! example "[Scala CLI](https://scala-cli.virtuslab.org/)"
    
    ```scala
    //> using dep com.kubuszok::hearth-better-printers::{{ hearth_version() }}
    ```

## Standalone usage

### When to Use What

**Scala 2:**
- `showCodePretty`: Use when you want to print code as it would appear in source (human-readable Scala code)
- `showRawPretty`: Use when you want to see the raw AST structure (useful for debugging macro implementations)
- `SyntaxHighlight.ANSI`: Use for terminal/console output with colors
- `SyntaxHighlight.plain`: Use when you need plain text without ANSI codes (e.g., for IDE hints or logs)

**Scala 3:**
- `FormattedTreeStructureAnsi`: Use for terminal/console output with colors and formatting
- `FormattedTreeStructure`: Use when you need plain text without ANSI codes (e.g., for IDE hints or logs)

Both provide indentation and formatting improvements over the built-in printers.

!!! example "Scala 2"

    ```scala
    // file: Printers.scala - part of Scala 2 example
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::hearth-better-printers::{{ hearth_version() }}

    import scala.language.experimental.macros
    import scala.reflect.macros.blackbox

    import hearth.treeprinter.{ShowCodePrettyScala2, SyntaxHighlight}

    // Mix-in ShowCodePrettyScala2 using `c` as the name of the `blackbox.Context` value.
    class Printers(val c: blackbox.Context) extends ShowCodePrettyScala2 {

      import c.universe._, c.internal._

      def previewASTImpl[A: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[String] = c.Expr[String](
        q"${showRawPretty(expr.tree, SyntaxHighlight.ANSI)}" // Use SyntaxHighlight.plain for plain text
      )

      def previewCodeImpl[A: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[String] = c.Expr[String](
        q"${showCodePretty(expr.tree, SyntaxHighlight.ANSI)}" // Use SyntaxHighlight.plain for plain text
      )
    }

    object Printers {

      def previewAST[A](expr: A): String = macro Printers.previewASTImpl[A]

      def previewCode[A](expr: A): String = macro Printers.previewCodeImpl[A]
    }
    ```

    ```scala
    // file: Printers.test.scala - part of Scala 2 example
    //> using test.dep org.scalameta::munit::{{ libraries.munit }}

    final class PrintersSpec extends munit.FunSuite {

      test("Print") {
        println(Printers.previewAST(2 + 2))
        println(Printers.previewCode(2 + 2))
      }
    }
    ```

!!! example "Scala 3"

    ```scala
    // file: Printers.scala - part of Scala 3 example
    //> using scala {{ scala.3 }}
    //> using dep com.kubuszok::hearth-better-printers::{{ hearth_version() }}

    import scala.quoted.*

    import hearth.treeprinter.ShowCodePrettyScala3

    // Mix-in ShowCodePrettyScala3 using `quotes` as the name of the `Quotes` value.
    class Printers(using val quotes: Quotes) extends ShowCodePrettyScala3 {

      import quotes.*, quotes.reflect.*

      def previewAST[A: Type](expr: Expr[A]): Expr[String] = Expr(
        expr.asTerm.show(using FormattedTreeStructureAnsi) // Use FormattedTreeStructure for plain text
      )
      
      def previewCode[A: Type](expr: Expr[A]): Expr[String] = Expr(
        expr.asTerm.show(using Printer.TreeAnsiCode) // For code-like output, use Printer.TreeAnsiCode
      )
    }

    object Printers {

      inline def previewAST[A](inline expr: A): String = ${ previewASTImpl('{ expr }) }
      def previewASTImpl[A: Type](expr: Expr[A])(using q: Quotes): Expr[String] = (new Printers).previewAST(expr)
    }
    ```

    ```scala
    // file: Printers.test.scala - part of Scala 3 example
    //> using test.dep org.scalameta::munit::{{ libraries.munit }}

    final class PrintersSpec extends munit.FunSuite {

      test("Print") {
        println(Printers.previewAST(2 + 2))  // Shows formatted AST structure
        println(Printers.previewCode(2 + 2))  // Shows formatted code
      }
    }
    ```

## Comparison with Built-in Printers

| Feature           | Scala 2 Built-in | Better Printers (Scala 2) | Scala 3 Built-in       | Better Printers (Scala 3) |
|-------------------|------------------|---------------------------|------------------------|---------------------------|
| Code rendering    |                  |                           |                        |                           |
| ANSI colors       | ❌               | ✅                        | ✅ (`TreeAnsiCode`)    | ✅                        |
| Indentation       | ❌               | ✅                        | ✅                     | ✅                        |
| Tree rendering    |                  |                           |                        |                           |
| ANSI colors       | ❌               | ✅                        | ❌                     | ✅                        |
| Indentation       | ❌               | ✅                        | ❌                     | ✅                        |

!!! tip
    If you're using `MacroCommons` from Hearth's Basic Utilities, you already have access to Better Printers through methods like `expr.prettyPrint` and `expr.prettyAST`. You don't need to use Better Printers directly unless you're building a standalone macro library.
