# Better Printers

Build-in tree/type printers in macros have their limitations:

 - Scala 2's `showCode` and `showRaw`: have no syntax highlighting and no line breaks and indentations, that makes reading code easier
 - Scala 3's `Printer.TreeAnsiCode` and `Printer.TypeReprAnsiCode` are fixing that, but `Printer.TreeStructure` has no support
   for syntax highlighing

This modules addresses these shortcomings.

## Basic Utilities integrations

If you are using `MacroCommons` and their:

 - `Expr.prettyPrint(expr)`/`expr.prettyPrint`
 - `Expr.plainPrint(expr)`/`expr.plainPrint`
 - `Expr.prettyAST(expr)`/`expr.prettyAST`
 - `Expr.plainAST(expr)`/`expr.plainAST`
 - `Type.prettyPrint[A]`/`Type[A].prettyPrint`
 - `Type.plainPrint[A]`/`Type[A].plainPrint`

then, you are using Better Printers already. You don't need to additionally add them to your project.

## Installation

[![Hearth Better Printers JVM versions](https://index.scala-lang.org/MateuszKubuszok/hearth/hearth-better-printers/latest.svg?platform=jvm)](https://central.sonatype.com/search?q=hearth&namespace=com.kubuszok&name=hearth-better-printers_3) <br>

!!! notice "Installation is only necessary if you want to use Better Printers without the core Hearth."

!!! example "[sbt](https://www.scala-sbt.org/)"

    ```scala
    libraryDependencies += "com.kubuszok" %% "hearth-better-printers" % "{{ hearth_version() }}"
    ```

!!! example "[Scala CLI](https://scala-cli.virtuslab.org/)"
    
    ```scala
    //> using dep com.kubuszok::hearth-better-printers::{{ hearth_version() }}
    ```

## Standalone usage

!!! example "Scala 2"

    ```scala
    // file: Printers.scala - part of Scala 2 example
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::hearth-better-printers::{{ hearth_version() }}

    import scala.language.experimental.macros
    import scala.reflect.macros.blackbox

    import hearth.treeprinter.{ShowCodePrettyScala2, SyntaxHighlight}

    // Mix-in ShowCodePrettyScala3 using `q` as the name of the `Quotes` value.
    class Printers(val c: blackbox.Context) extends ShowCodePrettyScala2 {

      import c.universe._, c.internal._

      def previewASTImpl[A: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[String] = c.Expr[String](
        q"${showRawPretty(expr.tree, SyntaxHighlight.ANSI)}" // or SyntaxHighlight.ANSI.plain
      )

      def previewCodeImpl[A: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[String] = c.Expr[String](
        q"${showCodePretty(expr.tree, SyntaxHighlight.ANSI)}" // or SyntaxHighlight.ANSI.plain
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
        expr.asTerm.show(using FormattedTreeStructureAnsi) // or FormattedTreeStructure
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
        println(Printers.previewAST(2 + 2))
      }
    }
    ```
