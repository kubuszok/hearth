# Cross Quotes

Cross Quotes is a library that provides a unified way to write quoted expressions and type operations that can be compiled on both Scala 2 and Scala 3. It bridges the gap between the different metaprogramming APIs in Scala 2 and Scala 3, allowing you to write code that works across both versions.

!!! warning "Work in Progress - follow [hearth#56](https://github.com/MateuszKubuszok/hearth/issues/56) to see the progress of documenting Hearth."

## Installation

[![Hearth Cross Quotes JVM versions](https://index.scala-lang.org/MateuszKubuszok/hearth/hearth-cross-quotes/latest.svg?platform=jvm)](https://central.sonatype.com/search?q=hearth&namespace=com.kubuszok&name=hearth-cross-quotes_3) <br>

!!! example "[sbt](https://www.scala-sbt.org/)"

    ```scala
    libraryDependencies ++= CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((3, _)) => Seq(compilerPlugin("com.kubuszok" % "hearth-cross-quotes" % "{{ hearth_version() }}_3"))
      case _            => Seq()
    }
    ```

!!! example "[Scala CLI](https://scala-cli.virtuslab.org/)"
    
    ```scala
    //> using target.scala {{ scala.3 }}
    //> using plugin com.kubuszok::hearth-cross-quotes::{{ hearth_version() }}
    ```

!!! warning

    Cross Quotes are implemented as macros on Scala 2 (so they are automatically pulled in with the rest of the library),
    and as a compiler plugin on Scala 3 - so they have to be added **only** on Scala 3.

## The Problem

Scala 2 and Scala 3 have fundamentally different metaprogramming systems:

- **Scala 2**: Uses macros with `scala.reflect.macros.blackbox.Context` and quasiquotes (`q""`, `tq""`, etc)
- **Scala 3**: Uses the new quotes system with `scala.quoted.Expr`, `scala.quoted.Type`, `scala.quoted.Quotes` and proper quotes
  and splices (`'{}` and `${}`)

This means that code using metaprogramming features cannot be shared between Scala 2 and Scala 3 without significant duplication or complex abstractions.

## The Solution

Cross Quotes provides a unified API that gets transformed into the appropriate native syntax for each Scala version:

- **Scala 2**: Uses a macro to transform Cross Quotes syntax into native **quasiquotes**
- **Scala 3**: Uses a compiler plugin to transform Cross Quotes syntax into native **quotes**

Additionally, under the hood, it passes around and sets some local copy of `blackbox.Context`/`scala.quoted.Quotes`,
that makes the whole process possible.

## Usage

### Basic Type Operations

#### `Type.of[A]`

Get a type representation that works in both Scala 2 and Scala 3:

!!! example "Type without arguments"

    ```scala
    val StringType = Type.of[String]
    ```

This gets transformed to:

!!! example "Scala 2 macro expansion result"

    ```scala
    val StringType = weakTypeTag[String].asInstanceOf[Type[String]]
    ```

!!! example "Scala 3 compiler plugin rewrite"

    ```scala
    val StringType = scala.quoted.Type.of[String]
    ```

If there are some type parameters:

!!! example "Type with type parameters"

    ```scala
    def OptionType[A: Type] = Type.of[Option[A]]
    ```

then it would also handle converting them as well:

!!! example "Scala 2 macro expansion result"

    ```scala
    def OptionType[A: Type] = {
      implicit val A: WeakTypeTag[A] = Type[A].asInstanceOf[WeakTypeTag[A]]
      weakTypeTag[Option[A]].asInstanceOf[Type[Option[A]]]
    }
    ```

!!! example "Scala 3 compiler plugin rewrite"

    ```scala
    def OptionType[A: Type] = {
      given A: scala.quoted.Type[A] = Type[A].asInstanceOf[scala.quoted.Type[A]]
      scala.quoted.Type.of[Option[A]]
    }
    ```

!!! warning

    Both `weakTypeTag[A]` and `scala.quoted.Type.of[A]` would pick up implicit if it was in the scope.

    So writing code like this:

    ```scala
    implicit val A: Type[A] = Type.of[A]
    ```

    generates:

    ```scala
    implicit val A: Type[A] = A // infinite recursion
    ```

    Therefore I **strongly** recommend to run `Type.of` in the scope where its results are **not** being pulled in as `implicit`s/`given`s.

    It will be necessary as there are no ad-hoc available instances for types, that were not passed e.g. via implicits

#### Type Constructor Operations

Cross Quotes supports type constructors with up to 22 type parameters:

!!! example "`Type.CtorN[F]` and `Type.CtorN.of[F]` syntax"

    ```scala
    // For type constructors with 1 type parameter
    val optionCtor: Type.Ctor1[Option] = Type.Ctor1.of[Option]

    // For type constructors with 2 type parameters  
    val eitherCtor: Type.Ctor2[Either] = Type.Ctor2.of[Either]

    // For type constructors with 3 type parameters
    val tuple3Ctor: Type.Ctor3[Tuple3] = Type.Ctor3.of[Tuple3]

    // And so on up to Type.Ctor22.of
    ```

Type constructors provide `.apply` and `.unapply` methods to create `Type` and match on it.

!!! example "`Type.CtorN[F]` `.apply` and `.unapply` syntax"

    ```scala
    val optionCtor: Type.Ctor1[Option]

    // We can apply a type to type constructor:
      
    implicit val optionString: Type[String]
    val optionString: Type[Option[String]] = optionCtor[String]

    // And we can unapply it:

    val unknownType: ??
    unknownType.Underlying match {
      case optionCtor(innerType : ??) => // unknownType is Option[innerType]
      case _                          => // unknownType is something else
    }
    ```

You can also specify bounds:

!!! example "`Type.CtorN[F]` bounds"

    ```scala
    // Upper bounded
    val upperBounded = Type.Ctor1.UpperBounded.of[String, Option]

    // Bounded with both lower and upper bounds
    val bounded = Type.Ctor1.Bounded.of[Nothing, String, Option]
    ```

### Expression Quoting and Splicing

#### `Expr.quote`

As the name suggest, it _quotes_ an expression to create its typed AST representation:

!!! example "Quotation of an expression"

    ```scala
    def simpleExpr: Expr[String] = Expr.quote {
      "Hello, World!"
    }

    def complexExpr: Expr[Int] = Expr.quote {
      val x = 1
      val y = 2
      x + y
    }
    ```

These gets transformed to:

!!! example "Scala 2 macro expansion result"

    ```scala
    def simpleExpr: Expr[String] =  c.Expr[String](q"""
      "Hello, World"
    """).asInstanceOf[Expr[String]]

    def complexExpr: Expr[Int] = c.Expr[Int](q"""
      val x = 1
      val y = 2
      x + y
    """).asInstanceOf[Expr[Int]]
    ```

!!! example "Scala 3 compiler plugin rewrite"

    ```scala
    def simpleExpr: Expr[String] = '{
      "Hello, World"
    }.asInstanceOf[Expr[String]]

    def complexExpr: Expr[Int] = '{
      val x = 1
      val y = 2
      x + y
    }.asInstanceOf[Expr[Int]]
    ```

which means that we are writing the code we want to finally get, IDE can provide all intellisense, syntax highlighting etc,
but what is actually being generated, is an expression's AST. 

#### `Expr.splice`

As its name suggest it, _splices_ (or _unquotes_) an AST of an expression back into the code:

!!! example "Splicing of an expression"

    ```scala
    def combineExpressions: Expr[String] = {
      val e1 = Expr.quote(1)
      val e2 = Expr.quote(2)
      
      Expr.quote {
        val a = Expr.splice(e1) + Expr.splice(e2)
        a.toString
      }
    }
    ```

This gets transformed to:

!!! example "Scala 2 macro expansion result"

    ```scala
    def combineExpressions: Expr[String] = {
      val e1 = c.Expr[Int](q"""1""").asInstanceOf[Expr[Int]]
      val e2 = c.Expr[Int](q"""2""").asInstanceOf[Expr[Int]]
      
      c.Expr[String](q"""
        val a = ${e1.asInstanceOf[c.Expr[Int]]} + ${e2.asInstanceOf[c.Expr[Int]]}
        a.toString
      """).asInstanceOf[Expr[String]]
    }
    ```

!!! example "Scala 3 compiler plugin rewrite"

    ```scala
    def combineExpressions: Expr[String] = {
      val e1 = '{ 1 }.asInstanceOf[Expr[Int]]
      val e2 = '{ 2 }.asInstanceOf[Expr[Int]]
      
      '{
        val a = ${e1.asInstanceOf[scala.quoted.Expr[Int]]} + ${e2.asInstanceOf[scala.quoted.Expr[Int]]}
        a.toString
      }.asInstanceOf[Expr[String]]
    }
    ```

As we can see, if we want to weave-in some quoted expression, we only can do it inside an _another_ quoted expression.

Therefore `Expr.splice` can be used **only directly inside** an `Expr.quote`, and **only** on an `Expr`.

### Working with Type Parameters

Cross Quotes handles type parameters automatically:

!!! example

    ```scala
    def genericExpr[A: Type](e: Expr[A]): Expr[String] = Expr.quote {
      Expr.splice(e).toString
    }
    ```

The `[A: Type]` context bound gets automatically converted to the appropriate syntax for each Scala version.

!!! note "TODO: explain how quotes handle inserting implicit `Type[A]` into the generated code"

### Pattern Matching on Types

You can pattern match on types using the type constructor operations, or using if with `=:=` clause:

!!! example "Pattern matching on types"

    ```scala
    val stringType = Type.of[String]
    val optionCtor = Type.Ctor1.of[Option]
    val eitherCtor = Type.Ctor2.of[Either]

    def analyzeType[In: Type]: Expr[String] = Type[In] match {
      case optionCtor(aParam) =>
        import aParam.Underlying as A
        Expr(s"Option of ${A.plainPrint}")
        
      case eitherCtor(aParam, bParam) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        Expr(s"Either of ${A.plainPrint} and ${B.plainPrint}")

      case str if str =:= stringType => Expr("String type")
        
      case _ => Expr("Unknown type")
    }
    ```

!!! warning

    Scala 3's `Type`s **do not** override `def equals` to behave the same as `=:=` on `TypeRepr` while Scala 2 `WeakTypeTag`s do.

    As a result, a pattern matching like ``case `stringType` =>`` would misleadingly give the expected results on Scala 2,
    and virtually always fail on Scala 3. Unfortunatelly, we cannot fix that in Hearth.

### Nested Expressions

Nested expressions are also correctly handled by Cross Quotes:

!!! example "Nested expressions and splices"

    ```scala
    def nestedExpr: Expr[String] = {
      def intToString(i: Expr[Int]): Expr[String] = Expr.quote {
        Expr.splice(i).toString
      }

      Expr.quote {
        def localMethod(i: Int): String = Expr.splice(intToString(Expr.quote(i)))
        localMethod(42)
      }
    }
    ```

## Examples

### Simple Type Analysis

!!! example

    ```scala
    // file: src/main/scala/TypeAnalysis.scala - part of Simple Type Analysis example
    //> using scala {{ scala.2_13 }} {{ scala.3 }}
    //> using dep com.kubuszok::hearth:{{ hearth_version() }}

    trait TypeAnalysis { this: hearth.MacroTypedCommons =>

      def analyzeOption[A: Type]: Expr[String] = {
        val optionTest = Type.Ctor1.of[Option]
        
        Type[A] match {
          case optionTest(aParam) =>
            import aParam.Underlying as A
            Expr(s"Option[${A.plainPrint}]")
          case _ => Expr("Not an Option")
        }
      }
    }

    object Stub { def main(args: Array[String]): Unit = () }
    ```

    ```scala
    // file: src/main/scala-2/project.scala - part of Simple Type Analysis example
    //> using target.scala {{ scala.2_13 }}
    //> using options -Xsource:3
    ```

    ```scala
    // file: src/main/scala-3/project.scala - part of Simple Type Analysis example
    //> using target.scala {{ scala.3 }}
    //> using plugin com.kubuszok::hearth-cross-quotes::{{ hearth_version() }}
    ```

### Generic Expression Building

!!! example

    ```scala
    // file: src/main/scala/TypeAnalysis.scala - part of Generic Expression Building example
    //> using scala {{ scala.2_13 }} {{ scala.3 }}
    //> using dep com.kubuszok::hearth:{{ hearth_version() }}
    package example

    trait ExpressionBuilder { this: hearth.MacroTypedCommons =>

      // XD, we have a bug to fix here
      def buildList[A: Type](elements: List[Expr[A]]): Expr[List[A]] = elements match {
        case Nil => Expr.quote { List.empty[A] }
        case head :: tail => Expr.quote {
          Expr.splice(head) :: Expr.splice(buildList(tail))
        }
      }
    }
    ```

    ```scala
    // file: src/main/scala-2/project.scala - part of Generic Expression Building example
    //> using target.scala {{ scala.2_13 }}
    //> using options -Xsource:3
    ```

    ```scala
    // file: src/main/scala-3/project.scala - part of Generic Expression Building example
    //> using target.scala {{ scala.3 }}
    //> using plugin com.kubuszok::hearth-cross-quotes::{{ hearth_version() }}
    ```

### Complex Type Matching

!!! example

    ```scala
    // file: src/main/scala/TypeAnalysis.scala - part of Complex Type Matching example
    //> using scala {{ scala.2_13 }} {{ scala.3 }}
    //> using dep com.kubuszok::hearth:{{ hearth_version() }}

    trait ComplexTypeMatching { this: hearth.MacroTypedCommons =>

      def analyzeTuple[In: Type]: Expr[String] = {
        val tuple2Test = Type.Ctor2.of[Tuple2]
        val tuple3Test = Type.Ctor3.of[Tuple3]
        
        Type[In] match {
          case tuple2Test(aParam, bParam) =>
            import aParam.Underlying as A, bParam.Underlying as B
            Expr(s"Tuple2[${A.plainPrint}, ${B.plainPrint}]")
            
          case tuple3Test(aParam, bParam, cParam) =>
            import aParam.Underlying as A, bParam.Underlying as B, cParam.Underlying as C
            Expr(s"Tuple3[${A.plainPrint}, ${B.plainPrint}, ${C.plainPrint}]")
            
          case _ => Expr("Not a supported tuple")
        }
      }
    }
    ```

    ```scala
    // file: src/main/scala-2/project.scala - part of Complex Type Matching example
    //> using target.scala {{ scala.2_13 }}
    //> using options -Xsource:3

    import scala.language.experimental.macros
    import scala.reflect.macros.blackbox

    class Example (val c: blackbox.Context) extends hearth.MacroCommonsScala2 with ComplexTypeMatching {

      def analyzeTupleImpl[In: c.WeakTypeTag]: c.Expr[String] = analyzeTuple[A]
    }
    object Example {

      def analyzeTuple[In]: String = macro Example.analyzeTupleImpl[In]
    }
    ```

    ```scala
    // file: src/main/scala-3/project.scala - part of Complex Type Matching example
    //> using target.scala {{ scala.3 }}
    //> using plugin com.kubuszok::hearth-cross-quotes::{{ hearth_version() }}

    import scala.quoted.*

    class Example(q: Quotes) extends hearth.MacroCommonsScala3(using q), ComplexTypeMatching

    object Example {

      def analyzeTuple[In] = ${ analyzeTupleImpl[In] }

      def analyzeTupleImpl[In: Type](using q: Quotes): Expr[String] = new Example(q).analyzeTuple[In]
    }
    ```

    ```scala
    // file: src/test/scala/ExampleSpec.scala - part of Complex Type Matching example
    //> using test.dep org.scalameta::munit::{{ libraries.munit }}

    /** Macro implementation of [[Show]] is in [[ShowMacrosImpl]]. */
    final class ExampleSpec extends munit.FunSuite {

      test("should show type of a tuple 2 or 3") {

        assertEquals(Example.analyzeTuple[(Int, String)], "Tuple2[scala.Int, java.lang.String]")
        assertEquals(Example.analyzeTuple[(Int, String, Double)], "Tuple3[scala.Int, java.lang.String, scala.Double]")
        assertEquals(Example.analyzeTuple[(Int, String, Double, Long)], "Not a supported tuple")
      }
    }
    ```

## Limitations

1. **Early Version**: Cross Quotes is still in an early development, so some complex expressions may fail with cryptic errors:

    Scala 2 implementation relies on printing the expression inside quasiquotes, but built-in implementation of `show(expr)` might
    print something that is not correct Scala code. E.g. anonymous classes and their constructors require a special workaround which removes
    an illegal Scala syntax. [hearth#66](https://github.com/MateuszKubuszok/hearth/issues/66) is intended to address it.

    Scala 3 implementation works with untyped trees. That means modifies the code before we can reliably tell, whether something is actually
    declared within `MacroCommons` mix-in.

    Both implementations might ommit some implicit `Type[A]` when constructing `Expr`s and `Type`s, especially if they are not passed as
    parameters, resulting in code that _should_ compile, but isn't.

2. **MicroCommons Dependency**: Can only be used inside traits that depend on `MacroCommons`/`MacroTypedCommons`:

    ```scala
    trait MyMacros { this: MacroTypedCommons => // or just this: MacroCommons =>
      // Your Cross Quotes code here
    }
    ```

3. **Type Constructor Limits**: Type constructors are limited to 22 type parameters, and none of them can be a hifger-kinded type:

    You can implement support for some type constructor with higher-kinded type parameters, but it cannot be available out-of-the-box:
    Scala 2's `WeakTypeTag[A]` can store **only a proper type**, while Scala 3's `Type` defines `A <: AnyKind` which can store all kinds,
    but which is not available on Scala 2, so any common interface has to be implemented anew for each supported type kindness.

### Known Issues

- Anonymous classes with complex type parameters may cause parsing issues
- Very deeply nested expressions might hit compiler limits
- Some edge cases in type parameter handling may not work as expected

!!! warning "Most issues with Scala 2 implementation should be addressed after [hearth#66](https://github.com/MateuszKubuszok/hearth/issues/66) is implemented."

## Debugging

You can enable logging to see how Cross Quotes transforms your code:

!!! example "Enabling cross-quotes logging in sbt"

    ```scala
    scalacOptions ++= CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((3, _)) => Seq(
        // set to true OR file1.scala,file2.scala,... if you want to debug cross-quotes generation on Scala 3
        "-P:hearth.cross-quotes:logging=false"
      )
      case Some((2, 13)) => Seq(
        // set to true OR file1.scala,file2.scala,... if you want to debug cross-quotes generation on Scala 2
        "-Xmacro-settings:hearth.cross-quotes.logging=false"
      )
    }
    ```

!!! example "Enabling cross-quotes logging in Scala CLI"

    ```scala
    //> using target.scala {{ scala.2_13 }}
    // set to true OR file1.scala,file2.scala,... if you want to debug cross-quotes generation on Scala 2
    //> using options "-Xmacro-settings:hearth.cross-quotes.logging=false"
    ```

    ```scala
    //> using target.scala {{ scala.3 }}
    //> using plugin com.kubuszok::hearth-cross-quotes::{{ hearth_version() }}
    // set to true OR file1.scala,file2.scala,... if you want to debug cross-quotes generation on Scala 3
    //> using options "-P:hearth.cross-quotes:logging=false"
    ```

This will show you the transformation from Cross Quotes syntax to the native Scala 2/3 syntax.
