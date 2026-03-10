# Cross Quotes

Cross Quotes is a library that provides a unified way to write quoted expressions and type operations that can be compiled on both Scala 2 and Scala 3. It bridges the gap between the different metaprogramming APIs in Scala 2 and Scala 3, allowing you to write code that works across both versions.

## Installation

[![Hearth Cross Quotes JVM versions](https://index.scala-lang.org/MateuszKubuszok/hearth/hearth-cross-quotes/latest.svg?platform=jvm)](https://central.sonatype.com/search?q=hearth&namespace=com.kubuszok&name=hearth-cross-quotes_3) <br>

!!! warning "Cross Quotes is implemented as macros on Scala 2 (so it is automatically pulled in with the rest of the library) and as a compiler plugin on Scala 3 — so it has to be added **only** on Scala 3."

!!! example "[sbt](https://www.scala-sbt.org/)"

    ```scala
    libraryDependencies ++= CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((3, _)) => Seq(
        compilerPlugin("com.kubuszok" % "hearth-cross-quotes" % "{{ hearth_version() }}_3")
      )
      case _            => Seq()
    }
    ```

!!! example "[Scala CLI](https://scala-cli.virtuslab.org/)"
    
    ```scala
    //> using target.scala {{ scala.3 }}
    //> using plugin com.kubuszok::hearth-cross-quotes::{{ hearth_version() }}
    ```

## The Problem

Scala 2 and Scala 3 have fundamentally different metaprogramming systems:

- **Scala 2**: Uses macros with `scala.reflect.macros.blackbox.Context` and quasiquotes (`q""`, `tq""`, etc)
- **Scala 3**: Uses the new quotes system with `scala.quoted.Expr`, `scala.quoted.Type`, `scala.quoted.Quotes` and proper quotes
  and splices (`'{}` and `${}`)

This means that code using metaprogramming features cannot be shared between Scala 2 and Scala 3 without significant duplication or complex abstractions.

!!! tip "If you only need to work with Scala 2/Scala 3, you might be fine just sticking to quasi-quotes/quotes."

## The Solution

Cross Quotes provides a unified API that gets transformed into the appropriate native syntax for each Scala version:

- **Scala 2**: Uses a macro to transform Cross Quotes syntax into native **quasiquotes**
- **Scala 3**: Uses a compiler plugin to transform Cross Quotes syntax into native **quotes**

Additionally, under the hood, it passes around and sets some local copy of `blackbox.Context`/`scala.quoted.Quotes`,
that makes the whole process possible.

!!! note "Automatic `Quotes` management"

    Because Cross Quotes handle `scala.quoted.Quotes` passing automatically, you do **not** need to use
    `passQuotes` or `withQuotes` when building expressions with `Expr.quote` / `Expr.splice`. These utilities
    are only needed when using native Scala 3 `'{ ... }` / `${ ... }` syntax — see
    [`passQuotes` and `withQuotes`](basic-utilities.md#passquotes-and-withquotes-scala-3-only) for details.

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

then it will also handle converting them:

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

    Both `weakTypeTag[A]` and `scala.quoted.Type.of[A]` pick up an implicit if it is in scope.

    So writing code like this:

    ```scala
    implicit val A: Type[A] = Type.of[A]
    ```

    generates:

    ```scala
    implicit val A: Type[A] = A // infinite recursion
    ```

    Therefore I **strongly** recommend running `Type.of` in the scope where its results are **not** being pulled in as `implicit`s/`given`s.

    This is necessary because there are no ad-hoc available instances for types that were not passed, e.g. via implicits.

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

#### Partial Application with `setA`, `setB`, ..., `setLast`

For type constructors with 2 or more type parameters, you can fix one parameter at a time to produce a smaller type constructor:

!!! example "Partial application of type constructors"

    ```scala
    val eitherCtor: Type.Ctor2[Either] = Type.Ctor2.of[Either]

    // Fix the first parameter (A) to String:
    // Returns a Ctor1 for Either[String, _]
    val eitherStringCtor: Type.Ctor1[Either[String, *]] = eitherCtor.setA[String]

    // Fix the last parameter to Int:
    // Returns a Ctor1 for Either[*, Int]
    val eitherIntCtor: Type.Ctor1[Either[*, Int]] = eitherCtor.setLast[Int]
    ```

The `setX` methods use the naming convention where type parameters are named `A`, `B`, `C`, ..., so `setA` fixes the first, `setB` the second, etc.

!!! tip "Order of partial application"

    When applying multiple parameters, apply them from the back (last parameter first). This way their names (positions) stay unchanged during partial application:

    ```scala
    // Good: names stay stable
    val ctor = Type.Ctor3.of[MyType]
    val result = ctor.setC[Double].setB[String].setA[Int]

    // Less predictable: after setA, the remaining params shift
    val result2 = ctor.setA[Int].setA[String].setA[Double]
    ```

#### Converting Between Typed and Untyped Representations

Type constructors can be converted to and from their untyped (platform-specific) representation:

!!! example "`asUntyped` and `fromUntyped`"

    ```scala
    val optionCtor: Type.Ctor1[Option] = Type.Ctor1.of[Option]

    // Convert to platform-specific representation
    // (c.universe.Type on Scala 2, TypeRepr on Scala 3)
    val untyped: UntypedType = optionCtor.asUntyped

    // Reconstruct from untyped (e.g. after receiving from another API)
    val reconstructed: Type.Ctor1[Option] = Type.Ctor1.fromUntyped[Option](untyped)

    // The reconstructed ctor works identically:
    val optionInt: Type[Option[Int]] = reconstructed[Int]
    ```

This is useful when you need to pass type constructors through APIs that work with untyped representations, or when extracting a type constructor from a `TypeRepr`/`c.universe.Type` that you obtained through reflection.

### Expression Quoting and Splicing

#### `Expr.quote`

As the name suggests, it _quotes_ an expression to create its typed AST representation:

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

These get transformed to:

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

This means that we are writing the code we ultimately want to produce, so the IDE can provide all IntelliSense, syntax highlighting, etc.,
while what actually gets generated is the expression's AST. 

#### `Expr.splice`

As its name suggests, it _splices_ (or _unquotes_) an AST of an expression back into the code:

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
        val a = ${e1.asInstanceOf[scala.quoted.Expr[Int]]} +
          ${e2.asInstanceOf[scala.quoted.Expr[Int]]}
        a.toString
      }.asInstanceOf[Expr[String]]
    }
    ```

As we can see, if we want to weave in a quoted expression, we can only do it inside another quoted expression.

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

!!! info "How `Type[A]` works inside cross-quotes"

    When you write `Expr.quote { ... }` with a type parameter `[A: Type]`, Cross Quotes needs to make
    that type information available inside the generated code. How this works differs between Scala versions:

    - **Scala 2**: `Type[A]` is backed by `c.WeakTypeTag[A]`. Cross Quotes generates an implicit conversion
      (`convertProvidedTypesForCrossQuotes`) that transforms Hearth's `Type[A]` into the `WeakTypeTag[A]`
      required by Scala 2 quasiquotes.
    - **Scala 3**: `Type[A]` is backed by `scala.quoted.Type[A]`. Scala 3 `'{ ... }` blocks natively accept
      `given Type[A]` in scope, so no conversion is needed.

    The `[A: Type]` context bound provides type information to both platforms transparently. You write your
    code once using Hearth's `Type[A]`, and Cross Quotes ensures it compiles correctly on both Scala 2 and 3.

### Pattern Matching on Types

You can pattern match on types using the type constructor operations, or by using an `if` with an `=:=` clause:

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

    As a result, a pattern like ``case `stringType` =>`` would misleadingly give the expected results on Scala 2
    and virtually always fail on Scala 3. Unfortunately, we cannot fix that in Hearth.

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

### Local Type Parameters in Splices

When a method defined inside `Expr.quote` has its own type parameters, those type parameters can be used
with `Type.of` inside `Expr.splice`. This is the key building block for generating type class instances
where methods have their own type parameters (e.g., `Functor.map[A, B]`).

!!! example "Using Type.of with local type params"

    ```scala
    def example: Expr[Data] =
      Expr.quote {
        def helper[A]: String = Expr.splice {
          hearth.fp.ignore(Type.of[A]) // Type.of[A] works for local type param A
          Expr("ok")
        }
        Data(helper[Int])
      }
    ```

!!! example "Functor skeleton with HKT + local type params"

    ```scala
    def functorSkeleton[F[_]](implicit FC: Type.Ctor1[F]): Expr[Data] = {
      hearth.fp.ignore(FC)
      Expr.quote {
        val functor: Functor[F] = new Functor[F] {
          def map[A, B](fa: F[A])(f: A => B): F[B] = {
            // Type.of[A] and Type.of[B] are available inside the splice
            val _evidence: String = Expr.splice {
              hearth.fp.ignore(Type.of[A], Type.of[B])
              Expr("ok")
            }
            null.asInstanceOf[F[B]] // placeholder
          }
        }
        // ...
      }
    }
    ```

!!! info "How local type params work across Scala versions"

    - **Scala 3**: Works natively. The staging system automatically provides `scala.quoted.Type[A]` for
      type params from enclosing `'{ ... }` blocks. No special handling needed.
    - **Scala 2**: Cross Quotes generates a workaround method with the same type param names as the originals
      and implicit `WeakTypeTag` context bounds. Free type symbols (`newFreeType("A")`) are passed as arguments,
      which the quasiquote system resolves by name to the actual type params in the generated code.

!!! example "Nested `Expr.quote` inside `Expr.splice` — Functor derivation"

    Nested `Expr.quote` inside `Expr.splice` works with local type params. This enables the
    quote-splice-quote pattern needed for type class derivation:

    ```scala
    // Derive Functor[List] using nested quotes
    def deriveFunctorList: Expr[Data] =
      Expr.quote {
        val functor: Functor[List] = new Functor[List] {
          def map[A, B](fa: List[A])(f: A => B): List[B] = Expr.splice {
            val faExpr: Expr[List[A]] = Expr.quote(fa)
            val fExpr: Expr[A => B] = Expr.quote(f)
            Expr.quote {
              Expr.splice(faExpr).map(Expr.splice(fExpr))
            }
          }
        }
        Data(functor.map(List(1, 2, 3))(_.toString).mkString(", "))
      }
    ```

### Best Practice: Passing `Type.of` Directly at Call Sites

When calling helper methods inside `Expr.splice` that need `Type` values for local type params,
**pass `Type.of[A]` directly as an explicit argument** at the call site rather than binding it
to a local `implicit val` or using context bounds:

!!! example "Recommended pattern"

    ```scala
    def deriveFunctor[F[_]](implicit FCtor: Type.Ctor1[F]): Expr[Data] = {
      hearth.fp.ignore(FCtor)
      Expr.quote {
        new Functor[F] {
          def map[A, B](fa: F[A])(f: A => B): F[B] =
            Expr.splice {
              // Pass Type.of[A] and Type.of[B] directly where implicits are expected
              deriveFunctorMapBody[F, A, B](FCtor, Expr.quote(fa), Expr.quote(f))(
                Type.of[A],
                Type.of[B]
              )
            }
        }
      }
    }

    // Helper method receives types as explicit implicit parameters
    def deriveFunctorMapBody[F[_], A, B](
      fCtor: Type.Ctor1[F],
      fa: Expr[F[A]],
      f: Expr[A => B]
    )(implicit
      A: Type[A],
      B: Type[B]
    ): Expr[F[B]] = {
      // A and B are fully usable here — passed explicitly, no circular resolution
      // fCtor, fa, f are also available
      ???
    }
    ```

**Why this pattern works and alternatives don't:**

1. **`implicit val evA: Type[A] = Type.of[A]` — circular resolution.** Cross Quotes expands
   `Type.of[A]` by looking for an implicit `Type[A]` in scope. If the result is assigned to
   an implicit val with the same type, the compiler resolves `Type.of[A]` to the val being
   initialized — infinite recursion. This is the same issue described in
   [the self-referential implicit gotcha](#known-issues-and-gotchas).

2. **`val tpeA = Type.of[A]; someHelper[A](tpeA)` — works but less ergonomic.** Using a
   non-implicit `val` avoids circular resolution, but forces the helper to take `Type` as a
   regular parameter rather than an implicit, making the API less natural.

3. **`Type.of[A]` at the call site — best of both worlds.** The helper declares its type
   params with normal implicit/context-bound syntax. At the call site, `Type.of[A]` is
   evaluated in a scope where no conflicting implicit exists yet, and the result is passed
   directly to the implicit parameter. No circular resolution, no extra bindings.

!!! tip "Key points to remember"

    - **Always pass `Type.of[A]` at the call site**, in the position where the implicit
      `Type[A]` parameter is expected. Don't bind it to a local implicit first.
    - **`def` helpers with implicit `Type` params** are the natural way to structure code that
      needs local type params — the `Type.of` values flow in as explicit implicit arguments.
    - **This composes with `Type.Ctor1[F]`** — the HKT type constructor can be passed as a
      regular parameter alongside the explicit `Type.of` arguments.
    - **Works on both Scala 2 and Scala 3** — on Scala 2, the free type symbols created by
      the workaround are passed through; on Scala 3, `Type.of[A]` is natively available inside
      splice bodies.

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

      def buildList[A: Type](elements: List[Expr[A]]): Expr[List[A]] = elements match {
        case Nil => Expr.quote { List.empty[A] }
        case head :: tail => Expr.quote {
          Expr.splice(head) :: Expr.splice(buildList(tail))
        }
      }
    }

    object Stub { def main(args: Array[String]): Unit = () }
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

    class Example(val c: blackbox.Context)
        extends hearth.MacroCommonsScala2
        with ComplexTypeMatching {

      def analyzeTupleImpl[In: c.WeakTypeTag]: c.Expr[String] = analyzeTuple[In]
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

      inline def analyzeTuple[In] = ${ analyzeTupleImpl[In] }

      def analyzeTupleImpl[In: Type](using q: Quotes): Expr[String] =
        new Example(q).analyzeTuple[In]
    }
    ```

    ```scala
    // file: src/test/scala/ExampleSpec.scala - part of Complex Type Matching example
    //> using test.dep org.scalameta::munit::{{ libraries.munit }}

    /** Macro implementation of [[Show]] is in [[ShowMacrosImpl]]. */
    final class ExampleSpec extends munit.FunSuite {

      test("should show type of a tuple 2 or 3") {

        assertEquals(
          Example.analyzeTuple[(Int, String)],
          "Tuple2[scala.Int, java.lang.String]"
        )
        assertEquals(
          Example.analyzeTuple[(Int, String, Double)],
          "Tuple3[scala.Int, java.lang.String, scala.Double]"
        )
        assertEquals(
          Example.analyzeTuple[(Int, String, Double, Long)],
          "Not a supported tuple"
        )
      }
    }
    ```

### Partial Application of Type Constructors

!!! example

    ```scala
    // file: src/main/scala/TypeAnalysis.scala - part of Partial Application example
    //> using scala {{ scala.2_13 }} {{ scala.3 }}
    //> using dep com.kubuszok::hearth:{{ hearth_version() }}

    trait PartialApplication { this: hearth.MacroTypedCommons =>

      /** Checks if a type matches F[_, _] with the first parameter fixed to Fixed. */
      def isFixedEither[Fixed: Type, In: Type]: Expr[Boolean] = {
        // Fix the first type parameter of Either to Fixed
        val eitherFixedCtor = Type.Ctor2.of[Either].setA[Fixed]

        Type[In] match {
          case eitherFixedCtor(_) => Expr(true)
          case _                  => Expr(false)
        }
      }
    }

    object Stub { def main(args: Array[String]): Unit = () }
    ```

    ```scala
    // file: src/main/scala-2/project.scala - part of Partial Application example
    //> using target.scala {{ scala.2_13 }}
    //> using options -Xsource:3

    import scala.language.experimental.macros
    import scala.reflect.macros.blackbox

    class Example(val c: blackbox.Context)
        extends hearth.MacroCommonsScala2
        with PartialApplication {

      def isFixedEitherImpl[Fixed: c.WeakTypeTag, In: c.WeakTypeTag]
          : c.Expr[Boolean] = isFixedEither[Fixed, In]
    }
    object Example {

      def isFixedEither[Fixed, In]: Boolean = macro Example.isFixedEitherImpl[Fixed, In]
    }
    ```

    ```scala
    // file: src/main/scala-3/project.scala - part of Partial Application example
    //> using target.scala {{ scala.3 }}
    //> using plugin com.kubuszok::hearth-cross-quotes::{{ hearth_version() }}

    import scala.quoted.*

    class Example(q: Quotes) extends hearth.MacroCommonsScala3(using q), PartialApplication

    object Example {

      inline def isFixedEither[Fixed, In]: Boolean = ${ isFixedEitherImpl[Fixed, In] }

      def isFixedEitherImpl[Fixed: Type, In: Type](using q: Quotes): Expr[Boolean] =
        new Example(q).isFixedEither[Fixed, In]
    }
    ```

    ```scala
    // file: src/test/scala/ExampleSpec.scala - part of Partial Application example
    //> using test.dep org.scalameta::munit::{{ libraries.munit }}

    final class ExampleSpec extends munit.FunSuite {

      test("should match Either[String, _] using partial application") {

        assertEquals(Example.isFixedEither[String, Either[String, Int]], true)
        assertEquals(Example.isFixedEither[String, Either[String, Double]], true)
        assertEquals(Example.isFixedEither[String, Either[Int, String]], false)
        assertEquals(Example.isFixedEither[String, Option[String]], false)
      }
    }
    ```

## Limitations

Since Cross-Quotes rewrites some code into the native macro representations of each Scala version, their limitations are a direct result of those representations.

 1. **Scala 2's `WeakTypeTag` implicit resolution**

    When constructing a new value with `c.weakTypeTag[SomeType]`, Scala 2 ignores:

    ```scala
    type X
    implicit val X: WeakTypeTag[X] = ...

    object someType {
      type Y = ...
      implicit val Y: WeakTypeTag[Y] = ...
    }
    import someType.Y
    ```

    As a result, if you need `Type[Either[X, Y]]`, it generates `WeakTypeTag[Either[X, Y]]` for local, abstract types,
    not the types represented by the implicits!

    However, it does not ignore type parameters and their implicits:

    ```scala
    def someMethod[A: WeakTypeTag] =
      weakTypeTag[Option[A]]

    def anotherMethod[B](implicit val B: WeakTypeTag[B]) =
      weakTypeTag[Either[String, B]]
    ```

    So Cross-Quotes tries to detect such situations and rewrites `Type.of[...]` to use that workaround under the hood.
    But it's a **best-effort approach**; passing `Type[A]` as a type bound is much more bulletproof.

 2. **Scala 2's `WeakTypeTag` lacks kind support**

    Scala 3's `Type[A]` uses the new `A <: AnyKind` type bound, so it can contain `String` (proper type), `List[_]`, `Either[_, _]`.

    Scala 2's `WeakTypeTag[A]` has to store a proper type `A`, because there is no `AnyKind` in Scala 2. So obtaining a type constructor requires manual intervention.

    That forced us to create `Type.CtorN` to have a structure allowing us to `apply` and `unapply` `Type`s. They have to be maintained manually,
    so we only support `Type.Ctor1` to `Type.Ctor22`, each accepting only proper types as type parameters.

    However, Scala 2 is also much worse than Scala 3 at figuring out which types have to be applied to a type constructor
    [to match a type alias](https://scastie.scala-lang.org/MateuszKubuszok/eGoFqkeUSGarPvoeo3OZow/7).

    This means that `Type.Ctor2.of[TypeAliasSwappingParams]` would `apply` the value, but it won't figure out the parameters that have to be passed.
    (There is a best-effort approach that matches a `Type` against a type alias only when it's using that exact alias, with no unaliasing, widening, etc).

    For the same reason, using [kind-projector](https://github.com/typelevel/kind-projector) on Scala 2 cannot be supported.

    We might improve the support for type constructors in the future but
    **full support for arbitrary type aliases and kind projections will probably never be possible**.

    However, quite often it can be workarounded by using unaliased full type constructor, and then partially applying some types to it:

    ```scala
    // Assuming given Type[String]
    Type.Ctor2.of[Either].setA[String] // =:= Type.Ctor1[Either[String, *]]
    // Assuming given Type[Int]
    Type.Ctor2.of[Either].setB[Int] // =:= Type.Ctor1[Either[*, Int]]
    ```

    Names `setA`, `setB`, `setC` etc assumes convention where type parameters of each type constructor are named: `A`, `B`, `C`, ...

    Since:

     - we are fixing 1 type parameter at a time
     - fixing the type paramter returns a type constructor if arity 1 smaller
     - type parameters would be reindexed

    to apply multiple type parameters we recommend applying them from the back - this way their names (positions) will stay unchanged
    over the whole proces of partial application (e.g. use `ctor.setP[P].setM[M].setA[A]` but not `ctor.setA[A].setM[M].setP[P]`).

 3. **Scala 2's `Expr`s**

    Quasiquotes do not resolve implicit `WeakTypeTag`s out of the box. You have to interpolate them yourself:

    ```scala
    q"""$expr.asInstanceOf[${weakTypeTag[A]}]"""
    ```

    And all of the types defined in the interpolated `String` should be fully qualified type names. Even if some type is imported,
    the macro would only see the types that are available in the scope during the expansion.

    For that reason the Scala 2 implementation relies on custom tree printers ([hearth#66](https://github.com/MateuszKubuszok/hearth/issues/66)).
    It takes a well-typed `Expr`, prints it as something preserving all these (imported/global) types, and injects `weakTypeTag`s where necessary.

    Since it's quite a complex process, there might still be issues with it, so **we recommend keeping quoted expressions as simple as possible**.

 4. **Scala 3's `Type.of[A]` implicit resolution**

    Scala 3's `Type.of[A]` and `'{ ... }: Expr[A]` have no issues picking up the local implicit `Type`s, like Scala 2 does.

    However, they require that each such implicit is defined as an `implicit val`/parameterless `given`. If obtaining such
    `Type` would involve any sort of implicit resolution, it would be ignored.

    For that reason Cross-Quotes uses a best-effort approach to detect such cases and create local `given`s to store resolved values.
    But, just like on Scala 2, **it's more reliable to use a `def` with type bounds**, where implicits are resolved before being passed
    into it, and the `Type.of` inside just picks the resolved values.

 5. **Scala 3's compiler-plugin tree-rewriting**

    `Type.of`, `Expr.quote` and `Expr.splice` can only be rewritten by the compiler plugin before the trees are typed.

    That means we cannot rely on resolved symbols or make sure that tree rewriting happens only for targeted methods.

    Cross-Quotes has to assume that types like `Type[A]`, `Expr[A]`, `CrossQuotes`, ... are present and represent the types defined
    in `MacroCommons`/`MacroTypedCommons`.

    **Using Cross-Quotes with code that, e.g., uses `scala.quoted.Type.of` directly is undefined behavior (and the compiler will probably crash)**.

While all of these are inconvenient, they can usually be worked around. The issues they cause typically occur when we are compiling the macro code, not when the user expands it, so they shouldn't be a problem for end users.

 6. **Local type params inside `Expr.splice` inside `Expr.quote`**

    When a method inside `Expr.quote` defines its own type parameters (e.g., `def map[A, B](...)`),
    `Type.of[A]` and `Type.of[B]` can be used inside `Expr.splice` to access those type params at
    macro-expansion time.

    **What works:**

    - `Type.of[A]`, `Type.of[B]` for local type params inside splices
    - Multiple local type params simultaneously
    - Combining HKT (`Type.Ctor1[F]`) with local type params in the same splice
    - Nested `Expr.quote` inside `Expr.splice` with local type params (including multi-level
      quote-splice-quote-splice-quote composition)
    - Type class derivation patterns (e.g., deriving `Functor[List]` with nested quotes)
    - Using `Type.Ctor1[F]` from an outer enclosing method inside a splice whose nearest method
      is different (e.g. `map[A, B]`) — the outer method's implicits are found automatically

### Known Issues and Gotchas

!!! warning "Self-referential implicit `Type` / `Type.CtorN` initialization"

    Both `weakTypeTag[A]` (Scala 2) and `scala.quoted.Type.of[A]` (Scala 3) pick up an implicit if it is in scope.
    Writing:

    ```scala
    implicit val A: Type[A] = Type.of[A]
    ```

    generates `implicit val A: Type[A] = A` — an infinite recursion. **Run `Type.of` in a scope where its result
    is not being pulled in as an implicit/given.**

    The same applies to `Type.CtorN` and to `Type.of` with local type params inside `Expr.splice`:

    ```scala
    // BAD — circular initialization:
    implicit val FC: Type.Ctor1[Option] = Type.Ctor1.of[Option]

    // GOOD — create the value outside, then assign:
    val optCtor = Type.Ctor1.of[Option] // or a factory method
    implicit val FC: Type.Ctor1[Option] = optCtor
    ```

    ```scala
    // BAD — inside Expr.splice, this causes a forward reference error on Scala 2:
    Expr.splice {
      implicit val evA: Type[A] = Type.of[A]
      someHelper[A]
    }

    // GOOD — use non-implicit vals and pass explicitly:
    Expr.splice {
      val tpeA = Type.of[A]
      val tpeB = Type.of[B]
      someHelper[A, B](tpeA, tpeB)
    }
    ```

!!! warning "Free types from local type params do not cross quasiquote boundaries (Scala 2)"

    On Scala 2, when `Expr.splice` inside `Expr.quote` uses local type params (e.g. `A`, `B` from
    `def map[A, B]` inside the quote body), these are resolved via free type symbols in the quasiquote.
    Free types are resolved **only within the quasiquote where they were created** — they do not propagate
    into separately-expanded quasiquotes.

    This means that if a helper method's `Expr.quote` expands into its own quasiquote, and that quasiquote
    references local type params that come from the outer workaround, those free types will fail to resolve:

    ```scala
    // BAD on Scala 2 — "free type variable A" error:
    // mapBody's Expr.quote expands into a separate quasiquote
    def mapBody[A: Type, B: Type](
      fa: Expr[List[A]], f: Expr[A => B]
    ): Expr[List[B]] =
      Expr.quote { Expr.splice(fa).map(Expr.splice(f)) }

    Expr.quote {
      def map[A, B](fa: List[A])(f: A => B): List[B] = Expr.splice {
        // A, B here are free types from the workaround —
        // they won't resolve inside mapBody's separate quasiquote
        mapBody[A, B](Expr.quote(fa), Expr.quote(f))
      }
    }
    ```

    **Workaround:** Keep `Expr.quote` calls that reference local type params inline in the splice body.
    Helper methods can do any non-quoting work (build strings, call `Expr(...)`, apply `Type.Ctor1`, etc.)
    — only the `Expr.quote` itself needs to stay inline:

    ```scala
    // GOOD — all Expr.quote calls are inline in the splice body:
    Expr.quote {
      def map[A, B](fa: List[A])(f: A => B): List[B] = Expr.splice {
        val faExpr: Expr[List[A]] = Expr.quote(fa)
        val fExpr: Expr[A => B] = Expr.quote(f)
        Expr.quote {
          Expr.splice(faExpr).map(Expr.splice(fExpr))
        }
      }
    }
    ```

    This limitation does not affect Scala 3, where the staging system handles type params natively across
    quote boundaries.

!!! warning "`val` vs `def` for `Expr` values — Quotes scope capture (Scala 3)"

    On Scala 3, every `Expr` is tied to the `Quotes` instance that created it. The cross-quotes plugin
    injects `given scala.quoted.Quotes = CrossQuotes.ctx` as a **val** (evaluated once), and
    `CrossQuotes.nestedCtx` dynamically updates the current context when entering splice bodies.

    If you store an `Expr` produced by a Hearth utility (`LambdaBuilder.build`, `ValDefBuilder.build`,
    etc.) in a **`val`**, the `Expr` is bound to the `Quotes` scope active at that point. When that
    `Expr` is later spliced inside `Expr.quote` — which enters a nested `Quotes` — the staging system
    detects a scope mismatch.

    **Use `def` instead of `val`** to re-evaluate the builder each time, picking up the current
    dynamic `Quotes` context:

    ```scala
    // BAD — lambda is captured at the outer Quotes scope:
    val lambda = LambdaBuilder.of1[A, B].map(...).build
    Expr.quote { Expr.splice(list).map(Expr.splice(lambda)) }
    // Fails: wrong staging level

    // GOOD — lambda is re-evaluated at the correct scope:
    def lambda = LambdaBuilder.of1[A, B].map(...).build
    Expr.quote { Expr.splice(list).map(Expr.splice(lambda)) }
    // Works: build runs with the nested Quotes context
    ```

    This is especially subtle when combined with `MIO`: `MIO` is naturally lazy and non-memoizing, so
    wrapping `Expr`-producing code in `MIO` works correctly. But extracting the result into a `val`
    (e.g. `val lambda = mio.runSync`) re-introduces the scope capture problem.

    This issue does not affect Scala 2, where `Expr` values are untyped trees without scope tracking.

!!! note "Very deeply nested expressions"

    Very deeply nested quote-splice-quote chains might hit compiler limits. If you encounter issues,
    try simplifying the nesting structure.

## Debugging

You can enable logging to see how Cross Quotes transforms your code:

!!! example "Enabling cross-quotes logging in sbt"

    ```scala
    scalacOptions ++= CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((3, _)) => Seq(
        // set to true OR file1.scala,file2.scala,...
        // to debug cross-quotes generation on Scala 3
        "-P:hearth.cross-quotes:logging=false"
      )
      case Some((2, 13)) => Seq(
        // set to true OR file1.scala,file2.scala,...
        // to debug cross-quotes generation on Scala 2
        "-Xmacro-settings:hearth.cross-quotes.logging=false"
      )
    }
    ```

!!! example "Enabling cross-quotes logging in Scala CLI"

    ```scala
    //> using target.scala {{ scala.2_13 }}
    // set to true OR file1.scala,file2.scala,...
    // to debug cross-quotes generation on Scala 2
    //> using options "-Xmacro-settings:hearth.cross-quotes.logging=false"
    ```

    ```scala
    //> using target.scala {{ scala.3 }}
    //> using plugin com.kubuszok::hearth-cross-quotes::{{ hearth_version() }}
    // set to true OR file1.scala,file2.scala,...
    // to debug cross-quotes generation on Scala 3
    //> using options "-P:hearth.cross-quotes:logging=false"
    ```

This will show you the transformation from Cross Quotes syntax to the native Scala 2/3 syntax.
