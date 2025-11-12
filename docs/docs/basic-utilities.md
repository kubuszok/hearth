# Basic Utilities

The Hearth API abstracts away from any particular macro system. It works with abstract types
backed by abstract methods, and later we implement them for a particular macro system.

!!! warning "Work in Progress - follow [hearth#56](https://github.com/MateuszKubuszok/hearth/issues/56) to see the progress of documenting Hearth."

## Installation

[![Hearth JVM versions](https://index.scala-lang.org/MateuszKubuszok/hearth/hearth/latest-by-scala-version.svg?platform=jvm)](https://central.sonatype.com/search?q=hearth&namespace=com.kubuszok&name=hearth_3) <br>
[![Hearth Scala.js 1.x versions](https://index.scala-lang.org/MateuszKubuszok/hearth/hearth/latest-by-scala-version.svg?platform=sjs1)](https://central.sonatype.com/search?q=hearth&namespace=com.kubuszok&name=hearth_sjs1_3) <br>
[![Hearth Scala Native 0.5 versions](https://index.scala-lang.org/MateuszKubuszok/hearth/hearth/latest-by-scala-version.svg?platform=native0.5)](https://central.sonatype.com/search?q=hearth&namespace=com.kubuszok&name=hearth_native0.5_3) <br>

!!! example "[sbt](https://www.scala-sbt.org/)"

    JVM only:

    ```scala
    libraryDependencies += "com.kubuszok" %% "hearth" % "{{ hearth_version() }}"
    ```

    JVM/Scala.js/Scala Native via [sbt-crossproject](https://github.com/portable-scala/sbt-crossproject) or [sbt-projectmatrix](https://github.com/sbt/sbt-projectmatrix)/sbt 2:

    ```scala
    libraryDependencies += "com.kubuszok" %%% "hearth" % "{{ hearth_version() }}"
    ```

!!! example "[Scala CLI](https://scala-cli.virtuslab.org/)"

    JVM only:
    
    ```scala
    //> using dep "com.kubuszok::hearth:{{ hearth_version() }}"
    ```

    JVM/Scala.js/Scala Native:
    
    ```scala
    //> using dep "com.kubuszok::hearth::{{ hearth_version() }}"
    ```

!!! warning

    If you want to cross-compile, you most likely also want to [add cross-quotes compiler plugin for Scala 3](cross-quotes.md#installation).

    It is not needed if you want to use Hearth directly with Scala-2-macros-only or with Scala-3-macros-only.

!!! tip "Examples in this documentation"

    Most examples in this documentation are intended to be runnable.

    That's why most file names follow the conventions from [sbt](https://www.scala-sbt.org/) + [projectmatrix](https://github.com/sbt/sbt-projectmatrix)
    (or sbt 2.0).

    However, all runnable examples are tested using [Scala CLI](https://scala-cli.virtuslab.org/) and contain the directives needed to make it happy.

## Your first Hearth-based macro

Once you add Hearth to your project you can start writing Hearth-powered macros.

You can decide to either:

 - write cross-compilable macros (we suggest also using [cross-quotes](cross-quotes.md) in such case)
 - write Scala-2-only or Scala-3-only macros

This is how you can write a cross-compilable macro.

!!! example "Cross-compilable API"

    If you want to write cross-compilable macros, you'll work with types and methods such as:

    ```scala
    // file: src/main/scala/example/SharedMacroLogic.scala - part of shared macro logic example
    //> using scala {{ scala.2_13 }} {{ scala.3 }}
    //> using dep com.kubuszok::hearth:{{ hearth_version() }}
    package example

    // Shared part
    trait SharedMacroLogic { this: hearth.MacroCommons =>

      // Type[A] - represents information about a type A, where we know what A is:
      //           it's either some known type like `java.lang.String`, or a type parameter,
      //           or that type we know exists, that we named as `A` (about that later)

      // Expr[A] - represents an expression of type A

      // These and other types become available to us in traits that extend from MacroCommons

      def yourMacro[A: Type](expr: Expr[A]): Expr[String] = Expr {
        s"${expr.plainPrint} : ${Type[A].plainPrint}"
      }
    }
    ```

    Then we pair `MacroCommons` with adapters we write for Scala 2 and Scala 3.

    ```scala
    // file: src/main/scala-2/example/YourMacro.scala - part of shared macro logic example
    //> using target.scala {{ scala.2_13 }}
    //> using options -Xsource:3
    package example

    import scala.language.experimental.macros
    import scala.reflect.macros.blackbox

    // Scala 2 adapter
    class YourMacro(val c: blackbox.Context) extends hearth.MacroCommonsScala2 with SharedMacroLogic {

      // Type[A] = c.WeakTypeTag[A]

      // Expr[A] = c.Expr[A]

      // Unfortunately, while compiler is clever enough to see that the types are the same when we apply them...
      def yourMacroImpl[A: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[String] = yourMacro(expr)
    }

    object Example {
      // ... it is not smart enough to see it if we want to call YourMacro.yourMacro directly.
      def callingMacro[A](expr: A): String = macro YourMacro.yourMacroImpl[A]
    }
    ```

    ```scala
    // file: src/main/scala-3/example/YourMacro.scala - part of shared macro logic example
    //> using target.scala {{ scala.3 }}
    //> using plugin com.kubuszok::hearth-cross-quotes::{{ hearth_version() }}
    package example

    import scala.quoted.*

    // Scala 3 adapter
    class YourMacro(q: Quotes) extends hearth.MacroCommonsScala3(using q), SharedMacroLogic

    object YourMacro {

      // Scala 3 requires all macros to be defined in objects or other stable locations.
      // Since we define things in a mixin, we need to write an adapter that instantiates it
      // inside a method.
      def yourMacro[A: Type](expr: Expr[A])(using q: Quotes): Expr[String] =
        new YourMacro(q).yourMacro(expr)
    }

    object Example {
      inline def callingMacro[A](inline expr: A): String = ${ YourMacro.yourMacro('expr) }
    }
    ```

    ```scala
    // file: src/test/scala/example/ExampleSpec.scala - part of shared macro logic example
    //> using test.dep org.scalameta::munit::{{ libraries.munit }}
    package example

    final class ExampleSpec extends munit.FunSuite {

      test("Example.callingMacro runs our cross-compilable macro") {
        assertEquals(Example.callingMacro("value"), "\"value\" : java.lang.String")
      }
    }
    ```

These examples show how you can write Scala-specific macros with Hearth.

!!! example "Scala 2 API"

    If you want to write Scala 2-only macros, you can simplify the code: use `MacroCommonsScala2` directly,
    use `c.WeakTypeTag` and `c.Expr` directly, etc., while still using extension methods and utilities from Hearth:

    ```scala
    // file: src/main/scala/example/Example.scala - part of Scala-2-only macro example
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::hearth:{{ hearth_version() }}
    package example

    import hearth.MacroCommonsScala2

    import scala.language.experimental.macros
    import scala.reflect.macros.blackbox

    class YourMacro(val c: blackbox.Context) extends hearth.MacroCommonsScala2 {

      // Type[A] = c.WeakTypeTag[A]

      // Expr[A] = c.Expr[A]

      def yourMacro[A: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[String] = Expr {
        s"${expr.plainPrint} : ${Type[A].plainPrint}"
      }
    }

    object Example {
      def callingMacro[A](expr: A): String = macro YourMacro.yourMacro[A]
    }
    ```

    ```scala
    // file: src/test/scala/example/ExampleSpec.scala - part of Scala-2-only macro example
    //> using test.dep org.scalameta::munit::{{ libraries.munit }}
    package example

    final class ExampleSpec extends munit.FunSuite {

      test("Example.callingMacro runs our Scala-3-only macro") {
        assertEquals(Example.callingMacro("value"), "\"value\" : java.lang.String")
      }
    }
    ```

!!! example "Scala 3 API"

    If you want to write Scala 3-only macros, you can simplify the code: use `MacroCommonsScala3` directly,
    use `quoted.Type` and `quoted.Expr` directly, etc., while still using extension methods and utilities from Hearth:

    ```scala
    // file: src/main/scala/example/Example.scala - part of Scala-3-only macro example
    //> using scala {{ scala.3 }}
    //> using dep com.kubuszok::hearth:{{ hearth_version() }}
    package example

    import hearth.MacroCommonsScala3

    import scala.quoted.*

    class YourMacro(q: Quotes) extends hearth.MacroCommonsScala3(using q) {

      // Type[A] = c.WeakTypeTag[A]

      // Expr[A] = c.Expr[A]

      def yourMacro[A: Type](expr: Expr[A]): Expr[String] = Expr {
        s"${expr.plainPrint} : ${Type[A].plainPrint}"
      }
    }
    object YourMacro {

      // Scala 3 requires all macros to be defined in objects or other stable locations.
      // Since we define things in a mixin, we need to write an adapter that instantiates it
      // inside a method.
      def yourMacro[A: Type](expr: Expr[A])(using q: Quotes): Expr[String] =
        new YourMacro(q).yourMacro(expr)
    }

    object Example {
      inline def callingMacro[A](inline expr: A): String = ${ YourMacro.yourMacro('expr) }
    }
    ```

    ```scala
    // file: src/test/scala/example/ExampleSpec.scala - part of Scala-3-only macro example
    //> using test.dep org.scalameta::munit::{{ libraries.munit }}
    package example

    final class ExampleSpec extends munit.FunSuite {

      test("Example.callingMacro runs our Scala-3-only macro") {
        assertEquals(Example.callingMacro("value"), "\"value\" : java.lang.String")
      }
    }
    ```

## Library conventions

Inside a shared-logic trait (`MacroCommons`) we work with abstract types and abstract methods:

!!! example "`MacroCommons` API"

    ```scala
    type Type[A]
    
    val Type: TypeModule
    trait TypeModule { this: Type.type => }
    
    implicit class TypeOps[A](tpe: Type[A]) {
      // ops
    }

    type Expr[A]
    val Expr: ExprModule
    trait ExprModule { this: Expr.type => }

    implicit class ExprOps[A](expr: Expr[A]) {
      // ops
    }
    ```
  
Their implementations are mixed in later (`MacroCommonsScala2`, `MacroCommonsScala3`).
   
Hearth implements shared utilities from the bottom up: `Type[A]` and `Expr[A]` build on top of `UntypedType` and `UntypedExpr`,
then `Method[A]`s build on top of `Type[A]` and `Expr[A]`, then `CaseClass[A]`, `Enum[A]` and `JavaBean[A]` build on top of them.
(The lower the abstraction level is, the less likely the API is to change).

For most common use cases you probably want to use high-level utilities like:

!!! example "High-level `Class[A]` API"

    ```scala
    Class[A] match {
      case caseClass: CaseClass[A] => // use case class utilities
      case enumType: Enum[A] =>       // use enum utilities
      case javaBean: JavaBean[A] =>   // use Java Bean utilities
      case classType: Class[A] =>     // just a class
    }
    ```

but they are a work in progress, and when the high-level API is not sufficient, you can fall back on lower-level ones.

!!! tip

    If there is no API for what you're trying to do, you can always define:

    ```scala
    // in some common trait in src/main/scala

    type Result // abstract type

    def mySharedAPI(): Result // abstract method
    ```

    in the common part and implement the platform-specific logic in the traits where you are mixing in:

    ```scala
    // in some trait/class in src/main/scala-2 (using MacroCommonsScala2 and Scala 2 types)
    //           and again in src/main/scala-3 (using MacroCommonsScala3 and Scala 3 types)

    final override type Result = ... // type implementation

    override final def mySharedAPI(): Result = ... // method implementation
    ```

    I suggest doing this as little as possible and separating _the intent_ (what you're trying to achieve) from _the means_
    (how you're doing it). That makes the code easier to reason about and maintain.

## `Type`, `UntypedType` and `??`

You can see 3 different representations of a type in Hearth codebase:

 * `Type[A]` - corresponds to `c.WeakTypeTag[A]` or `scala.quoted.Type[A]`. Because the type for which information
   is stored is reflected in the, well, type (e.g. `Type[String]` vs `Type[Int]`) we can use them in type bounds, implicits
   etc
 * `UntypedType` - corresponds to `c.Type` or `q.reflect.TypeRepr`. Because type for which information is stored
   is NOT reflected in the type, it's useful for working with the types of unknown kindness (e.g. `A` vs `F[_]` vs `G[_[_]]`, etc)
 * `??` - an existential type. We know that it's a `Type[A]` for some `A` but we don't know which. It differs from `UntypedType`, 
   because we can do this:
   ```scala
   val unknownType: ??
   import unknownType.Underlying as Param
   // now, references to Param would resolve to unknownType.Underlying

   Type[Param] // works!
   ```
   which is useful when we, for example, receive a list of subtypes of some enum `E`: we get a `List[??<:[E]]` (list of existential types
   with `E` as upper bound).

### Obtaining `Type`

You might see that some examples use `Type[A]`, some `Type(value)` and some use `Type.of[A]`. What is the difference?

`Type[A]` (`Type.apply[A]`) is the same thing as `implicitly[Type[A]]` - it summons a value of `Type[A]`
if it's already in the implicit scope: passed as a type bound (`[A: Type]`), `import`ed or defined explicitly
as `implicit`.

`Type(value)` (`Type.apply(value)`) is the same thing as `implicitly[TypeCodec[A]].apply(value)` -
it summons a value of [`TypeCodec[A]`](#typecodec) and uses it to convert `A` into `Type[A]`.
`TypeCodec[A]` has to be available in the implicit scope.

`Type.of[A]` is a [cross-quotes](cross-quotes.md#typeofa) utility that creates `Type[A]`. While on Scala 2 you can
always summon `c.WeakTypeTag[A]` and on Scala 3 you can always call `scala.quoted.Type.of[A]`,
in cross-compiled code there are no such implicits. Either the `Type` was passed into the macro,
or returned by some utility (often as `??`), or you have to create an instance yourself with `Type.of[A]`.

You should prefer `Type[A]` when `A: Type` is present, and only use `Type.of[A]` to construct the `Type[A]` for known types.
`Type(value)` is only intended to convert values that could be singleton types into singleton types.

### `Type` Operations

`Type[A]` provides many operations for introspecting and manipulating types:

**Printing and display**:

| Companion method        | Extension method        | Example result | Description                                          |
|-------------------------|-------------------------|----------------|------------------------------------------------------|
| `Type.shortName[Int]`   | `Type[Int].shortName`   | `"Int"`        | no package, no type parameters                       |
| `Type.fqcn[Int]`        | `Type[Int].fqcn`        | `"scala.Int"`  | fully-qualified class name, no type parameters       |
| `Type.plainPrint[Int]`  | `Type[Int].plainPrint`  | `"scala.Int"`  | with package name and type parameters, no coloring   |
| `Type.prettyPrint[Int]` | `Type[Int].prettyPrint` | `"scala.Int"`  | with package name and type parameters, ANSI coloring |

**`Type` predicates**:

| Companion method                       | Extension method                       | Description                                                          |
|----------------------------------------|----------------------------------------|----------------------------------------------------------------------|
|                                        |                                        | **Primitives and built-ins**                                         |
| `Type.isPrimitive[Int]`                | `Type[Int].isPrimitive`                | `true` for Boolean, Byte, Short, Int, Long, Float, Double, Char      |
| `Type.isArray[Array[Int]]`             | `Type[Array[Int]].isArray`             | `true` for array types                                               |
| `Type.isJvmBuiltIn[String]`            | `Type[String].isJvmBuiltIn`            | `true` for primitives, Unit, String, arrays                          |
| `Type.isTypeSystemSpecial[Any]`        | `Type[Any].isTypeSystemSpecial`        | `true` for Any, AnyRef, AnyVal, Null, Nothing                        |
|                                        |                                        | **Class types**                                                      |
| `Type.isClass[MyClass]`                | `Type[MyClass].isClass`                | `true` for classes                                                   |
| `Type.notJvmBuiltInClass[MyClass]`     | `Type[MyClass].notJvmBuiltInClass`     | `true` when `isClass && !isJvmBuiltIn`                               |
| `Type.isPlainOldJavaObject[MyClass]`   | `Type[MyClass].isPlainOldJavaObject`   | `true` for non-abstract, non-sealed, non-enum class                  |
| `Type.isJavaBean[MyClass]`             | `Type[MyClass].isJavaBean`             | `true` for POJO with default constructor                             |
|                                        |                                        | **Modifiers**                                                        |
| `Type.isAbstract[MyClass]`             | `Type[MyClass].isAbstract`             | `true` for abstract class or trait                                   |
| `Type.isFinal[MyClass]`                | `Type[MyClass].isFinal`                | `true` for final class                                               |
| `Type.isSealed[MyClass]`               | `Type[MyClass].isSealed`               | `true` for sealed trait/class                                        |
| `Type.isObject[MyClass]`               | `Type[MyClass].isObject`               | `true` for object/module                                             |
| `Type.isVal[MyClass]`                  | `Type[MyClass].isVal`                  | `true` for `someValue.type` (e.g. parameterless `enum` `case`s)      |
|                                        |                                        | **Case classes and enums**                                           |
| `Type.isCase[MyClass]`                 | `Type[MyClass].isCase`                 | `true` for case class, case object, or case val                      |
| `Type.isCaseClass[MyClass]`            | `Type[MyClass].isCaseClass`            | `true` for case class specifically                                   |
| `Type.isCaseObject[MyClass]`           | `Type[MyClass].isCaseObject`           | `true` for case object specifically                                  |
| `Type.isCaseVal[MyClass]`              | `Type[MyClass].isCaseVal`              | `true` for parameterless `enum` `case`s                              |
| `Type.isJavaEnum[MyClass]`             | `Type[MyClass].isJavaEnum`             | `true` for Java enum                                                 |
| `Type.isJavaEnumValue[MyClass]`        | `Type[MyClass].isJavaEnumValue`        | `true` for Java enum value                                           |

**`Type` relationships**:

| Companion method                       | Extension method                 | Description                                         |
|----------------------------------------|----------------------------------|-----------------------------------------------------|
| `Type.isSubtypeOf[String, AnyRef]`     | `Type[String] <:< Type[AnyRef]`  | whether the left type is a subtype of the right one |
| `Type.isSameAs[Int, Int]`              | `Type[Int] =:= Type[Int]`        | whether types are equal                             |

**Metadata and navigation**:

| Companion method                     | Extension method                     | Result type                                       | Description                      |
|--------------------------------------|--------------------------------------|---------------------------------------------------|----------------------------------|
| `Type.position[MyClass]`             | `Type[MyClass].position`             | `Option[Position]`                                | where type is defined            |
| `Type.companionObject[MyClass]`      | `Type[MyClass].companionObject`      | `Option[Expr_??]`                                 | companion object if exists       |
| `Type.annotations[MyClass]`          | `Type[MyClass].annotations`          | `List[Expr_??]`                                   | type annotations                 |
| `Type.directChildren[MyClass]`       | `Type[MyClass].directChildren`       | `Option[ListMap[String, ??<:[MyClass]]]`          | immediate subtypes               |
| `Type.exhaustiveChildren[MyClass]`   | `Type[MyClass].exhaustiveChildren`   | `Option[NonEmptyMap[String, ??<:[MyClass]]]`      | all subtypes                     |


**Visibility checks**:

| Companion method                        | Extension method                        | Description                                                 |
|-----------------------------------------|-----------------------------------------|-------------------------------------------------------------|
| `Type.isAvailable[MyClass](Everywhere)` | `Type[MyClass].isAvailable(Everywhere)` | `true` for public types                                     |
| `Type.isAvailable[MyClass](AtCallSite)` | `Type[MyClass].isAvailable(AtCallSite)` | `true` if accessible from call site (incl. package-private) |

### `TypeCodec`

`TypeCodec[A]` provides bidirectional conversion between singleton/literal type values and their type representation:

!!! example "`TypeCodec`'s `apply` and `unapply`"

    ```scala
    // Creating a type from a literal value
    val intType: Type[5] = Type(5)                // creates Type[5] (Int literal type)
    val stringType: Type["hello"] = Type("hello") // creates Type["hello"] (String literal type)

    // Extracting a value from a literal type (if available)
    Type.unapply[5](Type[5]) // Some(5)
    Type.unapply[Int](Type[Int]) // None (not a literal type)
    ```

Built-in codecs exist for:

- Primitives: `Null`, `Unit`, `Boolean`, `Byte`, `Short`, `Int`, `Long`, `Float`, `Double`, `Char`, `String`
- Collections: `Array[A]`, `Seq[A]`, `List[A]`, `Nil.type`, `Vector[A]`, `Map[K,V]`, `Set[A]`
- Options: `Option[A]`, `Some[A]`, `None.type`
- Either: `Either[L,R]`, `Left[L,R]`, `Right[L,R]`
- Module singletons: any `object` in classpath

### Existential Types

Existential types (`??`) allow working with types whose exact identity is unknown at macro-writing time but will be known at macro-execution time:

!!! example "Working with existential types"

    ```scala
    val tpe: ?? = someType.as_??
    import tpe.Underlying as A
    // Now A can be used as a type parameter, and implicit Type[A] is available

    Type[A] // works!
    ```

!!! example "Bounded variants"

    ```scala
    val subtypeOfFoo:   ??<:[Foo] = ...       // upper bound (subtypeOfFoo.Underlying <: Foo)
    val supertypeOfBar: ??>:[Bar] = ...       // lower bound (supertypeOfBar.Underlying >: Bar)
    val bounded:        Foo <:??<: Bar = ...  // both bounds (Foo <: bounded.Underlying <: Bar)
    ```

## `Expr`, `UntypedExpr` and `Expr_??`

You can see 3 different representations of expressions in Hearth codebase:

 * `Expr[A]` - corresponds to `c.Expr[A]` or `scala.quoted.Expr[A]`. Because the type for which information
   is stored is reflected in the, well, type (e.g. `Expr[String]` vs `Expr[Int]`) we can use them in quotes and splices
 * `UntypedExpr` - corresponds to `c.universe.Tree` or `q.reflect.Term`. Because type for which information is stored
   is NOT reflected in the type, it's useful for working with AST directly (almost always in the platform-specific code)
 * `Expr_??` - existential expression. We know that it's a `Expr[A]` for some `A` but we don't know which. It differs from `UntypedExpr`, 
   because we can do this:
   ```scala
   val unknownExpr: Expr_??
   import unknownExpr.{Underlying as Param, value as expr}
   Type[Param] // now, references to Param would resolve to unknownExpr.Underlying
   expr: Expr[Param] // and this is an Expr of Param type that we can use
   ```
   which is useful when we, for example, try to call some method or constructor.

### Obtaining `Expr`

You might see that some examples use `Expr(value)` and some use `Expr.quote(value)`. What is the difference?

`Expr(value)` (`Expr.apply(value)`) is the same thing as `implicitly[ExprCodec[A]].apply(value)` -
it summons a value of [`ExprCodec[A]`](#exprcodec) and uses it to convert `A` into `Expr[A]`.
`ExprCodec[A]` has to be available in the implicit scope.

`Expr.quote(value)` is a [cross-quotes](cross-quotes.md#exprquote) utility that creates `Expr[A]`.
On Scala 2 it would translate to `c.Expr(q"value")` and on Scala 3 to `'{ value }`.

You should use `Expr(value)` only when there is an `ExprCodec` that can lift a value available in the macro
into an expression that reconstructs it (only the final result!).

You should prefer `Expr.quote(value)` to preserve the whole code that creates the value, which is often possible,
as long as you are not quoting something that's only computed in the macro (for that `Expr.apply` + `Expr.splice`
might be necessary).

### Expression Operations

`Expr[A]` provides operations for working with expressions:

**Printing and display**:

| Companion method          | Extension method  | Example result               | Description                          |
|---------------------------|-------------------|------------------------------|--------------------------------------|
| `Expr.plainPrint(expr)`   | `expr.plainPrint` | `"42"`                       | code representation without colors   |
| `Expr.prettyPrint(expr)`  | `expr.prettyPrint`| `"42"`                       | colored code representation          |
| `Expr.plainAST(expr)`     | `expr.plainAST`   | `"Literal(IntConstant(42))"` | AST representation without colors    |
| `Expr.prettyAST(expr)`    | `expr.prettyAST`  | `"Literal(IntConstant(42))"` | colored AST representation           |

**Summoning implicits**:

| Companion method                        | Extension method                    | Result type                      | Description                              |
|-----------------------------------------|-------------------------------------|----------------------------------|------------------------------------------|
| `Expr.summonImplicit[Ordering[String]]` | `Type[Ordering[String]].summonExpr` | `Option[Expr[Ordering[String]]]` | summon implicit value as expression      |

**`Expr` operations**:

| Companion method                          | Extension method            | Result type  | Description                              |
|-------------------------------------------|-----------------------------|--------------|------------------------------------------|
| `Expr.upcast[String, AnyRef](stringExpr)` | `stringExpr.upcast[AnyRef]` | `Expr[...]`  | safe upcast to a supertype               |
| `Expr.suppressUnused[A](expr)`            | `expr.suppressUnused`       | `Expr[Unit]` | prevents "unused value" warnings         |

### `ExprCodec`

`ExprCodec[A]` generalizes over Scala 2's `Lifting`/`Unliftable` and Scala 3's `ToExpr`/`FromExpr`, allowing bidirectional conversion between values and expressions:

!!! example "`ExprCodec`'s `apply` and `unapply`"

    ```scala
    // Creating expressions from values (lifting)
    val intExpr: Expr[Int] = Expr(42)
    val listExpr: Expr[List[Int]] = Expr(List(1, 2, 3))
    val mapExpr: Expr[Map[String, Int]] = Expr(Map("a" -> 1, "b" -> 2))

    // Extracting values from expressions (unlifting)
    Expr.unapply(intExpr)   // Some(42)
    intExpr.value           // Some(42)
    ```

Built-in codecs exist for:

- Primitives: `Null`, `Unit`, `Boolean`, `Byte`, `Short`, `Int`, `Long`, `Float`, `Double`, `Char`, `String`
- Special: `Class[A]`, `ClassTag[A]`
- Collections: `Array[A]`, `Seq[A]`, `List[A]`, `Nil.type`, `Vector[A]`, `Map[K,V]`, `Set[A]`
- Options: `Option[A]`, `Some[A]`, `None.type`
- Either: `Either[L,R]`, `Left[L,R]`, `Right[L,R]`
- Data: Hearth's `data.Data` type

### `VarArgs`

Cross-platform handling of variadic arguments (Scala 2 uses `Seq[Expr[A]]`, Scala 3 uses `Expr[Seq[A]]`):

!!! example "How `VarArgs` are used"

    ```scala
    // file: src/main/scala/example/VarArgsMacro.scala - part of VarArgs example
    //> using scala {{ scala.2_13 }} {{ scala.3 }}
    //> using dep com.kubuszok::hearth:{{ hearth_version() }}

    trait VarArgsMacro { this: hearth.MacroCommons =>

      def myMacro[A: Type](args: VarArgs[A]): Expr[Unit] = {
        // VarArgs allow converting to some collection of Expr[A], here: List[Expr[A]]
        // regardless of whether it was the Scala 2 or Scala 3 variadic argument representation.
        val exprs: List[Expr[A]] = args.toList

        Environment.reportInfo(
          // work with individual expressions
          exprs.map(_.prettyPrint).mkString("\n")
        )

        Expr.quote { () }
      }
    }
    ```

    ```scala
    // file: src/main/scala-2/example/VarArgsMacroImpl.scala - part of VarArgs example
    //> using target.scala {{ scala.2_13 }}
    //> using options -Xsource:3

    import scala.language.experimental.macros
    import scala.reflect.macros.blackbox

    object Example {
      // Scala 2 macro-method - A* becomes Seq[Expr[A]]
      def method[A](args: A*): Unit = macro VarArgsMacroImpl.myMacroImpl[A]
    }

    // Scala 2 adapter
    class VarArgsMacroImpl(val c: blackbox.Context) extends hearth.MacroCommonsScala2 with VarArgsMacro {

      def myMacroImpl[A: c.WeakTypeTag](args: c.Expr[A]*): c.Expr[Unit] =
        myMacro[A](args)
    }
    ```

    ```scala
    // file: src/main/scala-3/example/VarArgsMacroImpl.scala - part of VarArgs example
    //> using target.scala {{ scala.3 }}
    //> using plugin com.kubuszok::hearth-cross-quotes::{{ hearth_version() }}

    import scala.quoted.*

    object Example {
      // Scala 3 inline def - A* becomes Expr[Seq[A]]
      inline def method[A](inline args: A*): Unit = ${ VarArgsMacroImpl.myMacroImpl('{ args }) }
    }

    // Scala 3 adapter
    class VarArgsMacroImpl(q: Quotes) extends hearth.MacroCommonsScala3(using q), VarArgsMacro
    object VarArgsMacroImpl {

      def myMacroImpl[A: Type](args: Expr[Seq[A]])(using q: Quotes): Expr[Unit] =
        new VarArgsMacroImpl(q).myMacro[A](args)
    }
    ```

    ```scala
    // file: src/test/scala/example/ExampleSpec.scala - part of VarArgs example
    //> using test.dep org.scalameta::munit::{{ libraries.munit }}

    final class ExampleSpec extends munit.FunSuite {

      test("Example.method should work") {
        Example.method("a", "b" + "c", s"d ${1}, ${2}, ${3}")
      }
    }
    ```

!!! tip "`VarArgs` only handles reading of variadic arguments passed into macro. It does not (yet) handle working with variadic arguments in AST."

### `MatchCase`

Build pattern-matching expressions programmatically.

!!! example "How `MatchCase`es are used"

    ```scala
    // file: src/main/scala/example/MatchCaseMacro.scala - part of MatchCase example
    //> using scala {{ scala.2_13 }} {{ scala.3 }}
    //> using dep com.kubuszok::hearth:{{ hearth_version() }}

    import hearth.fp.syntax.* // for map syntax on MatchCase

    trait MatchCaseMacro { this: hearth.MacroCommons =>

      val either = Type.Ctor2.of[Either]
      val left   = Type.Ctor2.of[Left]
      val right  = Type.Ctor2.of[Right]
      val unit   = Type.of[Unit]

      def makeMatch[L: Type, R: Type](either: Expr[Either[L, R]]): Expr[Unit] = {
        // Types required to make .matchOn work
        implicit val eitherType: Type[Either[L, R]] = this.either[L, R]
        implicit val leftType: Type[Left[L, R]]     = left[L, R]
        implicit val rightType: Type[Right[L, R]]   = right[L, R]
        implicit val unitType: Type[Unit]           = unit
        
        // either match {
        either.matchOn[Unit](
          // case l: Left[L, R] =>
          MatchCase.typeMatch[Left[L, R]]("l").map { l =>
            Expr.quote { println(s"left value ${ Expr.splice(l) }") }
          },
          // case r: Right[L, R] =>
          MatchCase.typeMatch[Right[L, R]]("r").map { r =>
            Expr.quote { println(s"right value ${ Expr.splice(r) }") }
          }
        )
        // }
      }
    }
    ```

    ```scala
    // file: src/main/scala-2/example/MatchCaseMacroImpl.scala - part of MatchCase example
    //> using target.scala {{ scala.2_13 }}
    //> using options -Xsource:3

    import scala.language.experimental.macros
    import scala.reflect.macros.blackbox

    object Example {
      def makeMatch[L, R](either: Either[L, R]): Unit = macro MatchCaseMacroImpl.makeMatchImpl[L, R]
    }

    // Scala 2 adapter
    class MatchCaseMacroImpl(val c: blackbox.Context) extends hearth.MacroCommonsScala2 with MatchCaseMacro {

      def makeMatchImpl[L: c.WeakTypeTag, R: c.WeakTypeTag](either: c.Expr[Either[L, R]]): c.Expr[Unit] =
        makeMatch[L, R](either)
    }
    ```

    ```scala
    // file: src/main/scala-3/example/MatchCaseMacroImpl.scala - part of MatchCase example
    //> using target.scala {{ scala.3 }}
    //> using plugin com.kubuszok::hearth-cross-quotes::{{ hearth_version() }}

    import scala.quoted.*

    object Example {
      // Scala 3 inline def - A* becomes Expr[Seq[A]]
      inline def makeMatch[L, R](inline either: Either[L, R]): Unit = ${ MatchCaseMacroImpl.makeMatchImpl('{ either }) }
    }

    // Scala 3 adapter
    class MatchCaseMacroImpl(q: Quotes) extends hearth.MacroCommonsScala3(using q), MatchCaseMacro
    object MatchCaseMacroImpl {

      def makeMatchImpl[L: Type, R: Type](either: Expr[Either[L, R]])(using q: Quotes): Expr[Unit] =
        new MatchCaseMacroImpl(q).makeMatch[L, R](either)
    }
    ```

    ```scala
    // file: src/test/scala/example/ExampleSpec.scala - part of MatchCase example
    //> using test.dep org.scalameta::munit::{{ libraries.munit }}

    final class ExampleSpec extends munit.FunSuite {

      test("Example.method should work") {
        Example.makeMatch(Left("string"): Either[String, Int])
        // expected output:
        // left value Left(string)

        Example.makeMatch(Right(2586): Either[String, Int])
        // expected output:
        // right value Right(2586)
      }
    }
    ```

!!! tip "`matchOn` and `MatchCase` are not needed if you can make the whole `match` at once inside (cross) quotes. Only if you have to construct the `case`s (e.g. types are being resolved, they're not known during the compilation of a macro) you have to use it as a `match`-builder."

### `FreshName`

Strategies for generating fresh variable names to avoid clashes, used, e.g., by [`MatchCase`](#matchcase)es,
[`Scoped`](#scoped) and [`LambdaBuilder`](#lambdabuilder)s.

| Syntax                 | Example        | Description                                                     |
|------------------------|----------------|-----------------------------------------------------------------|
| `FreshName.FromType`   | `string`       | use lowercased simple name of the type as a fresh name prefix   |
| `FreshName.FromExpr`   | `sth.tostring` | use printed expr as a fresh name prefix                         |
| `FreshName.FromPrefix` | `name`         | use provided name as a fresh name prefix                        |
| `"name"`               | `name`         | when implicit conversions are enabled, turns string into prefix |

### `Scoped`

Create scoped definitions (`val`, `var`, `lazy val`, `def`) with automatic scope management:

```scala
// Generate: { val x = expr; useX(x) }
Scoped.createVal(expr, "x").use { x: Expr[A] =>
  useX(x): Expr[B]
} // : Expr[B]

// Generate: { var x = initial; x = newValue; readX(x) }
Scoped.createVar(initial, "x").use { case (getter, setter) =>
  Expr.quote {
    Expr.splice(setter(newValue))
    Expr.splice(readX(getter))
  }
}

// Lazy and def work similarly
Scoped.createLazy(expr).use { x => ??? }
Scoped.createDef(expr).use { x => ??? }
```

!!! tip "`Scoped` utilities are not needed if you can make `var`/`val`/`lazy val`/`def` inside (cross) quotes. Only if you need to use the fresh names they become necessary to avoid the name clashes."

### `LambdaBuilder`

Build lambda expressions with unknown return types and error aggregation support:

```scala
// Simple lambda: (a: A) => useA(a)
LambdaBuilder.of1[A]("a").buildWith { a: Expr[A] =>
  useA(a): Expr[B]
} // : Expr[A => B]

// Two arguments: (a: A, b: B) => useAB(a, b)
LambdaBuilder.of2[A, B]("a", "b").buildWith { case (a, b) =>
  useAB(a, b): Expr[C]
} // : Expr[(A, B) => C]

// With error handling (using MIO or another effect)
LambdaBuilder.of1[A]("a").traverse[MIO, B] { a: Expr[A] =>
  validateAndUseA(a): MIO[Expr[B]]
}.map(_.build) // : MIO[Expr[A => B]]
```

Supports up to 22 parameters (`of1` through `of22`).

!!! tip "`LambdaBuilder`s are not needed if you can make the lambda inside (cross) quotes. Only if the types and/or numer of the arguments is unknown during the complation of the macro and have to be resolved, you need to use a lambda builder to stay flexible."

## `Method`

`Method` represents a method that can be called. Methods come in different flavors depending on how they're invoked:

 * `Method.NoInstance[Out]` - constructors, static methods, or stable object methods: `Companion.method(args)` 
 * `Method.OfInstance[A, Out]` - instance methods: `instance.method(args)`
 * `Method.Unsupported[A, Out]` - methods we cannot handle (e.g., polymorphic methods with explicit type parameters)

!!! warning "Type Parameter Limitation"
    Applying type parameters is not yet supported. Calls like `instance.method[A, B](values)` where type parameters 
    are applied explicitly or need to be inferred are represented as `Method.Unsupported`.

### Obtaining `Method`

We should just call one of the methods on `Method` or `Type[A]`, depending on which kind of method we need.

| Companion method                     | Extension method                   | Result type                          | Description                           |
|--------------------------------------|------------------------------------|--------------------------------------|---------------------------------------|
| `Method.primaryConstructor[MyClass]` | `Type[MyClass].primaryConstructor` | `Option[Method.NoInstance[MyClass]]` | primary constructor                   |
| `Method.defaultConstructor[MyClass]` | `Type[MyClass].defaultConstructor` | `Option[Method.NoInstance[MyClass]]` | nullary constructor                   |
| `Method.constructors[MyClass]`       | `Type[MyClass].constructors`       | `List[Method.NoInstance[MyClass]]`   | all constructors                      |
| `Method.methods[MyClass]`            | `Type[MyClass].methods`            | `List[Method.Of[MyClass]]`           | instance and companion object methods |

### `Method` Operations

**Metadata**:

| Method               | Result type        | Description        |
|----------------------|--------------------|--------------------|
| `method.name`        | `String`           | method name        |
| `method.position`    | `Option[Position]` | source location    |
| `method.annotations` | `List[Expr_??]`    | method annotations |
| `method.parameters`  | `Parameters`       | parameter groups   |

**Predicates**:

| Method                           | Description                         |
|----------------------------------|-------------------------------------|
|                                  | **Kind checks**                     |
| `method.isConstructor`           | `true` for constructors             |
| `method.isVal`                   | `true` for `val` member             |
| `method.isVar`                   | `true` for `var` member             |
| `method.isLazy`                  | `true` for `lazy val`               |
| `method.isDef`                   | `true` for `def` method             |
| `method.isImplicit`              | `true` for `implicit` `def`/`val`   |
|                                  | **Source info**                     |
| `method.isDeclared`              | `true` if declared in this `class`  |
| `method.isSynthetic`             | `true` if compiler-generated        |
| `method.isInherited`             | `true` if inherited from parent     |
| `method.isCaseField`             | `true` if `case class` field        |
| `method.isConstructorArgument`   | `true` if constructor parameter     |
|                                  | **Visibility**                      |
| `method.isAvailable(Everywhere)` | `true` for public methods           |
| `method.isAvailable(AtCallSite)` | `true` if accessible from call site |

**Arity**:

| Method             | Result type | Description                    |
|--------------------|-------------|--------------------------------|
| `method.arity`     | `Int`       | total parameter count          |
| `method.isNullary` | `Boolean`   | `true` if no parameters        |
| `method.isUnary`   | `Boolean`   | `true` if one parameter        |
| `method.isBinary`  | `Boolean`   | `true` if two parameters       |
| `method.isNAry(n)` | `Boolean`   | `true` if exactly n parameters |

**Accessors (getters/setters)**:

| Method                      | Result type       | Description                                       |
|-----------------------------|-------------------|---------------------------------------------------|
|                             |                   | **Scala-style accessors**                         |
| `method.isScalaGetter`      | `Boolean`         | `true` for val, var, lazy val (without `_=`)      |
| `method.isScalaSetter`      | `Boolean`         | `true` for var setter (`name_=`)                  |
| `method.scalaAccessorName`  | `Option[String]`  | field name if Scala accessor                      |
|                             |                   | **Java Bean accessors**                           |
| `method.isJavaGetter`       | `Boolean`         | `true` for `getX()` → X, `isX()` → Boolean        |
| `method.isJavaSetter`       | `Boolean`         | `true` for `setX(v)` → Unit                       |
| `method.javaAccessorName`   | `Option[String]`  | property name if Java Bean accessor               |
|                             |                   | **Combined**                                      |
| `method.isAccessor`         | `Boolean`         | `true` for any accessor (Scala or Java)           |
| `method.accessorName`       | `Option[String]`  | property name (tries both Scala and Java styles)  |

### Pattern Matching on `Method`s

The recommended way to handle methods is through pattern matching:

!!! example "How to handle a `Method.Of[Instance]`"

    ```scala
    method match {
      case m: Method.NoInstance[?] =>
        import m.Returned
        m(arguments) // Either[String, Expr[Returned]]
        
      case m: Method.OfInstance[?, ?] =>
        import m.{ Instance, Returned }
        m(instance /* Expr[Instance] */, arguments) // Either[String, Expr[Returned]]
        
      case Method.Unsupported(_, _) =>
        Left(method.reasonForUnsupported)
    }
    ```

### Parameters and Arguments

**Parameter** - represents a method's parameter:

| Property               | Result type                      | Description                  |
|------------------------|----------------------------------|------------------------------|
| `param.name`           | `String`                         | parameter name               |
| `param.index`          | `Int`                            | parameter position           |
| `param.position`       | `Option[Position]`               | source location              |
| `param.tpe`            | `??`                             | the parameter type           |
| `param.hasDefault`     | `Boolean`                        | has default value?           |
| `param.defaultValue`   | `Option[Existential[Method.Of]]` | default value's getter       |
| `param.annotations`    | `List[Expr_??]`                  | parameter annotations        |
| `param.isByName`       | `Boolean`                        | by-name parameter?           |
| `param.isImplicit`     | `Boolean`                        | implicit parameter?          |

**Parameters** - all parameters of a method, grouped by its parameter lists:

!!! example "`Parameters` are ordered"

    ```scala
    type Parameters = List[ListMap[String, Parameter]]

    // Example: def foo(a: Int, b: String)(implicit c: Boolean)
    // Parameters = List(
    //   ListMap("a" -> paramA, "b" -> paramB),
    //   ListMap("c" -> paramC)
    // )
    ```

**Arguments** - values to pass to a method, order does not matter but names have to match!
(we can skip arguments for params for which `param.hasDefault` is `true`):

!!! example "Building `Arguments`' `Map`"

    ```scala
    type Arguments = Map[String, Expr_??]

    // Build arguments
    val args: Arguments = Map(
      "a" -> Expr(42).as_??,
      "b" -> Expr("hello").as_??
    )
    ```

### Calling `Method`s

If we have `Method.Of[Instance]`, we should pattern-match it first to know what kind
of method it is. Once we do, we need to apply the `Arguments`
(and the instance, if it's `Method.OfInstance`):

!!! example "Constructor or static method"

    ```scala
    val ctor: Method.NoInstance[MyClass] = Type[MyClass].primaryConstructor.get
    ctor(
      arguments = Map("param1" -> arg1, "param2" -> arg2) // Arguments
    ) // Either[String, Expr[MyClass]]
    ```

!!! example "Instance method"

    ```scala
    val method: Method.OfInstance[MyClass, String] = Type[MyClass].methods.find(_.name = "toString").get
    import method.{ Instance, Returned }
    method(
      instance = myInstance, // Expr[Instance]
      arguments = Map()      // Arguments
    ) // Either[String, Expr[Returned]]
    ```

The methods' `apply` validates that:

- All required parameters are provided (it uses default values for arguments that aren't provided if they exist)
- Argument types match parameter types

and returns `Right(expr)` on success or `Left(error)` on failure.

## `Class`

`Class[A]` is a convenient utility that aggregates information about a type:

| Method              | Result type                        | Description                         |
|---------------------|------------------------------------|-------------------------------------|
| `cls.constructors`  | `List[Method.NoInstance[MyClass]]` | all constructors                    |
| `cls.methods`       | `List[Method.Of[MyClass]]`         | all methods                         |
| `cls.method("foo")` | `List[Method.Of[MyClass]]`         | all overloads named "foo"           |

`Class` also provides specialized views when applicable:

| Method            | Result type                     | Description                   |
|-------------------|---------------------------------|-------------------------------|
| `cls.asCaseClass` | `Option[CaseClass[MyClass]]`    | case class view if applicable |
| `cls.asEnum`      | `Option[Enum[MyClass]]`         | enum view if applicable       |
| `cls.asJavaBean`  | `Option[JavaBean[MyClass]]`     | Java Bean view if applicable  |

These utilities are built on top of `Type` and `Method` - you could implement similar functionality yourself, but they provide convenient shortcuts.

### `CaseClass[A]`

Specialized view for case classes, providing access to primary constructor and case fields:

| Method                     | Result type                          | Description                   |
|----------------------------|--------------------------------------|-------------------------------|
| `cc.primaryConstructor`    | `Method.NoInstance[Person]`          | primary constructor           |
| `cc.nonPrimaryConstructors`| `List[Method.NoInstance[Person]]`    | other constructors            |
| `cc.caseFields`            | `List[Method.Of[Person]]`            | case class fields             |

**Constructing instances:**

```scala
// Sequential field construction
cc.construct[MIO] { field: Parameter =>
  import field.tpe.Underlying as FieldType
  createExpr[FieldType]: MIO[Expr[FieldType]]
}.map(_.get) // MIO[Expr[Person]]

// Parallel field construction (for independent computations)
cc.parConstruct[MIO] { field: Parameter =>
  import field.tpe.Underlying as FieldType
  createExprInParallel[FieldType]: MIO[Expr[FieldType]]
}.map(_.get) // MIO[Expr[Person]]
```

**Deconstructing instances:**

```scala
val person: Expr[Person] = ???
val fieldValues: ListMap[String, Expr_??] = cc.caseFieldValuesAt(person)
// Map("name" -> nameExpr, "age" -> ageExpr, ...)
```

### `Enum[A]`

Specialized view for sealed traits, Scala 3 enums, or Java enums:

| Method                      | Result type                                | Description                |
|-----------------------------|--------------------------------------------|----------------------------|
| `enumm.directChildren`      | `ListMap[String, ??<:[Color]]`             | immediate subtypes         |
| `enumm.exhaustiveChildren`  | `Option[NonEmptyMap[String, ??<:[Color]]]` | all subtypes               |

**Pattern matching on enum values:**

```scala
val colorExpr: Expr[Color] = ???

// Sequential case handling
enumm.matchOn[MIO, String](colorExpr) { child: Expr_??<:[Color] =>
  import child.{Underlying as ChildType, value as childExpr}
  // childExpr: Expr[ChildType] where ChildType <: Color
  handleColor[ChildType](childExpr): MIO[Expr[String]]
}.map(_.get) // MIO[Expr[String]]

// Parallel case handling
enumm.parMatchOn[MIO, String](colorExpr) { child =>
  import child.{Underlying as ChildType, value as childExpr}
  handleColorInParallel[ChildType](childExpr): MIO[Expr[String]]
}.map(_.get) // MIO[Expr[String]]
```

This generates exhaustive pattern matching over all direct children:

```scala
// Generated code:
colorExpr match {
  case red: Red.type => handleColor(red)
  case Green(value) => handleColor(Green(value))
  case Blue => handleColor(Blue)
}
```

### `JavaBean[A]`

Specialized view for Java Beans (POJOs with default constructor and setters):

| Method                      | Result type                                       | Description                |
|-----------------------------|---------------------------------------------------|----------------------------|
| `bean.defaultConstructor`   | `Method.NoInstance[PersonBean]`                   | nullary constructor        |
| `bean.beanGetters`          | `List[Existential[Method.OfInstance[PersonBean, *]]]` | getter methods         |
| `bean.beanSetters`          | `List[Method.OfInstance[PersonBean, Unit]]`       | setter methods             |

**Constructing without setters:**

```scala
val instance: Option[Expr[PersonBean]] = bean.constructWithoutSetters
// Generates: new PersonBean()
```

**Constructing with setters:**

```scala
// Sequential setter calls
bean.constructWithSetters[MIO] { (name: String, param: Parameter) =>
  import param.tpe.Underlying as FieldType
  createValue[FieldType](name): MIO[Expr[FieldType]]
}.map(_.get) // MIO[Expr[PersonBean]]

// Parallel setter preparation (setters still called sequentially)
bean.parConstructWithSetters[MIO] { (name, param) =>
  import param.tpe.Underlying as FieldType
  createValueInParallel[FieldType](name): MIO[Expr[FieldType]]
}.map(_.get) // MIO[Expr[PersonBean]]
```

This generates code like:

```scala
{
  val bean = new PersonBean()
  bean.setName(nameValue)
  bean.setAge(ageValue)
  bean
}
```

## `Position`

Represents a source code location (corresponds to `c.universe.Position` on Scala 2, `quotes.reflect.Position` on Scala 3):

| Property / Method       | Result type                      | Example result                           | Description                                |
|-------------------------|----------------------------------|------------------------------------------|--------------------------------------------|
| `pos.file`              | `Option[java.nio.file.Path]`     |                                          | source file path                           |
| `pos.fileName`          | `Option[String]`                 |                                          | file name only (no path)                   |
| `pos.line`              | `Int`                            |                                          | line number                                |
| `pos.column`            | `Int`                            |                                          | column number                              |
| `pos.offset`            | `Int`                            |                                          | character offset from file start           |
| `pos.prettyPrint`       | `String`                         | `"MyFile.scala:42:10"`                   | short format with filename                 |
| `pos.prettyPrintLong`   | `String`                         | `"/full/path/to/MyFile.scala:42:10"`     | full path format                           |

Positions can be compared and sorted:

```scala
positions.sorted  // sorts by file path, then offset
```

!!! tip

    When returning `line` or `column`, Hearth adjusts the values because:

     - Scala 2 macro positions are 1-indexed while
     - Scala 3 macro positions are 0-indexed
    
    but when pointing to an actual file location the Scala 3 error printer already adjusts them.

    However, another adjustment is needed for `Environment.currentPosition` since:

     - Scala 2 points to the beginning of the `macroMethod` name
       ```scala
       someInstance.someMacroMethod
       //           ^ position of macro expansion on Scala 2
       ```
     - but on Scala 3 it depends on the arity of the method!
       ```scala
       someInstance.someMacroMethod
       //           ^ position of macro expansion on Scala 3 when nullary
       someInstance.someMacroMethodWithArgs(argument)
       //                                   ^ position of macro expansion on Scala 3 when non-nullary
       ```

    Hearth makes sure that the behavior is consistent and sane and aligns it to always point
    to the first letter of the macro method name when you use `.line` or `.column`,
    so you can get predictable behavior from e.g. `Environment.isExpandedAt`.

## `Environment`

`Environment` provides utilities for introspecting the compilation environment and reporting messages:

### Current Compilation Context

| Property / Method                      | Result type         | Description                                    |
|----------------------------------------|---------------------|------------------------------------------------|
| `Environment.currentPosition`          | `Position`          | where macro is expanded                        |
| `Environment.currentScalaVersion`      | `ScalaVersion`      | e.g. `2.13.10`, `3.3.0`                        |
| `Environment.currentLanguageVersion`   | `LanguageVersion`   | `Scala2_13` or `Scala3`                        |
| `Environment.isScala2_13`              | `Boolean`           | whether running under Scala 2.13               |
| `Environment.isScala3`                 | `Boolean`           | whether running under Scala 3                  |
| `Environment.currentJDKVersion`        | `JDKVersion`        | runtime JDK version                            |
| `Environment.currentPlatform`          | `Platform`          | `JVM`, `JS`, or `Native`                       |
| `Environment.isJvm`                    | `Boolean`           | whether running on JVM                         |
| `Environment.isJs`                     | `Boolean`           | whether running on Scala.js                    |
| `Environment.isNative`                 | `Boolean`           | whether running on Scala Native                |

### Macro Settings

Pass settings to macros via `-Xmacro-settings:key=value`:

```scala
// scalacOptions += "-Xmacro-settings:myMacro.debug=true"
// scalacOptions += "-Xmacro-settings:myMacro.threshold=42"

Environment.XMacroSettings  // List[String] - all settings as strings

Environment.typedSettings   // Either[String, data.Data] - parsed as JSON-like structure
Environment.typedSettings.toOption.flatMap(_.get("myMacro")).flatMap(_.get("debug"))
```

### Reporting

Report messages to the user during compilation:

| Method                                 | Result type | Description                                            |
|----------------------------------------|-------------|--------------------------------------------------------|
| `Environment.reportInfo(msg)`          | `Unit`      | report informational message                           |
| `Environment.reportWarn(msg)`          | `Unit`      | report warning message                                 |
| `Environment.reportError(msg)`         | `Unit`      | report error message                                   |
| `Environment.reportErrorAndAbort(msg)` | `Nothing`   | report error and abort compilation (throws exception)  |

!!! note "Reporting Behavior"
    For each level (INFO, WARN, ERROR), only **the first** call matters. Subsequent calls at the same level are no-ops.
    This behavior is consistent between Scala 2 and Scala 3 macros.

### Debug Helpers

**Selective macro expansion:**

Useful for debugging a specific macro invocation without seeing output from all expansions:

!!! example "Log only if macro is expanded at `MyFile.scala:42:10`"

    ```scala
    if (Environment.isExpandedAt("MyFile.scala:42:10")) {
      // Print debug info only for this specific expansion
      println(s"Debug: ${someValue}")
    }
    ```

Accepts formats:

- `"File.scala:12"` - matches line
- `"File.scala:12:34"` - matches line and column

### Macro Extensions

Allows you to extend macro functionality by adding dependencies that will be picked up during compilation
from the classpath.

!!! example

    The type of the macro's `trait`/`class` has to be referenced in the extension's type:

    ```scala
    package example

    // Define an extension for MyMacro
    trait MyMacroExtension extends MacroExtension[MyMacro] {

      // Will be able to call all the APIs exposed by MyMacro
      def extend(ctx: MyMacro): Unit
    }
    ```

    Similarly, in a macro `trait`/`class` we ask to load all implementations of a macro extension by its type:

    ```scala
    package example

    trait MyMacro { this: MacroCommons =>

      // Loads all extensions, and passes itself to their `extend` method.
      Environment.loadMacroExtensions[MyMacroExtension] match {
        case Right(()) => // all extensions loaded successfully
        case Left(errors) => // handle errors
      }
    }
    ```

    It requires `META-INF/services/example.MyMacroExtension` resource to list each extension's implementation
    on a separate line as a fully qualified class name, e.g.

    ```
    example.MyMacroExtensionFoo
    example.MyMacroExtensionBar
    ```

Extensions are discovered via Java's [ServiceLoader](https://docs.oracle.com/javase/8/docs/api/java/util/ServiceLoader.html) mechanism,
allowing functionality to be added just by having the right JAR on the classpath during compilation.

### `MIO` Termination Handling

When using Hearth's `MIO` effect system, handle Ctrl+C interruptions with:

!!! tip "Only needed if you run `MIO` yourself"

    ```scala
    Environment.handleMioTerminationException {
      // Code that might be interrupted while running MIO
      someMioComputation.unsafe.runToSync match ...
    }
    ```

Behavior can be configured via `-Xmacro-settings:hearth.mioTerminationShouldUseReportError=true|false`:

- `true` (default): Uses `reportErrorAndAbort` to show error and terminate current macro
- `false`: Prints to stderr and throws exception to terminate compilation

This is automatically handled when using [`mio.runToExprOrFail(...)(...)` extension](micro-fp.md#integration-with-macrocommons).
