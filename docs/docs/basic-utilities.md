# Basic Utilities

The Hearth API abstracts away from any particular macro system. It works with abstract types
backed by abstract methods that we later implement for a particular macro system.

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

Once you add Hearth to your project, you can start writing Hearth-powered macros.

You can either:

 - write cross-compilable macros (we suggest also using [cross-quotes](cross-quotes.md) in that case)
 - write Scala-2-only or Scala-3-only macros

Here is how you can write a cross-compilable macro.

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

These examples show how to write Scala-specific macros with Hearth.

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

For most common use cases, you probably want to use high-level utilities like:

!!! example "High-level `Class[A]` API"

    ```scala
    Class[A] match {
      case caseClass: CaseClass[A] => // use case class utilities
      case enumType: Enum[A] =>       // use enum utilities
      case javaBean: JavaBean[A] =>   // use Java Bean utilities
      case classType: Class[A] =>     // just a class
    }
    ```

They are a work in progress, and when the high-level API is not sufficient you can fall back on lower-level ones.

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

You can see 3 different representations of a type in the Hearth codebase:

 * `Type[A]` - corresponds to `c.WeakTypeTag[A]` or `scala.quoted.Type[A]`. Because the type for which information
   is stored is reflected in the, well, type (e.g. `Type[String]` vs `Type[Int]`), we can use them in type bounds, implicits,
   etc.
 * `UntypedType` - corresponds to `c.Type` or `q.reflect.TypeRepr`. Because the type for which information is stored
   is NOT reflected in the type, it's useful for working with the types of unknown kind (e.g. `A` vs `F[_]` vs `G[_[_]]`, etc)
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

### Why do so many types exist?

All 3 are necessary:

 * `Type[A]` represents the typed form of a proper type. If a method is type parametric, this is how
   the information is passed into the macro. (While Scala 3 allows handling any kind, on Scala 2 only
   proper types can be passed with typed representation.) It's also required for creating typed expressions on Scala 3.
   For other purposes it might not be _required_, but it's _easier to reason_ about the code if we keep things typed
   as much as possible.
 * `UntypedType` represents the untyped representation of a type. This allows handling types of any kind on Scala 2
   and also passing around "work in progress" kinds of types, e.g. something that is not yet a type constructor but needs to be modified.
   They are more powerful, but it's a low-level API, so **we avoid it as much as possible** in the Hearth API.
 * `Expr_??` exists because on Scala 3 we cannot use something like `Expr[?]` or `Expr[_]`, especially if we have to pass
   `Expr[_]` and `Type[_]` together and be able to prove that they both use the same existential type.
   Meanwhile, we need to represent, e.g., a list of annotations (we don't know their types ahead of time).
   `UntypedExpr`s would be overkill—we know their type, we could splice them or return from the macro—but we have to
   "forget" their actual type to, e.g., fit a bunch of them into some collection. We can still name their type and make it available
   as an `implicit`/`given`.

So we should prefer typed `Expr` when actually working with it, `Expr_??` when we have to pass around a collection of types,
and only use `UntypedExpr` if we need to design some low-level API ourselves to handle cases that the typed API would not.

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

You can see 3 different representations of expressions in the Hearth codebase:

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

### Why do so many expr representations exist?

All 3 are necessary:

 * `Expr[A]` represents the typed expression. If a method takes arguments, this is how the body of an argument is passed into the macro.
   This is what Scala 3's quotes create, and what Scala 2 quasi-quotes are aligned to by Hearth.
   Only typed expressions allow us to quote and splice in both Scala 2 and Scala 3, which is why this is the preferred representation.
   For other purposes it might not be required, but it's easier to reason about the code if we keep things typed as much as possible.
 * `UntypedExpr` represents the AST tree. This representation allows us to use the expression while constructing other AST nodes,
   which aren't necessarily representable as standalone expressions.
   They are more powerful, but it's a low-level API, so **we avoid it as much as possible** in Hearth.
 * `Expr_??` exists because on Scala 3 we cannot always use something like `Type[?]` or `Type[_]`. Meanwhile, we need to
   represent, e.g., a list of types of some method arguments or a list of children of some `enum`. `UntypedType`s would be overkill—we
   know that they are proper types, ready to use—but we have to "forget" their actual type to, e.g., fit a bunch of them
   into some collection. And we can easily make any of them available as an `implicit`/`given` while giving them a name.

So we should prefer typed `Expr` when actually working with it, `Expr_??` when we have to pass around a collection of expressions,
and only use `UntypedExpr` if we need to design some low-level API ourselves to handle cases that the typed API would not.

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

| Companion method                                                                     | Extension method                                                                 | Result type                         | Description                                                     |
|--------------------------------------------------------------------------------------|----------------------------------------------------------------------------------|-------------------------------------|-----------------------------------------------------------------|
| `Expr.summonImplicit[Ordering[String]]`                                              | `Type[Ordering[String]].summonExpr`                                              | `SummoningResult[Ordering[String]]` | summon implicit value as expression, on failure give reason why |
| `Expr.summonImplicitIgnoring[Ordering[String]](untypedMethod1, untypedMethod2, ...)` | `Type[Ordering[String]].summonExprIgnoring(untypedMethod1, untypedMethod2, ...)` | `SummoningResult[Ordering[String]]` | same as above, but ignores the symbols of provided methods      |

`summonImplicitIgnoring`/`summonExprIgnoring` rely on functionality that, on Scala 2.13, is available since 2.13.17 and on Scala 3 since 3.7.0 - it will throw if you use it on lower versions of Scala.

It can be used to e.g. prevent the macro from summoning itself, so that recursion will be handled within the same macro expansion by the macro.

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

Imagine you need to implement a macro with the following signature:

```scala
def someMethod(args: String*): Unit
```

Once you start implementing it, you will find out that:

 * on Scala 2 you would have to expect `Seq[Expr[String]]`
 * but on Scala 3 you would have to pass `Expr[Seq[String]]`

If you want to keep one version of the code for both implementations, you have to add adapters
that convert variadic arguments into the same representation.

`VarArgs` allows cross-platform handling of variadic arguments (Scala 2 uses `Seq[Expr[A]]`, Scala 3 uses `Expr[Seq[A]]`):

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

### `FreshName`

`FreshName` describes a strategy for generating fresh variable names to avoid clashes, used, e.g., by [`MatchCase`](#matchcase)es,
[`ValDefs`](#valdefs), [`ValDefBuilder`](#valdefbuilder)s, [`ValDefsCache`](#valdefscache) and [`LambdaBuilder`](#lambdabuilder)s.

When we need to generate a new `val`/`var`/`lazy val`/`def`/`case` binding, we cannot safely use just any string as a name;
we have to make sure there will be no name conflict. With `FreshName` we can tell the utilities
that create them which strategy we prefer:

| Syntax                         | Example               | Description                                                     |
|--------------------------------|-----------------------|-----------------------------------------------------------------|
| `FreshName.FromType`           | `string$macro$1`      | use lowercased simple name of the type as a fresh name prefix   |
| `FreshName.FromExpr`           | `sthtostring$macro$1` | use printed expr as a fresh name prefix (if available)          |
| `FreshName.FromPrefix("name")` | `name$macro$1`        | use provided name as a fresh name prefix                        |
| `"name"`                       | `name$macro$1`        | when implicit conversions are enabled, turns string into prefix |

### `MatchCase`

If you know the type of the expression that you would pattern match on, and all the cases upfront, things are simple:

```scala
val eitherExpr: Expr[Either[String, Int]] = ...

def rightCase(right: Expr[String]): Expr[Result]
def leftCase(left: Expr[Int]): Expr[Result]

Expr.quote {
  Expr.splice(eitherExpr) match {
    case Right(int) => Expr.splice { rightCase(Expr.quote(int)) }
    case Left(string) => Expr.splice { leftCase(Expr.quote(string)) }
  }
}
```

But what if:

 - the type is only resolved during the macro expansion?
 - and cases are figured out during the macro expansion as well?
 - maybe with some error handling (`Either[Error, Expr[Result]]`), where you would like to aggregate them
   (then direct style won't be enough)?

Then we would have to build that expression programmatically with `MatchCase`.

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

### `ValDefs`

If you know the types and number of `val`/`var`/`lazy val`/`def` upfront, things are simple:

```scala
Expr.quote {
  val definition1 = Expr.splice { ... }
  var definition2 = Expr.splice { ... }
  lazy val definition2 = Expr.splice { ... }
  def definition2 = { ... }
  ... // the code that uses the definitions defined above
}
```

But what if:

 - the types are only resolved during the macro expansion?
 - you don't know how many you will need?
 - maybe you have to compute them with some error handling (`Either[Error, Expr[Result]]`), and you would like to aggregate them (then direct style won't be enough)?

Then we would have to create these definitions programmatically with automatic scope management using `ValDefs`:

!!! example "How `ValDefs` are used"

    ```scala
    // file: src/main/scala/example/ValDefsMacro.scala - part of ValDefs example
    //> using scala {{ scala.2_13 }} {{ scala.3 }}
    //> using dep com.kubuszok::hearth:{{ hearth_version() }}

    import hearth.fp.syntax.*

    trait ValDefsMacro { this: hearth.MacroCommons =>

      private val IntType = Type.of[Int]

      // Use each definition immediately:
      def createAndUse(value: Expr[Int]): Expr[Int] = {
        implicit val intType: Type[Int] = IntType

        // Create a val: { val a = value; a + 1 }
        val valResult = ValDefs.createVal(value, "a").use { (a: Expr[Int]) =>
          Expr.quote(Expr.splice(a) + 1)
        }

        // Create a var: { var b = value; b = b + 1; b * 10 }
        val varResult = ValDefs.createVar(value, "b").use { case (b, set) =>
          Expr.quote {
            Expr.splice(set(Expr.quote(Expr.splice(b) + 1)))
            Expr.splice(b) * 10
          }
        }

        // Create a lazy val: { lazy val c = value; (c + 2) * 100 }
        val lazyResult = ValDefs.createLazy(value, "c").use { (c: Expr[Int]) =>
          Expr.quote((Expr.splice(c) + 2) * 100)
        }

        // Create a def: { def d = value; (d + 3) * 1000 }
        val defResult = ValDefs.createDef(value, "d").use { (d: Expr[Int]) =>
          Expr.quote((Expr.splice(d) + 3) * 1000)
        }

        // Create:
        // { val a = value; a + 1 } +
        //   { var b = value; b = b + 1; b * 10 } +
        //   { lazy val c = value; (c + 2) * 100 } +
        //   { def d = value; (d + 3) * 1000 }
        Expr.quote {
          Expr.splice(valResult) + Expr.splice(varResult) + 
          Expr.splice(lazyResult) + Expr.splice(defResult)
        }
      }

      // Combine definitions into a single scope before using them
      def createCombineThenUse(value: Expr[Int]): Expr[Int] = {
        implicit val intType: Type[Int] = IntType

        // Create a val: val a = value
        // AND create an expr: a + 1
        val valResult = ValDefs.createVal(value, "a").map { (a: Expr[Int]) =>
          Expr.quote(Expr.splice(a) + 1)
        }

        // Create a var: var b = value
        // AND create an expr: b = b + 1; b * 10
        val varResult = ValDefs.createVar(value, "b").map { case (b, set) =>
          Expr.quote {
            Expr.splice(set(Expr.quote(Expr.splice(b) + 1)))
            Expr.splice(b) * 10
          }
        }

        // Create a lazy val: lazy val c = value
        // AND create an expr: (c + 2) * 100
        val lazyResult = ValDefs.createLazy(value, "c").map { (c: Expr[Int]) =>
          Expr.quote((Expr.splice(c) + 2) * 100)
        }

        // Create a def: def d = value
        // AND create an expr: (d + 3) * 1000
        val defResult = ValDefs.createDef(value, "d").map { (d: Expr[Int]) =>
          Expr.quote((Expr.splice(d) + 3) * 1000)
        }

        // Create:
        // {
        //   val a = value
        //   var b = value
        //   lazy val c = value
        //   def d = value
        //   { a + 1 } +
        //     { b = b + 1; b * 10 } +
        //     { (c + 2) * 100 } +
        //     { (d + 3) * 1000 }
        // }
        valResult.tuple(varResult.tuple(lazyResult.tuple(defResult))).use {
          case (valExpr, (varExpr, (lazyExpr, defExpr))) =>
            Expr.quote {
              Expr.splice(valExpr) + Expr.splice(varExpr) + 
              Expr.splice(lazyExpr) + Expr.splice(defExpr)
            }
        }
      }
    }
    ```

    ```scala
    // file: src/main/scala-2/example/ValDefsMacroImpl.scala - part of ValDefs example
    //> using target.scala {{ scala.2_13 }}
    //> using options -Xsource:3

    import scala.language.experimental.macros
    import scala.reflect.macros.blackbox

    object Example {
      def createAndUse(value: Int): Int = macro ValDefsMacroImpl.createAndUseImpl
      def createCombineThenUse(value: Int): Int = macro ValDefsMacroImpl.createCombineThenUseImpl
    }

    // Scala 2 adapter
    class ValDefsMacroImpl(val c: blackbox.Context) extends hearth.MacroCommonsScala2 with ValDefsMacro {

      def createAndUseImpl(value: c.Expr[Int]): c.Expr[Int] =
        createAndUse(value)

      def createCombineThenUseImpl(value: c.Expr[Int]): c.Expr[Int] =
        createCombineThenUse(value)
    }
    ```

    ```scala
    // file: src/main/scala-3/example/ValDefsMacroImpl.scala - part of ValDefs example
    //> using target.scala {{ scala.3 }}
    //> using plugin com.kubuszok::hearth-cross-quotes::{{ hearth_version() }}

    import scala.quoted.*

    object Example {
      inline def createAndUse(inline value: Int): Int = ${ ValDefsMacroImpl.createAndUseImpl('value) }
      inline def createCombineThenUse(inline value: Int): Int = ${ ValDefsMacroImpl.createCombineThenUseImpl('value) }
    }

    // Scala 3 adapter
    class ValDefsMacroImpl(q: Quotes) extends hearth.MacroCommonsScala3(using q), ValDefsMacro
    object ValDefsMacroImpl {

      def createAndUseImpl(value: Expr[Int])(using q: Quotes): Expr[Int] =
        new ValDefsMacroImpl(q).createAndUse(value)
      def createCombineThenUseImpl(value: Expr[Int])(using q: Quotes): Expr[Int] =
        new ValDefsMacroImpl(q).createCombineThenUse(value)
    }
    ```

    ```scala
    // file: src/test/scala/example/ExampleSpec.scala - part of ValDefs example
    //> using test.dep org.scalameta::munit::{{ libraries.munit }}

    final class ExampleSpec extends munit.FunSuite {

      test("Example.createAndUse should work") {
        val result = Example.createAndUse(1)
        // val: 1 + 1 = 2
        // var: (1 + 1) * 10 = 20
        // lazy: (1 + 2) * 100 = 300
        // def: (1 + 3) * 1000 = 4000
        assertEquals(result, 2 + 20 + 300 + 4000)
      }

      test("Example.createCombineThenUse should work") {
        val result = Example.createCombineThenUse(1)
        // val: 1 + 1 = 2
        // var: (1 + 1) * 10 = 20
        // lazy: (1 + 2) * 100 = 300
        // def: (1 + 3) * 1000 = 4000
        assertEquals(result, 2 + 20 + 300 + 4000)
      }
    }
    ```

!!! tip "`ValDefs` utilities are not needed if you can make `var`/`val`/`lazy val`/`def` inside (cross) quotes. Only if you need to use the fresh names they become necessary to avoid the name clashes."

!!! tip "`ValDefs` creates `var`/`val`/`lazy val`/`def` when you know their (initial) value upfront. If you need to construct it, use [`ValDefBuilder`](#valdefbuilder)."

### `ValDefBuilder`

If your use case looks similar to [`ValDefs`](#valdefs) but you cannot provide the initial value immediately,
or your `def` has an arity larger than 0, you may need to use a builder to construct the `ValDefs` before you
can use it.

`ValDefBuilder` allows building definitions (`val`, `var`, `lazy val`, `def`) programmatically with
a support for error aggregation and recursive definitions:

!!! example "How `ValDefBuilder` is used"

    ```scala
    // file: src/main/scala/example/ValDefBuilderMacro.scala - part of ValDefBuilder example
    //> using scala {{ scala.2_13 }} {{ scala.3 }}
    //> using dep com.kubuszok::hearth:{{ hearth_version() }}

    import hearth.fp.syntax.*

    trait ValDefBuilderMacro { this: hearth.MacroCommons =>

      private val IntType = Type.of[Int]

      def buildDefinitions: Expr[Int] = {
        implicit val intType: Type[Int] = IntType

        // Build a val: val x = 1
        val valDef = ValDefBuilder.ofVal[Int]("x").map(_ => Expr.quote(1)).build.close

        // Build a var: var x = 2
        val varDef = ValDefBuilder.ofVar[Int]("x").map(_ => Expr.quote(2)).build.close

        // Build a lazy val: lazy val x = 3
        val lazyDef = ValDefBuilder.ofLazy[Int]("x").map(_ => Expr.quote(3)).build.close

        // Build a def with no parameters: def x() = 0
        val def0 = ValDefBuilder.ofDef0[Int]("x").buildWith(_ => Expr.quote(0)).close

        // Build a def with one parameter: def x(a: Int) = a * 10
        val def1 = ValDefBuilder
          .ofDef1[Int, Int]("x", "a")
          .buildWith { case (_, a) =>
            Expr.quote(Expr.splice(a) * 10)
          }
          .use(_(Expr.quote(1)))

        // Build a def with two parameters: def x(a: Int, b: Int) = (a + b) * 10
        val def2 = ValDefBuilder
          .ofDef2[Int, Int, Int]("x", "a", "b")
          .buildWith { case (_, (a, b)) =>
            Expr.quote((Expr.splice(a) + Expr.splice(b)) * 10)
          }
          .use(_(Expr.quote(1), Expr.quote(2)))

        Expr.quote {
          Expr.splice(valDef) + Expr.splice(varDef) + Expr.splice(lazyDef) +
          Expr.splice(def0) + Expr.splice(def1) + Expr.splice(def2)
        }
      }
    }
    ```

    ```scala
    // file: src/main/scala-2/example/ValDefBuilderMacroImpl.scala - part of ValDefBuilder example
    //> using target.scala {{ scala.2_13 }}
    //> using options -Xsource:3

    import scala.language.experimental.macros
    import scala.reflect.macros.blackbox

    object Example {
      def buildDefinitions: Int = macro ValDefBuilderMacroImpl.buildDefinitionsImpl
    }

    // Scala 2 adapter
    class ValDefBuilderMacroImpl(val c: blackbox.Context) extends hearth.MacroCommonsScala2 with ValDefBuilderMacro {

      def buildDefinitionsImpl: c.Expr[Int] = buildDefinitions
    }
    ```

    ```scala
    // file: src/main/scala-3/example/ValDefBuilderMacroImpl.scala - part of ValDefBuilder example
    //> using target.scala {{ scala.3 }}
    //> using plugin com.kubuszok::hearth-cross-quotes::{{ hearth_version() }}

    import scala.quoted.*

    object Example {
      inline def buildDefinitions: Int = ${ ValDefBuilderMacroImpl.buildDefinitionsImpl }
    }

    // Scala 3 adapter
    class ValDefBuilderMacroImpl(q: Quotes) extends hearth.MacroCommonsScala3(using q), ValDefBuilderMacro
    object ValDefBuilderMacroImpl {

      def buildDefinitionsImpl(using q: Quotes): Expr[Int] =
        new ValDefBuilderMacroImpl(q).buildDefinitions
    }
    ```

    ```scala
    // file: src/test/scala/example/ExampleSpec.scala - part of ValDefBuilder example
    //> using test.dep org.scalameta::munit::{{ libraries.munit }}

    final class ExampleSpec extends munit.FunSuite {

      test("Example.buildDefinitions should work") {
        val result = Example.buildDefinitions
        assertEquals(result, 1 + 2 + 3 + 0 + 10 + 30)
      }
    }
    ```

`def`s are supported up to 22 parameters (`ofDef0` through `ofDef22`).

!!! tip "`ValDefBuilder` is useful when you need to build definitions programmatically, especially when you need recursive definitions or error aggregation with effects like `MIO`."

### `ValDefsCache`

Let's say you need to construct a lot of definitions in the same scope. They might also call each other,
e.g. if you are deriving a type class and want to reuse some subroutines.

It would be handy to:

 * create some storage of all definitions created in this scope to pass around only 1 value
 * tracking which of them are still constructed and which are already done
 * but allowing us e.g. to call the `def`s whose bodies we are still computing
   but whose signature is already known (we could just "forward declare them")

`ValDefsCache` is exactly such an utility:

!!! example "How `ValDefsCache` is used"

    ```scala
    // file: src/main/scala/example/ValDefsCacheMacro.scala - part of ValDefsCache example
    //> using scala {{ scala.2_13 }} {{ scala.3 }}
    //> using dep com.kubuszok::hearth:{{ hearth_version() }}

    trait ValDefsCacheMacro { this: hearth.MacroCommons =>

      private val IntType = Type.of[Int]

      def buildCached: Expr[Int] = {
        implicit val intType: Type[Int] = IntType
        var cache = ValDefsCache.empty

        // Build and cache a val
        val valBuilder = ValDefBuilder.ofVal[Int]("x")
        cache = valBuilder.buildCachedWith(cache, "valKey")(_ => Expr.quote(1))
        val valValue = cache.get0Ary[Int]("valKey").fold(Expr.quote(0))(identity)

        // Build and cache a def with one parameter
        val defBuilder = ValDefBuilder.ofDef1[Int, Int]("x", "a")
        cache = defBuilder.buildCachedWith(cache, "defKey") { case (_, a) =>
          Expr.quote(Expr.splice(a) * 10)
        }
        val defValue = cache.get1Ary[Int, Int]("defKey").fold(Expr.quote(0))(_(Expr.quote(2)))

        // Convert cache to ValDefs and use it
        cache.toValDefs.use { _ =>
          Expr.quote {
            Expr.splice(valValue) + Expr.splice(defValue)
          }
        }
      }
    }
    ```

    ```scala
    // file: src/main/scala-2/example/ValDefsCacheMacroImpl.scala - part of ValDefsCache example
    //> using target.scala {{ scala.2_13 }}
    //> using options -Xsource:3

    import scala.language.experimental.macros
    import scala.reflect.macros.blackbox

    object Example {
      def buildCached: Int = macro ValDefsCacheMacroImpl.buildCachedImpl
    }

    // Scala 2 adapter
    class ValDefsCacheMacroImpl(val c: blackbox.Context) extends hearth.MacroCommonsScala2 with ValDefsCacheMacro {

      def buildCachedImpl: c.Expr[Int] = buildCached
    }
    ```

    ```scala
    // file: src/main/scala-3/example/ValDefsCacheMacroImpl.scala - part of ValDefsCache example
    //> using target.scala {{ scala.3 }}
    //> using plugin com.kubuszok::hearth-cross-quotes::{{ hearth_version() }}

    import scala.quoted.*

    object Example {
      inline def buildCached: Int = ${ ValDefsCacheMacroImpl.buildCachedImpl }
    }

    // Scala 3 adapter
    class ValDefsCacheMacroImpl(q: Quotes) extends hearth.MacroCommonsScala3(using q), ValDefsCacheMacro
    object ValDefsCacheMacroImpl {

      def buildCachedImpl(using q: Quotes): Expr[Int] =
        new ValDefsCacheMacroImpl(q).buildCached
    }
    ```

    ```scala
    // file: src/test/scala/example/ExampleSpec.scala - part of ValDefsCache example
    //> using test.dep org.scalameta::munit::{{ libraries.munit }}

    final class ExampleSpec extends munit.FunSuite {

      test("Example.buildCached should work") {
        val result = Example.buildCached
        assertEquals(result, 1 + 20)
      }
    }
    ```

!!! example "Using `ValDefsCache` with `MIO` for forward declarations"

    ```scala
    // file: src/main/scala/example/ValDefsCacheMioMacro.scala - part of ValDefsCache MIO example
    //> using scala {{ scala.2_13 }} {{ scala.3 }}
    //> using dep com.kubuszok::hearth:{{ hearth_version() }}

    import hearth.fp.syntax.*
    import hearth.fp.instances.*

    trait ValDefsCacheMioMacro { this: hearth.MacroCommons =>

      private val IntType = Type.of[Int]

      def buildWithForwardDeclare: Expr[Int] = {
        implicit val intType: Type[Int] = IntType
        val cacheLocal = ValDefsCache.mlocal

        val defBuilder = ValDefBuilder.ofDef1[Int, Int]("x", "a")
        
        val result = for {
          // Forward declare a def before building it
          _ <- cacheLocal.forwardDeclare("defKey", defBuilder)
          
          // Now we can use it before it's built (e.g., in recursive calls)
          _ <- cacheLocal.buildCachedWith("defKey", defBuilder) { case (_, a) =>
            Expr.quote(Expr.splice(a) * 10)
          }
          
          defValue <- cacheLocal.get1Ary[Int, Int]("defKey").map(_.fold(Expr.quote(0))(_(Expr.quote(2))))
          cache <- cacheLocal.get
        } yield cache.toValDefs.use { _ =>
          defValue
        }

        result.runToExprOrFail("buildWithForwardDeclare")((_, _) => "")
      }
    }
    ```

    ```scala
    // file: src/main/scala-2/example/ValDefsCacheMioMacroImpl.scala - part of ValDefsCache MIO example
    //> using target.scala {{ scala.2_13 }}
    //> using options -Xsource:3

    import scala.language.experimental.macros
    import scala.reflect.macros.blackbox

    object Example {
      def buildWithForwardDeclare: Int = macro ValDefsCacheMioMacroImpl.buildWithForwardDeclareImpl
    }

    // Scala 2 adapter
    class ValDefsCacheMioMacroImpl(val c: blackbox.Context) extends hearth.MacroCommonsScala2 with ValDefsCacheMioMacro {

      def buildWithForwardDeclareImpl: c.Expr[Int] = buildWithForwardDeclare
    }
    ```

    ```scala
    // file: src/main/scala-3/example/ValDefsCacheMioMacroImpl.scala - part of ValDefsCache MIO example
    //> using target.scala {{ scala.3 }}
    //> using plugin com.kubuszok::hearth-cross-quotes::{{ hearth_version() }}

    import scala.quoted.*

    object Example {
      inline def buildWithForwardDeclare: Int = ${ ValDefsCacheMioMacroImpl.buildWithForwardDeclareImpl }
    }

    // Scala 3 adapter
    class ValDefsCacheMioMacroImpl(q: Quotes) extends hearth.MacroCommonsScala3(using q), ValDefsCacheMioMacro
    object ValDefsCacheMioMacroImpl {

      def buildWithForwardDeclareImpl(using q: Quotes): Expr[Int] =
        new ValDefsCacheMioMacroImpl(q).buildWithForwardDeclare
    }
    ```

    ```scala
    // file: src/test/scala/example/ExampleSpec.scala - part of ValDefsCache MIO example
    //> using test.dep org.scalameta::munit::{{ libraries.munit }}

    final class ExampleSpec extends munit.FunSuite {

      test("Example.buildWithForwardDeclare should work") {
        val result = Example.buildWithForwardDeclare
        assertEquals(result, 20)
      }
    }
    ```

!!! tip "`ValDefsCache` is essential when building recursive definitions or when you need to reference definitions before they're fully constructed. Use `ValDefsCache.mlocal` with `MIO` for effectful computations."

### `LambdaBuilder`

If you know the types and number of parameters upfront, things are simple:

```scala
Expr.quote {
  val lambda1 = (a: Int) => Expr.splice { ... }
  val lambda2 = (a: Int, b: String) => Expr.splice { ... }
  ... // the code that uses the lambdas defined above
}
```

But what if:

 - the types are only resolved during the macro expansion?
 - you don't know how many parameters you will need?
 - maybe you have to compute them with some error handling (`Either[Error, Expr[Result]]`), and you would like to aggregate them (then direct style won't be enough)?

Then we would have to create these lambdas programmatically using `LambdaBuilder`:

!!! example "How `LambdaBuilder` is used"

    ```scala
    // file: src/main/scala/example/LambdaBuilderMacro.scala - part of LambdaBuilder example
    //> using scala {{ scala.2_13 }} {{ scala.3 }}
    //> using dep com.kubuszok::hearth:{{ hearth_version() }}

    import hearth.fp.syntax.*

    trait LambdaBuilderMacro { this: hearth.MacroCommons =>

      private val IntType = Type.of[Int]

      def buildLambdas: Expr[Int] = {
        implicit val intType: Type[Int] = IntType

        // Build a lambda with one parameter: (a: Int) => a + 1
        val lambda1 = LambdaBuilder
          .of1[Int]("a")
          .map { case (a) =>
            Expr.quote(Expr.splice(a) + 1)
          }
          .build

        // Build a lambda with two parameters: (a: Int, b: Int) => a * b + 1
        val lambda2 = LambdaBuilder
          .of2[Int, Int]("a", "b")
          .map { case ((a, b)) =>
            Expr.quote(Expr.splice(a) * Expr.splice(b) + 1)
          }
          .build

        // Build a lambda using buildWith: (a: Int) => a + 1
        val lambda3 = LambdaBuilder
          .of1[Int]("a")
          .buildWith { case (a) =>
            Expr.quote(Expr.splice(a) + 1)
          }

        Expr.quote {
          Expr.splice(lambda1)(2) + Expr.splice(lambda2)(2, 3) + Expr.splice(lambda3)(2)
        }
      }
    }
    ```

    ```scala
    // file: src/main/scala-2/example/LambdaBuilderMacroImpl.scala - part of LambdaBuilder example
    //> using target.scala {{ scala.2_13 }}
    //> using options -Xsource:3

    import scala.language.experimental.macros
    import scala.reflect.macros.blackbox

    object Example {
      def buildLambdas: Int = macro LambdaBuilderMacroImpl.buildLambdasImpl
    }

    // Scala 2 adapter
    class LambdaBuilderMacroImpl(val c: blackbox.Context) extends hearth.MacroCommonsScala2 with LambdaBuilderMacro {

      def buildLambdasImpl: c.Expr[Int] = buildLambdas
    }
    ```

    ```scala
    // file: src/main/scala-3/example/LambdaBuilderMacroImpl.scala - part of LambdaBuilder example
    //> using target.scala {{ scala.3 }}
    //> using plugin com.kubuszok::hearth-cross-quotes::{{ hearth_version() }}

    import scala.quoted.*

    object Example {
      inline def buildLambdas: Int = ${ LambdaBuilderMacroImpl.buildLambdasImpl }
    }

    // Scala 3 adapter
    class LambdaBuilderMacroImpl(q: Quotes) extends hearth.MacroCommonsScala3(using q), LambdaBuilderMacro
    object LambdaBuilderMacroImpl {

      def buildLambdasImpl(using q: Quotes): Expr[Int] =
        new LambdaBuilderMacroImpl(q).buildLambdas
    }
    ```

    ```scala
    // file: src/test/scala/example/ExampleSpec.scala - part of LambdaBuilder example
    //> using test.dep org.scalameta::munit::{{ libraries.munit }}

    final class ExampleSpec extends munit.FunSuite {

      test("Example.buildLambdas should work") {
        val result = Example.buildLambdas
        // lambda1(2) = 2 + 1 = 3
        // lambda2(2, 3) = 2 * 3 + 1 = 7
        // lambda3(2) = 2 + 1 = 3
        assertEquals(result, 3 + 7 + 3)
      }
    }
    ```

Supports up to 22 parameters (`of1` through `of22`).

!!! tip "`LambdaBuilder`s are not needed if you can make the lambda inside (cross) quotes. Only if the types and/or number of the arguments is unknown during the compilation of the macro and have to be resolved, you need to use a lambda builder to stay flexible."

## `Method`

If you know the method name and its signature upfront, things are simple:

```scala
Expr.quote {
  instance.someMethod(arg1, arg2)
  Companion.staticMethod(arg)
  new MyClass(arg1, arg2)
}
```

But what if:

 - you need to find a method by name dynamically (e.g., from a string literal)?
 - you need to introspect method parameters, annotations, or other metadata?
 - you need to handle different method types (instance methods vs static methods vs constructors)?
 - you need to validate arguments and handle errors before calling?

Then we would have to work with `Method` representations that provide introspection and programmatic method invocation.

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
    // file: src/main/scala/example/MethodPatternMatchingMacro.scala - part of Method pattern matching example
    //> using scala {{ scala.2_13 }} {{ scala.3 }}
    //> using dep com.kubuszok::hearth:{{ hearth_version() }}

    trait MethodPatternMatchingMacro { this: hearth.MacroCommons =>

      private val IntType = Type.of[Int]

      def handleMethod[A: Type](methodName: Expr[String]): Expr[String] = {
        implicit val intType: Type[Int] = IntType
        val name = Expr
          .unapply(methodName)
          .getOrElse(
            Environment.reportErrorAndAbort(s"Method name must be a string literal, got ${methodName.prettyPrint}")
          )
        
        val method: Method[A, Int] = Type[A].methods.filter(_.value.name == name) match {
          case Nil           => Environment.reportErrorAndAbort(s"Method $name not found")
          case method :: Nil =>
            import method.Underlying as Returned
            if (!(Returned <:< Type.of[Int])) Environment.reportErrorAndAbort(s"Method $name returns not an Int")
            else method.value.asInstanceOf[Method[A, Int]]
          case _ => Environment.reportErrorAndAbort(s"Method $name is not unique")
        }
        
        method match {
          case noInstance: Method.NoInstance[Int] @unchecked =>
            Expr.quote(s"Found no-instance method ${Expr.splice(methodName)}")
            
          case ofInstance: Method.OfInstance[A, Int] @unchecked =>
            import ofInstance.{ Returned }
            Expr.quote(s"Found instance method ${Expr.splice(methodName)} on ${Expr.splice(Expr(Type[A].plainPrint))}")
            
          case unsupported: Method.Unsupported[A, Int] @unchecked =>
            Environment.reportErrorAndAbort(s"Method ${name} is unsupported: ${unsupported.reasonForUnsupported}")
        }
      }
    }
    ```

    ```scala
    // file: src/main/scala-2/example/MethodPatternMatchingMacroImpl.scala - part of Method pattern matching example
    //> using target.scala {{ scala.2_13 }}
    //> using options -Xsource:3

    import scala.language.experimental.macros
    import scala.reflect.macros.blackbox

    object Example {
      def handleMethod[A](methodName: String): String = macro MethodPatternMatchingMacroImpl.handleMethodImpl[A]
    }

    // Scala 2 adapter
    class MethodPatternMatchingMacroImpl(val c: blackbox.Context) extends hearth.MacroCommonsScala2 with MethodPatternMatchingMacro {

      def handleMethodImpl[A: c.WeakTypeTag](methodName: c.Expr[String]): c.Expr[String] =
        handleMethod[A](methodName)
    }
    ```

    ```scala
    // file: src/main/scala-3/example/MethodPatternMatchingMacroImpl.scala - part of Method pattern matching example
    //> using target.scala {{ scala.3 }}
    //> using plugin com.kubuszok::hearth-cross-quotes::{{ hearth_version() }}

    import scala.quoted.*

    object Example {
      inline def handleMethod[A](inline methodName: String): String = ${ MethodPatternMatchingMacroImpl.handleMethodImpl[A]('{ methodName }) }
    }

    // Scala 3 adapter
    class MethodPatternMatchingMacroImpl(q: Quotes) extends hearth.MacroCommonsScala3(using q), MethodPatternMatchingMacro
    object MethodPatternMatchingMacroImpl {

      def handleMethodImpl[A: Type](methodName: Expr[String])(using q: Quotes): Expr[String] =
        new MethodPatternMatchingMacroImpl(q).handleMethod[A](methodName)
    }
    ```

    ```scala
    // file: src/test/scala/example/ExampleSpec.scala - part of Method pattern matching example
    //> using test.dep org.scalameta::munit::{{ libraries.munit }}

    final class ExampleSpec extends munit.FunSuite {

      test("Example.handleMethod should identify method types") {
        class TestClass {
          def instanceMethod(x: Int): Int = x + 1
        }
        object TestClass {
          def staticMethod(x: Int): Int = x + 2
        }
        
        assertEquals(Example.handleMethod[TestClass]("instanceMethod"), "Found instance method instanceMethod on ExampleSpec.TestClass")
        assertEquals(Example.handleMethod[TestClass]("staticMethod"), "Found no-instance method staticMethod")
      }
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

!!! example "Calling constructors and instance methods"

    ```scala
    // file: src/main/scala/example/CallingMethodsMacro.scala - part of calling Methods example
    //> using scala {{ scala.2_13 }} {{ scala.3 }}
    //> using dep com.kubuszok::hearth:{{ hearth_version() }}

    trait CallingMethodsMacro { this: hearth.MacroCommons =>

      private val IntType = Type.of[Int]

      // Call a no-instance method (constructor or companion method)
      def callNoInstanceMethod[A: Type](methodName: Expr[String])(params: VarArgs[Int]): Expr[Int] = {
        implicit val intType: Type[Int] = IntType
        val name = Expr
          .unapply(methodName)
          .getOrElse(
            Environment.reportErrorAndAbort(s"Method name must be a string literal, got ${methodName.prettyPrint}")
          )
        val method: Method[A, Int] = Type[A].methods.filter(_.value.name == name) match {
          case Nil           => Environment.reportErrorAndAbort(s"Method $name not found")
          case method :: Nil =>
            import method.Underlying as Returned
            if (!(Returned <:< Type.of[Int])) Environment.reportErrorAndAbort(s"Method $name returns not an Int")
            else method.value.asInstanceOf[Method[A, Int]]
          case _ => Environment.reportErrorAndAbort(s"Method $name is not unique")
        }
        method match {
          case noInstance: Method.NoInstance[Int] @unchecked =>
            val providedParams = params.toVector
            val arguments = noInstance.parameters.flatten.zipWithIndex.flatMap { case ((name, param), index) =>
              providedParams.lift(index) match {
                case _ if !(param.tpe.Underlying <:< Type.of[Int]) =>
                  Environment.reportErrorAndAbort(s"Parameter $name has wrong type: ${param.tpe.plainPrint} is not an Int")
                case Some(value)              => Some(name -> value.as_??)
                case None if param.hasDefault => None
                case _ => Environment.reportErrorAndAbort(s"Missing parameter for $name (not default value as well)")
              }
            }.toMap
            noInstance.apply(arguments) match {
              case Right(result) => result
              case Left(error)   => Environment.reportErrorAndAbort(s"Failed to call method $name: $error")
            }
          case _: Method.OfInstance[A, Int] @unchecked =>
            Environment.reportErrorAndAbort(s"Method $name is not a no-instance method")
          case unsupported: Method.Unsupported[A, Int] @unchecked =>
            Environment.reportErrorAndAbort(s"Method $name is unsupported: ${unsupported.reasonForUnsupported}")
        }
      }

      // Call an instance method
      def callInstanceMethod[A: Type](instance: Expr[A])(methodName: Expr[String])(params: VarArgs[Int]): Expr[Int] = {
        implicit val intType: Type[Int] = IntType
        val name = Expr
          .unapply(methodName)
          .getOrElse(
            Environment.reportErrorAndAbort(s"Method name must be a string literal, got ${methodName.prettyPrint}")
          )
        val method: Method[A, Int] = Type[A].methods.filter(_.value.name == name) match {
          case Nil           => Environment.reportErrorAndAbort(s"Method $name not found")
          case method :: Nil =>
            import method.Underlying as Returned
            if (!(Returned <:< Type.of[Int])) Environment.reportErrorAndAbort(s"Method $name returns not an Int")
            else method.value.asInstanceOf[Method[A, Int]]
          case _ => Environment.reportErrorAndAbort(s"Method $name is not unique")
        }
        method match {
          case _: Method.NoInstance[Int] @unchecked =>
            Environment.reportErrorAndAbort(s"Method $name is not an instance method")
          case ofInstance: Method.OfInstance[A, Int] @unchecked =>
            val providedParams = params.toVector
            val arguments = ofInstance.parameters.flatten.zipWithIndex.flatMap { case ((name, param), index) =>
              providedParams.lift(index) match {
                case _ if !(param.tpe.Underlying <:< Type.of[Int]) =>
                  Environment.reportErrorAndAbort(s"Parameter $name has wrong type: ${param.tpe.plainPrint} is not an Int")
                case Some(value)              => Some(name -> value.as_??)
                case None if param.hasDefault => None
                case _ => Environment.reportErrorAndAbort(s"Missing parameter for $name (not default value as well)")
              }
            }.toMap
            ofInstance.apply(instance, arguments) match {
              case Right(result) => result
              case Left(error)   => Environment.reportErrorAndAbort(s"Failed to call method $name: $error")
            }
          case unsupported: Method.Unsupported[A, Int] @unchecked =>
            Environment.reportErrorAndAbort(s"Method $name is unsupported: ${unsupported.reasonForUnsupported}")
        }
      }
    }
    ```

    ```scala
    // file: src/main/scala-2/example/CallingMethodsMacroImpl.scala - part of calling Methods example
    //> using target.scala {{ scala.2_13 }}
    //> using options -Xsource:3

    import scala.language.experimental.macros
    import scala.reflect.macros.blackbox

    object Example {
      def callNoInstanceMethod[A](methodName: String)(params: Int*): Int = macro CallingMethodsMacroImpl.callNoInstanceMethodImpl[A]
      def callInstanceMethod[A](instance: A)(methodName: String)(params: Int*): Int = macro CallingMethodsMacroImpl.callInstanceMethodImpl[A]
    }

    // Scala 2 adapter
    class CallingMethodsMacroImpl(val c: blackbox.Context) extends hearth.MacroCommonsScala2 with CallingMethodsMacro {

      def callNoInstanceMethodImpl[A: c.WeakTypeTag](methodName: c.Expr[String])(params: c.Expr[Int]*): c.Expr[Int] =
        callNoInstanceMethod[A](methodName)(params)
      
      def callInstanceMethodImpl[A: c.WeakTypeTag](instance: c.Expr[A])(methodName: c.Expr[String])(params: c.Expr[Int]*): c.Expr[Int] =
        callInstanceMethod[A](instance)(methodName)(params)
    }
    ```

    ```scala
    // file: src/main/scala-3/example/CallingMethodsMacroImpl.scala - part of calling Methods example
    //> using target.scala {{ scala.3 }}
    //> using plugin com.kubuszok::hearth-cross-quotes::{{ hearth_version() }}

    import scala.quoted.*

    object Example {
      inline def callNoInstanceMethod[A](inline methodName: String)(inline params: Int*): Int = ${ CallingMethodsMacroImpl.callNoInstanceMethodImpl[A]('{ methodName }, '{ params }) }
      inline def callInstanceMethod[A](inline instance: A)(inline methodName: String)(inline params: Int*): Int = ${ CallingMethodsMacroImpl.callInstanceMethodImpl[A]('{ instance }, '{ methodName }, '{ params }) }
    }

    // Scala 3 adapter
    class CallingMethodsMacroImpl(q: Quotes) extends hearth.MacroCommonsScala3(using q), CallingMethodsMacro
    object CallingMethodsMacroImpl {

      def callNoInstanceMethodImpl[A: Type](methodName: Expr[String], params: Expr[Seq[Int]])(using q: Quotes): Expr[Int] =
        new CallingMethodsMacroImpl(q).callNoInstanceMethod[A](methodName)(params)
      
      def callInstanceMethodImpl[A: Type](instance: Expr[A], methodName: Expr[String], params: Expr[Seq[Int]])(using q: Quotes): Expr[Int] =
        new CallingMethodsMacroImpl(q).callInstanceMethod[A](instance)(methodName)(params)
    }
    ```

    ```scala
    // file: src/test/scala/example/ExampleSpec.scala - part of calling Methods example
    //> using test.dep org.scalameta::munit::{{ libraries.munit }}

    final class ExampleSpec extends munit.FunSuite {

      test("Example.callNoInstanceMethod should call companion methods") {
        class TestClass(val value: Int) {
          def add(x: Int): Int = value + x
        }
        object TestClass {
          def create(x: Int, y: Int): Int = x + y
        }
        
        assertEquals(Example.callNoInstanceMethod[TestClass]("create")(1, 2), 3)
      }

      test("Example.callInstanceMethod should call instance methods") {
        class TestClass(val value: Int) {
          def add(x: Int): Int = value + x
        }
        
        val instance = new TestClass(10)
        assertEquals(Example.callInstanceMethod(instance)("add")(5), 15)
      }
    }
    ```

The methods' `apply` validates that:

- All required parameters are provided (it uses default values for arguments that aren't provided if they exist)
- Argument types match parameter types

and returns `Right(expr)` on success or `Left(error)` on failure.

## `Class`

If you know the type structure upfront, things are simple:

```scala
Expr.quote {
  new MyClass(arg1, arg2)
  instance.someMethod()
  caseClass.field
}
```

But what if:

 - you need to introspect a type to find its constructors or methods?
 - you need to work with specialized views (case classes, enums, Java Beans)?
 - you need to discover available methods or constructors dynamically?
 - you need to aggregate type information for code generation?

Then we would have to work with `Class[A]`, a convenient utility that aggregates information about a type:

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

!!! example "Constructing case class instances"

    ```scala
    // file: src/main/scala/example/CaseClassConstructMacro.scala - part of CaseClass construct example
    //> using scala {{ scala.2_13 }} {{ scala.3 }}
    //> using dep com.kubuszok::hearth:{{ hearth_version() }}

    import hearth.fp.effect.MIO
    import hearth.fp.instances.*
    import hearth.fp.syntax.*

    trait CaseClassConstructMacro { this: hearth.MacroCommons =>

      private val IntType = Type.of[Int]
      private val StringType = Type.of[String]

      def constructCaseClass[A: Type]: Expr[String] = Expr {
        CaseClass.parse[A].fold("<no case class>") { caseClass =>
          val makeArgument: Parameter => MIO[Expr_??] = field => {
            import field.tpe.Underlying as FieldType
            implicit val IntType: Type[Int] = this.IntType
            implicit val StringType: Type[String] = this.StringType
            if (FieldType <:< Type[Int]) MIO.pure(Expr(0).as_??)
            else if (FieldType <:< Type[String]) MIO.pure(Expr(field.name).as_??)
            else MIO.fail(new Exception(s"Field ${field.name} has wrong type: ${field.tpe.plainPrint}"))
          }
          val sequential = caseClass.construct(makeArgument).map { result =>
            result.fold("<failed to construct sequential>")(_.plainPrint)
          }
          val parallel = caseClass.parConstruct(makeArgument).map { result =>
            result.fold("<failed to construct parallel>")(_.plainPrint)
          }
          sequential
            .parMap2(parallel) { (sequential, parallel) =>
              s"sequential: $sequential, parallel: $parallel"
            }
            .unsafe
            .runSync
            ._2
            .fold(errors => s"<failed to construct: ${errors.mkString(", ")}>", identity)
        }
      }
    }
    ```

    ```scala
    // file: src/main/scala-2/example/CaseClassConstructMacroImpl.scala - part of CaseClass construct example
    //> using target.scala {{ scala.2_13 }}
    //> using options -Xsource:3

    import scala.language.experimental.macros
    import scala.reflect.macros.blackbox

    object Example {
      def constructCaseClass[A]: String = macro CaseClassConstructMacroImpl.constructCaseClassImpl[A]
    }

    // Scala 2 adapter
    class CaseClassConstructMacroImpl(val c: blackbox.Context) extends hearth.MacroCommonsScala2 with CaseClassConstructMacro {

      def constructCaseClassImpl[A: c.WeakTypeTag]: c.Expr[String] =
        constructCaseClass[A]
    }
    ```

    ```scala
    // file: src/main/scala-3/example/CaseClassConstructMacroImpl.scala - part of CaseClass construct example
    //> using target.scala {{ scala.3 }}
    //> using plugin com.kubuszok::hearth-cross-quotes::{{ hearth_version() }}

    import scala.quoted.*

    object Example {
      inline def constructCaseClass[A]: String = ${ CaseClassConstructMacroImpl.constructCaseClassImpl[A] }
    }

    // Scala 3 adapter
    class CaseClassConstructMacroImpl(q: Quotes) extends hearth.MacroCommonsScala3(using q), CaseClassConstructMacro
    object CaseClassConstructMacroImpl {

      def constructCaseClassImpl[A: Type](using q: Quotes): Expr[String] =
        new CaseClassConstructMacroImpl(q).constructCaseClass[A]
    }
    ```

    ```scala
    // file: src/test/scala/example/ExampleSpec.scala - part of CaseClass construct example
    //> using test.dep org.scalameta::munit::{{ libraries.munit }}

    final class ExampleSpec extends munit.FunSuite {

      test("Example.constructCaseClass should construct case class instances") {
        case class Person(name: String, age: Int)

        val result = Example.constructCaseClass[Person]
        assert(result.contains("sequential: new Person(\"name\", 0)"))
        assert(result.contains("parallel: new Person(\"name\", 0)"))
      }
    }
    ```

!!! example "Extracting case class field values"

    ```scala
    // file: src/main/scala/example/CaseClassDeconstructMacro.scala - part of CaseClass deconstruct example
    //> using scala {{ scala.2_13 }} {{ scala.3 }}
    //> using dep com.kubuszok::hearth:{{ hearth_version() }}

    trait CaseClassDeconstructMacro { this: hearth.MacroCommons =>

      def extractFields[A: Type](expr: Expr[A]): Expr[String] = Expr {
        CaseClass.parse[A].fold("<no case class>") { caseClass =>
          caseClass
            .caseFieldValuesAt(expr)
            .toList
            .sortBy(_._1)
            .map { case (name, value) =>
              s"$name: ${value.plainPrint}"
            }
            .mkString("(", ", ", ")")
        }
      }
    }
    ```

    ```scala
    // file: src/main/scala-2/example/CaseClassDeconstructMacroImpl.scala - part of CaseClass deconstruct example
    //> using target.scala {{ scala.2_13 }}
    //> using options -Xsource:3

    import scala.language.experimental.macros
    import scala.reflect.macros.blackbox

    object Example {
      def extractFields[A](expr: A): String = macro CaseClassDeconstructMacroImpl.extractFieldsImpl[A]
    }

    // Scala 2 adapter
    class CaseClassDeconstructMacroImpl(val c: blackbox.Context) extends hearth.MacroCommonsScala2 with CaseClassDeconstructMacro {

      def extractFieldsImpl[A: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[String] =
        extractFields[A](expr)
    }
    ```

    ```scala
    // file: src/main/scala-3/example/CaseClassDeconstructMacroImpl.scala - part of CaseClass deconstruct example
    //> using target.scala {{ scala.3 }}
    //> using plugin com.kubuszok::hearth-cross-quotes::{{ hearth_version() }}

    import scala.quoted.*

    object Example {
      inline def extractFields[A](inline expr: A): String = ${ CaseClassDeconstructMacroImpl.extractFieldsImpl[A]('{ expr }) }
    }

    // Scala 3 adapter
    class CaseClassDeconstructMacroImpl(q: Quotes) extends hearth.MacroCommonsScala3(using q), CaseClassDeconstructMacro
    object CaseClassDeconstructMacroImpl {

      def extractFieldsImpl[A: Type](expr: Expr[A])(using q: Quotes): Expr[String] =
        new CaseClassDeconstructMacroImpl(q).extractFields[A](expr)
    }
    ```

    ```scala
    // file: src/test/scala/example/ExampleSpec.scala - part of CaseClass deconstruct example
    //> using test.dep org.scalameta::munit::{{ libraries.munit }}

    final class ExampleSpec extends munit.FunSuite {

      test("Example.extractFields should extract case class fields") {
        case class Person(name: String, age: Int)
        
        val person = Person("Alice", 30)
        val result = Example.extractFields(person)
        assert(result.contains("age:"))
        assert(result.contains("name:"))
      }
    }
    ```

### `Enum[A]`

Specialized view for sealed traits, Scala 3 enums, or Java enums:

| Method                      | Result type                                | Description                |
|-----------------------------|--------------------------------------------|----------------------------|
| `enumm.directChildren`      | `ListMap[String, ??<:[Color]]`             | immediate subtypes         |
| `enumm.exhaustiveChildren`  | `Option[NonEmptyMap[String, ??<:[Color]]]` | all subtypes               |

!!! example "Pattern matching on sealed trait/enum values"

    ```scala
    // file: src/main/scala/example/EnumMatchMacro.scala - part of Enum match example
    //> using scala {{ scala.2_13 }} {{ scala.3 }}
    //> using dep com.kubuszok::hearth:{{ hearth_version() }}

    import hearth.fp.effect.MIO
    import hearth.fp.instances.*
    import hearth.fp.syntax.*

    trait EnumMatchMacro { this: hearth.MacroCommons =>

      private val StringType = Type.of[String]

      def matchEnum[A: Type](expr: Expr[A]): Expr[String] =
        Enum.parse[A].fold(Expr("<no enum>")) { enumm =>
          implicit val StringType: Type[String] = this.StringType
          val handle: Expr_??<:[A] => MIO[Expr[String]] = matched => {
            import matched.{Underlying as Subtype, value as matchedExpr}
            MIO.pure(Expr(s"subtype name: ${Subtype.plainPrint}, expr: ${matchedExpr.plainPrint}"))
          }
          val sequential = enumm
            .matchOn(expr)(handle)
            .map(_.getOrElse(Expr("<failed to perform exhaustive match>")))
          val parallel = enumm
            .parMatchOn(expr)(handle)
            .map(_.getOrElse(Expr("<failed to perform exhaustive match>")))
          sequential
            .parMap2(parallel) { (sequential, parallel) =>
              Expr.quote {
                s"sequential: ${Expr.splice(sequential)}, parallel: ${Expr.splice(parallel)}"
              }
            }
            .unsafe
            .runSync
            ._2
            .fold(errors => Expr(s"<failed to construct: ${errors.mkString(", ")}>"), identity)
        }
    }
    ```

    ```scala
    // file: src/main/scala-2/example/EnumMatchMacroImpl.scala - part of Enum match example
    //> using target.scala {{ scala.2_13 }}
    //> using options -Xsource:3

    import scala.language.experimental.macros
    import scala.reflect.macros.blackbox

    object Example {
      def matchEnum[A](expr: A): String = macro EnumMatchMacroImpl.matchEnumImpl[A]
    }

    // Scala 2 adapter
    class EnumMatchMacroImpl(val c: blackbox.Context) extends hearth.MacroCommonsScala2 with EnumMatchMacro {

      def matchEnumImpl[A: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[String] =
        matchEnum[A](expr)
    }
    ```

    ```scala
    // file: src/main/scala-3/example/EnumMatchMacroImpl.scala - part of Enum match example
    //> using target.scala {{ scala.3 }}
    //> using plugin com.kubuszok::hearth-cross-quotes::{{ hearth_version() }}

    import scala.quoted.*

    object Example {
      inline def matchEnum[A](inline expr: A): String = ${ EnumMatchMacroImpl.matchEnumImpl[A]('{ expr }) }
    }

    // Scala 3 adapter
    class EnumMatchMacroImpl(q: Quotes) extends hearth.MacroCommonsScala3(using q), EnumMatchMacro
    object EnumMatchMacroImpl {

      def matchEnumImpl[A: Type](expr: Expr[A])(using q: Quotes): Expr[String] =
        new EnumMatchMacroImpl(q).matchEnum[A](expr)
    }
    ```

    ```scala
    // file: src/test/scala/example/ExampleSpec.scala - part of Enum match example
    //> using test.dep org.scalameta::munit::{{ libraries.munit }}

    sealed trait Color
    object Color {
      case class Red(value: Int) extends Color
      case object Blue extends Color
    }

    final class ExampleSpec extends munit.FunSuite {

      test("Example.matchEnum should match on sealed trait values") {
        
        val red: Color = Color.Red(42)
        val result = Example.matchEnum(red)
        assert(result.contains("subtype name:"))
        assert(result.contains("Red"))
      }
    }
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

!!! example "Constructing JavaBean without setters"

    ```scala
    // file: src/main/scala/example/JavaBeanConstructMacro.scala - part of JavaBean construct example
    //> using scala {{ scala.2_13 }} {{ scala.3 }}
    //> using dep com.kubuszok::hearth:{{ hearth_version() }}

    trait JavaBeanConstructMacro { this: hearth.MacroCommons =>

      def constructJavaBean[A: Type]: Expr[String] = Expr {
        JavaBean.parse[A].fold("<no java bean>") { javaBean =>
          javaBean.constructWithoutSetters
            .fold("<failed to construct>")(_.plainPrint)
        }
      }
    }
    ```

    ```scala
    // file: src/main/scala-2/example/JavaBeanConstructMacroImpl.scala - part of JavaBean construct example
    //> using target.scala {{ scala.2_13 }}
    //> using options -Xsource:3

    import scala.language.experimental.macros
    import scala.reflect.macros.blackbox

    object Example {
      def constructJavaBean[A]: String = macro JavaBeanConstructMacroImpl.constructJavaBeanImpl[A]
    }

    // Scala 2 adapter
    class JavaBeanConstructMacroImpl(val c: blackbox.Context) extends hearth.MacroCommonsScala2 with JavaBeanConstructMacro {

      def constructJavaBeanImpl[A: c.WeakTypeTag]: c.Expr[String] =
        constructJavaBean[A]
    }
    ```

    ```scala
    // file: src/main/scala-3/example/JavaBeanConstructMacroImpl.scala - part of JavaBean construct example
    //> using target.scala {{ scala.3 }}
    //> using plugin com.kubuszok::hearth-cross-quotes::{{ hearth_version() }}

    import scala.quoted.*

    object Example {
      inline def constructJavaBean[A]: String = ${ JavaBeanConstructMacroImpl.constructJavaBeanImpl[A] }
    }

    // Scala 3 adapter
    class JavaBeanConstructMacroImpl(q: Quotes) extends hearth.MacroCommonsScala3(using q), JavaBeanConstructMacro
    object JavaBeanConstructMacroImpl {

      def constructJavaBeanImpl[A: Type](using q: Quotes): Expr[String] =
        new JavaBeanConstructMacroImpl(q).constructJavaBean[A]
    }
    ```

    ```java
    // file: src/test/java/example/PersonBean.java - part of JavaBean construct example
    //> using target.scope test

    class PersonBean {
      public PersonBean() { }
    }
    ```

    ```scala
    // file: src/test/scala/example/ExampleSpec.scala - part of JavaBean construct example
    //> using test.dep org.scalameta::munit::{{ libraries.munit }}

    final class ExampleSpec extends munit.FunSuite {

      test("Example.constructJavaBean should construct JavaBean without setters") {
        val result = Example.constructJavaBean[PersonBean]
        assert(result.contains("new"))
      }
    }
    ```

!!! example "Constructing JavaBean with setters"

    ```scala
    // file: src/main/scala/example/JavaBeanSettersMacro.scala - part of JavaBean setters example
    //> using scala {{ scala.2_13 }} {{ scala.3 }}
    //> using dep com.kubuszok::hearth:{{ hearth_version() }}

    import hearth.fp.effect.MIO
    import hearth.fp.instances.*
    import hearth.fp.syntax.*

    trait JavaBeanSettersMacro { this: hearth.MacroCommons =>

      private val booleanType: Type[Boolean] = Type.of[Boolean]
      private val intType: Type[Int] = Type.of[Int]
      private val stringType: Type[String] = Type.of[String]

      def constructWithSetters[A: Type]: Expr[String] = Expr {
        JavaBean.parse[A].fold("<no java bean>") { javaBean =>
          val setField: (String, Parameter) => MIO[Expr_??] = (name, input) => {
            import input.tpe.Underlying as FieldType
            implicit val BooleanType: Type[Boolean] = booleanType
            implicit val IntType: Type[Int] = intType
            implicit val StringType: Type[String] = stringType
            if (FieldType <:< Type[Boolean]) MIO.pure(Expr(true).as_??)
            else if (FieldType <:< Type[Int]) MIO.pure(Expr(0).as_??)
            else if (FieldType <:< Type[String]) MIO.pure(Expr(name).as_??)
            else MIO.fail(new Exception(s"Field $name has wrong type: ${input.tpe.plainPrint}"))
          }
          val sequential = javaBean.constructWithSetters(setField).map { result =>
            result.fold("<failed to construct with setters>")(_.plainPrint)
          }
          val parallel = javaBean.parConstructWithSetters(setField).map { result =>
            result.fold("<failed to construct with setters>")(_.plainPrint)
          }
          sequential
            .parMap2(parallel) { (sequential, parallel) =>
              s"sequential:\n$sequential\nparallel:\n$parallel"
            }
            .unsafe
            .runSync
            ._2
            .fold(errors => s"<failed to construct: ${errors.mkString(", ")}>", identity)
        }
      }
    }
    ```

    ```scala
    // file: src/main/scala-2/example/JavaBeanSettersMacroImpl.scala - part of JavaBean setters example
    //> using target.scala {{ scala.2_13 }}
    //> using options -Xsource:3

    import scala.language.experimental.macros
    import scala.reflect.macros.blackbox

    object Example {
      def constructWithSetters[A]: String = macro JavaBeanSettersMacroImpl.constructWithSettersImpl[A]
    }

    // Scala 2 adapter
    class JavaBeanSettersMacroImpl(val c: blackbox.Context) extends hearth.MacroCommonsScala2 with JavaBeanSettersMacro {

      def constructWithSettersImpl[A: c.WeakTypeTag]: c.Expr[String] =
        constructWithSetters[A]
    }
    ```

    ```scala
    // file: src/main/scala-3/example/JavaBeanSettersMacroImpl.scala - part of JavaBean setters example
    //> using target.scala {{ scala.3 }}
    //> using plugin com.kubuszok::hearth-cross-quotes::{{ hearth_version() }}

    import scala.quoted.*

    object Example {
      inline def constructWithSetters[A]: String = ${ JavaBeanSettersMacroImpl.constructWithSettersImpl[A] }
    }

    // Scala 3 adapter
    class JavaBeanSettersMacroImpl(q: Quotes) extends hearth.MacroCommonsScala3(using q), JavaBeanSettersMacro
    object JavaBeanSettersMacroImpl {

      def constructWithSettersImpl[A: Type](using q: Quotes): Expr[String] =
        new JavaBeanSettersMacroImpl(q).constructWithSetters[A]
    }
    ```

    ```java
    // file: src/test/java/example/PersonBean.java - part of JavaBean setters example
    //> using target.scope test

    class PersonBean {
      public PersonBean() { }

      private String name;
      public void setName(String name) { this.name = name; }

      private int age;
      public void setAge(int age) { this.age = age; }
    }
    ```

    ```scala
    // file: src/test/scala/example/ExampleSpec.scala - part of JavaBean setters example
    //> using test.dep org.scalameta::munit::{{ libraries.munit }}

    final class ExampleSpec extends munit.FunSuite {

      test("Example.constructWithSetters should construct JavaBean with setters") {
        val result = Example.constructWithSetters[PersonBean]
        assert(result.contains("setName"))
        assert(result.contains("setAge"))
      }
    }
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
