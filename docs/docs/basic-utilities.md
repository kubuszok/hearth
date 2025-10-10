# Basic Utilities

Hearth API abstracts away from the particular macro system. It works with abstract types
for which abstract methods are provided, and later on we implement them for a particular macro system.

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

    Majority of the examples in this documentation is intended to be runnable.

    That's why most file names would follow convention from [sbt](https://www.scala-sbt.org/) + [projectmatrix](https://github.com/sbt/sbt-projectmatrix) conventions
    (or sbt 2.0).

    However, all runnable examples are tested using [Scala CLI](https://scala-cli.virtuslab.org/) and are containing directives needed to make it happy.

## Your first Hearth-based macro

Once you added Hearth to your project you can start writing Hearth-powered macros.

You can decide to either:

 - write cross-compilable macros (we suggest also using [cross-quotes](cross-quotes.md) in such case)
 - writing Scala-2-only or Scala-3-only macros

This is how you can write a cross-compilable macro.

!!! example "Cross-compilable API"

    If you want to write cross-compilable macros, you'll see types and methods e.g.:

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

      // These and other types becomes available to us in traits that extends from MacroCommons

      def yourMacro[A: Type](expr: Expr[A]): Expr[String] = Expr {
        s"${expr.plainPrint} : ${Type[A].plainPrint}"
      }
    }
    ```

    Then we use `MacroCommons` with adapters that we would write for Scala 2 and Scala 3.

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

      // Unfortunatelly, while compiler is clever enough to see that the types are the same when we apply them...
      def yourMacroImpl[A: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[String] = yourMacro(expr)
    }

    object Example {
      // ... it is not smart enough to see it, if we wanted to call YourMacro.yourMacro directly.
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
      // Since we define things in a mixin, we need to write an adapter that instantiate it
      // in a method.
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

    If you want to write Scala 2-only macros, we can simplify the code - use `MacroCommonsScala2` directly,
    use `c.WeakTypeTag` and `c.Expr` directly, etc, but also using extension methods and utilities from Hearth:

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

      test("Example.callingMacro runs our cross-compilable macro") {
        assertEquals(Example.callingMacro("value"), "\"value\" : java.lang.String")
      }
    }
    ```

!!! example "Scala 3 API"

    If you want to write Scala 3-only macros, we can simplify the code - use `MacroCommonsScala3` directly,
    use `quoted.Type` and `quoted.Expr` directly, etc, but also using extension methods and utilities from Hearth:

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
      // Since we define things in a mixin, we need to write an adapter that instantiate it
      // in a method.
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

      test("Example.callingMacro runs our cross-compilable macro") {
        assertEquals(Example.callingMacro("value"), "\"value\" : java.lang.String")
      }
    }
    ```

## Library's conventions

Inside a shared-logic trait (`MacroCommons`) we are working on abstract types and abstract methods:

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
  
Their implementations are being mix-in later (`MacroCommonsScala2`, `MacroCommonsScala3`).
   
Hearth implements shared utilities from bottom up: `Type[A]` and `Expr[A]` build on top of `UntypedType` and `UntypedExpr`,
then `Method[A]`s build on top of `Type[A]` and `Expr[A]`, then `CaseClass[A]`, `Enum[A]` and `JavaBean[A]` build on top of them.
(The lower is the abstraction level, the less likely API is to change).

Probably for most common use cases you want to use high-level things like:

!!! example "High-level `Class[A]` API"

    ```scala
    Class[A] match {
      case caseClass: CaseClass[A] => // use case class utilities
      case enumType: Enum[A] =>       // use enum utilities
      case javaBean: JavaBean[A] =>   // use Java Bean utilities
      case classType: Class[A] =>     // just a class
    }
    ```

but they are work in progress, and when high level API is not sufficient, you can fall back on lower-lever.

 If there is no API for what you try to do, you can always define:

 ```scala
 def mySharedAPI(): Result
 ```

 in the common part and implement the platform-specific logic in the traits where you are mixing-in.

(I suggest doing as little of it as possible, and trying to separate the intents - what you try to achieve - from the means
how you are doing it. It makes code easier to reason and maintain).

## `Type`, `UntypedType` and `??`

You can see 3 different representations of a type in Hearth codebase:

 * `Type[A]` - corresponds to `c.WeakTypeTag[A]` or `scala.quoted.Type[A]`. Because the type for which information
   is stored is reflected in the, well, type (e.g. `Type[String]` vs `Type[Int]`) we can use them in type bounds, implicits
   etc
 * `UntypedType` - corresponds to `c.Type` or `q.reflect.TypeRepr`. Because type for which information is stored
   is NOT reflected in the type, it's useful for working with the types of unknown kindness (e.g. `A` vs `F[_]` vs `G[_[_]]`, etc)
 * `??` - existential type. We know that it's a `Type[A]` for some `A` but we don't know which. It differs from `UntypedType`, 
   because we can do this:
   ```scala
   val unknownType: ??
   import unknownType.Underlying as Param
   Type[Param] // now, references to Param would resolve to unknownType.Underlying
   ```
   which is useful, when we are e.g. receive a list of subtypes of some enum `E`: we get a `List[??<:[E]]` (list of existential types
   with `E` as upper bound).

### Type Operations

`Type[A]` provides many operations for introspecting and manipulating types:

**Printing and display:**

```scala
Type[Int].shortName      // "Int"
Type[Int].fcqn           // "scala.Int" (fully-qualified class name)
Type[Int].plainPrint     // "scala.Int" (without ANSI colors)
Type[Int].prettyPrint    // colored output for terminals
```

**Type predicates:**

```scala
// Primitives and built-ins
Type[Int].isPrimitive           // true for Boolean, Byte, Short, Int, Long, Float, Double, Char
Type[Array[Int]].isArray        // true
Type[String].isJvmBuiltIn       // true for primitives, Unit, String, arrays
Type[Any].isTypeSystemSpecial   // true for Any, AnyRef, AnyVal, Null, Nothing

// Class types
Type[MyClass].isClass                  // true for classes
Type[MyClass].notJvmBuiltInClass       // isClass && !isJvmBuiltIn
Type[MyClass].isPlainOldJavaObject     // non-abstract, non-sealed, non-enum class
Type[MyClass].isJavaBean               // POJO with default constructor

// Modifiers
Type[MyClass].isAbstract    // abstract class or trait
Type[MyClass].isFinal       // final class
Type[MyClass].isSealed      // sealed trait/class
Type[MyClass].isObject      // object/module

// Case classes and enums
Type[MyClass].isCase           // case class, case object, or case val
Type[MyClass].isCaseClass      // case class specifically
Type[MyClass].isCaseObject     // case object specifically
Type[MyClass].isJavaEnum       // Java enum
Type[MyClass].isJavaEnumValue  // Java enum value
```

**Type relationships:**

```scala
Type[String] <:< Type[AnyRef]  // true (String is subtype of AnyRef)
Type[Int] =:= Type[Int]        // true (types are equal)
```

**Metadata and navigation:**

```scala
Type[MyClass].position            // Option[Position] - where type is defined
Type[MyClass].companionObject     // Option[Expr_??] - companion object if exists
Type[MyClass].annotations         // List[Expr_??] - type annotations
Type[MyClass].directChildren      // Option[ListMap[String, ??<:[MyClass]]] - immediate subtypes
Type[MyClass].exhaustiveChildren  // Option[NonEmptyMap[String, ??<:[MyClass]]] - all subtypes
```

**Constructors and methods:**

```scala
Type[MyClass].primaryConstructor   // Option[Method.NoInstance[MyClass]]
Type[MyClass].defaultConstructor   // Option[Method.NoInstance[MyClass]] - nullary constructor
Type[MyClass].constructors         // List[Method.NoInstance[MyClass]]
Type[MyClass].methods              // List[Method.Of[MyClass]]
```

**Visibility check:**

```scala
Type[MyClass].isAvailable(Everywhere)  // public
Type[MyClass].isAvailable(AtCallSite)  // accessible from call site (including package-private)
```

### TypeCodec

`TypeCodec[A]` provides bidirectional conversion between singleton/literal type values and their type representation:

```scala
// Creating type from literal value
val intType: Type[5] = Type(5)           // creates Type[5] (literal type)
val stringType: Type["hello"] = Type("hello")

// Extracting value from literal type (if available)
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

Existential types allow working with types whose exact identity is unknown at macro-writing time but will be known at macro-execution time:

```scala
// Basic existential
val tpe: ?? = someType.as_??
import tpe.Underlying as T
// Now T can be used as a type parameter

// Bounded existentials
val subtypeOfFoo: ??<:[Foo] = ...  // upper bound
val supertypeOfBar: ??>:[Bar] = ... // lower bound  
val bounded: Foo <:??<: Bar = ...   // both bounds
```

## `Expr`, `UntypedExpr` and `Expr_??`

You can see 3 different representations of expressions in Hearth codebase:

 * `Expr[A]` - corresponds to `c.Expr[A]` or `scala.quoted.Expr[A]`. Because the type for which information
   is stored is reflected in the, well, type (e.g. `Expr[String]` vs `Expr[Int]`) we can use them in quotes and splices
 * `UntypedExpr` - corresponds to `c.universe.Tree` or `q.reflect.Term`. Because type for which information is stored
   is NOT reflected in the type, it's useful for working with AST directly (almost always in the platform-specific code)
 * `Expr_??` - existential expr. We know that it's a `Expr[A]` for some `A` but we don't know which. It differs from `UntypedExpr`, 
   because we can do this:
   ```scala
   val unknownExpr: Expr_??
   import unknownExpr.{Underlying as Param, value as expr}
   Type[Param] // now, references to Param would resolve to unknownExpr.Underlying
   expr: Expr[Param] // and this is an Expr of some type that we can use
   ```
   which is useful, when we are e.g. trying to call some method or constructor.

### Expression Operations

`Expr[A]` provides operations for working with expressions:

**Printing and display:**

```scala
val expr: Expr[Int] = Expr(42)
expr.plainPrint   // "42" (without ANSI colors)
expr.prettyPrint  // colored code representation
expr.plainAST     // AST representation without colors
expr.prettyAST    // colored AST representation
```

**Summoning implicits:**

```scala
Expr.summonImplicit[Ordering[String]]  // Option[Expr[Ordering[String]]]
Type[String].summonExpr                 // Option[Expr[String]] - if implicit String exists
```

**Type operations:**

```scala
val stringExpr: Expr[String] = ???
val anyRefExpr: Expr[AnyRef] = stringExpr.upcast[AnyRef]  // safe upcast
expr.suppressUnused  // Expr[Unit] - prevents "unused value" warnings
```

### ExprCodec

`ExprCodec[A]` generalizes over Scala 2's `Lifting`/`Unliftable` and Scala 3's `ToExpr`/`FromExpr`, allowing bidirectional conversion between values and expressions:

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

### VarArgs

Cross-platform handling of variadic arguments (Scala 2 uses `Seq[Expr[A]]`, Scala 3 uses `Expr[Seq[A]]`):

```scala
def myMacro[A: Type](args: VarArgs[A]): Expr[Unit] = {
  val exprs: List[Expr[A]] = args.toList
  // work with individual expressions
  ???
}
```

### MatchCase

Build pattern-matching expressions programmatically:

```scala
// Generate: expr match { case x: A => handleA(x); case y: B => handleB(y) }
expr.matchOn(
  MatchCase.typeMatch[A]("x").map { x: Expr[A] => handleA(x) },
  MatchCase.typeMatch[B]("y").map { y: Expr[B] => handleB(y) }
)
```

### FreshName

Strategies for generating fresh variable names to avoid clashes:

```scala
FreshName.FromType            // derive from type name
FreshName.FromExpr            // derive from expression
FreshName.FromPrefix("temp")  // use specific prefix
"myPrefix"                    // String implicitly converts to FromPrefix
```

### Scoped

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

### LambdaBuilder

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

// With error handling (using MIO or other effect)
LambdaBuilder.of1[A]("a").traverse[MIO, B] { a: Expr[A] =>
  validateAndUseA(a): MIO[Expr[B]]
}.map(_.build) // : MIO[Expr[A => B]]
```

Supports up to 22 parameters (`of1` through `of22`).

## `Method`

`Method` represents a method that can be called. Methods come in different flavors depending on how they're invoked:

 * `Method.NoInstance[Out]` - constructors, static methods, or stable object methods: `Companion.method(args)` 
 * `Method.OfInstance[A, Out]` - instance methods: `instance.method(args)`
 * `Method.Unsupported[A, Out]` - methods we cannot handle (e.g., polymorphic methods with explicit type parameters)

!!! warning "Type Parameter Limitation"
    Applying type parameters is not yet supported. Calls like `instance.method[A, B](values)` where type parameters 
    are applied explicitly or need to be inferred are represented as `Method.Unsupported`.

### Pattern Matching on Methods

The recommended way to handle methods is through pattern matching:

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

**Parameter** - represents a method parameter definition:

```scala
val param: Parameter = ???
param.name           // String - parameter name
param.index          // Int - parameter position
param.position       // Option[Position] - source location
param.tpe            // ?? - the parameter type
param.hasDefault     // Boolean - has default value?
param.defaultValue   // Option[Existential[Method.Of]] - default value getter
param.annotations    // List[Expr_??] - parameter annotations
param.isByName       // Boolean - by-name parameter?
param.isImplicit     // Boolean - implicit parameter?
```

**Parameters** - all parameters of a method, grouped by parameter list:

```scala
type Parameters = List[ListMap[String, Parameter]]

// Example: def foo(a: Int, b: String)(implicit c: Boolean)
// Parameters = List(
//   ListMap("a" -> paramA, "b" -> paramB),
//   ListMap("c" -> paramC)
// )
```

**Arguments** - values to pass to a method:

```scala
type Arguments = Map[String, Expr_??]

// Build arguments
val args: Arguments = Map(
  "a" -> Expr(42).as_??,
  "b" -> Expr("hello").as_??
)
```

### Method Properties

**Metadata:**

```scala
method.name           // String - method name
method.position       // Option[Position] - source location
method.annotations    // List[Expr_??] - method annotations
method.parameters     // Parameters - parameter groups
```

**Kind checks:**

```scala
method.isConstructor           // true for constructors
method.isVal                   // val member
method.isVar                   // var member
method.isLazy                  // lazy val
method.isDef                   // def method
method.isImplicit              // implicit def/val
```

**Source info:**

```scala
method.isDeclared              // declared in this class
method.isSynthetic             // compiler-generated
method.isInherited             // inherited from parent
method.isCaseField             // case class field
method.isConstructorArgument   // constructor parameter
```

**Arity:**

```scala
method.arity           // Int - total parameter count
method.isNullary       // no parameters
method.isUnary         // one parameter
method.isBinary        // two parameters
method.isNAry(n)       // exactly n parameters
```

**Accessors (getters/setters):**

```scala
// Scala-style accessors
method.isScalaGetter         // val, var, lazy val (without _=)
method.isScalaSetter         // var setter (name_=)
method.scalaAccessorName     // Option[String] - field name

// Java Bean accessors
method.isJavaGetter          // getX() -> X, isX() -> Boolean
method.isJavaSetter          // setX(v) -> Unit
method.javaAccessorName      // Option[String] - property name

// Combined
method.isAccessor            // any accessor
method.accessorName          // Option[String] - property name
```

**Visibility:**

```scala
method.isAvailable(Everywhere)  // public
method.isAvailable(AtCallSite)  // accessible from call site
```

### Calling Methods

```scala
// Constructor or static method
val ctor: Method.NoInstance[MyClass] = Type[MyClass].primaryConstructor.get
ctor(Map("param1" -> arg1, "param2" -> arg2)) // Either[String, Expr[MyClass]]

// Instance method
val method: Method.OfInstance[MyClass, String] = ???
import method.{ Instance, Returned }
method(
  instance = myInstance,  // Expr[Instance]
  arguments = Map("x" -> argX)
) // Either[String, Expr[Returned]]
```

The methods validate that:
- All required parameters are provided
- Argument types match parameter types
- Returns `Right(expr)` on success or `Left(error)` on failure

## `Class`

`Class[A]` is a convenient utility that aggregates information about a type:

```scala
val cls = Class[MyClass]
cls.constructors  // List[Method.NoInstance[MyClass]]
cls.methods       // List[Method.Of[MyClass]]
cls.method("foo") // List[Method.Of[MyClass]] - all overloads named "foo"
```

`Class` also provides specialized views when applicable:

```scala
cls.asCaseClass   // Option[CaseClass[MyClass]]
cls.asEnum        // Option[Enum[MyClass]]
cls.asJavaBean    // Option[JavaBean[MyClass]]
```

These utilities are built on top of `Type` and `Method` - you could implement similar functionality yourself, but these provide convenient shortcuts.

### `CaseClass[A]`

Specialized view for case classes, providing access to primary constructor and case fields:

```scala
val cc = CaseClass.parse[Person].get  // or Type[Person].asCaseClass.get

cc.primaryConstructor        // Method.NoInstance[Person] 
cc.nonPrimaryConstructors    // List[Method.NoInstance[Person]]
cc.caseFields                // List[Method.Of[Person]] - the fields
```

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

```scala
val enumm = Enum.parse[Color].get  // or Type[Color].asEnum.get

enumm.directChildren       // ListMap[String, ??<:[Color]] - immediate subtypes
enumm.exhaustiveChildren   // Option[NonEmptyMap[String, ??<:[Color]]] - all subtypes
```

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

```scala
val bean = JavaBean.parse[PersonBean].get  // or Type[PersonBean].asJavaBean.get

bean.defaultConstructor   // Method.NoInstance[PersonBean] - nullary constructor
bean.beanGetters          // List[Existential[Method.OfInstance[PersonBean, *]]]
bean.beanSetters          // List[Method.OfInstance[PersonBean, Unit]]
```

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

```scala
val pos: Position = Type[MyClass].position.get

pos.file        // Option[java.nio.file.Path] - source file
pos.fileName    // Option[String] - file name only
pos.line        // Int - line number
pos.column      // Int - column number
pos.offset      // Int - character offset from file start

pos.prettyPrint       // "MyFile.scala:42:10"
pos.prettyPrintLong   // "/full/path/to/MyFile.scala:42:10"
```

Positions can be compared and sorted:

```scala
import PositionOrdering.given  // or implicit val _ = PositionOrdering
positions.sorted  // sorts by file path, then offset
```

## `Environment`

`Environment` provides utilities for introspecting the compilation environment and reporting messages:

### Current Compilation Context

```scala
Environment.currentPosition     // Position - where macro is expanded

Environment.currentScalaVersion // ScalaVersion - e.g. 2.13.10, 3.3.0
Environment.currentLanguageVersion  // LanguageVersion - Scala2_13 or Scala3
Environment.isScala2_13         // Boolean
Environment.isScala3            // Boolean

Environment.currentJDKVersion   // JDKVersion - runtime JDK version
Environment.currentPlatform     // Platform - JVM, JS, or Native
Environment.isJvm               // Boolean
Environment.isJs                // Boolean  
Environment.isNative            // Boolean
```

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

```scala
Environment.reportInfo("Generating code for MyClass")
Environment.reportWarn("Type parameter might be incorrect")
Environment.reportError("Invalid configuration")
Environment.reportErrorAndAbort("Fatal error, cannot continue")  // throws exception
```

!!! note "Reporting Behavior"
    For each level (INFO, WARN, ERROR), only the **first** call matters. Subsequent calls at the same level are no-ops.
    This behavior is consistent between Scala 2 and Scala 3 macros.

### Debug Helpers

**Selective macro expansion:**

Useful for debugging a specific macro invocation without seeing output from all expansions:

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

Load service provider extensions to extend macro functionality:

```scala
// Define an extension
trait MyMacroExtension extends MacroExtension[MacroCommons] {
  def isDefinedAt(env: MacroCommons): Boolean = true
  def apply(env: MacroCommons): Unit = {
    // Configure or extend the macro environment
  }
}

// In your macro
Environment.loadMacroExtensions[MyMacroExtension] match {
  case Right(()) => // all extensions loaded successfully
  case Left(errors) => // handle errors
}
```

Extensions are discovered via Java's ServiceLoader mechanism, allowing functionality to be added just by having the right JAR on the classpath.

### MIO Termination Handling

When using Hearth's `MIO` effect system, handle Ctrl+C interruption:

```scala
Environment.handleMioTerminationException {
  // Code that might be interrupted
  someMioComputation.runToExprOrFail
}
```

Behavior can be configured via `-Xmacro-settings:hearth.mioTerminationShouldUseReportError=true|false`:
- `true` (default): Uses `reportErrorAndAbort` to show error and terminate current macro
- `false`: Prints to stderr and throws exception to terminate compilation

This is automatically handled when using `mio.runToExprOrFail(...)(...)` extension.

## Advanced Patterns

### Working with Existential Types

Existential types (`??`) are essential when dealing with types whose identity isn't known at macro-writing time:

```scala
// Getting an existential from a Type[A]
val tpe: Type[String] = Type.of[String]
val existential: ?? = tpe.as_??

// Using the existential - import the type to use it
import existential.Underlying as T
// Now T is available as a type parameter
val expr: Expr[T] = Expr.quote { ??? }: Expr[T]
val method: Method.Of[T] = ???
```

**Bounded existentials** constrain the possible types:

```scala
// Type must be subtype of Animal
val animal: ??<:[Animal] = dogType.as_??<:[Animal]

// Type must be supertype of Dog  
val supertype: ??>:[Dog] = animalType.as_??>:[Dog]

// Type must be between bounds
val bounded: Dog <:??<: Animal = dogType.as_<:??<:[Dog, Animal]
```

**Working with lists of existentials:**

```scala
// Enum children are existentials
val children: ListMap[String, ??<:[Color]] = enum.directChildren

children.foreach { case (name, childType) =>
  import childType.Underlying as Child
  // Child is now available as a type
  handleColorType[Child]: Unit
}
```

### Parallel vs Sequential Operations

Many operations come in two flavors:

**Sequential** (`construct`, `matchOn`):
- Processes items one after another
- Later computations can depend on earlier results
- Uses `Applicative` constraint

**Parallel** (`parConstruct`, `parMatchOn`):
- Processes items independently when possible
- Better for independent, expensive computations
- Uses `Parallel` constraint (which implies `Applicative`)

```scala
// Sequential: fields computed in order
caseClass.construct[MIO] { field =>
  // Can use results from previous fields
  computeField(field): MIO[Expr_??]
}

// Parallel: fields computed independently
caseClass.parConstruct[MIO] { field =>
  // Should not depend on other fields
  computeFieldIndependently(field): MIO[Expr_??]
}
```

!!! tip "When to Use Parallel"

    Use parallel variants when:

    - Computations are independent
    - You want to aggregate errors from all fields/cases (not fail on first error)

### DirectStyle Pattern

Many Hearth operations use the `DirectStyle[F]` pattern to enable working with effects:

```scala
def construct[F[_]: DirectStyle: Applicative](
    makeField: Parameter => F[Expr[_]]
): F[Option[Expr[A]]]
```

This allows:
- Using effect types like `MIO`, `Either`, `Option`, `List`
- Safely running computations within a scope
- Type-safe handling of dependent types

```scala
DirectStyle[MIO].scoped { runSafe =>
  import someExistential.Underlying as T
  val result: Expr[T] = runSafe(computation): Expr[T]
  // Use result
  result.as_??
}
```

The `runSafe` function ensures that:
- Effect is executed safely within the current scope
- Type information is preserved through dependent types
- Existential types are handled correctly

### Error Aggregation

When building complex code generation, aggregate errors instead of failing fast:

```scala
// Using MIO to aggregate errors
caseClass.parConstruct[MIO] { field =>
  MIO {
    if (isValid(field)) Right(createExpr(field))
    else Left(NonEmptyVector.of(s"Invalid field: ${field.name}"))
  }
}.runToExprOrFail  // Aggregates all errors before failing
```

### Type Safety with Method Calls

When calling methods with existential return types, use pattern matching:

```scala
val methods: List[Method.Of[MyClass]] = Type[MyClass].methods

methods.foreach { methodExists =>
  methodExists.value match {
    case m: Method.OfInstance[?, ?] =>
      import m.{ Instance, Returned }
      // Now Instance and Returned are available as types
      val result: Either[String, Expr[Returned]] = m(instance, args)
      result.map { expr: Expr[Returned] =>
        // Use expr with full type information
        expr.as_??
      }
    case _ => ???
  }
}
```

### Accessor Patterns

Handle both Scala and Java accessor patterns uniformly:

```scala
class.methods.filter(_.value.isAccessor).groupBy(_.value.accessorName).foreach {
  case (Some(propName), methods) =>
    val getters = methods.filter(_.value.isJavaGetter || _.value.isScalaGetter)
    val setters = methods.filter(_.value.isJavaSetter || _.value.isScalaSetter)
    // Handle property with getters and/or setters
}
```

### Debugging Tips

**1. Print types and expressions:**

```scala
println(s"Type: ${Type[MyClass].prettyPrint}")
println(s"Expr: ${expr.prettyPrint}")
println(s"AST: ${expr.prettyAST}")
```

**2. Use selective expansion for debugging:**

```scala
if (Environment.isExpandedAt("MyFile.scala:42:10")) {
  // Only debug this specific expansion
  println(s"Parameters: ${method.parameters}")
}
```

**3. Validate argument types:**

```scala
method match {
  case m: Method.OfInstance[?, ?] =>
    m(instance, args) match {
      case Right(result) => // success
      case Left(error) =>
        // Error message includes type mismatches
        Environment.reportError(error)
    }
}
```
