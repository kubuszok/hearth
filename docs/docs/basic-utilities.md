# Basic Utilities

Hearth API abstracts away from the particular macro system. It works with abstract types
for which abstract methods are provided, and later on we implement them for a particular macro system.

!!! example "Cross-compilable API"

    If you want to write cross-compilable macros, you'll see types and methods e.g.:

    ```scala
    // Shared part

    trait SharedMacroLogic { this: hearth.MacroCommons =>

      // Type[A] - represents information about a type A, where we know what A is:
      //           it's either some known type like `java.lang.String`, or a type parameter,
      //           or that type we know exists, that we named as `A` (about that later)

      // Expr[A] - represents an expression of type A

      // These and other types becomes available to us in traits that extends from MacroCommons

      def yourMacro[A: Type](expr: Expr[A]): Expr[String] = Expr {
        s"${expr.prettyPrint} : ${Type[A].prettyPrint}"
      }
    }
    ```

    Then we use `MacroCommons` with adapters that we would write for Scala 2 and Scala 3.

    ```scala
    // Scala 2 adapter

    import scala.language.experimental.macros
    import scala.reflect.macros.blackbox

    class YourMacro(val c: blackbox.Context) extends hearth.MacroCommonsScala2 with SharedMacroLogic {

      // Type[A] = c.WeakTypeTag[A]

      // Expr[A] = c.Expr[A]

      // Unfortunatelly, while compiler is clever enough to see that the types are the same when we apply them...
      def yourMacroImpl[A: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[String] = yourMacro(expr)
    }

    // ... it is not smart enough to see it, if we wanted to call YourMacro.yourMacro directly.
    def callingMacro[A](expr: A): String = macro YourMacro.yourMacroImpl
    ```

    ```scala
    // Scala 3 adapter

    import scala.quoted.*

    class YourMacro(q: Quotes) extends hearth.MacroCommonsScala3(using q), SharedMacroLogic

    object YourMacro {

      // Scala 3 requires all macros to be defined in objects or other stable locations.
      // Since we define things in a mixin, we need to write an adapter that instantiate it
      // in a method.
      def yourMacro[A: Type](expr: Expr[A])(using q: Quotes): Expr[String] =
        new YourMacro(using q).yourMacro(expr)
    }

    inline def callingMacro[A](inline expr: A): String = ${ YourMacro.yourMacro('expr) }
    ```

!!! example "Scala 2 API"

    If you want to write Scala 2-only macros, we can simplify the code - use `MacroCommonsScala2` directly,
    use `c.WeakTypeTag` and `c.Expr` directly, etc, but also using extension methods and utilities from Hearth:

    ```scala
    import hearth.MacroCommonsScala2

    import scala.language.experimental.macros
    import scala.reflect.macros.blackbox

    class YourMacro(val c: blackbox.Context) extends hearth.MacroCommonsScala2 {

      // Type[A] = c.WeakTypeTag[A]

      // Expr[A] = c.Expr[A]

      def yourMacro[A: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[String] = Expr {
        s"${expr.prettyPrint} : ${Type[A].prettyPrint}"
      }
    }

    def callingMacro[A](expr: A): String = macro YourMacro.yourMacro
    ```

!!! exmaple "Scala 3 API"

    If you want to write Scala 3-only macros, we can simplify the code - use `MacroCommonsScala3` directly,
    use `quoted.Type` and `quoted.Expr` directly, etc, but also using extension methods and utilities from Hearth:

    ```scala
    import hearth.MacroCommonsScala3

    import scala.quoted.*

    class YourMacro(q: Quotes) extends hearth.MacroCommonsScala3(using q) {

      // Type[A] = c.WeakTypeTag[A]

      // Expr[A] = c.Expr[A]

      def yourMacro[A: Type](expr: Expr[A]): Expr[String] = Expr {
        s"${expr.prettyPrint} : ${Type[A].prettyPrint}"
      }
    }
    object YourMacro {

      // Scala 3 requires all macros to be defined in objects or other stable locations.
      // Since we define things in a mixin, we need to write an adapter that instantiate it
      // in a method.
      def yourMacro[A: Type](expr: Expr[A])(using q: Quotes): Expr[String] =
        new YourMacro(using q).yourMacro(expr)
    }

    inline def callingMacro[A](inline expr: A): String = ${ YourMacro.yourMacro('expr) }
    ```

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
   which ise useful, when we are e.g. receive a list of subtypes of some enum `E`: we get a `List[??<:[E]]` (list of existential types
   with `E` as upper bound).

## `Expr`, `UntypedExpr` and `Expr_??`

You can see 3 different representations of a type in Hearth codebase:

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
   which ise useful, when we are e.g. trying to call some method or constructor.

## `Method`

`Method` represents a method that can be called. Currently we are supporting 2 kinds of methods:

 * `Method.NoInstance[Out]` - can be used to build calls like `Companon.method(args)` 
 * `Method.OfInstance[A, Out]` - can be used to build calls like `instance.method(args)`

Applying type parameters is not yet supported, so calls like `instance.method[A, B](values)`
where type parameters are applied explicitly or are inferred is not yet possible.

## `Class`

`Class[A]` is a handy utility which at once takes care of:

 * finding out all constructors
 * finding out all instance methods or companion object methods

of some class. It also exposes a views (`asCaseClass`, `asEnum`, `asJavaBean`) providing additional functionalities when available:

 * `CaseClass[A]` assumes that there is a primary constructor and a set of fields defined by it
 * `Enum[A]` exposes a list of subtypes and a way of pattern matching on them
 * `JavaBean[A]` assumes that there is a default constructor and a set of setters to call during construction

These utilities are there for convenience as you could implement them yourself using methods exposed by `Type` and `Method` interfaces.

## `Environment`

`Environment` provides some usful utilities like:

 * `currentPosition` - the position of the macro expansion
 * `currentScalaVersion` - 2.13 or 3?
 * `currentJDKVersion` - if we want to provide different features depending on JDK used
 * `currentPlatform` - JVM, JS or Native?
 * `XMacroSettings` - all settings provided as a `List` via `-Xmacro-setting:...` scalac option
 * `typedSettings` - same as above but parsed into a JSON-like structure
 * `reportInfo`/`reportWarn`/`reportError`/`reportErrorAndAbort` - provide a message to show as `[INFO]`, `[WARN]` or `[ERROR]`
   during compilation/by IDE/Scastie - for each level only the first call matters, all following are no-ops!
 * `isExpandedAt` - let you test your macros for a single use case (`if (isExpandedAt("someFile.scala:10:4"))`)
 * `loadMacroExtension` - allows defining API for macro extensions, so that functionaity of a macro could be expanded, just like
   implicit-based metaprogramming, but without providing new implicits - add extension on a classpath and its availabe to the macros!
