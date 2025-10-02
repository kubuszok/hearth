---
hide:
  - navigation
  - toc
---

<p style="text-align: center"><img src="assets/images/logo.svg" alt="Hearth logo" style="height: 250px" /></p>

<h1 style="margin-bottom:0">Hearth</h1>
<h2 style="margin-top:0">The first Scala macros' standard library.</h2>

The first library that focuses on helping you write robust and easy to maintain macros, that you can make cross-compilable between Scala 2 and Scala 3 macro systems.

Introduces among others:

 * reliable extension methods that make checking properties of types, expressions and methods easier
 * Magnolia-like utilities for creating and decomposing data types
 * improvements over built-in types and expression printing utilities
 * small FP library that let you reuse your Cats experience in macros (including Macro IO/MIO monad)
 * direct style utilities for working with cases that are hard to handle with monad and combinators (or even impossible to handle with them)
 * macro-extension system allowing to extend your macros just by adding a dependency to the class path - without any additional imports!
 * and finally, macro API that has implementation

!!! warning "Tutorials planned"

    Some good tutorial to macros and then Hearth is planned, but currently we offer a quick summary.

    For now you have to be able to learn about the code by looking around the code base, reading commmends
    and tests.

## How to use the library for cross-compilable macros?

Majority of the macros code would be shared, by putting it into a mix-in trait.

Then in Scala 2 and Scala 3 specific code you would write only adapters.

!!! example "`src/main/scala/example/Show.scala` - shared `Show` type class"

    ```scala
    package example

    /** toString as a type class - easy to understand what this type class want to do. */
    trait Show[A] {

      def show(value: A): String
    }

    /** Companion will contain the derivation adapted specific to Scala language version. */
    object Show extends ShowCompanionCompat
    ```

!!! example "`src/main/scala/example/ShowMacrosImpl.scala` - shared macro logic"

    ```scala
    package example

    import hearth.*

    private[example] trait ShowMacrosImpl { this: MacroCommons =>

      /** This method creates the code for a whole Show[A].
       * 
       * Notice, `Expr.quote` and `Expr.splice` utility,
       * which allows quoting and splicing to cross-compile!
       */
      def deriveTypeClass[A: Type]: Expr[Show[A]] = Expr.quote {
        new Show[A] {
          def show(value: A): String = Expr.splice {
            deriveOrFail[A](Expr.quote { value })
          }
        }
      }

      /* And this will create the code for `show.show(value)` body. */
      private def deriveOrFail[A: Type](value: Expr[A]): Expr[String] = ???
    }
    ```

!!! example "`src/main/scala-2/example/ShowCompanionCompat.scala` - adapter for Scala 2"

    ```scala
    package example

    import scala.language.experimental.macros
    import scala.reflect.macros.blackbox

    private[demo] trait ShowCompanionCompat { this: Show.type =>

      def derived[A]: Show[A] = macro ShowMacros.deriveTypeClassImpl[A]
    }

    // I hope that one day most of it could be automated, but for now we have to bear.
    private[demo] class ShowMacros(val c: blackbox.Context) extends hearth.MacroCommonsScala2 with ShowMacrosImpl {

      def deriveTypeClassImpl[A: c.WeakTypeTag]: c.Expr[Show.AutoDerived[A]] = deriveTypeClass[A]
    }
    ```

!!! example "`src/main/scala-3/example/ShowCompanionCompat.scala` - adapter for Scala 3"

    ```scala
    package example

    import scala.quoted.*

    private[demo] trait ShowCompanionCompat { this: Show.type =>

      inline def derived[A]: Show[A] = ${ ShowMacros.deriveTypeClass[A] }
    }

    // I hope that one day most of it could be automated, but for now we have to bear.
    private[demo] class ShowMacros(q: Quotes) extends hearth.MacroCommonsScala3(using q), ShowMacrosImpl

    private[demo] object ShowMacros {

      def deriveTypeClass[A: Type](using q: Quotes): Expr[Show.AutoDerived[A]] = new ShowMacros(q).deriveTypeClass[A]
    }
    ```

## How to make macro more maintainable?

I recommend working with macro as if it was server endpoint, that you'd deploy to call remotely:

 - you'd either receive a successful result or an error, if it's an error, it should provide enogh information for the user
   to correct all the issues at once (so inner errors should be aggregated when possible)
 - if you need to investigate, assume you couldn't always just put random `println`s - if there is some log of what **actually**
   **happened**, then you can investigate without having to edit and redeploy the code
 - if you are used to conventions of Cats/Scalaz/ZIO, you might want to work with something like IO monad,
   to reuse your intiotions and habits
   
How it translates to macros?

### Error aggregation

While you can immediatelly fail with something like:

!!! example "Immediate abortion"

    ```scala
    // inside a trait with this: MacroCommons =>
    Environment.reportErrorAndAbort("Error has happened")
    ```

you might want instead to use something like:

!!! example "`NonEmptyVector` for error aggregation"

    ```scala
    import hearth.fp.data.NonEmptyVector

    Either[NonEmptyVector[Throwable], SuccessfulValue]
    ```

We could gether errors in a collection and only right before exiting from the macro we would
combine all the errors into a single `String`.

And we can always make:

!!! example "Helper `DerivationError` stackless `Exception`"

    ```scala
    sealed trait DerivationError extends scala.util.control.NoStackTrace with Product with Serializable
    object DerivationError {
      final case class Error1(arg: String) extends DerivationError
      final case class Error2(arg: String) extends DerivationError
      // ...
    }
    ```

### Previewing results

Macros allow us to show some information which is not an error:

!!! example "Hint from a macro"

    ```scala
    Environment.reportInfo("Some information")
    Environment.reportWarn("Some warning")
    ```

It would be visible:

 - in the console during compilation
 - as a hint in IDE ([Metals](https://scalameta.org/metals/) with e.g. VS Code, IntelliJ)
 - as a hint in [Scastie](https://scastie.scala-lang.org/)

which isn't always true about `println`s (that would dissapear if users compile using some compilation server).

To decide whether we want the logs shown we could, e.g. provide a `scalac` option that would turn it on globally,
or an implicit which would enable this hints whenever it's imported into the scope:

!!! example "Dedicated type to enable showing logs"

    ```scala
    /** Import [[Show.LogDerivation]] in the scope to preview how the derivation is done.
      *
      * Put outside of companion to prevent the implicit from being summoned automatically!
      */
    implicit val logDerivation: LogDerivation = LogDerivation()

    /** Special type - is its implicit is in scope then macros will log the derivation process.
      *
      * @see
      *   [[hearth.demo.debug.logDerivation]] for details
      */
    sealed trait LogDerivation
    object LogDerivation {
      private object Instance extends LogDerivation
      def apply(): LogDerivation = Instance
    }
    ```

!!! example "Deciding whether or not to show log"

    ```scala
    // inside a trait with this: MacroCommons =>
    val LogDerivation: Type[example.LogDerivation] = Type.of[example.LogDerivation]

    /** Enables logging if we either:
      *   - import [[logDerivation]] in the scope
      *   - have set scalac option `-Xmacro-settings:show.logDerivation=true`
      */
    def shouldWeLogDerivation: Boolean = {
      implicit val LogDerivation: Type[example.LogDerivation] = LogDerivation
      def logDerivationImported = Expr.summonImplicit[Show.LogDerivation].isDefined

      def logDerivationSetGlobally = (for {
        data <- Environment.typedSettings.toOption
        show <- data.get("show")
        shouldLog <- show.get("logDerivation").flatMap(_.asBoolean)
      } yield shouldLog).getOrElse(false) // We don't want to fail the derivation if we can't parse the settings.

      logDerivationImported || logDerivationSetGlobally
    }

    if (shouldWeLogDerivation) {
      Environment.reportInfo("Some information")
    }
    ```


Unfortunatelly, on both Scala 2 and Scala 3, only the first such call provides a hint.
All the following will be no-ops, so we would have to aggregate the individual logs somehow.

One such approach would be to use a mutable collection to write to. Or passing around
an immutable collection, and have it updated (the `Writer` monad).

### Macro `IO`-monad

But if one likes to works with Cats/Scalaz/ZIO, and would like to reuse ones' experience with these libraries,
then there is an optional `MIO` (Macro `IO`) monad.

The name only refers to how similar it is in usage to IO monads from established ecosystems, since there is little
need to use an actual IO in macros. However:

 - it is lazy, non-memoized, and catches `NonFatal` exceptions

    !!! example "`MIO` type"

        ```scala
        import hearth.fp.effect.*

        val i: MIO[Int] = MIO {
          "this might throw".toInt
        }
        ```

 - it uses `hearth.fp.data.NonEmptyVector[Throwable]` as its error type already, allows both monadic composition
   (with fail-fast semantics) and `Parallel` (`.parMap2`, `.parTraverse`), which aggregates the errors from multiple
   `MIO`s

    !!! example "`.parTraverse` example"

        ```scala
        import hearth.fp.effect.*
        import hearth.fp.instances.*
        import hearth.fp.syntax.*

        list.parTraverse { (item: A) =>
          mioResult(item) // : MIO[B]
        } // : MIO[List[B]] aggregating errors from each `mioResult`!
        ```

 - is provides `MLocal` for controlled mutation

    !!! example "`MLocal` example"

        ```scala
        import hearth.fp.effect.*

        val counter = MLocal(initial = 0, fork = i => i + 1, join = (a, b) => a max b)
      
        // This is just a recipe for computation, it's not executed yet.
        // In this recepe we are reading the current value of the counter, and logging it to 3 different levels.
        val printSth = for {
          i <- counter.get
          _ <- Log.info("Print info: counter is now $i")
          _ <- Log.warn("Print warning: counter is now $i")
          _ <- Log.error("Print error: counter is now $i")
        } yield 1
        ```

 - it provides `Log` utility for appending _scoped_ logs:

    !!! example "`Log` example"

        ```scala
        import hearth.fp.effect.*

        Log.nestedScope("New nested scope") {
          Log.info("Stated doing X") >>
            someMioOperation(args) <*
            Log.info("Done doing X")
        }
        ```

 - it provides `async`-`await` operations for cases when monadic/parallel interfaces would be inconvenient or
   impossible to use, e.g.

    !!! example "`async`-`await` (direct style) example"

        ```scala
        import hearth.fp.effect.*

        MIO.async { await =>
          Expr.quote {
            new Show[A] {

              def show(a: A): String = Expr.splice {
                await( errorReturningMethod(Expr.quote { a }) ) // good luck handling it with for-comprehension
              }
            }
          }
        }
        ```

If the whole derivation was handled in `MIO`, and the result is some `MIO[Expr[A]]`, then you could, at once:

 - print its logs
 - return succesful `Expr[A]` or
 - combine failures into a single error message
 
with:

!!! example "Handle logging, showing errors and/or returning result at once"

    ```scala
    /** Converts the [[MIO]] results into an [[Expr]] or error message. */
    private def deriveOrFail[A: Type](value: Expr[A]): Expr[String] =
      Log.namedScope(s"Derivation for ${Type.prettyPrint[A]}") {
        computeMioResult(value)
      }
      .expandFinalResultOrFail(name, renderInfoLogs = shouldWeLogDerivation) { (errorLogs, errors) =>
        val errorsStr = errors.toVector.map { error => ...  }.mkString("\n")

        s"""Failed to derive Show for ${Type.prettyPrint[A]}:
            |$errorsStr
            |Error logs:
            |$errorLogs
            |""".stripMargin
      }
    ```

However, all of these are completely optional, if you are not fond of this style of programming,
then you can simply not use it.

## Utilities for making life easier

This part would need a real tutorial and docs, but for now:

 - inside a shared-logic trait (`MacroCommons`) we are working on abstract types and abstract methods:

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
   

 - Hearth implements shared utilities from bottom up: `Type[A]` and `Expr[A]` build on top of `UntypedType` and `UntypedExpr`,
   then `Method[A]`s build on top of `Type[A]` and `Expr[A]`, then `CaseClass[A]`, `Enum[A]` and `JavaBean[A]` build on top of them.
   (The lower is the abstraction level, the less likely API is to change).

 - probably for most common use cases you want to use high-level things like:

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

Probably, the current library and its documentation offer too little to make people without experience start writing macros,
but at this point we mostly want to _prove_ that it is possible to get to the stage when it would be signifficantly easier than now.
