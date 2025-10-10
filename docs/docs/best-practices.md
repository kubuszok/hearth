# Best Practices

!!! warning "Work in Progress - follow [hearth#56](https://github.com/MateuszKubuszok/hearth/issues/56) to see the progress of documenting Hearth."

## How to make a macro more user-friendly?

I recommend working with a macro as if it was server endpoint, that you'd deploy to call remotely:

 - you'd either receive a successful result or an error, if it's an error, it should **provide enough information for the user
   to correct all the issues at once** (so inner errors should be aggregated when possible)
 - errors should be informative, so it's good to create a dedicated algebraic data type for it - it's easier to record all
   information that caused some issue into a `case class` and then at before exiting from a macro combine a non-empty collection
   of errors, perhaps using their structure for grouping issues together, than turning a bunch of strings into something decent
 - if you need to investigate, assume you couldn't always just put random `println`s - if there is some log of what **actually**
   **happened**, then you or your user **can investigate without having to edit and redeploy the code**
 - if you need to provide a config to a macro, the global configs should be provided via dedicated `-Xmacro-settings` parameter
   (much more reliable than using some jvm `-D` option!), and local configs should be handled by macro's DSL or e.g. implicits
 - macros should not generate warnings, that users of `-Xfatal-errors` would have to manually suppress
   
How it translates to macros?

### Error aggregation

While you can immediatelly fail with something like:

!!! example "Immediate abortion"

    ```scala
    //> using dep com.kubuszok::hearth:{{ hearth_version() }}
    import hearth.MacroCommons

    trait YourMacros { this: MacroCommons =>

      def macroImpl: Expr[String] =
        Environment.reportErrorAndAbort("Error has happened")
    }
    ```

you might want instead to use something like:

!!! example "`NonEmptyVector` for error aggregation"

    ```scala
    //> using dep com.kubuszok::hearth-micro-fp:{{ hearth_version() }}
    import hearth.fp.data.NonEmptyVector

    val result: Either[NonEmptyVector[Throwable], String] =
      Left(NonEmptyVector(new Exception("your error")))
    ```

We could gether errors in a collection and only right before exiting from the macro we would
combine all the errors into a single `String`.

And we can always make:

!!! example "Helper `DerivationError` - a stackless `Exception`"

    ```scala
    //> using dep com.kubuszok::hearth-micro-fp:{{ hearth_version() }}
    import hearth.fp.data.NonEmptyVector

    sealed trait DerivationError extends scala.util.control.NoStackTrace with Product with Serializable
    object DerivationError {
      final case class NotSupported(msg: String) extends DerivationError
      final case class Private(arg: String) extends DerivationError
      // ...
    }

    val result: Either[NonEmptyVector[Throwable], String] =
      Left(NonEmptyVector(DerivationError.NotSupported("your msg")))
    ```

### Previewing results

Macros allow us to show some information which is not an error:

!!! example "Hint from a macro"

    ```scala
    //> using dep com.kubuszok::hearth:{{ hearth_version() }}
    import hearth.MacroCommons

    trait YourMacros { this: MacroCommons =>

      def macroImpl: Expr[String] = {
        // This will produce into message during compilation/in IDE/in Scastie
        Environment.reportInfo("Some information")
        // This will produce warn message during compilation/in IDE/in Scastie
        Environment.reportWarn("Some warning")

        Expr.quote { "result" }
      }
    }
    ```

It would be visible:

 - in the console during compilation
 - as a hint in IDE ([Metals](https://scalameta.org/metals/) with e.g. VS Code, IntelliJ)
 - as a hint in [Scastie](https://scastie.scala-lang.org/)

which isn't always true about `println`s (which would dissapear if users compile using some compilation server).

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

## How to make a macro more maintainable?

I recommend working with a macro as if it was any other business logic, that you would have to maintain for a while:

 - if you are used to conventions of Cats/Scalaz/ZIO, you might want to work with something like IO monad,
   to reuse your intuitions and habits
 - use `parTraverse` and `Parallel` when combining several independedn resuluts together, e.g. results fomputed for
   each field of a `case class` or each subtype of an `enum`
 - cache intermediate results, by generating internally `def`s instead of multiple type class instances - a bunch
   of local `def`s do not require additional allocations, and might potentially allow your type class to be recursive
 - separate reusable runtime utilities from the code that has to be generated during compilation - and check the runtime
   utilities with [MiMa](https://github.com/lightbend-labs/mima)
   
How it translates to macros?

### Macro `IO`-monad

If one likes to works with Cats/Scalaz/ZIO, and would like to reuse ones' experience with these libraries,
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
    //> using dep com.kubuszok::hearth:{{ hearth_version() }}
    //> using scala {{ scala.3 }}
    import hearth.MacroCommons
    import hearth.fp.effect.*

    trait YourMacros { this: MacroCommons =>

      /** Just an example, so we won't implement it */
      def computeMioResult[A: Type](expr: Expr[A]): MIO[Expr[String]]

      /** Converts the [[MIO]] results into an [[Expr]] or error message. */
      def deriveOrFail[A: Type](value: Expr[A]): Expr[String] =
        Log.namedScope(s"Derivation for ${Type.prettyPrint[A]}") {
          computeMioResult(value)
        }
        .runToExprOrFail("Our macro derivation") { (errorLogs, errors) =>
          val errorsStr = errors.toVector.map { error => error.getMessage  }.mkString("\n")

          s"""Failed to derive Show for ${Type.prettyPrint[A]}:
             |$errorsStr
             |Error logs:
             |$errorLogs
             |""".stripMargin
        }
    }
    ```

However, all of these are completely optional, if you are not fond of this style of programming,
then you can simply not use it.
