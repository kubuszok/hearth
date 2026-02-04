# Best Practices

## How to make a macro more user-friendly?

I recommend working with a macro as if it were a server endpoint that you'd deploy to call remotely:

 - you'd either receive a successful result or an error; if it's an error, it should **provide enough information for the user
   to correct all the issues at once** (so inner errors should be aggregated when possible)
 - errors should be informative, so it's good to create a dedicated algebraic data type for them - it's easier to record all
   information that caused an issue into a `case class` and then, before exiting from a macro, combine a non-empty collection
   of errors, perhaps using their structure for grouping issues together, than to turn a bunch of strings into something decent
 - if you need to investigate, assume you can't always just add random `println`s - if there is some log of what **actually**
   **happened**, then you or your user **can investigate without having to edit and redeploy the code**
 - if you need to provide a config to a macro, the global configs should be provided via a dedicated `-Xmacro-settings` parameter
   (much more reliable than using some JVM `-D` option!), and local configs should be handled by the macro's DSL or, e.g., implicits
 - macros should not generate warnings that users of `-Xfatal-errors` would have to manually suppress
   
How does it translate to macros?

### Use sanely-automatic type class derivation

Details in [the blog post](https://kubuszok.com/2025/sanely-automatic-derivation/).

An example is shown on [the landing page](index.md).

### Aggregate errors instead of failing fast

While you can immediately fail with something like:

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

We could gather errors in a collection and only right before exiting from the macro we would
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

Micro FP provides methods like [`.parTraverse`](micro-fp.md#parallel) to map collections
and aggregate their fallible results. If you don't like the provided types, you can
[implement your own](micro-fp.md#creating-type-class-instances).

Results for each `case class` field/`enum` subtype are perfect candidates for such result aggregation.

### Store potentially large/recursive logic as defs

Instead of inlining the whole logic into a single, giant expression, consider splitting it into
several smaller `def`s. This approach:

 * allows reusing some logic you generated once in another place that needs the same logic (e.g. during a type class derivation)
    * it also allows generating recursive code without tricks that make the whole type class instance lazily initialized
 * prevents issues with a method's body exceeding the allowed size (compilation fails if bytecode would exceed 64kB)
 * helps the JVM compile and optimize the code, since smaller methods are easier to analyze and profile

!!! tip "Use [`ValDefs`](basic-utilities.md#valdefs) and combine them with `.map2`/`.parMap2` and/or use [`ValDefsCache`](basic-utilities.md#valdefscache) to define multiple `def`s in scope, use them, and prepend before the final expression."

### Allow previewing macro logic

Macros allow us to show some information which is not an error:

!!! example "Hint from a macro"

    ```scala
    //> using dep com.kubuszok::hearth:{{ hearth_version() }}
    import hearth.MacroCommons

    trait YourMacros { this: MacroCommons =>

      def macroImpl: Expr[String] = {
        // This will produce an info message during compilation/in IDE/in Scastie
        Environment.reportInfo("Some information")
        // This will produce a warning message during compilation/in IDE/in Scastie
        Environment.reportWarn("Some warning")

        Expr.quote { "result" }
      }
    }
    ```

It would be visible:

 - in the console during compilation
 - as a hint in IDE ([Metals](https://scalameta.org/metals/) with e.g. VS Code, IntelliJ)
 - as a hint in [Scastie](https://scastie.scala-lang.org/)

which isn't always true about `println`s (which would disappear if users compile using some compilation server).

To decide whether we want the logs shown, we could, e.g., provide a `scalac` option that would turn it on globally,
or an implicit which would enable these hints whenever it's imported into the scope:

!!! example "Dedicated type to enable showing logs"

    ```scala
    /** Import [[LogDerivation]] in the scope to preview how the derivation is done.
      *
      * Put outside of companion to prevent the implicit from being summoned automatically!
      */
    implicit val logDerivation: LogDerivation = LogDerivation()

    /** Special type - if its implicit is in scope then macros will log the derivation process.
      * 
      * Let's say it's in the `example` package.
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
      def logDerivationImported = Expr.summonImplicit[example.LogDerivation].isDefined

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

Unfortunately, on both Scala 2 and Scala 3, only the first such call provides a hint.
All the following will be no-ops, so we would have to aggregate the individual logs somehow.

One such approach would be to use a mutable collection to write to. Or passing around
an immutable collection, and have it updated (the `Writer` monad).

### Use namespaces in `Xmacro-settings`

`-Xmacro-settings` is a global Scalac option intended to pass information to macros - but all macros see all the settings.

To make sure that several projects won't accidentally use the same setting name for a different thing use some prefix
e.g. `name-of-your-library.` before the actual setting. Such prefixes will also be friendly to [parsing settings
as `hearth.data.Data` format](basic-utilities.md#macro-settings).

### Suppress warnings

It's bad UX if your users have to suppress the warnings for the code they haven't even written in the first place.

Consider generating code looking more or less like:

```scala
{
  @scala.annotation.nowarn // suppress the compiler's linters
  @SuppressWarnings("org.wartremover.warts.All", "all") // suppress Wartremover and Scapegoat lints
  val result = ...
  result
}
```

and testing your macros with a reasonably large number of compiler linters enabled.

## How to make a macro more maintainable?

I recommend working with a macro as if it were any other business logic that you would have to maintain for a while:

 - define an ADT for errors instead of passing around only raw `String`s - you can turn them into an error message
   at the end of the derivation
 - define helper methods and types, split large chunks of logic into smaller ones, give them high-level names
   that explain _what you are trying to achieve_
 - write tests for your macros - while they are unit tests (expand macro, check its results), since you cannot
   _mock_ inside of a macro, it might feel a bit like small integration tests
 - if you are used to conventions of Cats/Scalaz/ZIO, you might want to work with something like an IO monad
   to reuse your intuitions and habits
 - use `parTraverse` and `Parallel` when combining several independent results, e.g. results computed for
   each field of a `case class` or each subtype of an `enum`
 - cache intermediate results, by generating internally `def`s instead of multiple type class instances - a bunch
   of local `def`s do not require additional allocations, and might potentially allow your type class to be recursive
 - separate reusable runtime utilities from the code that has to be generated during compilation - and check the runtime
   utilities with [MiMa](https://github.com/lightbend-labs/mima)
   
How it translates to macros?

### Use the Macro `IO` monad

If you like to work with Cats/Scalaz/ZIO and would like to reuse your experience with these libraries,
then there is an optional `MIO` (Macro `IO`) monad.

The name only refers to how similar it is in usage to IO monads from established ecosystems, since there is little
need to use actual IO in macros. However:

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

 - it provides `MLocal` for controlled mutation

    !!! example "`MLocal` example"

        ```scala
        import hearth.fp.effect.*

        val counter = MLocal(initial = 0, fork = i => i + 1, join = (a, b) => a max b)
      
        // This is just a recipe for computation, it's not executed yet.
        // In this recipe we are reading the current value of the counter, and logging it to 3 different levels.
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
          Log.info("Started doing X") >>
            someMioOperation(args) *>
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
 - return a successful `Expr[A]` or
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

### Use named scopes with `Log`s

If you are computing some smaller part of a whole expression returned by a macro, it's worth
wrapping it in its own named scope. When rendering the whole log the nesting will help
telling the story of what actually happened, and what is related to what.

!!! example "Nested logs with named scope"

    ```scala
    //> using dep com.kubuszok::hearth:{{ hearth_version() }}
    //> using scala {{ scala.3 }}
    import hearth.fp.effect.*

    Log.namedScope("I want all logs here grouped and indented!") {
      Log.info("log 1") >>
        Log.info("log 2") >>
        Log.info("log 3") >>
        Log.info("log 4")
    }
    ```

### Use MiMa to check all your runtime code

...and make sure that macros are only calling these runtime utilities [that MiMa is checking](https://github.com/lightbend-labs/mima).

Then you can rewrite all your macro's internals and change what code is output by a macro - if the interfaces you call, and the type you instantiate haven't broken compatibility,
the generated code will keep working even if it is linked against a newer runtime version.

But to make sure that _the behavior of the code generated by a macro_ is the same (minus fixed bugs) you have to write tests.

To plan your API changes in advance, and check how you could evolve it without breaking changes for the user, try [Mimalyzer](https://mimalyzer.indoorvivants.com/).

## How to debug macros, implicits and other metaprogramming utilities?

Debugging macros, implicit resolution, and compiler behavior in Scala relies on a mix of compiler flags, AST inspection, and controlled instrumentation. The techniques below are well-established, documented, and commonly used in real compiler and library work.

If you want to write a library that uses metaprogramming under the hood, you will have to learn these eventually.
But try to provide your users good enough error messages (and some debugging utilities e.g. logging from a macro if an implicit is present),
that they won't have to learn these themselves.

### Inspect macro expansion (Scala 2)

It's useful if we want to see how some macro would expand.
But since it shows results for _every_ macro expansion in a single compilation unit, I'd recommend using it in some REPL or a dedicated module,
and only work on a single case reproducing the issue we are trying to investigate.

!!! hint "If only the final tree is of interest, consider [Hearth's debugging utilities instead](debug-utilities.md)."

**What it gives you**:

  *	Visibility into macro expansion steps
  *	Ability to see generated trees without modifying macro code

**How**:

  *	Add to scalac options macro debug flags:
    *	`-Ymacro-debug-lite`
    *	`-Ymacro-debug-verbose`
  *	You can control the expansion:
    *	`-Ymacro-expand:none|normal|discard`

**When to use**:

  *	Macro expands incorrectly or unexpectedly
  *	Expansion differs between call sites
  *	You want compiler-side visibility without printlns

**Sources**:

  *	[Scala compiler options](https://docs.scala-lang.org/overviews/compiler-options/)

### Inspect compiler phases

It's useful when debugging a compiler plugin, or if want to find out how some phase is named to print its output.
(The later is virtually unusable in larger codebases, so it only makes sense if we can isolate a small snipped reproducing our issue, as it will still produce several screens of output).

**What it gives you**:

  *	A complete list of compiler phases
  *	Ability to understand where a transformation or failure happens

**How**:

  *	List phases by:
    *	Scala 2 & 3: `-Vphases` / `-Xshow-phases`
  *	Print trees after specific phases:
    *	`-Vprint:<phase>`
    *	`-Vprint-types`
    *	`-Vprint-pos`
  *	Interactive AST browser:
    *	`-Vbrowse:<phase> / -Ybrowse`
  * Additional compiler-level debugging switches
    *	Phase logging: `-Vlog:<phase>`
    *	Typer tracing: `-Vtyper`
    *	Tree validation: `-Ycheck:<phase>`
    *	Interactive error prompt: `-Xprompt`

**When to use**:

  *	Macro expansion happens but later phases break it
  *	Implicit is resolved but erased/transformed unexpectedly
  *	You need to know which phase introduced or removed a tree

**Sources**:

  *	[Compiler options (Scala 2)](https://docs.scala-lang.org/overviews/compiler-options/)
  *	[Scala 2 → Scala 3 option lookup](https://docs.scala-lang.org/scala3/guides/migration/options-lookup.html)

### Inspect implicit resolution

It's useful when the issue we have is not a macro expansion, but understanding which implicits are generated
(any perhaps which are missing). It's a more "pro" alternative to the ["binary search" approach](#binary-search-implicit-resolution-failures).

!!! hint "If implicit resolution succeeds, and you only want to see what exactly it generated, consider [Hearth's debugging utilities instead](debug-utilities.md)."

**What it gives you**:

  *	Full trace of implicit search
  *	Explanation why a candidate was rejected
  *	Compact or verbose resolution trees

**How**:

  *	Legacy logging:
    *	`-Xlog-implicits`
  *	Modern ([splain](https://github.com/tek/splain)-based) diagnostics:
    *	`-Vimplicits`
    *	`-Vimplicits-verbose-tree`
    * prior to `2.13.16` required a [`splain`](https://github.com/tek/splain) compiler plugin
  *	Explain mode:
    *	Scala 3: `-explain`, `-explain-types`

**When to use**:

  *	“Implicit not found” with many candidates
  *	Ambiguous implicit errors
  *	Typeclass derivation failures

**Sources**:

  *	[Compiler error options](https://docs.scala-lang.org/overviews/compiler-options/errors.html)
  *	[Scala 2.13.6 release (splain integration)](https://www.scala-lang.org/news/2.13.6/)
  *	[splain plugin (historical context)](https://github.com/tek/splain)
  *	[Practical walkthrough of implicit debugging](https://medium.com/virtuslab/debugging-implicits-2666b38ae415)

### “Binary search” implicit resolution failures

It's useful if you suspect that the investigation would be quick, and you can actually save some time
but not editing your build tool's compler's options. Or if you don't want to read long diagnostic outputs,
or need a debugging that could be performed by a LLM, just by checking if there are compilation errors.

If there is a lot of implicits, no obvious candidated where to start, and no clear idea which implicits
are already manually defined in the scope (rather than imported into it as low priority), this approach
can be rather tedious.

(It's also a great showcase why macro-based recursive derivation with some logging and user-oriented error messages,
can offer better user experience than Shapeless or Mirror-based approach).

**What it gives you**:

  *	Isolation of which implicit candidate causes failure
  *	Understanding of search scope and priority

**How**:

  *	Introduce temporary/stub implicits:
    * for compilation it's enough to write e.g. `implicit val fooEncoder: Encoder[Foo] = ???`
    * if you debugging e.g. `case class` create such stub for each field's type (avoiding duplicates!),
      if it's `sealed`/`enum`, create it for every case, etc
    * do not add a stub if you know that you defined such implicit in the same scope!
    *	force resolution with `implicitly[T]`, `summon[T]` (on Scala 3) or library's summon method
    * add/remove stubs until the code compiles
  * Comment out half of the stubs, and check if it still compiles
    * If it does, uncomment them and comment out the other half, check if it compiles
  * (Assuming it's only 1 implicit that's missing/ambiguous), now you should have 2 groups of stubs: one proved to
    not be needed (import or implicit in companion can be picked up), and one that is
  * Remove the unnecessary group of stubs
  * Commend out half of the remaining ones, and repeat the process
  * Continue until you identified the stub that is necessary to work:
    * if it's a type that cannot be derived - check if you are yet to define it, or have you missed importing it
    * if it's a type that can be derived, but it failed - you have to repeat the process for all its components
      (implicits for fields or subtypes)

**When to use**:

  *	Large implicit scopes
  *	Derivation stacks with multiple fallback instances
  *	Confusing error messages even with -Vimplicits

**Sources**:

  *	[StackOverflow discussion of implicit debugging techniques](https://stackoverflow.com/questions/59348301/in-scala-2-or-3-is-it-possible-to-debug-implicit-resolution-process-in-runtime)
  *	[Phase-level inspection (typer) in practice](https://medium.com/virtuslab/debugging-implicits-2666b38ae415)

### Printing and inspecting trees inside macros (Scala 2)

It's useful when working on a macro, and you want to e.g. create some code by hand, pass it to macro to print it,
and learn the shape of AST to create it yourself. Or if you already build the AST to check if it does what you want.

!!! hint "If you want better printing output (syntax highlighting, indentation), consider [better-printers](better-printers.md)."

!!! hint "If you are using [Hearth's basic utilities](basic-utilities.md), `expr.prettyPrint`, `expr.prettyAST`, `tpe.prettyPrint`, etc. is already using better printers under the hood!"

!!! hint "If you only want to debug one macro, and avoid printing intermediate results for every expansion, consider [Hearth's tools for selective debugging](basic-utilities.md#debug-helpers)."

**What it gives you**:

  *	Raw and desugared ASTs
  *	Generated source-like output

**How**:

  *	`showRaw(tree)`
  *	`showCode(tree)`

**Sources**:

  *	[Scala reflection printers API](https://www.scala-lang.org/api/2.13.1/scala-reflect/scala/reflect/api/Printers.html)

### Printing and inspecting trees inside macros (Scala 3)

It's useful when working on a macro, and you want to e.g. create some code by hand, pass it to macro to print it,
and learn the shape of AST to create it yourself. Or if you already build the AST to check if it does what you want.

!!! hint "If you want better printing output of AST (syntax highlighting, indentation), consider [better-printers](better-printers.md)."

!!! hint "If you are using [Hearth's basic utilities](basic-utilities.md), `expr.prettyPrint`, `expr.prettyAST`, `tpe.prettyPrint`, etc. is already using better printers or `.show(...)` under the hood!"

!!! hint "If you only want to debug one macro, and avoid printing intermediate results for every expansion, consider [Hearth's tools for selective debugging](basic-utilities.md#debug-helpers)."

**What it gives you**:

  *	Typed and untyped trees
  *	Structured or code-like output

**How**:

  * `tree.show(using Printer.TreeStructure)`
  * `tree.show(using Printer.Code)`

**Sources**:

  *	[Scala 3 reflection guide](https://docs.scala-lang.org/scala3/guides/macros/reflection.html)
