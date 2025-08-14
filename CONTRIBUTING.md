# Contributing to Hearth

First off, thanks for taking the time to contribute! ❤️

Hearth is currently at the Proof-of-Concept stage. We want to deliver enough functionality to proove its usefulness, and we want to make the current features solid
before we'd consider adding more of them and adjusting the design.

Currently the best way oh helping is contributing to [making current features production-ready](https://github.com/MateuszKubuszok/hearth/issues/36) to deliver the solid 0.1.0!

The second best, is raising awareness about the project ☺️

Once that is done, we have a lot more opportunities to contribute (feedback, documentation, bug reports, feature requests...).

----

### Table of contents

 1. [Issue or discussion?](#issue-or-discussion)
 2. [General coding guidelines](#general-coding-guidelines)
 3. [How to start working with the code?](#how-to-start-working-with-the-code)
 4. [General architecture overview](#general-architecture-overview)
    1. [How macros are usually called](#how-macros-are-usually-called)
    2. [Sharing implementation between Scala 2 and Scala 3](#sharing-implementation-between-scala-2-and-scala-3)
    3. [Cross-Quotes](#cross-quotes)
    4. [FP-module](#fp-module)
 5. [How to test changes?](#how-to-test-changes)
 6. [How to debug macros?](#how-to-debug-macros)

## Issue or discussion?

As long as the library is on its Proof-of-Concept stage - that is until [these tasks are done](https://github.com/MateuszKubuszok/hearth/issues/36) -
thers is no point in creating bug reports (we already know that these are not yet reliable) or creating new feature requests.

For the time being we suggest to discuss:

 - in the existing issues (when asking about them),
 - in pull requests (when when discussion is about addressing the issue)
 - creating a [new discussion](https://github.com/MateuszKubuszok/hearth/discussions), when it's none of the above
   (asking about what is the purpose of something, how something can be done, it is currently possible, etc)

Once the library is released as 0.1.0, with _some_ features provided, we would start using bugs and feature requests.

## General coding guidelines

 0. This library is currently a Proof-of-Concept - we are focusing on delivering the minimal initial functionality to make it useful
    and prove, that the community could benfit from it, and that contributors would find worth their time to help it grow.
 1. This library aims to follow the [Pareto-principle](https://en.wikipedia.org/wiki/Pareto_principle) - 80% of use cases
    should be able to implement with 20% of effort. Simple things should be easy, but for complex we don't necessarily need to provide
    a solution if it would complicate the API for everyone that does not have a complex use case (fallback to normal macros or lowel abstraction levels is always possible).
 2. This library is cross compiled: it should work on both Scala 2.13 and Scala 3, be published for normal Scala but also Scala.js and Scala Native.
 3. It is formatted with Scalafmt, using `-Xsource:3` syntax on Scala 2.13-specific _and_ shared code (enabling _some_ of Scala 3 syntax features)
    and with Scala 3 syntax on Scala 3-specific code.
 4. CI checks whether 2. and 3. are respected, but one can use `quick-test ; scalafmtAll` to quickly check if code compies and format code.
 5. Don't be afraid of using `git commit --amend`, `git rebase` and `git push --force` on **your branch** to make it cleaner. And keep messages descriptive!
 6. PR before merge should be passing all checks.
 7. We do **not** use the braceless syntax here.
 8. I prefer to use:
    ```scala
    package a
    package b
    package c
    ```

    instead of:

    ```scala
    package a.b.c
    ```

    because it automatically imports eveything from `a` and `b`, which remind us that the package visibility is hierarchical in Scala.
 9. If solution works great with Scala 3, but does not work at all with Scala 2.13 - it goes against the goals of this library.
    Even when we would implement features that only work on Scala 3 (e.g. opaque types, named tupled) - it has to be done in a way
    that does not break the cross-compiled code.

## How to start working with the code?

You need to have installed:

 1. [`git`](https://github.com/git-guides)
 2. Java SDK - for local development we recommend something like [Sdkman](https://sdkman.io/) or [jEnv](https://www.jenv.be/) for managing Java SDKs
 3. `sbt` - however, if you don't want to use some package manager to install it, you can run `./sbt` instead of `sbt` to have it installed for you
 4. IDE that supports Scala:

     1. One popular option is [IntelliJ IDEA](https://www.jetbrains.com/idea/download/) - its [core part is OSS](https://github.com/JetBrains/intellij-community),
        and [Scala plugin](https://blog.jetbrains.com/scala/) is [OSS as well](https://github.com/JetBrains/intellij-scala), so there are no issues in using them
        for development (or commercially).
       
        After installing IntelliJ you need to install Scala plugin to enable support for it

     2. Another option is to use [Scala Metals](https://scalameta.org/metals/) - it's a [Language Server Protocol](https://microsoft.github.io/language-server-protocol/)
        that provides Scala IDE-like features to any editor which support LSP - there are official installation instructions for Visual Studio Code, Vim, Sublime Text, Emacs, Helix, GitPod.
       
        If you don't have a preference, use Visual Studio Code, if you want to use AI, use something VSC-based like Cursor, Windsurf, Augumented Code or whatever is cheapest hype of the week.
        Then install Scala and Scala Metals extensions from the extension store.

     3. If you want to use AI in general, I suggest Scala Metals and reading about [Model Context Protocol](https://modelcontextprotocol.io/)
        that [they enable](https://scalameta.org/metals/docs/editors/user-configuration#start-mcp-server).

I recommend to:

 1. [fork the repository](https://github.com/MateuszKubuszok/hearth/fork) to your own GitHub
 2. [clone it](https://docs.github.com/en/get-started/git-basics/about-remote-repositories#cloning-with-ssh-urls) to your machine
 3. start in your terminal sbt shell with `sbt` (if you installed it globally) or `./sbt` (if you didn't)
 4. run in it `ci-jvm-2_13` and `ci-jvm-3` tasks to test if everything works
 5. if it does, then you should be able to open the folder in your IDE, and it should recognize it as sbt project and start indexing it
 6. if it succeeds you should be able to see that IDE imported modules like:
    - `hearth`/`hearth3`
    - `hearthCrossQuotes`/`hearthCrossQuotes3`
    - `hearthMicroFp`/`hearthMicroFp3`
    - `hearthTests`/`hearthTests3`

You will see _only_ either Scala 2.13 _or_ Scala 3 projects there. And you will _only_ see JVM projects.

While the build tool easily handles situations where something:

 - should be only used as a source code for Scala 2.13 or
 - should be only used as a source code for Scala 3 or
 - should be only used as a source code for JVM Scala (not Scala.js nor Scala Native) or
 - should be shared for all platforms and Scala versions

IDEs do not understand what to do in such cases: treat shared file as which language versions and which platform? Where should
"find all usages" and "jump to implementation" point to? So in `dev.properties` you see something like:

```properties
# Do not commit changes to this file!
# Change it locally, reload build in IDE, and develop.
# Consider running: git update-index --assume-unchanged dev.properties

# Allowed: 2.13, 3
ide.scala = 3
# Allowed: jvm, js, native
ide.platform = jvm

# Allowed: true, false
log.cross-quotes = false
```

You can use it control which version you want to work on in your IDE. You need to change `ide.scala`/`ide.platform` and reimport build.

You can open and edit sources for the other version as well, buy you will get only basic syntax highlighting and no intellisense.

## General architecture overview

Since this library should make it easier to work with macros, let us remember how to call a macro.

### How macros are usually called

Let's say you need:

```scala
def method[A, B](value: A): B = ??? // this should be implemented by a macro
```

When working with macros on Scala 2 you'd have to do:

```scala
// If the logic of macro is a single method (very rare case):

import scala.reflect.macros.blackbox

def methodImpl[A: c.WeakTypeTag, B: c.WeakTypeTag](c: blackbox.Context)(value: c.Expr[A]): c.Expr[B] = {
  import c.universe._
  // code that would create c.Expr[B]
  ???
}

import scala.language.experimental.macros

// 1. You need `macro` keyword.
// 2. You do not pass arguments, only types (optionally).
// 3. Names of method arguments and their grouping in parameter lists should be the same:
//    in the `method` signature and `methodImpl`
//    (plus extra `(c: blackbox.Context)` or `(c: whitebox.Context)`).
def method[A, B](value: A): B = macro methodImpl[A, B]
```

```scala
// If the logic of macro is a whole class with multiple methpds (more probably),
// it's known as a "macro bundle":

import scala.reflect.macros.blackbox

// The sole argument of the constructor as to be `blackbox.Context` or `whitebox.Context.
class ClassWithMacroImpl(val c: blackbox.Context) {

  import c.universe._

  def methodImpl[A: c.WeakTypeTag, B: c.WeakTypeTag](value: c.Expr[A]): c.Expr[B] = {
    // code that would create c.Expr[B]
    ???
  }
}

import scala.language.experimental.macros

// 1. You need `macro` keyword.
// 2. Again, you do not pass arguments, only types (optionally). And there is no `new`.
// 3. Names of method arguments and their grouping in parameter lists should be the same:
//    in the `method` signature and `methodImpl`.
def method[A, B](value: A): B = macro ClassWithMacroImpl.methodImpl[A, B]
```

meanwhile on Scala 3 you'd do:

```scala
import scala.quoted.*

object MacroImpl {
  
  // This code CANNOT be placed in a class, inline def can only unquote something
  // that is top-level definition or in an object.
  def methodImpl[A: Type, B: Type](using q: Quotes)(argument: Expr[A]): Expr[B] = {
    import q.reflect.*
    // code that would create c.Expr[B]
    ???
  }
}

// 1. You need to use `inline def` (or transparent `inline def`)
// 2. You need to unquote something that is top-level definition or method of top-level object.
// 3. You need to quote arguments yourself, but it allows them to be renamed and reordered.
inline def method[A, B](value: A): B = ${ MacroImpl.methodImpl[A, B]('{ value }) }
```

It seems that even at this level APIs seem incompatible, and sharing any logic would be out of question.

But it is.

### Sharing implementation between Scala 2 and Scala 3

There is a paper [C. Hofer et al., _Polymorphic Embedding of DSLs_, GPCE, 2008](https://www.informatik.uni-marburg.de/~rendel/hofer08polymorphic.pdf) which describes how we could define _algebras_ (interfaces) and then operate on them, deferring their implementation to the mixins.

For our case, it could look like this, for the shared part:

```scala
// Abstract types and definitions.
// Because it uses path-dependent types, the types and methods can refer each other.
trait MacroTypesAndOperations {

  // abstract types
  type Type[A]
  type Expr[A]

  // abstract operations
  def prettyPrintType[A](tpe: Type[A]): String
  def prettyPrintExpr[A](expr: Expr[A]): String

  def stringToExpr(str: String): Expr[String]
}


// Shared logic of some macro implementation using abstract types and operations.
trait ActualMacroLogic { this: MacroTypesAndOperations =>

  def printTypeAndExpr[A: Type](expr: Expr[A]): Expr[String] = {
    val typeName = prettyPrintType(implicitly[Type[A]])
    val exprCode = prettyPrintExpr(expr)
    stringToExpr(s"$exprCode: $typeName")
  }
}
```

(It would go into `src/main/scala` directory).

How could we implement it on Scala 2? Like that:

```scala
// Implements abstract types and operations for Scala 2.
trait MacroTypesAndOperationsScala2 extends MacroTypesAndOperations {

  val c: blackbox.Context

  type Type[A] = c.WeakTypeTag[A]
  type Expr[A] = c.Expr[A]

  def prettyPrintType[A](tpe: Type[A]): String = tpe.toString
  def prettyPrintExpr[A](expr: Expr[A]): String = showCode(expr)

  def stringToExpr(str: String): Expr[String] = c.Expr(q"$str")
}

// Wire everything together for Scala 2:

class MacroImpl(val c: blackbox.Context)
  extends MacroTypesAndOperationsScala2
  with ActualMacroLogic {

  // Unfortunatelly, macros aren't smart enough to notice that Type=c.WeakTypeTag and Expr=c.Expr
  // so we have to help them:
  def printTypeAndExprImpl[A: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[String] = 
    printTypeAndExpr(expr)
}

def printTypeAndExpr[A](expr: A): String = macro MacroImpl.printTypeAndExprImpl[A]
```

(It would go into `src/main/scala-2` directory).

And on Scala 3? Like this:

```scala
// Implements abstract types and operations for Scala 3.
abstract class MacroTypesAndOperationsScala3(using q: Quotes) extends MacroTypesAndOperations {

  type Type[A] = scala.quoted.Type[A]
  type Expr[A] = scala.quoted.Expr[A]

  def prettyPrintType[A](tpe: Type[A]): String = tpe.show
  def prettyPrintExpr[A](expr: Expr[A]): String = expr.show

  def stringToExpr(str: String): Expr[String] = scala.quoted.Expr("str")
}

// Wire everything together for Scala 2:
  
class MacroImpl(q: Quotes)
  extends MacroTypesAndOperationsScala3(using q)
  with ActualMacroLogic

object MacroImpl {

  // Unfortunatelly, because this method has to be in an object, we need to manually
  // formard it to class construction and its method call.
  def printTypeAndExpr[A: Type](using q: Quotes)(expr: Expr[A]): Expr[String] =
    new MacroImpl(using q).printTypeAndExpr[A](expr)
}

inline def printTypeAndExpr[A](inline expr: A): String = ${ MacroImpl.printTypeAndExpr[A]('{ expr }) }
```

(It would go into `src/main/scala-3` directory).

While it (currently) looks like a lot of work:

 - if _someone else_ handle both the abstract API and its implementation then all overhead of this approach
   boils down to manual forwarding of methods on Scala 2 and Scala 3
 - in the future, we might be able to do something to decrease this overhead (macro annotations? compiler plugins?)
 - the more complicated the logic of the macro, that we would like to run on both Scala 2 and 3,
   the more shared code we would write, so that overhead quickly becomes negligible to the benefit of having
   virtually single codebase for all non-trivial macro logic.

So the whole codebase implements this approach:

 - `src/main/scala` contains the interfaces which should be implemented by both macro implementations
 - `src/main/scala-2` contains implementations for Scala 2 (and Scala 2-specific utilities)
 - `src/main/scala-3` contains implementations for Scala 3 (and Scala 3-specific utilities)

We should strive to implement tests for this logic in `src/test/scala` and doing as little as possible in `src/test/scala-2`
and `src/test/scala-3`, to ensure that behavior of both versions of Scala is aligned as much as possible.

### Cross-Quotes

Scala 2 has quasi-quotes - `String`-interpolation with a special handling by the compiler:

```scala
val expr: c.Expr[B] = ...
c.Expr[String](
  q"""
  ${ expr }.toString
  """
)
```

 - when we write quasi-quotes the compiler turns this `String` into an AST
 - interpolating in quasi-quotes tells the compiler to splice `c.Expr`/`c.universe.Tree` into the built AST

Scala 3 has proper quotes and splices:

```scala
val expr: Expr[B] = ...
'{
  ${ expr }.toString
}
```

These are rather incompatible, and would normally force us to define an abstract method for any expr-gluing,
and implementing it separately for Scala 2 and 3, which is why we developed experimental **cross-quotes**
(with macros on Scala 2 and a compiler plugin on Scala 3):

```scala
val expr: Expr[B] = ...
Expr.quote {
  Expr.splice { expr }.toString
}
```

This allows us to keep even more code shared.

### FP module

While not strictly required for writing macros it is very convenient to:

 * use some data type for storing derivation errors, and even better, to be able to aggregate them
 * be able to `.traverse` or `.parTraverse` while compossible callible computations
 * be able to build-up some logs within a macro, so that, when needed, we could investigate how expansion works for some use case
 * reuse our intuitions and experience from libraries like Cats or ZIO (if it's your cup of tea)
 * without having a whole Cats/ZIO ecosystem inside the macro's classpath (it invites some evictions and version conflicts during compilation!)

The FP module does just that: provides a few type classes, `NonEmpty` collections, and `MIO` - macrio IO that let you reuse your
experience with Cats IO while developing macros.

Usage of FP module is mostly optional, (and certainly the usage of `MIO`). You can develop your code using direct-style, exceptions,
plain `Either`s or your own structures if that's your preference.

## How to test changes?

The simplest way is to run tests.

HOWEVER, if you simply run `sbt test`, it will run tests on both Scala versions on all 2 platforms (JVM, JS, Native).
Which means that it will try to compile:

 - 2 * 3 * 4 (`hearth-fp`, `hearth`, `hearth-tests`)
 - + 3 (`hearth-cross-quotes` macros for Scala 2 for all platforms)
 - + 1 (`hearth-cross-quotes` compiler plugin for Scala 3)
 - = 28 modules

Even on my computer it usually OOMs the build tool when Scala.js/Scala Naive starts linking.

But (currently) all (in future, majority) of tests live in `hearthTests` module.
So `hearthTests/test ; hearthTests3/test` (or `quick-test`) should be enough.

> If working on macros, sometimes even though you modified the code macro is not recompiled.
> Use `quick-clean` to clean tests and force recompilation.

### Writing tests

Since testing macros is a pain - you have to create a dedicated macro "endpoint" every time you need to test something - we created
`hearth.testdata.Data`.

It's a JSON-like structure, it can be turned from `Data` to `Expr[Data]` in the macro, and it has a nice `Diff`. So, we can use it
to:

 - define a macro which takes some parameters (telling it what we want to test)
 - gather as much properties for the shared set of parameters as possible (well, more as much as it makes sense,
   when they are thematically related)
 - structure these properties as a `Data` value
 - turn it to `Expr[Data]` and return from a macro
 - within unit test compare the `macroResult <==> expectedDiff`

We have a dedicated `MacroSuite` which defines a few convenient utilities:

 * `actual ==> expected` - compares 2 values for equality, if they are not equal it fails (borrowed from uTest)
 * `actual <==> expected` - compare 2 Strings or 2 Diffs for equality, if they are not equal it fails *8but with diff** printed

It also allows grouping tests together:

```scala
group("name of the group") {

  group("sub-group") {

    test("what is being tested") {

      // actual test
    }
  }
}
```

which is nice for organizing tests by method name or functionality.

MUnit has some built-in utilities for catching compilation errors:

```scala
compileErrors(
  """
  code that should not compile
  """
)
```

it is occasionally useful to test the compilation errors. `MacroSuite` defines some utilities to work with these as well:

```scala
compileErrors(
  """
  code that should not compile
  """
).check(
  "Lines of test",
  "That should appear in the error message",
  "In the same order",
  "But some lines (from the error) can be skipped, as long as all expected are there"
)

compileErrors(
  """
  code that should not compile
  """
).checkNot(
  "Message that should NOT appear in the error"
)

compileErrors(
  """
  code that should not compile
  """
).arePresent() // We cannot easily write the expected message, or it is platform-specific or sth.
```

## How to debug macros?

If you need to figure out what code is returned from macro, try this utility:

```scala
import hearth.debug.Debug

Debug.withFinalCodeInIDE {

  someCodeWithMacroExpansion
}
```

It should show you the code returned from the macro:

 - the the compilation output
 - as a hint overlay in VS Code (blue squiggly lines, hover over them and you'll see the output)
 - as a hint overlay in Scastie (likewise)

If the code you wish to preview is implicit, try:

```scala
import hearth.debug.Debug

Debug.withGivenCodeInIDE[TypeClass[A]]
```

If you are writing a new macro support, and you need to figure out the AST to create,
the AST variant of the 2 above is available as:

```scala
import hearth.debug.Debug

Debug.withFinalASTInIDE {

  someCodeWithMacroExpansion
}

Debug.withGivenASTInIDE[TypeClass[A]]
```

These you can use from outside of macro. If you are working on some utility and want to print only
the current part, you can write inside a macro:

```scala
if (Environment.isExpandedAt("MyMacroSpec.scala:40")) {

  // Notice! Only the first reportInfo and the first reportWarn
  // would be shown by the compiler! All following calls becomes no-ops!
  Environment.reportInfo("something I need to print")

  // If you need to log something else as well (e.g. logs, duh), you can use
  println("something I need to print")
  // but this will only be visible in the console, and ONLY if you are not using some
  // server-based build (arbitrary `println`s from the compiler are not forwarded).
}
```

This would print only if the currently expanded macro is defined in `MyMacroSpec.scala` at line `40`.

If there is more than 1 macro at that line, you can also try e.g. `"MyMacroSpec.scala:40:20"`
to only expand if it's a macro on line `40` starting at column `20`.

If you report from macro or compilation fails, then the log message would contain that `file.scala:line:column` location.

Be careful!

```scala
  Foo.bar // <- macro expansion
```

would have a column:

 - `6` on Scala 2 (macro is after the `.`)
 - `2` on Scala 2 (`Foo.` would also be counted towards current position)

so if you are filtering by column you have to adapt the value after changing which version of Scala you work on currently!
