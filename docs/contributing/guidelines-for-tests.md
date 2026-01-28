# Guidelines for tests

## Packages and test names

Tests use the following naming structure:

 - Cross-Quotes implementations from `hearth-cross-quotes`:

    - `Expr.quote` and `Expr.splice` -> `hearth.crossquotes.CrossExprsSpec`
    - `Type.of` and `Type.CtorN.of` -> `hearth.crossquotes.CrossTypesSpec`

 - `hearth.untyped` are currently not tested - we rely on `hearth.typed` to check all the relevant implementations
 - the remaining code is tested in paired `*Spec` classes, e.g.:

    - `hearth.Environment` utilities -> `hearth.Environment`
    - `hearth.data.Data` utilities -> `hearth.data.DataSpec`
    - `hearth.typed.Classes` utilities -> `hearth.typed.ClassesSpec`
    - `hearth.typed.Exprs` utilities -> `hearth.typed.ExprsSpec`
    - `hearth.typed.Methods` utilities -> `hearth.typed.MethodsSpec`
    - `hearth.typed.Types` utilities -> `hearth.typed.TypesSpec`

## Directory structure

By default, test should be placed in `hearthTests/src/test/scala`, so that:

 - the code is shared between Scala 2.13 and 3
 - the code is shared between JVM, JS and Native
 - the code is shared between the lowest supported Scala and the newest supported Scala

however there are exceptions:

 - `hearth.fp` uses JVM-specific utilities which CAN be used in JS and Native macros but CANNOT be linked by JS and Native code generators,
   therefore they can only be tested on JVM, and they are placed in `hearthTests/src/test/scalajvm`
 - `hearth.std.RulesSpec` tests `Rules` which use `hearth.fp`, so they have to be placed in `hearthTests/src/test/scalajvm` as well
 - shared tests, can only test with types which are guaranteed to exist on: Scala JVM, JS and Native. When testing against JVM-specific
   types, we need to create an additional `JvmSpec` and place it in `hearthTests/src/test/scalajvm`, e.g.:

     - `hearth.std.StdExtensionsSpec` (`test/scala`) has complimentary `hearth.std.StdExtensionsJvmSpec` (`test/scalajvm`)
     - `hearth.typed.ClassesSpec` (`test/scala`) has complimentary `hearth.typed.ClassesJvmSpec` (`test/scalajvm`)
     - `hearth.typed.MethodsSpec` (`test/scala`) has complimentary `hearth.typed.MethodsJvmSpec` (`test/scalajvm`)
     - `hearth.typed.TypesSpec` (`test/scala`) has complimentary `hearth.typed.TypesJvmSpec` (`test/scalajvm`)

 - similarly types that exists only on Scala 3, can be tested with additional `Scala3Spec` placed in `hearthTests/src/test/scala-3`, e.g.:

     - `hearth.std.StdExtensionsSpec` (`test/scala`) has complimentary `hearth.std.StdExtensionsScala3Spec` (`test/scala-3`)
     - `hearth.typed.ClassesSpec` (`test/scala`) has complimentary `hearth.typed.ClassesScala3Spec` (`test/scala-3`)
     - `hearth.typed.TypesSpec` (`test/scala`) has complimentary `hearth.typed.TypesScala3Spec` (`test/scala-3`)

 - for "sandwich" tests (whether it's possible to do `Scala 2.13 module -> Scala 3 module -> Scala 2.13 module` or `Scala 2.13 module -> Scala 3 module -> Scala 2.13 module`,
   allowing for Scala 3 code expanding macros that use Scala 2.13-compiled types, or Scala 2.13 code expanding macros that use Scala 3-compiled types)
   we are using a different module, placing tests in `hearthSandwichTests/src/test/scala`


## Fixtures

Majority of tested code are macro-utilities. They:

 - cannot be tested outside a macro
 - which cannot be define in the same compilation unit as the macro expansion
 - require a separate macro expansion each time we would like to provide it with a different input
 - has to be tested across multiple Scala versions and platforms

For that we are using the following pattern:

 - `TestedUtilityFixtureImpl.scala` file in the right package within `hearthTests/src/main/scala`:

   - contains multiple cross-compilable macros using tested utilities
   - using `hearth.data.Data` structure to expose multiple results in a single macro expansion

 - `TestedUtilityFixture.scala` file in the right package within `hearthTests/src/main/scala-2`:

   - contains adapters exposing cross-compilable code as Scala 2 macros

 - `TestedUtilityFixture.scala` file in the right package within `hearthTests/src/main/scala-3`:

   - contains adapters exposing cross-compilable code as Scala 3 macros

## Tests suites

`hearth.Suite` provides:

 - a way of grouping tests
 - checking equality check assertions with: `actual ==> expected`
 - specialized version of equality check assertion: `actual <==> expected` for `String`s and `hearth.data.Data`
   providing a diff which helps identify the issue if it occurs

`hearth.MacroSuite` is a specialization of the above which provides additionally:

 - a way of checking the macro errors assertions for error presence: `compileErrors("code").check("text that should appear")`
 - a way of checking the macro errors assertions for error absence: `compileErrors("code").checkNot("text that should not appear")`
 - a way of checking if there were any errors present: `compileErrors("code").arePresent()`

## Writing tests

Since testing macros is a pain - you have to create a dedicated macro "endpoint" every time you need to test something - we created
`hearth.data.Data`.

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
 * `actual <==> expected` - compare 2 Strings or 2 Diffs for equality, if they are not equal it fails (but with diff printed)

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

## Running tests

The simplest way to verify if macro is working correctly is to run tests.

HOWEVER, if you simply run `sbt test`, it will run tests on both Scala versions on all 2 platforms (JVM, JS, Native).
Which means that it will try to compile:

 - 2 * 3 * 4 (`hearth-fp`, `hearth`, `hearth-tests`)
 - + 3 (`hearth-cross-quotes` macros for Scala 2 for all platforms)
 - + 1 (`hearth-cross-quotes` compiler plugin for Scala 3)
 - = 28 modules

It usually crashes the build tool with `OutOfMemoryError` when Scala.js/Scala Native starts linking all these versions at once.

But all of tests live in `hearthTests` and `hearthSandwichTests` modules.
So running `hearthTests/test ; hearthTests3/test ; hearthSandwichTests/test ; hearthSandwichTests3/test`
(or `quick-test` if you are human) should be enough.

> If working on macros, sometimes even though you modified the code, the macro is not recompiled.
> Use `hearth/clean ; hearth3/clean ; hearthTests/clean ; hearthTests3/clean`
> (or `quick-clean` if you are human) to clean tests and force recompilation before another run.

## Critical Dependency Chain for Testing

**IMPORTANT:** `hearth-better-printers` is used by `hearth-cross-quotes` on Scala 2 to generate quasiquotes.

This means:
- **Fixes to `cross-quotes` might require fixes to `better-printers`**
- **Testing changes to either module requires compiling the full dependency chain:**
  1. Compile `hearth-better-printers`
  2. Compile `hearth-cross-quotes` (depends on better-printers)
  3. Compile `hearth` (depends on both modules)
  4. Compile and run `hearth-tests` (ensures changes are actually safe)

**When working on `cross-quotes` or `better-printers`:**
- Be aware that changes in `better-printers` will affect `cross-quotes` code generation on Scala 2
- Always test the complete chain before considering the work done
- Use MCP server to compile each module in order
- Run `hearth-tests` as the final verification step

**If a new behavior needs to be tested:**
- It would need a test in `hearth-tests` (depending on the use case: in shared, Scala 2-only, Scala 3-only, or JVM-only part)
- This test would most likely need an new fixture or an update to existing ones - they are defined in implementations on `main` source files
  and matched with adapters in Scala 2 and Scala 3 `main` source files
- It may or may not need new test classes (probably placed in `main` source files of `hearth-tests` but ask!)
- Fixtures is what actually calls macro utilities defined in `hearth` module

**Example workflow:**
```
Agent: I've updated better-printers to improve AST printing. Since cross-quotes uses
       better-printers for Scala 2 quasiquote generation, I need to verify the full chain:
       1. Compiling hearth-better-printers...
       2. Compiling hearth-cross-quotes...
       3. Compiling hearth...
       4. Running hearth-tests to verify safety...
```
