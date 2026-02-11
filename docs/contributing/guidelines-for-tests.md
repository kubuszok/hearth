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

 - `scala-newest` directories are used for tests that should only run on the **newest** supported Scala versions
   (currently 2.13.18 and 3.8.1 for regression testing). These are used for:

     - **Demo/example code** that showcases advanced features or complex macro implementations (e.g., `FastShowPrettySpec`)
     - **Features that require newer Scala versions** not available in the primary versions (2.13.16 and 3.3.7)
     - **Regression tests** for newer compiler versions

   Directory variants:
     - `hearthTests/src/main/scala-newest` - shared code for newest Scala versions (both 2.13.18 and 3.8.1)
     - `hearthTests/src/main/scala-newest-2` - Scala 2.13.18-specific code
     - `hearthTests/src/main/scala-newest-3` - Scala 3.8.1-specific code
     - `hearthTests/src/test/scala-newest` - shared tests for newest Scala versions
     - `hearthTests/src/test/scala-newest-2` - Scala 2.13.18-specific tests
     - `hearthTests/src/test/scala-newest-3` - Scala 3.8.1-specific tests

   **Important:** These directories are **only included** when the `NEWEST_SCALA_TESTS=true` environment variable is set.
   When this variable is set:
     - `hearthTests` (Scala 2.13) compiles with Scala 2.13.18 instead of 2.13.16
     - `hearthTests3` (Scala 3) compiles with Scala 3.8.1 instead of 3.3.7
     - The `scala-newest` directories are added to the source directories of the same project

   Without `NEWEST_SCALA_TESTS=true`, these directories are completely ignored and the projects use the primary versions.

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

### When to use scala-newest directories

**Use `scala-newest` when:**
- Creating demo/example code that showcases complex macro features (like `FastShowPrettySpec`)
- Testing features that require compiler capabilities only available in newer Scala versions
- Writing regression tests specifically for newer compiler versions (2.13.18 or 3.8.1)
- The code would benefit from newer language features but should not block development on primary versions

**Do NOT use `scala-newest` when:**
- Writing standard library tests (use `src/test/scala` instead)
- The code can work on primary versions (2.13.16 and 3.3.7)
- Adding core functionality tests (these should work on primary versions)
- The test is critical for CI/CD pipeline validation (primary versions are tested in CI)

**Example:**
```scala
// hearth-tests/src/test/scala-newest/hearth/demo/allfeatures/FastShowPrettySpec.scala
// This is a demo/example showcasing FastShowPretty derivation
// It uses scala-newest because it's a demonstration, not core library testing

final class FastShowPrettySpec extends MacroSuite {
  test("value types") {
    val result = FastShowPretty.render(ExampleValueClass(42), RenderConfig.Default)
    assertEquals(result, "42")
  }
}
```

## Best Practices for Testing Macros

Testing macros is challenging because they cannot be tested in the same compilation unit where they are defined. Hearth provides the **hearth-munit** module with specialized testing utilities to make macro testing easier and more maintainable.

### Using hearth-munit Test Suites

The **hearth-munit** module provides three main test suite types:

#### hearth.Suite

The base test suite providing:
- **Test grouping** with `group()` method for organizing tests hierarchically
- **Simple assertions** with `==>` operator for any type equality
- **Specialized assertions** with `<==>` operator for `String` and `hearth.data.Data` with diff output

```scala
import hearth.Suite

final class MyBasicSpec extends Suite {
  group("feature name") {
    test("specific behavior") {
      val result = computeSomething()
      result ==> expectedValue
    }
  }
}
```

#### hearth.MacroSuite

Extends `Suite` with compile-time error checking utilities:
- **Check error presence**: `compileErrors("code").check("expected text in error")`
- **Check error absence**: `compileErrors("code").checkNot("text that should not appear")`
- **Verify errors exist**: `compileErrors("code").arePresent()`

```scala
import hearth.MacroSuite

final class MyMacroSpec extends MacroSuite {
  group("error messages") {
    test("unsupported type produces helpful error") {
      compileErrors("""
        FastShowPretty.render(new Thread(), RenderConfig.Default)
      """).check(
        "Cannot derive FastShowPretty",
        "for type: java.lang.Thread"
      )
    }
  }
}
```

#### hearth.ScalaCheckSuite

Combines `Suite` with ScalaCheck for property-based testing:
- Provides standard `Arbitrary` instances
- Includes Hearth-specific `Arbitrary` instances for `hearth.data.Data` structures

```scala
import hearth.ScalaCheckSuite
import org.scalacheck.Prop.*

final class PropertySpec extends ScalaCheckSuite {
  property("roundtrip serialization") {
    forAll { (data: hearth.data.Data) =>
      val serialized = serialize(data)
      val deserialized = deserialize(serialized)
      deserialized == data
    }
  }
}
```

### Grouping Tests

Use the `group()` method to organize tests hierarchically by functionality, method name, or feature area:

```scala
final class ComprehensiveSpec extends MacroSuite {
  group("Type handling") {
    group("primitives") {
      test("Int") { /* ... */ }
      test("String") { /* ... */ }
    }

    group("collections") {
      test("List") { /* ... */ }
      test("Map") { /* ... */ }
    }
  }

  group("Error messages") {
    test("unsupported type") { /* ... */ }
    test("recursive type") { /* ... */ }
  }
}
```

Groups can be nested arbitrarily deep, and test names will reflect the full hierarchy in output.

### Assertion Operators: ==> vs <==>

#### The ==> Operator (Simple Equality)

Use `==>` for standard equality assertions on any type:

```scala
test("basic equality") {
  val result = 2 + 2
  result ==> 4

  val list = List(1, 2, 3)
  list.length ==> 3
}
```

**Characteristics:**
- Works with any type that has an `==` comparison
- On failure, shows: `expected: X but got: Y`
- Borrowed from uTest for familiarity
- Best for simple values, numbers, booleans, small collections

#### The <==> Operator (Diff-Based Equality)

Use `<==>` for `String` and `hearth.data.Data` comparisons with detailed diff output:

```scala
test("string comparison with diff") {
  val actual = "Hello\nWorld\nFoo"
  val expected = "Hello\nWorld\nBar"
  actual <==> expected
  // On failure, shows a line-by-line diff highlighting the difference
}

test("Data comparison with diff") {
  val result: Data = macroResult
  result <==> Data.obj(
    "name" -> Data.str("example"),
    "count" -> Data.num(42)
  )
  // On failure, shows structured diff of the Data tree
}
```

**Characteristics:**
- Only works with `String` and `hearth.data.Data`
- On failure, shows a detailed diff highlighting exact differences
- Essential for comparing multi-line strings or complex Data structures
- Makes debugging macro output much easier

**When to use which:**
- Use `==>` for: primitives, simple objects, small collections, boolean conditions
- Use `<==>` for: macro-generated strings, error messages, Data structures from fixtures

### Testing Multiple Properties with Data

The **hearth.data.Data** structure is a JSON-like format that allows testing multiple related properties in a single macro expansion:

**Why use Data?**
- Macros cannot be tested in the same compilation unit where they're defined
- Creating separate macros for each property is tedious and slow to compile
- Data lets you gather multiple test assertions from one macro invocation

#### Installation

To use **hearth-munit** in your tests:

!!! example "[sbt](https://www.scala-sbt.org/)"

    JVM only:

    ```scala
    libraryDependencies += "com.kubuszok" %% "hearth-munit" % "{{ hearth_version() }}" % Test
    ```

    JVM/Scala.js/Scala Native via [sbt-crossproject](https://github.com/portable-scala/sbt-crossproject), [sbt-projectmatrix](https://github.com/sbt/sbt-projectmatrix) or sbt 2:

    ```scala
    libraryDependencies += "com.kubuszok" %%% "hearth-munit" % "{{ hearth_version() }}" % Test
    ```

!!! example "[Scala CLI](https://scala-cli.virtuslab.org/)"

    JVM only:

    ```scala
    //> using test.dep "com.kubuszok::hearth-munit:{{ hearth_version() }}"
    ```

    JVM/Scala.js/Scala Native:

    ```scala
    //> using test.dep "com.kubuszok::hearth-munit::{{ hearth_version() }}"
    ```

!!! warning

    **hearth-munit** depends on the [core Hearth library](../user-guide/index.md#installation), so if you add hearth-munit, you'll automatically get hearth as well.

#### Pattern

1. **Create a fixture macro** that returns `Expr[Data]`:

```scala title="MyFeatureFixtureImpl.scala"
//> using scala 3.3.7
//> using dep "com.kubuszok::hearth:{{ hearth_version() }}"

package myfeature

import hearth.*
import hearth.data.Data
import hearth.std.*
import scala.quoted.*

object MyFeatureFixtureImpl {
  def testTypeInfo[A](using q: Quotes, tpe: Type[A]): Expr[Data] = {
    val mc = new MacroCommonsScala3(using q) {}
    import mc.*

    // Strip ANSI color codes from prettyPrint output for cleaner test comparisons
    val typeName = Type[A].prettyPrint.replaceAll("\u001b\\[([0-9]+)m", "")

    Expr(Data.map(
      "typeName" -> Data(typeName),
      "isSealed" -> Data(Type[A].isSealed)
    ))
  }
}
```

2. **Expose as a macro** in Scala 2/3 adapters:

```scala title="MyFeatureFixture.scala (Scala 2.13)"
// hearth-tests/src/main/scala-2/myfeature/MyFeatureFixture.scala
package myfeature

import hearth.data.Data
import scala.language.experimental.macros

object MyFeatureFixture {
  def testTypeInfo[A]: Data = macro MyFeatureFixtureImpl.testTypeInfo[A]
}
```

```scala title="MyFeatureFixture.scala (Scala 3)"
//> using scala 3.3.7
//> using dep "com.kubuszok::hearth:{{ hearth_version() }}"

package myfeature

import hearth.data.Data

object MyFeatureFixture {
  inline def testTypeInfo[A]: Data = ${ MyFeatureFixtureImpl.testTypeInfo[A] }
}
```

3. **Test multiple properties** in your spec:

```scala title="MyFeatureSpec.scala"
//> using scala 3.3.7
//> using test.dep "com.kubuszok::hearth-munit:{{ hearth_version() }}"
//> using file "MyFeatureFixtureImpl.scala"
//> using file "MyFeatureFixture.scala"

package myfeature

import hearth.MacroSuite
import hearth.data.Data

final class MyFeatureSpec extends MacroSuite {
  test("Option[Int] type info") {
    MyFeatureFixture.testTypeInfo[Option[Int]] <==> Data.map(
      "typeName" -> Data("scala.Option[scala.Int]"),
      "isSealed" -> Data(true)
    )
  }
}
```

**Benefits:**
- Single macro expansion tests multiple related properties
- `<==>` operator provides clear diff when any property fails
- Easy to add new properties without creating new macros
- Scales well for complex macro behavior validation

!!! tip "Running the example"

    Save all three files in the same directory and run with Scala CLI:

    ```bash
    scala-cli test .
    ```

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

### Running tests in scala-newest directories

Tests in `scala-newest` directories are **only** compiled and run when the `NEWEST_SCALA_TESTS=true` environment
variable is set. This variable switches the `hearthTests` and `hearthTests3` projects to use newer Scala versions
(2.13.18 and 3.8.1) and includes the `scala-newest` source directories.

**For Scala 3.8.1 (scala-newest-3):**
```bash
NEWEST_SCALA_TESTS=true sbt --client "hearthTests3/clean"
NEWEST_SCALA_TESTS=true sbt --client "hearthTests3/compile"
NEWEST_SCALA_TESTS=true sbt --client "hearthTests3/test"
# Or run a specific test:
NEWEST_SCALA_TESTS=true sbt --client "hearthTests3/testOnly hearth.demo.allfeatures.FastShowPrettySpec"
```

**For Scala 2.13.18 (scala-newest-2):**
```bash
NEWEST_SCALA_TESTS=true sbt --client "hearthTests/clean"
NEWEST_SCALA_TESTS=true sbt --client "hearthTests/compile"
NEWEST_SCALA_TESTS=true sbt --client "hearthTests/test"
# Or run a specific test:
NEWEST_SCALA_TESTS=true sbt --client "hearthTests/testOnly hearth.demo.allfeatures.FastShowPrettySpec"
```

**Using quick-test with newest versions:**

The `quick-test` command with `NEWEST_SCALA_TESTS=true` runs tests using the newest Scala versions:
```bash
NEWEST_SCALA_TESTS=true sbt --client "quick-clean"
NEWEST_SCALA_TESTS=true sbt --client "quick-test"
```

This will test against:
- Scala 2.13.18 (hearthTests upgraded from 2.13.16)
- Scala 3.8.1 (hearthTests3 upgraded from 3.3.7)
- Cross-version sandwich tests (with newest versions)

**Without the environment variable:**

Running the same commands **without** `NEWEST_SCALA_TESTS=true` will:
- Use primary versions (2.13.16 and 3.3.7)
- Ignore all `scala-newest` directories completely
- This is the normal development mode

**Important notes:**
- `scala-newest` directories are **only visible** when `NEWEST_SCALA_TESTS=true` is set
- The environment variable doesn't create separate projects; it modifies the existing `hearthTests` and `hearthTests3` projects
- When working on code in `scala-newest` directories, always clean the module before testing
- The newest versions (2.13.18 and 3.8.1) are used for regression testing, not for primary development
- Primary development versions are 2.13.16 and 3.3.7
- **CI runs tests BOTH ways:** with `NEWEST_SCALA_TESTS=false` (primary versions) AND `NEWEST_SCALA_TESTS=true` (newest versions)
  to ensure code works on both primary and newest Scala versions

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

## Adding a test for a bug fix

When reproducing a bug or verifying a fix, you need tests that demonstrate the problem and the solution.
For the complete bug-fix workflow including clean/compile/test cycles, see
[instruction for fixing a bug](instruction-for-fixing-a-bug.md).

### Choosing between extending a fixture and creating a new one

**Prefer extending an existing fixture** when the bug relates to functionality that already has test coverage.
For example:

 - Bug in `Expr.quote` behavior → extend `CrossExprsFixturesImpl.scala`
 - Bug in type handling → extend `CrossTypesFixturesImpl.scala` or `TypeFixturesImpl.scala`
 - Bug in class inspection → extend `ClassesFixturesImpl.scala`

**Create a new fixture** only when the bug exercises a fundamentally different code path that existing fixtures
do not cover. If unsure, ask.

### Testing compilation failures

If the bug manifests as code that should compile but doesn't (or shouldn't compile but does),
use `compileErrors`:

```scala
// Verify that buggy code triggers a compilation error
compileErrors(
  """
  // code that triggers the bug
  """
).arePresent()

// Verify a specific error message
compileErrors(
  """
  // code that triggers the bug
  """
).check(
  "expected error text"
)

// Verify that a specific misleading error does NOT appear
compileErrors(
  """
  // code that triggers the bug
  """
).checkNot(
  "misleading error that should not appear"
)
```

### Testing incorrect macro output

If the bug manifests as a macro producing wrong results, add a test case to an existing fixture
and assert the expected output:

```scala
SomeFixtures.testSomething(...) <==> Data.map(
  "key" -> Data("expected value")
)
```

### Cleaning after test changes

After adding or modifying fixtures, **always clean the module chain** before running tests.
Incremental compilation does NOT reliably re-expand macros when fixture implementations change.

At minimum, run `quick-clean` (cleans `hearthTests`, `hearthTests3`, `hearthSandwichTests`, `hearthSandwichTests3`)
before `quick-test`. For a full clean table by module, see the [bug fix instruction](instruction-for-fixing-a-bug.md).
