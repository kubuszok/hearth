# Guidelines for tests

## Packages and test names

Tests use the following naming structure:

 - Cross-Quotes implementations from `hearth-cross-quotes`:

    - `Expr.quote` and `Expr.splice` -> `hearth.crossquotes.CrossExprsSpec`
    - `Type.of` and `Type.CtorN.of` -> `hearth.crossquotes.CrossTypesSpec`

 - `hearth.untyped` are currently not tested - we rely on `hearth.typed` to check all the relevant implementations
 - the remaining code is tested in paired `*Spec` classes, e.g.:

    - `hearth.Environment` utilities -> `hearth.EnvironmentSpec`
    - `hearth.data.Data` utilities -> `hearth.data.DataSpec`
    - `hearth.typed.Classes` utilities -> `hearth.typed.ClassesSpec`
    - `hearth.typed.Exprs` utilities -> `hearth.typed.ExprsSpec`
    - `hearth.typed.Methods` utilities -> `hearth.typed.MethodsSpec`
    - `hearth.typed.Types` utilities -> `hearth.typed.TypesSpec`
    - `DestructuredExpr` utilities -> `hearth.typed.DestructuredExprsSpec` (cross-platform) and `hearth.typed.DestructuredExprsScala3Spec` (context functions)
    - `ExprCodec.derived` utilities -> `hearth.typed.ExprCodecSpec` (cross-platform round-trip tests)
    - Annotation type matching and destructuring -> `hearth.typed.MethodsSpec` (`testAnnotationDestructuring` fixture)

 - `hearth.typed.MethodsSpec` has several fixture functions for different aspects of the Method API:

    - `testMethodsExtraction[A]` — renders 25+ properties per method (invocation, predicates, visibility, accessors)
    - `testMethodExpectations[A](name)` — tests builder chain expectations, `knownReturning`, `toString` rendering
    - `testConstructorExpectations[A]` — same for constructors
    - `testMethodPrettyPrint[A](name)` — compares `plainPrint`, `prettyPrint` (ANSI), and ANSI-stripped output
    - `testCallInstanceViaFold[A](instance)(name)(params*)` — calls a method via `method.fold` and returns the result
    - `testCallConstructorViaFold[A](params*)` — constructs an instance via `fold`
    - `testMethodDefaults[A](name)` — tests parameter default values
    - `testParameterProperties[A](name)` — tests individual parameter properties

## Directory structure

By default, tests should be placed in `hearthTests/src/test/scala`, so that:

 - the code is shared between Scala 2.13 and 3
 - the code is shared between JVM, JS and Native
 - the code is shared between the lowest supported Scala and the newest supported Scala

however there are exceptions:

 - the library is published for the LTS versions (2.13.16 and 3.3.9), but `hearth-tests` is also compiled by newer
   compilers, in separate, always visible subprojects (tiers) that link against the LTS-compiled library:

     | Tier     | Scala versions  | Projects                                                     |
     |----------|-----------------|--------------------------------------------------------------|
     | LTS      | 2.13.16 + 3.3.9 | `hearthTests2`, `hearthTests` (all platforms)                 |
     | Next-LTS | 2.13.18 + 3.9.0 | `hearthTests2NextLts`, `hearthTestsNextLts` (JVM only)        |
     | Next     | 3.10.1          | `hearthTestsNext` (JVM only, no Scala 2.13)                   |

   Which tiers compile a source directory:

     | Directory                                                | LTS | Next-LTS | Next | Use for                                                     |
     |----------------------------------------------------------|:---:|:--------:|:----:|-------------------------------------------------------------|
     | `scala/`, `scala-2/`, `scala-3/`                         | ✓   | ✓        | ✓    | everything that compiles on every version (the default)     |
     | `scala-lts/`, `scala-lts-2/`, `scala-lts-3/`             | ✓   | ✓        | —    | syntax that 3.10+ deprecated or removed                     |
     | `scala-next-lts/`, `scala-next-lts-2/`, `scala-next-lts-3/` | —   | ✓        | —    | features missing from the LTS versions (2.13.17+, 3.4–3.9)  |
     | `scala-next/`, `scala-next-3/`                           | —   | —        | ✓    | features of 3.10+                                           |

   When a 3.10 deprecation forces code out of `scala-3/`, move the old-syntax version to `scala-lts-3/` and put the
   new-syntax version in `scala-next-3/`.

 - `hearth.fp` uses JVM-specific utilities which CAN be used in JS and Native macros but CANNOT be linked by JS and Native code generators,
   therefore they can only be tested on JVM, and they are placed in `hearthTests/src/test/scalajvm`
 - `hearth.std.RulesSpec` tests `Rules` which use `hearth.fp`, so they have to be placed in `hearthTests/src/test/scalajvm` as well
 - shared tests, can only test with types which are guaranteed to exist on: Scala JVM, JS and Native. When testing against JVM-specific
   types, we need to create an additional `JvmSpec` and place it in `hearthTests/src/test/scalajvm`, e.g.:

     - `hearth.std.StdExtensionsSpec` (`test/scala`) has complementary `hearth.std.StdExtensionsJvmSpec` (`test/scalajvm`)
     - `hearth.typed.ClassesSpec` (`test/scala`) has complementary `hearth.typed.ClassesJvmSpec` (`test/scalajvm`)
     - `hearth.typed.MethodsSpec` (`test/scala`) has complementary `hearth.typed.MethodsJvmSpec` (`test/scalajvm`)
     - `hearth.typed.TypesSpec` (`test/scala`) has complementary `hearth.typed.TypesJvmSpec` (`test/scalajvm`)

 - similarly, types that exist only on Scala 3 can be tested with additional `Scala3Spec` files placed in `hearthTests/src/test/scala-3`, e.g.:

     - `hearth.std.StdExtensionsSpec` (`test/scala`) has complementary `hearth.std.StdExtensionsScala3Spec` (`test/scala-3`)
     - `hearth.typed.ClassesSpec` (`test/scala`) has complementary `hearth.typed.ClassesScala3Spec` (`test/scala-3`)
     - `hearth.typed.TypesSpec` (`test/scala`) has complementary `hearth.typed.TypesScala3Spec` (`test/scala-3`)

 - for "sandwich" tests (whether it's possible to do `Scala 2.13 module -> Scala 3 module -> Scala 2.13 module` or `Scala 3 module -> Scala 2.13 module -> Scala 3 module`,
   allowing for Scala 3 code expanding macros that use Scala 2.13-compiled types, or Scala 2.13 code expanding macros that use Scala 3-compiled types)
   we are using a different module, placing tests in `hearthSandwichTests/src/test/scala`

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
  result <==> Data.map(
    "name" -> Data("example"),
    "count" -> Data(42)
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

    **hearth-munit** depends on the [core Hearth library](../user-guide/basic-utilities.md#installation), so if you add hearth-munit, you'll automatically get hearth as well.

#### Pattern

1. **Create a fixture macro** that returns `Expr[Data]`:

```scala title="MyFeatureFixtureImpl.scala"
//> using scala {{ scala.3 }}
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
//> using scala {{ scala.3 }}
//> using dep "com.kubuszok::hearth:{{ hearth_version() }}"

package myfeature

import hearth.data.Data

object MyFeatureFixture {
  inline def testTypeInfo[A]: Data = ${ MyFeatureFixtureImpl.testTypeInfo[A] }
}
```

3. **Test multiple properties** in your spec:

```scala title="MyFeatureSpec.scala"
//> using scala {{ scala.3 }}
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
 - which cannot be defined in the same compilation unit as the macro expansion
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

it is occasionally useful to test compilation errors. `MacroSuite` defines some utilities to work with these as well:

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

HOWEVER, if you simply run `sbt --client "test"`, it will run tests on both Scala versions on all 3 platforms (JVM, JS, Native).
That means compiling a large project matrix, and it usually crashes the build tool with `OutOfMemoryError`
when Scala.js/Scala Native starts linking all these versions at once.

But all of tests live in `hearthTests` and `hearthSandwichTests` modules.
So running `hearthTests/test ; hearthTests2/test ; hearthSandwichTests/test ; hearthSandwichTests2/test`
(the `quick-test` alias) should be enough.

> If working on macros, sometimes even though you modified the code, the macro is not recompiled.
> Use `hearth/clean ; hearth2/clean ; quick-clean` to clean tests and force recompilation before another run.

### Running the regression tiers

The Next-LTS and Next tiers are ordinary subprojects, so no environment variables are needed:

```bash
sbt --client "quick-clean-next-lts ; quick-test-next-lts" # 2.13.18 and 3.9.0
sbt --client "quick-clean-next ; quick-test-next"         # 3.10.1
# or a single suite:
sbt --client "hearthTestsNextLts/testOnly hearth.demo.allfeatures.FastShowPrettySpec"
```

CI runs every tier, so a change is only done when all of them pass.

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
- This test would most likely need a new fixture or an update to existing ones - they are defined in implementations on `main` source files
  and matched with adapters in Scala 2 and Scala 3 `main` source files
- It may or may not need new test classes (probably placed in `main` source files of `hearth-tests` but ask!)
- Fixtures are what actually call macro utilities defined in the `hearth` module

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
 - Bug in type handling → extend `CrossTypesFixturesImpl.scala` or `TypesFixturesImpl.scala`
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

At minimum, run `quick-clean` (cleans `hearthTests`, `hearthTests2`, `hearthSandwichTests`, `hearthSandwichTests2`)
before `quick-test`. For a full clean table by module, see the [bug fix instruction](instruction-for-fixing-a-bug.md).
