# Instruction for fixing a bug (end-to-end workflow)

This instruction covers the complete cycle of reproducing a bug with a failing test, applying a fix, and verifying the fix works.

**Key principle:** A "failing test" may mean either a runtime assertion failure OR code that does not compile.
Use `compileErrors(...)` to test compilation failures, or standard assertions (`<==>`) for incorrect behavior.

## Step 1 — Understand the bug

1. **Read the GitHub issue** carefully, including all comments — the root cause is sometimes different from the initial description
2. **Identify the affected module(s):**
   - `hearth-better-printers` — AST printing / quasiquote generation
   - `hearth-cross-quotes` — `Expr.quote`/`Expr.splice`/`Type.of` cross-platform macro support
   - `hearth` — main library utilities
   - `hearth-micro-fp` — FP utilities (rarely buggy)
3. **Identify the affected Scala version(s):** 2.13, 3, or both
4. **Check if the issue matches a known error pattern** — if so, use the dedicated instruction:
   - Compilation error with path `hearth/<macro>:...` → [Cross-Quotes errors caused by Better-Printers](instruction-for-fixing-cross-quotes-errors-on-scala-2-caused-by-better-printers.md)
   - Type names like `someValue.Underlying` in generated code (Scala 2) → [Cross-Quotes undetected implicit (Scala 2)](instruction-for-fixing-cross-quotes-errors-on-scala-2-caused-by-undetected-implicit.md)
   - Type names like `someValue.Underlying` in generated code (Scala 3) → [Cross-Quotes undetected implicit (Scala 3)](instruction-for-fixing-cross-quotes-errors-on-scala-3-caused-by-undetected-implicit.md)
   - `':' expected but identifier found.` → [Colon expected error](instruction-for-fixing-colon-expected-but-identifier-found.md)
   - `forward reference to value` → [Forward reference error](instruction-for-fixing-forward-reference-to-value.md)
   - `ScopeException: Cannot call ... on an Expr that was defined in a different Quotes context` → [ScopeException from raw quotes](instruction-for-fixing-scope-exception-in-scala-3-caused-by-raw-quotes.md) (`ScopeException` might be hidden)


## Step 2 — Reproduce with a failing test

### Where to place the test

Follow the rules from [guidelines for tests](guidelines-for-tests.md):

 - Shared (both Scala versions, all platforms): `hearth-tests/src/test/scala/`
 - Scala 2 only: `hearth-tests/src/test/scala-2/`
 - Scala 3 only: `hearth-tests/src/test/scala-3/`
 - JVM only: `hearth-tests/src/test/scalajvm/`

### Writing the test

**Prefer extending an existing fixture** over creating a new one when the bug relates to existing functionality.
For example, if the bug is about `Expr.quote` behavior, extend `CrossExprsFixturesImpl.scala` rather than creating a new fixture.

**If a new fixture IS needed**, follow the 3-level pattern:

1. `*FixturesImpl.scala` in `hearth-tests/src/main/scala/` — cross-compiled macro logic using `hearth.data.Data`
2. `*Fixtures.scala` in `hearth-tests/src/main/scala-2/` — Scala 2 macro adapter
3. `*Fixtures.scala` in `hearth-tests/src/main/scala-3/` — Scala 3 inline macro adapter

**For bugs where code does not compile** (the bug IS a compilation error):

```scala
compileErrors(
  """
  // code that triggers the bug
  """
).arePresent() // confirms the bug exists
```

or to verify a specific error message:

```scala
compileErrors(
  """
  // code that triggers the bug
  """
).check(
  "expected error text"
)
```

**For bugs where code compiles but produces wrong output:**

```scala
SomeFixtures.testSomething(...) <==> Data.map(
  "expected" -> Data("value")
)
```

### New test classes

If a new test file is needed, place example types and fixture data in `hearth-tests/src/main/` (not `src/test/`),
and the test spec in `hearth-tests/src/test/`. Ask the user if you are unsure about whether a new test class is needed.


## Step 3 — Clean and verify the test fails

**Always clean after writing a test that exercises macros** — incremental compilation does NOT re-expand macros
when their implementation changes, and it may not detect new test fixtures either.

### Clean commands by changed module

All commands use `sbt --client`:

| What changed | Minimum clean commands (Scala 2.13 + 3) |
|---|---|
| `hearth-better-printers` | `hearthBetterPrinters/clean ; hearthBetterPrinters3/clean ; hearthCrossQuotes/clean ; hearthCrossQuotes3/clean ; hearth/clean ; hearth3/clean ; quick-clean` |
| `hearth-cross-quotes` | `hearthCrossQuotes/clean ; hearthCrossQuotes3/clean ; hearth/clean ; hearth3/clean ; quick-clean` |
| `hearth` | `hearth/clean ; hearth3/clean ; quick-clean` |
| `hearth-tests` only (new test/fixture) | `quick-clean` |

Note: `quick-clean` cleans `hearthTests`, `hearthTests3`, `hearthSandwichTests`, `hearthSandwichTests3`.

Then run tests for the affected Scala version:

```bash
sbt --client "hearthTests/test"    # Scala 2.13
sbt --client "hearthTests3/test"   # Scala 3
```

**Confirm the new test fails as expected** before moving to the fix.


## Step 4 — Apply the fix

 - Make the fix in the appropriate module
 - Follow the dedicated instruction docs if the error pattern matches one of the known cases (see Step 1)
 - If working on `hearth-better-printers` or `hearth-cross-quotes`, remember the dependency chain:
   `hearth-better-printers` → `hearth-cross-quotes` → `hearth` → `hearth-tests`


## Step 5 — Clean and verify the fix works

Apply the **same clean commands** from Step 3 (the module chain is the same or larger if you changed additional modules).

Run the **full quick-test** to verify nothing is broken:

```bash
sbt --client "quick-clean"
sbt --client "quick-test"
```

This runs `hearthTests/test ; hearthTests3/test ; hearthSandwichTests/test ; hearthSandwichTests3/test` after cleaning all JVM test modules.

If only one Scala version is affected, you may test just that version first for a faster feedback loop, but **always
run `quick-test` as the final check** before considering the work done.


## Incremental compilation gotchas

 - **Always clean after macro changes** — incremental compilation does NOT re-expand macros when their
   implementation changes. You WILL get stale results if you skip cleaning.

 - **Dual cleaning is often needed:** hearth itself expands macros from `hearth-cross-quotes`. If you change
   `cross-quotes`, you must clean `hearth` (to re-expand macros in the library code) AND `hearth-tests`
   (to re-expand macros that consume the library).

 - **When in doubt:** `quick-clean` then `quick-test` — this cleans all JVM test modules and runs all JVM tests.

 - **Nuclear option** (if behavior seems wrong despite clean):

   ```bash
   sbt --client "hearthBetterPrinters/clean ; hearthBetterPrinters3/clean ; hearthCrossQuotes/clean ; hearthCrossQuotes3/clean ; hearthMicroFp/clean ; hearthMicroFp3/clean ; hearth/clean ; hearth3/clean ; quick-clean"
   sbt --client "quick-test"
   ```


## MCP limitations

 - The MCP server exposes only **one Scala version** at a time (controlled by `dev.properties`)
 - For bugs affecting one version: use MCP for that version when possible, then verify with `sbt --client` for the other
 - For testing both versions: use `sbt --client` directly — it compiles and tests both Scala 2.13 and 3
 - **Never change `dev.properties` yourself** — ask the user if a MCP version switch is needed


## Cross-quotes and macro-agnostic APIs

Code in hearth uses `Expr`, `Type`, etc. from **hearth's own API**, NOT `scala.quoted.Expr` or `c.Expr`.
Specifically:

 - `Expr.quote { ... Expr.splice { ... } }` is hearth's cross-platform alternative to Scala 3 quotes/splices
   and Scala 2 quasiquotes
 - When writing test fixtures, use hearth's API (traits from `hearth.typed`, `hearth.crossquotes`, etc.)
 - The implementation traits mix in `MacroTypedCommons` or similar base traits that provide the cross-platform API


## Cross-quotes / better-printers specific gotchas

 - On Scala 2, `cross-quotes` uses `better-printers` to generate quasiquotes — bugs in printing result
   in invalid generated code
 - Errors with path `hearth/<macro>:...` indicate generated quasiquote issues
   (see [instruction for fixing Cross-Quotes errors caused by Better-Printers](instruction-for-fixing-cross-quotes-errors-on-scala-2-caused-by-better-printers.md))
 - The full dependency chain for cross-quotes fixes is:
   `hearth-better-printers` → `hearth-cross-quotes` → `hearth` → `hearth-tests`
 - Debug logging: ask the user to set `log.cross-quotes = true` in `dev.properties`,
   or use the `isOurCulprit` pattern in `CrossQuotesMacros.scala`
   (see [instruction for debugging a macro](instruction-for-debugging-a-macro.md))
