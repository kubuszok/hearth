# Instruction for fixing ScopeException in Scala 3 caused by raw quotes

When code in `ExprsScala3.scala` uses raw Scala 3 quotation syntax (`'{ ... }` or `${ ... }`) inside a closure
that may execute in a **nested Quotes context**, the expansion fails at compile time with a `ScopeException`.

## Recognizing the issue

**Error message (at the call site that expands the macro):**
```
[error] .../SomeFile.scala:42:9: Cannot call `asTerm` on an `Expr` that was defined in a different `Quotes` context.
```
or
```
scala.quoted.runtime.impl.ScopeException:
  Cannot call `asTerm` on an `Expr` that was defined in a different `Quotes` context.
```

**Key symptom:**
- The error comes from Scala 3 macro expansion (not Scala 2)
- It references a `Quotes` scope mismatch
- It occurs in code that uses raw `'{ ... }` or `${ ... }` (as opposed to `Expr.quote { ... }` / `Expr.splice { ... }`)
- It typically appears inside a **closure** that is stored and invoked later, when the `Quotes` context has changed

**Not this issue if:**
- The error is on Scala 2 (Scala 2 does not have `Quotes` scoping)
- The code only uses `Expr.quote` / `Expr.splice` / `Type.of` (these go through `CrossQuotes` and handle scope automatically)
- The raw quote is used at top level (not inside a closure)


## What causes it

Hearth manages Quotes contexts through `CrossQuotes`. The flow is:

1. The class-level implicit `Quotes` (Q0) comes from `MacroCommonsScala3`
2. `Expr.splice` calls `CrossQuotes.nestedCtx`, which pushes a new Quotes context (Q1) onto a stack
3. Code inside the splice runs with Q1 as the "current" context
4. `CrossQuotes.ctx[Quotes]` retrieves the current context from the stack

**`Expr.quote` / `Expr.splice`** are transformed by the `CrossQuotesPlugin` compiler plugin to automatically
use `CrossQuotes.ctx` and wrap splices with `CrossQuotes.nestedCtx`. They are scope-safe.

**Raw `'{ ... }` / `${ ... }`** are NOT transformed by the plugin. They resolve `Quotes` through normal
implicit resolution, which finds Q0 (the class-level implicit). If the raw quote executes inside a nested context
(Q1), the `Expr`/`Term` values it produces belong to Q0, but the surrounding code expects Q1. This mismatch
causes `ScopeException`.

**Why closures trigger this:** The `buildValDef` closure in `ValDefBuilder.ofDefN` (or any similar closure)
captures the raw `'{ ... }` expression at definition time, when Q0 is in scope. But MIO's lazy evaluation and
`Expr.splice` → `CrossQuotes.nestedCtx` means the closure executes in a nested context (Q1). At execution time:
- `body.asTerm` uses Q1 (correct — `body` was created in Q1's context)
- `'{ val _ = $aExpr }.asTerm` uses Q0 (wrong — it should use Q1)

This Q0/Q1 mismatch is the `ScopeException`.


## Confirming the issue

### Step 1: Identify the raw quote

Look for `'{ ... }` or `${ ... }` in the suspected method. If the method only uses `Expr.quote` / `Expr.splice`,
this is not the issue (those are already scope-safe).

Common locations:
- `ExprsScala3.scala` in `ValDefBuilder.ofDefN` — the `buildValDef` closure's case body
- `ExprsScala3.scala` in `LambdaBuilder.ofN` — similar closure pattern
- Any closure in a Scala 3 platform-specific file that uses raw quotes and is invoked lazily

### Step 2: Write a reproduction test

The test must force the closure to execute in a nested Quotes context. The pattern is:

```scala
Expr.quote {
  val x: Int = Expr.splice {
    // This creates a nested Quotes context (Q1)
    // Inside here, any closures from builders will run with Q1
    someBuilder
      .traverse[MIO, Expr[Int]] { ... }
      .map(_.build.close)
      // MIO.scoped forces lazy evaluation
  }
  x
}
```

The key ingredients:
- `Expr.splice` (or `Expr.quote` inside `Expr.splice`) creates the nested Q1 context
- `MIO.scoped { runSafe => runSafe { ... } }` safely extracts the result from MIO, forcing lazy evaluation
- `.traverse` maps over the builder, which internally invokes the `buildValDef` closure in the current context

If the raw quote lacks `withQuotes`, this test will fail to compile with `ScopeException`.

### Step 3: Verify by temporarily removing `withQuotes`

If `withQuotes` is already present, temporarily remove it and verify the test fails.
If it is absent, verify the test already fails.


## Fixing the issue

Wrap the closure body that contains the raw quote with `withQuotes { ... }`:

**Before (broken):**
```scala
buildValDef = (body: Expr[Returned]) =>
  DefDef(
    name,
    {
      case List(List(a: Term)) =>
        Some {
          Block(
            List(
              ValDef(a1, Some(a)),
              '{ val _ = $aExpr }.asTerm  // ← uses Q0 (stale)
            ),
            body.asTerm.changeOwner(name)
          )
        }
    }
  )
```

**After (fixed):**
```scala
buildValDef = (body: Expr[Returned]) =>
  DefDef(
    name,
    {
      case List(List(a: Term)) => withQuotes {  // ← provides current Quotes
        Some {
          Block(
            List(
              ValDef(a1, Some(a)),
              '{ val _ = $aExpr }.asTerm  // ← now uses CrossQuotes.ctx (current)
            ),
            body.asTerm.changeOwner(name)
          )
        }
      }
    }
  )
```

`withQuotes` is defined in `Expr.platformSpecific` (imported via `import Expr.platformSpecific.*`):

```scala
def withQuotes[A](thunk: scala.quoted.Quotes ?=> A): A =
  thunk(using CrossQuotes.ctx[Quotes])
```

It replaces the implicit `Quotes` inside the thunk with `CrossQuotes.ctx[Quotes]` — the dynamically current
Quotes context — instead of whatever the lexical scope provides (typically the stale class-level Q0).


## When is `withQuotes` NOT needed?

Methods that do NOT use raw `'{ ... }` or `${ ... }` do not need `withQuotes`. For example:

- `ValDefBuilder.ofVal` — its `buildValDef` is `body.asTerm.changeOwner(name)` (no raw quotes)
- `ValDefBuilder.ofVar` — its `buildVar` is `body.asTerm.changeOwner(name)` (no raw quotes)
- `ValDefBuilder.ofLazy` — same pattern, no raw quotes
- `ValDefBuilder.ofDef0` — same pattern, no raw quotes

These only use `body.asTerm` which correctly belongs to the current context (since `body` was created there).
The raw quotes (`'{ val _ = $aExpr }`) are what introduce the scope mismatch, because they capture a `Quotes`
from lexical scope rather than from `CrossQuotes.ctx`.


## Historical context

| Commit                                     | What was fixed                                                           |
|--------------------------------------------|--------------------------------------------------------------------------|
| `113c2deaacd24b819c4bb0eb3b1a61fdde9ec9ac` | quick fix in `LambdaBuilder.of1` — temporary workaround for one arity    | 
| `1b8340404ccbf65543825a3c836150de0fdf3c01` | reproduction and fix for every `LambdaBuilder.ofN`                       |
| `1c01e680431ef8178ea38901d99ae500e439bd70` | quick fix in `ValDefBuilder.ofDef4` — temporary workaround for one arity |
| `655e1251a349c229c11227e82570d534a29ed69e` | reproduction and fix for every `ValDefBuilder.ofDefN`                    |


## Checklist for future occurrences

When adding new code in `ExprsScala3.scala` (or any Scala 3 platform-specific file):

1. **Does it use raw `'{ ... }` or `${ ... }`?**
   - If no → no action needed
   - If yes → continue to step 2

2. **Is the raw quote inside a closure that might execute in a different Quotes context?**
   - If it is directly in a macro body (not inside a closure) → typically safe
   - If it is inside `buildValDef`, `buildVar`, or any closure stored in a builder → needs `withQuotes`

3. **Wrap with `withQuotes { ... }`** and write a test that exercises the builder through
   `Expr.splice` → `MIO.scoped` → `traverse` to confirm it works in a nested context.
