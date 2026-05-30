# AI Agent Guidelines for Hearth

This document provides guidelines for AI agents (like Claude Code, GitHub Copilot, Cursor, etc.) working with the Hearth codebase.

## Project Overview

**Hearth** is the first Scala macros' standard library, designed to make macro development easier and more maintainable across Scala 2.13 and Scala 3.

- **Language**: Scala (pure Scala library)
- **Scala Versions**: 2.13.16, 3.3.8 (primary); 2.13.18, 3.8.4 (regression testing)
- **Platforms**: JVM, Scala.js, Scala Native
- **Build Tool**: SBT (but see restrictions below)
- **License**: Apache 2.0

### Repository Structure

```
hearth/
├── hearth/                          # Main library module
│   └── src/main/
│       ├── scala/                   # Shared code (all versions/platforms)
│       ├── scala-2/                 # Scala 2.13-specific implementations
│       ├── scala-3/                 # Scala 3-specific implementations
│       ├── scalajs/                 # Scala.js platform code
│       ├── scalajvm/                # JVM platform code
│       └── scalanative/             # Scala Native platform code
│
├── hearth-better-printers/          # Better alternatives to showCode/showRaw
├── hearth-cross-quotes/             # Cross-platform quoting utilities
├── hearth-micro-fp/                 # Lightweight FP utilities
├── hearth-munit/                    # MUnit-based testing utilities
│
├── hearth-tests/                    # Test suite for main library
├── hearth-sandwich-tests/           # Cross-compilation tests (Scala 2 ↔ 3)
├── hearth-sandwich-examples-213/    # Scala 2.13 test cases
├── hearth-sandwich-examples-3/      # Scala 3 test cases
│
├── docs/                            # MkDocs documentation
├── project/                         # SBT build configuration
├── scripts/                         # Utility scripts
│
├── build.sbt                        # Main build configuration
├── dev.properties                   # Local IDE settings (DO NOT COMMIT)
├── CONTRIBUTING.md                  # Contribution guidelines
└── README.md                        # Project introduction
```

### Module dependencies

**Published modules:**
1. `hearth-better-printers` - Base printer utilities
2. `hearth-cross-quotes` - Core cross-platform quoting (depends on `hearth-better-printers`)
3. `hearth-micro-fp` - FP utilities (no dependencies)
4. `hearth` - Main library (depends on `hearth-micro-fp` and `hearth-better-printers`)
5. `hearth-munit` - MUnit integration (optional)

**Test modules:**
- `hearth-tests` - Main test suite (depends on `hearth`)
- `hearth-sandwich-tests` - Cross-compilation validation (depends on `hearth-tests`, both `compile` and `test` config)

### Development Configuration

The `dev.properties` file controls IDE settings:

```properties
# Choose IDE Scala version: 2.13 or 3
ide.scala = 3

# Choose IDE platform: jvm, js, or native
ide.platform = jvm

# Enable cross-quotes logging: true, false, or file names
log.cross-quotes = false
```

**Important:**
- DO NOT commit changes to `dev.properties`
- Consider running: `git update-index --assume-unchanged dev.properties`
- After changing settings, reload build in IDE

## Global rules

 - ✅ **allowed operations**:

    ```bash
    git status
    git diff
    git log
    git branch
    git show
    sbt --client "..."
    ```

 - ❌ **forbidden operations**:

    ```bash
    git commit
    git commit -m "message"
    git commit --amend
    git push
    git push --force
    git rebase
    git merge
    git reset --hard
    sbt          # bare sbt without --client is forbidden
    ```

 - **prefer working with MCP server** when available, it allows for:

    - **Compilation**: Request compilation through MCP tools
    - **Testing**: Run tests through MCP tools
    - **Diagnostics**: Get compiler errors, warnings, and type information
    - **Code navigation**: Find definitions, references, implementations
    - **Type information**: Query types at specific positions

 - **use `sbt --client` when MCP is insufficient** — e.g. when the task requires both Scala 2 and Scala 3
   (MCP exposes only 1 version at a time), or when MCP is down. Always use `--client` flag as sbt startup
   is expensive and kills the feedback loop — **never run bare `sbt` without `--client`**
 - **redirect sbt output to a temporary file** when running long compilation/test cycles — this avoids
   re-running the same expensive command just to inspect a different part of the output:
   ```bash
   sbt --client "quick-clean ; quick-test" 2>&1 | tee /tmp/sbt-output.txt
   ```
   Then use `grep`, `tail`, `head`, etc. on `/tmp/sbt-output.txt` to inspect results. Only re-run
   sbt if code was actually modified.
 - **do not modify `dev.properties`** - it's primarily used by the developer to focus on working on one platform in their IDE without the issues related to shared sources
   (IDE not being able to identify whether source file should be treated as a part of Scala 2 project or Scala 3 project, JVM or JS or Native)

### Finding the MCP Server Address

The MCP server configuration is stored in one of these files:
- `.metals/mcp.json` (standard Metals configuration)
- `.cursor/mcp.json` (Cursor IDE configuration)

Example configuration:
```json
{
  "servers": {
    "hearth-metals": {
      "url": "http://localhost:58885/sse"
    }
  }
}
```

### MCP Server Project Configuration

**IMPORTANT:** The MCP server is configured to use **only ONE Scala version** (either 2.13 OR 3) and **only ONE platform** (JVM, JS, or Native) at a time.

**Why this limitation?**
- IDEs (VS Code, Cursor, IntelliJ) get confused when a single file belongs to multiple projects
- This prevents duplicate errors, inconsistent type information, and navigation issues
- The configuration is controlled by `dev.properties`

**Before starting work, ALWAYS:**

1. **Check which projects are currently enabled:**
   ```bash
   cat dev.properties
   ```
   Look at the `ide.scala` and `ide.platform` settings.

2. **Verify you're working on the correct version/platform:**
   - If working on Scala 3-specific features, ensure `ide.scala = 3`
   - If working on Scala 2.13-specific features, ensure `ide.scala = 2.13`
   - If working on platform-specific code, ensure `ide.platform` matches

3. **If the configuration doesn't match your work:**
   - **SUGGEST** to the user: "I notice `dev.properties` is set to Scala X.X and platform Y, but we're working on Scala A.B / platform Z code. Should I update `dev.properties` to match?"
   - **Wait for confirmation** before proceeding
   - **After changing `dev.properties`:** The user must reload the Metals build server in their IDE
   - **DO NOT commit** the `dev.properties` changes

**Example workflow:**
```
Agent: I see we're working on Scala 3-specific code in `src/main/scala-3/`, but dev.properties
       is configured for Scala 2.13. Should I update dev.properties to use Scala 3 and ask you
       to reload the build server?

User: Yes, go ahead

Agent: [Updates dev.properties to set ide.scala = 3]
       Please reload the Metals build server in your IDE for the changes to take effect.
```

## Quality Assurance

### Binary Compatibility

The project uses **MiMa** (Migration Manager) for binary compatibility checking.

When making changes:
- Be aware of binary compatibility constraints
- Breaking changes require major version bumps
- MiMa checks run automatically via CI
- Consult MCP server for compatibility reports

### CI/CD Pipeline

The GitHub Actions CI pipeline includes:
- **Formatting checks** - Scalafmt compliance
- **Snippet validation** - Scala CLI snippets in docs
- **Matrix testing** - 2 Scala versions × 3 platforms × 2 JDK versions each
- **Code coverage** - JVM only
- **Binary compatibility** - MiMa checks

Agents should:
- make sure that the code was actually compiled by sbt and that `quick-test` succeeded
- make sure that new snippets are runnable Scala CLI examples that pass tests
- coverage and MiMa can be skipped
- **before reporting that a task is done**, run `sbt --client "quick-clean ; quick-test"` as the final
  verification step — while MCP speeds up the development loop, `sbt --client` compiles and tests across
  both Scala versions and is the more reliable way to confirm a fix

If an agent was used to generate the code (e.g. following GitHub issue instructions),
but an agent was not able to run the compilation and tests (e.g. because GitHub sandboxing
prevents downloading artifacts), it should inform user that any PR they would attempt to make
will be closed immediately.

### Test Conventions

Tests follow a macro fixture pattern with cross-platform assertions:

1. **Shared fixture** (`hearth-tests/src/main/scala/hearth/typed/XxxFixturesImpl.scala`) — macro implementations returning `Expr[Data]` or `Expr[String]`
2. **Scala 2 bridge** (`hearth-tests/src/main/scala-2/hearth/typed/XxxFixtures.scala`) — `macro` definitions delegating to the shared fixture
3. **Scala 3 bridge** (`hearth-tests/src/main/scala-3/hearth/typed/XxxFixtures.scala`) — `inline def` + `${ impl }` delegating to the shared fixture
4. **Test spec** (`hearth-tests/src/test/scala/hearth/typed/XxxSpec.scala`) — assertions using `<==>` operator

**Assertion style — always use `<==>` with full `Data` structures:**
```scala
// CORRECT — full Data structure comparison with <==>
testMyMacro[MyType] <==> Data.map(
  "field1" -> Data("value1"),
  "field2" -> Data(true),
  "nested" -> Data.map("inner" -> Data(42))
)

// CORRECT — string comparison with <==>
testMyStringMacro[MyType] <==> "expected string"

// WRONG — never manually unwrap Data with .asMap.get.apply(...)
result.asMap.get.apply("field").asString.get ==> "value"  // DO NOT DO THIS
```

The `<==>` operator provides structured diffs on failure, showing exactly which fields differ between expected and actual. Manual unwrapping loses this diagnostic benefit and doesn't match the codebase convention.

## Bug Fix Workflow

For the complete bug-fix workflow (reproduce → fix → verify), see [Instruction for fixing a bug](docs/contributing/instruction-for-fixing-a-bug.md).

**Quick reference — clean commands by changed module** (all using `sbt --client`):

| What changed | Clean commands |
|---|---|
| `hearth-better-printers` | `hearthBetterPrinters/clean ; hearthBetterPrinters3/clean ; hearthCrossQuotes/clean ; hearthCrossQuotes3/clean ; hearth/clean ; hearth3/clean ; quick-clean` |
| `hearth-cross-quotes` | `hearthCrossQuotes/clean ; hearthCrossQuotes3/clean ; hearth/clean ; hearth3/clean ; quick-clean` |
| `hearth` | `hearth/clean ; hearth3/clean ; quick-clean` |
| `hearth-tests` only | `quick-clean` |

**Key reminders:**
- Always clean after macro changes — incremental compilation does NOT re-expand macros
- `quick-clean` then `quick-test` is the standard verify cycle
- MCP supports only 1 Scala version at a time — use `sbt --client` for the other version

## Key API patterns

### Method API architecture

The `Method` API has a layered architecture with platform-specific untyped code and shared typed code:

**Untyped layer** (platform-specific):
- `hearth/src/main/scala/hearth/untyped/UntypedMethods.scala` — shared abstract types and `UntypedMethodMethods` trait
- `hearth/src/main/scala-2/hearth/untyped/UntypedMethodsScala2.scala` — Scala 2 implementation
- `hearth/src/main/scala-3/hearth/untyped/UntypedMethodsScala3.scala` — Scala 3 implementation (includes `SyntheticNamedTupleConstructor`)

**Typed layer** (shared):
- `hearth/src/main/scala/hearth/typed/Methods.scala` — `Method` sealed trait with 4 variants (`OnInstance`, `ApplyTypes`, `ApplyValues`, `Result`), `buildChain`, `fold`/`foldF`, `AppliedState`

**Rendering:**
- `plainPrint`/`prettyPrint` share code via `renderSignature(instanceTpe, SyntaxHighlight)` — the `SyntaxHighlight` parameter controls ANSI coloring
- Platform-specific `paramTypePrints(hl)` and `signatureSegments(hl)` accept the same parameter
- `toString` includes applied-state info when the builder chain is partially consumed

**Test infrastructure:**
- `hearth-tests/src/main/scala/hearth/typed/MethodsFixturesImpl.scala` — shared fixture implementations
- `hearth-tests/src/main/scala-2/hearth/typed/MethodsFixtures.scala` — Scala 2 macro bridges
- `hearth-tests/src/main/scala-3/hearth/typed/MethodsFixtures.scala` — Scala 3 macro bridges
- `hearth-tests/src/test/scala/hearth/typed/MethodsSpec.scala` — cross-platform tests
- `hearth-tests/src/main/scala/hearth/examples/methods_overhaul.scala` — test data classes

**Method-level modifier flags:**
- `isFinal`, `isAbstract`, `isOverride` exposed on both `UntypedMethod` and `Method`
- Scala 2: `symbol.isFinal`, `(symbol: Symbol).isAbstract`, `symbol.asMethod.overrides.nonEmpty`
- Scala 3: `Flags.Final`, `Flags.Deferred`, `Flags.Override`
- `renderSignature` displays `final`, `abstract override`, `override` keywords

**Type hierarchy APIs:**
- `isTrait`, `parents`, `baseClasses` on both `UntypedType` and `Type`
- Scala 3: `parents`/`baseClasses` use Java reflection to call `TypeRepr` methods, bypassing `UntypedTypeMethods` extension shadowing
- Existing `isEnumeration` code in Scala 3 calls `typeReprBaseClasses` helper to avoid the same shadowing issue

**Cross-platform considerations:**
- `isPrivate`/`isProtected` are normalized: `private[pkg]` → `isPrivate=false`, `privateWithin=Some("pkg")`
- Type rendering must produce identical output on Scala 2 and 3 (no `LanguageVersion` conditionals in tests)
- Scala 3 has clause interleaving; Scala 2 does not — this is tested in `scala-newest-3` only
- `isOverride` differs for `java.lang.Object` methods: `true` on Scala 2 (overrides from `Any`), `false` on Scala 3 (root class)

### AnonymousInstance architecture

`AnonymousInstance` lives inside the `Classes` trait, alongside `CaseClass`, `SingletonValue`, etc. It provides compile-time anonymous subtype instantiation — `new Foo with Bar { override def ... }` — for use cases like mocking and proxy generation.

**Shared code** (`Classes.scala`):
- `AnonymousInstanceError` sealed trait with 8 error variants (rendered to strings at API boundary)
- `MethodClassification` with 4 variants: `MustOverride`, `MayOverride`, `CannotOverride`, `DiamondConflict`
- `ClassifiedMethod` pairs a `Method` with its classification and declaring type
- `OverrideContext` provides `self: Expr_??` (this reference + novel type), `method: UntypedMethod`, `parameters: List[Expr_??]`, `returnType: ??`
- `OverrideBody` functional interface for override body callbacks
- `AnonymousInstance[A]` class view with `parse`, `parseWithMixins`, `construct`
- Validation: rejects final/sealed types, checks constructor accessibility, at most one class parent

**Platform-specific tree generation** (in `UntypedTypesScala2.scala` / `UntypedTypesScala3.scala`):
- `unsafeNewSubtype` generates anonymous class trees
- Scala 2: quasiquotes `q"new $parent(..$args) with ..$traits { ..$overrides }"`
- Scala 3: `Symbol.newClass` + `ClassDef.apply` via Java reflection (avoiding `@experimental`)
- Scala 3: prepends `AnyRef` when first parent is a trait (required by `Symbol.newClass`)

**Test infrastructure:**
- `hearth-tests/src/main/scala/hearth/typed/AnonymousInstanceFixturesImpl.scala` — shared fixtures
- `hearth-tests/src/main/scala-2/hearth/typed/AnonymousInstanceFixtures.scala` — Scala 2 macro bridges
- `hearth-tests/src/main/scala-3/hearth/typed/AnonymousInstanceFixtures.scala` — Scala 3 macro bridges
- `hearth-tests/src/test/scala/hearth/typed/AnonymousInstanceSpec.scala` — cross-platform tests (13 tests)
- `hearth-tests/src/main/scala/hearth/examples/anonymous_instances.scala` — test data classes

### DestructuredExpr architecture

`DestructuredExpr` is inlined into the `Exprs` / `ExprsScala2` / `ExprsScala3` traits (not a separate trait). It provides semantic expression decomposition where method calls are resolved against Hearth's `Method` API.

**Shared code** (end of `Exprs.scala`):
- `DestructuredExpr` sealed trait with 7 node types: `MethodCall`, `Lambda`, `Lambda.ParamRef`, `Literal`, `Singleton`, `Block`, `NonDestructurable`
- `MethodCall.Applied` sealed trait: `AppliedInstance`, `AppliedTypes`, `AppliedValues`
- `FieldPath`, `FieldPathSegment`, `LambdaInfo` convenience types
- `DestructuredExpr.parse`, `parseUntyped`, `extractFieldPath`, `extractLambda`
- `protected def destructureExpr` — abstract platform hook

**Platform-specific parsing** (end of `ExprsScala3.scala` / `ExprsScala2.scala`):
- `dstrImpl` — recursive tree walker that resolves `Method` by symbol equality (`method.symbol == tree.symbol`)
- `dstrFlattenCall` — peels `Apply`/`TypeApply` chains to extract core term + call steps
- `dstrTryMethodCall` — resolves the method from `UntypedMethod.methods(qualifierType)`
- Lambda parameters tracked via `Map[Symbol, Lambda.Param]` so `Ident` references become `ParamRef`
- Scala 3: handles `Flags.ExtensionMethod` where receiver is first value argument (context functions)
- Types are widened on both platforms to avoid singleton types (`p.type`)

**Test infrastructure:**
- `hearth-tests/src/main/scala/hearth/typed/DestructuredExprsFixturesImpl.scala` — shared fixtures
- `hearth-tests/src/main/scala-2/hearth/typed/DestructuredExprsFixtures.scala` — Scala 2 macro bridges
- `hearth-tests/src/main/scala-3/hearth/typed/DestructuredExprsFixtures.scala` — Scala 3 macro bridges
- `hearth-tests/src/test/scala/hearth/typed/DestructuredExprsSpec.scala` — cross-platform tests (19 tests)
- `hearth-tests/src/test/scala-3/hearth/typed/DestructuredExprsScala3Spec.scala` — Scala 3-only context function tests (2 tests)
- `hearth-tests/src/main/scala/hearth/examples/parsed_exprs.scala` — test data + DSL extensions (implicit class)
- `hearth-tests/src/main/scala-3/hearth/examples/parsed_exprs-s3.scala` — Scala 3-only context function DSL extensions

### ExprCodec derivation

`ExprCodec.derived[A]` provides semi-automatic derivation of `ExprCodec` instances for case classes, sealed traits, and singletons. The user calls it explicitly inside their macro; it is NOT an implicit.

**Shared derivation logic** (`hearth/src/main/scala/hearth/typed/ExprCodecDerivation.scala`):
- `deriveExprCodecInternal[A: Type]: ExprCodec[A]` — rule-based dispatch: singleton → case class → enum
- `deriveSingletonCodec` — uses `SingletonValue.singletonExpr` for `toExpr`
- `deriveCaseClassCodec` — uses `Product.productElement` for field access, `CaseClass.construct` for tree building
- `deriveEnumCodec` — matches runtime class name against `Enum.directChildren`, recursively derives child codecs
- `resolveFieldCodecForDerivation` — looks up built-in ExprCodec by type equality, falls back to recursive derivation
- `fromExpr` uses `semiEval` for all types

**Entry points:**
- Scala 2 (`hearth/src/main/scala-2/hearth/typed/ExprsCompat.scala`): `ExprCodec.derived[A]` is a `macro def` backed by `ExprCodecDerivationMacros`, which generates a tree calling `deriveExprCodecInternal[A]`
- Scala 3 (`hearth/src/main/scala-3/hearth/typed/ExprsCompat.scala`): `ExprCodec.derived[A]` is an `inline def` that calls `deriveExprCodecInternal[A]` directly

**Mixed into MacroCommons** via `ExprCodecDerivation` trait in `MacroTypedCommons`.

**Test infrastructure:**
- `hearth-tests/src/main/scala/hearth/examples/expr_codecs.scala` — test data (case classes, sealed traits, singletons)
- `hearth-tests/src/main/scala/hearth/typed/ExprCodecFixturesImpl.scala` — shared round-trip fixtures
- `hearth-tests/src/main/scala-2/hearth/typed/ExprCodecFixtures.scala` — Scala 2 macro bridges
- `hearth-tests/src/main/scala-3/hearth/typed/ExprCodecFixtures.scala` — Scala 3 macro bridges
- `hearth-tests/src/test/scala/hearth/typed/ExprCodecSpec.scala` — cross-platform tests (6 tests)

## Skills

Are available in [contributing documentation](docs/contributing/_index.md).

The bug fix workflow is documented in [instruction for fixing a bug](docs/contributing/instruction-for-fixing-a-bug.md).
