# Instruction for developing Type Constructors

This instruction covers adding new features, fixing bugs, or extending `Type.CtorN` (type constructors) in the hearth library.

## Architecture overview

Type constructors (`Type.Ctor1` through `Type.Ctor22`) are generated from code in `project/`:

| Generator | Output | Purpose |
|-----------|--------|---------|
| `project/TypeConstructorsGen.scala` | `hearth/.../TypeConstructors.scala` | Public API trait with `CtorN` types, `of`, `fromUntyped`, `apply`, `unapply`, `setX`, `asUntyped` |
| `project/CrossCtorTestGen.scala` | 4 files in `hearth-tests/` | Test impl trait, Scala 2/3 bridges, spec file |
| `project/CrossQuotesMacrosGen.scala` | Cross-quotes macro/plugin code | `Type.CtorN.of[F]` rewriting for Scala 2 macros and Scala 3 compiler plugin |
| `project/ArityGen.scala` | (shared helpers, not a generator) | Naming conventions: `paramName`, `params`, `lower`, `upper`, `writeIfChanged` |

All generators are wired in `build.sbt` via `SourceGenPlugin`. Generated files go to `sourceManaged` directories and should never be checked in.

### File relationships

```
project/ArityGen.scala (shared naming: A,B,C...; L1,U1; Ctor1,Ctor2...)
       │
       ├── project/TypeConstructorsGen.scala ──► hearth/TypeConstructors.scala (API)
       │
       ├── project/CrossCtorTestGen.scala
       │       ├── generate()            ──► CrossCtorInjectionFixturesImplGen.scala (trait)
       │       ├── generateScala2Bridge()──► CrossCtorInjectionFixtures.scala (Scala 2)
       │       ├── generateScala3Bridge()──► CrossCtorInjectionFixtures.scala (Scala 3)
       │       └── generateSpec()        ──► CrossCtorInjectionSpec.scala (test)
       │
       └── project/CrossQuotesMacrosGen.scala ──► Cross-quotes macro/plugin code
```

The hand-written file `hearth-tests/src/main/scala/hearth/crossquotes/CrossCtorInjectionFixturesImpl.scala` extends the generated `CrossCtorInjectionFixturesImplGen` trait and adds tests that don't fit the mechanical arity pattern.


## Naming conventions

`ArityGen` defines the naming:

- `paramName(i)` is **0-based**: `A`=0, `B`=1, ..., `V`=21
- `params(n)` returns the first N param names: `params(3)` = `Seq("A", "B", "C")`
- `lower(i)` / `upper(i)` are **1-based**: `L1`, `U1`, `L2`, `U2`, ...
- `ctorName(n)` = `"Ctor1"`, `"Ctor2"`, etc.
- `writeIfChanged(file, content)` writes only if content differs (avoids recompilation)

Test-specific naming in `CrossCtorTestGen`:

- `typeName(1)` = `"Option"`, `typeName(2)` = `"Either"`, `typeName(n≥3)` = `"ArityN"`
- `ctorVarName(1)` = `"OptionCtor"`, `ctorVarName(2)` = `"EitherCtor"`, `ctorVarName(n≥3)` = `"ArityNCtor"`
- `containerName(1)` = `"Container"`, `containerName(n≥2)` = `"ContainerN"`


## How build.sbt wires generation

### Public API (hearth module)

`TypeConstructorsGen` generates one file with Scala-version-specific content:

```scala
// build.sbt (hearth module)
val content = if (isScala3) TypeConstructorsGen.scala3() else TypeConstructorsGen.scala2()
ArityGen.writeIfChanged(file, content)
```

### Test files (hearth-tests module)

`CrossCtorTestGen` generates 3 files. The impl trait goes to `Compile / sourceManaged`, the bridge file also goes there (with content selected by `scalaVersion.value`), and the spec file goes to `Test / sourceManaged`:

```scala
// build.sbt (hearthTests module)
SourceGenPlugin.autoImport.generateHearthSources := {
  val outDir = (Compile / sourceManaged).value
  val isScala3 = scalaVersion.value.startsWith("3.")
  val implGen = outDir / "hearth" / "crossquotes" / "CrossCtorInjectionFixturesImplGen.scala"
  ArityGen.writeIfChanged(implGen, CrossCtorTestGen.generate())
  val bridge = outDir / "hearth" / "crossquotes" / "CrossCtorInjectionFixtures.scala"
  val bridgeContent = if (isScala3) CrossCtorTestGen.generateScala3Bridge()
                      else CrossCtorTestGen.generateScala2Bridge()
  ArityGen.writeIfChanged(bridge, bridgeContent)
  Seq(implGen, bridge)
},
Test / sourceGenerators += Def.task {
  val outDir = (Test / sourceManaged).value
  val spec = outDir / "hearth" / "crossquotes" / "CrossCtorInjectionSpec.scala"
  ArityGen.writeIfChanged(spec, CrossCtorTestGen.generateSpec())
  Seq(spec)
}.taskValue,
```


## Common development tasks

### Adding a new method to Type.CtorN

1. Add the method signature to the `Bounded` trait in `TypeConstructorsGen.scala` (both `scala2()` and `scala3()` codepaths)
2. Add the implementation in the `FromUntypedImpl` class and potentially the `Impl` class
3. Add tests: either extend `CrossCtorTestGen` with a new `genTestXxx` method, or add hand-written tests to `CrossCtorInjectionFixturesImpl.scala`
4. If the method involves Cross-Quotes rewriting, update `CrossQuotesMacrosGen.scala` as well

### Adding a test for existing Ctor functionality

If the test follows the arity pattern (same test for Ctor1 through Ctor22):

1. Add a new `genTestXxx` method in `CrossCtorTestGen.generate()`
2. Add a corresponding `genScala2Xxx`/`genScala3Xxx` in the bridge generators
3. Add a `genSpecXxx` method in the spec generator
4. Wire these into `generate()`, `generateScala2Bridge()`, `generateScala3Bridge()`, and `generateSpec()`

If the test is special-cased (e.g. identity ctor, imported ctor):

1. Add it to the hand-written `CrossCtorInjectionFixturesImpl.scala`
2. Add bridge methods manually to the Scala 2/3 bridge generators (or keep them hand-written if they don't follow a pattern)

### Extending max arity beyond 22

1. Change `maxArity` in `ArityGen.scala` (currently 22)
2. Add more letters to `typeParamNames` (currently A through V = 22 letters)
3. Add corresponding `ArityN` example types in `hearth-tests/src/main/scala/hearth/examples/kinds.scala`
4. All generators will automatically pick up the new arity


## Pitfalls and edge cases

### Dollar sign escaping in string interpolation

When generating code that contains literal `$` characters (e.g. Scala 3 synthetic type parameter names like `_$1`, `_$2`), be careful with Scala string interpolation:

```scala
// WRONG: s"_$$i" with 4 dollar signs produces literal "_$i" for every iteration
// because $$ = literal $, then i is treated as literal text

// CORRECT: s"_$$${i}" or s"_$$$i" produces "_$1", "_$2", etc.
// because $$ = literal $, then ${i} or $i interpolates the variable
```

This is a common source of bugs in the test generators. If generated expected values contain `$` followed by a number, double-check the escaping.

### Scala 3 synthetic type lambda parameter naming

When `setA` is called on a type constructor, Scala 3 produces a type lambda with synthetic parameter names that differ from the original:

| Source | Example | Synthetic params |
|--------|---------|-----------------|
| `Type.Ctor1.of[Either].setA[String]` → `Ctor1` | `Either[String, _]` | `X` |
| `Type.Ctor2.of[Arity3].setA[String]` → `Ctor2` | `Arity3[String, _, _]` | `X, Y` |
| `Type.Ctor3.of[Arity4].setA[String]` → `Ctor3` | `Arity4[String, _, _, _]` | `B, C, D` (original names) |
| `Type.Ctor21.of[Arity22].setA[String]` → `Ctor21` | `Arity22[String, _, ..., _]` | `B, C, ..., V` (original names) |
| `Type.Ctor22.of[Arity22]` (no setA) → `Ctor22` | `Arity22[_, ..., _]` | `A, B, ..., V` (original names) |

The pattern: **Ctor1 and Ctor2 (after setA) use synthetic names `X` and `X, Y`**. All higher arities preserve the original parameter names starting from `B`. This matters for `asUntyped` and `typeOf` expected values in tests.

### Version-specific bridge patterns

The Scala 2 bridge is a **macro bundle class** (`final private class ... (val c: blackbox.Context)`). Methods are "Impl" suffixed and typically forward to the shared trait.

The Scala 3 bridge uses **inline def + private splice method** pairs in the companion object:
```scala
inline def testXxx: Data = ${ testXxxImpl }
private def testXxxImpl(using q: Quotes): Expr[Data] =
  new CrossCtorInjectionFixtures(q).testXxx
```

The `testCtorExtractN` methods are the exception — they contain substantive logic (not just forwarding) because they need to use platform-specific APIs (`c.weakTypeOf` on Scala 2, `TypeRepr.of` + `AppliedType` match on Scala 3).

### sbt shutdown after project/ changes

Since all generators live in `project/`, modifying them changes the build definition. After any change to files in `project/`, run:
```bash
sbt --client "shutdown"
```
before the next `sbt --client` command, to force sbt to reload the build definition.


## Testing workflow

After modifying any generator:

```bash
# 1. Shutdown sbt (required after project/ changes)
sbt --client "shutdown"

# 2. Clean and test
sbt --client "quick-clean ; quick-test" 2>&1 | tee /tmp/sbt-output.txt
```

If tests fail, inspect the generated files in `target/jvm-2.13/src_managed/` and `target/jvm-3/src_managed/` to understand what was actually produced. Compare against the expected values in the spec to identify mismatches.
