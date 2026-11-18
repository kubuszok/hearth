# Derivation Checklist

Principles to follow when implementing a type class derivation macro with Hearth. Not every item applies to every type class — skip what genuinely doesn't make sense for your use case.

For detailed guidance on each topic, see [Best Practices](best-practices.md) and the [landing page example](index.md).

## Entry Points

- [ ] Provide two entry points: one that returns a type class instance (`deriveTypeClass[A]`) and one that inlines the derivation result directly (`deriveInline[A]`) — both should delegate to the same core logic
- [ ] Offer a single derivation API — do not split it into "automatic" and "semi-automatic" imports with different semantics. If users need to control where derivation may happen, give them a global opt-in/opt-out (e.g. a `-Xmacro-settings` policy with allowed scopes, or an explicit import) whose compile error tells them to define an instance
- [ ] Separate runtime utilities (checked by [MiMa](https://github.com/lightbend-labs/mima)) from macro-generated code

## Type Coverage

- [ ] Handle primitives (Boolean, Byte, Short, Int, Long, Float, Double, Char, String) directly, without allocating a type class instance for each

    - [ ] Handle built-in types (primitives, `String`, `BigDecimal`, `java.time`, `UUID`, ...) in a dedicated rule placed before the collection rule — `String` is an `IsCollection` of `Char`, which shows up e.g. with Scala 3 `derives` on generic types

- [ ] Handle optional types (`Option`, etc.)

    - [ ] Decide and document the semantics of optional values: a missing field vs `null` vs a default value, and `Option[Option[A]]`

- [ ] Handle collections (`List`, `Vector`, `Set`, `Array`, `Seq`, etc.) and maps (`Map[K, V]`)

    - [ ] Check `IsMap` before `IsCollection` (`Map <: Iterable`)

- [ ] Use the standard extensions (`IsOption`, `IsCollection`, `IsMap`, `IsEither`, `IsValueType`) instead of hard-coding `Option`/`List`/`Map`, and load them once per expansion, so that integration modules (macro extensions) work
- [ ] Handle case classes — iterate over fields, derive recursively for each field type

    - [ ] Handle default values of case class parameters (configurable), including generic case classes and classes from other compilation units

- [ ] Handle sealed traits and enums — pattern-match on subtypes, derive recursively for each
- [ ] Handle singletons (case objects, parameterless enum cases) as a dedicated rule
- [ ] Handle value types (classes extending `AnyVal`, Scala 3 opaque types) through `IsValueType` — unwrap and recurse on the underlying type

    - [ ] On Scala 3 a single-element named tuple is an opaque type, so it can match `IsValueType` — check `Type[A].isNamedTuple` first if you handle named tuples differently

- [ ] Handle tuples, `Either`s, named tuples (Scala 3), union types (Scala 3), and type-parametric types — unless the type class genuinely has no meaningful behavior for them
- [ ] Handle Java Beans (`JavaBean`) and Java enums, if the type class makes sense for them

## Implicit Resolution

- [ ] Always prefer user-provided implicits — check for existing instances before deriving from scratch, so users can override the default behavior

    - [ ] Place the "use a user-provided implicit" rule before every built-in, value-type and collection rule

- [ ] Ignore self-summoning — exclude the `derived` method (and, for subtype type classes, the parent library's auto-derivation methods) from implicit search (`summonExprIgnoring`) to prevent infinite macro expansion

    - [ ] Additionally, exclude the current type class's type from implicit search, to prevent infinite recursion on `implicit val foo: TypeClass[Foo] = TypeClass.derive[Foo]`
    - [ ] However, if you allow inlined derivation, like `val result = TypeClass.inlined(foo)`, do not exclude the current type from non-autoderived instances, it will let you keep the same behavior as the derivation was not inlined (using user-provided implicit if available, falling back to autoderivation otherwise)

- [ ] Cache every resolved implicit as a `lazy val` — do not summon or allocate the same instance multiple times in the generated code
- [ ] Handle generic contexts — derivation should work correctly when invoked inside a generic function where type parameters are not yet known
- [ ] Never key recursion detection or cache entries by `Type.prettyPrint` — it contains ANSI colours and its output can depend on how the type was read; compare `Type`s, or use `Type.plainPrint`

## Recursion and Caching

- [ ] Derive recursively by walking the type tree internally, rather than relying on implicit resolution to recurse — this means users never need to learn about semi-automatic derivation or write `implicit val foo: TypeClass[Foo] = TypeClass.derive[Foo]`
- [ ] Cache derived logic for case classes and enums as local `def`s — this keeps generated code compact, avoids the 64kB method size limit, and enables recursive data structures automatically
- [ ] The combination of caching and macro-internal recursion should make recursive types like `case class Tree(children: List[Tree])` work transparently with `val tc = TypeClass.derive[Tree]`, with no user-side tricks
- [ ] Multi-method type classes: derive every method body at the outer level into a shared `ValDefsCache` (sibling `Expr.splice`s get separate `Quotes` on Scala 3), and wrap the *whole* instance with `toValDefs.use` — if only a method body is wrapped, the cached `lazy val`s are re-initialised on every call

## Error Reporting

- [ ] Aggregate errors across all fields and subtypes rather than failing on the first one

    - [ ] For multi-method type classes, also aggregate across the method bodies (`parTuple`/`parMap2` instead of a `for`-comprehension)

- [ ] Model errors as an ADT and render them once, at the entry point (`runToExprOrFail`) — no `reportErrorAndAbort` inside rules
- [ ] Provide a detailed reason for each unsupported type — explain what went wrong (missing public constructor, private enum subtype, unsupported shape, etc.) so the user can fix all issues at once
- [ ] Include a hint in the error message telling the user how to enable debug logging

## Configuration

- [ ] Read global options from namespaced `-Xmacro-settings:<library>.<key>=...` (`Environment.typedSettings`), never from JVM properties
- [ ] Read local options from an implicit config, annotations or a DSL, and evaluate them at compile time where possible (`semiEval`)
- [ ] Warn or fail on unknown and invalid values. Do not separate list values with `,` — Scala 3 splits `-Xmacro-settings` on commas
- [ ] Make the `runToExprOrFail` timeout configurable (e.g. `<library>.timeout`) — the 2 s default is too tight for big types on a loaded CI machine

## Introspection

- [ ] Allow inspecting the derivation logic and generated expression on demand, with no external tooling required
- [ ] Provide two mechanisms for enabling logging: an import-based approach (e.g., `import mymodule.debug.logDerivation`) and a scalac option (e.g., `-Xmacro-settings:myModule.logDerivation=true`)

    - [ ] Allow restricting the logging to a single expansion (e.g. with `Environment.isExpandedAt("File.scala:12")`)

- [ ] Log every rule attempt, match, yield, cache hit, and recursive descent — wrap each derivation step in a named scope so the log tells the full story of what happened

    - [ ] Since logging can be expensive, make all logging calls `lazy`/by-name so they are only evaluated when the logging is enabled

- [ ] Document how to profile the expansion: `-Xmacro-settings:hearth.mioBenchmarkScopes=true` and `hearth.mioBenchmarkFlameGraphDir=<dir>` (speedscope flame graphs, see [Micro FP](micro-fp.md#benchmarking-scopes-and-flame-graphs))

## Generated Code Quality

- [ ] Suppress compiler warnings on generated code so users of `-Xfatal-warnings` never need `@nowarn` annotations for code they didn't write

    - [ ] Compile with `-Xfatal-warnings` (at least on CI) and with as much linters enabled as possible - some of your users will surely have them turned on

- [ ] Check runtime utilities with [MiMa](https://github.com/lightbend-labs/mima) — see also the [Library Author Guide](https://docs.scala-lang.org/overviews/contributors/index.html)
- [ ] Generated code must only call public, MiMa-checked runtime helpers — what is accessible at the expansion site differs from what is accessible inside the library
- [ ] Build final instances through factory methods taking lambdas instead of an anonymous class per expansion (fewer classes, less metaspace)
- [ ] Inline built-ins and compile-time-known configuration, and benchmark the generated code (JMH) against hand-written code or the library being replaced

### Keep the generated code linear

Methods over 8000 bytes of bytecode are never JIT-compiled by HotSpot (`DontCompileHugeMethods` is on by default), and 64 KB is "Method too large". Big types reach both limits unless the code grows linearly with the number of fields/subtypes:

- [ ] One local `def` per field/subtype and flat statements in the method that calls them
- [ ] No `try`, `if` or `match` inside argument lists (e.g. a constructor call with one per field) — the operand-stack spills and `StackMapTable` frames grow quadratically; bind each to a `val` first
- [ ] No nesting per field (a closure, `flatMap` or `match` inside the previous field's)
- [ ] Branch in the macro on statically known configuration instead of emitting a runtime `if` per field; for runtime configuration, compute the decision once per call, not per field
- [ ] No local `lazy val`s in code that runs per call — each one allocates a holder and goes through a synchronized initializer
- [ ] Cache summoned instances and helpers once per type class instance, not once per call
- [ ] Do not build tuples or functions whose arity is the number of fields (`Tuple22`/`Function22` limits on Scala 2, boxed `TupleXXL` on Scala 3)

## Cross-Compilation (Hearth-specific)

- [ ] Put shared macro logic in a mix-in trait (`src/main/scala/`) that extends `MacroCommons`
- [ ] Write Scala 2 and Scala 3 adapters in platform-specific source directories
- [ ] Use [Cross Quotes](cross-quotes.md) (`Expr.quote` / `Expr.splice`) for quoting in shared code
- [ ] Test against both Scala versions

## Testing

- [ ] Test each supported type category: primitives, value types, optionals, collections, maps, case classes (zero-field, single-field, nested), sealed traits and enums (with case objects and case classes), tuples, recursive data structures
- [ ] Test Scala 3-only types (Scala 3 `enum` declarations, named tuples, opaque types) in `src/test/scala-3/` (sbt-projectmatrix/sbt 2.0 convention)
- [ ] Test with fixtures defined in another module or compilation unit (types read from the classpath/TASTy), not only in the same file
- [ ] Test that user-provided implicits are preferred over derived ones
- [ ] Test every configuration option, its default, invalid values, and precedence (e.g. annotation > local > global)
- [ ] Test with an integration provider (a macro extension) on the test classpath
- [ ] Test that unsupported types produce clear compile-time error messages with all reasons aggregated — assert the messages, with paths and hints
- [ ] Test that recursive types work transparently — no `lazy val`, no manual instance, no tricks
- [ ] Test that semiautomatic derivation does not cause infinite recursion
- [ ] Never move a failing shared test into version- or platform-specific directories to get CI green — keep a reproducer and track the bug
- [ ] After a macro change, clean the test modules — incremental compilation does not re-expand macros

## Stress Testing

Small models hide size and complexity problems. Run these on Scala 2.13 and 3, with compile-time-evaluated and runtime configuration.

### Big types

- [ ] A wide case class (~100 fields, with default values and `Option`s; much larger fixtures can exhaust the compiler's memory) compiles within the derivation timeout
- [ ] No generated method in it exceeds 8000 bytes — check with a test reading the `Code` attribute lengths of the compiled class files, or with `javap -c -p` (the last instruction's offset)
- [ ] A large sealed hierarchy / enum (100+ subtypes) compiles, and the generated dispatch stays under the same limit
- [ ] Deep nesting (a chain of 20+ nested case classes) compiles and works
- [ ] The same field type repeated many times is derived once (a cache hit in the derivation log), and compile time grows linearly with the number of fields (compare e.g. 50 and 100)

### Nasty recursion

- [ ] Direct recursion: `case class Tree(children: List[Tree])`
- [ ] Mutual recursion: `A -> B -> A`
- [ ] Recursion through `Option`, `Either`, `Map` keys and values, collections and types from integration providers
- [ ] Generic recursive types: `case class Tree[A](value: A, children: List[Tree[A]])`
- [ ] Recursion through a sealed trait / enum (a subtype referring to its parent)
- [ ] A user-provided instance in the middle of the cycle (it is used, and derivation does not loop around it)
- [ ] Recursive types defined in another compilation unit
- [ ] Polymorphic (HKT) derivation over recursive types, e.g. `Functor` for `Tree[A](value: A, children: List[Tree[A]])`

### Benchmarks

- [ ] Benchmarks include a wide type and the runtime-configuration path, not only small models with the default configuration
