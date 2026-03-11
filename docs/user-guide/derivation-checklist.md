# Derivation Checklist

Principles to follow when implementing a type class derivation macro with Hearth. Not every item applies to every type class — skip what genuinely doesn't make sense for your use case.

For detailed guidance on each topic, see [Best Practices](best-practices.md) and the [landing page example](index.md).

## Entry Points

- [ ] Provide two entry points: one that returns a type class instance (`deriveTypeClass[A]`) and one that inlines the derivation result directly (`deriveInline[A]`) — both should delegate to the same core logic
- [ ] Separate runtime utilities (checked by [MiMa](https://github.com/lightbend-labs/mima)) from macro-generated code

## Type Coverage

- [ ] Handle primitives (Boolean, Byte, Short, Int, Long, Float, Double, Char, String) directly, without allocating a type class instance for each
- [ ] Handle optional types (`Option`, etc.)
- [ ] Handle collections (`List`, `Vector`, `Set`, `Array`, `Seq`, etc.) and maps (`Map[K, V]`)
- [ ] Handle case classes — iterate over fields, derive recursively for each field type
- [ ] Handle sealed traits and enums — pattern-match on subtypes, derive recursively for each
- [ ] Handle singletons (case objects, parameterless enum cases) as a dedicated rule
- [ ] Handle value types (classes extending `AnyVal`) — unwrap and recurse on the underlying type
- [ ] Handle tuples, `Either`s, named tuples (Scala 3), union types (Scala 3), and type-parametric types — unless the type class genuinely has no meaningful behavior for them

## Implicit Resolution

- [ ] Always prefer user-provided implicits — check for existing instances before deriving from scratch, so users can override the default behavior
- [ ] Ignore self-summoning — exclude the `derived` method (and, for subtype type classes, the parent library's auto-derivation methods) from implicit search to prevent infinite macro expansion

    - [ ] Additionally, exclude the current type class's type from implicit search, to prevent infinite recursion on `implicit val foo: TypeClass[Foo] = TypeClass.derive[Foo]`
    - [ ] However, if you allow inlined derivation, like `val result = TypeClass.inlined(foo)`, do not exclude the current type from non-autoderived instances, it will let you keep the same behavior as the derivation was not inlined (using user-provided implicit if available, falling back to autoderivation otherwise)

- [ ] Cache every resolved implicit as a `lazy val` — do not summon or allocate the same instance multiple times in the generated code
- [ ] Handle generic contexts — derivation should work correctly when invoked inside a generic function where type parameters are not yet known

## Recursion and Caching

- [ ] Derive recursively by walking the type tree internally, rather than relying on implicit resolution to recurse — this means users never need to learn about semi-automatic derivation or write `implicit val foo: TypeClass[Foo] = TypeClass.derive[Foo]`
- [ ] Cache derived logic for case classes and enums as local `def`s — this keeps generated code compact, avoids the 64kB method size limit, and enables recursive data structures automatically
- [ ] The combination of caching and macro-internal recursion should make recursive types like `case class Tree(children: List[Tree])` work transparently with `val tc = TypeClass.derive[Tree]`, with no user-side tricks

## Error Reporting

- [ ] Aggregate errors across all fields and subtypes rather than failing on the first one
- [ ] Provide a detailed reason for each unsupported type — explain what went wrong (missing public constructor, private enum subtype, unsupported shape, etc.) so the user can fix all issues at once
- [ ] Include a hint in the error message telling the user how to enable debug logging

## Introspection

- [ ] Allow inspecting the derivation logic and generated expression on demand, with no external tooling required
- [ ] Provide two mechanisms for enabling logging: an import-based approach (e.g., `import mymodule.debug.logDerivation`) and a scalac option (e.g., `-Xmacro-settings:myModule.logDerivation=true`)
- [ ] Log every rule attempt, match, yield, cache hit, and recursive descent — wrap each derivation step in a named scope so the log tells the full story of what happened

    - [ ] Since logging can be expensive, make all logging calls `lazy`/by-name so they are only evaluated when the logging is enabled

## Generated Code Quality

- [ ] Suppress compiler warnings on generated code so users of `-Xfatal-warnings` never need `@nowarn` annotations for code they didn't write

    - [ ] Compile with `-Xfatal-warnings` (at least on CI) and with as much linters enabled as possible - some of your users will surely have them turned on

- [ ] Check runtime utilities with [MiMa](https://github.com/lightbend-labs/mima) — see also the [Library Author Guide](https://docs.scala-lang.org/overviews/contributors/index.html)

## Cross-Compilation (Hearth-specific)

- [ ] Put shared macro logic in a mix-in trait (`src/main/scala/`) that extends `MacroCommons`
- [ ] Write Scala 2 and Scala 3 adapters in platform-specific source directories
- [ ] Use [Cross Quotes](cross-quotes.md) (`Expr.quote` / `Expr.splice`) for quoting in shared code
- [ ] Test against both Scala versions

## Testing

- [ ] Test each supported type category: primitives, value types, optionals, collections, maps, case classes (zero-field, single-field, nested), sealed traits and enums (with case objects and case classes), tuples, recursive data structures
- [ ] Test Scala 3-only types (Scala 3 `enum` declarations, named tuples, opaque types) in `src/test/scala-3/` (sbt-projectmatrix/sbt 2.0 convention)
- [ ] Test that user-provided implicits are preferred over derived ones
- [ ] Test that unsupported types produce clear compile-time error messages with all reasons aggregated
- [ ] Test that recursive types work transparently — no `lazy val`, no manual instance, no tricks
- [ ] Test that semiautomatic derivation does not cause infinite recursion
