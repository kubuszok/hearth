# Guidelines for code style

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

## Code Organization Patterns

1. **Cross-version code sharing:**
   - Shared code goes in `src/main/scala/`
   - Scala 2-specific code in `src/main/scala-2/`
   - Scala 3-specific code in `src/main/scala-3/`
   - Newest Scala 3 features in `src/main/scala-newest-3/`

2. **Cross-platform code sharing:**
   - Shared platform code in base `scala/` directory
   - Platform-specific code in `scalajs/`, `scalajvm/`, `scalanative/`

3. **Architectural pattern:**
   - Uses trait-based polymorphic embedding (Hofer et al., GPCE 2008)
   - Abstract traits define operations and types
   - Platform/version-specific implementations via trait mixins
   - Shared logic inherits from abstract traits

## Code Style Requirements

**Mandatory rules (enforced by CI):**

1. **No braceless Scala 3 syntax** - Always use braces for blocks
   ```scala
   // ❌ WRONG
   if condition then
     doSomething()

   // ✅ CORRECT
   if (condition) {
     doSomething()
   }
   ```

2. **Nested package declarations** (not dot notation)
   ```scala
   // ❌ WRONG
   package hearth.std.extensions

   // ✅ CORRECT
   package hearth
   package std
   package extensions
   ```

   **Rationale:** Reminds us that package visibility is hierarchical in Scala

3. **Scalafmt compliance:**
   - Scala 2 shared code: `-Xsource:3` syntax (enables some Scala 3 features)
   - Scala 3-specific code: Full Scala 3 syntax
   - Configuration in `.scalafmt.conf`

4. **Cross-compilation is mandatory:**
   - Code must work on both Scala 2.13 and Scala 3
   - If a solution only works on one version, it's not acceptable
   - Even Scala 3-only features must not break cross-compilation

5. **No external dependencies:**
   - Only stdlib + macro infrastructure allowed
   - FP utilities already provided in `hearth-micro-fp`