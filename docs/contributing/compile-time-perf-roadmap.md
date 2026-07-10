# Compile-time performance roadmap

Follow-up work for making Hearth-based macro expansion cheaper, grounded in JFR profiling of a
macro-dense Scala 3 Chimney derivation (~160 independent `transformInto` expansions). See the
`hearth-macro-perf` skill for the audit checklist and the profiling methodology (JFR on HotSpot
Temurin, fold stacks, filter to `hearth`/`chimney.internal.compiletime` frames).

## What the profile established

Self-time of macro expansion breaks down as ~40% compiler type/reflection ops + 23% jdk/stdlib
(Hearth's own collections/boxing/strings). **MIO is 0.5% self-time** — the effect system is a thin
pass-through wrapper, *not* a bottleneck; leave it alone. The old-vs-new diff (pre-Hearth Chimney
vs Hearth-based) showed macro expansion roughly **doubled** while non-macro compilation stayed
flat, and essentially all of the increase is two Hearth layers that don't exist in the old design:

- **macro-extension provider scan** (~22% of macro; 0% in old Chimney),
- **cross-quotes / `Ctor.Bounded.unapply` type-constructor matching** (~20%; 0% in old).

Method conversion (`Type.methods` → `toTyped` on *all* methods) is another ~12.6%.

Already shipped (PR #347): lazy provider skip reasons + method-list caching (macro self-samples
4374 → 3892, macro's share of the compile 27.2% → 24.9%). This roadmap is the next layers.

---

## The end-to-end target (the number that matters)

The sample-fraction deltas above are *diagnostic* — they tell us where the time goes. The
**north-star metric is the whole-module compile-time ratio: Hearth-based Chimney vs the old
`macro-commons` Chimney**, per Scala version. Known state so far (wall-clock of a Chimney module
compile, old design as 1.0×):

| | Scala 3 | Scala 2 |
|---|---|---|
| pre-optimization (Hearth as-migrated) | ~3.0× | ~2.0× |
| after the optimizations so far | ~1.7× | ~1.5× |
| goal | → 1.0× | → 1.0× |

Still too much, but the right direction. This ratio — not the profiler sample fractions — is what
each roadmap item is ultimately judged against, so it needs a **repeatable measurement harness**:

1. **Two checkouts, same modules.** Current Hearth-based branch vs the last pre-migration
   `macro-commons` commit (e.g. the one behind CI run `22825414357`; profiled here via a disposable
   local clone — a `git worktree` breaks sbt-dynver's jgit, so use `git clone --local`). Compile the
   *same* module set on both.
2. **Compile only — no test run.** Time `<module>/Compile/compile` (and `Test/compile` separately if
   the derivations live in tests), not `test`; a full `test` bundles unrelated runtime work.
3. **Both Scala versions**, since the ratio differs markedly (3× vs 2×) — Scala 3's quote/TASTy path
   and Hearth's cross-quotes layer are heavier than Scala 2's.
4. **Warm + repeated.** Clean the module (macros do not re-expand incrementally — see the bug-fix
   workflow), then take the best of N runs on an otherwise-idle machine; wall-clock bounced ±20–40%
   on a loaded box during this work.
5. **Confounds to report, not hide:** the two commits also differ in Scala-3 minor version and in
   the `summonIgnoring` switch (both landed in `2.0.0-development` before Hearth), so part of the
   ratio is *not* attributable to Hearth. The profiling here found the compiler's own type-op
   fraction unchanged (19.8% ↔ 19.8%), i.e. the version confound is small for the *macro* portion —
   but say so explicitly when quoting a ratio.

Track the ratio (both versions) as a row per optimization landed, so the roadmap's progress is
visible against the 1.0× goal rather than only as isolated sample-fraction wins.

---

## Part A — Lazy `Method` fields ("half the Methods utils as `lazy val`")

**Root cause** (`Methods.scala` `UntypedMethod.toTyped`): converting one `UntypedMethod → Method`
eagerly resolves the *entire* signature — `resolvedParamTypesByName` (does `safeMemberType` +
`PolyType.appliedTo` + a `MethodType`/`PolyType` walk), `resolveExpectations`, each
`Parameter.tpe`, `knownReturning`. But the hot callers use a fraction: `methodGetter(name)` needs
`name` + return type; provider scans need name/shape; `isConstructorArgument`/`isNullary` need only
arity. For the common "convert all, use one" pattern (`methodGetter`/`setterCandidates` over a
product), ~24 of ~25 methods have their full signature resolved and thrown away.

**Findings from a first implementation attempt (read before starting):**

- **Parameters are resolved TWICE per `toTyped`** (`UntypedMethodsScala3.toTyped`): once inside
  `typedExpectations` (`up.asTyped[Instance]` per `NeedsValues` clause) and once for
  `totalParams = untyped.parameters.asTyped[Instance]`. Each call rebuilds the full
  `typesByParamName` map (`safeMemberType` + `appliedTo` + `MethodType`/`PolyType` walk). Dedup
  alone (resolve once, share slices between expectations and totalParameters) halves the cost with
  zero laziness risk. Check the Scala 2 `toTyped` (UntypedMethodsScala2.scala:494) for the same shape.
- **`Parameter.tpe` deferral is shape-coupled:** `UntypedParameters.toTyped` decides which params
  exist via `typesByParamName.get(paramName)` inside a `flatMap` (UntypedMethodsScala3.scala:171-182)
  — a param missing from the map is silently DROPPED (defensive, e.g. extension-method receiver
  alignment). So making `Parameter.tpe` by-name does not by itself defer the map: the existence
  check forces it. To defer, the shape decision must be decoupled first (e.g. prove the drop is
  impossible for the clause shapes that reach here, or keep-and-throw-lazily) — behavioral risk,
  needs its own test pass.
- **`returnType` is the cleanest deferral target:** its *Option-ness* is decided by cheap flags
  (`hasUnresolvedTypeParams` / `isNoSymbol` / `isConstructor`); only the value inside `Some` pays
  `safeMemberType`. Deferring means `buildChain(returnType: Option[() => ??])` and a lazy
  `knownReturning` on the chain nodes — `hadKnownReturning = returnType.isDefined` stays cheap.
  Callers like Chimney's `methodGetter` force `knownReturning` only for name-matched candidates, so
  ~24 of ~25 methods per type never pay it.
- `Parameter` has NO companion (`new` on Scala 2, universal apply on Scala 3, 5 construction sites);
  a by-name ctor param is source-compatible at all sites but flips the erased signature → MiMa filter.

**Steps**

1. Audit `Method` / `Parameter` / `MethodExpectation` fields into three tiers:
   - *Eager + cheap* → keep eager: `name`, `symbol`, clause/param **arity**, modifier flags that
     are single symbol-flag reads.
   - *Expensive + often-unused* → make `lazy val` (fed by a by-name/thunk from `toTyped`): resolved
     **parameter types**, **return type** (`knownReturning`), the **expectations / `AppliedState`**
     chain, anything doing `appliedTo` / substitution.
   - *Structural* → builder-chain steps, resolved on demand as today.
2. Restructure `toTyped` to capture the cheap identity eagerly and defer the signature-resolving
   closures into `lazy val`s on the `Method`. `Parameter.tpe` becomes lazy (or `parameters`
   resolves types on first access).
3. **Compounds with the #347 method-list cache:** cached `Method`s never accessed beyond `name`
   never pay signature resolution at all — the cache becomes "list cheaply, resolve per-touch"
   instead of "convert all eagerly, reuse."
4. **Guards:** `Method` is public → MiMa (lazy fields compile to a private backing + accessor,
   compat-safe; verify). Watch **evaluation-error timing** — a malformed signature now surfaces on
   first *access* rather than at conversion; full `quick-test` must confirm no test depends on eager
   failure.

**Expected:** a large cut of the 12.6% for search-by-name callers.

---

## Part B — `Type.Ctor` traversal heuristics (faster provider/type dispatch)

The `Ctor.Bounded.unapply` `'[HKT[a]]` match is the ~20% cost; the scan runs it ~15× per field.
Three layered cheap prefilters, cheapest first.

### B1 — Cheap-flag reject (O(1), dotty-memoized denotation reads)

Before any `<:<` or match, check `A.typeSymbol` flags/identity. Fastest rejects:
`A.typeSymbol == defn.ArrayClass` (Array provider), "is `A` even a class/trait?" (rejects
primitives / function types for collection providers), case/sealed/module flags where a provider
only wants certain shapes.

### B2 — Head-symbol index (the structural win)

Split each companion's providers:

- **Exact-constructor providers** (Array, IArray, `java.util.*`, EnumSet, …) → a
  `HashMap[headSymbol → provider]`. For a type `A`, `A.dealias.typeSymbol` → O(1) lookup to the one
  candidate (or none) instead of iterating all providers.
- **Subtype providers** (Iterable, Option, Either — match *subtypes*) → a small "always-try" bucket
  gated by the `mightMatch` cheap `<:< Bound[Any]` (see the negative-gate design below). There are
  only a few of these.

A scan becomes *one map lookup + a couple gated `<:<`s*, not ~15 full matches. This is the
**systematic version of the per-provider negative gate** — do the gate first (proves the soundness
pattern cheaply), then generalize to the index.

### B3 — Compute `A`'s fingerprint once (`Type.Cache`)

Dealiased head symbol + arity + key flags computed once per type per expansion and reused across
all provider checks, so no provider re-dealiases `A`.

**Arity grouping** is mostly already implicit (Ctor1 shapes live in `IsCollection`/`IsOption`,
Ctor2 in `IsMap`/`IsEither`), so within a companion providers are same-arity — the leverage is
B1/B2, not further arity splitting.

---

## Prerequisite / already-designed: the sound negative gate

A `mightMatch[A](tpe): Boolean = true` hook was added to `StdExtensions#ProvidedCompanion#Provider`
(defaults to `true` = no behavior change). It is a cheap **sound** negative pre-filter:

- **covariant bound** (Option, Iterable, Seq, Either, …): `tpe <:< Bound[Any]` — sound because
  `A <: Bound[X] ⟹ A <: Bound[Any]` by covariance; uses the cached `<:<`, skipping the QuoteMatcher
  + arg extraction.
- **exact/invariant constructor** (Array, IArray, `java.util.*`): `ctor.sameTypeConstructorAs(...)`.

Remaining wiring: gate in `firstMatch` and `CtorLikes.parse`; when a provider is gated out, record a
**constant lazy skip reason** (e.g. `"pre-filtered: cannot match"`) rather than nothing — otherwise a
total failure where everything gated out would report the misleading "No providers registered".

**Soundness is the whole ballgame:** `mightMatch` must never return `false` for a type the provider
could build — a wrong gate *silently* drops support. The full `'[HKT[a]]` match is
**conformance-based** (it accepts any `T <:< HKT[a]`, including abstract types bounded by `HKT[X]`
and subclasses like `ArrayList <: java.util.List`), so the gate must be conformance-based too:

- **Covariant constructor** (`Option`, `Iterable`, `Iterator`, `Either`, `Try`): `tpe <:< Bound[Any]`
  is sound (`T <: F[X] ⟹ T <: F[Any]` by covariance) and cheap (cached `<:<` with a fast negative).
- **Non-generic type** (`String`, the `java.lang.*` boxes): `tpe <:< TheType` is sound.
- **Invariant constructor** (`Array`, `java.util.*`, `Optional`): `<:< F[Any]` is UNSOUND-in-reverse
  (it's `false` for `F[Int]`, so it would gate out real matches). `sameTypeConstructorAs` (head-symbol
  compare) is also unsound: it rejects abstract types bounded by `F[X]` and subclasses, which the
  full match accepts. The sound form is a **wildcard bound** (`tpe <:< F[?]`) — verify cross-quotes
  can express `Type.of[F[?]]` on both Scala versions; where it cannot, **leave the default
  `mightMatch = true`** (an ungated provider is merely slower, never wrong).
- Bottom types already short-circuit before providers (`isBottomType`); `Some[_]` is `<: Option[Any]`
  so the gate admits it and `parse` itself skips it (correct).

Note the 8 `IsValueTypeProviderForJava*` box providers already do a cheap `=:=` in `parse` (no quote
match) — no gate needed; `IsValueType`'s AnyVal provider already self-gates with `<:< AnyVal`.

**Gate largely subsumes B2:** once every built-in provider either gates cheaply or is genuinely
conformance-broad, a scan is ~15 cheap checks + full parses only for plausible matchers — which is
most of what the head-symbol index would buy. Build B2 only if post-gate profiles still show scan
cost.

---

## Part C — `Type.Cache`: bucketing + Cross-Quotes scoping (DONE) and Chimney adoption

**Analysis answers (2026-07-10):**

1. *"Do we always need to initialize all methods/parameters when looking for some?"* — No, and the
   split is: **`UntypedMethod` construction is cheap** (wraps a `Symbol` + flags; `typeParameters`
   already lazy) — the untyped enumeration costs one members walk (+ the #327 baseClasses sweep on
   Scala 3). The expensive step is **`toTyped`** (parameters resolved twice + `returnType`, see Part
   A findings). So a by-name lookup should filter at the UNTYPED level (symbol-name compare is
   free) and convert only matches. DONE: `Method.unsortedMethodsNamed[A](name): List[Method]` +
   `Type[A].unsortedMethodsNamed(name)`, cached per `(type, name)` (a `Type.Cache` holding the
   untyped list + a mutable `Map[String, List[Method]]`). Switched internally: `canonicalCopyMethod`
   ("copy") and the 8 collection providers' Builder-"result" lookups. REMAINING: the 8
   `IsValueTypeProviderForJava*` box lookups ("valueOf"/"xxxValue" — lazily forced, low value) and
   the big consumer, Chimney's `methodGetter`/`setterCandidates` (a Chimney PR after release —
   those never convert the ~24 non-matching methods of a type only queried by name).

2. *"Can `Type.Cache` be bucketed by something cheap?"* — Yes, DONE: entries are hash-bucketed by
   the **dealiased type symbol** (`UntypedTypeModule.cacheBucketKey`; `NoSymbol` → shared sentinel),
   so the `=:=` scan only runs against same-symbol candidates. This is sound because it mirrors
   `isSameAs`'s fast negative (differing non-NoSymbol dealiased symbols ⟹ not `=:=`) AND because a
   cache miss merely recomputes — bucketing can never produce a wrong hit, only (for exotic
   NoSymbol-vs-symbol pairs) a harmless miss. Further discriminators (arity, flags packed into an
   Int) can be added to the key later, but symbol identity already scatters near-uniformly.

3. *"Chimney should use Hearth's `Type.Cache`"* — **it could not, until now**: Chimney's `TypeCache`
   (MacroCommonsCompat.scala) is NOT a plain reimplementation — it partitions entries by
   `cacheScopeToken` (the ACTIVE `Quotes` per `Expr.splice` on Scala 3) because cached values embed
   materialized `Expr`s, and handing an `Expr` across splice scopes trips `-Xcheck-macros`
   ScopeException (bites when one expansion evaluates several splices, e.g. Iso/Codec). Hearth's
   `Type.Cache` lacked this — which also means Hearth's own `firstMatchCache` (std provider views
   close over `factoryExpr`s) and `methodsOfCache` carried the same LATENT hazard. Now DONE:
   `Type.Cache` partitions by `CrossQuotes.ctx` (the active `Quotes` on Scala 3; the constant
   blackbox `Context` on Scala 2), fixing the hazard and making it a superset of Chimney's cache.
   **Remaining (Chimney PR, after a Hearth release/snapshot):** replace the 8 `new TypeCache[...]`
   sites (`TotallyBuildIterables`, `PartiallyBuildIterables`, `OptionalValues`, `SealedHierarchies`,
   `Total/PartialOuterTransformers`, …) with `Type.Cache` (`cache(key)(value)` →
   `cache.getOrPut(key)(value)`), then delete `TypeCache` + `cacheScopeToken` from
   `MacroCommonsCompat`.

## Sequencing & measurement

Biggest-bang order when credits/time are tight:

1. **Negative gate** — smallest, proves the soundness pattern.
2. **Part A (lazy `Method`)** — broad, low-risk, compounds with #347.
3. **Part B2 index** — the bigger refactor, once the gate validates the reject logic.

Every step:

- **Full `quick-test` on both Scala versions.** An unsound gate/index → a *silently* unrecognized
  type → failed derivation; lazy fields → an error-timing shift. The suite is the safety net.
- **Re-profile** the dense `ScaledProfile` load (JFR on Temurin; see the
  `hearth-compile-time-profiling` memory / the `hearth-macro-perf` skill) and compare
  `method-conversion%` and `Bounded.unapply%` before/after. Wall-clock is noisy — trust the
  sample-fraction deltas.
