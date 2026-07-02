# Binary compatibility and mix-ins

Hearth is a published library, so we check binary compatibility with [MiMa](https://github.com/lightbend-labs/mima)
against the last released version on every build. This document explains **which** binary-compatibility breakages
actually affect our users and which are safe to suppress — a distinction that is unusual for Hearth because of how its
public API is shaped.

If you are only here to make CI green: **do not blanket-suppress MiMa errors.** Read the
[decision procedure](#decision-procedure) below and suppress *only* the breakages that are provably not observable by
users, each with a comment citing why.


## Where MiMa is configured

Everything lives in `build.sbt`:

- `mimaPreviousVersion` — the last released version we compare against. **Bump it on every release.**
- `mimaSettings` — sets `mimaPreviousArtifacts` for the published modules
  (`hearth-better-printers`, `hearth-cross-quotes`, `hearth-micro-fp`, `hearth`, `hearth-munit`) and
  `mimaFailOnNoPrevious := true` for them (a missing previous artifact is an error, never a silent skip).
- `mimaBinaryIssueFilters` — the allow-list of suppressed breakages. Keep it minimal and commented.

The previous artifact is built with `.cross(crossVersion.value)` rather than `%%`, so each project compares against the
**platform-matching** previous artifact (`hearth_2.13`, `hearth_sjs1_2.13`, `hearth_native0.5_3`, …). Using plain `%%`
would make every platform resolve the JVM artifact (`hearth_2.13`), and MiMa would then report all JVM-only classes
(e.g. the `hearth.std.extensions.*ForJava*` providers) as "missing" on JS/Native — a false positive.

`mimaReportBinaryIssues` is already part of the `ci-*` aliases, so CI runs it for every published module on every
platform (JVM, JS, Native) and both Scala versions. This is supported: sbt-mima 1.1.6 reads the JVM `.class` files that
scalac emits alongside the `.sjsir` / native IR, so MiMa works on Scala.js and Scala Native for **both** Scala 2.13 and
Scala 3. `hearth-cross-quotes` is compiled but not MiMa-reported (it is a Scala 3 compiler plugin / a Scala 2 macro
library, not a normal library API surface).


## Why Hearth's public API is unusual

Hearth's public surface is not a set of concrete classes with methods. It is a set of **traits** — `MacroCommons` and
the traits it mixes in (`Types`, `Exprs`, `Methods`, `Classes`, …). The intended usage is:

```scala
// User code — a macro bundle that mixes Hearth in:
final class MyMacros(val c: blackbox.Context) extends MacroCommonsScala2 with MyOwnLogic
// or on Scala 3:
final class MyMacros(q: Quotes) extends MacroCommonsScala3(using q), MyOwnLogic
```

Two facts follow from this:

1. **We are the only ones who implement these traits.** `MacroCommonsScala2` / `MacroCommonsScala3` and the
   platform-specific traits they pull in are maintained exclusively by us.
2. **Users mix these traits into their own final bundle classes.** The linker resolves the mixin composition of the
   user's bundle against *whichever version of Hearth is on their classpath at build time*.

This is what makes the usual "adding a member is safe" intuition only partially true here.


## The mechanics: trait members become forwarders

When a trait is mixed into a class, the Scala compiler emits, in the mixing class, **forwarder** members for the
trait's concrete members (and requires implementations for its abstract members). The set of forwarders/obligations is
baked into the mixing class's bytecode **at the version it was compiled against**.

So consider a user's bundle `MyMacros` compiled against Hearth *N*, then run against Hearth *N+1*:

- If *N+1* **added a top-level member to a trait that `MyMacros` mixes in** (a new `val`/`var`/`object`, or a new
  abstract `def`), then `MyMacros`'s bytecode does not carry the forwarder/implementation that *N+1*'s linearization now
  expects. The result is a `LinkageError` / `AbstractMethodError` at class-load or call time. **This is a real, breaking
  change** and MiMa reports it as `ReversedMissingMethodProblem`, `NewAbstractMethodProblem`, or
  `InheritedNewAbstractMethodProblem`. **Keep these.**

- If *N+1* only **changed something nested** — a method/`val`/`object` added to a *nested* class or object such as
  `Classes#CaseClass`, or a `def` local to another member — then no new obligation lands on `MyMacros`. Those nested
  definitions are only ever instantiated or mixed in by **Hearth's own code**, which ships in the very same artifact as
  the interface. When the user upgrades to *N+1* they evict *N*'s `hearth.jar` and get *N+1*'s interface **and** *N+1*'s
  implementation atomically. There is no window in which the user's bytecode sees the new interface but an old
  implementation, so the change **can never be witnessed as a link error by user code**.

Put differently: the only observer who could ever notice a nested change is Hearth itself — and Hearth is part of what
gets evicted, so it upgrades in lockstep. That is why nested additions are not user-observable even when they are, in a
strict bytecode sense, "different".

> Note: MiMa does not even *report* adding a method to a nested **class** — added class methods are forward-compatible.
> The nested exception matters for the cases MiMa *does* flag: members added to a nested **trait** or **object** that it
> turns into forwarders/obligations.


## Decision procedure

When MiMa reports a problem, classify the changed member:

1. **Is it a top-level member of a trait that users mix in** (directly or transitively through `MacroCommons`)?
   → **Breaking. Do not suppress.** Either revert the change, make the addition non-breaking (e.g. give a trait `def` a
   concrete default body instead of leaving it abstract, so no new obligation is imposed — MiMa may still flag it, in
   which case re-evaluate), or accept it and bump the **major**/**minor** version per the project's versioning policy.

2. **Is it nested inside a class/object/def, reachable and implemented only by Hearth's own code?**
   → **Not user-observable. Safe to suppress** with a `mimaBinaryIssueFilters` entry that **cites why** it is nested and
   not observable.

3. **Not sure?** Treat it as breaking (case 1). The cost of a false "safe" is a runtime `LinkageError` in a user's
   macro; the cost of a false "breaking" is a version bump.


## How to suppress (only case 2)

Add an entry to `mimaBinaryIssueFilters` in `build.sbt`, with a comment explaining why it is not user-observable:

```scala
mimaBinaryIssueFilters ++= Seq(
  // Nested addition: `newHelper` was added to the nested trait `SomeTrait#SomeNestedTrait`, which is only mixed in by
  // Hearth's own code (evicted together with this interface), so no user bundle carries a forwarder obligation for it.
  exclude[ReversedMissingMethodProblem]("hearth.typed.SomeTrait#SomeNestedTrait.newHelper")
)
```

Never add a filter that matches a top-level trait member, and never use broad wildcards that could also hide a genuine
top-level breakage.


## On releases

Bump `mimaPreviousVersion` in `build.sbt` to the version you just released, so the next development cycle is checked
against it. When you do, prune any `mimaBinaryIssueFilters` entries that were only relevant to older comparisons.
