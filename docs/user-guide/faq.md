# FAQ

## Why is this necessary? Couldn't Scala 3 just support Scala 2 macros?

**No.**

Macros are closely tied to the compiler's internals. Scala 2 and Scala 3 have different compilers with different internals, and such a translation layer
could be more complex than either of the compilers (if anyone would be willing to fund it).

Hearth is possible, because:

 - it's **not a translation layer**, it's **one high-level abstraction over 2 low-level APIs**
 - it's **not aiming to handle all possible cases**, only the common ones
 - it's resorting to some "unprincipled" (but well thought out and tested) hacks under the hood, which will fail in some cases - it might
  be seen as a "non-elegant" solution that occasionally requires a workaround - but by not promising perfection it immediately unblocks people

## Do I need this library if I don't want to cross-compile?

**You don't _need_ it, but it is _still_ useful.**

There are many gotchas when developing a macro, even for common cases:

 - type parameters aren't automatically applied; if a class has type parameters they need to be
   reapplied to its methods and child subtypes
 - constructing pattern-matching on Scala 3 requires building the AST from scratch
 - quasi-quoting expressions on Scala 2 offers no help from the IDE since all it sees is
   an interpolated `String`
 - constructing names for `val`s, `var`s, `lazy val`s, `def`s, bindings in pattern-matching...
   requires "fresh name generation" to avoid accidental name conflicts
 - providing a healthy architecture of a macro - with error handling, user-friendly debugging,
   composing code with FP patterns known from other ecosystems - would feel discouraging if one had to reimplement
   all the utilities from scratch for each new macro

All of the above and more contribute to bad UX for macro maintainers and discourage keeping the same good practices that we
use in other Scala projects. Providing a set of ready-to-use utilities helps make macro development sane.

## Do I need to use all of the utilities?

**No. You can use only the ones that you need.**

The dependencies between the modules are:

```
              Core (Basic Utilities)
                        ┃
       ┏━━━━━━━━━━━━━━━━╋━━━━━━━━━━━━━━━━┓
       ┃                ┃                ┃
Better Printers      Micro FP      Cross Quotes
```

This modular design allows picking only what you want to use:

 * you can use [Better Printers](better-printers.md) without using the rest of the utilities if you only want
   better output
 * you can use [Micro FP](micro-fp.md) if you only want to use a few type classes and/or Macro IO
 * you can use [Basic Utilities](basic-utilities.md) without explicitly depending on Better Printers or Micro FP
    * while core tries to not force you into FP-style, many utilities provide type class instances allowing
      e.g. `.map`, `.traverse`, `.parTraverse`, but also type classes provide instances for: `Id`, `Either`, `Option`, ...
      to allow you working with them without any new collection types
    * [Cross Quotes](cross-quotes.md) are completely optional; they are encouraged if you want to build for both Scala 2 and Scala 3,
      but if you only want to target one of them, you can work with quasi-quotes/quotes/ASTs directly

Utilities work best if used together, but it's easier to learn one tool at a time and reach for the next one
only once we stumble upon the problem that a new tool would solve.

## Does this library completely eliminate the need to learn macros?

**No. Its goal is to handle the most common cases with a sane API, but it's impossible to cover all cases.**

However, it should be able to provide enough utilities, that many macros could be implemented using only
the Hearth's APIs. And for remaining cases it could allow using a sane API for the majority of the time,
reaching for low-level (native) ASTs in one place, and keeping everything high-level everywhere else.

To implement these use-case-specific utilities you would have to know the macro API for the Scala version,
that you are working with, but since there should be only a small portion of the whole codebase, it would be easier
to test them (since you should be able to trust the behavior of the existing utilities).

## Is Cross-Building a macro supported?

**No.**

Scala 3 supports a concept described as [cross-building a macro](https://docs.scala-lang.org/scala3/guides/migration/tutorial-macro-cross-building.html).

What it means is that in Scala 3 code one is able to write:

```scala
object Macros {

  // this will be called only by Scala 2.13
  def macroMethod = macro scala2macroMethodImpl

  // this will be called only by Scala 3
  inline def macroMethod = ${ scala3macroMethodImpl }
}
```

and then compile and publish it as a Scala 3 artifact, and it will expand macros in both Scala 3 and Scala 2.13 (using this artifact via `Cross.for2_13Use3`).

The catch is that:

 1. both `scala2macroMethodImpl` and `scala3macroMethodImpl` have to be available where `macroMethod` is defined
 2. Scala 3 cannot compile the Scala 2's quasi-quotes (it can compile manually written ASTs)
 3. you cannot have on the classpath two versions of the same class - e.g. compiled once by 2.13 and once by 3

In practice it makes it virtually impossible to use when you need to provide a type class derivation via the companion object:

 1. the companion object would have to provide both `def macro` and `inline def`
 2. that means two non-abstract, stable definitions must exist, one with the compiled macro for Scala 2.13 and one for Scala 3
 3. if quasi-quotes are necessary (most of the time they are), the Scala 2.13 macro code would have to be moved to another module
 4. inside the macro you almost certainly need a `c.Type` definition of the type class
 5. that has to be defined in the same file as its companion object, which would live in the module that depends on the module containing the macro
 6. even if you find a creative workaround, the 2.13-only module with quasiquotes and the Scala 3 module with quotes would have to depend on
    the same module (either 2.13 or 3) to avoid different runtime versions of the same type class

It already creates something like:

```
Scala 2.13 OR 3 <──── Scala 2.13 ONLY <──── Scala 3 ONLY
└ runtime code        └ quasiquotes         ├ quotes
                                            ├ def macro using quasiquotes
                                            └ inline def using quotes
```

so even if we don't want a type class derivation defined in a companion object, it's a rather complex multimodule setup. If you want derivation (even the semi-automatic one),
you have to introduce some non-obvious solutions that make even the simplest macro feel rather scary.

I have seen very few examples of macros that could use cross-building, and none of them implemented a type class derivation.

Meanwhile, Hearth has [cross-quotes](cross-quotes.md) which are implemented by having something implemented differently on Scala 2 and on Scala 3,
while the types are named the same way, so using Hearth is already violating the requirements of this scheme.

Changing Hearth to allow it would be possible, but it would introduce a huge burden on the maintainers without providing much value to (most) users.
It allows sandwich, however, with macros being expanded by Scala 2.13 only or Scala 3 only (`2.13 module <- 3 module <- 2.13 module` module or `3 module <- 2.13 module <- 3 module`).

## What are the requirements to use this library?

Currently Hearth is built against:

 - **Scala {{ scala.2_13 }}**
 - **Scala {{ scala.3 }}**

with **JDK 11** bytecode.

The features introduced by later versions of Scala/JDK (when already supported) are handled via runtime reflection and feature discovery.

These requirements might change, but we'll **try to stay on the lowest version** of Scala/JDK possible, **while testing against the newest**
(currently {{ scala.newest_2_13 }}, {{ scala.newest_3 }}).

## Can I use micro-FP/MIO in the production code?

**It's not recommended.**

It's not common knowledge, but macros are _always expanded by the JVM_ - even if you are running them on a Scala.js or Scala Native build,
because Scala.js/Native actually create _both_ JVM bytecode _as well as_ the code for the other platform.

At the same time, when Scala.js/Scala Native are linking the final code, they only do it for the code that was actually used and called,
which means the macro code is stripped and it is _not_ checked for JVM-only code.

This allows us to, e.g., implement `DirectStyle` using features that are unavailable on Scala.js/Scala Native and would not be valid code
for cross-compilation. But if these "illegal" calls only happen in a macro, which is stripped before linking by Scala.js/Scala Native,
the issue never occurs.

Hearth is intended to be used within macros.

So if you only use it within macros, it works for all versions of Scala: JVM, JS, and Native. If you use it in production code
it might not link correctly on Scala.js/Scala Native.

Also, its MIO is _not_ truly parallel - such functionality is not needed for macros.

## How to fix `forward reference to value ... defined on line ... extends over definition of value ...`?

It happened when you write:

```scala
// With Cross-Quotes
implicit val someType: Type[A] = Type.of[A]
```

Depending on the platform, it translates to:

```scala
// Scala 2
implicit val someType: Type[A] = c.weakTypeTag[A]
```

or

```scala
// Scala 3
implicit val someType: Type[A] = scala.quoted.Type.of[A]
```

In both cases the native macro utility summons `Type[A]` - since we are just declaring `implicit`/`given` of that type
we are creating a circular dependency. We have to prevent the utility from summoning its own result by:

```scala
// In one scope, that does not define implicit Type[A]:
private val SomeType = Type.of[A]

locally {
  // In another scope, where we can expose a computed result as an implicit:
  implicit val someType: Type[A] = SomeType
}
```

While it's a bit mundane, it makes it rather explicit which types we are using or not, and where.
