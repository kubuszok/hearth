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
 3. if quasiquotes are necessary (most of the time they are), the Scala 2.13 macro code would have to be moved to another module
 4. inside the macro you almost certainly need a `c.Type` definition of the type class
 5. that has to be defined in the same file as its companion object, which would live in the module that depends on the module containing the macro
 6. even if you find a creative workaround, the 2.13-only module with quasiquotes and the Scala 3 module with quotes would have to depend on
    the same module (either 2.13 or 3) to avoid different runtime versions of the same type class

It already creates something like:

```
Scala 2.13 OR 3 <---- Scala 2.13 ONLY <---- Scala 3 ONLY
- runtime code        - quasiquotes         - quotes
                                            - def macro using quasiquotes
                                            - inline def using quotes
```

so even if we don't want a type class derivation defined in a companion object, it's a rather complex multimodule setup. If you want derivation (even the semi-automatic one),
you have to introduce some non-obvious solutions that make even the simplest macro feel rather scary.

I have seen very few examples of macros that could use cross-building, and none of them implemented a type class derivation.

Meanwhile, Hearth has [cross-quotes](cross-quotes.md) which are implemented by having something implemented differently on Scala 2 and on Scala 3,
while the types are named the same way, so using Hearth is already violating the requirements of this scheme.

Changing Hearth to allow it would be possible, but it would introduce a huge burden on the maintainers without providing much value to (most) users.

## What are the requirements to use this library?

Currently Hearth is built against:

 - **Scala {{ scala.2_13 }}**
 - **Scala {{ scala.3 }}**

with **JDK 11** bytecode.

The features introduced by later versions of Scala/JDK (when already supported) are handled via runtime reflection and feature discovery.

These requirements might change, but we'll **try to stay on the lowest version** of Scala/JDK possible, **while testing against the newest**.

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
