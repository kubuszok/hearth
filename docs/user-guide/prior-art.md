# Prior Art & Influences

Hearth didn't emerge in a vacuum. It builds on ideas, patterns, and hard-won lessons from several projects across the Scala ecosystem. This page credits those influences and tells the story of how the ecosystem evolved toward a library like Hearth.

## Reusable Macro Utilities

[AVSystem's scala-commons](https://github.com/AVSystem/scala-commons) was one of the earliest Scala 2 projects to extract reusable macro utilities into a shared foundation. Its `MacroCommons` trait gathered frequently needed helpers — fresh name generation, type manipulation, implicit lookup — into a single mix-in that multiple macro-heavy modules could extend. The project also included `TypeClassDerivation`, a framework for deriving type class instances for product and sum types, and `GenCodec`, a serialization codec built on top of it.

What scala-commons demonstrated was that macro code didn't have to be a pile of one-off copy-paste. Common operations could be factored out, tested once, and reused. Hearth's own `MacroCommons` trait and its approach to composable utilities trace directly back to this idea.

## Type Class Derivation Patterns

[The Type Astronaut's Guide to Shapeless](https://underscore.io/books/shapeless-guide/) gave the Scala community a mental model for thinking about type class derivation generically. By explaining how `HList` and `Coproduct` representations could be used to derive instances for arbitrary product and sum types, it turned type class derivation from a mysterious art into something that could be taught, learned, and reasoned about. Even projects that never used Shapeless directly benefited from the vocabulary and conceptual framework it established.

[Circe](https://circe.github.io/circe/) took these ideas and turned them into a practical, widely-adopted JSON library. Its distinction between semi-automatic and automatic derivation became a template that many other libraries followed: semi-automatic derivation gives users explicit control over which types get instances, while automatic derivation provides convenience at the cost of longer compile times and less predictable implicit resolution. Circe showed what a good derivation API should look like from the user's perspective — but its reliance on Shapeless (and later Scala 3 mirrors) meant that the underlying machinery was outside the library author's control, making deep customization and cross-compilation difficult.

## Improving the Developer Experience

[Magnolia](https://github.com/softwaremill/magnolia) reimagined what the derivation API could feel like for library authors. Its `join` and `split` interface made writing derivations far more intuitive: you described how to handle products and sums in high-level terms, and Magnolia took care of the underlying representation. This was a genuine UX improvement over working with raw `HList`/`Coproduct` or writing boilerplate-heavy macro code from scratch.

However, Magnolia stayed within the boundaries of existing macro practices. It handled the common case well but didn't address recursion (users still needed to define implicits at every level of a recursive type), and its cross-compilation story between Scala 2 and Scala 3 required maintaining two separate implementations with different APIs.

## Solving Recursion and Cross-Compilation

[jsoniter-scala](https://github.com/plokhotnyuk/jsoniter-scala) showed that semi-automatic derivation could handle recursive types out of the box if the macro itself managed the recursion internally. Instead of relying on implicit resolution to recurse into nested types (which breaks on recursive structures without explicit user intervention), jsoniter-scala's macros walked the type tree themselves, generating the complete codec in a single expansion. This eliminated an entire class of user-facing errors and proved that "the macro handles recursion" was a viable and superior approach.

[Chimney](https://chimney.readthedocs.io/) and its extracted [chimney-macro-commons](https://github.com/scalalandio/chimney-macro-commons) tackled two problems at once. First, Chimney's macros used internal recursion to handle arbitrarily nested transformations, reinforcing the pattern that jsoniter-scala had pioneered. Second, chimney-macro-commons extracted cross-compilation utilities — a shared API that could target both Scala 2 and Scala 3 macro systems from a single codebase. This was the first serious attempt at making macro cross-compilation practical for a non-trivial library. Hearth's cross-compilation approach learned directly from chimney-macro-commons' successes and limitations.

[pipez](https://github.com/MateuszKubuszok/pipez) was an earlier project by Hearth's author that spearheaded the cross-compilation approach used in Chimney 0.8.0 and chimney-macro-commons. It explored how far the abstraction over differences between Scala 2 and Scala 3 macro systems could be pushed, testing which patterns could be shared and where platform-specific code was unavoidable. The lessons from pipez — about what abstractions were practical and where they broke down — fed directly into both Chimney's cross-compilation story and Hearth's design.

## The Synthesis

Looking at the arc: scala-commons showed that macro utilities could be reusable. Shapeless and Circe established the vocabulary and patterns for type class derivation. Magnolia improved the developer experience of writing derivations. jsoniter-scala proved that macro-internal recursion was practical. pipez pioneered the cross-compilation approach that Chimney 0.8.0 and chimney-macro-commons then proved could work for real-world macros at scale.

Hearth synthesizes these lessons into a single, cohesive library: reusable utilities with a high-level API, macro-internal recursion as the default, cross-compilation as a first-class concern, and the developer experience improvements that the ecosystem had been converging toward. It stands on the shoulders of these projects, and this page exists to acknowledge that debt.
