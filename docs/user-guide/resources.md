# Resources & Further Reading

A curated collection of external references that complement the Hearth documentation.

## Derivation Theory

- [Sanely Automatic Derivation](https://kubuszok.com/2025/sanely-automatic-derivation/) — blog post explaining why macro-internal recursion produces better error messages, handles recursive types out of the box, and simplifies the user-facing API compared to traditional semi-automatic or fully-automatic approaches.

## JVM & Runtime

- [JVM Scala Book](https://kubuszok.com/jvm-scala-book/) — comprehensive reference covering JVM internals relevant to Scala developers, including class loading, bytecode, and runtime behavior that affects macro-generated code.

## Official Scala Documentation

- [Scala Overviews](https://docs.scala-lang.org/overviews/) — the index of official Scala documentation covering compiler options, reflection, collections, and more.
- [Library Author Guide](https://docs.scala-lang.org/overviews/contributors/index.html) — official guidance on publishing Scala libraries, including binary compatibility, versioning, and cross-building considerations.

## Cross-Compilation Background

- [The State of TASTy Reader](https://kubuszok.com/2025/state-of-tasty-reader/) — an article examining the current capabilities and limitations of Scala 3's TASTy reader when consuming Scala 2.13 artifacts, and what it means for cross-compilation strategies.

## Related Projects

For the story of how the Scala macro ecosystem evolved and which projects influenced Hearth's design, see [Prior Art & Influences](prior-art.md).
