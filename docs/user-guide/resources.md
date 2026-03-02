# Resources & Further Reading

A curated collection of external references that complement the Hearth documentation.

## Derivation Theory

- [**Sanely Automatic Derivation**](https://kubuszok.com/2025/sanely-automatic-derivation/) — blog post explaining why macro-internal recursion produces better error messages, handles recursive types out of the box, and simplifies the user-facing API compared to traditional semi-automatic or fully-automatic approaches.
- [**Kindlings**](https://github.com/kubuszok/kindlings) - repository which puts derivation theory into practice by implementing several derivation macros for Scala.

## JVM & Runtime

- [**JVM Scala Book**](https://leanpub.com/jvm-scala-book/) — comprehensive reference covering JVM internals relevant to Scala developers, including class loading, bytecode, and runtime behavior that affects macro-generated code. (Disclaimer: I've written it.)

## Official Scala Documentation

- [**Scala Overviews**](https://docs.scala-lang.org/overviews/) — the index of official Scala documentation covering compiler options, reflection, collections, and more.
- [**Library Author Guide**](https://docs.scala-lang.org/overviews/contributors/index.html) — official guidance on publishing Scala libraries, including binary compatibility, versioning, and cross-building considerations.
- [**Scala 2 macros documentation**](https://docs.scala-lang.org/overviews/macros/overview.html)

    - Additionally I suggest opening [code in a browser](https://github.com/scala/scala/tree/2.13.x/src/reflect/scala/reflect/api) API directory and keeping the tab opened (might be a good idea since in my IDE sources are NOT imported so I see no documentation)

- [**Scala 3 macros documentation**](https://docs.scala-lang.org/scala3/guides/macros/macros.html)

    - Additionally I suggest opening [code in a browser](https://github.com/scala/scala3/blob/3.3.7/library/src/scala/quoted/Quotes.scala) API directory and keeping the tab opened (might be a good idea since in my IDE sources are NOT imported so I see no documentation)

### Cross-Building Background

- [**The State of TASTy Reader**](https://www.scala-lang.org/blog/state-of-tasty-reader.html) — an article examining the current capabilities and limitations of Scala 3's TASTy reader when consuming Scala 2.13 artifacts, and what it means for cross-compilation strategies.

### EPFL Papers

- [**C. Hofer et al. Polymorphic Embedding of DSLs, GPCE, 2008**](https://www.informatik.uni-marburg.de/~rendel/hofer08polymorphic.pdf) - theoretical background on using traits with abstact types and methods, and mix-ins with implementation to define DSLs and "interpret" them at runtime.
- [**Scala Macros, a Technical Report**](https://infoscience.epfl.ch/record/183862?ln=en) (Scala 2 macros)
- [**Quasiquotes for Scala**](https://infoscience.epfl.ch/record/185242?ln=en) (Scala 2 macros)
- [**Scalable Metaprogramming in Scala 3**](https://infoscience.epfl.ch/record/299370?ln=en) (Scala 3 macros)
- potentially all the other [**EPFL papers**](https://infoscience.epfl.ch/search?query=scala%20macros&configuration=researchoutputs)
 
## Related Projects

For the story of how the Scala macro ecosystem evolved and which projects influenced Hearth's design, see [Prior Art & Influences](prior-art.md).
