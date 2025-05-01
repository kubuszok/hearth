# Hearth

The first Scala macros' standard library.

Planned features:

 - being able to build the code with `Type`s, `Expr`s and high-level utilities that operate on them - limitting the need for AST and Symbols manipulation
 - cross-compilable API, allowing reuse of the macro code for both Scala 2 and Scala 3
 - exhaustive documentation lowering the barrier of entry
 - no dependencies on additional ecosystems (some FP-utilities would be already provided)

## Work in Progress

Hearth is an evolution of [chimney-macro-commons](https://github.com/scalalandio/chimney-macro-commons/) idea,
but one that could serve not only [Chimney](https://chimney.readthedocs.io/) but also any library that uses macros.

While some utilities could be easily migrated, some requres a lot of new effort to make them more generic.
What you see here should be treated as an early alpha.
