<p align="center"><img src="docs/user-guide/assets/images/logo.svg" alt="Hearth logo" height="250px" /></p>

# Hearth

[![CI build](https://github.com/kubuszok/hearth/actions/workflows/ci.yml/badge.svg?branch=master)](https://github.com/kubuszok/hearth/actions)
[![License](https://img.shields.io/:license-Apache%202-green.svg)](http://www.apache.org/licenses/LICENSE-2.0.txt)
[![Join the discussions at https://github.com/kubuszok/hearth/discussions](https://img.shields.io/github/discussions/kubuszok/hearth)](https://github.com/kubuszok/hearth/discussions)

The first Scala macros' standard library.

Hearth helps you write robust, fast-to-compile, easy-to-maintain macros - for Scala 3, for Scala 2.13, or for both.

 - **corner cases already handled** - generic, opaque and singleton types, named tuples, enums, default values,
   recursive types - tested on Scala 2.13 and Scala 3, on JVM, Scala.js and Scala Native
 - **building blocks instead of compiler trees** - types, expressions, methods, case classes and enums, with error
   aggregation and logging built in
 - **integrate a library once** - a module on the classpath (e.g. for Cats, Iron or Refined) is picked up by every
   Hearth-based macro that asks Hearth about collections, options or wrapper types, with no extra imports
 - **single-version or cross-compiled** - Scala 3-only, Scala 2-only, or one macro for both; in single-version code the
   compiler's own API stays available
 - **general purpose** - type class derivation, optics, dependency injection, mocking, parsers and more
 - exhaustive documentation and no dependencies on other ecosystems

## Call for Feedback

Hearth is an evolution of the [chimney-macro-commons](https://github.com/scalalandio/chimney-macro-commons/) idea,
but one that could serve not only the [Chimney](https://chimney.readthedocs.io/) but also any other library that uses macros.

The current version has all planned features, beside bugfixes no further changes will be made unless community will request for them or contribute them.

Please, take a look at [Roadmap](https://github.com/kubuszok/hearth/issues/10) for more information about what is already done.

See [Chimney](https://github.com/scalalandio/chimney),
[Kindlings](https://github.com/kubuszok/kindlings),
[Pipez](https://github.com/kubuszok/pipez)
and [Refined-compat](https://github.com/kubuszok/refined-compat)
for examples how Hearth can be used to implement a macro-powered library.

## Contribution

If you want to help get this library released, first, thank you, and second, please see [`CONTRIBUTING.md`](CONTRIBUTING.md).
