<p align="center"><img src="docs/user-guide/assets/images/logo.svg" alt="Hearth logo" height="250px" /></p>

# Hearth

[![CI build](https://github.com/kubuszok/hearth/actions/workflows/ci.yml/badge.svg?branch=master)](https://github.com/kubuszok/hearth/actions)
[![License](https://img.shields.io/:license-Apache%202-green.svg)](http://www.apache.org/licenses/LICENSE-2.0.txt)
[![Join the discussions at https://github.com/kubuszok/hearth/discussions](https://img.shields.io/github/discussions/kubuszok/hearth)](https://github.com/kubuszok/hearth/discussions)

The first Scala macros' standard library.

Hearth gives macro authors robust, well-tested utilities that handle the corner cases you would otherwise learn about
only from your users' bug reports - "this obviously should work, but it doesn't". It is worth using even if you
target only Scala 3 or only Scala 2.13.

Goals:

 - **robust building blocks instead of hand-written ASTs** - work with `Type`s, `Expr`s, `Method`s, case classes,
   enums and other well-defined APIs, instead of reverse-engineering compiler trees and building them from scratch
 - **macros that are easy to get right** - error aggregation and logging with `MIO`, readable error messages that let
   your users fix their code in one go, fast compilation and well-performing generated code
 - **no lock-in** - the native representation (`c.Type`/`c.Tree`, `TypeRepr`/`Term`) is always available: use Hearth
   for the 90% it covers and the low-level API for the rest
 - **integrate a library once** - with macro extensions discovered from the classpath and standard interfaces
   (`IsCollection`, `IsOption`, `IsValueType`, ...), one integration module (e.g. for Cats collections, Iron, Refined
   or newtypes) makes every Hearth-based macro that uses them support those types, with no extra imports
 - **one macro for Scala 2.13 and Scala 3**, when you need it - a cross-compilable API (with optional Cross Quotes),
   while Scala 2-only and Scala 3-only macros are supported just as well
 - not only type class derivation - also optics, dependency injection, mocking, parsers and more
 - exhaustive documentation lowering the barrier of entry
 - no dependencies on additional ecosystems (some FP-utilities are already provided!)

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
