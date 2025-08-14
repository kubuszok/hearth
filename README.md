<p align="center"><img src="docs/docs/assets/images/logo.svg" alt="Hearth logo" height="250px" /></p>

# Hearth

[![CI build](https://github.com/MateuszKubuszok/hearth/actions/workflows/ci.yml/badge.svg?branch=master)](https://github.com/MateuszKubuszok/hearth/actions)
[![License](https://img.shields.io/:license-Apache%202-green.svg)](http://www.apache.org/licenses/LICENSE-2.0.txt)
[![Join the discussions at https://github.com/MateuszKubuszok/hearth/discussions](https://img.shields.io/github/discussions/MateuszKubuszok/hearth)](https://github.com/MateuszKubuszok/hearth/discussions)

The first Scala macros' standard library.

Planned features:

 - being able to build the code with `Type`s, `Expr`s and high-level utilities that operate on them - limitting the need for AST and Symbols manipulation
 - cross-compilable API, allowing reuse of the macro code for both Scala 2 and Scala 3
 - exhaustive documentation lowering the barrier of entry
 - no dependencies on additional ecosystems (some FP-utilities would be already provided)

## Work in Progress

Hearth is an evolution of the [chimney-macro-commons](https://github.com/scalalandio/chimney-macro-commons/) idea,
but one that could serve not only the [Chimney](https://chimney.readthedocs.io/) but also any other library that uses macros.

While some utilities could be easily migrated, some requres a lot of new effort to make them more generic.
What you see here should be treated as an early alpha.
Please, take a look at [Roadmap](https://github.com/MateuszKubuszok/hearth/issues/10) and [Productisation](https://github.com/MateuszKubuszok/hearth/issues/36) for more infomation what could be in the scope for `0.1.0` release.

## Contribution

If you want to help get this library out of the door then, first, thank you!, and second, please see [`CONTRIBUTING.md`](CONTRIBUTING.md).
