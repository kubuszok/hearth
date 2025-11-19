<p align="center"><img src="docs/docs/assets/images/logo.svg" alt="Hearth logo" height="250px" /></p>

# Hearth

[![CI build](https://github.com/MateuszKubuszok/hearth/actions/workflows/ci.yml/badge.svg?branch=master)](https://github.com/MateuszKubuszok/hearth/actions)
[![License](https://img.shields.io/:license-Apache%202-green.svg)](http://www.apache.org/licenses/LICENSE-2.0.txt)
[![Join the discussions at https://github.com/MateuszKubuszok/hearth/discussions](https://img.shields.io/github/discussions/MateuszKubuszok/hearth)](https://github.com/MateuszKubuszok/hearth/discussions)

The first Scala macros' standard library.

Goals:

 - being able to build the code with `Type`s, `Expr`s and high-level utilities that operate on them - limitting the need for AST and Symbols manipulation
 - cross-compilable API, allowing reuse of the macro code for both Scala 2 and Scala 3
 - exhaustive documentation lowering the barrier of entry
 - no dependencies on additional ecosystems (some FP-utilities are already provided!)

## Call for Feedback

Hearth is an evolution of the [chimney-macro-commons](https://github.com/scalalandio/chimney-macro-commons/) idea,
but one that could serve not only the [Chimney](https://chimney.readthedocs.io/) but also any other library that uses macros.

The current version does not yet have all planned features, and we'd like to hear from you is the current API good enough or what to change.

Please, take a look at [Roadmap](https://github.com/MateuszKubuszok/hearth/issues/10) for more infomation what is already done,
and what's in the scope for future releases.

## Contribution

If you want to help get this library out of the door then, first, thank you!, and second, please see [`CONTRIBUTING.md`](CONTRIBUTING.md).
