<p align="center"><img src="docs/user-guide/assets/images/logo.svg" alt="Hearth logo" height="250px" /></p>

# Hearth

[![CI build](https://github.com/kubuszok/hearth/actions/workflows/ci.yml/badge.svg?branch=master)](https://github.com/kubuszok/hearth/actions)
[![License](https://img.shields.io/:license-Apache%202-green.svg)](http://www.apache.org/licenses/LICENSE-2.0.txt)
[![Join the discussions at https://github.com/kubuszok/hearth/discussions](https://img.shields.io/github/discussions/kubuszok/hearth)](https://github.com/kubuszok/hearth/discussions)

The first Scala macros' standard library.

Goals:

 - being able to build the code with `Type`s, `Expr`s and high-level utilities that operate on them - limiting the need for AST and Symbols manipulation
 - cross-compilable API, allowing reuse of the macro code for both Scala 2 and Scala 3
 - exhaustive documentation lowering the barrier of entry
 - no dependencies on additional ecosystems (some FP-utilities are already provided!)
 - promote best-practices in macro development, to deliver great developer experience

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
