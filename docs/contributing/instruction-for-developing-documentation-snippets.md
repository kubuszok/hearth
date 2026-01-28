# Instruction for testing documentation snippets

Ideally, all utilities should have a section/subsection with examples showing how they can be used with a macro code.
Ideally, each such example should be a snippet (or a group of snippets) that can be run using Scala CLI.


## Existing Documentation

- **README.md** - Project introduction
- **CONTRIBUTING.md** - Guide for the people who would like to set up their development environment and start contributing
- **AGENTS.md** - Entry point for coding agents
- **docs/contributing** - instructions about common development patterns usable for both humans and AI agents
- **docs/user-guide** - MkDocs documentation with Material theme
  - `basic-utilities.md` - Basic macro utilities
  - `better-printers.md` - AST printing utilities
  - `cross-quotes.md` - Cross-platform quoting
  - `micro-fp.md` - FP utilities
  - `standard-extensions.md` - Utilities that are opinionated (contrary to basic utilities) but standardize on some common patterns
  - `debug-utilities.md` - Debugging tools
  - `source-utilities.md` - Source code utilities
  - `best-practices.md` - Best practices guide
  - `faq.md` - Frequently asked questions


## When to Update Documentation

Update documentation when:
- Adding new public APIs
- Changing existing behavior
- Adding new modules or significant features
- Fixing documentation errors

DO NOT create new documentation files unless explicitly permitted or requested.


## How to define runnable snippets

When writing snippets:

 * snippets with no `//>` nor `file:` are skipped by default as pseudocode
 * snippets can be grouped together as multiple files of the same test by `file: [name of the file] - part of [name of the group]`
   within the same section
    * when the same file name appears across several snippets within the same section they are concatenated into 1 file
 * `{{ hearth_version() }}` is replaced by value passed via `--extra "hearth-version=..."`
 * `docs/docs/mkdocs.yml`'s `extra` section can be passed and interpolated as well to e.g. provide a Scala 2 version
   with `{{ scala.2_13 }}` or a dependency version with `{{ libraries.munit }}`
 * the rules of passing/failing tests are provided in [the ScalaCLI.md Spec README](https://github.com/MateuszKubuszok/scala-cli-md-spec)

To ensure that snippets are cross-compilable, and actually expand macros, 4 files would be needed:

 1. file with the shared macro logic, defining dependencies on hearth and 2 targets:

    ```scala
    // file: src/main/scala/example/ExampleMacro.scala - part of example macro
    //> using scala {{ scala.newest_2_13 }} {{ scala.newest_3 }}
    //> using dep com.kubuszok::hearth:{{ hearth_version() }}

    trait ExampleMacro { this: hearth.MacroCommons =>
      // macro methods
    }
    ```

 2. Scala 2 adapter, with -Xsource:3 compiler option:

    ```scala
    // file: src/main/scala-2/example/Example.scala - part of example macro
    //> using target.scala {{ scala.2_13 }}
    //> using options -Xsource:3

    import scala.language.experimental.macros
    import scala.reflect.macros.blackbox

    class Example(val c: blackbox.Context) extends hearth.MacroCommonsScala2 with ExampleMacro {
      // method(s) forwarding to ExampleMacro method(s)
    }
    object Example {
      // def macro method(s) calling Example.methods
    }
    ```

 3. Scala 3 adapter, with a compiler plugin:

    ```scala
    // file: src/main/scala-3/example/Example.scala - part of example macro
    //> using target.scala {{ scala.3 }}
    //> using plugin com.kubuszok::hearth-cross-quotes::{{ hearth_version() }}

    import scala.quoted.*

    class Example(q: Quotes) extends hearth.MacroCommonsScala3(using q), ExampleMacro
    object Example {

      // inline def method(s) splicing
      // def methods accepting Types, Exprs and using Quotes and returning Expr
      // which forward to new Example(q).method
    }
    ```
  
 4. The test specification
    
    ```scala
    // file: src/test/scala/example/ExampleSpec.scala - part of example macro
    //> using test.dep org.scalameta::munit::{{ libraries.munit }}

    final class ExampleSpec extends munit.FunSuite {
      // tests
    }
    ```

`Example`, `ExampleMacro` and `ExampleSpec` can be changed to better reflect what the example is about.
It is important that these 4 files have different paths, so that test runner wouldn't decide to append one to another.

Similarly `example macro` in `part of example macro` can be changed - the important part is that all snippets are within
the same markdown section and contains `// file: [path to file] - part of [example name]` with the same `example name`.


## How to run and tests runnable snippets

[ScalaCLI.md Spec](https://github.com/MateuszKubuszok/scala-cli-md-spec) requires [Scala CLI](https://scala-cli.virtuslab.org/) installed.
It also requires that all snippets are implemented as Scala CLI snippets.

Our documentation uses dependencies like:

```scala
//> using dep com.kubuszok::hearth:{{ hearth_version() }}
```

It will require us to provide the Hearth version to use in snippets, both for rendering the documentation - then
it's just substituted and rendered - and for running the snippets - then we need to also make sure that
either it's a published Hearth version, or that we published it locally.

We can publish the current artifacts for local testing with:

```bash
sbt publish-local-for-tests
```

then, we should also copy the `version` that is generated for the current commit.

When it's installed, you can run:

```bash
# --list-only acts like a dry-run
scala-cli run scripts/test-snippets.scala -- --extra "hearth-version=$hearth_version_that_we_copied" "$PWD/docs/user-guide" --list-only
```

It will show us all snippets that can be run as tests. If we skip the `--list-only`, we will run them all:

```bash
scala-cli run scripts/test-snippets.scala -- --extra "hearth-version=$hearth_version_that_we_copied" "$PWD/docs/user-guide"
```

Since it takes a while to test all snippets, we can use filtering to identify the names of snippets we want to run
(they do not necessarily overlap with the names of links generated by markdown):

```bash
# --test-only filters, we can use * as a wildcard, but we have to use single quotes to make sure it's not interpreted by the shell
# --list-only would let us test if we filtered what we want before actually running the tests
scala-cli run scripts/test-snippets.scala -- --extra "hearth-version=$hearth_version_that_we_copied" "$PWD/docs/user-guide" --test-only  "basic-utilities.md#\`JavaBean"'*' --list-only
```

Once we are sure, that we would run the snippets that we want, we can finally do:

```bash
scala-cli run scripts/test-snippets.scala -- --extra "hearth-version=$hearth_version_that_we_copied" "$PWD/docs/user-guide" --test-only  "basic-utilities.md#\`JavaBean"'*'
```
