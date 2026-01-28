# Contributing to Hearth

First off, thanks for taking the time to contribute! ❤️

Currently the best way oh helping is contributing to [help us tackle some tasks from the Roadmap](https://github.com/MateuszKubuszok/hearth/issues/10) to deliver the solid next version! This includes testing Hearth, providing us with reproduction for bugs and maybe even fixes!

The second best, is raising awareness about the project ☺️

Once that is done, we have a lot more opportunities to contribute (e.g. suggesting new features).

----

### Table of contents

 1. [How to start working with the code?](#how-to-start-working-with-the-code)
 2. [Guidelines and instructions](#guidelines-and-instructions)

## How to start working with the code?

You need to have installed:

 1. [`git`](https://github.com/git-guides)
 2. Java SDK - for local development we recommend something like [Sdkman](https://sdkman.io/) or [jEnv](https://www.jenv.be/) for managing Java SDKs
 3. `sbt` - however, if you don't want to use some package manager to install it, you can run `./sbt` instead of `sbt` to have it installed for you
 4. IDE that supports Scala:

     1. One popular option is [IntelliJ IDEA](https://www.jetbrains.com/idea/download/) - its [core part is OSS](https://github.com/JetBrains/intellij-community),
        and [Scala plugin](https://blog.jetbrains.com/scala/) is [OSS as well](https://github.com/JetBrains/intellij-scala), so there are no issues in using them
        for development (or commercially).
       
        After installing IntelliJ you need to install Scala plugin to enable support for it

     2. Another option is to use [Scala Metals](https://scalameta.org/metals/) - it's a [Language Server Protocol](https://microsoft.github.io/language-server-protocol/)
        that provides Scala IDE-like features to any editor which support LSP - there are official installation instructions for Visual Studio Code, Vim, Sublime Text, Emacs, Helix, GitPod.
       
        If you don't have a preference, use Visual Studio Code, if you want to use AI, use something VSC-based like Cursor, Windsurf, Augumented Code or whatever is cheapest hype of the week.
        Then install Scala and Scala Metals extensions from the extension store.

     3. If you want to use AI in general, I suggest Scala Metals and reading about [Model Context Protocol](https://modelcontextprotocol.io/)
        that [they enable](https://scalameta.org/metals/docs/editors/user-configuration#start-mcp-server).

I recommend to:

 1. [fork the repository](https://github.com/MateuszKubuszok/hearth/fork) to your own GitHub
 2. [clone it](https://docs.github.com/en/get-started/git-basics/about-remote-repositories#cloning-with-ssh-urls) to your machine
 3. start in your terminal sbt shell with `sbt` (if you installed it globally) or `./sbt` (if you didn't)
 4. run in it `ci-jvm-2_13` and `ci-jvm-3` tasks to test if everything works
 5. if it does, then you should be able to open the folder in your IDE, and it should recognize it as sbt project and start indexing it
 6. if it succeeds you should be able to see that IDE imported modules like:
    - `hearth`/`hearth3`
    - `hearthCrossQuotes`/`hearthCrossQuotes3`
    - `hearthMicroFp`/`hearthMicroFp3`
    - `hearthTests`/`hearthTests3`

You will see _only_ either Scala 2.13 _or_ Scala 3 projects there. And you will _only_ see JVM projects.

While the build tool easily handles situations where something:

 - should be only used as a source code for Scala 2.13 or
 - should be only used as a source code for Scala 3 or
 - should be only used as a source code for JVM Scala (not Scala.js nor Scala Native) or
 - should be shared for all platforms and Scala versions

IDEs do not understand what to do in such cases: treat shared file as which language versions and which platform? Where should
"find all usages" and "jump to implementation" point to? So in `dev.properties` you see something like:

```properties
# Do not commit changes to this file!
# Change it locally, reload build in IDE, and develop.
# Consider running: git update-index --assume-unchanged dev.properties

# Allowed: 2.13, 3
ide.scala = 3
# Allowed: jvm, js, native
ide.platform = jvm

# Allowed: true, false, |-separated list of file names
log.cross-quotes = false
```

You can use it control which version you want to work on in your IDE. You need to change `ide.scala`/`ide.platform` and reimport build.

You can open and edit sources for the other version as well, buy you will get only basic syntax highlighting and no intellisense.

## Guidelines and instructions

Are available in [contributing documentation](docs/contributing/_index.md).
