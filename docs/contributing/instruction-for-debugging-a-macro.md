# Instruction for debugging a macro

If we add some debug code to a macro, it will be printed out in every macro expansion.
To make it easier to debug only the expansion we are interested in, we can use some Hearth utilities.

## Debugging by previewing result of a macro expansion

If you need to figure out what code is returned from macro, try this utility:

```scala
import hearth.debug.Debug

Debug.withFinalCodeInIDE {

  someCodeWithMacroExpansion
}
```

It should show you the code returned from the macro:

 - in the compilation output
 - as a hint overlay in VS Code (blue squiggly lines, hover over them and you'll see the output)
 - as a hint overlay in Scastie (likewise)

If the code you wish to preview is implicit, try:

```scala
import hearth.debug.Debug

Debug.withGivenCodeInIDE[TypeClass[A]]
```

If you are writing a new macro support, and you need to figure out the AST to create,
the AST variant of the 2 above is available as:

```scala
import hearth.debug.Debug

Debug.withFinalASTInIDE {

  someCodeWithMacroExpansion
}

Debug.withGivenASTInIDE[TypeClass[A]]
```

## Debugging by selectively printing from within a macro expansion

These you can use from outside of macro. If you are working on some utility and want to print only
the current part, you can write inside a macro:

```scala
if (Environment.isExpandedAt("MyMacroSpec.scala:40")) {

  // Notice! Only the first reportInfo and the first reportWarn
  // would be shown by the compiler! All following calls becomes no-ops!
  Environment.reportInfo("something I need to print")

  // If you need to log something else as well (e.g. logs, duh), you can use
  println("something I need to print")
  // but this will only be visible in the console, and ONLY if you are not using some
  // server-based build (arbitrary `println`s from the compiler are not forwarded).
}
```

This would print only if the currently expanded macro is defined in `MyMacroSpec.scala` at line `40`.

If there is more than 1 macro at that line, you can also try e.g. `"MyMacroSpec.scala:40:20"`
to only expand if it's a macro on line `40` starting at column `20`.

If you report from macro or compilation fails, then the log message would contain that `file.scala:line:column` location.

Be careful!

```scala
  Foo.bar // <- macro expansion
```

would have a column:

 - `6` on Scala 2 (macro is after the `.`)
 - `2` on Scala 3 (`Foo.` would also be counted towards current position)

so if you are filtering by column you have to adapt the value after changing which version of Scala you work on currently!
