# Instruction for fixing Cross-Quotes errors on Scala 2 caused by Better-Printers

When you see compilation errors with paths like:
```
[error] [path to project]/hearth/<macro>:[row]:[column] ...
```

This indicates an error in a **generated quasiquote** by `cross-quotes` on Scala 2.

**What this means:**
- The `<macro>` portion in the path shows its generated macro code, not a regular source file
- The error is in code that `cross-quotes` generated using `better-printers`
- The issue is likely in how `better-printers` is converting AST to quasiquote syntax

**How to debug:**

1. **Identify the source:**
   - Look at the error message and stack trace to find which macro is failing
   - It's possible there won't be a file, nor a line and column number, but there could be a line within `Expr.quote`
     expansion that caused that error - it can be used to find which `Expr.quote` expansion failed

2. **Confirm that the suspected line is the issue:**
    - You can add a temporary value in `CrossQuotesMacros.scala` (before `loggingEnabled`):

      ```scala
      // Use actual file name instead of FileName.scala
      // Use actual line number instead of 100
      val isOurCulprit = c.enclosingPosition.source.path.endsWith("FileName.scala") && c.enclosingPosition.line == 100
      ```

    - You can also temporarily modify `loggingEnabled` by adding `|| isOurCulprit`
    - you can use that condition to add a debug print in the `.tap { ... }` section of `CrossQuotesMacros.scala`, right before `.pipe(c.parse(_))`,
      It should be a condition like (`if (isOurCulprit) { println(s"source: ${source}\nast: {showRawPretty(tree, SyntaxHighlight.plain)}") }`)
    - If that fails (the line doesn't look like a suspect), we can try putting in that `.tap { ... }` conditional printing of the invalid code:
      `if (source.contains("invalid code")) { println(s"source: ${source}\nast: {showRawPretty(tree, SyntaxHighlight.plain)}") }`
    - If we can see that `println` in the compilation output right before the error, we found our offending snippet.
      The printed line would contain the AST that is incorrectly handled by (probably) the `hearth-better-printers` for Scala 2

3. **Enable logging (optional, if the previous step failed):**
   - Set `log.cross-quotes = true` in `dev.properties` to see generated code
   - This helps visualize what quasiquote is being generated
   - Remember to reload the build server after changing this setting

4. **Confirm that better-printers are the culprit:**
   - In `ShowCodePrettyScala2.scala` identify the method in `HighlighedCodePrinter` that could be used for testing the hypothesis
     which code is incorrectly handled, e.g. by putting a `case` in `processTreePrinting(tree: Tree): Unit` that pattern-matches
     on the exact shape of incorrectly handled tree and returning a fixed correct printing with `print("correct code")`
   - If the recompilation shows that this fixes the error (it might uncover another error, but we won't go to fixing it just yet)
     we confirmed that this AST shape that requires fixed handling, if not, we need to test another hypothesis
   - We can remove/comment out this debug print before moving to the next step

5. **Fix the issue:**
   - If we identified an offending shape we need to form a hypothesis which of the methods fails to handle it as expected,
     Then, we need to add a debug `scala.Predef.println` (normal println is overshadowed, by methods prepending to printer)
     That would inform us about every case in the whole codebase where this case occurs:
       - if we print for the cases where code break, we are on the right track
       - if we also print for cases which do not trigger error, we know that there are cases where this format is legitimate
   - We should make sure that these debug prints contains `showCode`, `showRaw`, possibly `Symbol`s and their flags, to give us
     as much information as possible to distinguish correctly handled cases from incorrectly handled cases
   - We should then special case handling for the incorrect ones using the information about the differences
   - Then test the full dependency chain (better-printers → cross-quotes → hearth → hearth-tests, with clean of all these modules
     to avoid false positives/negatives due to invalid incremental compilation cache) to verify the fix works for all affected macros

6. **Clean up:**
    - remove `isOurCulprit` variable and all of its usages
    - remove all debug prints introduced in the previous steps

**Example:**
```
[error] /Users/dev/hearth/<macro>:15:23: expected ")" but found identifier
```
This suggests `better-printers` generated a quasiquote with unbalanced parentheses or incorrect syntax at that position.
