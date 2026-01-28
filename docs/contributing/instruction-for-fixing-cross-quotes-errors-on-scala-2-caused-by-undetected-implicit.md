# Instruction for fixing Cross-Quotes errors on Scala 2 caused by undetected implicit

When you see generated code containing type names like `someValue.Underlying` (where `someValue` is in scope via an `import`),
the compilation of the macro with `Expr.quote`, `Type.of`, or `Type.CtorN.of` succeeds without errors,
but the expansion of that macro fails.

**What this means:**
- The cross-quotes system failed to detect an implicit type reference that needs to be replaced
- The issue is either in `importedUnderlyingTypes` not finding the candidate, or in `ImplicitTypeReferenceReplacer` not handling a tree shape

**How to debug:**

1. **Identify the source:**
   - Look at the error message to find which macro expansion is failing
   - Note the offending type reference (e.g., `someValue.Underlying`)

2. **Confirm that the suspected line is the issue:**
   - You can add a temporary value in `CrossQuotesMacros.scala` (before `loggingEnabled`):

     ```scala
     // Use actual file name instead of FileName.scala
     // Use actual line number instead of 100
     val isOurCulprit = c.enclosingPosition.source.path.endsWith("FileName.scala") && c.enclosingPosition.line == 100
     ```

   - You can also temporarily modify `loggingEnabled` by adding `|| isOurCulprit`

3. **Enable logging (optional):**
   - Set `log.cross-quotes = true` in `dev.properties` to see generated code
   - This helps visualize what quasiquote is being generated
   - Remember to reload the build server after changing this setting

4. **Determine the cause - missing candidate or unhandled tree:**

   **Option A: Missing candidate in `importedUnderlyingTypes`:**
   - Add debug printing to show all candidates found for the culprit expansion:
     ```scala
     if (isOurCulprit) {
       println(s"importedUnderlyingTypes candidates: ${candidates.map(showRaw(_)).mkString("\n")}")
     }
     ```
   - If the offending type is not in the candidates, the issue is in how candidates are collected

   **Option B: Unhandled tree in `ImplicitTypeReferenceReplacer`:**
   - Modify the default case in `ImplicitTypeReferenceReplacer`:
     ```scala
     case tree =>
       val treeStr = try {
         showCode(tree)
       } catch {
         case _: Throwable => showRaw(tree)
       }
       if (treeStr.contains("offendingTypeName")) {
         println(s"Unhandled tree containing offending type:")
         println(s"  code: $treeStr")
         println(s"  raw:  ${showRaw(tree)}")
       }
       super.transform(tree)
     ```
   - The `showRaw(tree)` output will help identify the exact tree shape that needs handling

5. **Fix the issue:**
   - If the candidate is missing: fix `importedUnderlyingTypes` to include the missing case
   - If the tree is unhandled: add a new case to `override def transform(tree: Tree): Tree = tree match {`
     within `ImplicitTypeReferenceReplacer` that handles the identified tree shape

6. **Test the fix:**
   - Test the full dependency chain (better-printers → cross-quotes → hearth → hearth-tests)
   - Clean all these modules to avoid false positives/negatives due to invalid incremental compilation cache
   - Verify the fix works for all affected macros

7. **Clean up:**
   - Remove `isOurCulprit` variable and all of its usages
   - Remove all debug prints introduced in the previous steps

**Example:**
TODO
