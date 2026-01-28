# Instruction for fixing Cross-Quotes errors on Scala 3 caused by undetected implicit

When you see generated code containing type names like `someValue.Underlying` (where `someValue` is in scope via an `import`),
the compilation of the macro with `Expr.quote`, `Type.of`, or `Type.CtorN.of` succeeds without errors,
but the expansion of that macro fails.

(TODO there is an actual error by Scala 3  emitted during compilation but I do not remember it, I'd have to recreate it)

**What this means:**
- The cross-quotes system failed to detect an implicit type reference that needs to be replaced
- Unlike Scala 2, Scala 3 relies on a macro plugin - better-printers are not used and cannot be the issue
- The issue is in `CrossQuotesPlugin.scala` - either in finding candidates or in extracting given candidates from a tree shape

**How to debug:**

1. **Identify the source:**
   - Look at the error message to find which macro expansion is failing
   - Note the offending type reference (e.g., `someValue.Underlying`)

2. **Confirm that the suspected line is the issue:**
   - Add a temporary method in `CrossQuotesPlugin.scala`:

     ```scala
     // Use actual file name instead of FileName.scala
     // Use actual line number instead of 100
     // Note: Scala 3 represents lines internally with 0-based indexing, so we add +1
     def isOurCulprit(pos: Position): Boolean =
       pos.source.path.endsWith("FileName.scala") && pos.line + 1 == 100
     ```

3. **Debug print candidates and injected givens:**
   - Add debug printing to show candidates found for the culprit expansion:
     ```scala
     if (isOurCulprit(tree.sourcePos)) {
       println(s"New candidates: ${newCandidates.map(_.show).mkString("\n")}")
       println(s"Injected givens: ${injectGivens.map(_.show).mkString("\n")}")
     }
     ```

4. **Print the tree at the suspected position:**
   - Add debug printing to see the AST at the offending position:
     ```scala
     if (isOurCulprit(tree.sourcePos)) {
       println(s"Tree at culprit position:")
       println(s"  show: ${tree.show}")
       println(s"  structure: ${tree.toString}")
     }
     ```
   - Seeing the AST can help identify the suspect - an omitted tree that should be considered as an implicit source candidate

5. **Identify the unhandled tree shape:**
   - Compare the printed tree structure with existing cases in `override def transform(tree: untpd.Tree)(using Context): untpd.Tree = tree match {`
   - The offending tree is likely a shape not currently handled for candidate extraction

6. **Fix the issue:**
   - Add the identified tree shape to `override def transform(tree: untpd.Tree)(using Context): untpd.Tree = tree match {`
   - Extract new given candidates from the tree appropriately

7. **Test the fix:**
   - Perform a full recompilation (clean build) to test the fix
   - Verify the fix works for all affected macros

8. **Clean up:**
   - Remove `isOurCulprit` method and all of its usages
   - Remove all debug prints introduced in the previous steps

**Example:**
TODO
