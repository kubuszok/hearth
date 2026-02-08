# Contributing documentation

The referenced documents should be detailed, unambiguous, and informative enough to enable both human contribution
as well as guide agentic coding and debugging.


## Architectural overview and code guidelines

 1. [Architecture overview](architecture-overview.md) - always relevant for implementing new features and debugging or fixing existing ones
 2. [Guidelines for code style](guidelines-for-code-style.md) - always relevant for creating new or modifying existing code
 3. [Guidelines for tests](guidelines-for-tests.md) - relevant for writing new tests


## Instructions for adding new code, tests and documentation

### Instruction for debugging a macro

If we add some debug code to a macro, it will be printed out in every macro expansion.
To make it easier to debug only the expansion we are interested in, we can use some Hearth utilities.

[Instruction for debugging a macro](instruction-for-debugging-a-macro.md).

### Instruction for developing a basic utility

This instruction covers adding new utilities or extending support for new types in the hearth library.

[Instruction for developing a basic utility](instruction-for-developing-basic-utility.md).

### Instruction for testing documentation snippets

Ideally, all utilities should have a section/subsection with examples showing how they can be used with a macro code.
Ideally, each such example should be a snippet (or a group of snippets) that can be run using Scala CLI.

[Instruction for testing documentation snippets](instruction-for-testing-documentation-snippets.md).


## Instructions for fixing bugs and compilation errors

### Instruction for fixing a bug (end-to-end workflow)

Complete workflow for reproducing bugs with failing tests, applying fixes, and verifying them.
Covers clean/compile/test cycles, incremental compilation gotchas, and MCP limitations.

[Instruction for fixing a bug](instruction-for-fixing-a-bug.md).

### Instruction for fixing `':' expected but identifier found.`

This happens when there is some special character at the end of the name, e.g. `?` right before `:` type ascription.

**Example:**
```
[error]  [path to file]: ':' expected but identifier found.
[error]         companionExpr_??: Expr_??,
```

[Instruction for fixing `':' expected but identifier found.`](instruction-for-fixing-colon-expected-but-identifier-found.md).

### Instruction for fixing Cross-Quotes errors on Scala 2 caused by Better-Printers

When you see compilation errors with paths like:
```
[error] [path to project]/hearth/<macro>:[row]:[column] ...
```

This indicates an error in a **generated quasiquote** by `cross-quotes` on Scala 2.

**Example:**
```
[error] /Users/dev/hearth/<macro>:15:23: expected ")" but found identifier
```
This suggests `better-printers` generated a quasiquote with unbalanced parentheses or incorrect syntax at that position.

[Instruction for fixing Cross-Quotes errors on Scala 2 caused by Better-Printers](instruction-for-fixing-cross-quotes-errors-on-scala-2-caused-by-better-printers.md).

### Instruction for debugging Cross-Quotes errors on Scala 2 caused by undetected implicit

When you see generated code containing type names like `someValue.Underlying` (where `someValue` is in scope via an `import`),
the compilation of the macro with `Expr.quote`, `Type.of`, or `Type.CtorN.of` succeeds without errors,
but the expansion of that macro fails.

**Example:**
TODO

[Instruction for fixing Cross-Quotes errors on Scala 2 caused by undetected implicit](instruction-for-fixing-cross-quotes-errors-on-scala-2-caused-by-undetected-implicit.md).

### Instruction for fixing Cross-Quotes errors on Scala 3 caused by undetected implicit

When you see generated code containing type names like `someValue.Underlying` (where `someValue` is in scope via an `import`),
the compilation of the macro with `Expr.quote`, `Type.of`, or `Type.CtorN.of` succeeds without errors,
but the expansion of that macro fails.

(TODO there is an actual error by Scala 3  emitted during compilation but I do not remember it, I'd have to recreate it)

**Example:**
TODO

[Instruction for fixing Cross-Quotes errors on Scala 3 caused by undetected implicit](instruction-for-fixing-cross-quotes-errors-on-scala-3-caused-by-undetected-implicit.md).

### Instruction for fixing `forward reference to value` caused by implicit

It happens when you write:

```scala
// With Cross-Quotes
implicit val someType: Type[A] = Type.of[A]
```

**Example error:**
```
forward reference to value ... defined on line ... extends over definition of value ...
```

[Instruction for fixing `forward reference to value` caused by implicit](instruction-for-fixing-forward-reference-to-value.md).
