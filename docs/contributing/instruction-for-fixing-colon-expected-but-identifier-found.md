# Instruction for fixing `':' expected but identifier found.`

This happens when there is some special character at the end of the name, e.g. `?` right before `:` type ascription.

Then without a spacebar to separate identifier from the `:` type ascription, compiler gets confised.
It can be fixed by adding a spacebar before `:`, e.g:

`companionExpr_??: Expr_??` -> `companionExpr_?? : Expr_??`

**Example:**
```
[error]  [path to file]: ':' expected but identifier found.
[error]         companionExpr_??: Expr_??,
```
