package hearth.crossquotes

import hearth.*

// Regression for https://github.com/kubuszok/hearth/issues/300 (Scala 2 only).
//
// Users routinely stash `Type.CtorN.of[...]` results in vals named after the type
// they hold. Those names (`List`, `Map`, ...) shadow `scala.List`/`scala.Map`, which
// the generated `Type.CtorN.of` unapply used to reference via *bare* `List(...)` /
// `Seq(...)` extractors — capturing the user's val and blowing up with a
// `Nothing <:??<: Any` type mismatch deep inside the emitted code.
//
// This is a Scala 2 codegen bug (Scala 2 quasiquotes are not hygienic), hence the
// fixture lives under `scala-2`. It exists purely to be compiled: each `Type.CtorN.of`
// below expands at compile time, so if the generated quasiquotes are not hygienic,
// compilation fails.
private[crossquotes] trait Issue300ReproFixturesImpl { this: MacroCommons =>

  private object Types {
    val List: Type.Ctor1[List] = Type.Ctor1.of[List]
    val Map: Type.Ctor2[Map] = Type.Ctor2.of[Map]
    val Option: Type.Ctor1[Option] = Type.Ctor1.of[Option]
    val Set: Type.Ctor1[Set] = Type.Ctor1.of[Set]
    val Seq: Type.Ctor1[Seq] = Type.Ctor1.of[Seq]
  }
}
