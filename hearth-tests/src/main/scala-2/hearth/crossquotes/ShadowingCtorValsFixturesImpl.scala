package hearth.crossquotes

import hearth.*

// Users routinely stash `Type.CtorN.of[...]` results in vals named after the type they hold. Those names (`List`,
// `Map`, ...) shadow `scala.List`/`scala.Map`, so if the generated `Type.CtorN.of` unapply referenced *bare*
// `List(...)` / `Seq(...)` extractors, it would capture the user's val and fail with a `Nothing <:??<: Any` type
// mismatch deep inside the emitted code.
//
// Scala 2 only, as Scala 2 quasiquotes are not hygienic. It exists purely to be compiled: each `Type.CtorN.of` below
// expands at compile time.
private[crossquotes] trait ShadowingCtorValsFixturesImpl { this: MacroCommons =>

  private object Types {
    val List: Type.Ctor1[List] = Type.Ctor1.of[List]
    val Map: Type.Ctor2[Map] = Type.Ctor2.of[Map]
    val Option: Type.Ctor1[Option] = Type.Ctor1.of[Option]
    val Set: Type.Ctor1[Set] = Type.Ctor1.of[Set]
    val Seq: Type.Ctor1[Seq] = Type.Ctor1.of[Seq]
  }
}
