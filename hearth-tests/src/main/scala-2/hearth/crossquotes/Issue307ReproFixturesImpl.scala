package hearth.crossquotes

import hearth.*

// Regression for https://github.com/kubuszok/hearth/issues/307 (Scala 2 only).
//
// `Type.CtorN.UpperBounded.of` / `Bounded.of` with a NON-`Any` upper bound used to emit a `matchResult`
// that cast the extracted argument to `Type[scala.Any]` before calling `.as_<:??<:[L, U]` — which requires
// `U >: Any`, so any real upper bound (e.g. `Marker`) made the generated code fail to typecheck with
// `type arguments do not conform ... [L, U >: Any]`.
//
// Like the #300 repro this is a Scala 2 codegen bug (quasiquotes are not hygienic / typed), hence the fixture
// lives under `scala-2`. It exists purely to be compiled: each `Type.CtorN.*.of` below expands at compile
// time, so if the generated code does not typecheck, compilation fails.
private[crossquotes] trait Issue307ReproFixturesImpl { this: MacroCommons =>

  private object Types {
    val Upper1: Type.Ctor1.UpperBounded[Issue307.Marker, Issue307.F1] =
      Type.Ctor1.UpperBounded.of[Issue307.Marker, Issue307.F1]
    val Upper2: Type.Ctor2.UpperBounded[Issue307.Marker, Issue307.Marker, Issue307.F2] =
      Type.Ctor2.UpperBounded.of[Issue307.Marker, Issue307.Marker, Issue307.F2]
    val Bounded1: Type.Ctor1.Bounded[Issue307.SubMarker, Issue307.Marker, Issue307.G1] =
      Type.Ctor1.Bounded.of[Issue307.SubMarker, Issue307.Marker, Issue307.G1]
  }
}

private[crossquotes] object Issue307 {
  sealed trait Marker
  final class SubMarker extends Marker
  trait F1[A <: Marker]
  trait F2[A <: Marker, B <: Marker]
  trait G1[A >: SubMarker <: Marker]
}
