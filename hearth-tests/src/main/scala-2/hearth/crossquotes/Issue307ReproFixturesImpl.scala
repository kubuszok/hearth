package hearth.crossquotes

import hearth.*
import hearth.data.Data

// Regression for https://github.com/kubuszok/hearth/issues/307 (Scala 2 only).
//
// `Type.CtorN.UpperBounded.of` / `Bounded.of` with a NON-`Any` upper bound used to emit a `matchResult`
// that cast the extracted argument to `Type[scala.Any]` before calling `.as_<:??<:[L, U]` — which requires
// `U >: Any`, so any real upper bound (e.g. `Marker`) made the generated code fail to typecheck with
// `type arguments do not conform ... [L, U >: Any]`.
//
// Round 2: the generated `unapply` must also MATCH a WILDCARD type argument (`F1[_ <: Marker]`), which reaches
// `baseType(HKT.typeSymbol)` as a PACKED `ExistentialType(_, TypeRef(...))` rather than a bare `TypeRef`. The
// `test*Match` methods below exercise that path. (The full cross-compilation failure — where the constructor's prefix
// is captured via an alias in a SEPARATE library so the structural-`==` fallback also misses — is validated by the
// standalone repro linked from the issue; a single compilation unit cannot reproduce it.)
//
// Like the #300 repro this is a Scala 2 codegen bug (quasiquotes are not hygienic / typed), hence the fixture
// lives under `scala-2`.
private[crossquotes] trait Issue307ReproFixturesImpl { this: MacroCommons =>

  private object Types {
    val Upper1: Type.Ctor1.UpperBounded[Issue307.Marker, Issue307.F1] =
      Type.Ctor1.UpperBounded.of[Issue307.Marker, Issue307.F1]
    val Upper2: Type.Ctor2.UpperBounded[Issue307.Marker, Issue307.Marker, Issue307.F2] =
      Type.Ctor2.UpperBounded.of[Issue307.Marker, Issue307.Marker, Issue307.F2]
    val Bounded1: Type.Ctor1.Bounded[Issue307.SubMarker, Issue307.Marker, Issue307.G1] =
      Type.Ctor1.Bounded.of[Issue307.SubMarker, Issue307.Marker, Issue307.G1]
  }

  // A wildcard argument is extracted as a fresh existential skolem (`_$N`) whose printed name is callsite- and
  // compiler-version-dependent, so we only report WHETHER the constructor matched (the point of the #307 round-2 fix).

  def testCtor1UpperBoundedMatch[In: Type]: Expr[Data] = Expr(Data(Type[In] match {
    case Types.Upper1(_) => true
    case _               => false
  }))

  def testCtor2UpperBoundedMatch[In: Type]: Expr[Data] = Expr(Data(Type[In] match {
    case Types.Upper2(_, _) => true
    case _                  => false
  }))

  def testCtor1BoundedMatch[In: Type]: Expr[Data] = Expr(Data(Type[In] match {
    case Types.Bounded1(_) => true
    case _                 => false
  }))
}

private[crossquotes] object Issue307 {
  sealed trait Marker
  final class SubMarker extends Marker
  trait F1[A <: Marker]
  trait F2[A <: Marker, B <: Marker]
  trait G1[A >: SubMarker <: Marker]
}
