package hearth.crossquotes

import hearth.*
import hearth.data.Data

// Scala 2 only, as it concerns quasiquote codegen (quasiquotes are neither hygienic nor typed).
//
// For `Type.CtorN.UpperBounded.of` / `Bounded.of` with a NON-`Any` upper bound the generated `matchResult` must not
// cast the extracted argument to `Type[scala.Any]` before calling `.as_<:??<:[L, U]`: that requires `U >: Any`, so any
// real upper bound (e.g. `Marker`) fails to typecheck with `type arguments do not conform ... [L, U >: Any]`.
//
// The generated `unapply` must also MATCH a WILDCARD type argument (`F1[_ <: Marker]`), which reaches
// `baseType(HKT.typeSymbol)` as a PACKED `ExistentialType(_, TypeRef(...))` rather than a bare `TypeRef`. (When the
// constructor's prefix is captured via an alias in a SEPARATE library, the structural-`==` fallback misses too - but
// that needs separate compilation, a single compilation unit cannot reproduce it.)
private[crossquotes] trait BoundedCtorWildcardMatchFixturesImpl { this: MacroCommons =>

  private object Types {
    val Upper1: Type.Ctor1.UpperBounded[BoundedCtorWildcardExamples.Marker, BoundedCtorWildcardExamples.F1] =
      Type.Ctor1.UpperBounded.of[BoundedCtorWildcardExamples.Marker, BoundedCtorWildcardExamples.F1]
    val Upper2: Type.Ctor2.UpperBounded[
      BoundedCtorWildcardExamples.Marker,
      BoundedCtorWildcardExamples.Marker,
      BoundedCtorWildcardExamples.F2
    ] =
      Type.Ctor2.UpperBounded
        .of[BoundedCtorWildcardExamples.Marker, BoundedCtorWildcardExamples.Marker, BoundedCtorWildcardExamples.F2]
    val Bounded1: Type.Ctor1.Bounded[
      BoundedCtorWildcardExamples.SubMarker,
      BoundedCtorWildcardExamples.Marker,
      BoundedCtorWildcardExamples.G1
    ] =
      Type.Ctor1.Bounded
        .of[BoundedCtorWildcardExamples.SubMarker, BoundedCtorWildcardExamples.Marker, BoundedCtorWildcardExamples.G1]
  }

  // A wildcard argument is extracted as a fresh existential skolem (`_$N`) whose printed name is callsite- and
  // compiler-version-dependent, so we only report WHETHER the constructor matched.

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

private[crossquotes] object BoundedCtorWildcardExamples {
  sealed trait Marker
  final class SubMarker extends Marker
  trait F1[A <: Marker]
  trait F2[A <: Marker, B <: Marker]
  trait G1[A >: SubMarker <: Marker]
}
