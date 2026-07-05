package hearth
package crossquotes

import hearth.data.Data

/** Fixtures for [[BoundedCtorSpec]] — a CROSS-COMPILED (2.13 + 3) exercise of `Type.CtorN.UpperBounded.of` /
  * `Type.CtorN.Bounded.of` (hearth#344). Before the Scala 3 cross-quotes plugin gained the 3-select rewrite cases,
  * these expanded to the `@compileTimeOnly` stub on Scala 3 (the existing `Issue307ReproFixturesImpl` is Scala-2-only,
  * so the gap was not exercised).
  */
trait BoundedCtorFixturesImpl { this: MacroCommons =>

  def testUpperBounded1[In: Type]: Expr[Data] = {
    val Ctor = Type.Ctor1.UpperBounded.of[BoundedCtors.Marker, BoundedCtors.F1]
    Expr(Data(Type[In] match {
      case Ctor(_) => true
      case _       => false
    }))
  }

  def testUpperBounded2[In: Type]: Expr[Data] = {
    val Ctor = Type.Ctor2.UpperBounded.of[BoundedCtors.Marker, BoundedCtors.Marker, BoundedCtors.F2]
    Expr(Data(Type[In] match {
      case Ctor(_, _) => true
      case _          => false
    }))
  }

  def testBounded1[In: Type]: Expr[Data] = {
    val Ctor = Type.Ctor1.Bounded.of[BoundedCtors.SubMarker, BoundedCtors.Marker, BoundedCtors.G1]
    Expr(Data(Type[In] match {
      case Ctor(_) => true
      case _       => false
    }))
  }
}

object BoundedCtors {
  sealed trait Marker
  final class SubMarker extends Marker
  trait F1[A <: Marker]
  trait F2[A <: Marker, B <: Marker]
  trait G1[A >: SubMarker <: Marker]
}
