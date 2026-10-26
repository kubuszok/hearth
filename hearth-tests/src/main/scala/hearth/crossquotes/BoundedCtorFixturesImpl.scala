package hearth
package crossquotes

import hearth.data.Data

/** Cross-compiled counterpart of the Scala-2-only `BoundedCtorWildcardMatchFixturesImpl`: on Scala 3
  * `Type.CtorN.UpperBounded.of` / `Type.CtorN.Bounded.of` need their own cross-quotes plugin rewrite, or they expand to
  * the `@compileTimeOnly` stub.
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
