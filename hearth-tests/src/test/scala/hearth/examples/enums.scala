package hearth
package examples
package enums

object WeekDay extends Enumeration {
  type WeekDay = Value
  val Mon, Tue, Wed, Thu, Fri, Sat, Sun = Value
}

object Planet extends Enumeration {
  protected case class PlanetVal(mass: Double, radius: Double) extends super.Val {
    def surfaceGravity: Double = Planet.G * mass / (radius * radius)
    def surfaceWeight(otherMass: Double): Double = otherMass * surfaceGravity
  }
  import scala.language.implicitConversions
  implicit def valueToPlanetVal(x: Value): PlanetVal = x.asInstanceOf[PlanetVal]
  val G: Double = 6.67300e-11
  val Mercury = PlanetVal(3.303e+23, 2.4397e6)
  val Venus = PlanetVal(4.869e+24, 6.0518e6)
  val Earth = PlanetVal(5.976e+24, 6.37814e6)
  val Mars = PlanetVal(6.421e+23, 3.3972e6)
  val Jupiter = PlanetVal(1.9e+27, 7.1492e7)
  val Saturn = PlanetVal(5.688e+26, 6.0268e7)
  val Uranus = PlanetVal(8.686e+25, 2.5559e7)
  val Neptune = PlanetVal(1.024e+26, 2.4746e7)
}

sealed trait ExampleSealedTrait
object ExampleSealedTrait {
  case class ExampleSealedTraitClass(a: Int) extends ExampleSealedTrait
  case object ExampleSealedTraitObject extends ExampleSealedTrait
}

sealed trait ExampleSealedTraitWithTypeParam[+A]
object ExampleSealedTraitWithTypeParam {
  case class ExampleSealedTraitWithTypeParamClass[A](a: A) extends ExampleSealedTraitWithTypeParam[A]
  case object ExampleSealedTraitWithTypeParamObject extends ExampleSealedTraitWithTypeParam[Nothing]
}

sealed trait ExampleSealedTraiGADT[A]
object ExampleSealedTraiGADT {
  case class ExampleSealedTraitWithTypeParamClass(str: String) extends ExampleSealedTraiGADT[String]
  case object ExampleSealedTraitWithTypeParamObject extends ExampleSealedTraiGADT[Unit]
}
