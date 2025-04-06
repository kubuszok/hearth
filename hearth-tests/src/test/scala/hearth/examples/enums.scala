package hearth
package examples
package enums

// TODO: enumeration classes

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
