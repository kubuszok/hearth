package hearth
package fp
package data

import hearth.fp.instances.*

final class NonEmptyVectorSpec extends ScalaCheckSuite with Laws {

  group("Instances for NonEmptyVector") {

    group("should follow Traverse laws") {
      traverseLaws[NonEmptyVector, Option, String]
    }
  }

  group("Instances for Either[NonEmptyVector, *]") {

    group("should follow Functor laws") {
      functorLaws[Either[NonEmptyVector[String], *], Int]
    }

    group("should follow Applicative laws") {
      applicativeLaws[Either[NonEmptyVector[String], *], Int, String, Int]
    }

    group("should follow Parallel laws") {
      parallelLaws[Either[NonEmptyVector[String], *], Int, String]
    }

    group("should follow Traverse laws") {
      traverseLaws[Either[NonEmptyVector[String], *], Option, String]
    }
  }
}
