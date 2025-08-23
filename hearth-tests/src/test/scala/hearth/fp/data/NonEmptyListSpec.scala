package hearth
package fp
package data

import hearth.fp.instances.*

final class NonEmptyListSpec extends ScalaCheckSuite with Laws {

  group("Instances for NonEmptyList") {

    group("should follow Traverse laws") {
      traverseLaws[NonEmptyList, Option, String]
    }
  }

  group("Instances for Either[NonEmptyList, *]") {

    group("should follow Functor laws") {
      functorLaws[Either[NonEmptyList[String], *], Int]
    }

    group("should follow Applicative laws") {
      applicativeLaws[Either[NonEmptyList[String], *], Int, String, Int]
    }

    group("should follow Parallel laws") {
      parallelLaws[Either[NonEmptyList[String], *], Int, String]
    }

    group("should follow Traverse laws") {
      traverseLaws[Either[NonEmptyList[String], *], Option, String]
    }
  }
}
