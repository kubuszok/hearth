package hearth
package fp

import hearth.fp.instances.*
import scala.util.Try

final class StandardLibraryInstancesSpec extends ScalaCheckSuite with Laws {

  group("Instances for Id") {
    group("should follow Functor laws") {
      functorLaws[Id, Int]
    }

    group("should follow Applicative laws") {
      applicativeLaws[Id, Int, String, Int]
    }

    group("should follow Parallel laws") {
      parallelLaws[Id, Int, String]
    }

    group("should follow Traverse laws") {
      traverseLaws[Id, Option, String]
    }

    group("should follow DirectStyle laws") {
      directStyleLaws[Id, Int]
    }
  }

  group("Instances for Either") {

    group("should follow Functor laws") {
      functorLaws[Either[String, *], Int]
    }

    group("should follow Applicative laws") {
      applicativeLaws[Either[String, *], Int, String, Int]
    }

    group("should follow Parallel laws when left is List") {
      parallelLaws[Either[List[String], *], Int, String]
    }
    group("should follow Parallel laws when left is Vector") {
      parallelLaws[Either[Vector[String], *], Int, String]
    }

    group("should follow Traverse laws") {
      traverseLaws[Either[String, *], Option, String]
    }

    group("should follow DirectStyle laws") {
      directStyleLaws[Either[String, *], Int]
    }
  }

  group("Instances for Option") {

    group("should follow Functor laws") {
      functorLaws[Option, Int]
    }

    group("should follow Applicative laws") {
      applicativeLaws[Option, Int, String, Int]
    }

    group("should follow Parallel laws") {
      parallelLaws[Option, Int, String]
    }

    group("should follow Traverse laws") {
      traverseLaws[Option, Option, String]
    }

    group("should follow DirectStyle laws") {
      directStyleLaws[Option, Int]
    }
  }

  group("Instances for Try") {
    group("should follow Functor laws") {
      functorLaws[Try, Int]
    }

    group("should follow Applicative laws") {
      applicativeLaws[Try, Int, String, Int]
    }

    group("should follow Traverse laws") {
      traverseLaws[Try, Option, String]
    }

    group("should follow DirectStyle laws") {
      directStyleLaws[Try, Int]
    }
  }

  group("Instances for List") {

    group("should follow Functor laws") {
      functorLaws[List, Int]
    }

    group("should follow Applicative laws") {
      applicativeLaws[List, Int, String, Int]
    }

    group("should follow Traverse laws") {
      traverseLaws[List, Option, Int]
    }
  }

  group("Instances for Vector") {

    group("should follow Functor laws") {
      functorLaws[Vector, Int]
    }

    group("should follow Applicative laws") {
      applicativeLaws[Vector, Int, String, Int]
    }

    group("should follow Traverse laws") {
      traverseLaws[Vector, Option, Int]
    }
  }
}
