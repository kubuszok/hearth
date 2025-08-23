package hearth
package fp
package effect

final class MioSpec extends ScalaCheckSuite with Laws {

  group("Instances for MIO") {

    group("should follow Functor laws") {
      functorLaws[MIO, Int]
    }

    group("should follow Applicative laws") {
      applicativeLaws[MIO, Int, String, Int]
    }

    group("should follow Parallel laws") {
      parallelLaws[MIO, Int, String]
    }
    
    group("should follow DirectStyle laws") {
      directStyleLaws[MIO, Int]
    }
  }
}
