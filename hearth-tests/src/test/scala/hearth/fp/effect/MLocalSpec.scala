package hearth
package fp
package effect

final class MLocalSpec extends ScalaCheckSuite {

  group("MLocal") {

    test("should work with sequential semantics") {

      val counter = MLocal(0)(identity)(_ + _)

      val inc = counter.get.flatMap(a => counter.set(a + 1))

      def add(n: Int): MIO[Unit] = 
        if (n > 0) inc >> add(n - 1)
        else MIO.void

      (add(10).tuple(add(10)) >> counter.get).unsafe.runSync._2 === Right(20)
    }

    test("should work with parallel semantics") {

      val counter = MLocal(0)(identity)(_ + _)

      val inc = counter.get.flatMap(a => counter.set(a + 1))

      def add(n: Int): MIO[Unit] = 
        if (n > 0) inc >> add(n - 1)
        else MIO.void

      (add(10).parTuple(add(10)) >> counter.get).unsafe.runSync._2 === Right(20)
    }
  }
}
