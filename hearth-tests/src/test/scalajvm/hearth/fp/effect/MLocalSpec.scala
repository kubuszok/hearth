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

    test("should work with nested use of .scoped") {
      val mlocal = MLocal(Map.empty[String, Int])((m) => m)((m1, m2) => m1 ++ m2)

      def append(name: String, value: Int) = for {
        m <- mlocal.get
        _ <- mlocal.set(m.updated(name, value))
      } yield ()

      def reproduction(name: String)(value: MIO[Int]) = for {
        _ <- append(s"forward-declare-$name", 1)
        _ <- MIO.scoped { runSafe =>
          runSafe {
            append(s"define-$name", runSafe(value))
          }
        }
      } yield ()

      val program = reproduction("a") {
        reproduction("b")(MIO(10)).as(5)
      } >> mlocal.get

      program.unsafe.runSync._2 === Right(
        Map("forward-declare-a" -> 1, "define-a" -> 5, "forward-declare-b" -> 1, "define-b" -> 10)
      )
    }
  }
}
