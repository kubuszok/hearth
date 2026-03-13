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

  group("MLocal.unsafeSharedParallel") {

    test("sequential semantics unchanged") {
      val shared = MLocal.unsafeSharedParallel(0)((_, b) => b)

      val program = for {
        _ <- shared.set(1)
        _ <- shared.set(2)
        v <- shared.get
      } yield v

      program.unsafe.runSync._2 === Right(2)
    }

    test("branch B sees branch A's writes") {
      val shared = MLocal.unsafeSharedParallel(0)((_, b) => b)

      val branchA = shared.set(42)
      val branchB = shared.get

      // In parMap2, A runs first, then B. B should see A's write.
      val program = branchA.parMap2(branchB)((_, b) => b)

      program.unsafe.runSync._2 === Right(42)
    }

    test("latest write wins after join") {
      val shared = MLocal.unsafeSharedParallel(0)((_, b) => b)

      val branchA = shared.set(1)
      val branchB = shared.set(2)

      // B runs after A, so B's value (2) should be the post-join result.
      val program = branchA.parMap2(branchB)((_, _) => ()) >> shared.get

      program.unsafe.runSync._2 === Right(2)
    }

    test("no fork applied") {
      // For a regular MLocal, fork would transform the value.
      // For shared, the value should pass through unchanged.
      val shared = MLocal.unsafeSharedParallel(100)((_, b) => b)

      val branchA = shared.get
      val branchB = shared.get

      val program = for {
        _ <- shared.set(42)
        result <- branchA.parMap2(branchB)((a, b) => (a, b))
      } yield result

      program.unsafe.runSync._2 === Right((42, 42))
    }

    test("mixed shared + forked locals") {
      val shared = MLocal.unsafeSharedParallel(0)((_, b) => b)
      val forked = MLocal(0)(identity)(_ + _)

      val branchA = shared.set(10) >> forked.get.flatMap(a => forked.set(a + 1))
      val branchB = shared.get.flatMap(v => forked.get.flatMap(a => forked.set(a + v)))

      val program = for {
        _ <- branchA.parMap2(branchB)((_, _) => ())
        s <- shared.get
        f <- forked.get
      } yield (s, f)

      // shared: B sees A's write (10), post-join latest wins => 10
      // forked: A sets to 1, B sets to 10 (0 + 10), join = 1 + 10 = 11
      program.unsafe.runSync._2 === Right((10, 11))
    }

    test("nested parMap2") {
      val shared = MLocal.unsafeSharedParallel(0)((_, b) => b)

      val innerA = shared.get.flatMap(v => shared.set(v + 1))
      val innerB = shared.get.flatMap(v => shared.set(v + 10))

      val outerA = innerA.parMap2(innerB)((_, _) => ()) >> shared.get
      val outerB = shared.get

      val program = outerA.parMap2(outerB)((a, b) => (a, b))

      // inner: A sets 0+1=1, B sees 1, sets 1+10=11. outerA reads 11.
      // outerB sees 11 (shared from outerA's result).
      program.unsafe.runSync._2 === Right((11, 11))
    }

    test("map accumulation across branches") {
      val shared = MLocal.unsafeSharedParallel(Map.empty[String, Int])(_ ++ _)

      val branchA = shared.get.flatMap(m => shared.set(m.updated("a", 1)))
      val branchB = shared.get.flatMap(m => shared.set(m.updated("b", 2)))

      // A adds "a", B sees {"a" -> 1} and adds "b" on top.
      val program = branchA.parMap2(branchB)((_, _) => ()) >> shared.get

      program.unsafe.runSync._2 === Right(Map("a" -> 1, "b" -> 2))
    }

    test("works with .scoped (DirectStyle)") {
      val shared = MLocal.unsafeSharedParallel(0)((_, b) => b)

      val program = for {
        _ <- MIO.scoped { runSafe =>
          runSafe(shared.set(runSafe(MIO(42))))
        }
        v <- shared.get
      } yield v

      program.unsafe.runSync._2 === Right(42)
    }

    test("untouched shared local preserves value") {
      val shared = MLocal.unsafeSharedParallel(99)((_, b) => b)

      val branchA = MIO(1)
      val branchB = MIO(2)

      val program = branchA.parMap2(branchB)((_, _) => ()) >> shared.get

      program.unsafe.runSync._2 === Right(99)
    }

    test("error aggregation still works") {
      val shared = MLocal.unsafeSharedParallel(0)((_, b) => b)
      val errors = MLocal(Vector.empty[String])(identity)(_ ++ _)

      val branchA = for {
        _ <- shared.set(1)
        _ <- errors.get.flatMap(e => errors.set(e :+ "errorA"))
      } yield ()

      val branchB = for {
        _ <- errors.get.flatMap(e => errors.set(e :+ "errorB"))
      } yield ()

      val program = branchA.parMap2(branchB)((_, _) => ()) >> errors.get

      // Both branches' errors should be aggregated via the forked MLocal's join (++)
      program.unsafe.runSync._2 === Right(Vector("errorA", "errorB"))
    }

    test("branch B reads shared local that A created and modified") {
      val shared = MLocal.unsafeSharedParallel(0)((_, b) => b)

      // Neither branch has set the value before parMap2.
      // A initializes it, B should see A's value.
      val branchA = shared.set(77)
      val branchB = shared.get

      val program = branchA.parMap2(branchB)((_, b) => b)

      program.unsafe.runSync._2 === Right(77)
    }

    test("chained parMap2 with shared local accumulation") {
      val shared = MLocal.unsafeSharedParallel(Map.empty[String, Int])(_ ++ _)

      def addEntry(key: String, value: Int): MIO[Unit] =
        shared.get.flatMap(m => shared.set(m.updated(key, value)))

      // Chain multiple parMap2 calls — shared state should accumulate across all of them.
      val program = for {
        _ <- addEntry("x", 1).parMap2(addEntry("y", 2))((_, _) => ())
        _ <- addEntry("z", 3).parMap2(addEntry("w", 4))((_, _) => ())
        result <- shared.get
      } yield result

      program.unsafe.runSync._2 === Right(Map("x" -> 1, "y" -> 2, "z" -> 3, "w" -> 4))
    }
  }
}
