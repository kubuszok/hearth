package hearth
package fp
package effect

import org.scalacheck.Prop.*

final class MEvalSpec extends ScalaCheckSuite with Laws {

  group("MEval's monadic family of methods'") {

    test(".flatMap should follow Left Identity law") {
      forAll { (a: Int, f: Int => MEval[Int]) =>
        MEval.pure(a).flatMap(f) === f(a)
      }
    }

    test(".flatMap should follow Right Identity law") {
      forAll { (fa: MEval[Int]) =>
        fa.flatMap(MEval.pure) === fa
      }
    }

    test(".flatMap should follow Associative law") {
      forAll { (fa: MEval[Int], f: Int => MEval[Int], g: Int => MEval[Int]) =>
        fa.flatMap(f).flatMap(g) === fa.flatMap(a => f(a).flatMap(g))
      }
    }

    test(".flatten should behave like .flatMap(identity)") {

      forAll { (fa: MEval[MEval[Int]]) =>
        fa.flatten === fa.flatMap(identity)
      }
    }

    test(".flatTap should behave like .flatMap(a => f(a).as(a)) (perform side-effects, but discarding results)") {
      forAll { (fa: MEval[Int], f: Int => MEval[Unit]) =>
        fa.flatTap(f) === fa.flatMap(a => f(a).as(a))
      }
    }

    test(".map should follow Identity law") {
      forAll { (fa: MEval[Int]) =>
        fa.map(identity) === fa
      }
    }

    test(".map should follow Composition law") {
      forAll { (fa: MEval[Int], f: Int => Int, g: Int => Int) =>
        fa.map(f).map(g) === fa.map(f andThen g)
      }
    }

    test(".mapTap should behave like .map(a => ignore(f(a)); a)") {
      forAll { (fa: MEval[Int], f: Int => Int) =>
        fa.mapTap(f) === fa.map { a => ignore(f(a)); a }
      }
    }

    test(".map2 should follow Associativity law") {
      forAll { (fa: MEval[Int], fb: MEval[Int], fc: MEval[Int]) =>
        fa.map2(fb)(_ -> _).map2(fc) { case ((a, b), c) => (a, b, c) } === fa.map2(fb.map2(fc)(_ -> _)) {
          case (a, (b, c)) => (a, b, c)
        }
      }
    }

    test(".map2 should follow Left Identity law") {
      forAll { (fa: MEval[Int]) =>
        fa.map2(MEval.void)((a, _) => a) === fa
      }
    }

    test(".map2 should follow Right Identity law") {
      forAll { (fa: MEval[Int]) =>
        MEval.void.map2(fa)((_, b) => b) === fa
      }
    }

    test(".tuple should follow Left Identity law") {
      forAll { (fa: MEval[Int]) =>
        fa.tuple(MEval.void).map(_._1) === fa
      }
    }

    test(".tuple should follow Right Identity law") {
      forAll { (fa: MEval[Int]) =>
        MEval.void.tuple(fa).map(_._2) === fa
      }
    }

    test(".as should map successful value to pure value") {
      forAll { (fa: MEval[Int], b: Int) =>
        fa.as(b) === fa.map(_ => b)
      }
    }

    test(".void should map successful value to Unit value") {
      forAll { (fa: MEval[Int]) =>
        fa.void === fa.map(_ => ())
      }
    }

    test(">> should behave like .flatMap(_ => fb)") {
      forAll { (fa: MEval[Int], fb: MEval[Int]) =>
        fa >> fb === fa.flatMap(_ => fb)
      }
    }

    test("<* should behave like .map2(fb)((a, _) => a)") {
      forAll { (fa: MEval[Int], fb: MEval[Int]) =>
        fa <* fb === fa.map2(fb)((a, _) => a)
      }
    }

    test("*> should behave like .map2(fb)((_, b) => b)") {
      forAll { (fa: MEval[Int], fb: MEval[Int]) =>
        fa *> fb === fa.map2(fb)((_, b) => b)
      }
    }
  }

  group("MEval") {

    test("should be stack-safe with .flatMap") {
      def countdown(n: Int): MEval[Unit] =
        if (n <= 0) MEval.void
        else MEval(n - 1).flatMap(countdown)

      countdown(100000) === MEval.void
    }

    // I have not found a way to make this stack-safe.
    // test("should be stack-safe with .scoped") {
    //   def countdown(n: Int): MEval[Unit] = MEval.scoped { runSafe =>
    //     if (n <= 0) ()
    //     else runSafe(countdown(n - 1))
    //   }

    //   countdown(100000) === MEval.void
    // }
  }

  group("Instances for MEval") {

    group("should follow Functor laws") {
      functorLaws[MEval, Int]
    }

    group("should follow Applicative laws") {
      applicativeLaws[MEval, Int, String, Int]
    }

    group("should follow DirectStyle laws") {
      directStyleLaws[MEval, Int]
    }
  }
}
