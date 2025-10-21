package hearth
package fp

import hearth.fp.syntax.*
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.*

trait Laws { this: ScalaCheckSuite =>

  def functorLaws[F[_]: Functor, A: Arbitrary: ArbitraryF[F, *]](implicit
      loc: munit.Location,
      assertEqA: AssertEq[F[A]],
      assertEqString: AssertEq[F[String]]
  ): Unit = {

    property("Identity law") {
      forAll { (fa: F[A]) =>
        fa.map(identity) === fa
      }
    }

    property("Composition law") {
      forAll { (fa: F[A], b: A) =>
        val f = (_: A) -> b
        val g = (_: (A, A)).toString
        fa.map(f andThen g) === fa.map(f).map(g)
      }
    }
  }

  def applicativeLaws[F[_]: Applicative, A: ArbitraryF[F, *], B: ArbitraryF[F, *], C: ArbitraryF[F, *]](implicit
      loc: munit.Location,
      assertEqFA: AssertEq[F[A]],
      assertEqFAB: AssertEq[F[(A, B, C)]]
  ): Unit = {

    property("Associativity law") {
      forAll { (fa: F[A], fb: F[B], fc: F[C]) =>
        fa.map2(fb)(_ -> _).map2(fc) { case ((a, b), c) => (a, b, c) } ===
          fa.map2(fb.map2(fc)(_ -> _)) { case (a, (b, c)) => (a, b, c) }
      }
    }

    property("Left Identity law") {
      forAll { (fa: F[A]) =>
        ().pure[F].map2(fa)((_, a) => a) === fa
      }
    }

    property("Right Identity law") {
      forAll { (fa: F[A]) =>
        fa.map2(().pure[F])((a, _) => a) === fa
      }
    }
  }

  def parallelLaws[F[_]: Parallel, A: Arbitrary, B: Arbitrary](implicit
      loc: munit.Location,
      assertEqFAB: AssertEq[F[(A, B)]]
  ): Unit =

    property("For pure values .map2 and .parMap2 are equivalent") {
      forAll { (a: A, b: B) =>
        a.pure[F].map2(b.pure[F])(_ -> _) === a.pure[F].parMap2(b.pure[F])(_ -> _)
      }
    }

  def traverseLaws[F[_]: Traverse, G[_]: Parallel, A: ArbitraryF[F, *]](implicit
      loc: munit.Location,
      assertEqFA: AssertEq[F[A]],
      assertEqFGString: AssertEq[G[F[String]]]
  ): Unit = {

    property("Identity law") {
      import hearth.fp.instances.*
      forAll { (fa: F[A]) =>
        fa.traverse(a => a: Id[A]) === fa
        fa.parTraverse(a => a: Id[A]) === fa
      }
    }

    property("Naturality law") {
      forAll { (fa: F[A]) =>
        fa.traverse(a => a.toString.pure[G]) === fa.map(_.toString.pure[G]).sequence
        fa.parTraverse(a => a.toString.pure[G]) === fa.map(_.toString.pure[G]).parSequence
      }
    }
  }

  def directStyleLaws[F[_]: Applicative: DirectStyle, A: Arbitrary: ArbitraryF[F, *]](implicit
      loc: munit.Location,
      assertEqFA: AssertEq[F[A]],
      assertEqFAB: AssertEq[F[(A, A)]]
  ): Unit = {

    property("No runSafe can be used to replace .pure") {
      forAll { (a: A) =>
        DirectStyle[F].scoped { _ =>
          a
        } === a.pure[F]
      }
    }

    property("Single runSafe can be used to replace .map") {
      forAll { (fa: F[A], a: A) =>
        DirectStyle[F].scoped { runSafe =>
          runSafe(fa) -> a
        } === fa.map(_ -> a)
      }
    }

    property("Multiple runSafe can be used to replace .map2") {
      forAll { (fa: F[A], fb: F[A]) =>
        DirectStyle[F].scoped { runSafe =>
          runSafe(fa) -> runSafe(fb)
        } === fa.map2(fb)(_ -> _)
      }
    }
  }
}
