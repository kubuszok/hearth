package hearth

import org.scalacheck.{Arbitrary, Gen, Prop, Test as ScalaCheckTest}
import org.scalacheck.util.Pretty
import org.scalacheck.rng.Seed
import munit.{FailException, FailExceptionLike, Location, TestOptions}
import munit.internal.FutureCompat.*

import scala.util.{Failure, Success, Try}
import scala.language.implicitConversions

/** Copy-pasted from
  * https://github.com/typelevel/munit-scalacheck/blob/main/src/main/scala/munit/scalacheck/ScalaCheckSuite.scala but
  * with a different parent - [[hearth.Suite]] instead of [[munit.FunSuite]], so it can be nested in
  * [[hearth.Suite#group]].
  */
trait ScalaCheckSuite extends Suite {

  def property(name: String)(body: => Prop)(implicit loc: Location): Unit =
    property(new TestOptions(name, Set.empty, loc))(body)

  def property(options: TestOptions)(body: => Prop)(implicit loc: Location): Unit =
    test(options)(body)

  // Allow property bodies of type Unit
  // This is done to support using MUnit assertions in property bodies
  // instead of returning a Boolean.
  implicit def unitToProp(unit: Unit): Prop = Prop.passed

  override def munitTestTransforms: List[TestTransform] =
    scalaCheckPropTransform +: super.munitTestTransforms

  protected def scalaCheckTestParameters = ScalaCheckTest.Parameters.default

  protected def scalaCheckPrettyParameters = Pretty.defaultParams

  protected def scalaCheckInitialSeed: String = Seed.random().toBase64

  private val scalaCheckPropTransform: TestTransform =
    new TestTransform(
      "ScalaCheck Prop",
      t =>
        t.withBodyMap(
          _.transformCompat {
            case Success(prop: Prop) => propToTry(prop, t)
            case r                   => r
          }(munitExecutionContext)
        )
    )

  private def propToTry(prop: Prop, test: Test): Try[Unit] = {
    import ScalaCheckTest.*
    def makeSeed() =
      scalaCheckTestParameters.initialSeed.getOrElse(
        Seed.fromBase64(scalaCheckInitialSeed).get
      )
    val initialSeed = makeSeed()
    var seed: Seed = initialSeed
    val result = check(
      scalaCheckTestParameters,
      Prop { genParams =>
        val r = prop(genParams.withInitialSeed(seed))
        seed = seed.next
        r
      }
    )
    def renderResult(r: Result): String = {
      val resultMessage = Pretty.pretty(r, scalaCheckPrettyParameters)
      if (r.passed) {
        resultMessage
      } else {
        val seedMessage =
          s"""|Failing seed: ${initialSeed.toBase64}
              |You can reproduce this failure by adding the following override to your suite:
              |
              |  override def scalaCheckInitialSeed = "${initialSeed.toBase64}"
              |""".stripMargin
        seedMessage + "\n" + resultMessage
      }
    }

    result.status match {
      case Passed | Proved(_) =>
        Success(())
      case status @ PropException(_, e, _) =>
        e match {
          case f: FailExceptionLike[?] =>
            // Promote FailException (i.e failed assertions) to property failures
            val r = result.copy(status = Failed(status.args, status.labels))
            Failure(f.withMessage(e.getMessage + "\n\n" + renderResult(r)))
          case _ =>
            Failure(
              new FailException(
                message = renderResult(result),
                cause = e,
                isStackTracesEnabled = false,
                location = test.location
              )
            )
        }
      case _ =>
        // Fail using the test location
        implicit val loc = test.location
        Try(fail("\n" + renderResult(result)))
    }
  }

  // Our own extensions

  type ArbitraryF[F[_], A] = Arbitrary[F[A]]

  trait AssertEq[A] {

    def assertEq(a: A, b: A)(implicit loc: munit.Location): Unit
  }
  object AssertEq extends LowPriorityAssertEq {

    implicit val AssertEqForData: AssertEq[hearth.data.Data] = new AssertEq[hearth.data.Data] {

      def assertEq(a: hearth.data.Data, b: hearth.data.Data)(implicit loc: munit.Location): Unit = a <==> b
    }

    implicit val AssertEqForString: AssertEq[String] = new AssertEq[String] {

      def assertEq(a: String, b: String)(implicit loc: munit.Location): Unit = a <==> b
    }

    implicit def AssertEqForMIO[A]: AssertEq[hearth.fp.effect.MIO[A]] = new AssertEq[hearth.fp.effect.MIO[A]] {

      def assertEq(a: hearth.fp.effect.MIO[A], b: hearth.fp.effect.MIO[A])(implicit loc: munit.Location): Unit =
        a.unsafe.runSync._2 ==> b.unsafe.runSync._2
    }
  }
  private[ScalaCheckSuite] trait LowPriorityAssertEq {

    implicit def AssertEqForArrowAssert[A]: AssertEq[A] = new AssertEq[A] {

      def assertEq(a: A, b: A)(implicit loc: munit.Location): Unit = a ==> b
    }
  }

  implicit class AssertEqOps[A](a: A) {

    def ===(b: A)(implicit assertEq: AssertEq[A], loc: munit.Location): Unit = assertEq.assertEq(a, b)
  }

  // Special instances for Hearth types

  implicit def ArbitraryForNonEmptyList[A: Arbitrary]: Arbitrary[hearth.fp.data.NonEmptyList[A]] = Arbitrary(
    Gen.nonEmptyListOf(Arbitrary.arbitrary[A]).map(hearth.fp.data.NonEmptyList.fromList(_).get)
  )
  implicit def ArbitraryForNonEmptyVector[A: Arbitrary]: Arbitrary[hearth.fp.data.NonEmptyVector[A]] = Arbitrary(
    Gen.nonEmptyListOf(Arbitrary.arbitrary[A]).map(_.toVector).map(hearth.fp.data.NonEmptyVector.fromVector(_).get)
  )
  implicit def ArbitraryForMIO[A: Arbitrary]: Arbitrary[hearth.fp.effect.MIO[A]] = Arbitrary(
    Gen.oneOf(
      Gen.const(hearth.fp.effect.MIO.pure(Arbitrary.arbitrary[A].sample.get)),
      Gen.const(hearth.fp.effect.MIO.fail(ExampleError(Arbitrary.arbitrary[String].sample.get)))
    )
  )
  private case class ExampleError(message: String) extends Throwable(message)
}
