package hearth
package fp

import hearth.fp.data.{NonEmptyList, NonEmptyVector}
import hearth.fp.instances.*
import hearth.fp.syntax.*

import scala.util.{Failure, Success, Try}

/** The laws only compare pure values, so here we check how the instances behave when some values are failures. */
final class ErrorAggregationSpec extends Suite {

  private val exception1 = new RuntimeException("error 1")
  private val exception2 = new RuntimeException("error 2")

  group("Applicative.map2") {

    test("should keep only the first error and not evaluate the second value after a failure") {
      var evaluated = 0
      def second[F[_]](value: F[Int]): F[Int] = { evaluated += 1; value }

      (Left("error 1"): Either[String, Int]).map2(second[Either[String, *]](Left("error 2")))(_ + _) ==> Left(
        "error 1"
      )
      (Left(List("error 1")): Either[List[String], Int])
        .map2(second[Either[List[String], *]](Left(List("error 2"))))(_ + _) ==> Left(List("error 1"))
      (Left(Vector("error 1")): Either[Vector[String], Int])
        .map2(second[Either[Vector[String], *]](Left(Vector("error 2"))))(_ + _) ==> Left(Vector("error 1"))
      (Left(NonEmptyList.one("error 1")): Either[NonEmptyList[String], Int])
        .map2(second[Either[NonEmptyList[String], *]](Left(NonEmptyList.one("error 2"))))(_ + _) ==> Left(
        NonEmptyList.one("error 1")
      )
      (Left(NonEmptyVector.one("error 1")): Either[NonEmptyVector[String], Int])
        .map2(second[Either[NonEmptyVector[String], *]](Left(NonEmptyVector.one("error 2"))))(_ + _) ==> Left(
        NonEmptyVector.one("error 1")
      )
      (None: Option[Int]).map2(second[Option](Some(1)))(_ + _) ==> None
      (Failure(exception1): Try[Int]).map2(second[Try](Failure(exception2)))(_ + _) ==> Failure(exception1)
      evaluated ==> 0
    }

    test("should combine the values when both succeed") {
      (Right(1): Either[String, Int]).map2(Right(2): Either[String, Int])(_ + _) ==> Right(3)
      (Some(1): Option[Int]).map2(Some(2): Option[Int])(_ + _) ==> Some(3)
      (Success(1): Try[Int]).map2(Success(2): Try[Int])(_ + _) ==> Success(3)
      (Right(1): Either[String, Int]).map2(Left("error"): Either[String, Int])(_ + _) ==> Left("error")
    }
  }

  group("Parallel.parMap2") {

    test("should aggregate the errors of both values, in order") {
      (Left(List("error 1")): Either[List[String], Int]).parMap2(Left(List("error 2")): Either[List[String], Int])(
        _ + _
      ) ==> Left(List("error 1", "error 2"))
      (Left(Vector("error 1")): Either[Vector[String], Int])
        .parMap2(Left(Vector("error 2")): Either[Vector[String], Int])(_ + _) ==> Left(Vector("error 1", "error 2"))
      (Left(NonEmptyList("error 1", "error 2")): Either[NonEmptyList[String], Int])
        .parMap2(Left(NonEmptyList.one("error 3")): Either[NonEmptyList[String], Int])(_ + _) ==> Left(
        NonEmptyList("error 1", "error 2", "error 3")
      )
      (Left(NonEmptyVector.one("error 1")): Either[NonEmptyVector[String], Int])
        .parMap2(Left(NonEmptyVector("error 2", "error 3")): Either[NonEmptyVector[String], Int])(_ + _) ==> Left(
        NonEmptyVector("error 1", "error 2", "error 3")
      )
    }

    test("should evaluate the second value even if the first one failed") {
      var evaluated = 0
      def second[F[_]](value: F[Int]): F[Int] = { evaluated += 1; value }

      (Left(List("error 1")): Either[List[String], Int])
        .parMap2(second[Either[List[String], *]](Right(2)))(_ + _) ==> Left(List("error 1"))
      (Left(Vector("error 1")): Either[Vector[String], Int])
        .parMap2(second[Either[Vector[String], *]](Right(2)))(_ + _) ==> Left(Vector("error 1"))
      (Left(NonEmptyList.one("error 1")): Either[NonEmptyList[String], Int])
        .parMap2(second[Either[NonEmptyList[String], *]](Right(2)))(_ + _) ==> Left(NonEmptyList.one("error 1"))
      (Left(NonEmptyVector.one("error 1")): Either[NonEmptyVector[String], Int])
        .parMap2(second[Either[NonEmptyVector[String], *]](Right(2)))(_ + _) ==> Left(NonEmptyVector.one("error 1"))
      (None: Option[Int]).parMap2(second[Option](Some(1)))(_ + _) ==> None
      evaluated ==> 5
    }

    test("should keep the error of the only failing value and combine the values when both succeed") {
      (Right(1): Either[List[String], Int]).parMap2(Left(List("error 2")): Either[List[String], Int])(_ + _) ==> Left(
        List("error 2")
      )
      (Right(1): Either[List[String], Int]).parMap2(Right(2): Either[List[String], Int])(_ + _) ==> Right(3)
      (Right(1): Either[List[String], Int]).parTuple(Right("a"): Either[List[String], String]) ==> Right((1, "a"))
      (Some(1): Option[Int]).parMap2(Some(2): Option[Int])(_ + _) ==> Some(3)
    }

    test("Id should just apply the function") {
      Parallel[Id].parMap2(1, 2)(_ + _) ==> 3
      Applicative[Id].map2(1, 2)(_ + _) ==> 3
      Applicative[Id].pure(1) ==> 1
    }
  }

  group("Applicative.pure syntax") {

    test("should lift a value into Option, Either, List and Vector") {
      "hello".pure[Option] ==> Some("hello")
      42.pure[Either[String, *]] ==> Right(42)
      42.pure[Either[List[String], *]] ==> Right(42)
      42.pure[List] ==> List(42)
      42.pure[Vector] ==> Vector(42)
      42.pure[Try] ==> Success(42)
    }
  }

  group("Traverse.traverse / Traverse.parTraverse") {

    def validate(visited: scala.collection.mutable.ListBuffer[Int])(i: Int): Either[List[String], Int] = {
      visited += i
      if (i % 2 == 0) Right(i / 2) else Left(List(s"$i is odd"))
    }

    test("traverse should stop at the first failure and keep only its error") {
      val visited = scala.collection.mutable.ListBuffer.empty[Int]
      List(2, 1, 4, 3).traverse(validate(visited)) ==> Left(List("1 is odd"))
      visited.toList ==> List(2, 1)
    }

    test("parTraverse should visit every element and aggregate all errors in order") {
      val visited = scala.collection.mutable.ListBuffer.empty[Int]
      List(2, 1, 4, 3).parTraverse(validate(visited)) ==> Left(List("1 is odd", "3 is odd"))
      visited.toList ==> List(2, 1, 4, 3)
    }

    test("traverse and parTraverse should succeed when every element succeeds") {
      val visited = scala.collection.mutable.ListBuffer.empty[Int]
      List(2, 4).traverse(validate(visited)) ==> Right(List(1, 2))
      List(2, 4).parTraverse(validate(visited)) ==> Right(List(1, 2))
      Vector(2, 4).parTraverse(validate(visited)) ==> Right(Vector(1, 2))
      NonEmptyList(2, 4).parTraverse(validate(visited)) ==> Right(NonEmptyList(1, 2))
      NonEmptyVector(2, 4).parTraverse(validate(visited)) ==> Right(NonEmptyVector(1, 2))
      List.empty[Int].parTraverse(validate(visited)) ==> Right(List.empty[Int])
    }

    test("parTraverse should aggregate errors for every traversable data type") {
      val visited = scala.collection.mutable.ListBuffer.empty[Int]
      Vector(1, 2, 3).parTraverse(validate(visited)) ==> Left(List("1 is odd", "3 is odd"))
      NonEmptyList(1, 2, 3).parTraverse(validate(visited)) ==> Left(List("1 is odd", "3 is odd"))
      NonEmptyVector(1, 2, 3).parTraverse(validate(visited)) ==> Left(List("1 is odd", "3 is odd"))
      (Some(1): Option[Int]).parTraverse(validate(visited)) ==> Left(List("1 is odd"))
      (None: Option[Int]).parTraverse(validate(visited)) ==> Right(None)
      (Right(1): Either[String, Int]).parTraverse(validate(visited)) ==> Left(List("1 is odd"))
      (Left("not traversed"): Either[String, Int]).parTraverse(validate(visited)) ==> Right(Left("not traversed"))
      (Success(2): Try[Int]).traverse(validate(visited)) ==> Right(Success(1))
      (Failure(exception1): Try[Int]).traverse(validate(visited)) ==> Right(Failure(exception1))
    }

    test("sequence should fail fast and parSequence should aggregate errors") {
      val values: List[Either[List[String], Int]] = List(Right(1), Left(List("error 1")), Left(List("error 2")))
      values.sequence ==> Left(List("error 1"))
      values.parSequence ==> Left(List("error 1", "error 2"))
      List[Option[Int]](Some(1), Some(2)).sequence ==> Some(List(1, 2))
      List[Option[Int]](Some(1), None).sequence ==> None
    }

    test("Functor.map derived from traverse should transform every element") {
      Functor[NonEmptyList].map(NonEmptyList(1, 2, 3))(_ * 2) ==> NonEmptyList(2, 4, 6)
      Functor[NonEmptyVector].map(NonEmptyVector(1, 2, 3))(_ * 2) ==> NonEmptyVector(2, 4, 6)
    }
  }

  group("DirectStyle") {

    test("should not run the rest of the block after the first failure") {
      var reached = false
      DirectStyle[Either[List[String], *]].scoped { runSafe =>
        val a = runSafe(Right(1): Either[List[String], Int])
        val b = runSafe(Left(List("error 1")): Either[List[String], Int])
        reached = true
        val c = runSafe(Left(List("error 2")): Either[List[String], Int])
        a + b + c
      } ==> Left(List("error 1"))
      reached ==> false

      DirectStyle[Option].scoped { runSafe =>
        val a = runSafe(None: Option[Int])
        reached = true
        a
      } ==> None
      reached ==> false

      DirectStyle[Try].scoped { runSafe =>
        val a = runSafe(Failure(exception1): Try[Int])
        reached = true
        a
      } ==> Failure(exception1)
      reached ==> false
    }

    test("a failure inside a nested scope should only fail that scope") {
      DirectStyle[Option].scoped { outer =>
        val inner = DirectStyle[Option].scoped { inner =>
          inner(None: Option[Int])
        }
        outer(Some(1): Option[Int]) -> inner
      } ==> Some((1, None))
    }

    test("a failure of the outer runSafe used inside a nested scope should fail the outer scope") {
      DirectStyle[Either[String, *]].scoped { outer =>
        val inner = DirectStyle[Either[String, *]].scoped { _ =>
          outer(Left("outer error"): Either[String, Int])
        }
        inner
      } ==> Left("outer error")
    }
  }
}
