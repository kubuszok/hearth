package hearth
package std

import hearth.fp.Id
import hearth.fp.data.{NonEmptyList, NonEmptyMap, NonEmptyVector}
import scala.util.{Failure, Success, Try}

final class RulesSpec extends Suite {

  // Mock rule implementations for testing

  /** Rule that always matches */
  final case class MatchingRule(name: String, result: Int) extends Rule {
    def attempt: Rule.Applicability[Int] = Rule.matched(result)
  }

  /** Rule that always yields */
  final case class YieldingRule(name: String, reasons: Vector[String]) extends Rule {
    def attempt: Rule.Applicability[Int] = Rule.yielded(reasons*)
  }

  /** Rule that matches conditionally */
  final case class ConditionalRule(name: String, shouldMatch: Boolean, result: Int, reasons: Vector[String])
      extends Rule {
    def attempt: Rule.Applicability[Int] =
      if (shouldMatch) Rule.matched(result)
      else Rule.yielded(reasons*)
  }

  // Helper to extract attempt from any rule type
  private def attemptRule(r: Rule): Rule.Applicability[Int] = r match {
    case m: MatchingRule    => m.attempt
    case y: YieldingRule    => y.attempt
    case c: ConditionalRule => c.attempt
  }

  group("Rules factory methods") {

    test("Rules.apply creates Rules from NonEmptyList") {
      val rule1 = MatchingRule("rule1", 1)
      val rule2 = MatchingRule("rule2", 2)
      Rules.from(NonEmptyList(rule1, rule2))(attemptRule) ==> Right(1) // First rule matches
    }

    test("Rules.apply creates Rules from NonEmptyVector") {
      val rule1 = MatchingRule("rule1", 1)
      val rule2 = MatchingRule("rule2", 2)
      Rules.from(NonEmptyVector(rule1, rule2))(attemptRule) ==> Right(1) // First rule matches
    }

    test("Rules.from creates Rules from varargs") {
      val rule1 = MatchingRule("rule1", 1)
      val rule2 = MatchingRule("rule2", 2)
      Rules(rule1, rule2)(attemptRule) ==> Right(1) // First rule matches
    }
  }

  group("Synchronous rule application") {

    group("Single rule") {

      test("returns Right with result when rule matches") {
        val rule = MatchingRule("rule1", 42)
        Rules(rule)(attemptRule) ==> Right(42)
      }

      test("returns Left with rule and reasons when rule yields") {
        val rule = YieldingRule("rule1", Vector("reason1", "reason2"))
        Rules(rule)(attemptRule) ==> Left(NonEmptyMap(rule -> Vector("reason1", "reason2")))
      }

      test("returns Left with empty reasons when rule yields with empty Vector") {
        val rule = YieldingRule("rule1", Vector.empty)
        Rules(rule)(attemptRule) ==> Left(NonEmptyMap(rule -> Vector.empty))
      }
    }

    group("Multiple rules") {

      test("returns Right from first matching rule, doesn't check subsequent rules") {
        val rule1 = MatchingRule("rule1", 1)
        val rule2 = MatchingRule("rule2", 2)
        val rule3 = MatchingRule("rule3", 3)
        Rules(rule1, rule2, rule3)(attemptRule) ==> Right(1)
      }

      test("returns Right from middle rule when first yields, doesn't check subsequent rules") {
        val rule1 = YieldingRule("rule1", Vector("reason1"))
        val rule2 = MatchingRule("rule2", 42)
        val rule3 = MatchingRule("rule3", 99)
        Rules(rule1, rule2, rule3)(attemptRule) ==> Right(42)
      }

      test("returns Left with all rules and reasons when all rules yield, preserving order") {
        val rule1 = YieldingRule("rule1", Vector("reason1"))
        val rule2 = YieldingRule("rule2", Vector("reason2a", "reason2b"))
        val rule3 = YieldingRule("rule3", Vector("reason3"))
        Rules(rule1, rule2, rule3)(attemptRule) ==> Left(
          NonEmptyMap(rule1 -> Vector("reason1"), rule2 -> Vector("reason2a", "reason2b"), rule3 -> Vector("reason3"))
        )
      }

      test("preserves rule order in failure map") {
        val rule1 = YieldingRule("rule1", Vector("r1"))
        val rule2 = YieldingRule("rule2", Vector("r2"))
        val rule3 = YieldingRule("rule3", Vector("r3"))
        Rules(rule1, rule2, rule3)(attemptRule) ==> Left(
          NonEmptyMap(rule1 -> Vector("r1"), rule2 -> Vector("r2"), rule3 -> Vector("r3"))
        )
      }

      test("handles conditional rules correctly") {
        val rule1 = ConditionalRule("rule1", shouldMatch = false, 1, Vector("no match"))
        val rule2 = ConditionalRule("rule2", shouldMatch = true, 42, Vector("should not see"))
        val rule3 = ConditionalRule("rule3", shouldMatch = false, 99, Vector("should not see"))
        Rules(rule1, rule2, rule3)(attemptRule) ==> Right(42)
      }
    }
  }

  group("Effectful rule application") {

    group("Option effect") {

      test("returns Some(Right(result)) when rule matches") {
        val rule = MatchingRule("rule1", 42)
        Rules(rule)[Option, Int](rule => Some(attemptRule(rule))) ==> Some(Right(42))
      }

      test("returns Some(Left(ListMap)) when rule yields") {
        val rule = YieldingRule("rule1", Vector("reason1"))
        Rules(rule)[Option, Int](rule => Some(attemptRule(rule))) ==> Some(Left(NonEmptyMap(rule -> Vector("reason1"))))
      }

      test("returns None when effect fails") {
        val rule = MatchingRule("rule1", 42)
        Rules(rule)[Option, Int](_ => None) ==> None
      }

      test("returns Some(Right(result)) from first matching rule with multiple rules") {
        val rule1 = YieldingRule("rule1", Vector("reason1"))
        val rule2 = MatchingRule("rule2", 42)
        val rule3 = MatchingRule("rule3", 99)
        Rules(rule1, rule2, rule3)[Option, Int](rule => Some(attemptRule(rule))) ==> Some(Right(42))
      }

      test("returns Some(Left(ListMap)) when all rules yield") {
        val rule1 = YieldingRule("rule1", Vector("reason1"))
        val rule2 = YieldingRule("rule2", Vector("reason2"))
        Rules(rule1, rule2)[Option, Int](rule => Some(attemptRule(rule))) ==> Some(
          Left(
            NonEmptyMap(
              rule1 -> Vector("reason1"),
              rule2 -> Vector("reason2")
            )
          )
        )
      }
    }

    group("Either effect") {

      test("returns Right(Right(result)) when rule matches") {
        val rule = MatchingRule("rule1", 42)
        Rules(rule)[Either[String, *], Int](rule => Right(attemptRule(rule))) ==> Right(Right(42))
      }

      test("returns Right(Left(ListMap)) when rule yields") {
        val rule = YieldingRule("rule1", Vector("reason1"))
        Rules(rule)[Either[String, *], Int](rule => Right(attemptRule(rule))) ==> Right(
          Left(NonEmptyMap(rule -> Vector("reason1")))
        )
      }

      test("returns Left(error) when effect fails") {
        val rule = MatchingRule("rule1", 42)
        Rules(rule)[Either[String, *], Int](_ => Left("effect error")) ==> Left("effect error")
      }

      test("returns Right(Right(result)) from first matching rule with multiple rules") {
        val rule1 = YieldingRule("rule1", Vector("reason1"))
        val rule2 = MatchingRule("rule2", 42)
        Rules(rule1, rule2)[Either[String, *], Int](rule => Right(attemptRule(rule))) ==> Right(Right(42))
      }

      test("returns Right(Left(ListMap)) when all rules yield") {
        val rule1 = YieldingRule("rule1", Vector("reason1"))
        val rule2 = YieldingRule("rule2", Vector("reason2"))
        Rules(rule1, rule2)[Either[String, *], Int](rule => Right(attemptRule(rule))) ==> Right(
          Left(
            NonEmptyMap(
              rule1 -> Vector("reason1"),
              rule2 -> Vector("reason2")
            )
          )
        )
      }
    }

    group("Try effect") {

      test("returns Success(Right(result)) when rule matches") {
        val rule = MatchingRule("rule1", 42)
        Rules(rule)[Try, Int](rule => Success(attemptRule(rule))) ==> Success(Right(42))
      }

      test("returns Success(Left(ListMap)) when rule yields") {
        val rule = YieldingRule("rule1", Vector("reason1"))
        Rules(rule)[Try, Int](rule => Success(attemptRule(rule))) ==> Success(
          Left(NonEmptyMap(rule -> Vector("reason1")))
        )
      }

      test("returns Failure(exception) when effect fails") {
        val rule = MatchingRule("rule1", 42)
        val exception = new RuntimeException("effect error")
        Rules(rule)[Try, Int](_ => Failure(exception)) ==> Failure(exception)
      }

      test("returns Success(Right(result)) from first matching rule with multiple rules") {
        val rule1 = YieldingRule("rule1", Vector("reason1"))
        val rule2 = MatchingRule("rule2", 42)
        Rules(rule1, rule2)[Try, Int](rule => Success(attemptRule(rule))) ==> Success(Right(42))
      }

      test("returns Success(Left(ListMap)) when all rules yield") {
        val rule1 = YieldingRule("rule1", Vector("reason1"))
        val rule2 = YieldingRule("rule2", Vector("reason2"))
        Rules(rule1, rule2)[Try, Int](rule => Success(attemptRule(rule))) ==> Success(
          Left(
            NonEmptyMap(
              rule1 -> Vector("reason1"),
              rule2 -> Vector("reason2")
            )
          )
        )
      }
    }

    group("Id effect") {

      test("behaves like synchronous version when using Id") {
        val rule = MatchingRule("rule1", 42)
        Rules(rule)[Id, Int](rule => attemptRule(rule)) ==> Right(42)
      }

      test("returns Right(result) from first matching rule with Id") {
        val rule1 = YieldingRule("rule1", Vector("reason1"))
        val rule2 = MatchingRule("rule2", 42)
        Rules(rule1, rule2)[Id, Int](rule => attemptRule(rule)) ==> Right(42)
      }

      test("returns Left(ListMap) when all rules yield with Id") {
        val rule1 = YieldingRule("rule1", Vector("reason1"))
        val rule2 = YieldingRule("rule2", Vector("reason2"))
        Rules(rule1, rule2)[Id, Int](rule => attemptRule(rule)) ==> Left(
          NonEmptyMap(
            rule1 -> Vector("reason1"),
            rule2 -> Vector("reason2")
          )
        )
      }
    }
  }
}
