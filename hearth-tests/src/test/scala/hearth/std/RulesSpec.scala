package hearth
package std

import scala.collection.immutable.ListMap
import scala.util.{Failure, Success, Try}

final class RulesSpec extends Suite {

  // Mock rule implementations for testing

  /** Rule that always matches */
  final case class MatchingRule(name: String, result: Int) extends Rule {
    def attempt: Rule.Applicability[Int] = Rule.Applicability.Matched(result)
  }

  /** Rule that always yields */
  final case class YieldingRule(name: String, reasons: Vector[String]) extends Rule {
    def attempt: Rule.Applicability[Int] = Rule.Applicability.Yielded(reasons)
  }

  /** Rule that matches conditionally */
  final case class ConditionalRule(name: String, shouldMatch: Boolean, result: Int, reasons: Vector[String])
      extends Rule {
    def attempt: Rule.Applicability[Int] =
      if (shouldMatch) Rule.Applicability.Matched(result)
      else Rule.Applicability.Yielded(reasons)
  }

  // Helper to extract attempt from any rule type
  private def attemptRule(r: Rule): Rule.Applicability[Int] = r match {
    case m: MatchingRule    => m.attempt
    case y: YieldingRule    => y.attempt
    case c: ConditionalRule => c.attempt
  }

  group("Rules factory methods") {

    test("Rules.apply creates Rules from Iterable") {
      val rule1 = MatchingRule("rule1", 1)
      val rule2 = MatchingRule("rule2", 2)
      val rules = Rules.apply(List(rule1, rule2))
      val result = rules.apply(attemptRule)
      result ==> Right(1) // First rule matches
    }

    test("Rules.from creates Rules from varargs") {
      val rule1 = MatchingRule("rule1", 1)
      val rule2 = MatchingRule("rule2", 2)
      val rules = Rules.from(rule1, rule2)
      val result = rules.apply(attemptRule)
      result ==> Right(1) // First rule matches
    }

    test("Empty Iterable produces empty Rules") {
      val rules = Rules.apply(List.empty[MatchingRule])
      val result = rules.apply(attemptRule)
      result ==> Left(ListMap.empty)
    }

    test("Empty varargs produces empty Rules") {
      val rules = Rules.from[MatchingRule]()
      val result = rules.apply(attemptRule)
      result ==> Left(ListMap.empty)
    }
  }

  group("Synchronous rule application") {

    group("Empty rules") {

      test("returns Left with empty ListMap when no rules provided") {
        val rules = Rules.from[MatchingRule]()
        val result = rules.apply(attemptRule)
        result ==> Left(ListMap.empty)
      }
    }

    group("Single rule") {

      test("returns Right with result when rule matches") {
        val rule = MatchingRule("rule1", 42)
        val rules = Rules.from(rule)
        val result = rules.apply(attemptRule)
        result ==> Right(42)
      }

      test("returns Left with rule and reasons when rule yields") {
        val rule = YieldingRule("rule1", Vector("reason1", "reason2"))
        val rules = Rules.from(rule)
        val result = rules.apply(attemptRule)
        result ==> Left(ListMap(rule -> Vector("reason1", "reason2")))
      }

      test("returns Left with empty reasons when rule yields with empty Vector") {
        val rule = YieldingRule("rule1", Vector.empty)
        val rules = Rules.from(rule)
        val result = rules.apply(attemptRule)
        result ==> Left(ListMap(rule -> Vector.empty))
      }
    }

    group("Multiple rules") {

      test("returns Right from first matching rule, doesn't check subsequent rules") {
        val rule1 = MatchingRule("rule1", 1)
        val rule2 = MatchingRule("rule2", 2)
        val rule3 = MatchingRule("rule3", 3)
        val rules = Rules.from(rule1, rule2, rule3)
        val result = rules.apply(attemptRule)
        result ==> Right(1)
      }

      test("returns Right from middle rule when first yields, doesn't check subsequent rules") {
        val rule1 = YieldingRule("rule1", Vector("reason1"))
        val rule2 = MatchingRule("rule2", 42)
        val rule3 = MatchingRule("rule3", 99)
        val rules = Rules.from(rule1, rule2, rule3)
        val result = rules.apply(attemptRule)
        result ==> Right(42)
      }

      test("returns Left with all rules and reasons when all rules yield, preserving order") {
        val rule1 = YieldingRule("rule1", Vector("reason1"))
        val rule2 = YieldingRule("rule2", Vector("reason2a", "reason2b"))
        val rule3 = YieldingRule("rule3", Vector("reason3"))
        val rules = Rules.from(rule1, rule2, rule3)
        val result = rules.apply(attemptRule)
        val expectedMap = ListMap(
          rule1 -> Vector("reason1"),
          rule2 -> Vector("reason2a", "reason2b"),
          rule3 -> Vector("reason3")
        )
        result ==> Left(expectedMap)
      }

      test("preserves rule order in failure map") {
        val rule1 = YieldingRule("rule1", Vector("r1"))
        val rule2 = YieldingRule("rule2", Vector("r2"))
        val rule3 = YieldingRule("rule3", Vector("r3"))
        val rules = Rules.from(rule1, rule2, rule3)
        val result = rules.apply(attemptRule)
        val Left(failureMap) = result: @unchecked
        val keys = failureMap.keys.toList
        keys(0) ==> rule1
        keys(1) ==> rule2
        keys(2) ==> rule3
      }

      test("handles conditional rules correctly") {
        val rule1 = ConditionalRule("rule1", shouldMatch = false, 1, Vector("no match"))
        val rule2 = ConditionalRule("rule2", shouldMatch = true, 42, Vector("should not see"))
        val rule3 = ConditionalRule("rule3", shouldMatch = false, 99, Vector("should not see"))
        val rules = Rules.from(rule1, rule2, rule3)
        val result = rules.apply(attemptRule)
        result ==> Right(42)
      }
    }
  }

  group("Effectful rule application") {

    group("Option effect") {

      test("returns Some(Right(result)) when rule matches") {
        val rule = MatchingRule("rule1", 42)
        val rules = Rules.from(rule)
        val result = rules.apply[Option, Int](r => Some(attemptRule(r)))
        result ==> Some(Right(42))
      }

      test("returns Some(Left(ListMap)) when rule yields") {
        val rule = YieldingRule("rule1", Vector("reason1"))
        val rules = Rules.from(rule)
        val result = rules.apply[Option, Int](r => Some(attemptRule(r)))
        result ==> Some(Left(ListMap(rule -> Vector("reason1"))))
      }

      test("returns None when effect fails") {
        val rule = MatchingRule("rule1", 42)
        val rules = Rules.from(rule)
        val result = rules.apply[Option, Int](_ => None)
        result ==> None
      }

      test("returns Some(Right(result)) from first matching rule with multiple rules") {
        val rule1 = YieldingRule("rule1", Vector("reason1"))
        val rule2 = MatchingRule("rule2", 42)
        val rule3 = MatchingRule("rule3", 99)
        val rules = Rules.from(rule1, rule2, rule3)
        val result = rules.apply[Option, Int](r => Some(attemptRule(r)))
        result ==> Some(Right(42))
      }

      test("returns Some(Left(ListMap)) when all rules yield") {
        val rule1 = YieldingRule("rule1", Vector("reason1"))
        val rule2 = YieldingRule("rule2", Vector("reason2"))
        val rules = Rules.from(rule1, rule2)
        val result = rules.apply[Option, Int](r => Some(attemptRule(r)))
        val expectedMap = ListMap(
          rule1 -> Vector("reason1"),
          rule2 -> Vector("reason2")
        )
        result ==> Some(Left(expectedMap))
      }
    }

    group("Either effect") {

      test("returns Right(Right(result)) when rule matches") {
        val rule = MatchingRule("rule1", 42)
        val rules = Rules.from(rule)
        val result = rules.apply[Either[String, *], Int](r => Right(attemptRule(r)))
        result ==> Right(Right(42))
      }

      test("returns Right(Left(ListMap)) when rule yields") {
        val rule = YieldingRule("rule1", Vector("reason1"))
        val rules = Rules.from(rule)
        val result = rules.apply[Either[String, *], Int](r => Right(attemptRule(r)))
        result ==> Right(Left(ListMap(rule -> Vector("reason1"))))
      }

      test("returns Left(error) when effect fails") {
        val rule = MatchingRule("rule1", 42)
        val rules = Rules.from(rule)
        val result = rules.apply[Either[String, *], Int](_ => Left("effect error"))
        result ==> Left("effect error")
      }

      test("returns Right(Right(result)) from first matching rule with multiple rules") {
        val rule1 = YieldingRule("rule1", Vector("reason1"))
        val rule2 = MatchingRule("rule2", 42)
        val rules = Rules.from(rule1, rule2)
        val result = rules.apply[Either[String, *], Int](r => Right(attemptRule(r)))
        result ==> Right(Right(42))
      }

      test("returns Right(Left(ListMap)) when all rules yield") {
        val rule1 = YieldingRule("rule1", Vector("reason1"))
        val rule2 = YieldingRule("rule2", Vector("reason2"))
        val rules = Rules.from(rule1, rule2)
        val result = rules.apply[Either[String, *], Int](r => Right(attemptRule(r)))
        val expectedMap = ListMap(
          rule1 -> Vector("reason1"),
          rule2 -> Vector("reason2")
        )
        result ==> Right(Left(expectedMap))
      }
    }

    group("Try effect") {

      test("returns Success(Right(result)) when rule matches") {
        val rule = MatchingRule("rule1", 42)
        val rules = Rules.from(rule)
        val result = rules.apply[Try, Int](r => Success(attemptRule(r)))
        result ==> Success(Right(42))
      }

      test("returns Success(Left(ListMap)) when rule yields") {
        val rule = YieldingRule("rule1", Vector("reason1"))
        val rules = Rules.from(rule)
        val result = rules.apply[Try, Int](r => Success(attemptRule(r)))
        result ==> Success(Left(ListMap(rule -> Vector("reason1"))))
      }

      test("returns Failure(exception) when effect fails") {
        val rule = MatchingRule("rule1", 42)
        val rules = Rules.from(rule)
        val exception = new RuntimeException("effect error")
        val result = rules.apply[Try, Int](_ => Failure(exception))
        result ==> Failure(exception)
      }

      test("returns Success(Right(result)) from first matching rule with multiple rules") {
        val rule1 = YieldingRule("rule1", Vector("reason1"))
        val rule2 = MatchingRule("rule2", 42)
        val rules = Rules.from(rule1, rule2)
        val result = rules.apply[Try, Int](r => Success(attemptRule(r)))
        result ==> Success(Right(42))
      }

      test("returns Success(Left(ListMap)) when all rules yield") {
        val rule1 = YieldingRule("rule1", Vector("reason1"))
        val rule2 = YieldingRule("rule2", Vector("reason2"))
        val rules = Rules.from(rule1, rule2)
        val result = rules.apply[Try, Int](r => Success(attemptRule(r)))
        val expectedMap = ListMap(
          rule1 -> Vector("reason1"),
          rule2 -> Vector("reason2")
        )
        result ==> Success(Left(expectedMap))
      }
    }

    group("Id effect") {

      test("behaves like synchronous version when using Id") {
        val rule = MatchingRule("rule1", 42)
        val rules = Rules.from(rule)
        val result = rules.apply[hearth.fp.Id, Int](attemptRule)
        result ==> Right(42)
      }

      test("returns Right(result) from first matching rule with Id") {
        val rule1 = YieldingRule("rule1", Vector("reason1"))
        val rule2 = MatchingRule("rule2", 42)
        val rules = Rules.from(rule1, rule2)
        val result = rules.apply[hearth.fp.Id, Int](attemptRule)
        result ==> Right(42)
      }

      test("returns Left(ListMap) when all rules yield with Id") {
        val rule1 = YieldingRule("rule1", Vector("reason1"))
        val rule2 = YieldingRule("rule2", Vector("reason2"))
        val rules = Rules.from(rule1, rule2)
        val result = rules.apply[hearth.fp.Id, Int](attemptRule)
        val expectedMap = ListMap(
          rule1 -> Vector("reason1"),
          rule2 -> Vector("reason2")
        )
        result ==> Left(expectedMap)
      }
    }
  }
}
