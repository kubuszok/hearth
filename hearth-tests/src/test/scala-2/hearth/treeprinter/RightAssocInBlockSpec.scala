package hearth
package treeprinter

/** Test that right-associative operator desugaring produces unique synthetic val names.
  *
  * In Scala 2, `a :: b` with complex `a` desugars to `{ val rassoc$N = a; b.::(rassoc$N) }`. When nested deeply (e.g.
  * 12-element list built with `foldLeft` + `Expr.quote`), all synthetic vals must have unique names. Otherwise, the
  * compiler loses track of bindings at deep nesting and emits `not found: value rassoc$1`.
  */
final class RightAssocInBlockSpec extends MacroSuite {

  test("showCodePretty should preserve unique suffixes on synthetic rassoc vals") {
    val printed = ShowCodePrettyFixtures.testExprPlainPrint {
      identity(1) :: identity(2) :: identity(3) :: identity(4) :: identity(5) :: identity(6) :: Nil
    }
    // Each nested :: creates a unique rassoc$N val.
    // removeMacroSuffix must NOT strip the $N suffix from compiler-generated names.
    assert(
      printed.contains("rassoc$"),
      s"Generated code should contain 'rassoc$$N' with unique suffixes but was:\n$printed"
    )

    // All 6 rassoc vals should have distinct names
    val rassocPattern = "rassoc\\$\\d+".r
    val rassocNames = rassocPattern.findAllIn(printed).toList
    val uniqueNames = rassocNames.distinct
    assert(
      uniqueNames.size == 6,
      s"Expected 6 unique rassoc names but found ${uniqueNames.size}: ${uniqueNames.mkString(", ")}\nin:\n$printed"
    )
  }

  test("showCodePretty should preserve unique suffixes for simple :: with complex expression") {
    val printed = ShowCodePrettyFixtures.testExprPlainPrint {
      identity(1) :: Nil
    }
    assert(
      printed.contains("rassoc$"),
      s"Generated code should contain 'rassoc$$N' but was:\n$printed"
    )
  }

  test("removeMacroSuffix should still strip Hearth $macro$ suffixes") {
    val dollar = "$"
    val input = s"val foo${dollar}macro${dollar}1 = 42; val bar${dollar}macro${dollar}2 = foo${dollar}macro${dollar}1"
    val cleaned = hearth.removeMacroSuffix(input)
    assert(
      !cleaned.contains(s"${dollar}macro${dollar}"),
      s"Hearth macro suffixes should be removed but got: $cleaned"
    )
    assert(
      cleaned.contains("val foo = 42"),
      s"Expected clean 'foo' name but got: $cleaned"
    )
  }
}
