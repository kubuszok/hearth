package hearth
package treeprinter

/** The Scala 2 compiler generates `private[this] val x$$N = ...` inside a Block, which is not valid Scala syntax when
  * printed back. This typically happens with:
  *   - `val _ = expr` used to suppress unused expression warnings
  *   - Nested Expr.splice in Cross-Quotes
  */
final class PrivateThisInBlockSpec extends MacroSuite {

  test("showCodePretty should not print private[this] modifier in Block context") {
    val printed = ShowCodePrettyFixtures.testExprPrettyPrint {
      val _ = "unused expression"
      42
    }

    assert(!printed.contains("private[this]"), s"Generated code should not contain 'private[this]' but was:\n$printed")

    assert(printed.nonEmpty, "Generated code should not be empty")
  }

  test("showCodePretty should not print private[this] modifier in nested blocks") {
    val printed = ShowCodePrettyFixtures.testExprPrettyPrint {
      val _ = {
        val _ = "nested unused"
        "intermediate"
      }
      val result = 42
      result
    }

    assert(!printed.contains("private[this]"), s"Generated code should not contain 'private[this]' but was:\n$printed")

    assert(printed.nonEmpty, "Generated code should not be empty")
  }
}
