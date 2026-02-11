package hearth
package treeprinter

/** Test case for GitHub issue #169: private[this] val in Block context generates invalid code.
  *
  * The issue occurs when the compiler generates `private[this] val x$$N = ...` inside a Block, which is not valid Scala
  * syntax. This typically happens with:
  *   - `val _ = expr` used to suppress unused expression warnings
  *   - Nested Expr.splice in Cross-Quotes
  */
final class PrivateThisInBlockSpec extends MacroSuite {

  test("showCodePretty should not print private[this] modifier in Block context") {
    val printed = ShowCodePrettyFixtures.testExprPrettyPrint {
      val _ = "unused expression"
      42
    }

    // The generated code should not contain "private[this] val" since it's invalid in a Block
    assert(!printed.contains("private[this]"), s"Generated code should not contain 'private[this]' but was:\n$printed")

    // The code should be valid and compile
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

    // The generated code should not contain "private[this] val" since it's invalid in a Block
    assert(!printed.contains("private[this]"), s"Generated code should not contain 'private[this]' but was:\n$printed")

    // The code should be valid and compile
    assert(printed.nonEmpty, "Generated code should not be empty")
  }
}
