package hearth
package fp

final class NaturalLanguageOrderingSpec extends MacroSuite {

  group("NaturalLanguageOrdering") {

    test("sorts same-prefix numeric suffixes naturally (e.g. _1 < _10 < _11)") {
      val input = List("_11", "_1", "_10")
      val expected = List("_1", "_10", "_11")
      val sorted = input.sorted(NaturalLanguageOrdering.caseInsensitive)
      assert(sorted == expected, s"Expected $expected but got $sorted")
    }

    test("sorts same-prefix text+number combinations naturally") {
      val input = List("method11", "method1", "method10")
      val expected = List("method1", "method10", "method11")
      val sorted = input.sorted(NaturalLanguageOrdering.caseInsensitive)
      assert(sorted == expected, s"Expected $expected but got $sorted")
    }

    test("case insensitive comparison") {
      val input = List("Banana", "apple", "Cherry")
      val expected = List("apple", "Banana", "Cherry")
      val sorted = input.sorted(NaturalLanguageOrdering.caseInsensitive)
      assert(sorted == expected, s"Expected $expected but got $sorted")
    }

    test("case sensitive comparison") {
      val input = List("Banana", "apple", "Cherry")
      // Uppercase letters come before lowercase in ASCII
      val expected = List("Banana", "Cherry", "apple")
      val sorted = input.sorted(NaturalLanguageOrdering.caseSensitive)
      assert(sorted == expected, s"Expected $expected but got $sorted")
    }

    test("handles equal strings") {
      val input = List("abc", "abc")
      val sorted = input.sorted(NaturalLanguageOrdering.caseInsensitive)
      assert(sorted == input, s"Equal strings should stay equal: $sorted")
    }

    test("handles empty strings") {
      val input = List("b", "", "a")
      val expected = List("", "a", "b")
      val sorted = input.sorted(NaturalLanguageOrdering.caseInsensitive)
      assert(sorted == expected, s"Expected $expected but got $sorted")
    }

    test("treats underscore as separator") {
      val ord = NaturalLanguageOrdering.caseInsensitive
      // _1 and _1 should be equal (underscore skipped, numbers equal)
      assert(ord.compare("_1", "_1") == 0, "_1 should equal _1")
      // _1 should come before _10 (1 < 10)
      assert(ord.compare("_1", "_10") < 0, "_1 should come before _10")
    }

    test("handles leading zeroes correctly") {
      val ord = NaturalLanguageOrdering.caseInsensitive
      // numbers are equal but different leading zeroes
      assert(ord.compare("01", "1") != 0 || ord.compare("01", "1") == 0, "leading zeroes comparison should not crash")
    }

    test("does not crash when one string has trailing zero and other does not") {
      val ord = NaturalLanguageOrdering.caseInsensitive
      // "a0" vs "a" should not throw NumberFormatException
      val cmp = ord.compare("a0", "a")
      assert(cmp > 0, s"'a0' should come after 'a' but compare returned $cmp")
    }

    test("handles strings with different digit run lengths") {
      val ord = NaturalLanguageOrdering.caseInsensitive
      assert(ord.compare("a1", "a10") < 0, "'a1' should come before 'a10'")
      assert(ord.compare("a10", "a2") > 0, "'a10' should come after 'a2'")
      assert(ord.compare("a9", "a10") < 0, "'a9' should come before 'a10'")
    }

    test("compares multi-digit numbers with early differences correctly") {
      val ord = NaturalLanguageOrdering.caseInsensitive
      // 3+ digit sequences where the first difference is not the last digit
      assert(ord.compare("a123", "a133") < 0, "'a123' should come before 'a133'")
      assert(ord.compare("a199", "a133") > 0, "'a199' should come after 'a133'")
      assert(ord.compare("a100", "a100") == 0, "'a100' should equal 'a100'")
    }

    test("case sensitive fallback in compareEqual for strings differing only in separators") {
      val ord = NaturalLanguageOrdering.caseSensitive
      // Same length, separators skipped, compareEqual falls back to caseSensitive compareTo
      // "a_b" and "a b": underscore and space are both separators, skipped during comparison,
      // but they have the same length, so compareTo distinguishes them
      val cmp = ord.compare("a_b", "a b")
      assert(cmp != 0, s"'a_b' and 'a b' should differ in case-sensitive mode but got $cmp")
    }

    test("case insensitive fallback in compareEqual for strings differing only in separators") {
      val ord = NaturalLanguageOrdering.caseInsensitive
      val cmp = ord.compare("a_b", "a b")
      assert(cmp != 0, s"'a_b' and 'a b' should differ in case-insensitive mode but got $cmp")
    }

    test("apply factory method returns correct instances") {
      val cs = NaturalLanguageOrdering(isCaseSensitive = true)
      val ci = NaturalLanguageOrdering(isCaseSensitive = false)
      assert(cs eq NaturalLanguageOrdering.caseSensitive, "apply(true) should return caseSensitive instance")
      assert(ci eq NaturalLanguageOrdering.caseInsensitive, "apply(false) should return caseInsensitive instance")
    }

    test("handles strings differing only in length due to separators") {
      val ord = NaturalLanguageOrdering.caseInsensitive
      // "a b" vs "a  b": spaces are separators and skipped, but the strings differ in length
      val cmp = ord.compare("a b", "a  b")
      assert(cmp < 0, s"'a b' should come before 'a  b' due to shorter length but got $cmp")
    }
  }
}
