package hearth
package treeprinter

final class RemoveMacroSuffixSpec extends MacroSuite {

  group("removeMacroSuffix") {

    test("should strip only the $macro$N suffixes of Hearth's fresh names") {
      val dollar = "$"
      val input =
        s"val a${dollar}macro${dollar}12 = b${dollar}macro${dollar}3 + rassoc${dollar}1 + evidence${dollar}1 + x${dollar}2"
      hearth.removeMacroSuffix(input) <==> s"val a = b + rassoc${dollar}1 + evidence${dollar}1 + x${dollar}2"
    }

    test("should leave a name ending with $macro but without a number intact") {
      val dollar = "$"
      hearth.removeMacroSuffix(s"name${dollar}macro") <==> s"name${dollar}macro"
    }
  }
}
