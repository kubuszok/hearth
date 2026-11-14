package hearth
package typed

import hearth.data.Data
import hearth.examples.exportaliases.*

final class ExportAliasSpec extends MacroSuite {

  group("Scala 3 export aliases") {

    group("for <:< and =:=") {
      import ExportAliasFixtures.testSubtyping

      val sameType = Data.map("A <:< B" -> Data(true), "B <:< A" -> Data(true), "A =:= B" -> Data(true))

      test("a hand-written alias is the same type as the class it stands for") {
        testSubtyping[Mode, Outer.ModeAlias] <==> sameType
      }

      // Known gap: the fast negative path of `<:<` reads the flags of the supertype's `typeSymbol` without dealiasing
      // it, and the symbol of an export alias is final, so any other class is taken to be unrelated to it. Remove
      // `.fail` once fixed.
      test("an export alias is the same type as the class it stands for".fail) {
        testSubtyping[Mode, Outer.Mode] <==> sameType
      }
    }
  }
}
