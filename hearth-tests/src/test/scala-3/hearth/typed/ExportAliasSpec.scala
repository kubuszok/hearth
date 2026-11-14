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

    group("for Method build()") {
      import ExportAliasFixtures.testConstructWith

      test("accepts an argument of the class a hand-written alias parameter type stands for") {
        testConstructWith[Outer.ViaAlias, Mode](Mode.B) <==> Data("ViaAlias(B)")
      }

      // Known gap: `build()` checks `argument <:< parameter`, which is false when the parameter's type is an export
      // alias (see the `<:<` test above). Remove `.fail` once fixed.
      test("accepts an argument of the class an export alias parameter type stands for".fail) {
        testConstructWith[Outer.ViaExport, Mode](Mode.B) <==> Data("ViaExport(B)")
      }
    }

    group("for directChildren") {
      import ExportAliasFixtures.testChildrenConformToParent

      val allConform = Data.map("A" -> Data(true), "B" -> Data(true))

      test("of an enum through a hand-written alias conform to the alias") {
        testChildrenConformToParent[api.ParameterizedEnum] <==> allConform
        testChildrenConformToParent[Agents.ParameterizedEnumAlias] <==> allConform
        testChildrenConformToParent[api.ParameterlessEnum] <==> allConform
        testChildrenConformToParent[Agents.ParameterlessEnumAlias] <==> allConform
      }

      // Known gap: a case from `directChildren` is a `TermRef` whose type symbol is the enum class, so `<:<` against
      // the export alias takes the fast negative path (see the `<:<` test above). Derivations that keep only the
      // children conforming to the parent, to drop GADT cases, drop them all. Remove `.fail` once fixed.
      test("of an enum through an export alias conform to the alias".fail) {
        testChildrenConformToParent[Agents.ParameterizedEnumExport] <==> allConform
        testChildrenConformToParent[Agents.ParameterlessEnumExport] <==> allConform
      }
    }
  }
}
