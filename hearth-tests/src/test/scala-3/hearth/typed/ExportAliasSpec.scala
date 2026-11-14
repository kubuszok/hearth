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

      test("an export alias is the same type as the class it stands for") {
        testSubtyping[Mode, Outer.Mode] <==> sameType
      }
    }

    group("for Method build()") {
      import ExportAliasFixtures.testConstructWith

      test("accepts an argument of the class a hand-written alias parameter type stands for") {
        testConstructWith[Outer.ViaAlias, Mode](Mode.B) <==> Data("ViaAlias(B)")
      }

      test("accepts an argument of the class an export alias parameter type stands for") {
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

      test("of an enum through an export alias conform to the alias") {
        testChildrenConformToParent[Agents.ParameterizedEnumExport] <==> allConform
        testChildrenConformToParent[Agents.ParameterlessEnumExport] <==> allConform
      }

      test("of a sealed trait through a hand-written alias conform to the alias") {
        testChildrenConformToParent[api.SealedTrait] <==> allConform
        testChildrenConformToParent[Agents.SealedTraitAlias] <==> allConform
      }

      test("of a sealed trait through an export alias conform to the alias") {
        testChildrenConformToParent[Agents.SealedTraitExport] <==> allConform
      }
    }
  }
}
