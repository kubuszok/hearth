package hearth
package typed

import hearth.data.Data

final class AnonymousInstanceSpec extends MacroSuite {

  group("typed.Classes") {

    group("AnonymousInstance") {

      group("parse: validation rejects invalid types") {
        import AnonymousInstanceFixtures.testAnonymousInstanceParse

        test("rejects final class") {
          testAnonymousInstanceParse[examples.anonymous_instances.FinalClass] <==> Data.map(
            "result" -> Data("incompatible"),
            "reason" -> Data(
              "Cannot create anonymous instance of final type hearth.examples.anonymous_instances.FinalClass"
            )
          )
        }

        test("rejects sealed trait") {
          testAnonymousInstanceParse[examples.anonymous_instances.SealedTrait] <==> Data.map(
            "result" -> Data("incompatible"),
            "reason" -> Data(
              "Cannot create anonymous instance of sealed type hearth.examples.anonymous_instances.SealedTrait"
            )
          )
        }

        test("rejects class with private constructor") {
          testAnonymousInstanceParse[examples.anonymous_instances.PrivateConstructor] <==> Data.map(
            "result" -> Data("incompatible"),
            "reason" -> Data(
              "No accessible constructor for hearth.examples.anonymous_instances.PrivateConstructor: " +
                "new hearth.examples.anonymous_instances.PrivateConstructor(x: scala.Int)"
            )
          )
        }
      }

      group("parse: method classification for single parent") {
        import AnonymousInstanceFixtures.testAnonymousInstanceParse

        test("simple trait — all methods abstract") {
          testAnonymousInstanceParse[examples.anonymous_instances.SimpleTrait] <==> Data.map(
            "result" -> Data("compatible"),
            "classParent" -> Data("<none>"),
            "traitParents" -> Data("hearth.examples.anonymous_instances.SimpleTrait"),
            "mustOverride" -> Data("value"),
            "mayOverride" -> Data(""),
            "cannotOverride" -> Data(""),
            "diamondConflicts" -> Data("")
          )
        }

        test("trait with concrete and abstract methods") {
          testAnonymousInstanceParse[examples.anonymous_instances.TraitWithConcreteMethod] <==> Data.map(
            "result" -> Data("compatible"),
            "classParent" -> Data("<none>"),
            "traitParents" -> Data("hearth.examples.anonymous_instances.TraitWithConcreteMethod"),
            "mustOverride" -> Data("abstractMethod"),
            "mayOverride" -> Data("concreteMethod"),
            "cannotOverride" -> Data(""),
            "diamondConflicts" -> Data("")
          )
        }

        test("trait with final method — classified as CannotOverride") {
          testAnonymousInstanceParse[examples.anonymous_instances.TraitWithFinalMethod] <==> Data.map(
            "result" -> Data("compatible"),
            "classParent" -> Data("<none>"),
            "traitParents" -> Data("hearth.examples.anonymous_instances.TraitWithFinalMethod"),
            "mustOverride" -> Data("abstractMethod"),
            "mayOverride" -> Data(""),
            "cannotOverride" -> Data("finalMethod"),
            "diamondConflicts" -> Data("")
          )
        }

        test("trait with val, var, and def") {
          testAnonymousInstanceParse[examples.anonymous_instances.TraitWithValAndVar] <==> Data.map(
            "result" -> Data("compatible"),
            "classParent" -> Data("<none>"),
            "traitParents" -> Data("hearth.examples.anonymous_instances.TraitWithValAndVar"),
            "mustOverride" -> Data("abstractDef, abstractVal, abstractVar, abstractVar_="),
            "mayOverride" -> Data(""),
            "cannotOverride" -> Data(""),
            "diamondConflicts" -> Data("")
          )
        }

        test("abstract class without args") {
          testAnonymousInstanceParse[examples.anonymous_instances.AbstractClassNoArgs] <==> Data.map(
            "result" -> Data("compatible"),
            "classParent" -> Data("hearth.examples.anonymous_instances.AbstractClassNoArgs"),
            "traitParents" -> Data(""),
            "mustOverride" -> Data("abstractMethod"),
            "mayOverride" -> Data("concreteMethod"),
            "cannotOverride" -> Data(""),
            "diamondConflicts" -> Data("")
          )
        }

        test("abstract class with constructor args") {
          testAnonymousInstanceParse[examples.anonymous_instances.AbstractClassWithArgs] <==> Data.map(
            "result" -> Data("compatible"),
            "classParent" -> Data("hearth.examples.anonymous_instances.AbstractClassWithArgs"),
            "traitParents" -> Data(""),
            "mustOverride" -> Data("abstractMethod"),
            "mayOverride" -> Data("x, y"),
            "cannotOverride" -> Data(""),
            "diamondConflicts" -> Data("")
          )
        }

        test("generic parent with applied type parameters") {
          testAnonymousInstanceParse[examples.anonymous_instances.GenericParent[Int]] <==> Data.map(
            "result" -> Data("compatible"),
            "classParent" -> Data("<none>"),
            "traitParents" -> Data("hearth.examples.anonymous_instances.GenericParent[scala.Int]"),
            "mustOverride" -> Data("transform, value"),
            "mayOverride" -> Data(""),
            "cannotOverride" -> Data(""),
            "diamondConflicts" -> Data("")
          )
        }

        test("abstract class with multiple constructors") {
          testAnonymousInstanceParse[examples.anonymous_instances.AbstractClassMultipleCtors] <==> Data.map(
            "result" -> Data("compatible"),
            "classParent" -> Data("hearth.examples.anonymous_instances.AbstractClassMultipleCtors"),
            "traitParents" -> Data(""),
            "mustOverride" -> Data("abstractMethod"),
            "mayOverride" -> Data("x"),
            "cannotOverride" -> Data(""),
            "diamondConflicts" -> Data("")
          )
        }
      }

      group("parse: multiple parents") {
        import AnonymousInstanceFixtures.testAnonymousInstanceParseWithMixins

        test("class parent with trait mixin") {
          testAnonymousInstanceParseWithMixins[
            examples.anonymous_instances.AbstractClassNoArgs,
            examples.anonymous_instances.MixinA
          ] <==> Data.map(
            "result" -> Data("compatible"),
            "classParent" -> Data("hearth.examples.anonymous_instances.AbstractClassNoArgs"),
            "traitParents" -> Data("hearth.examples.anonymous_instances.MixinA"),
            "mustOverride" -> Data("abstractMethod, fromA"),
            "mayOverride" -> Data("concreteMethod"),
            "cannotOverride" -> Data(""),
            "diamondConflicts" -> Data(""),
            "constructorCount" -> Data(1)
          )
        }

        test("trait parent with trait mixin") {
          testAnonymousInstanceParseWithMixins[
            examples.anonymous_instances.MixinA,
            examples.anonymous_instances.MixinB
          ] <==> Data.map(
            "result" -> Data("compatible"),
            "classParent" -> Data("<none>"),
            "traitParents" -> Data(
              "hearth.examples.anonymous_instances.MixinA, hearth.examples.anonymous_instances.MixinB"
            ),
            "mustOverride" -> Data("fromA, fromB"),
            "mayOverride" -> Data(""),
            "cannotOverride" -> Data(""),
            "diamondConflicts" -> Data(""),
            "constructorCount" -> Data(0)
          )
        }

        test("diamond conflict — two traits implementing same method") {
          testAnonymousInstanceParseWithMixins[
            examples.anonymous_instances.DiamondLeft,
            examples.anonymous_instances.DiamondRight
          ] <==> Data.map(
            "result" -> Data("compatible"),
            "classParent" -> Data("<none>"),
            "traitParents" -> Data(
              "hearth.examples.anonymous_instances.DiamondLeft, hearth.examples.anonymous_instances.DiamondRight"
            ),
            "mustOverride" -> Data(""),
            "mayOverride" -> Data(""),
            "cannotOverride" -> Data(""),
            "diamondConflicts" -> Data("method"),
            "constructorCount" -> Data(0)
          )
        }

        test("abstract class with multiple constructors — reports constructor count") {
          testAnonymousInstanceParseWithMixins[
            examples.anonymous_instances.AbstractClassMultipleCtors,
            examples.anonymous_instances.MixinA
          ] <==> Data.map(
            "result" -> Data("compatible"),
            "classParent" -> Data("hearth.examples.anonymous_instances.AbstractClassMultipleCtors"),
            "traitParents" -> Data("hearth.examples.anonymous_instances.MixinA"),
            "mustOverride" -> Data("abstractMethod, fromA"),
            "mayOverride" -> Data("x"),
            "cannotOverride" -> Data(""),
            "diamondConflicts" -> Data(""),
            "constructorCount" -> Data(2)
          )
        }
      }

      group("construct: building anonymous instances") {
        import AnonymousInstanceFixtures.{testAnonymousInstanceConstruct, testAnonymousInstanceConstructWithMixins}

        test("simple trait") {
          testAnonymousInstanceConstruct[examples.anonymous_instances.SimpleTrait] <==> "success"
        }

        test("trait with concrete method") {
          testAnonymousInstanceConstruct[examples.anonymous_instances.TraitWithConcreteMethod] <==> "success"
        }

        test("abstract class without args") {
          testAnonymousInstanceConstruct[examples.anonymous_instances.AbstractClassNoArgs] <==> "success"
        }

        test("abstract class with constructor args") {
          testAnonymousInstanceConstruct[examples.anonymous_instances.AbstractClassWithArgs] <==> "success"
        }

        test("generic parent with applied type parameters") {
          testAnonymousInstanceConstruct[examples.anonymous_instances.GenericParent[Int]] <==> "success"
        }

        test("abstract class with default constructor args") {
          testAnonymousInstanceConstruct[examples.anonymous_instances.AbstractClassWithDefaults] <==> "success"
        }

        test("class parent with trait mixin") {
          testAnonymousInstanceConstructWithMixins[
            examples.anonymous_instances.AbstractClassNoArgs,
            examples.anonymous_instances.MixinA
          ] <==> "success"
        }

        test("two trait mixins") {
          testAnonymousInstanceConstructWithMixins[
            examples.anonymous_instances.MixinA,
            examples.anonymous_instances.MixinB
          ] <==> "success"
        }

        test("rejects final class") {
          testAnonymousInstanceConstruct[examples.anonymous_instances.FinalClass] <==>
            "incompatible: Cannot create anonymous instance of final type hearth.examples.anonymous_instances.FinalClass"
        }

        test("rejects sealed trait") {
          testAnonymousInstanceConstruct[examples.anonymous_instances.SealedTrait] <==>
            "incompatible: Cannot create anonymous instance of sealed type hearth.examples.anonymous_instances.SealedTrait"
        }
      }

      group("construct: constructor selection") {
        import AnonymousInstanceFixtures.testAnonymousInstanceConstructWithCtorIndex

        test("selects first constructor (primary) by index 0") {
          testAnonymousInstanceConstructWithCtorIndex[
            examples.anonymous_instances.AbstractClassMultipleCtors
          ](0) <==> "success"
        }

        test("selects second constructor by index 1") {
          testAnonymousInstanceConstructWithCtorIndex[
            examples.anonymous_instances.AbstractClassMultipleCtors
          ](1) <==> "success"
        }
      }
    }
  }
}
