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

        test("rejects type not accessible at the call site") {
          AnonymousInstanceFixtures.testAnonymousInstanceParseInaccessible <==> Data.map(
            "result" -> Data("incompatible"),
            "reason" -> Data(
              "Type hearth.examples.anonymous_instances.PackagePrivateTrait is not accessible at the call site"
            )
          )
        }

        test("rejects more than one class parent") {
          AnonymousInstanceFixtures.testAnonymousInstanceParseWithMixins[
            examples.anonymous_instances.AbstractClassNoArgs,
            examples.anonymous_instances.AbstractClassWithArgs
          ] <==> Data.map(
            "result" -> Data("incompatible"),
            "reason" -> Data(
              "At most one class parent allowed, got: hearth.examples.anonymous_instances.AbstractClassNoArgs, " +
                "hearth.examples.anonymous_instances.AbstractClassWithArgs"
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

        test("trait with vararg abstract method") {
          testAnonymousInstanceParse[examples.anonymous_instances.TraitWithVarargMethod] <==> Data.map(
            "result" -> Data("compatible"),
            "classParent" -> Data("<none>"),
            "traitParents" -> Data("hearth.examples.anonymous_instances.TraitWithVarargMethod"),
            "mustOverride" -> Data("sum"),
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

        test("trait with overloaded abstract methods — each overload is MustOverride") {
          testAnonymousInstanceParse[examples.anonymous_instances.TraitWithOverloads] <==> Data.map(
            "result" -> Data("compatible"),
            "classParent" -> Data("<none>"),
            "traitParents" -> Data("hearth.examples.anonymous_instances.TraitWithOverloads"),
            "mustOverride" -> Data("overloaded, overloaded, overloaded"),
            "mayOverride" -> Data(""),
            "cannotOverride" -> Data(""),
            "diamondConflicts" -> Data("")
          )
        }

        test("trait with generic abstract method — MustOverride") {
          testAnonymousInstanceParse[examples.anonymous_instances.TraitWithGenericMethod] <==> Data.map(
            "result" -> Data("compatible"),
            "classParent" -> Data("<none>"),
            "traitParents" -> Data("hearth.examples.anonymous_instances.TraitWithGenericMethod"),
            "mustOverride" -> Data("identity"),
            "mayOverride" -> Data(""),
            "cannotOverride" -> Data(""),
            "diamondConflicts" -> Data("")
          )
        }

        test("trait with bounded generic abstract method — MustOverride") {
          testAnonymousInstanceParse[examples.anonymous_instances.TraitWithBoundedGenericMethod] <==> Data.map(
            "result" -> Data("compatible"),
            "classParent" -> Data("<none>"),
            "traitParents" -> Data("hearth.examples.anonymous_instances.TraitWithBoundedGenericMethod"),
            "mustOverride" -> Data("firstOf"),
            "mayOverride" -> Data(""),
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

        test("trait with vararg abstract method") {
          testAnonymousInstanceConstruct[examples.anonymous_instances.TraitWithVarargMethod] <==> "success"
        }

        test("trait with overloaded abstract methods (default override bodies)") {
          testAnonymousInstanceConstruct[examples.anonymous_instances.TraitWithOverloads] <==> "success"
        }

        test("each overloaded override is generated distinctly and resolved at runtime") {
          // overloaded(Int): String -> "int->string"; overloaded(String): Int -> 1; overloaded(Int, Int): Int -> 2
          AnonymousInstanceFixtures.testAnonymousInstanceConstructOverloads <==> "int->string|1|2"
        }

        test("generic override body is polymorphic and applied at distinct instantiations") {
          // identity[T](t) returns its argument, exercised at T = String and T = Int
          AnonymousInstanceFixtures.testAnonymousInstanceConstructGeneric <==> "hello|42"
        }

        test("generic method with concrete return sees returnType=String (not Any)") {
          // describe[T](x): String — override returns a String, requires ctx.returnType to be the concrete String
          AnonymousInstanceFixtures.testAnonymousInstanceConstructGenericConcreteReturn <==> "described|described"
        }

        test("generic method with no value params can name its type parameter via ctx.typeParameters") {
          // emptyList[T]: List[T] — override builds List.empty[T] using ctx.typeParameters.head
          AnonymousInstanceFixtures.testAnonymousInstanceConstructGenericFactory <==> "[]|0"
        }

        test("symbolic/operator method name is synthesized and resolved") {
          // +(x: Int): Int — override returns its argument
          AnonymousInstanceFixtures.testAnonymousInstanceConstructSymbolic <==> "41"
        }

        test("method with a trailing implicit parameter clause keeps the implicit modifier") {
          // run(cmd)(implicit cfg): String — override joins both, invoked with implicit cfg = 7
          AnonymousInstanceFixtures.testAnonymousInstanceConstructImplicitParam <==> "go7"
        }

        test("abstract val is overridden with a val member") {
          // val abstractVal: String — override emits a val
          AnonymousInstanceFixtures.testAnonymousInstanceConstructAbstractVal <==> "the-val"
        }

        test("this.type return resolves to the synthesized subtype, gated via OverrideContext.returnsThisType") {
          // chain: this.type returns `this` (returnsThisType=true); sibling: Int returns 7 (returnsThisType=false) —
          // proves the flag distinguishes them and `this` conforms to the override's this.type
          AnonymousInstanceFixtures.testAnonymousInstanceConstructThisType <==> "true|7"
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

        // Regression for commit 7c82fc9 (Scala 2: premature c.typecheck of the generated `new` tree broke
        // anonymous instances in call-site-local scopes).
        test("anonymous instance of a trait defined locally at the call site") {
          trait LocalTrait {
            def value: Int
          }
          testAnonymousInstanceConstruct[LocalTrait] <==> "success"
        }

        // Regression for commit 7c82fc9 (Scala 2: premature c.typecheck of the generated `new` tree broke
        // anonymous instances whose override bodies reference call-site-scoped expressions).
        test("override body can splice an expression captured from the call site") {
          val captured = List("captured", "value").mkString("-") // a local val, not a literal
          AnonymousInstanceFixtures.testAnonymousInstanceCapturedOverride(captured) <==> "captured-value!"
        }

        test("override body can reference a lambda parameter of an enclosing Expr.quote (cats-tagless pattern)") {
          AnonymousInstanceFixtures.testAnonymousInstanceOverrideReferencingQuoteParam.apply("quoted") <==> "quoted!"
        }

        test("override body can use the override's own parameters together with captured expressions") {
          val captured = List("ctx").mkString
          AnonymousInstanceFixtures.testAnonymousInstanceOverrideUsingParams(captured) <==> "ctx! echo"
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

      group("construct: error reporting") {
        import AnonymousInstanceFixtures.{
          testAnonymousInstanceConstructNoOverrides,
          testAnonymousInstanceConstructNoOverridesWithMixins,
          testAnonymousInstanceConstructOverridingFinal
        }

        test("reports missing required override") {
          testAnonymousInstanceConstructNoOverrides[examples.anonymous_instances.SimpleTrait] <==>
            "errors: Missing required override: value (hearth.examples.anonymous_instances.SimpleTrait: def value: scala.Int)"
        }

        test("aggregates multiple missing required overrides") {
          testAnonymousInstanceConstructNoOverridesWithMixins[
            examples.anonymous_instances.MixinA,
            examples.anonymous_instances.MixinB
          ] <==>
            ("errors: Missing required override: fromA (hearth.examples.anonymous_instances.MixinA: def fromA: java.lang.String); " +
              "Missing required override: fromB (hearth.examples.anonymous_instances.MixinA: def fromB: scala.Int)")
        }

        test("reports overriding a final method") {
          testAnonymousInstanceConstructOverridingFinal[examples.anonymous_instances.TraitWithFinalMethod](
            "finalMethod"
          ) <==>
            "errors: Cannot override final method: finalMethod (hearth.examples.anonymous_instances.TraitWithFinalMethod: final def finalMethod: scala.Int)"
        }

        test("reports unresolved diamond conflict") {
          testAnonymousInstanceConstructNoOverridesWithMixins[
            examples.anonymous_instances.DiamondLeft,
            examples.anonymous_instances.DiamondRight
          ] <==>
            "errors: Diamond conflict for method 'method' — conflicting implementations from: hearth.examples.anonymous_instances.DiamondLeft"
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
