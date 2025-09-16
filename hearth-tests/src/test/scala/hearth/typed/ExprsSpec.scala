package hearth
package typed

import hearth.data.Data

/** Macro implementation is in [[ExprsFixturesImpl]] */
final class ExprsSpec extends MacroSuite {

  group("trait typed.Exprs") {

    group("methods: Expr.{plainPrint, prettyPrint, plainAST, prettyAST}, expected behavior") {
      import ExprsFixtures.testExprPrinters

      test("should print expression") {
        testExprPrinters("test value") <==> Data.map(
          "Expr.plainPrint" -> Data("\"test value\""),
          "Expr.prettyPrint" -> Data("\"test value\""),
          "Expr.plainAST" -> Data(
            if (LanguageVersion.byHearth.isScala3) "Inlined(None, Nil, Literal(StringConstant(\"test value\")))"
            else "Literal(Constant(\"test value\"))"
          ),
          "Expr.prettyAST" -> Data(
            if (LanguageVersion.byHearth.isScala3) "Inlined(None, Nil, Literal(StringConstant(\"test value\")))"
            else "Literal(Constant(\"test value\"))"
          )
        )
      }
    }

    group("methods: Expr.summonImplicit, expected behavior") {
      import ExprsFixtures.testExprSummoning

      test("should summon implicit of available") {
        testExprSummoning[List[Int]] <==> Data.map("Expr.summonImplicit" -> Data("<failed to summon>"))
        testExprSummoning[DummyImplicit] <==> Data.map(
          "Expr.summonImplicit" -> Data(
            if (LanguageVersion.byHearth.isScala3) "scala.DummyImplicit.dummyImplicit"
            else "scala.this.DummyImplicit.dummyImplicit"
          )
        )
      }
    }

    group("methods: Expr.upcast, expected behavior") {
      import ExprsFixtures.testExprUpcasting

      test("should upcast expression in compile time if it's possible") {
        testExprUpcasting[Any, String]("string") <==> Data.map("Expr.upcast" -> Data("<failed to upcast>"))
        testExprUpcasting[List[Int], Seq[Any]](List(1, 2, 3)) <==> Data.map(
          "Expr.upcast" -> Data(
            if (LanguageVersion.byHearth.isScala3) "scala.List.apply[scala.Int](1, 2, 3)"
            else "(scala.`package`.List.apply[Int](1, 2, 3): Seq[Any])"
          )
        )
      }
    }

    group("methods: Expr.suppressUnused, expected behavior") {
      import ExprsFixtures.testSuppressUnused

      test("should suppress unused expression") {
        val expr = 1

        testSuppressUnused(expr)
      }
    }

    group("type MatchCase") {

      test("method MatchCase.matchType should allow pattern-matching by the type") {
        import ExprsFixtures.testMatchCaseTypeMatch

        def expand(example: examples.enums.ExampleSealedTrait): Data = testMatchCaseTypeMatch(example)

        expand(examples.enums.ExampleSealedTrait.ExampleSealedTraitClass(1)) <==> Data.map(
          "name" -> Data("ExampleSealedTraitClass"),
          "type" -> Data("hearth.examples.enums.ExampleSealedTrait.ExampleSealedTraitClass"),
          "matchCase" -> Data("examplesealedtraitclass")
        )
        expand(examples.enums.ExampleSealedTrait.ExampleSealedTraitObject) <==> Data.map(
          "name" -> Data("ExampleSealedTraitObject"),
          "type" -> Data("hearth.examples.enums.ExampleSealedTrait.ExampleSealedTraitObject.type"),
          "matchCase" -> Data("examplesealedtraitobject")
        )
      }

      test("method MatchCase.partition should allow branching MatchCase in a macro") {
        import ExprsFixtures.testMatchCasePartition

        // Tests whether the result build using these methods compiles
        @scala.annotation.nowarn // suppress "unreachable code" error
        def run = testMatchCasePartition[Seq[Int], List[Int]](List(1, 2, 3)) ==> List(1, 2, 3)
        run
      }

      test("method MatchCase.traverse should allow traversing MatchCase in a macro") {
        import ExprsFixtures.testMatchCaseTraverse

        // Tests whether the result build using these methods compiles
        @scala.annotation.nowarn // suppress "unreachable code" error
        def run = testMatchCaseTraverse[Seq[Int], List[Int]](List(1, 2, 3)) ==> List(1, 2, 3)
        run
      }
    }

    group("type Scoped") {

      test(
        "methods Scoped.{createVal, createVar, createLazy, createDef, use} should create a scoped block with a definition, and allow using it"
      ) {
        import ExprsFixtures.testScopeCreateAndUse

        // Tests whether the result build using these methods compiles and correct
        testScopeCreateAndUse ==> Data(2 + 20 + 300 + 4000)
      }

      test("methods Scoped.{partition, close} should allow branching and closing a scoped block") {
        import ExprsFixtures.testScopePartitionAndClose

        // Tests whether the result build using these methods compiles and correct
        testScopePartitionAndClose[Seq[Int], List[Int]](List(1, 2, 3)) ==> List(1, 2, 3)
      }

      test("methods Scoped.{traverse, close} should allow traversing and closing a scoped block") {
        import ExprsFixtures.testScopeTraverseAndClose

        // Tests whether the result build using these methods compiles and correct
        testScopeTraverseAndClose[Seq[Int], List[Int]](List(1, 2, 3)) ==> List(1, 2, 3)
      }
    }

    group("type LambdaBuilder") {

      test("methods LambdaBuilder.{of1, of2, ..., of21, of22, build} should allow building a lambda") {
        import ExprsFixtures.testLambdaBuilderOfNAndBuild

        testLambdaBuilderOfNAndBuild ==> Data.map(
          "of1" -> Data(2 + 1),
          "of2" -> Data(2 * 3 + 1),
          "of3" -> Data(2 * 3 * 5 + 1),
          "of4" -> Data(2 * 3 * 5 * 7 + 1),
          "of5" -> Data(2 * 3 * 5 * 7 * 11 + 1),
          "of6" -> Data(2 * 3 * 5 * 7 * 11 * 13 + 1),
          "of7" -> Data(2 * 3 * 5 * 7 * 11 * 13 * 17 + 1),
          "of8" -> Data(2 * 3 * 5 * 7 * 11 * 13 * 17 * 19 + 1),
          "of9" -> Data(2 * 3 * 5 * 7 * 11 * 13 * 17 * 19 * 23 + 1),
          "of10" -> Data(2 * 3 * 5 * 7 * 11 * 13 * 17 * 19 * 23 * 29 + 1),
          "of11" -> Data(2 * 3 * 5 * 7 * 11 * 13 * 17 * 19 * 23 * 29 * 31 + 1),
          "of12" -> Data(2 * 3 * 5 * 7 * 11 * 13 * 17 * 19 * 23 * 29 * 31 * 37 + 1),
          "of13" -> Data(2 * 3 * 5 * 7 * 11 * 13 * 17 * 19 * 23 * 29 * 31 * 37 * 41 + 1),
          "of14" -> Data(2 * 3 * 5 * 7 * 11 * 13 * 17 * 19 * 23 * 29 * 31 * 37 * 41 * 43 + 1),
          "of15" -> Data(2 * 3 * 5 * 7 * 11 * 13 * 17 * 19 * 23 * 29 * 31 * 37 * 41 * 43 * 47 + 1),
          "of16" -> Data(2 * 3 * 5 * 7 * 11 * 13 * 17 * 19 * 23 * 29 * 31 * 37 * 41 * 43 * 47 * 53 + 1),
          "of17" -> Data(2 * 3 * 5 * 7 * 11 * 13 * 17 * 19 * 23 * 29 * 31 * 37 * 41 * 43 * 47 * 53 * 59 + 1),
          "of18" -> Data(2 * 3 * 5 * 7 * 11 * 13 * 17 * 19 * 23 * 29 * 31 * 37 * 41 * 43 * 47 * 53 * 59 * 61 + 1),
          "of19" -> Data(2 * 3 * 5 * 7 * 11 * 13 * 17 * 19 * 23 * 29 * 31 * 37 * 41 * 43 * 47 * 53 * 59 * 61 * 67 + 1),
          "of20" -> Data(
            2 * 3 * 5 * 7 * 11 * 13 * 17 * 19 * 23 * 29 * 31 * 37 * 41 * 43 * 47 * 53 * 59 * 61 * 67 * 71 + 1
          ),
          "of21" -> Data(
            2 * 3 * 5 * 7 * 11 * 13 * 17 * 19 * 23 * 29 * 31 * 37 * 41 * 43 * 47 * 53 * 59 * 61 * 67 * 71 * 73 + 1
          ),
          "of22" -> Data(
            2 * 3 * 5 * 7 * 11 * 13 * 17 * 19 * 23 * 29 * 31 * 37 * 41 * 43 * 47 * 53 * 59 * 61 * 67 * 71 * 73 * 79 + 1
          )
        )
      }

      test("methods LambdaBuilder.buildWith should allow building a lambda") {
        import ExprsFixtures.testLambdaBuilderBuildWith

        testLambdaBuilderBuildWith ==> Data(2 + 1)
      }

      test("methods LambdaBuilder.partition should allow branching LambdaBuilder in a macro") {
        import ExprsFixtures.testLambdaBuilderPartition

        testLambdaBuilderPartition[Int](5) ==> Data(5 + 2)
      }

      test("methods LambdaBuilder.traverse should allow traversing LambdaBuilder in a macro") {
        import ExprsFixtures.testLambdaBuilderTraverse

        testLambdaBuilderTraverse[Int](5) ==> Data(5 + 2)
      }
    }
  }
}
