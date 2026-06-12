package hearth
package typed

import hearth.data.Data

/** Tests for dependent/inner types - types defined inside a class/object.
  *
  * Reproduces https://github.com/kubuszok/hearth/issues/226
  *
  * The core issue is that for inner/dependent enum parameterless cases, `Ident(sym.termRef)` produces
  * `InnerEnum.this.Qux` — a `this` reference valid only inside the defining scope. The fix: use `Ref(sym)` which
  * produces a fully qualified path valid at any call site.
  *
  * Inner types are defined in the companion object to avoid Scala 3's "missing outer accessor" limitation with inline
  * macros expanding inside a class that has inner types.
  */
object DependentTypesSpec {

  // ---- Inner sealed trait with case class + case object ----

  sealed trait InnerSealedTrait
  object InnerSealedTrait {
    case class InnerCC(a: Int) extends InnerSealedTrait
    case object InnerObj extends InnerSealedTrait
  }

  // ---- Inner enum (Scala 3 only) ----

  enum InnerEnum {
    case Bar(a: Int)
    case Baz(b: String)
    case Qux // parameterless case - this is the problematic one
  }

  // ---- Inner enum with type param ----

  enum InnerEnumWithTypeParam[+A] {
    case Wrapped(value: A)
    case Empty // parameterless
  }
}

final class DependentTypesSpec extends MacroSuite {
  import DependentTypesSpec.*

  group("Dependent/inner types - issue #226") {

    group("Enum[A] diagnostics for inner types") {
      import ClassesFixtures.testDependentEnumDiagnostic

      test("inner sealed trait is recognized as enum") {
        testDependentEnumDiagnostic[InnerSealedTrait] <==>
          """child=InnerCC: type=hearth.typed.DependentTypesSpec.InnerSealedTrait.InnerCC, pretty=hearth.typed.DependentTypesSpec.InnerSealedTrait.InnerCC, isVal=false, isObject=false, isCaseClass=true, isCaseObject=false, isCaseVal=false, hasSingleton=false, singletonPrint=<none>
            |child=InnerObj: type=hearth.typed.DependentTypesSpec.InnerSealedTrait.InnerObj.type, pretty=hearth.typed.DependentTypesSpec.InnerSealedTrait.InnerObj.type, isVal=false, isObject=true, isCaseClass=false, isCaseObject=true, isCaseVal=false, hasSingleton=true, singletonPrint=typed.DependentTypesSpec.InnerSealedTrait.InnerObj""".stripMargin
      }

      test("inner enum is recognized as enum") {
        testDependentEnumDiagnostic[InnerEnum] <==>
          """child=Bar: type=hearth.typed.DependentTypesSpec.InnerEnum.Bar, pretty=hearth.typed.DependentTypesSpec.InnerEnum.Bar, isVal=false, isObject=false, isCaseClass=true, isCaseObject=false, isCaseVal=false, hasSingleton=false, singletonPrint=<none>
            |child=Baz: type=hearth.typed.DependentTypesSpec.InnerEnum.Baz, pretty=hearth.typed.DependentTypesSpec.InnerEnum.Baz, isVal=false, isObject=false, isCaseClass=true, isCaseObject=false, isCaseVal=false, hasSingleton=false, singletonPrint=<none>
            |child=Qux: type=hearth.typed.DependentTypesSpec.InnerEnum.Qux.type, pretty=hearth.typed.DependentTypesSpec.InnerEnum.Qux.type, isVal=true, isObject=false, isCaseClass=false, isCaseObject=false, isCaseVal=true, hasSingleton=true, singletonPrint=typed.DependentTypesSpec.InnerEnum.Qux""".stripMargin
      }

      test("inner enum with type param is recognized as enum") {
        testDependentEnumDiagnostic[InnerEnumWithTypeParam[Int]] <==>
          """child=Wrapped: type=hearth.typed.DependentTypesSpec.InnerEnumWithTypeParam.Wrapped[scala.Int], pretty=hearth.typed.DependentTypesSpec.InnerEnumWithTypeParam.Wrapped[scala.Int], isVal=false, isObject=false, isCaseClass=true, isCaseObject=false, isCaseVal=false, hasSingleton=false, singletonPrint=<none>
            |child=Empty: type=hearth.typed.DependentTypesSpec.InnerEnumWithTypeParam.Empty.type, pretty=hearth.typed.DependentTypesSpec.InnerEnumWithTypeParam.Empty.type, isVal=true, isObject=false, isCaseClass=false, isCaseObject=false, isCaseVal=true, hasSingleton=true, singletonPrint=typed.DependentTypesSpec.InnerEnumWithTypeParam.Empty""".stripMargin
      }
    }

    group("Diagnostic: inner enum TypeRepr details") {
      import ClassesFixtures.testDependentEnumTypeReprDiagnostic

      // The report embeds raw compiler output (`TypeRepr.show`, `flags.show`, summoned Mirror trees), which
      // differs between Scala 3 minor versions — this spec is also compiled under the NEWEST_SCALA_TESTS
      // (Scala 3.8.4) matrix — so we pin only the structure: one `child=` block per case, in declaration order.
      test("should report TypeRepr details for inner enum") {
        val report: String = testDependentEnumTypeReprDiagnostic[InnerEnum]
        report.linesIterator.filter(_.startsWith("child=")).mkString("\n") <==> "child=Bar:\nchild=Baz:\nchild=Qux:"
      }

      test("should report TypeRepr details for inner sealed trait") {
        val report: String = testDependentEnumTypeReprDiagnostic[InnerSealedTrait]
        report.linesIterator.filter(_.startsWith("child=")).mkString("\n") <==> "child=InnerCC:\nchild=InnerObj:"
      }
    }

    group("Enum[A].matchOn for inner sealed trait") {
      import ClassesFixtures.testEnumMatchOnAndParMatchOn

      test("should match on inner sealed trait case class") {
        def code(input: InnerSealedTrait) = testEnumMatchOnAndParMatchOn(input)
        code(InnerSealedTrait.InnerCC(1)) <==>
          "sequential: subtype name: hearth.typed.DependentTypesSpec.InnerSealedTrait.InnerCC, expr: InnerCC, parallel: subtype name: hearth.typed.DependentTypesSpec.InnerSealedTrait.InnerCC, expr: InnerCC"
      }

      test("should match on inner sealed trait case object") {
        def code(input: InnerSealedTrait) = testEnumMatchOnAndParMatchOn(input)
        code(InnerSealedTrait.InnerObj) <==>
          "sequential: subtype name: hearth.typed.DependentTypesSpec.InnerSealedTrait.InnerObj.type, expr: InnerObj, parallel: subtype name: hearth.typed.DependentTypesSpec.InnerSealedTrait.InnerObj.type, expr: InnerObj"
      }
    }

    group("Enum[A].matchOn for inner enum (issue #226)") {
      import ClassesFixtures.testEnumMatchOnAndParMatchOn

      test("should match on inner enum case class (Bar)") {
        def code(input: InnerEnum) = testEnumMatchOnAndParMatchOn(input)
        code(InnerEnum.Bar(42)) <==>
          "sequential: subtype name: hearth.typed.DependentTypesSpec.InnerEnum.Bar, expr: Bar, parallel: subtype name: hearth.typed.DependentTypesSpec.InnerEnum.Bar, expr: Bar"
      }

      test("should match on inner enum case class (Baz)") {
        def code(input: InnerEnum) = testEnumMatchOnAndParMatchOn(input)
        code(InnerEnum.Baz("hello")) <==>
          "sequential: subtype name: hearth.typed.DependentTypesSpec.InnerEnum.Baz, expr: Baz, parallel: subtype name: hearth.typed.DependentTypesSpec.InnerEnum.Baz, expr: Baz"
      }

      test("should match on inner enum parameterless case (Qux) without ClassCastException") {
        def code(input: InnerEnum) = testEnumMatchOnAndParMatchOn(input)
        code(InnerEnum.Qux) <==>
          "sequential: subtype name: hearth.typed.DependentTypesSpec.InnerEnum.Qux.type, expr: Qux, parallel: subtype name: hearth.typed.DependentTypesSpec.InnerEnum.Qux.type, expr: Qux"
      }
    }

    group("Enum[A].matchOn for inner enum with type param") {
      import ClassesFixtures.testEnumMatchOnAndParMatchOn

      test("should match on inner enum with type param (Wrapped)") {
        def code(input: InnerEnumWithTypeParam[Int]) = testEnumMatchOnAndParMatchOn(input)
        code(InnerEnumWithTypeParam.Wrapped(42)) <==>
          "sequential: subtype name: hearth.typed.DependentTypesSpec.InnerEnumWithTypeParam.Wrapped[scala.Int], expr: Wrapped, parallel: subtype name: hearth.typed.DependentTypesSpec.InnerEnumWithTypeParam.Wrapped[scala.Int], expr: Wrapped"
      }

      test("should match on inner enum with type param (Empty) without ClassCastException") {
        def code(input: InnerEnumWithTypeParam[Int]) = testEnumMatchOnAndParMatchOn(input)
        code(InnerEnumWithTypeParam.Empty) <==>
          "sequential: subtype name: hearth.typed.DependentTypesSpec.InnerEnumWithTypeParam.Empty.type, expr: Empty, parallel: subtype name: hearth.typed.DependentTypesSpec.InnerEnumWithTypeParam.Empty.type, expr: Empty"
      }
    }

    group("MatchCase.typeMatch for inner sealed trait children") {
      import ExprsFixtures.testMatchCaseTypeMatch

      test("should type-match on inner sealed trait case class") {
        testMatchCaseTypeMatch[InnerSealedTrait](InnerSealedTrait.InnerCC(1)) <==> Data.map(
          "name" -> Data("InnerCC"),
          "type" -> Data("hearth.typed.DependentTypesSpec.InnerSealedTrait.InnerCC"),
          "matchCase" -> Data("innercc")
        )
      }

      test("should type-match on inner sealed trait case object") {
        testMatchCaseTypeMatch[InnerSealedTrait](InnerSealedTrait.InnerObj) <==> Data.map(
          "name" -> Data("InnerObj"),
          "type" -> Data("hearth.typed.DependentTypesSpec.InnerSealedTrait.InnerObj.type"),
          "matchCase" -> Data("innerobj")
        )
      }
    }

    group("MatchCase.typeMatch for inner enum children") {
      import ExprsFixtures.testMatchCaseTypeMatch

      test("should type-match on inner enum case class") {
        testMatchCaseTypeMatch[InnerEnum](InnerEnum.Bar(42)) <==> Data.map(
          "name" -> Data("Bar"),
          "type" -> Data("hearth.typed.DependentTypesSpec.InnerEnum.Bar"),
          "matchCase" -> Data("bar")
        )
      }

      test("should type-match on inner enum parameterless case") {
        testMatchCaseTypeMatch[InnerEnum](InnerEnum.Qux) <==> Data.map(
          "name" -> Data("Qux"),
          "type" -> Data("hearth.typed.DependentTypesSpec.InnerEnum.Qux.type"),
          "matchCase" -> Data("qux")
        )
      }
    }

    group("MatchCase.typeMatch for inner enum with type param") {
      import ExprsFixtures.testMatchCaseTypeMatch

      test("should type-match on inner enum with type param (Wrapped)") {
        testMatchCaseTypeMatch[InnerEnumWithTypeParam[Int]](InnerEnumWithTypeParam.Wrapped(42)) <==> Data.map(
          "name" -> Data("Wrapped"),
          "type" -> Data("hearth.typed.DependentTypesSpec.InnerEnumWithTypeParam.Wrapped[scala.Int]"),
          "matchCase" -> Data("wrapped")
        )
      }

      test("should type-match on inner enum with type param (Empty)") {
        testMatchCaseTypeMatch[InnerEnumWithTypeParam[Int]](InnerEnumWithTypeParam.Empty) <==> Data.map(
          "name" -> Data("Empty"),
          "type" -> Data("hearth.typed.DependentTypesSpec.InnerEnumWithTypeParam.Empty.type"),
          "matchCase" -> Data("empty")
        )
      }
    }

    group("MatchCase.eqValueSingleton for inner parameterless cases") {
      import ExprsFixtures.testMatchCaseEqValueSingleton

      test("should work for inner enum parameterless case (Qux)") {
        @scala.annotation.nowarn
        def run = testMatchCaseEqValueSingleton[InnerEnum.Qux.type] <==> Data.map(
          "singletonOf" -> Data("found"),
          "matched" -> Data("matched")
        )
        run
      }

      test("should work for inner enum with type param (Empty)") {
        @scala.annotation.nowarn
        def run = testMatchCaseEqValueSingleton[InnerEnumWithTypeParam.Empty.type] <==> Data.map(
          "singletonOf" -> Data("found"),
          "matched" -> Data("matched")
        )
        run
      }

      test("should work for inner sealed trait case object (InnerObj)") {
        @scala.annotation.nowarn
        def run = testMatchCaseEqValueSingleton[InnerSealedTrait.InnerObj.type] <==> Data.map(
          "singletonOf" -> Data("found"),
          "matched" -> Data("matched")
        )
        run
      }
    }

    group("singletonOf via directChildren for inner types") {
      import ExprsFixtures.testChildrenSingletonOf

      test("should find singletonOf for inner enum children") {
        testChildrenSingletonOf[InnerEnum] <==> Data.map(
          "Bar" -> Data("none"),
          "Baz" -> Data("none"),
          "Qux" -> Data("found")
        )
      }

      test("should find singletonOf for inner sealed trait children") {
        testChildrenSingletonOf[InnerSealedTrait] <==> Data.map(
          "InnerCC" -> Data("none"),
          "InnerObj" -> Data("found")
        )
      }

      test("should find singletonOf for inner enum with type param children") {
        testChildrenSingletonOf[InnerEnumWithTypeParam[Int]] <==> Data.map(
          "Wrapped" -> Data("none"),
          "Empty" -> Data("found")
        )
      }
    }

    group("MatchCase.eqValue with runtime value for inner types") {
      import ExprsFixtures.testMatchCaseEqValue

      test("should match inner enum parameterless case by value") {
        @scala.annotation.nowarn
        def run = testMatchCaseEqValue[InnerEnum](InnerEnum.Qux) <==> Data.map(
          "matched" -> Data("matched")
        )
        run
      }

      test("should match inner sealed trait case object by value") {
        @scala.annotation.nowarn
        def run = testMatchCaseEqValue[InnerSealedTrait](InnerSealedTrait.InnerObj) <==> Data.map(
          "matched" -> Data("matched")
        )
        run
      }
    }
  }
}
