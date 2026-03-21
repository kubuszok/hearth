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
        val diag = testDependentEnumDiagnostic[InnerSealedTrait]
        diag.contains("InnerCC") ==> true
        diag.contains("InnerObj") ==> true
      }

      test("inner enum is recognized as enum") {
        val diag = testDependentEnumDiagnostic[InnerEnum]
        diag.contains("Bar") ==> true
        diag.contains("Baz") ==> true
        diag.contains("Qux") ==> true
      }

      test("inner enum with type param is recognized as enum") {
        val diag = testDependentEnumDiagnostic[InnerEnumWithTypeParam[Int]]
        diag.contains("Wrapped") ==> true
        diag.contains("Empty") ==> true
      }
    }

    group("Diagnostic: inner enum TypeRepr details") {
      import ClassesFixtures.testDependentEnumTypeReprDiagnostic

      test("should report TypeRepr details for inner enum") {
        val report: String = testDependentEnumTypeReprDiagnostic[InnerEnum]
        report.nonEmpty ==> true
      }

      test("should report TypeRepr details for inner sealed trait") {
        val report: String = testDependentEnumTypeReprDiagnostic[InnerSealedTrait]
        report.nonEmpty ==> true
      }
    }

    group("Enum[A].matchOn for inner sealed trait") {
      import ClassesFixtures.testEnumMatchOnAndParMatchOn

      test("should match on inner sealed trait case class") {
        def code(input: InnerSealedTrait) = testEnumMatchOnAndParMatchOn(input)
        code(InnerSealedTrait.InnerCC(1)).contains("InnerCC") ==> true
      }

      test("should match on inner sealed trait case object") {
        def code(input: InnerSealedTrait) = testEnumMatchOnAndParMatchOn(input)
        code(InnerSealedTrait.InnerObj).contains("InnerObj") ==> true
      }
    }

    group("Enum[A].matchOn for inner enum (issue #226)") {
      import ClassesFixtures.testEnumMatchOnAndParMatchOn

      test("should match on inner enum case class (Bar)") {
        def code(input: InnerEnum) = testEnumMatchOnAndParMatchOn(input)
        code(InnerEnum.Bar(42)).contains("Bar") ==> true
      }

      test("should match on inner enum case class (Baz)") {
        def code(input: InnerEnum) = testEnumMatchOnAndParMatchOn(input)
        code(InnerEnum.Baz("hello")).contains("Baz") ==> true
      }

      test("should match on inner enum parameterless case (Qux) without ClassCastException") {
        def code(input: InnerEnum) = testEnumMatchOnAndParMatchOn(input)
        code(InnerEnum.Qux).contains("Qux") ==> true
      }
    }

    group("Enum[A].matchOn for inner enum with type param") {
      import ClassesFixtures.testEnumMatchOnAndParMatchOn

      test("should match on inner enum with type param (Wrapped)") {
        def code(input: InnerEnumWithTypeParam[Int]) = testEnumMatchOnAndParMatchOn(input)
        code(InnerEnumWithTypeParam.Wrapped(42)).contains("Wrapped") ==> true
      }

      test("should match on inner enum with type param (Empty) without ClassCastException") {
        def code(input: InnerEnumWithTypeParam[Int]) = testEnumMatchOnAndParMatchOn(input)
        code(InnerEnumWithTypeParam.Empty).contains("Empty") ==> true
      }
    }

    group("MatchCase.typeMatch for inner sealed trait children") {
      import ExprsFixtures.testMatchCaseTypeMatch

      test("should type-match on inner sealed trait case class") {
        val result: Data = testMatchCaseTypeMatch[InnerSealedTrait](InnerSealedTrait.InnerCC(1))
        result.toString.contains("InnerCC") ==> true
      }

      test("should type-match on inner sealed trait case object") {
        val result: Data = testMatchCaseTypeMatch[InnerSealedTrait](InnerSealedTrait.InnerObj)
        result.toString.contains("InnerObj") ==> true
      }
    }

    group("MatchCase.typeMatch for inner enum children") {
      import ExprsFixtures.testMatchCaseTypeMatch

      test("should type-match on inner enum case class") {
        val result: Data = testMatchCaseTypeMatch[InnerEnum](InnerEnum.Bar(42))
        result.toString.contains("Bar") ==> true
      }

      test("should type-match on inner enum parameterless case") {
        val result: Data = testMatchCaseTypeMatch[InnerEnum](InnerEnum.Qux)
        result.toString.contains("Qux") ==> true
      }
    }

    group("MatchCase.typeMatch for inner enum with type param") {
      import ExprsFixtures.testMatchCaseTypeMatch

      test("should type-match on inner enum with type param (Wrapped)") {
        val result: Data = testMatchCaseTypeMatch[InnerEnumWithTypeParam[Int]](InnerEnumWithTypeParam.Wrapped(42))
        result.toString.contains("Wrapped") ==> true
      }

      test("should type-match on inner enum with type param (Empty)") {
        val result: Data = testMatchCaseTypeMatch[InnerEnumWithTypeParam[Int]](InnerEnumWithTypeParam.Empty)
        result.toString.contains("Empty") ==> true
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
