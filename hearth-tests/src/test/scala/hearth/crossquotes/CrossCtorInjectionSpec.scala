package hearth
package crossquotes

import hearth.data.Data

/** Tests for Type.CtorN injection into cross-quotes.
  *
  * Macro implementation is in [[hearth.cq.CrossQuotesMacros]] (Scala 2) and [[hearth.cq.CrossQuotesPlugin]] (Scala 3).
  *
  * Fixtures are in [[CrossCtorInjectionFixturesImpl]].
  */
final class CrossCtorInjectionSpec extends MacroSuite {

  group("Cross-Quotes CtorN injection") {

    group("for Type.of with imported Type.Ctor1 (Result pattern)") {

      test("should resolve Result[X] when Result is imported from a CtorLikeOf-like container") {
        CrossCtorInjectionFixtures.testTypeOfWithImportedCtor1[Int] <==> Data.map(
          "resultOfInt" -> Data("scala.util.Either[java.lang.String, scala.Int]"),
          "resultOfString" -> Data("scala.util.Either[java.lang.String, java.lang.String]"),
          "resultOfA" -> Data("scala.util.Either[java.lang.String, scala.Int]")
        )
      }

      test("should resolve Result[X] with String type parameter") {
        CrossCtorInjectionFixtures.testTypeOfWithImportedCtor1[String] <==> Data.map(
          "resultOfInt" -> Data("scala.util.Either[java.lang.String, scala.Int]"),
          "resultOfString" -> Data("scala.util.Either[java.lang.String, java.lang.String]"),
          "resultOfA" -> Data("scala.util.Either[java.lang.String, java.lang.String]")
        )
      }
    }

    group("for Type.CtorN.asUntyped") {

      test("should return valid UntypedType for type constructors of all arities") {
        val isScala3 = LanguageVersion.byHearth.isScala3
        CrossCtorInjectionFixtures.testCtorAsUntyped <==> Data.map(
          "optionCtorUntyped" -> Data(
            if (isScala3) "scala.Option[A >: scala.Nothing <: scala.Any] => scala.Option[A]"
            else "scala.Option"
          ),
          "eitherCtorUntyped" -> Data(
            if (isScala3)
              "scala.util.Either[A >: scala.Nothing <: scala.Any, B >: scala.Nothing <: scala.Any] => scala.util.Either[A, B]"
            else "scala.Either"
          ),
          "arity3CtorUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity3[A >: scala.Nothing <: scala.Any, B >: scala.Nothing <: scala.Any, C >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity3[A, B, C]"
            else "hearth.examples.kinds.Arity3"
          ),
          "arity4CtorUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity4[A >: scala.Nothing <: scala.Any, B >: scala.Nothing <: scala.Any, C >: scala.Nothing <: scala.Any, D >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity4[A, B, C, D]"
            else "hearth.examples.kinds.Arity4"
          ),
          "arity5CtorUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity5[A >: scala.Nothing <: scala.Any, B >: scala.Nothing <: scala.Any, C >: scala.Nothing <: scala.Any, D >: scala.Nothing <: scala.Any, E >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity5[A, B, C, D, E]"
            else "hearth.examples.kinds.Arity5"
          ),
          "arity6CtorUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity6[A >: scala.Nothing <: scala.Any, B >: scala.Nothing <: scala.Any, C >: scala.Nothing <: scala.Any, D >: scala.Nothing <: scala.Any, E >: scala.Nothing <: scala.Any, F >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity6[A, B, C, D, E, F]"
            else "hearth.examples.kinds.Arity6"
          ),
          "arity7CtorUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity7[A >: scala.Nothing <: scala.Any, B >: scala.Nothing <: scala.Any, C >: scala.Nothing <: scala.Any, D >: scala.Nothing <: scala.Any, E >: scala.Nothing <: scala.Any, F >: scala.Nothing <: scala.Any, G >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity7[A, B, C, D, E, F, G]"
            else "hearth.examples.kinds.Arity7"
          ),
          "arity8CtorUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity8[A >: scala.Nothing <: scala.Any, B >: scala.Nothing <: scala.Any, C >: scala.Nothing <: scala.Any, D >: scala.Nothing <: scala.Any, E >: scala.Nothing <: scala.Any, F >: scala.Nothing <: scala.Any, G >: scala.Nothing <: scala.Any, H >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity8[A, B, C, D, E, F, G, H]"
            else "hearth.examples.kinds.Arity8"
          ),
          "arity9CtorUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity9[A >: scala.Nothing <: scala.Any, B >: scala.Nothing <: scala.Any, C >: scala.Nothing <: scala.Any, D >: scala.Nothing <: scala.Any, E >: scala.Nothing <: scala.Any, F >: scala.Nothing <: scala.Any, G >: scala.Nothing <: scala.Any, H >: scala.Nothing <: scala.Any, I >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity9[A, B, C, D, E, F, G, H, I]"
            else "hearth.examples.kinds.Arity9"
          ),
          "arity10CtorUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity10[A >: scala.Nothing <: scala.Any, B >: scala.Nothing <: scala.Any, C >: scala.Nothing <: scala.Any, D >: scala.Nothing <: scala.Any, E >: scala.Nothing <: scala.Any, F >: scala.Nothing <: scala.Any, G >: scala.Nothing <: scala.Any, H >: scala.Nothing <: scala.Any, I >: scala.Nothing <: scala.Any, J >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity10[A, B, C, D, E, F, G, H, I, J]"
            else "hearth.examples.kinds.Arity10"
          ),
          "arity11CtorUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity11[A >: scala.Nothing <: scala.Any, B >: scala.Nothing <: scala.Any, C >: scala.Nothing <: scala.Any, D >: scala.Nothing <: scala.Any, E >: scala.Nothing <: scala.Any, F >: scala.Nothing <: scala.Any, G >: scala.Nothing <: scala.Any, H >: scala.Nothing <: scala.Any, I >: scala.Nothing <: scala.Any, J >: scala.Nothing <: scala.Any, K >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity11[A, B, C, D, E, F, G, H, I, J, K]"
            else "hearth.examples.kinds.Arity11"
          ),
          "arity12CtorUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity12[A >: scala.Nothing <: scala.Any, B >: scala.Nothing <: scala.Any, C >: scala.Nothing <: scala.Any, D >: scala.Nothing <: scala.Any, E >: scala.Nothing <: scala.Any, F >: scala.Nothing <: scala.Any, G >: scala.Nothing <: scala.Any, H >: scala.Nothing <: scala.Any, I >: scala.Nothing <: scala.Any, J >: scala.Nothing <: scala.Any, K >: scala.Nothing <: scala.Any, L >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity12[A, B, C, D, E, F, G, H, I, J, K, L]"
            else "hearth.examples.kinds.Arity12"
          ),
          "arity13CtorUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity13[A >: scala.Nothing <: scala.Any, B >: scala.Nothing <: scala.Any, C >: scala.Nothing <: scala.Any, D >: scala.Nothing <: scala.Any, E >: scala.Nothing <: scala.Any, F >: scala.Nothing <: scala.Any, G >: scala.Nothing <: scala.Any, H >: scala.Nothing <: scala.Any, I >: scala.Nothing <: scala.Any, J >: scala.Nothing <: scala.Any, K >: scala.Nothing <: scala.Any, L >: scala.Nothing <: scala.Any, M >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity13[A, B, C, D, E, F, G, H, I, J, K, L, M]"
            else "hearth.examples.kinds.Arity13"
          ),
          "arity14CtorUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity14[A >: scala.Nothing <: scala.Any, B >: scala.Nothing <: scala.Any, C >: scala.Nothing <: scala.Any, D >: scala.Nothing <: scala.Any, E >: scala.Nothing <: scala.Any, F >: scala.Nothing <: scala.Any, G >: scala.Nothing <: scala.Any, H >: scala.Nothing <: scala.Any, I >: scala.Nothing <: scala.Any, J >: scala.Nothing <: scala.Any, K >: scala.Nothing <: scala.Any, L >: scala.Nothing <: scala.Any, M >: scala.Nothing <: scala.Any, N >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity14[A, B, C, D, E, F, G, H, I, J, K, L, M, N]"
            else "hearth.examples.kinds.Arity14"
          ),
          "arity15CtorUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity15[A >: scala.Nothing <: scala.Any, B >: scala.Nothing <: scala.Any, C >: scala.Nothing <: scala.Any, D >: scala.Nothing <: scala.Any, E >: scala.Nothing <: scala.Any, F >: scala.Nothing <: scala.Any, G >: scala.Nothing <: scala.Any, H >: scala.Nothing <: scala.Any, I >: scala.Nothing <: scala.Any, J >: scala.Nothing <: scala.Any, K >: scala.Nothing <: scala.Any, L >: scala.Nothing <: scala.Any, M >: scala.Nothing <: scala.Any, N >: scala.Nothing <: scala.Any, O >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O]"
            else "hearth.examples.kinds.Arity15"
          ),
          "arity16CtorUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity16[A >: scala.Nothing <: scala.Any, B >: scala.Nothing <: scala.Any, C >: scala.Nothing <: scala.Any, D >: scala.Nothing <: scala.Any, E >: scala.Nothing <: scala.Any, F >: scala.Nothing <: scala.Any, G >: scala.Nothing <: scala.Any, H >: scala.Nothing <: scala.Any, I >: scala.Nothing <: scala.Any, J >: scala.Nothing <: scala.Any, K >: scala.Nothing <: scala.Any, L >: scala.Nothing <: scala.Any, M >: scala.Nothing <: scala.Any, N >: scala.Nothing <: scala.Any, O >: scala.Nothing <: scala.Any, P >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P]"
            else "hearth.examples.kinds.Arity16"
          ),
          "arity17CtorUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity17[A >: scala.Nothing <: scala.Any, B >: scala.Nothing <: scala.Any, C >: scala.Nothing <: scala.Any, D >: scala.Nothing <: scala.Any, E >: scala.Nothing <: scala.Any, F >: scala.Nothing <: scala.Any, G >: scala.Nothing <: scala.Any, H >: scala.Nothing <: scala.Any, I >: scala.Nothing <: scala.Any, J >: scala.Nothing <: scala.Any, K >: scala.Nothing <: scala.Any, L >: scala.Nothing <: scala.Any, M >: scala.Nothing <: scala.Any, N >: scala.Nothing <: scala.Any, O >: scala.Nothing <: scala.Any, P >: scala.Nothing <: scala.Any, Q >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q]"
            else "hearth.examples.kinds.Arity17"
          ),
          "arity18CtorUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity18[A >: scala.Nothing <: scala.Any, B >: scala.Nothing <: scala.Any, C >: scala.Nothing <: scala.Any, D >: scala.Nothing <: scala.Any, E >: scala.Nothing <: scala.Any, F >: scala.Nothing <: scala.Any, G >: scala.Nothing <: scala.Any, H >: scala.Nothing <: scala.Any, I >: scala.Nothing <: scala.Any, J >: scala.Nothing <: scala.Any, K >: scala.Nothing <: scala.Any, L >: scala.Nothing <: scala.Any, M >: scala.Nothing <: scala.Any, N >: scala.Nothing <: scala.Any, O >: scala.Nothing <: scala.Any, P >: scala.Nothing <: scala.Any, Q >: scala.Nothing <: scala.Any, R >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R]"
            else "hearth.examples.kinds.Arity18"
          ),
          "arity19CtorUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity19[A >: scala.Nothing <: scala.Any, B >: scala.Nothing <: scala.Any, C >: scala.Nothing <: scala.Any, D >: scala.Nothing <: scala.Any, E >: scala.Nothing <: scala.Any, F >: scala.Nothing <: scala.Any, G >: scala.Nothing <: scala.Any, H >: scala.Nothing <: scala.Any, I >: scala.Nothing <: scala.Any, J >: scala.Nothing <: scala.Any, K >: scala.Nothing <: scala.Any, L >: scala.Nothing <: scala.Any, M >: scala.Nothing <: scala.Any, N >: scala.Nothing <: scala.Any, O >: scala.Nothing <: scala.Any, P >: scala.Nothing <: scala.Any, Q >: scala.Nothing <: scala.Any, R >: scala.Nothing <: scala.Any, S >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S]"
            else "hearth.examples.kinds.Arity19"
          ),
          "arity20CtorUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity20[A >: scala.Nothing <: scala.Any, B >: scala.Nothing <: scala.Any, C >: scala.Nothing <: scala.Any, D >: scala.Nothing <: scala.Any, E >: scala.Nothing <: scala.Any, F >: scala.Nothing <: scala.Any, G >: scala.Nothing <: scala.Any, H >: scala.Nothing <: scala.Any, I >: scala.Nothing <: scala.Any, J >: scala.Nothing <: scala.Any, K >: scala.Nothing <: scala.Any, L >: scala.Nothing <: scala.Any, M >: scala.Nothing <: scala.Any, N >: scala.Nothing <: scala.Any, O >: scala.Nothing <: scala.Any, P >: scala.Nothing <: scala.Any, Q >: scala.Nothing <: scala.Any, R >: scala.Nothing <: scala.Any, S >: scala.Nothing <: scala.Any, T >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T]"
            else "hearth.examples.kinds.Arity20"
          ),
          "arity21CtorUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity21[A >: scala.Nothing <: scala.Any, B >: scala.Nothing <: scala.Any, C >: scala.Nothing <: scala.Any, D >: scala.Nothing <: scala.Any, E >: scala.Nothing <: scala.Any, F >: scala.Nothing <: scala.Any, G >: scala.Nothing <: scala.Any, H >: scala.Nothing <: scala.Any, I >: scala.Nothing <: scala.Any, J >: scala.Nothing <: scala.Any, K >: scala.Nothing <: scala.Any, L >: scala.Nothing <: scala.Any, M >: scala.Nothing <: scala.Any, N >: scala.Nothing <: scala.Any, O >: scala.Nothing <: scala.Any, P >: scala.Nothing <: scala.Any, Q >: scala.Nothing <: scala.Any, R >: scala.Nothing <: scala.Any, S >: scala.Nothing <: scala.Any, T >: scala.Nothing <: scala.Any, U >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U]"
            else "hearth.examples.kinds.Arity21"
          ),
          "arity22CtorUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity22[A >: scala.Nothing <: scala.Any, B >: scala.Nothing <: scala.Any, C >: scala.Nothing <: scala.Any, D >: scala.Nothing <: scala.Any, E >: scala.Nothing <: scala.Any, F >: scala.Nothing <: scala.Any, G >: scala.Nothing <: scala.Any, H >: scala.Nothing <: scala.Any, I >: scala.Nothing <: scala.Any, J >: scala.Nothing <: scala.Any, K >: scala.Nothing <: scala.Any, L >: scala.Nothing <: scala.Any, M >: scala.Nothing <: scala.Any, N >: scala.Nothing <: scala.Any, O >: scala.Nothing <: scala.Any, P >: scala.Nothing <: scala.Any, Q >: scala.Nothing <: scala.Any, R >: scala.Nothing <: scala.Any, S >: scala.Nothing <: scala.Any, T >: scala.Nothing <: scala.Any, U >: scala.Nothing <: scala.Any, V >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V]"
            else "hearth.examples.kinds.Arity22"
          )
        )
      }
    }

    group("for Type.CtorN.setX.asUntyped") {

      test("should return valid UntypedType for partially applied type constructors of all arities") {
        val isScala3 = LanguageVersion.byHearth.isScala3
        CrossCtorInjectionFixtures.testCtorSetAsUntyped <==> Data.map(
          "eitherSetAUntyped" -> Data(
            if (isScala3)
              "scala.util.Either[_$1 >: scala.Nothing <: scala.Any] => scala.util.Either[java.lang.String, _$1]"
            else "({type λ[B] = scala.Either})#λ"
          ),
          "eitherSetBUntyped" -> Data(
            if (isScala3)
              "scala.util.Either[_$1 >: scala.Nothing <: scala.Any] => scala.util.Either[_$1, scala.Int]"
            else "({type λ[A] = scala.Either})#λ"
          ),
          "arity3SetAUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity3[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity3[java.lang.String, _$1, _$2]"
            else "({type λ[B, C] = hearth.examples.kinds.Arity3})#λ"
          ),
          "arity3SetCUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity3[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity3[_$1, _$2, scala.Int]"
            else "({type λ[A, B] = hearth.examples.kinds.Arity3})#λ"
          ),
          "arity4SetAUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity4[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity4[java.lang.String, _$1, _$2, _$3]"
            else "({type λ[B, C, D] = hearth.examples.kinds.Arity4})#λ"
          ),
          "arity4SetDUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity4[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity4[_$1, _$2, _$3, scala.Int]"
            else "({type λ[A, B, C] = hearth.examples.kinds.Arity4})#λ"
          ),
          "arity5SetAUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity5[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity5[java.lang.String, _$1, _$2, _$3, _$4]"
            else "({type λ[B, C, D, E] = hearth.examples.kinds.Arity5})#λ"
          ),
          "arity5SetEUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity5[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity5[_$1, _$2, _$3, _$4, scala.Int]"
            else "({type λ[A, B, C, D] = hearth.examples.kinds.Arity5})#λ"
          ),
          "arity6SetAUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity6[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity6[java.lang.String, _$1, _$2, _$3, _$4, _$5]"
            else "({type λ[B, C, D, E, F] = hearth.examples.kinds.Arity6})#λ"
          ),
          "arity6SetFUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity6[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity6[_$1, _$2, _$3, _$4, _$5, scala.Int]"
            else "({type λ[A, B, C, D, E] = hearth.examples.kinds.Arity6})#λ"
          ),
          "arity7SetAUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity7[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity7[java.lang.String, _$1, _$2, _$3, _$4, _$5, _$6]"
            else "({type λ[B, C, D, E, F, G] = hearth.examples.kinds.Arity7})#λ"
          ),
          "arity7SetGUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity7[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity7[_$1, _$2, _$3, _$4, _$5, _$6, scala.Int]"
            else "({type λ[A, B, C, D, E, F] = hearth.examples.kinds.Arity7})#λ"
          ),
          "arity8SetAUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity8[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity8[java.lang.String, _$1, _$2, _$3, _$4, _$5, _$6, _$7]"
            else "({type λ[B, C, D, E, F, G, H] = hearth.examples.kinds.Arity8})#λ"
          ),
          "arity8SetHUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity8[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity8[_$1, _$2, _$3, _$4, _$5, _$6, _$7, scala.Int]"
            else "({type λ[A, B, C, D, E, F, G] = hearth.examples.kinds.Arity8})#λ"
          ),
          "arity9SetAUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity9[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity9[java.lang.String, _$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8]"
            else "({type λ[B, C, D, E, F, G, H, I] = hearth.examples.kinds.Arity9})#λ"
          ),
          "arity9SetIUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity9[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity9[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, scala.Int]"
            else "({type λ[A, B, C, D, E, F, G, H] = hearth.examples.kinds.Arity9})#λ"
          ),
          "arity10SetAUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity10[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity10[java.lang.String, _$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9]"
            else "({type λ[B, C, D, E, F, G, H, I, J] = hearth.examples.kinds.Arity10})#λ"
          ),
          "arity10SetJUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity10[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity10[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, scala.Int]"
            else "({type λ[A, B, C, D, E, F, G, H, I] = hearth.examples.kinds.Arity10})#λ"
          ),
          "arity11SetAUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity11[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity11[java.lang.String, _$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10]"
            else "({type λ[B, C, D, E, F, G, H, I, J, K] = hearth.examples.kinds.Arity11})#λ"
          ),
          "arity11SetKUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity11[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity11[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, scala.Int]"
            else "({type λ[A, B, C, D, E, F, G, H, I, J] = hearth.examples.kinds.Arity11})#λ"
          ),
          "arity12SetAUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity12[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity12[java.lang.String, _$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11]"
            else "({type λ[B, C, D, E, F, G, H, I, J, K, L] = hearth.examples.kinds.Arity12})#λ"
          ),
          "arity12SetLUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity12[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity12[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, scala.Int]"
            else "({type λ[A, B, C, D, E, F, G, H, I, J, K] = hearth.examples.kinds.Arity12})#λ"
          ),
          "arity13SetAUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity13[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity13[java.lang.String, _$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12]"
            else "({type λ[B, C, D, E, F, G, H, I, J, K, L, M] = hearth.examples.kinds.Arity13})#λ"
          ),
          "arity13SetMUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity13[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity13[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, scala.Int]"
            else "({type λ[A, B, C, D, E, F, G, H, I, J, K, L] = hearth.examples.kinds.Arity13})#λ"
          ),
          "arity14SetAUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity14[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity14[java.lang.String, _$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13]"
            else "({type λ[B, C, D, E, F, G, H, I, J, K, L, M, N] = hearth.examples.kinds.Arity14})#λ"
          ),
          "arity14SetNUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity14[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity14[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, scala.Int]"
            else "({type λ[A, B, C, D, E, F, G, H, I, J, K, L, M] = hearth.examples.kinds.Arity14})#λ"
          ),
          "arity15SetAUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity15[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity15[java.lang.String, _$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14]"
            else "({type λ[B, C, D, E, F, G, H, I, J, K, L, M, N, O] = hearth.examples.kinds.Arity15})#λ"
          ),
          "arity15SetOUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity15[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity15[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, scala.Int]"
            else "({type λ[A, B, C, D, E, F, G, H, I, J, K, L, M, N] = hearth.examples.kinds.Arity15})#λ"
          ),
          "arity16SetAUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity16[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity16[java.lang.String, _$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15]"
            else "({type λ[B, C, D, E, F, G, H, I, J, K, L, M, N, O, P] = hearth.examples.kinds.Arity16})#λ"
          ),
          "arity16SetPUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity16[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity16[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, scala.Int]"
            else "({type λ[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O] = hearth.examples.kinds.Arity16})#λ"
          ),
          "arity17SetAUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity17[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity17[java.lang.String, _$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16]"
            else "({type λ[B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q] = hearth.examples.kinds.Arity17})#λ"
          ),
          "arity17SetQUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity17[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity17[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16, scala.Int]"
            else "({type λ[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P] = hearth.examples.kinds.Arity17})#λ"
          ),
          "arity18SetAUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity18[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity18[java.lang.String, _$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16, _$17]"
            else "({type λ[B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R] = hearth.examples.kinds.Arity18})#λ"
          ),
          "arity18SetRUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity18[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity18[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16, _$17, scala.Int]"
            else "({type λ[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q] = hearth.examples.kinds.Arity18})#λ"
          ),
          "arity19SetAUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity19[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity19[java.lang.String, _$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16, _$17, _$18]"
            else "({type λ[B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S] = hearth.examples.kinds.Arity19})#λ"
          ),
          "arity19SetSUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity19[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity19[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16, _$17, _$18, scala.Int]"
            else "({type λ[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R] = hearth.examples.kinds.Arity19})#λ"
          ),
          "arity20SetAUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity20[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any, _$19 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity20[java.lang.String, _$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16, _$17, _$18, _$19]"
            else "({type λ[B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T] = hearth.examples.kinds.Arity20})#λ"
          ),
          "arity20SetTUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity20[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any, _$19 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity20[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16, _$17, _$18, _$19, scala.Int]"
            else "({type λ[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S] = hearth.examples.kinds.Arity20})#λ"
          ),
          "arity21SetAUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity21[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any, _$19 >: scala.Nothing <: scala.Any, _$20 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity21[java.lang.String, _$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16, _$17, _$18, _$19, _$20]"
            else
              "({type λ[B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U] = hearth.examples.kinds.Arity21})#λ"
          ),
          "arity21SetUUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity21[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any, _$19 >: scala.Nothing <: scala.Any, _$20 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity21[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16, _$17, _$18, _$19, _$20, scala.Int]"
            else
              "({type λ[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T] = hearth.examples.kinds.Arity21})#λ"
          ),
          "arity22SetAUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity22[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any, _$19 >: scala.Nothing <: scala.Any, _$20 >: scala.Nothing <: scala.Any, _$21 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity22[java.lang.String, _$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16, _$17, _$18, _$19, _$20, _$21]"
            else
              "({type λ[B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V] = hearth.examples.kinds.Arity22})#λ"
          ),
          "arity22SetVUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity22[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any, _$19 >: scala.Nothing <: scala.Any, _$20 >: scala.Nothing <: scala.Any, _$21 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity22[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16, _$17, _$18, _$19, _$20, _$21, scala.Int]"
            else
              "({type λ[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U] = hearth.examples.kinds.Arity22})#λ"
          )
        )
      }
    }

    group("for Type.of with higher-kinded Type.CtorN") {

      test("should resolve Type.of[Trait[Result]] when Result comes from Type.Ctor1") {
        val isScala3 = LanguageVersion.byHearth.isScala3
        CrossCtorInjectionFixtures.testTypeOfWithCtor1Higher <==> Data.map(
          "functorOfResult" -> Data(
            if (isScala3)
              "hearth.fp.Functor[[X >: scala.Nothing <: scala.Any] => scala.util.Either[java.lang.String, X]]"
            else "hearth.fp.Functor[container.Result]"
          ),
          "containerOfResult" -> Data(
            if (isScala3)
              "hearth.crossquotes.Container[[X >: scala.Nothing <: scala.Any] => scala.util.Either[java.lang.String, X]]"
            else "hearth.crossquotes.Container[container.Result]"
          )
        )
      }

      test("should resolve Type.of[Container2[Result]] when Result comes from Type.Ctor2") {
        val isScala3 = LanguageVersion.byHearth.isScala3
        CrossCtorInjectionFixtures.testTypeOfWithCtor2Higher <==> Data.map(
          "container2OfResult" -> Data(
            if (isScala3)
              "hearth.crossquotes.Container2[[X >: scala.Nothing <: scala.Any, Y >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity3[java.lang.String, X, Y]]"
            else "hearth.crossquotes.Container2[container.Result]"
          )
        )
      }

      test("should resolve Type.of[Container3[Result]] when Result comes from Type.Ctor3") {
        val isScala3 = LanguageVersion.byHearth.isScala3
        CrossCtorInjectionFixtures.testTypeOfWithCtor3Higher <==> Data.map(
          "container3OfResult" -> Data(
            if (isScala3)
              "hearth.crossquotes.Container3[[B >: scala.Nothing <: scala.Any, C >: scala.Nothing <: scala.Any, D >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity4[java.lang.String, B, C, D]]"
            else "hearth.crossquotes.Container3[container.Result]"
          )
        )
      }

      test("should resolve Type.of[Container4[Result]] when Result comes from Type.Ctor4") {
        val isScala3 = LanguageVersion.byHearth.isScala3
        CrossCtorInjectionFixtures.testTypeOfWithCtor4Higher <==> Data.map(
          "container4OfResult" -> Data(
            if (isScala3)
              "hearth.crossquotes.Container4[[B >: scala.Nothing <: scala.Any, C >: scala.Nothing <: scala.Any, D >: scala.Nothing <: scala.Any, E >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity5[java.lang.String, B, C, D, E]]"
            else "hearth.crossquotes.Container4[container.Result]"
          )
        )
      }

      test("should resolve Type.of[Container5[Result]] when Result comes from Type.Ctor5") {
        val isScala3 = LanguageVersion.byHearth.isScala3
        CrossCtorInjectionFixtures.testTypeOfWithCtor5Higher <==> Data.map(
          "container5OfResult" -> Data(
            if (isScala3)
              "hearth.crossquotes.Container5[[B >: scala.Nothing <: scala.Any, C >: scala.Nothing <: scala.Any, D >: scala.Nothing <: scala.Any, E >: scala.Nothing <: scala.Any, F >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity6[java.lang.String, B, C, D, E, F]]"
            else "hearth.crossquotes.Container5[container.Result]"
          )
        )
      }

      test("should resolve Type.of[Container6[Result]] when Result comes from Type.Ctor6") {
        val isScala3 = LanguageVersion.byHearth.isScala3
        CrossCtorInjectionFixtures.testTypeOfWithCtor6Higher <==> Data.map(
          "container6OfResult" -> Data(
            if (isScala3)
              "hearth.crossquotes.Container6[[B >: scala.Nothing <: scala.Any, C >: scala.Nothing <: scala.Any, D >: scala.Nothing <: scala.Any, E >: scala.Nothing <: scala.Any, F >: scala.Nothing <: scala.Any, G >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity7[java.lang.String, B, C, D, E, F, G]]"
            else "hearth.crossquotes.Container6[container.Result]"
          )
        )
      }

      test("should resolve Type.of[Container7[Result]] when Result comes from Type.Ctor7") {
        val isScala3 = LanguageVersion.byHearth.isScala3
        CrossCtorInjectionFixtures.testTypeOfWithCtor7Higher <==> Data.map(
          "container7OfResult" -> Data(
            if (isScala3)
              "hearth.crossquotes.Container7[[B >: scala.Nothing <: scala.Any, C >: scala.Nothing <: scala.Any, D >: scala.Nothing <: scala.Any, E >: scala.Nothing <: scala.Any, F >: scala.Nothing <: scala.Any, G >: scala.Nothing <: scala.Any, H >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity8[java.lang.String, B, C, D, E, F, G, H]]"
            else "hearth.crossquotes.Container7[container.Result]"
          )
        )
      }

      test("should resolve Type.of[Container8[Result]] when Result comes from Type.Ctor8") {
        val isScala3 = LanguageVersion.byHearth.isScala3
        CrossCtorInjectionFixtures.testTypeOfWithCtor8Higher <==> Data.map(
          "container8OfResult" -> Data(
            if (isScala3)
              "hearth.crossquotes.Container8[[B >: scala.Nothing <: scala.Any, C >: scala.Nothing <: scala.Any, D >: scala.Nothing <: scala.Any, E >: scala.Nothing <: scala.Any, F >: scala.Nothing <: scala.Any, G >: scala.Nothing <: scala.Any, H >: scala.Nothing <: scala.Any, I >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity9[java.lang.String, B, C, D, E, F, G, H, I]]"
            else "hearth.crossquotes.Container8[container.Result]"
          )
        )
      }

      test("should resolve Type.of[Container9[Result]] when Result comes from Type.Ctor9") {
        val isScala3 = LanguageVersion.byHearth.isScala3
        CrossCtorInjectionFixtures.testTypeOfWithCtor9Higher <==> Data.map(
          "container9OfResult" -> Data(
            if (isScala3)
              "hearth.crossquotes.Container9[[B >: scala.Nothing <: scala.Any, C >: scala.Nothing <: scala.Any, D >: scala.Nothing <: scala.Any, E >: scala.Nothing <: scala.Any, F >: scala.Nothing <: scala.Any, G >: scala.Nothing <: scala.Any, H >: scala.Nothing <: scala.Any, I >: scala.Nothing <: scala.Any, J >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity10[java.lang.String, B, C, D, E, F, G, H, I, J]]"
            else "hearth.crossquotes.Container9[container.Result]"
          )
        )
      }

      test("should resolve Type.of[Container10[Result]] when Result comes from Type.Ctor10") {
        val isScala3 = LanguageVersion.byHearth.isScala3
        CrossCtorInjectionFixtures.testTypeOfWithCtor10Higher <==> Data.map(
          "container10OfResult" -> Data(
            if (isScala3)
              "hearth.crossquotes.Container10[[B >: scala.Nothing <: scala.Any, C >: scala.Nothing <: scala.Any, D >: scala.Nothing <: scala.Any, E >: scala.Nothing <: scala.Any, F >: scala.Nothing <: scala.Any, G >: scala.Nothing <: scala.Any, H >: scala.Nothing <: scala.Any, I >: scala.Nothing <: scala.Any, J >: scala.Nothing <: scala.Any, K >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity11[java.lang.String, B, C, D, E, F, G, H, I, J, K]]"
            else "hearth.crossquotes.Container10[container.Result]"
          )
        )
      }

      test("should resolve Type.of[Container11[Result]] when Result comes from Type.Ctor11") {
        val isScala3 = LanguageVersion.byHearth.isScala3
        CrossCtorInjectionFixtures.testTypeOfWithCtor11Higher <==> Data.map(
          "container11OfResult" -> Data(
            if (isScala3)
              "hearth.crossquotes.Container11[[B >: scala.Nothing <: scala.Any, C >: scala.Nothing <: scala.Any, D >: scala.Nothing <: scala.Any, E >: scala.Nothing <: scala.Any, F >: scala.Nothing <: scala.Any, G >: scala.Nothing <: scala.Any, H >: scala.Nothing <: scala.Any, I >: scala.Nothing <: scala.Any, J >: scala.Nothing <: scala.Any, K >: scala.Nothing <: scala.Any, L >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity12[java.lang.String, B, C, D, E, F, G, H, I, J, K, L]]"
            else "hearth.crossquotes.Container11[container.Result]"
          )
        )
      }

      test("should resolve Type.of[Container12[Result]] when Result comes from Type.Ctor12") {
        val isScala3 = LanguageVersion.byHearth.isScala3
        CrossCtorInjectionFixtures.testTypeOfWithCtor12Higher <==> Data.map(
          "container12OfResult" -> Data(
            if (isScala3)
              "hearth.crossquotes.Container12[[B >: scala.Nothing <: scala.Any, C >: scala.Nothing <: scala.Any, D >: scala.Nothing <: scala.Any, E >: scala.Nothing <: scala.Any, F >: scala.Nothing <: scala.Any, G >: scala.Nothing <: scala.Any, H >: scala.Nothing <: scala.Any, I >: scala.Nothing <: scala.Any, J >: scala.Nothing <: scala.Any, K >: scala.Nothing <: scala.Any, L >: scala.Nothing <: scala.Any, M >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity13[java.lang.String, B, C, D, E, F, G, H, I, J, K, L, M]]"
            else "hearth.crossquotes.Container12[container.Result]"
          )
        )
      }

      test("should resolve Type.of[Container13[Result]] when Result comes from Type.Ctor13") {
        val isScala3 = LanguageVersion.byHearth.isScala3
        CrossCtorInjectionFixtures.testTypeOfWithCtor13Higher <==> Data.map(
          "container13OfResult" -> Data(
            if (isScala3)
              "hearth.crossquotes.Container13[[B >: scala.Nothing <: scala.Any, C >: scala.Nothing <: scala.Any, D >: scala.Nothing <: scala.Any, E >: scala.Nothing <: scala.Any, F >: scala.Nothing <: scala.Any, G >: scala.Nothing <: scala.Any, H >: scala.Nothing <: scala.Any, I >: scala.Nothing <: scala.Any, J >: scala.Nothing <: scala.Any, K >: scala.Nothing <: scala.Any, L >: scala.Nothing <: scala.Any, M >: scala.Nothing <: scala.Any, N >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity14[java.lang.String, B, C, D, E, F, G, H, I, J, K, L, M, N]]"
            else "hearth.crossquotes.Container13[container.Result]"
          )
        )
      }

      test("should resolve Type.of[Container14[Result]] when Result comes from Type.Ctor14") {
        val isScala3 = LanguageVersion.byHearth.isScala3
        CrossCtorInjectionFixtures.testTypeOfWithCtor14Higher <==> Data.map(
          "container14OfResult" -> Data(
            if (isScala3)
              "hearth.crossquotes.Container14[[B >: scala.Nothing <: scala.Any, C >: scala.Nothing <: scala.Any, D >: scala.Nothing <: scala.Any, E >: scala.Nothing <: scala.Any, F >: scala.Nothing <: scala.Any, G >: scala.Nothing <: scala.Any, H >: scala.Nothing <: scala.Any, I >: scala.Nothing <: scala.Any, J >: scala.Nothing <: scala.Any, K >: scala.Nothing <: scala.Any, L >: scala.Nothing <: scala.Any, M >: scala.Nothing <: scala.Any, N >: scala.Nothing <: scala.Any, O >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity15[java.lang.String, B, C, D, E, F, G, H, I, J, K, L, M, N, O]]"
            else "hearth.crossquotes.Container14[container.Result]"
          )
        )
      }

      test("should resolve Type.of[Container15[Result]] when Result comes from Type.Ctor15") {
        val isScala3 = LanguageVersion.byHearth.isScala3
        CrossCtorInjectionFixtures.testTypeOfWithCtor15Higher <==> Data.map(
          "container15OfResult" -> Data(
            if (isScala3)
              "hearth.crossquotes.Container15[[B >: scala.Nothing <: scala.Any, C >: scala.Nothing <: scala.Any, D >: scala.Nothing <: scala.Any, E >: scala.Nothing <: scala.Any, F >: scala.Nothing <: scala.Any, G >: scala.Nothing <: scala.Any, H >: scala.Nothing <: scala.Any, I >: scala.Nothing <: scala.Any, J >: scala.Nothing <: scala.Any, K >: scala.Nothing <: scala.Any, L >: scala.Nothing <: scala.Any, M >: scala.Nothing <: scala.Any, N >: scala.Nothing <: scala.Any, O >: scala.Nothing <: scala.Any, P >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity16[java.lang.String, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P]]"
            else "hearth.crossquotes.Container15[container.Result]"
          )
        )
      }

      test("should resolve Type.of[Container16[Result]] when Result comes from Type.Ctor16") {
        val isScala3 = LanguageVersion.byHearth.isScala3
        CrossCtorInjectionFixtures.testTypeOfWithCtor16Higher <==> Data.map(
          "container16OfResult" -> Data(
            if (isScala3)
              "hearth.crossquotes.Container16[[B >: scala.Nothing <: scala.Any, C >: scala.Nothing <: scala.Any, D >: scala.Nothing <: scala.Any, E >: scala.Nothing <: scala.Any, F >: scala.Nothing <: scala.Any, G >: scala.Nothing <: scala.Any, H >: scala.Nothing <: scala.Any, I >: scala.Nothing <: scala.Any, J >: scala.Nothing <: scala.Any, K >: scala.Nothing <: scala.Any, L >: scala.Nothing <: scala.Any, M >: scala.Nothing <: scala.Any, N >: scala.Nothing <: scala.Any, O >: scala.Nothing <: scala.Any, P >: scala.Nothing <: scala.Any, Q >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity17[java.lang.String, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q]]"
            else "hearth.crossquotes.Container16[container.Result]"
          )
        )
      }

      test("should resolve Type.of[Container17[Result]] when Result comes from Type.Ctor17") {
        val isScala3 = LanguageVersion.byHearth.isScala3
        CrossCtorInjectionFixtures.testTypeOfWithCtor17Higher <==> Data.map(
          "container17OfResult" -> Data(
            if (isScala3)
              "hearth.crossquotes.Container17[[B >: scala.Nothing <: scala.Any, C >: scala.Nothing <: scala.Any, D >: scala.Nothing <: scala.Any, E >: scala.Nothing <: scala.Any, F >: scala.Nothing <: scala.Any, G >: scala.Nothing <: scala.Any, H >: scala.Nothing <: scala.Any, I >: scala.Nothing <: scala.Any, J >: scala.Nothing <: scala.Any, K >: scala.Nothing <: scala.Any, L >: scala.Nothing <: scala.Any, M >: scala.Nothing <: scala.Any, N >: scala.Nothing <: scala.Any, O >: scala.Nothing <: scala.Any, P >: scala.Nothing <: scala.Any, Q >: scala.Nothing <: scala.Any, R >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity18[java.lang.String, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R]]"
            else "hearth.crossquotes.Container17[container.Result]"
          )
        )
      }

      test("should resolve Type.of[Container18[Result]] when Result comes from Type.Ctor18") {
        val isScala3 = LanguageVersion.byHearth.isScala3
        CrossCtorInjectionFixtures.testTypeOfWithCtor18Higher <==> Data.map(
          "container18OfResult" -> Data(
            if (isScala3)
              "hearth.crossquotes.Container18[[B >: scala.Nothing <: scala.Any, C >: scala.Nothing <: scala.Any, D >: scala.Nothing <: scala.Any, E >: scala.Nothing <: scala.Any, F >: scala.Nothing <: scala.Any, G >: scala.Nothing <: scala.Any, H >: scala.Nothing <: scala.Any, I >: scala.Nothing <: scala.Any, J >: scala.Nothing <: scala.Any, K >: scala.Nothing <: scala.Any, L >: scala.Nothing <: scala.Any, M >: scala.Nothing <: scala.Any, N >: scala.Nothing <: scala.Any, O >: scala.Nothing <: scala.Any, P >: scala.Nothing <: scala.Any, Q >: scala.Nothing <: scala.Any, R >: scala.Nothing <: scala.Any, S >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity19[java.lang.String, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S]]"
            else "hearth.crossquotes.Container18[container.Result]"
          )
        )
      }

      test("should resolve Type.of[Container19[Result]] when Result comes from Type.Ctor19") {
        val isScala3 = LanguageVersion.byHearth.isScala3
        CrossCtorInjectionFixtures.testTypeOfWithCtor19Higher <==> Data.map(
          "container19OfResult" -> Data(
            if (isScala3)
              "hearth.crossquotes.Container19[[B >: scala.Nothing <: scala.Any, C >: scala.Nothing <: scala.Any, D >: scala.Nothing <: scala.Any, E >: scala.Nothing <: scala.Any, F >: scala.Nothing <: scala.Any, G >: scala.Nothing <: scala.Any, H >: scala.Nothing <: scala.Any, I >: scala.Nothing <: scala.Any, J >: scala.Nothing <: scala.Any, K >: scala.Nothing <: scala.Any, L >: scala.Nothing <: scala.Any, M >: scala.Nothing <: scala.Any, N >: scala.Nothing <: scala.Any, O >: scala.Nothing <: scala.Any, P >: scala.Nothing <: scala.Any, Q >: scala.Nothing <: scala.Any, R >: scala.Nothing <: scala.Any, S >: scala.Nothing <: scala.Any, T >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity20[java.lang.String, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T]]"
            else "hearth.crossquotes.Container19[container.Result]"
          )
        )
      }

      test("should resolve Type.of[Container20[Result]] when Result comes from Type.Ctor20") {
        val isScala3 = LanguageVersion.byHearth.isScala3
        CrossCtorInjectionFixtures.testTypeOfWithCtor20Higher <==> Data.map(
          "container20OfResult" -> Data(
            if (isScala3)
              "hearth.crossquotes.Container20[[B >: scala.Nothing <: scala.Any, C >: scala.Nothing <: scala.Any, D >: scala.Nothing <: scala.Any, E >: scala.Nothing <: scala.Any, F >: scala.Nothing <: scala.Any, G >: scala.Nothing <: scala.Any, H >: scala.Nothing <: scala.Any, I >: scala.Nothing <: scala.Any, J >: scala.Nothing <: scala.Any, K >: scala.Nothing <: scala.Any, L >: scala.Nothing <: scala.Any, M >: scala.Nothing <: scala.Any, N >: scala.Nothing <: scala.Any, O >: scala.Nothing <: scala.Any, P >: scala.Nothing <: scala.Any, Q >: scala.Nothing <: scala.Any, R >: scala.Nothing <: scala.Any, S >: scala.Nothing <: scala.Any, T >: scala.Nothing <: scala.Any, U >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity21[java.lang.String, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U]]"
            else "hearth.crossquotes.Container20[container.Result]"
          )
        )
      }

      test("should resolve Type.of[Container21[Result]] when Result comes from Type.Ctor21") {
        val isScala3 = LanguageVersion.byHearth.isScala3
        CrossCtorInjectionFixtures.testTypeOfWithCtor21Higher <==> Data.map(
          "container21OfResult" -> Data(
            if (isScala3)
              "hearth.crossquotes.Container21[[B >: scala.Nothing <: scala.Any, C >: scala.Nothing <: scala.Any, D >: scala.Nothing <: scala.Any, E >: scala.Nothing <: scala.Any, F >: scala.Nothing <: scala.Any, G >: scala.Nothing <: scala.Any, H >: scala.Nothing <: scala.Any, I >: scala.Nothing <: scala.Any, J >: scala.Nothing <: scala.Any, K >: scala.Nothing <: scala.Any, L >: scala.Nothing <: scala.Any, M >: scala.Nothing <: scala.Any, N >: scala.Nothing <: scala.Any, O >: scala.Nothing <: scala.Any, P >: scala.Nothing <: scala.Any, Q >: scala.Nothing <: scala.Any, R >: scala.Nothing <: scala.Any, S >: scala.Nothing <: scala.Any, T >: scala.Nothing <: scala.Any, U >: scala.Nothing <: scala.Any, V >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity22[java.lang.String, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V]]"
            else "hearth.crossquotes.Container21[container.Result]"
          )
        )
      }

      test("should resolve Type.of[Container22[Result]] when Result comes from Type.Ctor22") {
        val isScala3 = LanguageVersion.byHearth.isScala3
        CrossCtorInjectionFixtures.testTypeOfWithCtor22Higher <==> Data.map(
          "container22OfResult" -> Data(
            if (isScala3)
              "hearth.crossquotes.Container22[[A >: scala.Nothing <: scala.Any, B >: scala.Nothing <: scala.Any, C >: scala.Nothing <: scala.Any, D >: scala.Nothing <: scala.Any, E >: scala.Nothing <: scala.Any, F >: scala.Nothing <: scala.Any, G >: scala.Nothing <: scala.Any, H >: scala.Nothing <: scala.Any, I >: scala.Nothing <: scala.Any, J >: scala.Nothing <: scala.Any, K >: scala.Nothing <: scala.Any, L >: scala.Nothing <: scala.Any, M >: scala.Nothing <: scala.Any, N >: scala.Nothing <: scala.Any, O >: scala.Nothing <: scala.Any, P >: scala.Nothing <: scala.Any, Q >: scala.Nothing <: scala.Any, R >: scala.Nothing <: scala.Any, S >: scala.Nothing <: scala.Any, T >: scala.Nothing <: scala.Any, U >: scala.Nothing <: scala.Any, V >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V]]"
            else "hearth.crossquotes.Container22[container.Result]"
          )
        )
      }

    }

    group("for Expr.quote with Type.CtorN body") {

      test("should create anonymous class extending Container[Option] (Ctor1)") {
        CrossCtorInjectionFixtures.testExprQuoteWithCtor1Body <==> Data("Some(ctor injection works)")
      }

      test("should create anonymous class extending Container2[Either] (Ctor2)") {
        CrossCtorInjectionFixtures.testExprQuoteWithCtor2Body <==> Data("ctor2 works")
      }

      test("should create anonymous class extending Container3[Arity3] (Ctor3)") {
        CrossCtorInjectionFixtures.testExprQuoteWithCtor3Body <==> Data("ctor3 works")
      }

      test("should create anonymous class extending Container4[Arity4] (Ctor4)") {
        CrossCtorInjectionFixtures.testExprQuoteWithCtor4Body <==> Data("ctor4 works")
      }

      test("should create anonymous class extending Container5[Arity5] (Ctor5)") {
        CrossCtorInjectionFixtures.testExprQuoteWithCtor5Body <==> Data("ctor5 works")
      }

      test("should create anonymous class extending Container6[Arity6] (Ctor6)") {
        CrossCtorInjectionFixtures.testExprQuoteWithCtor6Body <==> Data("ctor6 works")
      }

      test("should create anonymous class extending Container7[Arity7] (Ctor7)") {
        CrossCtorInjectionFixtures.testExprQuoteWithCtor7Body <==> Data("ctor7 works")
      }

      test("should create anonymous class extending Container8[Arity8] (Ctor8)") {
        CrossCtorInjectionFixtures.testExprQuoteWithCtor8Body <==> Data("ctor8 works")
      }

      test("should create anonymous class extending Container9[Arity9] (Ctor9)") {
        CrossCtorInjectionFixtures.testExprQuoteWithCtor9Body <==> Data("ctor9 works")
      }

      test("should create anonymous class extending Container10[Arity10] (Ctor10)") {
        CrossCtorInjectionFixtures.testExprQuoteWithCtor10Body <==> Data("ctor10 works")
      }

      test("should create anonymous class extending Container11[Arity11] (Ctor11)") {
        CrossCtorInjectionFixtures.testExprQuoteWithCtor11Body <==> Data("ctor11 works")
      }

      test("should create anonymous class extending Container12[Arity12] (Ctor12)") {
        CrossCtorInjectionFixtures.testExprQuoteWithCtor12Body <==> Data("ctor12 works")
      }

      test("should create anonymous class extending Container13[Arity13] (Ctor13)") {
        CrossCtorInjectionFixtures.testExprQuoteWithCtor13Body <==> Data("ctor13 works")
      }

      test("should create anonymous class extending Container14[Arity14] (Ctor14)") {
        CrossCtorInjectionFixtures.testExprQuoteWithCtor14Body <==> Data("ctor14 works")
      }

      test("should create anonymous class extending Container15[Arity15] (Ctor15)") {
        CrossCtorInjectionFixtures.testExprQuoteWithCtor15Body <==> Data("ctor15 works")
      }

      test("should create anonymous class extending Container16[Arity16] (Ctor16)") {
        CrossCtorInjectionFixtures.testExprQuoteWithCtor16Body <==> Data("ctor16 works")
      }

      test("should create anonymous class extending Container17[Arity17] (Ctor17)") {
        CrossCtorInjectionFixtures.testExprQuoteWithCtor17Body <==> Data("ctor17 works")
      }

      test("should create anonymous class extending Container18[Arity18] (Ctor18)") {
        CrossCtorInjectionFixtures.testExprQuoteWithCtor18Body <==> Data("ctor18 works")
      }

      test("should create anonymous class extending Container19[Arity19] (Ctor19)") {
        CrossCtorInjectionFixtures.testExprQuoteWithCtor19Body <==> Data("ctor19 works")
      }

      test("should create anonymous class extending Container20[Arity20] (Ctor20)") {
        CrossCtorInjectionFixtures.testExprQuoteWithCtor20Body <==> Data("ctor20 works")
      }

      test("should create anonymous class extending Container21[Arity21] (Ctor21)") {
        CrossCtorInjectionFixtures.testExprQuoteWithCtor21Body <==> Data("ctor21 works")
      }

      test("should create anonymous class extending Container22[Arity22] (Ctor22)") {
        CrossCtorInjectionFixtures.testExprQuoteWithCtor22Body <==> Data("ctor22 works")
      }

    }
    group("for Type.CtorN.fromUntyped") {

      test("should roundtrip of -> asUntyped -> fromUntyped for all arities") {
        val isScala3 = LanguageVersion.byHearth.isScala3
        CrossCtorInjectionFixtures.testCtorFromUntyped <==> Data.map(
          "optionApply" -> Data("scala.Option[scala.Int]"),
          "optionUnapplyMatch" -> Data("true"),
          "optionUnapplyNoMatch" -> Data("false"),
          "optionAsUntyped" -> Data(
            if (isScala3) "scala.Option[A >: scala.Nothing <: scala.Any] => scala.Option[A]"
            else "scala.Option"
          ),
          "eitherApply" -> Data("scala.util.Either[java.lang.String, scala.Int]"),
          "eitherUnapplyMatch" -> Data("true"),
          "eitherUnapplyNoMatch" -> Data("false"),
          "eitherAsUntyped" -> Data(
            if (isScala3)
              "scala.util.Either[A >: scala.Nothing <: scala.Any, B >: scala.Nothing <: scala.Any] => scala.util.Either[A, B]"
            else "scala.Either"
          ),
          "arity3Apply" -> Data("hearth.examples.kinds.Arity3[scala.Int, scala.Int, scala.Int]"),
          "arity3UnapplyMatch" -> Data("true"),
          "arity3UnapplyNoMatch" -> Data("false"),
          "arity3AsUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity3[A >: scala.Nothing <: scala.Any, B >: scala.Nothing <: scala.Any, C >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity3[A, B, C]"
            else "hearth.examples.kinds.Arity3"
          ),
          "arity4Apply" -> Data("hearth.examples.kinds.Arity4[scala.Int, scala.Int, scala.Int, scala.Int]"),
          "arity4UnapplyMatch" -> Data("true"),
          "arity4UnapplyNoMatch" -> Data("false"),
          "arity4AsUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity4[A >: scala.Nothing <: scala.Any, B >: scala.Nothing <: scala.Any, C >: scala.Nothing <: scala.Any, D >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity4[A, B, C, D]"
            else "hearth.examples.kinds.Arity4"
          ),
          "arity5Apply" -> Data("hearth.examples.kinds.Arity5[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"),
          "arity5UnapplyMatch" -> Data("true"),
          "arity5UnapplyNoMatch" -> Data("false"),
          "arity5AsUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity5[A >: scala.Nothing <: scala.Any, B >: scala.Nothing <: scala.Any, C >: scala.Nothing <: scala.Any, D >: scala.Nothing <: scala.Any, E >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity5[A, B, C, D, E]"
            else "hearth.examples.kinds.Arity5"
          ),
          "arity6Apply" -> Data(
            "hearth.examples.kinds.Arity6[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "arity6UnapplyMatch" -> Data("true"),
          "arity6UnapplyNoMatch" -> Data("false"),
          "arity6AsUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity6[A >: scala.Nothing <: scala.Any, B >: scala.Nothing <: scala.Any, C >: scala.Nothing <: scala.Any, D >: scala.Nothing <: scala.Any, E >: scala.Nothing <: scala.Any, F >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity6[A, B, C, D, E, F]"
            else "hearth.examples.kinds.Arity6"
          ),
          "arity7Apply" -> Data(
            "hearth.examples.kinds.Arity7[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "arity7UnapplyMatch" -> Data("true"),
          "arity7UnapplyNoMatch" -> Data("false"),
          "arity7AsUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity7[A >: scala.Nothing <: scala.Any, B >: scala.Nothing <: scala.Any, C >: scala.Nothing <: scala.Any, D >: scala.Nothing <: scala.Any, E >: scala.Nothing <: scala.Any, F >: scala.Nothing <: scala.Any, G >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity7[A, B, C, D, E, F, G]"
            else "hearth.examples.kinds.Arity7"
          ),
          "arity8Apply" -> Data(
            "hearth.examples.kinds.Arity8[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "arity8UnapplyMatch" -> Data("true"),
          "arity8UnapplyNoMatch" -> Data("false"),
          "arity8AsUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity8[A >: scala.Nothing <: scala.Any, B >: scala.Nothing <: scala.Any, C >: scala.Nothing <: scala.Any, D >: scala.Nothing <: scala.Any, E >: scala.Nothing <: scala.Any, F >: scala.Nothing <: scala.Any, G >: scala.Nothing <: scala.Any, H >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity8[A, B, C, D, E, F, G, H]"
            else "hearth.examples.kinds.Arity8"
          ),
          "arity9Apply" -> Data(
            "hearth.examples.kinds.Arity9[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "arity9UnapplyMatch" -> Data("true"),
          "arity9UnapplyNoMatch" -> Data("false"),
          "arity9AsUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity9[A >: scala.Nothing <: scala.Any, B >: scala.Nothing <: scala.Any, C >: scala.Nothing <: scala.Any, D >: scala.Nothing <: scala.Any, E >: scala.Nothing <: scala.Any, F >: scala.Nothing <: scala.Any, G >: scala.Nothing <: scala.Any, H >: scala.Nothing <: scala.Any, I >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity9[A, B, C, D, E, F, G, H, I]"
            else "hearth.examples.kinds.Arity9"
          ),
          "arity10Apply" -> Data(
            "hearth.examples.kinds.Arity10[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "arity10UnapplyMatch" -> Data("true"),
          "arity10UnapplyNoMatch" -> Data("false"),
          "arity10AsUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity10[A >: scala.Nothing <: scala.Any, B >: scala.Nothing <: scala.Any, C >: scala.Nothing <: scala.Any, D >: scala.Nothing <: scala.Any, E >: scala.Nothing <: scala.Any, F >: scala.Nothing <: scala.Any, G >: scala.Nothing <: scala.Any, H >: scala.Nothing <: scala.Any, I >: scala.Nothing <: scala.Any, J >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity10[A, B, C, D, E, F, G, H, I, J]"
            else "hearth.examples.kinds.Arity10"
          ),
          "arity11Apply" -> Data(
            "hearth.examples.kinds.Arity11[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "arity11UnapplyMatch" -> Data("true"),
          "arity11UnapplyNoMatch" -> Data("false"),
          "arity11AsUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity11[A >: scala.Nothing <: scala.Any, B >: scala.Nothing <: scala.Any, C >: scala.Nothing <: scala.Any, D >: scala.Nothing <: scala.Any, E >: scala.Nothing <: scala.Any, F >: scala.Nothing <: scala.Any, G >: scala.Nothing <: scala.Any, H >: scala.Nothing <: scala.Any, I >: scala.Nothing <: scala.Any, J >: scala.Nothing <: scala.Any, K >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity11[A, B, C, D, E, F, G, H, I, J, K]"
            else "hearth.examples.kinds.Arity11"
          ),
          "arity12Apply" -> Data(
            "hearth.examples.kinds.Arity12[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "arity12UnapplyMatch" -> Data("true"),
          "arity12UnapplyNoMatch" -> Data("false"),
          "arity12AsUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity12[A >: scala.Nothing <: scala.Any, B >: scala.Nothing <: scala.Any, C >: scala.Nothing <: scala.Any, D >: scala.Nothing <: scala.Any, E >: scala.Nothing <: scala.Any, F >: scala.Nothing <: scala.Any, G >: scala.Nothing <: scala.Any, H >: scala.Nothing <: scala.Any, I >: scala.Nothing <: scala.Any, J >: scala.Nothing <: scala.Any, K >: scala.Nothing <: scala.Any, L >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity12[A, B, C, D, E, F, G, H, I, J, K, L]"
            else "hearth.examples.kinds.Arity12"
          ),
          "arity13Apply" -> Data(
            "hearth.examples.kinds.Arity13[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "arity13UnapplyMatch" -> Data("true"),
          "arity13UnapplyNoMatch" -> Data("false"),
          "arity13AsUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity13[A >: scala.Nothing <: scala.Any, B >: scala.Nothing <: scala.Any, C >: scala.Nothing <: scala.Any, D >: scala.Nothing <: scala.Any, E >: scala.Nothing <: scala.Any, F >: scala.Nothing <: scala.Any, G >: scala.Nothing <: scala.Any, H >: scala.Nothing <: scala.Any, I >: scala.Nothing <: scala.Any, J >: scala.Nothing <: scala.Any, K >: scala.Nothing <: scala.Any, L >: scala.Nothing <: scala.Any, M >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity13[A, B, C, D, E, F, G, H, I, J, K, L, M]"
            else "hearth.examples.kinds.Arity13"
          ),
          "arity14Apply" -> Data(
            "hearth.examples.kinds.Arity14[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "arity14UnapplyMatch" -> Data("true"),
          "arity14UnapplyNoMatch" -> Data("false"),
          "arity14AsUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity14[A >: scala.Nothing <: scala.Any, B >: scala.Nothing <: scala.Any, C >: scala.Nothing <: scala.Any, D >: scala.Nothing <: scala.Any, E >: scala.Nothing <: scala.Any, F >: scala.Nothing <: scala.Any, G >: scala.Nothing <: scala.Any, H >: scala.Nothing <: scala.Any, I >: scala.Nothing <: scala.Any, J >: scala.Nothing <: scala.Any, K >: scala.Nothing <: scala.Any, L >: scala.Nothing <: scala.Any, M >: scala.Nothing <: scala.Any, N >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity14[A, B, C, D, E, F, G, H, I, J, K, L, M, N]"
            else "hearth.examples.kinds.Arity14"
          ),
          "arity15Apply" -> Data(
            "hearth.examples.kinds.Arity15[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "arity15UnapplyMatch" -> Data("true"),
          "arity15UnapplyNoMatch" -> Data("false"),
          "arity15AsUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity15[A >: scala.Nothing <: scala.Any, B >: scala.Nothing <: scala.Any, C >: scala.Nothing <: scala.Any, D >: scala.Nothing <: scala.Any, E >: scala.Nothing <: scala.Any, F >: scala.Nothing <: scala.Any, G >: scala.Nothing <: scala.Any, H >: scala.Nothing <: scala.Any, I >: scala.Nothing <: scala.Any, J >: scala.Nothing <: scala.Any, K >: scala.Nothing <: scala.Any, L >: scala.Nothing <: scala.Any, M >: scala.Nothing <: scala.Any, N >: scala.Nothing <: scala.Any, O >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O]"
            else "hearth.examples.kinds.Arity15"
          ),
          "arity16Apply" -> Data(
            "hearth.examples.kinds.Arity16[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "arity16UnapplyMatch" -> Data("true"),
          "arity16UnapplyNoMatch" -> Data("false"),
          "arity16AsUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity16[A >: scala.Nothing <: scala.Any, B >: scala.Nothing <: scala.Any, C >: scala.Nothing <: scala.Any, D >: scala.Nothing <: scala.Any, E >: scala.Nothing <: scala.Any, F >: scala.Nothing <: scala.Any, G >: scala.Nothing <: scala.Any, H >: scala.Nothing <: scala.Any, I >: scala.Nothing <: scala.Any, J >: scala.Nothing <: scala.Any, K >: scala.Nothing <: scala.Any, L >: scala.Nothing <: scala.Any, M >: scala.Nothing <: scala.Any, N >: scala.Nothing <: scala.Any, O >: scala.Nothing <: scala.Any, P >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P]"
            else "hearth.examples.kinds.Arity16"
          ),
          "arity17Apply" -> Data(
            "hearth.examples.kinds.Arity17[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "arity17UnapplyMatch" -> Data("true"),
          "arity17UnapplyNoMatch" -> Data("false"),
          "arity17AsUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity17[A >: scala.Nothing <: scala.Any, B >: scala.Nothing <: scala.Any, C >: scala.Nothing <: scala.Any, D >: scala.Nothing <: scala.Any, E >: scala.Nothing <: scala.Any, F >: scala.Nothing <: scala.Any, G >: scala.Nothing <: scala.Any, H >: scala.Nothing <: scala.Any, I >: scala.Nothing <: scala.Any, J >: scala.Nothing <: scala.Any, K >: scala.Nothing <: scala.Any, L >: scala.Nothing <: scala.Any, M >: scala.Nothing <: scala.Any, N >: scala.Nothing <: scala.Any, O >: scala.Nothing <: scala.Any, P >: scala.Nothing <: scala.Any, Q >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q]"
            else "hearth.examples.kinds.Arity17"
          ),
          "arity18Apply" -> Data(
            "hearth.examples.kinds.Arity18[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "arity18UnapplyMatch" -> Data("true"),
          "arity18UnapplyNoMatch" -> Data("false"),
          "arity18AsUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity18[A >: scala.Nothing <: scala.Any, B >: scala.Nothing <: scala.Any, C >: scala.Nothing <: scala.Any, D >: scala.Nothing <: scala.Any, E >: scala.Nothing <: scala.Any, F >: scala.Nothing <: scala.Any, G >: scala.Nothing <: scala.Any, H >: scala.Nothing <: scala.Any, I >: scala.Nothing <: scala.Any, J >: scala.Nothing <: scala.Any, K >: scala.Nothing <: scala.Any, L >: scala.Nothing <: scala.Any, M >: scala.Nothing <: scala.Any, N >: scala.Nothing <: scala.Any, O >: scala.Nothing <: scala.Any, P >: scala.Nothing <: scala.Any, Q >: scala.Nothing <: scala.Any, R >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R]"
            else "hearth.examples.kinds.Arity18"
          ),
          "arity19Apply" -> Data(
            "hearth.examples.kinds.Arity19[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "arity19UnapplyMatch" -> Data("true"),
          "arity19UnapplyNoMatch" -> Data("false"),
          "arity19AsUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity19[A >: scala.Nothing <: scala.Any, B >: scala.Nothing <: scala.Any, C >: scala.Nothing <: scala.Any, D >: scala.Nothing <: scala.Any, E >: scala.Nothing <: scala.Any, F >: scala.Nothing <: scala.Any, G >: scala.Nothing <: scala.Any, H >: scala.Nothing <: scala.Any, I >: scala.Nothing <: scala.Any, J >: scala.Nothing <: scala.Any, K >: scala.Nothing <: scala.Any, L >: scala.Nothing <: scala.Any, M >: scala.Nothing <: scala.Any, N >: scala.Nothing <: scala.Any, O >: scala.Nothing <: scala.Any, P >: scala.Nothing <: scala.Any, Q >: scala.Nothing <: scala.Any, R >: scala.Nothing <: scala.Any, S >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S]"
            else "hearth.examples.kinds.Arity19"
          ),
          "arity20Apply" -> Data(
            "hearth.examples.kinds.Arity20[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "arity20UnapplyMatch" -> Data("true"),
          "arity20UnapplyNoMatch" -> Data("false"),
          "arity20AsUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity20[A >: scala.Nothing <: scala.Any, B >: scala.Nothing <: scala.Any, C >: scala.Nothing <: scala.Any, D >: scala.Nothing <: scala.Any, E >: scala.Nothing <: scala.Any, F >: scala.Nothing <: scala.Any, G >: scala.Nothing <: scala.Any, H >: scala.Nothing <: scala.Any, I >: scala.Nothing <: scala.Any, J >: scala.Nothing <: scala.Any, K >: scala.Nothing <: scala.Any, L >: scala.Nothing <: scala.Any, M >: scala.Nothing <: scala.Any, N >: scala.Nothing <: scala.Any, O >: scala.Nothing <: scala.Any, P >: scala.Nothing <: scala.Any, Q >: scala.Nothing <: scala.Any, R >: scala.Nothing <: scala.Any, S >: scala.Nothing <: scala.Any, T >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T]"
            else "hearth.examples.kinds.Arity20"
          ),
          "arity21Apply" -> Data(
            "hearth.examples.kinds.Arity21[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "arity21UnapplyMatch" -> Data("true"),
          "arity21UnapplyNoMatch" -> Data("false"),
          "arity21AsUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity21[A >: scala.Nothing <: scala.Any, B >: scala.Nothing <: scala.Any, C >: scala.Nothing <: scala.Any, D >: scala.Nothing <: scala.Any, E >: scala.Nothing <: scala.Any, F >: scala.Nothing <: scala.Any, G >: scala.Nothing <: scala.Any, H >: scala.Nothing <: scala.Any, I >: scala.Nothing <: scala.Any, J >: scala.Nothing <: scala.Any, K >: scala.Nothing <: scala.Any, L >: scala.Nothing <: scala.Any, M >: scala.Nothing <: scala.Any, N >: scala.Nothing <: scala.Any, O >: scala.Nothing <: scala.Any, P >: scala.Nothing <: scala.Any, Q >: scala.Nothing <: scala.Any, R >: scala.Nothing <: scala.Any, S >: scala.Nothing <: scala.Any, T >: scala.Nothing <: scala.Any, U >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U]"
            else "hearth.examples.kinds.Arity21"
          ),
          "arity22Apply" -> Data(
            "hearth.examples.kinds.Arity22[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "arity22UnapplyMatch" -> Data("true"),
          "arity22UnapplyNoMatch" -> Data("false"),
          "arity22AsUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity22[A >: scala.Nothing <: scala.Any, B >: scala.Nothing <: scala.Any, C >: scala.Nothing <: scala.Any, D >: scala.Nothing <: scala.Any, E >: scala.Nothing <: scala.Any, F >: scala.Nothing <: scala.Any, G >: scala.Nothing <: scala.Any, H >: scala.Nothing <: scala.Any, I >: scala.Nothing <: scala.Any, J >: scala.Nothing <: scala.Any, K >: scala.Nothing <: scala.Any, L >: scala.Nothing <: scala.Any, M >: scala.Nothing <: scala.Any, N >: scala.Nothing <: scala.Any, O >: scala.Nothing <: scala.Any, P >: scala.Nothing <: scala.Any, Q >: scala.Nothing <: scala.Any, R >: scala.Nothing <: scala.Any, S >: scala.Nothing <: scala.Any, T >: scala.Nothing <: scala.Any, U >: scala.Nothing <: scala.Any, V >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V]"
            else "hearth.examples.kinds.Arity22"
          )
        )
      }

    }

    group("for Type.CtorN.fromUntyped extraction") {

      test("should work for Ctor1 extracted from Option[Int]") {
        CrossCtorInjectionFixtures.testCtorExtract1 <==> Data.map(
          "apply" -> Data("scala.Option[scala.Int]"),
          "unapplyMatch" -> Data("true"),
          "unapplyNoMatch" -> Data("false")
        )
      }

      test("should work for Ctor2 extracted from Either[String, Int]") {
        CrossCtorInjectionFixtures.testCtorExtract2 <==> Data.map(
          "apply" -> Data("scala.util.Either[java.lang.String, scala.Int]"),
          "unapplyMatch" -> Data("true"),
          "unapplyNoMatch" -> Data("false")
        )
      }

      test("should work for Ctor3 extracted from Arity3[Int, String, Boolean]") {
        CrossCtorInjectionFixtures.testCtorExtract3 <==> Data.map(
          "apply" -> Data("hearth.examples.kinds.Arity3[scala.Int, java.lang.String, scala.Boolean]"),
          "unapplyMatch" -> Data("true"),
          "unapplyNoMatch" -> Data("false")
        )
      }

      test("should work for Ctor4 extracted from Arity4[Int, ...]") {
        CrossCtorInjectionFixtures.testCtorExtract4 <==> Data.map(
          "apply" -> Data(
            "hearth.examples.kinds.Arity4[scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "unapplyMatch" -> Data("true"),
          "unapplyNoMatch" -> Data("false")
        )
      }

      test("should work for Ctor5 extracted from Arity5[Int, ...]") {
        CrossCtorInjectionFixtures.testCtorExtract5 <==> Data.map(
          "apply" -> Data(
            "hearth.examples.kinds.Arity5[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "unapplyMatch" -> Data("true"),
          "unapplyNoMatch" -> Data("false")
        )
      }

      test("should work for Ctor6 extracted from Arity6[Int, ...]") {
        CrossCtorInjectionFixtures.testCtorExtract6 <==> Data.map(
          "apply" -> Data(
            "hearth.examples.kinds.Arity6[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "unapplyMatch" -> Data("true"),
          "unapplyNoMatch" -> Data("false")
        )
      }

      test("should work for Ctor7 extracted from Arity7[Int, ...]") {
        CrossCtorInjectionFixtures.testCtorExtract7 <==> Data.map(
          "apply" -> Data(
            "hearth.examples.kinds.Arity7[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "unapplyMatch" -> Data("true"),
          "unapplyNoMatch" -> Data("false")
        )
      }

      test("should work for Ctor8 extracted from Arity8[Int, ...]") {
        CrossCtorInjectionFixtures.testCtorExtract8 <==> Data.map(
          "apply" -> Data(
            "hearth.examples.kinds.Arity8[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "unapplyMatch" -> Data("true"),
          "unapplyNoMatch" -> Data("false")
        )
      }

      test("should work for Ctor9 extracted from Arity9[Int, ...]") {
        CrossCtorInjectionFixtures.testCtorExtract9 <==> Data.map(
          "apply" -> Data(
            "hearth.examples.kinds.Arity9[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "unapplyMatch" -> Data("true"),
          "unapplyNoMatch" -> Data("false")
        )
      }

      test("should work for Ctor10 extracted from Arity10[Int, ...]") {
        CrossCtorInjectionFixtures.testCtorExtract10 <==> Data.map(
          "apply" -> Data(
            "hearth.examples.kinds.Arity10[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "unapplyMatch" -> Data("true"),
          "unapplyNoMatch" -> Data("false")
        )
      }

      test("should work for Ctor11 extracted from Arity11[Int, ...]") {
        CrossCtorInjectionFixtures.testCtorExtract11 <==> Data.map(
          "apply" -> Data(
            "hearth.examples.kinds.Arity11[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "unapplyMatch" -> Data("true"),
          "unapplyNoMatch" -> Data("false")
        )
      }

      test("should work for Ctor12 extracted from Arity12[Int, ...]") {
        CrossCtorInjectionFixtures.testCtorExtract12 <==> Data.map(
          "apply" -> Data(
            "hearth.examples.kinds.Arity12[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "unapplyMatch" -> Data("true"),
          "unapplyNoMatch" -> Data("false")
        )
      }

      test("should work for Ctor13 extracted from Arity13[Int, ...]") {
        CrossCtorInjectionFixtures.testCtorExtract13 <==> Data.map(
          "apply" -> Data(
            "hearth.examples.kinds.Arity13[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "unapplyMatch" -> Data("true"),
          "unapplyNoMatch" -> Data("false")
        )
      }

      test("should work for Ctor14 extracted from Arity14[Int, ...]") {
        CrossCtorInjectionFixtures.testCtorExtract14 <==> Data.map(
          "apply" -> Data(
            "hearth.examples.kinds.Arity14[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "unapplyMatch" -> Data("true"),
          "unapplyNoMatch" -> Data("false")
        )
      }

      test("should work for Ctor15 extracted from Arity15[Int, ...]") {
        CrossCtorInjectionFixtures.testCtorExtract15 <==> Data.map(
          "apply" -> Data(
            "hearth.examples.kinds.Arity15[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "unapplyMatch" -> Data("true"),
          "unapplyNoMatch" -> Data("false")
        )
      }

      test("should work for Ctor16 extracted from Arity16[Int, ...]") {
        CrossCtorInjectionFixtures.testCtorExtract16 <==> Data.map(
          "apply" -> Data(
            "hearth.examples.kinds.Arity16[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "unapplyMatch" -> Data("true"),
          "unapplyNoMatch" -> Data("false")
        )
      }

      test("should work for Ctor17 extracted from Arity17[Int, ...]") {
        CrossCtorInjectionFixtures.testCtorExtract17 <==> Data.map(
          "apply" -> Data(
            "hearth.examples.kinds.Arity17[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "unapplyMatch" -> Data("true"),
          "unapplyNoMatch" -> Data("false")
        )
      }

      test("should work for Ctor18 extracted from Arity18[Int, ...]") {
        CrossCtorInjectionFixtures.testCtorExtract18 <==> Data.map(
          "apply" -> Data(
            "hearth.examples.kinds.Arity18[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "unapplyMatch" -> Data("true"),
          "unapplyNoMatch" -> Data("false")
        )
      }

      test("should work for Ctor19 extracted from Arity19[Int, ...]") {
        CrossCtorInjectionFixtures.testCtorExtract19 <==> Data.map(
          "apply" -> Data(
            "hearth.examples.kinds.Arity19[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "unapplyMatch" -> Data("true"),
          "unapplyNoMatch" -> Data("false")
        )
      }

      test("should work for Ctor20 extracted from Arity20[Int, ...]") {
        CrossCtorInjectionFixtures.testCtorExtract20 <==> Data.map(
          "apply" -> Data(
            "hearth.examples.kinds.Arity20[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "unapplyMatch" -> Data("true"),
          "unapplyNoMatch" -> Data("false")
        )
      }

      test("should work for Ctor21 extracted from Arity21[Int, ...]") {
        CrossCtorInjectionFixtures.testCtorExtract21 <==> Data.map(
          "apply" -> Data(
            "hearth.examples.kinds.Arity21[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "unapplyMatch" -> Data("true"),
          "unapplyNoMatch" -> Data("false")
        )
      }

      test("should work for Ctor22 extracted from Arity22[Int, ...]") {
        CrossCtorInjectionFixtures.testCtorExtract22 <==> Data.map(
          "apply" -> Data(
            "hearth.examples.kinds.Arity22[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "unapplyMatch" -> Data("true"),
          "unapplyNoMatch" -> Data("false")
        )
      }

    }

    group("for Type.CtorN.setX.apply/unapply") {

      test("should verify apply/unapply for Ctor2-Ctor8 setA/setLast") {
        CrossCtorInjectionFixtures.testCtorSetApplyUnapply2to8 <==> Data.map(
          "eitherSetAApply" -> Data(
            "scala.util.Either[scala.Int, scala.Int]"
          ),
          "eitherSetAUnapplyMatch" -> Data("true"),
          "eitherSetAUnapplyNoMatch" -> Data("false"),
          "eitherSetBApply" -> Data(
            "scala.util.Either[scala.Int, scala.Int]"
          ),
          "eitherSetBUnapplyMatch" -> Data("true"),
          "eitherSetBUnapplyNoMatch" -> Data("false"),
          "arity3SetAApply" -> Data(
            "hearth.examples.kinds.Arity3[scala.Int, scala.Int, scala.Int]"
          ),
          "arity3SetAUnapplyMatch" -> Data("true"),
          "arity3SetAUnapplyNoMatch" -> Data("false"),
          "arity3SetCApply" -> Data(
            "hearth.examples.kinds.Arity3[scala.Int, scala.Int, scala.Int]"
          ),
          "arity3SetCUnapplyMatch" -> Data("true"),
          "arity3SetCUnapplyNoMatch" -> Data("false"),
          "arity4SetAApply" -> Data(
            "hearth.examples.kinds.Arity4[scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "arity4SetAUnapplyMatch" -> Data("true"),
          "arity4SetAUnapplyNoMatch" -> Data("false"),
          "arity4SetDApply" -> Data(
            "hearth.examples.kinds.Arity4[scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "arity4SetDUnapplyMatch" -> Data("true"),
          "arity4SetDUnapplyNoMatch" -> Data("false"),
          "arity5SetAApply" -> Data(
            "hearth.examples.kinds.Arity5[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "arity5SetAUnapplyMatch" -> Data("true"),
          "arity5SetAUnapplyNoMatch" -> Data("false"),
          "arity5SetEApply" -> Data(
            "hearth.examples.kinds.Arity5[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "arity5SetEUnapplyMatch" -> Data("true"),
          "arity5SetEUnapplyNoMatch" -> Data("false"),
          "arity6SetAApply" -> Data(
            "hearth.examples.kinds.Arity6[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "arity6SetAUnapplyMatch" -> Data("true"),
          "arity6SetAUnapplyNoMatch" -> Data("false"),
          "arity6SetFApply" -> Data(
            "hearth.examples.kinds.Arity6[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "arity6SetFUnapplyMatch" -> Data("true"),
          "arity6SetFUnapplyNoMatch" -> Data("false"),
          "arity7SetAApply" -> Data(
            "hearth.examples.kinds.Arity7[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "arity7SetAUnapplyMatch" -> Data("true"),
          "arity7SetAUnapplyNoMatch" -> Data("false"),
          "arity7SetGApply" -> Data(
            "hearth.examples.kinds.Arity7[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "arity7SetGUnapplyMatch" -> Data("true"),
          "arity7SetGUnapplyNoMatch" -> Data("false"),
          "arity8SetAApply" -> Data(
            "hearth.examples.kinds.Arity8[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "arity8SetAUnapplyMatch" -> Data("true"),
          "arity8SetAUnapplyNoMatch" -> Data("false"),
          "arity8SetHApply" -> Data(
            "hearth.examples.kinds.Arity8[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "arity8SetHUnapplyMatch" -> Data("true"),
          "arity8SetHUnapplyNoMatch" -> Data("false")
        )
      }

      test("should verify apply/unapply for Ctor9-Ctor15 setA/setLast") {
        CrossCtorInjectionFixtures.testCtorSetApplyUnapply9to15 <==> Data.map(
          "arity9SetAApply" -> Data(
            "hearth.examples.kinds.Arity9[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "arity9SetAUnapplyMatch" -> Data("true"),
          "arity9SetAUnapplyNoMatch" -> Data("false"),
          "arity9SetIApply" -> Data(
            "hearth.examples.kinds.Arity9[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "arity9SetIUnapplyMatch" -> Data("true"),
          "arity9SetIUnapplyNoMatch" -> Data("false"),
          "arity10SetAApply" -> Data(
            "hearth.examples.kinds.Arity10[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "arity10SetAUnapplyMatch" -> Data("true"),
          "arity10SetAUnapplyNoMatch" -> Data("false"),
          "arity10SetJApply" -> Data(
            "hearth.examples.kinds.Arity10[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "arity10SetJUnapplyMatch" -> Data("true"),
          "arity10SetJUnapplyNoMatch" -> Data("false"),
          "arity11SetAApply" -> Data(
            "hearth.examples.kinds.Arity11[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "arity11SetAUnapplyMatch" -> Data("true"),
          "arity11SetAUnapplyNoMatch" -> Data("false"),
          "arity11SetKApply" -> Data(
            "hearth.examples.kinds.Arity11[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "arity11SetKUnapplyMatch" -> Data("true"),
          "arity11SetKUnapplyNoMatch" -> Data("false"),
          "arity12SetAApply" -> Data(
            "hearth.examples.kinds.Arity12[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "arity12SetAUnapplyMatch" -> Data("true"),
          "arity12SetAUnapplyNoMatch" -> Data("false"),
          "arity12SetLApply" -> Data(
            "hearth.examples.kinds.Arity12[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "arity12SetLUnapplyMatch" -> Data("true"),
          "arity12SetLUnapplyNoMatch" -> Data("false"),
          "arity13SetAApply" -> Data(
            "hearth.examples.kinds.Arity13[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "arity13SetAUnapplyMatch" -> Data("true"),
          "arity13SetAUnapplyNoMatch" -> Data("false"),
          "arity13SetMApply" -> Data(
            "hearth.examples.kinds.Arity13[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "arity13SetMUnapplyMatch" -> Data("true"),
          "arity13SetMUnapplyNoMatch" -> Data("false"),
          "arity14SetAApply" -> Data(
            "hearth.examples.kinds.Arity14[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "arity14SetAUnapplyMatch" -> Data("true"),
          "arity14SetAUnapplyNoMatch" -> Data("false"),
          "arity14SetNApply" -> Data(
            "hearth.examples.kinds.Arity14[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "arity14SetNUnapplyMatch" -> Data("true"),
          "arity14SetNUnapplyNoMatch" -> Data("false"),
          "arity15SetAApply" -> Data(
            "hearth.examples.kinds.Arity15[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "arity15SetAUnapplyMatch" -> Data("true"),
          "arity15SetAUnapplyNoMatch" -> Data("false"),
          "arity15SetOApply" -> Data(
            "hearth.examples.kinds.Arity15[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "arity15SetOUnapplyMatch" -> Data("true"),
          "arity15SetOUnapplyNoMatch" -> Data("false")
        )
      }

      test("should verify apply/unapply for Ctor16-Ctor22 setA/setLast") {
        CrossCtorInjectionFixtures.testCtorSetApplyUnapply16to22 <==> Data.map(
          "arity16SetAApply" -> Data(
            "hearth.examples.kinds.Arity16[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "arity16SetAUnapplyMatch" -> Data("true"),
          "arity16SetAUnapplyNoMatch" -> Data("false"),
          "arity16SetPApply" -> Data(
            "hearth.examples.kinds.Arity16[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "arity16SetPUnapplyMatch" -> Data("true"),
          "arity16SetPUnapplyNoMatch" -> Data("false"),
          "arity17SetAApply" -> Data(
            "hearth.examples.kinds.Arity17[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "arity17SetAUnapplyMatch" -> Data("true"),
          "arity17SetAUnapplyNoMatch" -> Data("false"),
          "arity17SetQApply" -> Data(
            "hearth.examples.kinds.Arity17[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "arity17SetQUnapplyMatch" -> Data("true"),
          "arity17SetQUnapplyNoMatch" -> Data("false"),
          "arity18SetAApply" -> Data(
            "hearth.examples.kinds.Arity18[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "arity18SetAUnapplyMatch" -> Data("true"),
          "arity18SetAUnapplyNoMatch" -> Data("false"),
          "arity18SetRApply" -> Data(
            "hearth.examples.kinds.Arity18[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "arity18SetRUnapplyMatch" -> Data("true"),
          "arity18SetRUnapplyNoMatch" -> Data("false"),
          "arity19SetAApply" -> Data(
            "hearth.examples.kinds.Arity19[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "arity19SetAUnapplyMatch" -> Data("true"),
          "arity19SetAUnapplyNoMatch" -> Data("false"),
          "arity19SetSApply" -> Data(
            "hearth.examples.kinds.Arity19[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "arity19SetSUnapplyMatch" -> Data("true"),
          "arity19SetSUnapplyNoMatch" -> Data("false"),
          "arity20SetAApply" -> Data(
            "hearth.examples.kinds.Arity20[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "arity20SetAUnapplyMatch" -> Data("true"),
          "arity20SetAUnapplyNoMatch" -> Data("false"),
          "arity20SetTApply" -> Data(
            "hearth.examples.kinds.Arity20[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "arity20SetTUnapplyMatch" -> Data("true"),
          "arity20SetTUnapplyNoMatch" -> Data("false"),
          "arity21SetAApply" -> Data(
            "hearth.examples.kinds.Arity21[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "arity21SetAUnapplyMatch" -> Data("true"),
          "arity21SetAUnapplyNoMatch" -> Data("false"),
          "arity21SetUApply" -> Data(
            "hearth.examples.kinds.Arity21[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "arity21SetUUnapplyMatch" -> Data("true"),
          "arity21SetUUnapplyNoMatch" -> Data("false"),
          "arity22SetAApply" -> Data(
            "hearth.examples.kinds.Arity22[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "arity22SetAUnapplyMatch" -> Data("true"),
          "arity22SetAUnapplyNoMatch" -> Data("false"),
          "arity22SetVApply" -> Data(
            "hearth.examples.kinds.Arity22[scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int, scala.Int]"
          ),
          "arity22SetVUnapplyMatch" -> Data("true"),
          "arity22SetVUnapplyNoMatch" -> Data("false")
        )
      }

    }

    group("for Type.CtorN.setMiddle.asUntyped") {

      test("should verify middle setters asUntyped for Ctor3-Ctor7") {
        val isScala3 = LanguageVersion.byHearth.isScala3
        CrossCtorInjectionFixtures.testCtorSetMiddleAsUntyped3to7 <==> Data.map(
          "arity3SetBUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity3[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity3[_$1, scala.Int, _$2]"
            else "({type λ[A, C] = hearth.examples.kinds.Arity3})#λ"
          ),
          "arity4SetBUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity4[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity4[_$1, scala.Int, _$2, _$3]"
            else "({type λ[A, C, D] = hearth.examples.kinds.Arity4})#λ"
          ),
          "arity4SetCUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity4[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity4[_$1, _$2, scala.Int, _$3]"
            else "({type λ[A, B, D] = hearth.examples.kinds.Arity4})#λ"
          ),
          "arity5SetBUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity5[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity5[_$1, scala.Int, _$2, _$3, _$4]"
            else "({type λ[A, C, D, E] = hearth.examples.kinds.Arity5})#λ"
          ),
          "arity5SetCUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity5[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity5[_$1, _$2, scala.Int, _$3, _$4]"
            else "({type λ[A, B, D, E] = hearth.examples.kinds.Arity5})#λ"
          ),
          "arity5SetDUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity5[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity5[_$1, _$2, _$3, scala.Int, _$4]"
            else "({type λ[A, B, C, E] = hearth.examples.kinds.Arity5})#λ"
          ),
          "arity6SetBUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity6[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity6[_$1, scala.Int, _$2, _$3, _$4, _$5]"
            else "({type λ[A, C, D, E, F] = hearth.examples.kinds.Arity6})#λ"
          ),
          "arity6SetCUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity6[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity6[_$1, _$2, scala.Int, _$3, _$4, _$5]"
            else "({type λ[A, B, D, E, F] = hearth.examples.kinds.Arity6})#λ"
          ),
          "arity6SetDUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity6[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity6[_$1, _$2, _$3, scala.Int, _$4, _$5]"
            else "({type λ[A, B, C, E, F] = hearth.examples.kinds.Arity6})#λ"
          ),
          "arity6SetEUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity6[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity6[_$1, _$2, _$3, _$4, scala.Int, _$5]"
            else "({type λ[A, B, C, D, F] = hearth.examples.kinds.Arity6})#λ"
          ),
          "arity7SetBUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity7[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity7[_$1, scala.Int, _$2, _$3, _$4, _$5, _$6]"
            else "({type λ[A, C, D, E, F, G] = hearth.examples.kinds.Arity7})#λ"
          ),
          "arity7SetCUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity7[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity7[_$1, _$2, scala.Int, _$3, _$4, _$5, _$6]"
            else "({type λ[A, B, D, E, F, G] = hearth.examples.kinds.Arity7})#λ"
          ),
          "arity7SetDUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity7[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity7[_$1, _$2, _$3, scala.Int, _$4, _$5, _$6]"
            else "({type λ[A, B, C, E, F, G] = hearth.examples.kinds.Arity7})#λ"
          ),
          "arity7SetEUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity7[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity7[_$1, _$2, _$3, _$4, scala.Int, _$5, _$6]"
            else "({type λ[A, B, C, D, F, G] = hearth.examples.kinds.Arity7})#λ"
          ),
          "arity7SetFUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity7[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity7[_$1, _$2, _$3, _$4, _$5, scala.Int, _$6]"
            else "({type λ[A, B, C, D, E, G] = hearth.examples.kinds.Arity7})#λ"
          )
        )
      }

      test("should verify middle setters asUntyped for Ctor8-Ctor12") {
        val isScala3 = LanguageVersion.byHearth.isScala3
        CrossCtorInjectionFixtures.testCtorSetMiddleAsUntyped8to12 <==> Data.map(
          "arity8SetBUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity8[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity8[_$1, scala.Int, _$2, _$3, _$4, _$5, _$6, _$7]"
            else "({type λ[A, C, D, E, F, G, H] = hearth.examples.kinds.Arity8})#λ"
          ),
          "arity8SetCUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity8[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity8[_$1, _$2, scala.Int, _$3, _$4, _$5, _$6, _$7]"
            else "({type λ[A, B, D, E, F, G, H] = hearth.examples.kinds.Arity8})#λ"
          ),
          "arity8SetDUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity8[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity8[_$1, _$2, _$3, scala.Int, _$4, _$5, _$6, _$7]"
            else "({type λ[A, B, C, E, F, G, H] = hearth.examples.kinds.Arity8})#λ"
          ),
          "arity8SetEUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity8[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity8[_$1, _$2, _$3, _$4, scala.Int, _$5, _$6, _$7]"
            else "({type λ[A, B, C, D, F, G, H] = hearth.examples.kinds.Arity8})#λ"
          ),
          "arity8SetFUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity8[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity8[_$1, _$2, _$3, _$4, _$5, scala.Int, _$6, _$7]"
            else "({type λ[A, B, C, D, E, G, H] = hearth.examples.kinds.Arity8})#λ"
          ),
          "arity8SetGUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity8[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity8[_$1, _$2, _$3, _$4, _$5, _$6, scala.Int, _$7]"
            else "({type λ[A, B, C, D, E, F, H] = hearth.examples.kinds.Arity8})#λ"
          ),
          "arity9SetBUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity9[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity9[_$1, scala.Int, _$2, _$3, _$4, _$5, _$6, _$7, _$8]"
            else "({type λ[A, C, D, E, F, G, H, I] = hearth.examples.kinds.Arity9})#λ"
          ),
          "arity9SetCUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity9[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity9[_$1, _$2, scala.Int, _$3, _$4, _$5, _$6, _$7, _$8]"
            else "({type λ[A, B, D, E, F, G, H, I] = hearth.examples.kinds.Arity9})#λ"
          ),
          "arity9SetDUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity9[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity9[_$1, _$2, _$3, scala.Int, _$4, _$5, _$6, _$7, _$8]"
            else "({type λ[A, B, C, E, F, G, H, I] = hearth.examples.kinds.Arity9})#λ"
          ),
          "arity9SetEUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity9[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity9[_$1, _$2, _$3, _$4, scala.Int, _$5, _$6, _$7, _$8]"
            else "({type λ[A, B, C, D, F, G, H, I] = hearth.examples.kinds.Arity9})#λ"
          ),
          "arity9SetFUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity9[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity9[_$1, _$2, _$3, _$4, _$5, scala.Int, _$6, _$7, _$8]"
            else "({type λ[A, B, C, D, E, G, H, I] = hearth.examples.kinds.Arity9})#λ"
          ),
          "arity9SetGUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity9[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity9[_$1, _$2, _$3, _$4, _$5, _$6, scala.Int, _$7, _$8]"
            else "({type λ[A, B, C, D, E, F, H, I] = hearth.examples.kinds.Arity9})#λ"
          ),
          "arity9SetHUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity9[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity9[_$1, _$2, _$3, _$4, _$5, _$6, _$7, scala.Int, _$8]"
            else "({type λ[A, B, C, D, E, F, G, I] = hearth.examples.kinds.Arity9})#λ"
          ),
          "arity10SetBUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity10[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity10[_$1, scala.Int, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9]"
            else "({type λ[A, C, D, E, F, G, H, I, J] = hearth.examples.kinds.Arity10})#λ"
          ),
          "arity10SetCUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity10[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity10[_$1, _$2, scala.Int, _$3, _$4, _$5, _$6, _$7, _$8, _$9]"
            else "({type λ[A, B, D, E, F, G, H, I, J] = hearth.examples.kinds.Arity10})#λ"
          ),
          "arity10SetDUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity10[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity10[_$1, _$2, _$3, scala.Int, _$4, _$5, _$6, _$7, _$8, _$9]"
            else "({type λ[A, B, C, E, F, G, H, I, J] = hearth.examples.kinds.Arity10})#λ"
          ),
          "arity10SetEUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity10[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity10[_$1, _$2, _$3, _$4, scala.Int, _$5, _$6, _$7, _$8, _$9]"
            else "({type λ[A, B, C, D, F, G, H, I, J] = hearth.examples.kinds.Arity10})#λ"
          ),
          "arity10SetFUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity10[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity10[_$1, _$2, _$3, _$4, _$5, scala.Int, _$6, _$7, _$8, _$9]"
            else "({type λ[A, B, C, D, E, G, H, I, J] = hearth.examples.kinds.Arity10})#λ"
          ),
          "arity10SetGUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity10[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity10[_$1, _$2, _$3, _$4, _$5, _$6, scala.Int, _$7, _$8, _$9]"
            else "({type λ[A, B, C, D, E, F, H, I, J] = hearth.examples.kinds.Arity10})#λ"
          ),
          "arity10SetHUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity10[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity10[_$1, _$2, _$3, _$4, _$5, _$6, _$7, scala.Int, _$8, _$9]"
            else "({type λ[A, B, C, D, E, F, G, I, J] = hearth.examples.kinds.Arity10})#λ"
          ),
          "arity10SetIUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity10[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity10[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, scala.Int, _$9]"
            else "({type λ[A, B, C, D, E, F, G, H, J] = hearth.examples.kinds.Arity10})#λ"
          ),
          "arity11SetBUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity11[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity11[_$1, scala.Int, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10]"
            else "({type λ[A, C, D, E, F, G, H, I, J, K] = hearth.examples.kinds.Arity11})#λ"
          ),
          "arity11SetCUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity11[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity11[_$1, _$2, scala.Int, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10]"
            else "({type λ[A, B, D, E, F, G, H, I, J, K] = hearth.examples.kinds.Arity11})#λ"
          ),
          "arity11SetDUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity11[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity11[_$1, _$2, _$3, scala.Int, _$4, _$5, _$6, _$7, _$8, _$9, _$10]"
            else "({type λ[A, B, C, E, F, G, H, I, J, K] = hearth.examples.kinds.Arity11})#λ"
          ),
          "arity11SetEUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity11[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity11[_$1, _$2, _$3, _$4, scala.Int, _$5, _$6, _$7, _$8, _$9, _$10]"
            else "({type λ[A, B, C, D, F, G, H, I, J, K] = hearth.examples.kinds.Arity11})#λ"
          ),
          "arity11SetFUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity11[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity11[_$1, _$2, _$3, _$4, _$5, scala.Int, _$6, _$7, _$8, _$9, _$10]"
            else "({type λ[A, B, C, D, E, G, H, I, J, K] = hearth.examples.kinds.Arity11})#λ"
          ),
          "arity11SetGUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity11[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity11[_$1, _$2, _$3, _$4, _$5, _$6, scala.Int, _$7, _$8, _$9, _$10]"
            else "({type λ[A, B, C, D, E, F, H, I, J, K] = hearth.examples.kinds.Arity11})#λ"
          ),
          "arity11SetHUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity11[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity11[_$1, _$2, _$3, _$4, _$5, _$6, _$7, scala.Int, _$8, _$9, _$10]"
            else "({type λ[A, B, C, D, E, F, G, I, J, K] = hearth.examples.kinds.Arity11})#λ"
          ),
          "arity11SetIUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity11[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity11[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, scala.Int, _$9, _$10]"
            else "({type λ[A, B, C, D, E, F, G, H, J, K] = hearth.examples.kinds.Arity11})#λ"
          ),
          "arity11SetJUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity11[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity11[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, scala.Int, _$10]"
            else "({type λ[A, B, C, D, E, F, G, H, I, K] = hearth.examples.kinds.Arity11})#λ"
          ),
          "arity12SetBUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity12[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity12[_$1, scala.Int, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11]"
            else "({type λ[A, C, D, E, F, G, H, I, J, K, L] = hearth.examples.kinds.Arity12})#λ"
          ),
          "arity12SetCUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity12[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity12[_$1, _$2, scala.Int, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11]"
            else "({type λ[A, B, D, E, F, G, H, I, J, K, L] = hearth.examples.kinds.Arity12})#λ"
          ),
          "arity12SetDUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity12[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity12[_$1, _$2, _$3, scala.Int, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11]"
            else "({type λ[A, B, C, E, F, G, H, I, J, K, L] = hearth.examples.kinds.Arity12})#λ"
          ),
          "arity12SetEUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity12[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity12[_$1, _$2, _$3, _$4, scala.Int, _$5, _$6, _$7, _$8, _$9, _$10, _$11]"
            else "({type λ[A, B, C, D, F, G, H, I, J, K, L] = hearth.examples.kinds.Arity12})#λ"
          ),
          "arity12SetFUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity12[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity12[_$1, _$2, _$3, _$4, _$5, scala.Int, _$6, _$7, _$8, _$9, _$10, _$11]"
            else "({type λ[A, B, C, D, E, G, H, I, J, K, L] = hearth.examples.kinds.Arity12})#λ"
          ),
          "arity12SetGUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity12[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity12[_$1, _$2, _$3, _$4, _$5, _$6, scala.Int, _$7, _$8, _$9, _$10, _$11]"
            else "({type λ[A, B, C, D, E, F, H, I, J, K, L] = hearth.examples.kinds.Arity12})#λ"
          ),
          "arity12SetHUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity12[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity12[_$1, _$2, _$3, _$4, _$5, _$6, _$7, scala.Int, _$8, _$9, _$10, _$11]"
            else "({type λ[A, B, C, D, E, F, G, I, J, K, L] = hearth.examples.kinds.Arity12})#λ"
          ),
          "arity12SetIUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity12[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity12[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, scala.Int, _$9, _$10, _$11]"
            else "({type λ[A, B, C, D, E, F, G, H, J, K, L] = hearth.examples.kinds.Arity12})#λ"
          ),
          "arity12SetJUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity12[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity12[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, scala.Int, _$10, _$11]"
            else "({type λ[A, B, C, D, E, F, G, H, I, K, L] = hearth.examples.kinds.Arity12})#λ"
          ),
          "arity12SetKUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity12[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity12[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, scala.Int, _$11]"
            else "({type λ[A, B, C, D, E, F, G, H, I, J, L] = hearth.examples.kinds.Arity12})#λ"
          )
        )
      }

      test("should verify middle setters asUntyped for Ctor13-Ctor17") {
        val isScala3 = LanguageVersion.byHearth.isScala3
        CrossCtorInjectionFixtures.testCtorSetMiddleAsUntyped13to17 <==> Data.map(
          "arity13SetBUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity13[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity13[_$1, scala.Int, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12]"
            else "({type λ[A, C, D, E, F, G, H, I, J, K, L, M] = hearth.examples.kinds.Arity13})#λ"
          ),
          "arity13SetCUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity13[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity13[_$1, _$2, scala.Int, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12]"
            else "({type λ[A, B, D, E, F, G, H, I, J, K, L, M] = hearth.examples.kinds.Arity13})#λ"
          ),
          "arity13SetDUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity13[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity13[_$1, _$2, _$3, scala.Int, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12]"
            else "({type λ[A, B, C, E, F, G, H, I, J, K, L, M] = hearth.examples.kinds.Arity13})#λ"
          ),
          "arity13SetEUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity13[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity13[_$1, _$2, _$3, _$4, scala.Int, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12]"
            else "({type λ[A, B, C, D, F, G, H, I, J, K, L, M] = hearth.examples.kinds.Arity13})#λ"
          ),
          "arity13SetFUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity13[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity13[_$1, _$2, _$3, _$4, _$5, scala.Int, _$6, _$7, _$8, _$9, _$10, _$11, _$12]"
            else "({type λ[A, B, C, D, E, G, H, I, J, K, L, M] = hearth.examples.kinds.Arity13})#λ"
          ),
          "arity13SetGUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity13[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity13[_$1, _$2, _$3, _$4, _$5, _$6, scala.Int, _$7, _$8, _$9, _$10, _$11, _$12]"
            else "({type λ[A, B, C, D, E, F, H, I, J, K, L, M] = hearth.examples.kinds.Arity13})#λ"
          ),
          "arity13SetHUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity13[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity13[_$1, _$2, _$3, _$4, _$5, _$6, _$7, scala.Int, _$8, _$9, _$10, _$11, _$12]"
            else "({type λ[A, B, C, D, E, F, G, I, J, K, L, M] = hearth.examples.kinds.Arity13})#λ"
          ),
          "arity13SetIUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity13[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity13[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, scala.Int, _$9, _$10, _$11, _$12]"
            else "({type λ[A, B, C, D, E, F, G, H, J, K, L, M] = hearth.examples.kinds.Arity13})#λ"
          ),
          "arity13SetJUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity13[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity13[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, scala.Int, _$10, _$11, _$12]"
            else "({type λ[A, B, C, D, E, F, G, H, I, K, L, M] = hearth.examples.kinds.Arity13})#λ"
          ),
          "arity13SetKUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity13[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity13[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, scala.Int, _$11, _$12]"
            else "({type λ[A, B, C, D, E, F, G, H, I, J, L, M] = hearth.examples.kinds.Arity13})#λ"
          ),
          "arity13SetLUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity13[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity13[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, scala.Int, _$12]"
            else "({type λ[A, B, C, D, E, F, G, H, I, J, K, M] = hearth.examples.kinds.Arity13})#λ"
          ),
          "arity14SetBUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity14[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity14[_$1, scala.Int, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13]"
            else "({type λ[A, C, D, E, F, G, H, I, J, K, L, M, N] = hearth.examples.kinds.Arity14})#λ"
          ),
          "arity14SetCUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity14[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity14[_$1, _$2, scala.Int, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13]"
            else "({type λ[A, B, D, E, F, G, H, I, J, K, L, M, N] = hearth.examples.kinds.Arity14})#λ"
          ),
          "arity14SetDUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity14[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity14[_$1, _$2, _$3, scala.Int, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13]"
            else "({type λ[A, B, C, E, F, G, H, I, J, K, L, M, N] = hearth.examples.kinds.Arity14})#λ"
          ),
          "arity14SetEUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity14[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity14[_$1, _$2, _$3, _$4, scala.Int, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13]"
            else "({type λ[A, B, C, D, F, G, H, I, J, K, L, M, N] = hearth.examples.kinds.Arity14})#λ"
          ),
          "arity14SetFUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity14[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity14[_$1, _$2, _$3, _$4, _$5, scala.Int, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13]"
            else "({type λ[A, B, C, D, E, G, H, I, J, K, L, M, N] = hearth.examples.kinds.Arity14})#λ"
          ),
          "arity14SetGUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity14[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity14[_$1, _$2, _$3, _$4, _$5, _$6, scala.Int, _$7, _$8, _$9, _$10, _$11, _$12, _$13]"
            else "({type λ[A, B, C, D, E, F, H, I, J, K, L, M, N] = hearth.examples.kinds.Arity14})#λ"
          ),
          "arity14SetHUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity14[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity14[_$1, _$2, _$3, _$4, _$5, _$6, _$7, scala.Int, _$8, _$9, _$10, _$11, _$12, _$13]"
            else "({type λ[A, B, C, D, E, F, G, I, J, K, L, M, N] = hearth.examples.kinds.Arity14})#λ"
          ),
          "arity14SetIUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity14[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity14[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, scala.Int, _$9, _$10, _$11, _$12, _$13]"
            else "({type λ[A, B, C, D, E, F, G, H, J, K, L, M, N] = hearth.examples.kinds.Arity14})#λ"
          ),
          "arity14SetJUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity14[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity14[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, scala.Int, _$10, _$11, _$12, _$13]"
            else "({type λ[A, B, C, D, E, F, G, H, I, K, L, M, N] = hearth.examples.kinds.Arity14})#λ"
          ),
          "arity14SetKUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity14[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity14[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, scala.Int, _$11, _$12, _$13]"
            else "({type λ[A, B, C, D, E, F, G, H, I, J, L, M, N] = hearth.examples.kinds.Arity14})#λ"
          ),
          "arity14SetLUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity14[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity14[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, scala.Int, _$12, _$13]"
            else "({type λ[A, B, C, D, E, F, G, H, I, J, K, M, N] = hearth.examples.kinds.Arity14})#λ"
          ),
          "arity14SetMUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity14[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity14[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, scala.Int, _$13]"
            else "({type λ[A, B, C, D, E, F, G, H, I, J, K, L, N] = hearth.examples.kinds.Arity14})#λ"
          ),
          "arity15SetBUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity15[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity15[_$1, scala.Int, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14]"
            else "({type λ[A, C, D, E, F, G, H, I, J, K, L, M, N, O] = hearth.examples.kinds.Arity15})#λ"
          ),
          "arity15SetCUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity15[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity15[_$1, _$2, scala.Int, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14]"
            else "({type λ[A, B, D, E, F, G, H, I, J, K, L, M, N, O] = hearth.examples.kinds.Arity15})#λ"
          ),
          "arity15SetDUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity15[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity15[_$1, _$2, _$3, scala.Int, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14]"
            else "({type λ[A, B, C, E, F, G, H, I, J, K, L, M, N, O] = hearth.examples.kinds.Arity15})#λ"
          ),
          "arity15SetEUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity15[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity15[_$1, _$2, _$3, _$4, scala.Int, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14]"
            else "({type λ[A, B, C, D, F, G, H, I, J, K, L, M, N, O] = hearth.examples.kinds.Arity15})#λ"
          ),
          "arity15SetFUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity15[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity15[_$1, _$2, _$3, _$4, _$5, scala.Int, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14]"
            else "({type λ[A, B, C, D, E, G, H, I, J, K, L, M, N, O] = hearth.examples.kinds.Arity15})#λ"
          ),
          "arity15SetGUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity15[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity15[_$1, _$2, _$3, _$4, _$5, _$6, scala.Int, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14]"
            else "({type λ[A, B, C, D, E, F, H, I, J, K, L, M, N, O] = hearth.examples.kinds.Arity15})#λ"
          ),
          "arity15SetHUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity15[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity15[_$1, _$2, _$3, _$4, _$5, _$6, _$7, scala.Int, _$8, _$9, _$10, _$11, _$12, _$13, _$14]"
            else "({type λ[A, B, C, D, E, F, G, I, J, K, L, M, N, O] = hearth.examples.kinds.Arity15})#λ"
          ),
          "arity15SetIUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity15[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity15[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, scala.Int, _$9, _$10, _$11, _$12, _$13, _$14]"
            else "({type λ[A, B, C, D, E, F, G, H, J, K, L, M, N, O] = hearth.examples.kinds.Arity15})#λ"
          ),
          "arity15SetJUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity15[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity15[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, scala.Int, _$10, _$11, _$12, _$13, _$14]"
            else "({type λ[A, B, C, D, E, F, G, H, I, K, L, M, N, O] = hearth.examples.kinds.Arity15})#λ"
          ),
          "arity15SetKUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity15[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity15[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, scala.Int, _$11, _$12, _$13, _$14]"
            else "({type λ[A, B, C, D, E, F, G, H, I, J, L, M, N, O] = hearth.examples.kinds.Arity15})#λ"
          ),
          "arity15SetLUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity15[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity15[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, scala.Int, _$12, _$13, _$14]"
            else "({type λ[A, B, C, D, E, F, G, H, I, J, K, M, N, O] = hearth.examples.kinds.Arity15})#λ"
          ),
          "arity15SetMUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity15[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity15[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, scala.Int, _$13, _$14]"
            else "({type λ[A, B, C, D, E, F, G, H, I, J, K, L, N, O] = hearth.examples.kinds.Arity15})#λ"
          ),
          "arity15SetNUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity15[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity15[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, scala.Int, _$14]"
            else "({type λ[A, B, C, D, E, F, G, H, I, J, K, L, M, O] = hearth.examples.kinds.Arity15})#λ"
          ),
          "arity16SetBUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity16[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity16[_$1, scala.Int, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15]"
            else "({type λ[A, C, D, E, F, G, H, I, J, K, L, M, N, O, P] = hearth.examples.kinds.Arity16})#λ"
          ),
          "arity16SetCUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity16[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity16[_$1, _$2, scala.Int, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15]"
            else "({type λ[A, B, D, E, F, G, H, I, J, K, L, M, N, O, P] = hearth.examples.kinds.Arity16})#λ"
          ),
          "arity16SetDUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity16[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity16[_$1, _$2, _$3, scala.Int, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15]"
            else "({type λ[A, B, C, E, F, G, H, I, J, K, L, M, N, O, P] = hearth.examples.kinds.Arity16})#λ"
          ),
          "arity16SetEUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity16[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity16[_$1, _$2, _$3, _$4, scala.Int, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15]"
            else "({type λ[A, B, C, D, F, G, H, I, J, K, L, M, N, O, P] = hearth.examples.kinds.Arity16})#λ"
          ),
          "arity16SetFUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity16[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity16[_$1, _$2, _$3, _$4, _$5, scala.Int, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15]"
            else "({type λ[A, B, C, D, E, G, H, I, J, K, L, M, N, O, P] = hearth.examples.kinds.Arity16})#λ"
          ),
          "arity16SetGUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity16[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity16[_$1, _$2, _$3, _$4, _$5, _$6, scala.Int, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15]"
            else "({type λ[A, B, C, D, E, F, H, I, J, K, L, M, N, O, P] = hearth.examples.kinds.Arity16})#λ"
          ),
          "arity16SetHUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity16[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity16[_$1, _$2, _$3, _$4, _$5, _$6, _$7, scala.Int, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15]"
            else "({type λ[A, B, C, D, E, F, G, I, J, K, L, M, N, O, P] = hearth.examples.kinds.Arity16})#λ"
          ),
          "arity16SetIUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity16[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity16[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, scala.Int, _$9, _$10, _$11, _$12, _$13, _$14, _$15]"
            else "({type λ[A, B, C, D, E, F, G, H, J, K, L, M, N, O, P] = hearth.examples.kinds.Arity16})#λ"
          ),
          "arity16SetJUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity16[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity16[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, scala.Int, _$10, _$11, _$12, _$13, _$14, _$15]"
            else "({type λ[A, B, C, D, E, F, G, H, I, K, L, M, N, O, P] = hearth.examples.kinds.Arity16})#λ"
          ),
          "arity16SetKUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity16[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity16[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, scala.Int, _$11, _$12, _$13, _$14, _$15]"
            else "({type λ[A, B, C, D, E, F, G, H, I, J, L, M, N, O, P] = hearth.examples.kinds.Arity16})#λ"
          ),
          "arity16SetLUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity16[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity16[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, scala.Int, _$12, _$13, _$14, _$15]"
            else "({type λ[A, B, C, D, E, F, G, H, I, J, K, M, N, O, P] = hearth.examples.kinds.Arity16})#λ"
          ),
          "arity16SetMUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity16[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity16[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, scala.Int, _$13, _$14, _$15]"
            else "({type λ[A, B, C, D, E, F, G, H, I, J, K, L, N, O, P] = hearth.examples.kinds.Arity16})#λ"
          ),
          "arity16SetNUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity16[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity16[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, scala.Int, _$14, _$15]"
            else "({type λ[A, B, C, D, E, F, G, H, I, J, K, L, M, O, P] = hearth.examples.kinds.Arity16})#λ"
          ),
          "arity16SetOUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity16[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity16[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, scala.Int, _$15]"
            else "({type λ[A, B, C, D, E, F, G, H, I, J, K, L, M, N, P] = hearth.examples.kinds.Arity16})#λ"
          ),
          "arity17SetBUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity17[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity17[_$1, scala.Int, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16]"
            else "({type λ[A, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q] = hearth.examples.kinds.Arity17})#λ"
          ),
          "arity17SetCUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity17[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity17[_$1, _$2, scala.Int, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16]"
            else "({type λ[A, B, D, E, F, G, H, I, J, K, L, M, N, O, P, Q] = hearth.examples.kinds.Arity17})#λ"
          ),
          "arity17SetDUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity17[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity17[_$1, _$2, _$3, scala.Int, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16]"
            else "({type λ[A, B, C, E, F, G, H, I, J, K, L, M, N, O, P, Q] = hearth.examples.kinds.Arity17})#λ"
          ),
          "arity17SetEUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity17[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity17[_$1, _$2, _$3, _$4, scala.Int, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16]"
            else "({type λ[A, B, C, D, F, G, H, I, J, K, L, M, N, O, P, Q] = hearth.examples.kinds.Arity17})#λ"
          ),
          "arity17SetFUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity17[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity17[_$1, _$2, _$3, _$4, _$5, scala.Int, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16]"
            else "({type λ[A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q] = hearth.examples.kinds.Arity17})#λ"
          ),
          "arity17SetGUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity17[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity17[_$1, _$2, _$3, _$4, _$5, _$6, scala.Int, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16]"
            else "({type λ[A, B, C, D, E, F, H, I, J, K, L, M, N, O, P, Q] = hearth.examples.kinds.Arity17})#λ"
          ),
          "arity17SetHUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity17[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity17[_$1, _$2, _$3, _$4, _$5, _$6, _$7, scala.Int, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16]"
            else "({type λ[A, B, C, D, E, F, G, I, J, K, L, M, N, O, P, Q] = hearth.examples.kinds.Arity17})#λ"
          ),
          "arity17SetIUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity17[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity17[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, scala.Int, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16]"
            else "({type λ[A, B, C, D, E, F, G, H, J, K, L, M, N, O, P, Q] = hearth.examples.kinds.Arity17})#λ"
          ),
          "arity17SetJUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity17[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity17[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, scala.Int, _$10, _$11, _$12, _$13, _$14, _$15, _$16]"
            else "({type λ[A, B, C, D, E, F, G, H, I, K, L, M, N, O, P, Q] = hearth.examples.kinds.Arity17})#λ"
          ),
          "arity17SetKUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity17[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity17[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, scala.Int, _$11, _$12, _$13, _$14, _$15, _$16]"
            else "({type λ[A, B, C, D, E, F, G, H, I, J, L, M, N, O, P, Q] = hearth.examples.kinds.Arity17})#λ"
          ),
          "arity17SetLUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity17[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity17[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, scala.Int, _$12, _$13, _$14, _$15, _$16]"
            else "({type λ[A, B, C, D, E, F, G, H, I, J, K, M, N, O, P, Q] = hearth.examples.kinds.Arity17})#λ"
          ),
          "arity17SetMUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity17[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity17[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, scala.Int, _$13, _$14, _$15, _$16]"
            else "({type λ[A, B, C, D, E, F, G, H, I, J, K, L, N, O, P, Q] = hearth.examples.kinds.Arity17})#λ"
          ),
          "arity17SetNUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity17[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity17[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, scala.Int, _$14, _$15, _$16]"
            else "({type λ[A, B, C, D, E, F, G, H, I, J, K, L, M, O, P, Q] = hearth.examples.kinds.Arity17})#λ"
          ),
          "arity17SetOUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity17[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity17[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, scala.Int, _$15, _$16]"
            else "({type λ[A, B, C, D, E, F, G, H, I, J, K, L, M, N, P, Q] = hearth.examples.kinds.Arity17})#λ"
          ),
          "arity17SetPUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity17[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity17[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, scala.Int, _$16]"
            else "({type λ[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Q] = hearth.examples.kinds.Arity17})#λ"
          )
        )
      }

      test("should verify middle setters asUntyped for Ctor18-Ctor22") {
        val isScala3 = LanguageVersion.byHearth.isScala3
        CrossCtorInjectionFixtures.testCtorSetMiddleAsUntyped18to22 <==> Data.map(
          "arity18SetBUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity18[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity18[_$1, scala.Int, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16, _$17]"
            else "({type λ[A, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R] = hearth.examples.kinds.Arity18})#λ"
          ),
          "arity18SetCUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity18[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity18[_$1, _$2, scala.Int, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16, _$17]"
            else "({type λ[A, B, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R] = hearth.examples.kinds.Arity18})#λ"
          ),
          "arity18SetDUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity18[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity18[_$1, _$2, _$3, scala.Int, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16, _$17]"
            else "({type λ[A, B, C, E, F, G, H, I, J, K, L, M, N, O, P, Q, R] = hearth.examples.kinds.Arity18})#λ"
          ),
          "arity18SetEUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity18[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity18[_$1, _$2, _$3, _$4, scala.Int, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16, _$17]"
            else "({type λ[A, B, C, D, F, G, H, I, J, K, L, M, N, O, P, Q, R] = hearth.examples.kinds.Arity18})#λ"
          ),
          "arity18SetFUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity18[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity18[_$1, _$2, _$3, _$4, _$5, scala.Int, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16, _$17]"
            else "({type λ[A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q, R] = hearth.examples.kinds.Arity18})#λ"
          ),
          "arity18SetGUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity18[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity18[_$1, _$2, _$3, _$4, _$5, _$6, scala.Int, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16, _$17]"
            else "({type λ[A, B, C, D, E, F, H, I, J, K, L, M, N, O, P, Q, R] = hearth.examples.kinds.Arity18})#λ"
          ),
          "arity18SetHUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity18[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity18[_$1, _$2, _$3, _$4, _$5, _$6, _$7, scala.Int, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16, _$17]"
            else "({type λ[A, B, C, D, E, F, G, I, J, K, L, M, N, O, P, Q, R] = hearth.examples.kinds.Arity18})#λ"
          ),
          "arity18SetIUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity18[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity18[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, scala.Int, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16, _$17]"
            else "({type λ[A, B, C, D, E, F, G, H, J, K, L, M, N, O, P, Q, R] = hearth.examples.kinds.Arity18})#λ"
          ),
          "arity18SetJUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity18[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity18[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, scala.Int, _$10, _$11, _$12, _$13, _$14, _$15, _$16, _$17]"
            else "({type λ[A, B, C, D, E, F, G, H, I, K, L, M, N, O, P, Q, R] = hearth.examples.kinds.Arity18})#λ"
          ),
          "arity18SetKUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity18[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity18[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, scala.Int, _$11, _$12, _$13, _$14, _$15, _$16, _$17]"
            else "({type λ[A, B, C, D, E, F, G, H, I, J, L, M, N, O, P, Q, R] = hearth.examples.kinds.Arity18})#λ"
          ),
          "arity18SetLUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity18[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity18[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, scala.Int, _$12, _$13, _$14, _$15, _$16, _$17]"
            else "({type λ[A, B, C, D, E, F, G, H, I, J, K, M, N, O, P, Q, R] = hearth.examples.kinds.Arity18})#λ"
          ),
          "arity18SetMUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity18[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity18[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, scala.Int, _$13, _$14, _$15, _$16, _$17]"
            else "({type λ[A, B, C, D, E, F, G, H, I, J, K, L, N, O, P, Q, R] = hearth.examples.kinds.Arity18})#λ"
          ),
          "arity18SetNUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity18[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity18[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, scala.Int, _$14, _$15, _$16, _$17]"
            else "({type λ[A, B, C, D, E, F, G, H, I, J, K, L, M, O, P, Q, R] = hearth.examples.kinds.Arity18})#λ"
          ),
          "arity18SetOUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity18[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity18[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, scala.Int, _$15, _$16, _$17]"
            else "({type λ[A, B, C, D, E, F, G, H, I, J, K, L, M, N, P, Q, R] = hearth.examples.kinds.Arity18})#λ"
          ),
          "arity18SetPUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity18[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity18[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, scala.Int, _$16, _$17]"
            else "({type λ[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Q, R] = hearth.examples.kinds.Arity18})#λ"
          ),
          "arity18SetQUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity18[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity18[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16, scala.Int, _$17]"
            else "({type λ[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, R] = hearth.examples.kinds.Arity18})#λ"
          ),
          "arity19SetBUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity19[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity19[_$1, scala.Int, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16, _$17, _$18]"
            else "({type λ[A, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S] = hearth.examples.kinds.Arity19})#λ"
          ),
          "arity19SetCUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity19[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity19[_$1, _$2, scala.Int, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16, _$17, _$18]"
            else "({type λ[A, B, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S] = hearth.examples.kinds.Arity19})#λ"
          ),
          "arity19SetDUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity19[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity19[_$1, _$2, _$3, scala.Int, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16, _$17, _$18]"
            else "({type λ[A, B, C, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S] = hearth.examples.kinds.Arity19})#λ"
          ),
          "arity19SetEUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity19[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity19[_$1, _$2, _$3, _$4, scala.Int, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16, _$17, _$18]"
            else "({type λ[A, B, C, D, F, G, H, I, J, K, L, M, N, O, P, Q, R, S] = hearth.examples.kinds.Arity19})#λ"
          ),
          "arity19SetFUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity19[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity19[_$1, _$2, _$3, _$4, _$5, scala.Int, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16, _$17, _$18]"
            else "({type λ[A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q, R, S] = hearth.examples.kinds.Arity19})#λ"
          ),
          "arity19SetGUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity19[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity19[_$1, _$2, _$3, _$4, _$5, _$6, scala.Int, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16, _$17, _$18]"
            else "({type λ[A, B, C, D, E, F, H, I, J, K, L, M, N, O, P, Q, R, S] = hearth.examples.kinds.Arity19})#λ"
          ),
          "arity19SetHUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity19[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity19[_$1, _$2, _$3, _$4, _$5, _$6, _$7, scala.Int, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16, _$17, _$18]"
            else "({type λ[A, B, C, D, E, F, G, I, J, K, L, M, N, O, P, Q, R, S] = hearth.examples.kinds.Arity19})#λ"
          ),
          "arity19SetIUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity19[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity19[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, scala.Int, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16, _$17, _$18]"
            else "({type λ[A, B, C, D, E, F, G, H, J, K, L, M, N, O, P, Q, R, S] = hearth.examples.kinds.Arity19})#λ"
          ),
          "arity19SetJUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity19[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity19[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, scala.Int, _$10, _$11, _$12, _$13, _$14, _$15, _$16, _$17, _$18]"
            else "({type λ[A, B, C, D, E, F, G, H, I, K, L, M, N, O, P, Q, R, S] = hearth.examples.kinds.Arity19})#λ"
          ),
          "arity19SetKUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity19[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity19[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, scala.Int, _$11, _$12, _$13, _$14, _$15, _$16, _$17, _$18]"
            else "({type λ[A, B, C, D, E, F, G, H, I, J, L, M, N, O, P, Q, R, S] = hearth.examples.kinds.Arity19})#λ"
          ),
          "arity19SetLUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity19[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity19[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, scala.Int, _$12, _$13, _$14, _$15, _$16, _$17, _$18]"
            else "({type λ[A, B, C, D, E, F, G, H, I, J, K, M, N, O, P, Q, R, S] = hearth.examples.kinds.Arity19})#λ"
          ),
          "arity19SetMUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity19[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity19[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, scala.Int, _$13, _$14, _$15, _$16, _$17, _$18]"
            else "({type λ[A, B, C, D, E, F, G, H, I, J, K, L, N, O, P, Q, R, S] = hearth.examples.kinds.Arity19})#λ"
          ),
          "arity19SetNUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity19[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity19[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, scala.Int, _$14, _$15, _$16, _$17, _$18]"
            else "({type λ[A, B, C, D, E, F, G, H, I, J, K, L, M, O, P, Q, R, S] = hearth.examples.kinds.Arity19})#λ"
          ),
          "arity19SetOUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity19[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity19[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, scala.Int, _$15, _$16, _$17, _$18]"
            else "({type λ[A, B, C, D, E, F, G, H, I, J, K, L, M, N, P, Q, R, S] = hearth.examples.kinds.Arity19})#λ"
          ),
          "arity19SetPUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity19[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity19[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, scala.Int, _$16, _$17, _$18]"
            else "({type λ[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Q, R, S] = hearth.examples.kinds.Arity19})#λ"
          ),
          "arity19SetQUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity19[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity19[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16, scala.Int, _$17, _$18]"
            else "({type λ[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, R, S] = hearth.examples.kinds.Arity19})#λ"
          ),
          "arity19SetRUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity19[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity19[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16, _$17, scala.Int, _$18]"
            else "({type λ[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, S] = hearth.examples.kinds.Arity19})#λ"
          ),
          "arity20SetBUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity20[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any, _$19 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity20[_$1, scala.Int, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16, _$17, _$18, _$19]"
            else "({type λ[A, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T] = hearth.examples.kinds.Arity20})#λ"
          ),
          "arity20SetCUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity20[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any, _$19 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity20[_$1, _$2, scala.Int, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16, _$17, _$18, _$19]"
            else "({type λ[A, B, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T] = hearth.examples.kinds.Arity20})#λ"
          ),
          "arity20SetDUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity20[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any, _$19 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity20[_$1, _$2, _$3, scala.Int, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16, _$17, _$18, _$19]"
            else "({type λ[A, B, C, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T] = hearth.examples.kinds.Arity20})#λ"
          ),
          "arity20SetEUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity20[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any, _$19 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity20[_$1, _$2, _$3, _$4, scala.Int, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16, _$17, _$18, _$19]"
            else "({type λ[A, B, C, D, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T] = hearth.examples.kinds.Arity20})#λ"
          ),
          "arity20SetFUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity20[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any, _$19 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity20[_$1, _$2, _$3, _$4, _$5, scala.Int, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16, _$17, _$18, _$19]"
            else "({type λ[A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q, R, S, T] = hearth.examples.kinds.Arity20})#λ"
          ),
          "arity20SetGUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity20[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any, _$19 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity20[_$1, _$2, _$3, _$4, _$5, _$6, scala.Int, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16, _$17, _$18, _$19]"
            else "({type λ[A, B, C, D, E, F, H, I, J, K, L, M, N, O, P, Q, R, S, T] = hearth.examples.kinds.Arity20})#λ"
          ),
          "arity20SetHUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity20[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any, _$19 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity20[_$1, _$2, _$3, _$4, _$5, _$6, _$7, scala.Int, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16, _$17, _$18, _$19]"
            else "({type λ[A, B, C, D, E, F, G, I, J, K, L, M, N, O, P, Q, R, S, T] = hearth.examples.kinds.Arity20})#λ"
          ),
          "arity20SetIUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity20[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any, _$19 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity20[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, scala.Int, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16, _$17, _$18, _$19]"
            else "({type λ[A, B, C, D, E, F, G, H, J, K, L, M, N, O, P, Q, R, S, T] = hearth.examples.kinds.Arity20})#λ"
          ),
          "arity20SetJUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity20[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any, _$19 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity20[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, scala.Int, _$10, _$11, _$12, _$13, _$14, _$15, _$16, _$17, _$18, _$19]"
            else "({type λ[A, B, C, D, E, F, G, H, I, K, L, M, N, O, P, Q, R, S, T] = hearth.examples.kinds.Arity20})#λ"
          ),
          "arity20SetKUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity20[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any, _$19 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity20[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, scala.Int, _$11, _$12, _$13, _$14, _$15, _$16, _$17, _$18, _$19]"
            else "({type λ[A, B, C, D, E, F, G, H, I, J, L, M, N, O, P, Q, R, S, T] = hearth.examples.kinds.Arity20})#λ"
          ),
          "arity20SetLUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity20[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any, _$19 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity20[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, scala.Int, _$12, _$13, _$14, _$15, _$16, _$17, _$18, _$19]"
            else "({type λ[A, B, C, D, E, F, G, H, I, J, K, M, N, O, P, Q, R, S, T] = hearth.examples.kinds.Arity20})#λ"
          ),
          "arity20SetMUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity20[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any, _$19 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity20[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, scala.Int, _$13, _$14, _$15, _$16, _$17, _$18, _$19]"
            else "({type λ[A, B, C, D, E, F, G, H, I, J, K, L, N, O, P, Q, R, S, T] = hearth.examples.kinds.Arity20})#λ"
          ),
          "arity20SetNUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity20[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any, _$19 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity20[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, scala.Int, _$14, _$15, _$16, _$17, _$18, _$19]"
            else "({type λ[A, B, C, D, E, F, G, H, I, J, K, L, M, O, P, Q, R, S, T] = hearth.examples.kinds.Arity20})#λ"
          ),
          "arity20SetOUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity20[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any, _$19 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity20[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, scala.Int, _$15, _$16, _$17, _$18, _$19]"
            else "({type λ[A, B, C, D, E, F, G, H, I, J, K, L, M, N, P, Q, R, S, T] = hearth.examples.kinds.Arity20})#λ"
          ),
          "arity20SetPUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity20[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any, _$19 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity20[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, scala.Int, _$16, _$17, _$18, _$19]"
            else "({type λ[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Q, R, S, T] = hearth.examples.kinds.Arity20})#λ"
          ),
          "arity20SetQUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity20[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any, _$19 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity20[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16, scala.Int, _$17, _$18, _$19]"
            else "({type λ[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, R, S, T] = hearth.examples.kinds.Arity20})#λ"
          ),
          "arity20SetRUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity20[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any, _$19 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity20[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16, _$17, scala.Int, _$18, _$19]"
            else "({type λ[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, S, T] = hearth.examples.kinds.Arity20})#λ"
          ),
          "arity20SetSUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity20[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any, _$19 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity20[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16, _$17, _$18, scala.Int, _$19]"
            else "({type λ[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, T] = hearth.examples.kinds.Arity20})#λ"
          ),
          "arity21SetBUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity21[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any, _$19 >: scala.Nothing <: scala.Any, _$20 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity21[_$1, scala.Int, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16, _$17, _$18, _$19, _$20]"
            else
              "({type λ[A, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U] = hearth.examples.kinds.Arity21})#λ"
          ),
          "arity21SetCUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity21[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any, _$19 >: scala.Nothing <: scala.Any, _$20 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity21[_$1, _$2, scala.Int, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16, _$17, _$18, _$19, _$20]"
            else
              "({type λ[A, B, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U] = hearth.examples.kinds.Arity21})#λ"
          ),
          "arity21SetDUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity21[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any, _$19 >: scala.Nothing <: scala.Any, _$20 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity21[_$1, _$2, _$3, scala.Int, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16, _$17, _$18, _$19, _$20]"
            else
              "({type λ[A, B, C, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U] = hearth.examples.kinds.Arity21})#λ"
          ),
          "arity21SetEUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity21[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any, _$19 >: scala.Nothing <: scala.Any, _$20 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity21[_$1, _$2, _$3, _$4, scala.Int, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16, _$17, _$18, _$19, _$20]"
            else
              "({type λ[A, B, C, D, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U] = hearth.examples.kinds.Arity21})#λ"
          ),
          "arity21SetFUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity21[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any, _$19 >: scala.Nothing <: scala.Any, _$20 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity21[_$1, _$2, _$3, _$4, _$5, scala.Int, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16, _$17, _$18, _$19, _$20]"
            else
              "({type λ[A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U] = hearth.examples.kinds.Arity21})#λ"
          ),
          "arity21SetGUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity21[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any, _$19 >: scala.Nothing <: scala.Any, _$20 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity21[_$1, _$2, _$3, _$4, _$5, _$6, scala.Int, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16, _$17, _$18, _$19, _$20]"
            else
              "({type λ[A, B, C, D, E, F, H, I, J, K, L, M, N, O, P, Q, R, S, T, U] = hearth.examples.kinds.Arity21})#λ"
          ),
          "arity21SetHUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity21[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any, _$19 >: scala.Nothing <: scala.Any, _$20 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity21[_$1, _$2, _$3, _$4, _$5, _$6, _$7, scala.Int, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16, _$17, _$18, _$19, _$20]"
            else
              "({type λ[A, B, C, D, E, F, G, I, J, K, L, M, N, O, P, Q, R, S, T, U] = hearth.examples.kinds.Arity21})#λ"
          ),
          "arity21SetIUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity21[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any, _$19 >: scala.Nothing <: scala.Any, _$20 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity21[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, scala.Int, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16, _$17, _$18, _$19, _$20]"
            else
              "({type λ[A, B, C, D, E, F, G, H, J, K, L, M, N, O, P, Q, R, S, T, U] = hearth.examples.kinds.Arity21})#λ"
          ),
          "arity21SetJUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity21[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any, _$19 >: scala.Nothing <: scala.Any, _$20 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity21[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, scala.Int, _$10, _$11, _$12, _$13, _$14, _$15, _$16, _$17, _$18, _$19, _$20]"
            else
              "({type λ[A, B, C, D, E, F, G, H, I, K, L, M, N, O, P, Q, R, S, T, U] = hearth.examples.kinds.Arity21})#λ"
          ),
          "arity21SetKUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity21[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any, _$19 >: scala.Nothing <: scala.Any, _$20 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity21[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, scala.Int, _$11, _$12, _$13, _$14, _$15, _$16, _$17, _$18, _$19, _$20]"
            else
              "({type λ[A, B, C, D, E, F, G, H, I, J, L, M, N, O, P, Q, R, S, T, U] = hearth.examples.kinds.Arity21})#λ"
          ),
          "arity21SetLUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity21[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any, _$19 >: scala.Nothing <: scala.Any, _$20 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity21[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, scala.Int, _$12, _$13, _$14, _$15, _$16, _$17, _$18, _$19, _$20]"
            else
              "({type λ[A, B, C, D, E, F, G, H, I, J, K, M, N, O, P, Q, R, S, T, U] = hearth.examples.kinds.Arity21})#λ"
          ),
          "arity21SetMUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity21[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any, _$19 >: scala.Nothing <: scala.Any, _$20 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity21[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, scala.Int, _$13, _$14, _$15, _$16, _$17, _$18, _$19, _$20]"
            else
              "({type λ[A, B, C, D, E, F, G, H, I, J, K, L, N, O, P, Q, R, S, T, U] = hearth.examples.kinds.Arity21})#λ"
          ),
          "arity21SetNUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity21[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any, _$19 >: scala.Nothing <: scala.Any, _$20 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity21[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, scala.Int, _$14, _$15, _$16, _$17, _$18, _$19, _$20]"
            else
              "({type λ[A, B, C, D, E, F, G, H, I, J, K, L, M, O, P, Q, R, S, T, U] = hearth.examples.kinds.Arity21})#λ"
          ),
          "arity21SetOUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity21[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any, _$19 >: scala.Nothing <: scala.Any, _$20 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity21[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, scala.Int, _$15, _$16, _$17, _$18, _$19, _$20]"
            else
              "({type λ[A, B, C, D, E, F, G, H, I, J, K, L, M, N, P, Q, R, S, T, U] = hearth.examples.kinds.Arity21})#λ"
          ),
          "arity21SetPUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity21[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any, _$19 >: scala.Nothing <: scala.Any, _$20 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity21[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, scala.Int, _$16, _$17, _$18, _$19, _$20]"
            else
              "({type λ[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Q, R, S, T, U] = hearth.examples.kinds.Arity21})#λ"
          ),
          "arity21SetQUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity21[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any, _$19 >: scala.Nothing <: scala.Any, _$20 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity21[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16, scala.Int, _$17, _$18, _$19, _$20]"
            else
              "({type λ[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, R, S, T, U] = hearth.examples.kinds.Arity21})#λ"
          ),
          "arity21SetRUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity21[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any, _$19 >: scala.Nothing <: scala.Any, _$20 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity21[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16, _$17, scala.Int, _$18, _$19, _$20]"
            else
              "({type λ[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, S, T, U] = hearth.examples.kinds.Arity21})#λ"
          ),
          "arity21SetSUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity21[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any, _$19 >: scala.Nothing <: scala.Any, _$20 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity21[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16, _$17, _$18, scala.Int, _$19, _$20]"
            else
              "({type λ[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, T, U] = hearth.examples.kinds.Arity21})#λ"
          ),
          "arity21SetTUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity21[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any, _$19 >: scala.Nothing <: scala.Any, _$20 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity21[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16, _$17, _$18, _$19, scala.Int, _$20]"
            else
              "({type λ[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, U] = hearth.examples.kinds.Arity21})#λ"
          ),
          "arity22SetBUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity22[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any, _$19 >: scala.Nothing <: scala.Any, _$20 >: scala.Nothing <: scala.Any, _$21 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity22[_$1, scala.Int, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16, _$17, _$18, _$19, _$20, _$21]"
            else
              "({type λ[A, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V] = hearth.examples.kinds.Arity22})#λ"
          ),
          "arity22SetCUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity22[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any, _$19 >: scala.Nothing <: scala.Any, _$20 >: scala.Nothing <: scala.Any, _$21 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity22[_$1, _$2, scala.Int, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16, _$17, _$18, _$19, _$20, _$21]"
            else
              "({type λ[A, B, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V] = hearth.examples.kinds.Arity22})#λ"
          ),
          "arity22SetDUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity22[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any, _$19 >: scala.Nothing <: scala.Any, _$20 >: scala.Nothing <: scala.Any, _$21 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity22[_$1, _$2, _$3, scala.Int, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16, _$17, _$18, _$19, _$20, _$21]"
            else
              "({type λ[A, B, C, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V] = hearth.examples.kinds.Arity22})#λ"
          ),
          "arity22SetEUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity22[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any, _$19 >: scala.Nothing <: scala.Any, _$20 >: scala.Nothing <: scala.Any, _$21 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity22[_$1, _$2, _$3, _$4, scala.Int, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16, _$17, _$18, _$19, _$20, _$21]"
            else
              "({type λ[A, B, C, D, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V] = hearth.examples.kinds.Arity22})#λ"
          ),
          "arity22SetFUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity22[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any, _$19 >: scala.Nothing <: scala.Any, _$20 >: scala.Nothing <: scala.Any, _$21 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity22[_$1, _$2, _$3, _$4, _$5, scala.Int, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16, _$17, _$18, _$19, _$20, _$21]"
            else
              "({type λ[A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V] = hearth.examples.kinds.Arity22})#λ"
          ),
          "arity22SetGUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity22[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any, _$19 >: scala.Nothing <: scala.Any, _$20 >: scala.Nothing <: scala.Any, _$21 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity22[_$1, _$2, _$3, _$4, _$5, _$6, scala.Int, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16, _$17, _$18, _$19, _$20, _$21]"
            else
              "({type λ[A, B, C, D, E, F, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V] = hearth.examples.kinds.Arity22})#λ"
          ),
          "arity22SetHUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity22[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any, _$19 >: scala.Nothing <: scala.Any, _$20 >: scala.Nothing <: scala.Any, _$21 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity22[_$1, _$2, _$3, _$4, _$5, _$6, _$7, scala.Int, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16, _$17, _$18, _$19, _$20, _$21]"
            else
              "({type λ[A, B, C, D, E, F, G, I, J, K, L, M, N, O, P, Q, R, S, T, U, V] = hearth.examples.kinds.Arity22})#λ"
          ),
          "arity22SetIUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity22[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any, _$19 >: scala.Nothing <: scala.Any, _$20 >: scala.Nothing <: scala.Any, _$21 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity22[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, scala.Int, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16, _$17, _$18, _$19, _$20, _$21]"
            else
              "({type λ[A, B, C, D, E, F, G, H, J, K, L, M, N, O, P, Q, R, S, T, U, V] = hearth.examples.kinds.Arity22})#λ"
          ),
          "arity22SetJUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity22[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any, _$19 >: scala.Nothing <: scala.Any, _$20 >: scala.Nothing <: scala.Any, _$21 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity22[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, scala.Int, _$10, _$11, _$12, _$13, _$14, _$15, _$16, _$17, _$18, _$19, _$20, _$21]"
            else
              "({type λ[A, B, C, D, E, F, G, H, I, K, L, M, N, O, P, Q, R, S, T, U, V] = hearth.examples.kinds.Arity22})#λ"
          ),
          "arity22SetKUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity22[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any, _$19 >: scala.Nothing <: scala.Any, _$20 >: scala.Nothing <: scala.Any, _$21 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity22[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, scala.Int, _$11, _$12, _$13, _$14, _$15, _$16, _$17, _$18, _$19, _$20, _$21]"
            else
              "({type λ[A, B, C, D, E, F, G, H, I, J, L, M, N, O, P, Q, R, S, T, U, V] = hearth.examples.kinds.Arity22})#λ"
          ),
          "arity22SetLUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity22[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any, _$19 >: scala.Nothing <: scala.Any, _$20 >: scala.Nothing <: scala.Any, _$21 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity22[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, scala.Int, _$12, _$13, _$14, _$15, _$16, _$17, _$18, _$19, _$20, _$21]"
            else
              "({type λ[A, B, C, D, E, F, G, H, I, J, K, M, N, O, P, Q, R, S, T, U, V] = hearth.examples.kinds.Arity22})#λ"
          ),
          "arity22SetMUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity22[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any, _$19 >: scala.Nothing <: scala.Any, _$20 >: scala.Nothing <: scala.Any, _$21 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity22[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, scala.Int, _$13, _$14, _$15, _$16, _$17, _$18, _$19, _$20, _$21]"
            else
              "({type λ[A, B, C, D, E, F, G, H, I, J, K, L, N, O, P, Q, R, S, T, U, V] = hearth.examples.kinds.Arity22})#λ"
          ),
          "arity22SetNUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity22[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any, _$19 >: scala.Nothing <: scala.Any, _$20 >: scala.Nothing <: scala.Any, _$21 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity22[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, scala.Int, _$14, _$15, _$16, _$17, _$18, _$19, _$20, _$21]"
            else
              "({type λ[A, B, C, D, E, F, G, H, I, J, K, L, M, O, P, Q, R, S, T, U, V] = hearth.examples.kinds.Arity22})#λ"
          ),
          "arity22SetOUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity22[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any, _$19 >: scala.Nothing <: scala.Any, _$20 >: scala.Nothing <: scala.Any, _$21 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity22[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, scala.Int, _$15, _$16, _$17, _$18, _$19, _$20, _$21]"
            else
              "({type λ[A, B, C, D, E, F, G, H, I, J, K, L, M, N, P, Q, R, S, T, U, V] = hearth.examples.kinds.Arity22})#λ"
          ),
          "arity22SetPUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity22[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any, _$19 >: scala.Nothing <: scala.Any, _$20 >: scala.Nothing <: scala.Any, _$21 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity22[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, scala.Int, _$16, _$17, _$18, _$19, _$20, _$21]"
            else
              "({type λ[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Q, R, S, T, U, V] = hearth.examples.kinds.Arity22})#λ"
          ),
          "arity22SetQUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity22[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any, _$19 >: scala.Nothing <: scala.Any, _$20 >: scala.Nothing <: scala.Any, _$21 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity22[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16, scala.Int, _$17, _$18, _$19, _$20, _$21]"
            else
              "({type λ[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, R, S, T, U, V] = hearth.examples.kinds.Arity22})#λ"
          ),
          "arity22SetRUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity22[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any, _$19 >: scala.Nothing <: scala.Any, _$20 >: scala.Nothing <: scala.Any, _$21 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity22[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16, _$17, scala.Int, _$18, _$19, _$20, _$21]"
            else
              "({type λ[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, S, T, U, V] = hearth.examples.kinds.Arity22})#λ"
          ),
          "arity22SetSUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity22[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any, _$19 >: scala.Nothing <: scala.Any, _$20 >: scala.Nothing <: scala.Any, _$21 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity22[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16, _$17, _$18, scala.Int, _$19, _$20, _$21]"
            else
              "({type λ[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, T, U, V] = hearth.examples.kinds.Arity22})#λ"
          ),
          "arity22SetTUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity22[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any, _$19 >: scala.Nothing <: scala.Any, _$20 >: scala.Nothing <: scala.Any, _$21 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity22[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16, _$17, _$18, _$19, scala.Int, _$20, _$21]"
            else
              "({type λ[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, U, V] = hearth.examples.kinds.Arity22})#λ"
          ),
          "arity22SetUUntyped" -> Data(
            if (isScala3)
              "hearth.examples.kinds.Arity22[_$1 >: scala.Nothing <: scala.Any, _$2 >: scala.Nothing <: scala.Any, _$3 >: scala.Nothing <: scala.Any, _$4 >: scala.Nothing <: scala.Any, _$5 >: scala.Nothing <: scala.Any, _$6 >: scala.Nothing <: scala.Any, _$7 >: scala.Nothing <: scala.Any, _$8 >: scala.Nothing <: scala.Any, _$9 >: scala.Nothing <: scala.Any, _$10 >: scala.Nothing <: scala.Any, _$11 >: scala.Nothing <: scala.Any, _$12 >: scala.Nothing <: scala.Any, _$13 >: scala.Nothing <: scala.Any, _$14 >: scala.Nothing <: scala.Any, _$15 >: scala.Nothing <: scala.Any, _$16 >: scala.Nothing <: scala.Any, _$17 >: scala.Nothing <: scala.Any, _$18 >: scala.Nothing <: scala.Any, _$19 >: scala.Nothing <: scala.Any, _$20 >: scala.Nothing <: scala.Any, _$21 >: scala.Nothing <: scala.Any] => hearth.examples.kinds.Arity22[_$1, _$2, _$3, _$4, _$5, _$6, _$7, _$8, _$9, _$10, _$11, _$12, _$13, _$14, _$15, _$16, _$17, _$18, _$19, _$20, scala.Int, _$21]"
            else
              "({type λ[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, V] = hearth.examples.kinds.Arity22})#λ"
          )
        )
      }

    }

  }
}
