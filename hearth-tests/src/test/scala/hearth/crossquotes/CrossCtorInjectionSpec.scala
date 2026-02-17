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
  }
}
