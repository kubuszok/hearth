package hearth
package typed

/** Macro implementation is in [[TypesFixturesImpl]] */
final class TypeLambdaCtorScala2Spec extends MacroSuite {

  // scalafmt rewrites the `F[_]` binder of kind-projector's `Lambda` to `F[?]`, which kind-projector does not read as
  // a higher-kinded type parameter.
  // format: off
  group("Type.CtorK1.of on the macro's own type parameter, written as a kind-projector type lambda") {
    import TypesFixtures.testCtorK1OfOwnTypeParameter

    test("Expr.quote using the lambda as a type argument keeps the fixed type arguments") {
      TypesFixtures.testQuoteWithCtorK1OfOwnTypeParameter[
        Lambda[F[_] => examples.kinds.HigherKindedWithExtra[F, String]]
      ] <==> "constructed"
    }

    test("keeps the type arguments the lambda fixes") {
      testCtorK1OfOwnTypeParameter[Lambda[F[_] => examples.kinds.HigherKindedWithExtra[F, String]]] <==>
        "hearth.examples.kinds.HigherKindedWithExtra[scala.Option, java.lang.String]"
    }
  }
  // format: on
}
