package hearth
package typed

/** Macro implementation is in [[TypesFixturesImpl]] */
final class TypeLambdaCtorScala2Spec extends MacroSuite {

  group("Type.CtorK1.of on the macro's own type parameter, written as a kind-projector type lambda") {
    import TypesFixtures.testCtorK1OfOwnTypeParameter

    test("Expr.quote using the lambda as a type argument keeps the fixed type arguments") {
      TypesFixtures.testQuoteWithCtorK1OfOwnTypeParameter[
        Lambda[F[?] => examples.kinds.HigherKindedWithExtra[F, String]]
      ] <==> "constructed"
    }

    test("keeps the type arguments the lambda fixes") {
      testCtorK1OfOwnTypeParameter[Lambda[F[?] => examples.kinds.HigherKindedWithExtra[F, String]]] <==>
        "hearth.examples.kinds.HigherKindedWithExtra[scala.Option, java.lang.String]"
    }
  }
}
