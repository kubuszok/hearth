package hearth
package crossquotes

import hearth.data.Data

trait CrossCtorInjectionFixturesImpl extends CrossCtorInjectionFixturesImplGen { this: MacroCommons =>

  /** Test: Type.of resolves types when an imported Type.Ctor1 has a recognized name (like "Result").
    *
    * Demonstrates that importing a Type.Ctor1 value named "Result" makes `Type.of[Result[A]]` work correctly. This
    * pattern is used by CtorLikeOf and similar traits.
    */
  def testTypeOfWithImportedCtor1[A: Type]: Expr[Data] = {
    // Create a container mimicking CtorLikeOf's pattern:
    // - type alias Result[X] defines the type constructor
    // - val Result: Type.Ctor1[Result] provides the type constructor value
    // The name "Result" is in the ImportedCrossTypeImplicit set
    val container = makeCtorResultContainer
    import container.Result

    Expr(
      Data.map(
        "resultOfInt" -> Data(Type.of[Result[Int]].plainPrint),
        "resultOfString" -> Data(Type.of[Result[String]].plainPrint),
        "resultOfA" -> Data(Type.of[Result[A]].plainPrint)
      )
    )
  }

  /** Test: Type.of[Trait[F]] works when F comes from Type.Ctor1. */
  def testTypeOfWithCtor1Higher: Expr[Data] = {
    val container = makeCtorResultContainer
    import container.Result

    Expr(
      Data.map(
        "functorOfResult" -> Data(Type.of[hearth.fp.Functor[Result]].plainPrint),
        "containerOfResult" -> Data(Type.of[Container[Result]].plainPrint)
      )
    )
  }

  /** Test: Type.identityCtor1Untyped returns valid UntypedType. */
  def testIdentityCtor1Untyped: Expr[Data] =
    Expr(
      Data.map(
        "identityCtor1Untyped" -> Data(UntypedType.plainPrint(Type.identityCtor1Untyped))
      )
    )

}
