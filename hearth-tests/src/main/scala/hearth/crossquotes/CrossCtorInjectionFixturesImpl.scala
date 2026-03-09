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

  /** Test: Type.of resolves types when implicit val/def Type.CtorN is declared directly in the block body.
    *
    * On Scala 3, this exercises the `.orElse(TypeCtorAppliedTypeTree.unapply(...))` branches in
    * CrossQuotesPlugin.blockGivenCandidates — specifically the `ValDef` handler (implicit val with Type.CtorN) and the
    * `DefDef` handler (implicit def with Type.CtorN). These branches handle `implicit val/def` returning
    * `Type.CtorN[HKT]` directly in the enclosing block (not via imports).
    */
  def testTypeOfWithDirectCtorValDef[A: Type]: Expr[Data] = {
    // Create ctors outside the block that uses Type.of, to avoid the infinite loop
    // from the plugin injecting a given that references the val being initialized.
    val optCtor = makeOptionCtor
    val eitherCtor = makeEitherCtor
    // Declare as implicit val/def in the block body — this triggers the plugin's
    // .orElse(TypeCtorAppliedTypeTree.unapply) branches for ValDef and DefDef.
    implicit val OptionCtor: Type.Ctor1[Option] = optCtor
    implicit def EitherCtor: Type.Ctor2[Either] = eitherCtor
    hearth.fp.ignore(OptionCtor, EitherCtor) // suppress unused warnings on Scala 2
    Expr(
      Data.map(
        "optionOfA" -> Data(Type.of[Option[A]].plainPrint),
        "eitherOfStringAndA" -> Data(Type.of[Either[String, A]].plainPrint)
      )
    )
  }

  /** Factory for Option ctor — outside the test block to avoid circular initialization. */
  protected def makeOptionCtor: Type.Ctor1[Option] = Type.Ctor1.of[Option]

  /** Factory for Either ctor — outside the test block to avoid circular initialization. */
  protected def makeEitherCtor: Type.Ctor2[Either] = Type.Ctor2.of[Either]

  /** Test: Type.of resolves types when Type.Ctor1 is provided as a context bound / implicit parameter on the enclosing
    * method.
    *
    * On Scala 3, this exercises the `CtxBoundsCtorTypeTree` extractor and the `TypeCtorAppliedTypeTree` fallback in
    * `CrossQuotesPlugin.boundGivenCandidates`. On Scala 2, it exercises `extractCtorHKT` in
    * `enclosingMethodImplicitTypes`.
    */
  def testTypeOfWithCtor1ContextBound[F[_]](implicit FC: Type.Ctor1[F]): Expr[Data] = {
    hearth.fp.ignore(FC)
    Expr(
      Data.map(
        "fOfInt" -> Data(FC.apply[Int](using Type.of[Int]).plainPrint),
        "fOfString" -> Data(FC.apply[String](using Type.of[String]).plainPrint)
      )
    )
  }

  /** Test: Expr.quote works with F from Type.Ctor1 context bound in type positions. */
  def testExprQuoteWithCtor1Param[F[_]](implicit FC: Type.Ctor1[F]): Expr[Data] = {
    hearth.fp.ignore(FC)
    Expr.quote {
      val container: Container[F] = new Container[F] {
        override def value: F[String] = null.asInstanceOf[F[String]]
      }
      Data(container.getClass.getSimpleName)
    }
  }

  /** Test: Type.of[F[Int]] works when F comes from Type.Ctor1 context bound. */
  def testTypeOfWithCtorHKTApplied[F[_]](implicit FC: Type.Ctor1[F]): Expr[Data] = {
    hearth.fp.ignore(FC)
    val fOfInt = Type.of[F[Int]]
    Expr(Data(fOfInt.plainPrint))
  }

  /** Test: Type.of[F[A]] works when F comes from Type.Ctor1 and A from Type. */
  def testTypeOfWithCtorHKTAndTypeParam[F[_], A](implicit FC: Type.Ctor1[F], A: Type[A]): Expr[Data] = {
    hearth.fp.ignore(FC)
    val fOfA = Type.of[F[A]]
    Expr(Data(fOfA.plainPrint))
  }

  /** Test: Type.of[A] inside Expr.splice inside Expr.quote, where A is a local type param. */
  def testTypeOfLocalParamInSplice: Expr[Data] =
    Expr.quote {
      def helper[A]: String = Expr.splice {
        hearth.fp.ignore(Type.of[A]) // verify Type.of[A] is obtainable for local type param
        Expr("ok")
      }
      Data(helper[Int])
    }

}
