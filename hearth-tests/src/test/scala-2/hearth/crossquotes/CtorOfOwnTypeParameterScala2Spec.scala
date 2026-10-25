package hearth
package crossquotes

/** Scala 2 only: `Type.CtorN.of[F]` / `Type.CtorK1.of[F]` where `F` is a type parameter of the enclosing macro method
  * picks up an implicit `WeakTypeTag[F[Any]]` of that method (see `TypesSpec` for the positive cases) - and without one
  * it fails with an actionable error instead of silently building a constructor of the abstract `F` (or crashing).
  */
final class CtorOfOwnTypeParameterScala2Spec extends MacroSuite {

  group("Cross-Quotes (Scala 2): Type.CtorN.of / Type.CtorK1.of on the macro method's own type parameter") {

    test("Type.Ctor1.of[F] without an implicit WeakTypeTag[F[Any]] explains how to provide F") {
      compileErrors(
        """
        import scala.reflect.macros.blackbox
        final class Bundle(val c: blackbox.Context) extends hearth.MacroCommonsScala2 {
          def impl[F[_]]: c.Tree = { Type.Ctor1.of[F]; c.universe.EmptyTree }
        }
        """
      ).check(
        "Type.Ctor1.of[F]: `F` is a type parameter of `impl`, so the type constructor it stands for is unknown here.",
        "`c.WeakTypeTag[F[Any]]` (or `Type[F[Any]]`) parameter - add one to `impl` and it will be used here,",
        "`Type.Ctor1.fromUntyped[F](...)`",
        "(`implicit F: Type.Ctor1[F]`)"
      )
    }

    test("Type.CtorK1.of[Alg] without an implicit WeakTypeTag[Alg[Option]] explains how to provide Alg") {
      compileErrors(
        """
        import scala.reflect.macros.blackbox
        final class Bundle(val c: blackbox.Context) extends hearth.MacroCommonsScala2 {
          def impl[Alg[_[_]]]: c.Tree = { Type.CtorK1.of[Alg]; c.universe.EmptyTree }
        }
        """
      ).check(
        "Type.CtorK1.of[Alg]: `Alg` is a type parameter of `impl`",
        "`c.WeakTypeTag[Alg[Option]]` (or `Type[Alg[Option]]`)"
      )
    }
  }
}
