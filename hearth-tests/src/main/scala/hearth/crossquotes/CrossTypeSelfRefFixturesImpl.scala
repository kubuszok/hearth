package hearth
package crossquotes

import hearth.data.Data

/** Fixtures for testing [[CrossTypeSelfRefSpec]].
  *
  * Regression tests for hearth#285: `implicit val ConfigT: Type[Configuration] = Type.of[Configuration]` used to make
  * the generated implicit/given lookup resolve to the very value being defined:
  *   - Scala 2: `weakTypeTag` picked up the in-definition implicit via `convertProvidedTypesForCrossQuotes`, producing
  *     `implicit val ConfigT = ConfigT` (forward-reference error in blocks, null/infinite recursion in class bodies),
  *   - Scala 3: the plugin injected `given castedConfigT: scala.quoted.Type[...] = Type[...]...` which summoned the val
  *     being initialized (forward-reference error in blocks, null at runtime in class bodies).
  *
  * The fix excludes the definition currently being initialized from the implicit search performed by the generated
  * code, so `Type.of`/`Type.CtorN.of` fall back to direct materialization from the literal type argument.
  */
trait CrossTypeSelfRefFixturesImpl { this: MacroTypedCommons =>

  /** Mirrors the downstream `StandardMacroExtension` pattern from hearth#285: a class-level `implicit val` whose own
    * RHS calls `Type.of` for the very type the val provides.
    */
  implicit val classLevelSelfRefType: Type[examples.classes.ExampleClass] =
    Type.of[examples.classes.ExampleClass]

  def testSelfReferentialImplicitType: Expr[Data] = {
    implicit val methodLocalSelfRefType: Type[examples.classes.ExampleTrait] =
      Type.of[examples.classes.ExampleTrait]
    // A second self-referential val in the same block: its expansion must neither resolve to itself nor
    // forward-reference candidates injected for statements that follow it.
    implicit val secondLocalSelfRefType: Type[examples.classes.ExampleClassWithTypeParam[Int]] =
      Type.of[examples.classes.ExampleClassWithTypeParam[Int]]
    Expr(
      Data.map(
        "classLevelVal" -> Data(classLevelSelfRefType.plainPrint),
        "methodLocalVal" -> Data(methodLocalSelfRefType.plainPrint),
        "secondLocalVal" -> Data(secondLocalSelfRefType.plainPrint),
        // The self-referential implicit must still be a usable implicit for subsequent expansions.
        "implicitStillUsable" -> Data(Type.of[Option[examples.classes.ExampleTrait]].plainPrint)
      )
    )
  }

  def testSelfReferentialImplicitCtor: Expr[Data] = {
    implicit val selfRefOptionCtor: Type.Ctor1[Option] = Type.Ctor1.of[Option]
    Expr(
      Data.map(
        "appliedCtor" -> Data(selfRefOptionCtor.apply[Int](using Type.of[Int]).plainPrint)
      )
    )
  }
}
