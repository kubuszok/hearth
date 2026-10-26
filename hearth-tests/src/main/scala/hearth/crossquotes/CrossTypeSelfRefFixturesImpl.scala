package hearth
package crossquotes

import hearth.data.Data

/** In `implicit val ConfigT: Type[Configuration] = Type.of[Configuration]` the implicit/given lookup generated for
  * `Type.of` could resolve to the very value being defined:
  *   - Scala 2: `weakTypeTag` picks up the in-definition implicit via `convertProvidedTypesForCrossQuotes`, producing
  *     `implicit val ConfigT = ConfigT` (forward-reference error in blocks, null/infinite recursion in class bodies),
  *   - Scala 3: the plugin injects `given castedConfigT: scala.quoted.Type[...] = Type[...]...` which summons the val
  *     being initialized (forward-reference error in blocks, null at runtime in class bodies).
  *
  * So the definition being initialized has to be excluded from that search, and `Type.of`/`Type.CtorN.of` materialize
  * directly from the literal type argument instead.
  */
trait CrossTypeSelfRefFixturesImpl { this: MacroTypedCommons =>

  /** A class-level `implicit val` whose own RHS calls `Type.of` for the very type it provides - a common pattern in
    * macro extensions.
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
