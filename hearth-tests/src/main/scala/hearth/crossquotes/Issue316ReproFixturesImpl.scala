package hearth
package crossquotes

import hearth.data.Data

/** Fixtures for hearth#316: sibling `implicit lazy val`/`implicit val` `Type[_]` definitions declared in the same
  * template must not force each other during their own initialization.
  *
  * On Scala 3 the Cross-Quotes plugin injects a `casted$name` given (referencing the sibling val) into each Type
  * definition's RHS and force-evaluates it via the `hearth.fp.ignore` suppression. With two mutually-visible siblings
  * that made each one's initializer force the other, whose initializer forced back — a mutual lazy-val init deadlock
  * that hung the compiler on the lazy-val `CountDownLatch` with no diagnostic (in Chimney this hit `Unit`/`Null` and
  * `AnyType`). The fix excludes sibling template-level Type givens while initializing any one of them.
  */
trait Issue316ReproFixturesImpl { this: MacroTypedCommons =>

  implicit lazy val UnitT: Type[Unit] = Type.of[Unit]
  implicit lazy val NullT: Type[Null] = Type.of[Null]
  implicit lazy val AnyT: Type[Any] = Type.of[Any]

  def testSiblingImplicitTypes: Expr[Data] = Expr(
    Data.map(
      // Touching each lazy val runs its (plugin-generated) initializer — the deadlock point.
      "unit" -> Data(UnitT.plainPrint),
      "null" -> Data(NullT.plainPrint),
      "any" -> Data(AnyT.plainPrint),
      // The siblings must remain usable as implicits for subsequent expansions.
      "usable" -> Data(Type.of[Option[Unit]].plainPrint)
    )
  )
}
