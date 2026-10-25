package hearth
package crossquotes

/** kindlings feedback (2): on Scala 3 the cross-quotes plugin's rewrite of `Type.of[...]` fails with
  * `[E006] Not found: type D` when an `import x.Underlying as D` FOLLOWS the `Type.of` in the same block.
  *
  * `importBeforeTypeOf` is the workaround (import first) and compiles on both platforms; `importAfterTypeOf` is the
  * reproducer: it compiles on Scala 2.13 but not on Scala 3.
  */
trait ImportOrderReproFixturesImpl { this: MacroCommons =>

  def importBeforeTypeOf(expr: Expr[Any]): Expr[String] = {
    val tpe: ?? = expr.tpe.as_??
    import tpe.Underlying as D
    implicit val anyType: Type[Any] = Type.of[Any]
    Expr(s"${Type[D].plainPrint} / ${Type[Any].plainPrint}")
  }

  def importAfterTypeOf(expr: Expr[Any]): Expr[String] = {
    implicit val anyType: Type[Any] = Type.of[Any]
    val tpe: ?? = expr.tpe.as_??
    import tpe.Underlying as D
    Expr(s"${Type[D].plainPrint} / ${Type[Any].plainPrint}")
  }

  // The same in a class body: a class-level import is in scope only for the members AFTER it, so `Type.of` in a member
  // BEFORE `import classLevelType.Underlying as C` must not get the `casted$C` given injected.
  def classBodyTypeOfBeforeImport: Expr[String] = Expr(Type.of[Option[Int]].plainPrint)

  private lazy val classLevelType: ?? = Type.of[List[String]].as_??
  import classLevelType.Underlying as C

  def classBodyTypeOfAfterImport: Expr[String] = Expr(s"${Type[C].plainPrint} / ${Type.of[Option[C]].plainPrint}")
}
