package hearth
package crossquotes

/** The Scala 3 cross-quotes plugin turns `import x.Underlying as D` into a `given Type[D]`, which must be visible only
  * to `Type.of`s AFTER the import - injected into an earlier one it fails with `[E006] Not found: type D`.
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

  // In a class body members are visible everywhere, but an import still only from its position onwards.
  def classBodyTypeOfBeforeImport: Expr[String] = Expr(Type.of[Option[Int]].plainPrint)

  private lazy val classLevelType: ?? = Type.of[List[String]].as_??
  import classLevelType.Underlying as C

  def classBodyTypeOfAfterImport: Expr[String] = Expr(s"${Type[C].plainPrint} / ${Type.of[Option[C]].plainPrint}")
}
