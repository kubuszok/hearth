package hearth
package crossquotes

trait CrossByNameSpliceFixturesImpl { this: MacroCommons =>

  private val AnyType: Type[Any] = Type.of[Any]

  private def defaultOf(param: Parameter): Option[Expr[Any]] =
    if (!param.hasDefault) None
    else
      param.defaultValue.flatMap { method =>
        type EitherString[X] = Either[String, X]
        method
          .foldF[EitherString](
            onInstance = _ => Left("instance"),
            onTypes = _ => Right(Map.empty),
            onValues = _ => Right(Map.empty)
          )
          .flatten
          .toOption
          .map { ee =>
            import ee.Underlying
            implicit val AnyT: Type[Any] = AnyType
            ee.value.upcast[Any]
          }
      }

  def testDefaultsSplicedIntoStrict[A: Type]: Expr[List[Any]] = {
    implicit val ListAnyT: Type[List[Any]] = Type.of[List[Any]]
    val defaults = CaseClass.parse[A].toOption.toList.flatMap(_.primaryConstructor.parameters.flatten.toList).flatMap {
      case (_, param) => defaultOf(param).toList
    }
    defaults.foldRight(Expr.quote(List.empty[Any])) { (default, acc) =>
      Expr.quote(CrossByNameSpliceHelpers.strict(Expr.splice(default), true) :: Expr.splice(acc))
    }
  }

  def testDefaultsWithoutUpcastIntoByName[A: Type]: Expr[List[Any]] = {
    implicit val ListAnyT: Type[List[Any]] = Type.of[List[Any]]
    val defaults = CaseClass.parse[A].toOption.toList.flatMap(_.primaryConstructor.parameters.flatten.toList).flatMap {
      case (_, param) =>
        param.defaultValue.toList.flatMap { method =>
          type EitherString[X] = Either[String, X]
          method
            .foldF[EitherString](
              onInstance = _ => Left("instance"),
              onTypes = _ => Right(Map.empty),
              onValues = _ => Right(Map.empty)
            )
            .flatten
            .toOption
            .toList
        }
    }
    defaults.foldRight(Expr.quote(List.empty[Any])) { (default, acc) =>
      import default.Underlying as D
      val value: Expr[D] = default.value
      Expr.quote(CrossByNameSpliceHelpers.force(Expr.splice(value), true) :: Expr.splice(acc))
    }
  }

  def testUpcastLiteralSplicedIntoByName: Expr[List[Any]] = {
    implicit val ListAnyT: Type[List[Any]] = Type.of[List[Any]]
    implicit val AnyT: Type[Any] = AnyType
    implicit val IntT: Type[Int] = Type.of[Int]
    val default: Expr[Any] = Expr(7).upcast[Any]
    Expr.quote(List(CrossByNameSpliceHelpers.force(Expr.splice(default), true)))
  }

  def testDefaultsSplicedIntoByName[A: Type]: Expr[List[Any]] = {
    implicit val ListAnyT: Type[List[Any]] = Type.of[List[Any]]
    val defaults = CaseClass.parse[A].toOption.toList.flatMap(_.primaryConstructor.parameters.flatten.toList).flatMap {
      case (_, param) => defaultOf(param).toList
    }
    defaults.foldRight(Expr.quote(List.empty[Any])) { (default, acc) =>
      Expr.quote(CrossByNameSpliceHelpers.force(Expr.splice(default), true) :: Expr.splice(acc))
    }
  }
}

object CrossByNameSpliceHelpers {
  def strict(default: Any, enabled: Boolean): Any = if (enabled) default else null
  def force(default: => Any, enabled: Boolean): Any = if (enabled) default else null
}
