package hearth
package typed

/** Fixtured for testing [[CrossQuotesSpec]]. */
trait CrossQuotesFixturesImpl { this: MacroTypedCommons =>

  def simpleType: Expr[String] = Expr(Type.of[Int].prettyPrint)

  def genericType[A: Type]: Expr[String] = Expr(Type.of[scala.collection.immutable.List[A]].prettyPrint)

  def unsanitizedType: Expr[String] = {
    import scala.collection.immutable.ListMap
    Expr(Type.of[ListMap[Int, String]].prettyPrint)
  }

  def unsanitizedExpr: Expr[String] = {
    import scala.collection.immutable.ListMap
    Expr.quote {
      ListMap(1 -> "2").toString
    }
  }

  def simpleExpr: Expr[String] = {
    val e1 = Expr.quote(1)
    val e2 = Expr.quote(2)

    Expr.quote {
      val a = Expr.splice(e1) + Expr.splice(e2)
      a.toString
    }
  }

  def genericExpr[A: Type](e: Expr[A]): Expr[String] = Expr.quote {
    Expr.splice(e).toString
  }

  def nestedExpr: Expr[String] = {
    def intToString(i: Expr[Int]): Expr[String] = Expr.quote {
      Expr.splice(i).toString
    }

    Expr.quote {
      def localMethod(i: Int): String = Expr.splice(intToString(Expr.quote(i)))
      localMethod(42)
    }
  }
}
