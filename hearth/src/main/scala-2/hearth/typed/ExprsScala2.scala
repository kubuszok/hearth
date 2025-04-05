package hearth
package typed

trait ExprsScala2 extends Exprs { this: MacroCommonsScala2 =>

  import c.universe.*

  final override type Expr[A] = c.Expr[A]

  object Expr extends ExprModule {

    object platformSpecific {

      def refineToLiteral[U: Type](literal: Type.Literal[U], tree: Tree, value: U): Expr[U] =
        if (Environment.currentScalaVersion == ScalaVersion.Scala2_12) c.Expr[U](tree)
        else {
          val aType = literal(value).as_??
          import aType.Underlying as A
          c.Expr[A](tree).asInstanceOf[Expr[U]]
        }
    }
    import platformSpecific.*

    override def prettyPrint[A](expr: Expr[A]): String =
      expr.tree
        .toString()
        // removes $macro$n from freshterms to make it easier to test and read
        .replaceAll("\\$macro", "")
        .replaceAll("\\$\\d+", "")
        // color expression for better UX (not as good as Scala 3 coloring but better than none)
        .split('\n')
        .map(line => Console.MAGENTA + line + Console.RESET)
        .mkString("\n")

    override def summonImplicit[A: Type]: Option[Expr[A]] =
      scala.util
        .Try(c.inferImplicitValue(Type[A].tpe, silent = true, withMacrosDisabled = false))
        .toOption
        .filterNot(_ == EmptyTree)
        .map(c.Expr[A](_))

    override def upcast[A: Type, B: Type](expr: Expr[A]): Expr[B] = {
      assert(
        Type[A] <:< Type[B],
        s"Upcasting can only be done to type proved to be super type! Failed ${Type.prettyPrint[A]} <:< ${Type.prettyPrint[B]} check"
      )
      if (Type[A] =:= Type[B]) expr.asInstanceOf[Expr[B]] // types are identical in practice, we can just cast
      else c.Expr[B](q"($expr : ${Type[B]})") // check A <:< B AND add a syntax to force upcasting
    }

    object BooleanExprCodec extends ExprCodec[Boolean] {
      override def toExpr(value: Boolean): Expr[Boolean] = refineToLiteral(Type.BooleanLiteral, q"$value", value)
      override def fromExpr(expr: Expr[Boolean]): Option[Boolean] = ???
    }
    object IntExprCodec extends ExprCodec[Int] {
      override def toExpr(value: Int): Expr[Int] = refineToLiteral(Type.IntLiteral, q"$value", value)
      override def fromExpr(expr: Expr[Int]): Option[Int] = ???
    }
    object LongExprCodec extends ExprCodec[Long] {
      override def toExpr(value: Long): Expr[Long] = refineToLiteral(Type.LongLiteral, q"$value", value)
      override def fromExpr(expr: Expr[Long]): Option[Long] = ???
    }
    object FloatExprCodec extends ExprCodec[Float] {
      override def toExpr(value: Float): Expr[Float] = refineToLiteral(Type.FloatLiteral, q"$value", value)
      override def fromExpr(expr: Expr[Float]): Option[Float] = ???
    }
    object DoubleExprCodec extends ExprCodec[Double] {
      override def toExpr(value: Double): Expr[Double] = refineToLiteral(Type.DoubleLiteral, q"$value", value)
      override def fromExpr(expr: Expr[Double]): Option[Double] = ???
    }
    object CharExprCodec extends ExprCodec[Char] {
      override def toExpr(value: Char): Expr[Char] = refineToLiteral(Type.CharLiteral, q"$value", value)
      override def fromExpr(expr: Expr[Char]): Option[Char] = ???
    }
    object StringExprCodec extends ExprCodec[String] {
      override def toExpr(value: String): Expr[String] = refineToLiteral(Type.StringLiteral, q"$value", value)
      override def fromExpr(expr: Expr[String]): Option[String] = ???
    }
  }
}
