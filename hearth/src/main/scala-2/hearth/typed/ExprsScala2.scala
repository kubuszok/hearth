package hearth
package typed

trait ExprsScala2 extends Exprs { this: MacroCommonsScala2 =>

  import c.universe.*

  final override type Expr[A] = c.Expr[A]

  object Expr extends ExprModule {

    object platformSpecific {

      abstract class LiteralExprCodec[U: Type: TypeCodec: Liftable] extends ExprCodec[U] {

      override def toExpr(value: U): Expr[U] = 
        if (Environment.currentScalaVersion == ScalaVersion.Scala2_12) c.Expr[U](q"$value")
        else {
          val aType = Type[U](value).as_??
          import aType.Underlying as A
          c.Expr[A](q"$value").asInstanceOf[Expr[U]]
        }
      override def fromExpr(expr: Expr[U]): Option[U] = ??? // TODO
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

    object BooleanExprCodec extends LiteralExprCodec[Boolean]
    object IntExprCodec extends LiteralExprCodec[Int]
    object LongExprCodec extends LiteralExprCodec[Long]
    object FloatExprCodec extends LiteralExprCodec[Float]
    object DoubleExprCodec extends LiteralExprCodec[Double]
    object CharExprCodec extends LiteralExprCodec[Char]
    object StringExprCodec extends LiteralExprCodec[String]
  }
}
