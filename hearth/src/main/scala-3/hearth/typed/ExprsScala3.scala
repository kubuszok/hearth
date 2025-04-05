package hearth
package typed

trait ExprsScala3 extends Exprs { this: MacroCommonsScala3 =>

  import quotes.*, quotes.reflect.*

  final override type Expr[A] = scala.quoted.Expr[A]

  object Expr extends ExprModule {

    object platformSpecific {

      extension [A](expr: Expr[A]) {
        // Required by -Xcheck-macros to pass.
        def resetOwner(using Type[A]): Expr[A] = expr.asTerm.changeOwner(Symbol.spliceOwner).asExprOf[A]
      }
    }
    import platformSpecific.resetOwner

    def prettyPrint[A](expr: Expr[A]): String =
      expr.asTerm
        .show(using Printer.TreeAnsiCode)
        // remove $macro$n from freshterms to make it easier to test and read
        .replaceAll("\\$macro", "")
        .replaceAll("\\$\\d+", "")

    def summonImplicit[A: Type]: Option[Expr[A]] = scala.quoted.Expr.summon[A]

    def upcast[A: Type, B: Type](expr: Expr[A]): Expr[B] = {
      Predef.assert(
        Type[A] <:< Type[B],
        s"Upcasting can only be done to type proved to be super type! Failed ${Type.prettyPrint[A]} <:< ${Type.prettyPrint[B]} check"
      )
      expr.asInstanceOf[Expr[B]] // check that A <:< B without upcasting in code (Scala 3 should get away without it)
    }

    object BooleanExprCodec extends ExprCodec[Boolean] {
      def toExpr(value: Boolean): Expr[Boolean] = scala.quoted.Expr(value)
      def fromExpr(expr: Expr[Boolean]): Option[Boolean] = ???
    }
    object IntExprCodec extends ExprCodec[Int] {
      def toExpr(value: Int): Expr[Int] = scala.quoted.Expr(value)
      def fromExpr(expr: Expr[Int]): Option[Int] = ???
    }
    object LongExprCodec extends ExprCodec[Long] {
      def toExpr(value: Long): Expr[Long] = scala.quoted.Expr(value)
      def fromExpr(expr: Expr[Long]): Option[Long] = ???
    }
    object FloatExprCodec extends ExprCodec[Float] {
      def toExpr(value: Float): Expr[Float] = scala.quoted.Expr(value)
      def fromExpr(expr: Expr[Float]): Option[Float] = ???
    }
    object DoubleExprCodec extends ExprCodec[Double] {
      def toExpr(value: Double): Expr[Double] = scala.quoted.Expr(value)
      def fromExpr(expr: Expr[Double]): Option[Double] = ???
    }
    object CharExprCodec extends ExprCodec[Char] {
      def toExpr(value: Char): Expr[Char] = scala.quoted.Expr(value)
      def fromExpr(expr: Expr[Char]): Option[Char] = ???
    }
    object StringExprCodec extends ExprCodec[String] {
      def toExpr(value: String): Expr[String] = scala.quoted.Expr(value)
      def fromExpr(expr: Expr[String]): Option[String] = ???
    }
  }
}
