package hearth
package typed

trait ExprsScala3 extends Exprs { this: MacroCommonsScala3 =>

  import quotes.*, quotes.reflect.*

  final override type Expr[A] = scala.quoted.Expr[A]

  object Expr extends ExprModule {

    object platformSpecific {

      abstract class LiteralExprCodec[U: scala.quoted.FromExpr: scala.quoted.ToExpr] extends ExprCodec[U] {
        def toExpr(value: U): Expr[U] = scala.quoted.Expr(value)
        def fromExpr(expr: Expr[U]): Option[U] = scala.quoted.Expr.unapply(expr)
      }

      extension [A](expr: Expr[A]) {
        // Required by -Xcheck-macros to pass.
        def resetOwner(using Type[A]): Expr[A] = expr.asTerm.changeOwner(Symbol.spliceOwner).asExprOf[A]
      }
    }
    import platformSpecific.*

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

    object BooleanExprCodec extends LiteralExprCodec[Boolean]
    object IntExprCodec extends LiteralExprCodec[Int]
    object LongExprCodec extends LiteralExprCodec[Long]
    object FloatExprCodec extends LiteralExprCodec[Float]
    object DoubleExprCodec extends LiteralExprCodec[Double]
    object CharExprCodec extends LiteralExprCodec[Char]
    object StringExprCodec extends LiteralExprCodec[String]
  }
}
