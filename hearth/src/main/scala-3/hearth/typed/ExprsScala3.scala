package hearth
package typed

trait ExprsScala3 extends Exprs { this: MacroCommonsScala3 =>

  import quotes.*, quotes.reflect.*

  final override type Expr[A] = scala.quoted.Expr[A]

  object Expr extends ExprModule {

    object platformSpecific {

      final class ExprCodecImpl[A](using val from: scala.quoted.FromExpr[A], val to: scala.quoted.ToExpr[A])
          extends ExprCodec[A] {
        def toExpr(value: A): Expr[A] = to(value)
        def fromExpr(expr: Expr[A]): Option[A] = from.unapply(expr)
      }

      extension (self: ExprCodec.type) {

        def make[A: scala.quoted.FromExpr: scala.quoted.ToExpr]: ExprCodec[A] =
          new ExprCodecImpl[A]
      }

      extension [A](expr: Expr[A]) {
        // Required by -Xcheck-macros to pass.
        def resetOwner(using Type[A]): Expr[A] = expr.asTerm.changeOwner(Symbol.spliceOwner).asExprOf[A]
      }

      object implicits {

        given ExprCodecIsFromExpr[A: ExprCodec]: scala.quoted.FromExpr[A] = ExprCodec[A] match {
          case impl: ExprCodecImpl[A] => impl.from
          case unknown =>
            new scala.quoted.FromExpr[A] {
              override def unapply(expr: Expr[A])(using scala.quoted.Quotes): Option[A] = unknown.fromExpr(expr)
            }
        }

        given ExprCodecIsToExpr[A: ExprCodec]: scala.quoted.ToExpr[A] = ExprCodec[A] match {
          case impl: ExprCodecImpl[A] => impl.to
          case unknown =>
            new scala.quoted.ToExpr[A] {
              override def apply(value: A)(using scala.quoted.Quotes): Expr[A] = unknown.toExpr(value)
            }
        }
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

    override val BooleanExprCodec: ExprCodec[Boolean] = ExprCodec.make[Boolean]
    override val IntExprCodec: ExprCodec[Int] = ExprCodec.make[Int]
    override val LongExprCodec: ExprCodec[Long] = ExprCodec.make[Long]
    override val FloatExprCodec: ExprCodec[Float] = ExprCodec.make[Float]
    override val DoubleExprCodec: ExprCodec[Double] = ExprCodec.make[Double]
    override val CharExprCodec: ExprCodec[Char] = ExprCodec.make[Char]
    override val StringExprCodec: ExprCodec[String] = ExprCodec.make[String]
  }
}
