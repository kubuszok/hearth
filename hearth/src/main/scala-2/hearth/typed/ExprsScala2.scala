package hearth
package typed

trait ExprsScala2 extends Exprs { this: MacroCommonsScala2 =>

  import c.universe.*

  final override type Expr[A] = c.Expr[A]

  object Expr extends ExprModule {

    object platformSpecific {

      final class ExprCodecImpl[A](typeCodec: Option[TypeCodec[A]])(implicit
          val to: Liftable[A],
          val from: Unliftable[A]
      ) extends ExprCodec[A] {

        override def toExpr(value: A): Expr[A] = typeCodec match {
          case Some(codec) if Environment.currentScalaVersion != ScalaVersion.Scala2_12 =>
            val aType = codec.toType(value).as_??
            import aType.Underlying as B
            c.Expr[B](to.apply(value)).asInstanceOf[Expr[A]]
          case _ => c.Expr[A](to.apply(value))
        }

        override def fromExpr(expr: Expr[A]): Option[A] = from.unapply(expr.tree)
      }

      implicit class ExprCodecCompanionOps(private val self: ExprCodec.type) {

        def make[A: Liftable: Unliftable]: ExprCodec[A] =
          new ExprCodecImpl[A](None)

        def withTypeCodec[A: Liftable: Unliftable: TypeCodec]: ExprCodec[A] =
          new ExprCodecImpl[A](Some(TypeCodec[A]))
      }

      object implicits {

        implicit def ExprCodecLiftable[A: ExprCodec]: Liftable[A] = ExprCodec[A] match {
          case impl: ExprCodecImpl[A] => impl.to
          case unknown                => value => unknown.toExpr(value).tree
        }

        implicit def ExprCodecUnliftable[A: ExprCodec]: Unliftable[A] = ExprCodec[A] match {
          case impl: ExprCodecImpl[A] => impl.from
          case unknown                => tree => unknown.fromExpr(c.Expr[A](tree))
        }
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

    override val BooleanExprCodec: ExprCodec[Boolean] = ExprCodec.withTypeCodec[Boolean]
    override val IntExprCodec: ExprCodec[Int] = ExprCodec.withTypeCodec[Int]
    override val LongExprCodec: ExprCodec[Long] = ExprCodec.withTypeCodec[Long]
    override val FloatExprCodec: ExprCodec[Float] = ExprCodec.withTypeCodec[Float]
    override val DoubleExprCodec: ExprCodec[Double] = ExprCodec.withTypeCodec[Double]
    override val CharExprCodec: ExprCodec[Char] = ExprCodec.withTypeCodec[Char]
    override val StringExprCodec: ExprCodec[String] = ExprCodec.withTypeCodec[String]
  }
}
