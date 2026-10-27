package hearth
package crossquotes

import hearth.data.Data

trait CrossQuotesEdgeCasesScala3FixturesImpl { this: MacroCommons =>

  def testOpaqueTypes: Expr[Data] = {
    import hearth.examples.crossquotesedge.opaques.*

    val OptionCtor = Type.Ctor1.of[Option]
    val WrappedCtor = Type.Ctor1.of[Wrapped]

    def un1[F[_], X](ctor: Type.Ctor1[F], tpe: Type[X]): Data =
      Data(ctor.unapply(tpe).map(_.Underlying.plainPrint).getOrElse("<no match>"))

    Expr(
      Data.map(
        "Type.of" -> Data.map(
          "plainPrint" -> Data(Type.of[Plain].plainPrint),
          "is not the same as its representation" -> Data(Type.of[Plain] =:= Type.of[Int]),
          "is not a subtype of its representation" -> Data(Type.of[Plain] <:< Type.of[Int])
        ),
        "Ctor1 of the representation" -> Data.map(
          "unapply on the opaque type" -> un1(OptionCtor, Type.of[Wrapped[Int]])
        ),
        "Ctor1 of the opaque type" -> Data.map(
          "apply" -> Data(WrappedCtor.apply(using Type.of[String]).plainPrint),
          "unapply on the opaque type" -> un1(WrappedCtor, Type.of[Wrapped[Int]]),
          "unapply on the representation" -> un1(WrappedCtor, Type.of[Option[Int]])
        )
      )
    )
  }

  def testQuoteSpliceTypes[A: Type](value: Expr[A]): Expr[Data] = {
    def matchOnEither(either: Expr[Either[String, Int]]): Expr[String] = Expr.quote {
      Expr.splice(either) match {
        case Right(int)   => "right: " + int.toString
        case Left(string) => "left: " + string
      }
    }

    @scala.annotation.nowarn
    def shadowedInQuote[B: Type](b: Expr[B]): Expr[String] = Expr.quote {
      def describe[B](x: B): String = "described " + x.toString
      describe[String](Expr.splice(b).toString)
    }

    def blockEndingWithTypedVal: Expr[Either[String, Int]] = Expr.quote {
      val right: Either[String, Int] = Right(5)
      right
    }

    def tuplePattern(xs: Expr[List[Int]]): Expr[String] = Expr.quote {
      val (evens, odds) = Expr.splice(xs).partition(_ % 2 == 0)
      evens.sum.toString + "/" + odds.sum.toString
    }

    def namedArgsOutOfOrder(x: Expr[Int]): Expr[Int] = Expr.quote {
      def subtract(a: Int, b: Int): Int = a - b
      subtract(b = Expr.splice(x), a = 100)
    }

    def partialFunction(xs: Expr[List[Int]]): Expr[String] = Expr.quote {
      Expr.splice(xs).collect { case x if x > 1 => x * 2 }.mkString(",")
    }

    Expr.quote {
      Data.map(
        "partial function literal" -> Data(Expr.splice(partialFunction(Expr.quote(List(1, 2, 3))))),
        "named arguments out of declaration order" -> Data(Expr.splice(namedArgsOutOfOrder(Expr.quote(1)))),
        "block ending with a reference to its typed val" -> Data(Expr.splice(matchOnEither(blockEndingWithTypedVal))),
        "tuple pattern in a val definition" -> Data(Expr.splice(tuplePattern(Expr.quote(List(1, 2, 3))))),
        "spliced literal keeps the static type of its Expr" -> Data(
          Expr.splice(matchOnEither(Expr.quote[Either[String, Int]](Right(5))))
        ),
        "type parameter shadowed by a method inside the quote" -> Data(Expr.splice(shadowedInQuote(value)))
      )
    }
  }
}
