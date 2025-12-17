package hearth
package crossquotes

import hearth.data.Data

/** Fixtured for testing [[CrossExprsSpec]]. */
trait CrossExprsFixturesImpl { this: MacroTypedCommons =>

  def testExprOf[ExampleType: Type](example: Expr[ExampleType]): Expr[Data] = {
    // no FCQN using _root_.package.name.Class, etc
    def unsanitized: Expr[Data] = {
      import scala.collection.immutable.ListMap
      Expr.quote {
        Data(ListMap(1 -> "2").toString)
      }
    }

    // we can use multiple splices in a single Expr.quote block
    def multipleSplices: Expr[Data] = {
      val e1 = Expr.quote(1)
      val e2 = Expr.quote(2)

      Expr.quote {
        val a = Expr.splice(e1) + Expr.splice(e2)
        Data(a.toString)
      }
    }

    // we can use generic expressions in a single Expr.quote as long as there is an implicit Type[A] in the scope
    @scala.annotation.nowarn
    def generics[A: Type](e: Expr[A]): Expr[Data] = {
      val viaTypeBound = {
        def body[B: Type](expr: Expr[B]) = Expr.quote {
          Data(Expr.splice(expr).toString)
        }
        body[A](e)
      }
      val viaImplicitParam = {
        def body[B](expr: Expr[B])(implicit B: Type[B]) = Expr.quote {
          Data(Expr.splice(expr).toString)
        }
        body[A](e)
      }
      val viaImport = {
        def body(b: Expr_??) = {
          import b.{Underlying as B, value as expr}
          Expr.quote {
            Data(Expr.splice(expr).toString)
          }
        }
        body(e.as_??)
      }
      val viaDeclaredVal = {
        def body[B](tpe: Type[B], expr: Expr[B]) = {
          implicit val B: Type[B] = tpe
          Expr.quote {
            Data(Expr.splice(expr).toString)
          }
        }
        body(Type[A], e)
      }

      Expr.quote {
        Data.map(
          "viaTypeBound" -> Expr.splice(viaTypeBound),
          "viaImplicitParam" -> Expr.splice(viaImplicitParam),
          "viaImport" -> Expr.splice(viaImport),
          "viaDeclaredVal" -> Expr.splice(viaDeclaredVal)
        )
      }
    }

    def nestedSplices: Expr[Data] = {
      def intToString(i: Expr[Int]): Expr[String] = Expr.quote {
        Expr.splice(i).toString
      }

      Expr.quote {
        def localMethod(i: Int): String = Expr.splice(intToString(Expr.quote(i)))
        Data(localMethod(42))
      }
    }

    // TODO: edge cases

    def chainingOnSplice(sb: Expr[StringBuilder], name: Expr[String]): Expr[Data] =
      Expr.quote {
        val value = Expr.splice(sb).append("<").append(Expr.splice(name)).append(">")
        Data(value.toString)
      }

    Expr.quote {
      Data.map(
        "features" -> Data.map(
          "unsanitizedBlocks" -> Expr.splice(unsanitized),
          "multipleSplices" -> Expr.splice(multipleSplices),
          "genericsAndImplicitlyPassedTypes" -> Expr.splice(generics(example)),
          "nestedQuotesAndSplices" -> Expr.splice(nestedSplices)
        ),
        "edgeCases" -> Data.map(
          "chainingOnSplice" -> Expr.splice(chainingOnSplice(Expr.quote(new StringBuilder), Expr("name")))
        )
      )
    }
  }
}
