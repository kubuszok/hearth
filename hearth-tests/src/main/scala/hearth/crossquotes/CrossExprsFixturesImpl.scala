package hearth
package crossquotes

import hearth.data.Data

/** Fixtured for testing [[CrossExprsSpec]]. */
trait CrossExprsFixturesImpl { this: MacroTypedCommons =>

  // TODO: replicate test cases from testTypeOf from CrossTypesFixturesImpl

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

    // TODO: test with multiple approaches to passing A like in Types spec
    // we can use generic expressions in a single Expr.quote as long as there is an implicit Type[A] in the scope
    def generics[A: Type](e: Expr[A]): Expr[Data] = Expr.quote {
      Data(Expr.splice(e).toString)
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

    @scala.annotation.nowarn
    def chainingOnSplice(sb: Expr[StringBuilder], name: Expr[String]): Expr[Data] =
      Expr.quote {
        val value = Expr.splice(sb).append("<").append(Expr.splice(name)).append(">")
        Data(value.toString)
      }
    @scala.annotation.nowarn
    val sbt = Type.of[StringBuilder]
    @scala.annotation.nowarn
    val sb = {
      implicit val sb: Type[StringBuilder] = sbt
      Expr.quote(new StringBuilder)
    }

    Expr.quote {
      Data.map(
        "features" -> Data.map(
          "unsanitizedBlocks" -> Expr.splice(unsanitized),
          "multipleSplices" -> Expr.splice(multipleSplices),
          "generics" -> Expr.splice(generics(example)),
          "nestedQuotesAndSplices" -> Expr.splice(nestedSplices)
        ),
        "edgeCases" -> Data.map(
          // "chainingOnSplice" -> Data("<name>")
          "chainingOnSplice" -> Expr.splice(chainingOnSplice(Expr.quote(new StringBuilder), Expr("name")))
        )
      )
    }
  }
}
