package hearth
package crossquotes

import hearth.data.Data

/** Fixtures for testing [[CrossExprsSpec]]. */
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

    // Reproduces the issue we had when trying to chain Expr.splice on a StringBuilder
    def chainingOnSplice(sb: Expr[StringBuilder], name: Expr[String]): Expr[Data] =
      Expr.quote {
        val value = Expr.splice(sb).append("<").append(Expr.splice(name)).append(">")
        Data(value.toString)
      }

    def implicitTypeSubstitution: Expr[Data] = {
      // Reproduces the issue we had when implementing StdExtensions (A was not substituted in Expr on Scala 2)
      def fromTypeParamInferred[A: Type](value: Expr[Iterable[A]]): Expr[Data] = Expr.quote {
        Data(Expr.splice(value).map(item => item.toString).mkString)
      }
      val result1 = {
        implicit val A: Type[Int] = IntType
        fromTypeParamInferred(Expr.quote[Iterable[Int]](List(1, 2, 3)))
      }

      // Reproduces the issue we had when implementing StdExtensions (A was not substituted in Expr on Scala 2)
      def fromTypeParamExplicit[A: Type](value: Expr[Iterable[A]]): Expr[Data] = Expr.quote {
        Data(Expr.splice(value).map((item: A) => item.toString).mkString)
      }
      val result2 = {
        implicit val A: Type[Int] = IntType
        fromTypeParamExplicit(Expr.quote[Iterable[Int]](List(1, 2, 3)))
      }

      trait MapType[A, Item] {

        type Key
        @typed.ImportedCrossTypeImplicit
        implicit val Key: Type[Key]

        type Value
        @typed.ImportedCrossTypeImplicit
        implicit val Value: Type[Value]

        def toIterable(item: Expr[A]): Expr[Iterable[Item]]
        def key(item: Expr[Item]): Expr[Key]
        def value(item: Expr[Item]): Expr[Value]
      }
      trait MapTypeWithResult[A, Item] extends MapType[A, Item] {
        def result: Expr[Data]
      }
      val mapTypeExample = new MapType[Map[Int, String], (Int, String)] {
        type Key = Int
        val Key = IntType
        type Value = String
        val Value = StringType

        override def toIterable(item: Expr[Map[Int, String]]): Expr[Iterable[(Int, String)]] = Expr.quote {
          Expr.splice(item).toList
        }
        override def key(item: Expr[(Int, String)]): Expr[Int] = Expr.quote {
          Expr.splice(item)._1
        }
        override def value(item: Expr[(Int, String)]): Expr[String] = Expr.quote {
          Expr.splice(item)._2
        }
      }

      def fromNestedImport[A: Type](value: Expr[A], mapType: Existential[MapType[A, *]]): Expr[Data] = {
        import mapType.{Underlying as Pair, value as mapTypeOf}
        import mapTypeOf.{Key, Value}
        Expr.quote {
          val it = Expr.splice(mapTypeOf.toIterable(value))
          Data(it.map { pair =>
            val key = Expr.splice(mapTypeOf.key(Expr.quote(pair)))
            val value = Expr.splice(mapTypeOf.value(Expr.quote(pair)))
            "key: " + key.toString + ", value: " + value.toString
          }.mkString)
        }
      }
      val result3 = fromNestedImport(
        Expr.quote(Map(1 -> "one", 2 -> "two")),
        Existential[MapType[Map[Int, String], *], (Int, String)](mapTypeExample)(using Type.of[(Int, String)])
      )(using Type.of[Map[Int, String]])

      def fromSplittedNestedImport[A: Type](value: Expr[A], mapType: Existential[MapType[A, *]]): Expr[Data] = {
        import mapType.{Underlying as Pair, value as mapTypeOf}
        import mapTypeOf.{Key, Value}
        Expr.quote {
          val it = Expr.splice(mapTypeOf.toIterable(value))
          Data(it.map { pair =>
            val key = Expr.splice(mapTypeOf.key(Expr.quote(pair)))
            val value = Expr.splice(mapTypeOf.value(Expr.quote(pair)))
            "key: " + key.toString + ", value: " + value.toString
          }.mkString)
        }
      }
      val result4 = fromSplittedNestedImport(
        Expr.quote(Map(1 -> "one", 2 -> "two")),
        Existential[MapType[Map[Int, String], *], (Int, String)](mapTypeExample)(using Type.of[(Int, String)])
      )(using Type.of[Map[Int, String]])

      // Reproduces issue #168: implicit val Key/Value defined in the same class should be found by Cross-Quotes
      def fromSameClassImplicit[A: Type](a: Expr[A], mapType: Existential[MapType[A, *]]): Expr[Data] = {
        // NO import of Key/Value - they come from the anonymous class's own implicit vals
        val wrapper: MapTypeWithResult[A, mapType.Underlying] = new MapTypeWithResult[A, mapType.Underlying] {
          override type Key = mapType.value.Key
          implicit override val Key: Type[Key] = mapType.value.Key
          override type Value = mapType.value.Value
          implicit override val Value: Type[Value] = mapType.value.Value

          override def toIterable(item: Expr[A]): Expr[Iterable[mapType.Underlying]] =
            mapType.value.toIterable(item)
          override def key(item: Expr[mapType.Underlying]): Expr[Key] = mapType.value.key(item)
          override def value(item: Expr[mapType.Underlying]): Expr[Value] = mapType.value.value(item)

          // This Expr.quote should find Key and Value from the enclosing class's implicit vals.
          // Pair is imported (as it would be in real Provider code), but Key/Value are NOT imported.
          override def result: Expr[Data] = {
            import mapType.Underlying as Pair
            Expr.quote {
              val it = Expr.splice(toIterable(a))
              Data(it.map { pair =>
                val k = Expr.splice(key(Expr.quote(pair)))
                val v = Expr.splice(value(Expr.quote(pair)))
                "key: " + k.toString + ", value: " + v.toString
              }.mkString)
            }
          }
        }
        wrapper.result
      }
      val result5 = fromSameClassImplicit(
        Expr.quote(Map(1 -> "one", 2 -> "two")),
        Existential[MapType[Map[Int, String], *], (Int, String)](mapTypeExample)(using Type.of[(Int, String)])
      )(using Type.of[Map[Int, String]])

      Expr.quote {
        Data.map(
          "fromTypeParamInferred" -> Expr.splice(result1),
          "fromTypeParamExplicit" -> Expr.splice(result2),
          "fromNestedImport" -> Expr.splice(result3),
          "fromSplittedNestedImport" -> Expr.splice(result4),
          "fromSameClassImplicit" -> Expr.splice(result5)
        )
      }
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
          "chainingOnSplice" -> Expr.splice(chainingOnSplice(Expr.quote(new StringBuilder), Expr("name"))),
          "implicitTypeSubstitution" -> Expr.splice(implicitTypeSubstitution)
        )
      )
    }
  }

  private val IntType = Type.of[Int]
  private val StringType = Type.of[String]
}
