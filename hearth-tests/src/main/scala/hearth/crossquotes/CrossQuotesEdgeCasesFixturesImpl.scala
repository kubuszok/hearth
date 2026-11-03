package hearth
package crossquotes

import hearth.data.Data

trait CrossQuotesEdgeCasesFixturesImpl { this: MacroCommons =>

  private val EdgeIntType = Type.of[Int]
  private val EdgeStringType = Type.of[String]
  private val EdgeDoubleType = Type.of[Double]

  trait EdgeTypeHolder {
    type Key
    implicit val Key: Type[Key]
    def listOfKey: String = Type.of[List[Key]].plainPrint
  }

  private def printedOrNoMatch(result: Option[String]): Data = Data(result.getOrElse("<no match>"))

  def testTypeOfEdgeCases[A: Type]: Expr[Data] = {
    import hearth.examples.crossquotesedge.*

    val holder: EdgeTypeHolder = new EdgeTypeHolder {
      type Key = Int
      val Key = EdgeIntType
    }

    Expr(
      Data.map(
        "special types" -> Data.map(
          "Nothing" -> Data(Type.plainPrint[Nothing](using Type.of[Nothing])),
          "Null" -> Data(Type.of[Null].plainPrint),
          "Any" -> Data(Type.of[Any].plainPrint),
          "AnyRef" -> Data(Type.of[AnyRef].plainPrint),
          "AnyVal" -> Data(Type.of[AnyVal].plainPrint),
          "Unit" -> Data(Type.of[Unit].plainPrint)
        ),
        "singleton types" -> Data.map(
          "string literal" -> Data(Type.of["foo"].plainPrint),
          "int literal" -> Data(Type.of[42].plainPrint),
          "object" -> Data(Type.of[SingletonObject.type].plainPrint),
          "object is not widened" -> Data(Type.of[SingletonObject.type] =:= Type.of[AnyRef])
        ),
        "applied to the type parameter" -> Data.map(
          "array" -> Data(Type.of[Array[A]].plainPrint),
          "function" -> Data(Type.of[A => String].plainPrint),
          "tuple" -> Data(Type.of[(A, String)].plainPrint),
          "nested" -> Data(Type.of[Map[String, List[Option[A]]]].plainPrint)
        ),
        "compound type" -> Data.map(
          "is subtype of the first component" -> Data(Type.of[MarkerA with MarkerB] <:< Type.of[MarkerA]),
          "is subtype of the second component" -> Data(Type.of[MarkerA with MarkerB] <:< Type.of[MarkerB]),
          "component is not a subtype of the compound" -> Data(Type.of[MarkerA] <:< Type.of[MarkerA with MarkerB])
        ),
        "path-dependent types" -> Data.map(
          "selected through a stable path" -> Data(Type.of[PathDependent.first.Inner].plainPrint),
          "different prefixes are different types" -> Data(
            Type.of[PathDependent.first.Inner] =:= Type.of[PathDependent.second.Inner]
          ),
          "type projection" -> Data(Type.of[Outer#Inner].plainPrint),
          "path-dependent type is a subtype of the projection" -> Data(
            Type.of[PathDependent.first.Inner] <:< Type.of[Outer#Inner]
          )
        ),
        "type aliases" -> Data.map(
          "alias is the same as the aliased type" -> Data(Type.of[aliases.IntToString] =:= Type.of[Int => String]),
          "applied alias is the same as the aliased type" -> Data(
            Type.of[aliases.StringEither[A]] =:= Type.of[Either[String, A]]
          )
        ),
        "implicit Types in scope" -> Data.map(
          "nested method with its own context bound" -> Data {
            def inner[C: Type]: String = Type.of[Option[C]].plainPrint
            inner[String](using EdgeStringType)
          },
          "two type parameters" -> Data {
            def body[B: Type, C: Type]: String = Type.of[Either[B, C]].plainPrint
            body[A, String](using Type[A], EdgeStringType)
          },
          "import from an upper-bounded existential" -> Data {
            def body(b: ??<:[AnyVal]): String = {
              import b.Underlying as B
              Type.of[List[B]].plainPrint
            }
            body(EdgeIntType.as_??<:[AnyVal])
          },
          "abstract type member with an implicit Type in the same class" -> Data(holder.listOfKey),
          "imported abstract type member with an implicit Type" -> Data {
            import holder.Key
            Type.of[Option[Key]].plainPrint
          }
        )
      )
    )
  }

  def testCtorEdgeCases: Expr[Data] = {
    import hearth.examples.crossquotesedge.*
    import hearth.examples.kinds.{Arity2, IntPredicate}

    val OptionCtor = Type.Ctor1.of[Option]
    val SeqCtor = Type.Ctor1.of[Seq]
    val IterableCtor = Type.Ctor1.of[Iterable]
    val ComparableCtor = Type.Ctor1.of[java.lang.Comparable]
    val EitherCtor = Type.Ctor2.of[Either]
    val Function1Ctor = Type.Ctor2.of[Function1]
    val Arity2Ctor = Type.Ctor2.of[Arity2]
    val SwappingCtor = Type.Ctor2.of[SwappingSub]
    val FirstInnerCtor = Type.Ctor1.of[PathDependent.first.InnerGeneric]

    def un1[F[_], X](ctor: Type.Ctor1[F], tpe: Type[X]): Data =
      printedOrNoMatch(ctor.unapply(tpe).map(_.Underlying.plainPrint))
    def un2[F[_, _], X](ctor: Type.Ctor2[F], tpe: Type[X]): Data =
      printedOrNoMatch(ctor.unapply(tpe).map { case (a, b) =>
        a.Underlying.plainPrint + ", " + b.Underlying.plainPrint
      })

    Expr(
      Data.map(
        "unapply on subtypes" -> Data.map(
          "Option on Some[Int]" -> un1(OptionCtor, Type.of[Some[Int]]),
          "Option on None.type" -> un1(OptionCtor, Type.of[None.type]),
          "Seq on List[Int]" -> un1(SeqCtor, Type.of[List[Int]]),
          "Iterable on Map[String, Int]" -> un1(IterableCtor, Type.of[Map[String, Int]]),
          "Comparable on String" -> un1(ComparableCtor, Type.of[String]),
          "Either on Right[String, Int]" -> un2(EitherCtor, Type.of[Right[String, Int]]),
          "Function1 on a class extending a function" -> un2(Function1Ctor, Type.of[IntPredicate]),
          "Arity2 on a subtype fixing an argument" -> un2(Arity2Ctor, Type.of[FixingSub[String]]),
          "Arity2 on a subtype swapping arguments" -> un2(Arity2Ctor, Type.of[SwappingSub[Int, String]]),
          "subtype constructor on its supertype" -> un2(SwappingCtor, Type.of[Arity2[Int, String]])
        ),
        "unapply on aliases" -> Data.map(
          "Option on an alias without type parameters" -> un1(OptionCtor, Type.of[aliases.IntOption]),
          "Either on an applied alias" -> un2(EitherCtor, Type.of[aliases.StringEither[Int]]),
          "Function1 on an alias of a function" -> un2(Function1Ctor, Type.of[aliases.IntToString])
        ),
        "unapply on special types" -> Data.map(
          "Option on Nothing" -> un1(OptionCtor, Type.of[Nothing]),
          "Option on Null" -> un1(OptionCtor, Type.of[Null]),
          "Option on Any" -> un1(OptionCtor, Type.of[Any])
        ),
        "apply" -> Data.map(
          "Option of Nothing" -> Data(OptionCtor.apply[Nothing](using Type.of[Nothing]).plainPrint),
          "Option of an applied type" -> Data(OptionCtor.apply[List[Int]](using Type.of[List[Int]]).plainPrint),
          "Option of a literal type, unapplied back" -> un1(OptionCtor, OptionCtor.apply(using Type.of["x"]))
        ),
        "path-dependent constructor" -> Data.map(
          "apply" -> Data(FirstInnerCtor.apply(using EdgeIntType).plainPrint),
          "unapply with the same prefix" -> un1(FirstInnerCtor, Type.of[PathDependent.first.InnerGeneric[Int]]),
          "unapply with a different prefix" -> un1(FirstInnerCtor, Type.of[PathDependent.second.InnerGeneric[Int]])
        )
      )
    )
  }

  def testBoundedCtorClaims: Expr[Data] = {
    import hearth.examples.crossquotesedge.*

    val UpperBoundedOption = Type.Ctor1.UpperBounded.of[String, Option]
    val BoundedOption = Type.Ctor1.Bounded.of[Nothing, String, Option]
    val KennelCtor = Type.Ctor1.Bounded.of[Puppy, Animal, Kennel]

    Expr(
      Data.map(
        "UpperBounded" -> Data.map(
          "apply" -> Data(UpperBoundedOption.apply(using EdgeStringType).plainPrint),
          "unapply within bounds" -> printedOrNoMatch(
            UpperBoundedOption.unapply(Type.of[Option[String]]).map(_.Underlying.plainPrint)
          ),
          "unapply outside of bounds" -> printedOrNoMatch(
            UpperBoundedOption.unapply(Type.of[Option[Int]]).map(_.Underlying.plainPrint)
          )
        ),
        "Bounded" -> Data.map(
          "apply" -> Data(BoundedOption.apply(using EdgeStringType).plainPrint),
          "unapply within bounds" -> printedOrNoMatch(
            BoundedOption.unapply(Type.of[Option[String]]).map(_.Underlying.plainPrint)
          ),
          "unapply outside of bounds" -> printedOrNoMatch(
            BoundedOption.unapply(Type.of[Option[Int]]).map(_.Underlying.plainPrint)
          )
        ),
        "Bounded with a constructor bounded both ways" -> Data.map(
          "apply" -> Data(KennelCtor.apply(using Type.of[Dog]).plainPrint),
          "unapply at the lower bound" -> printedOrNoMatch(
            KennelCtor.unapply(Type.of[Kennel[Puppy]]).map(_.Underlying.plainPrint)
          ),
          "unapply between the bounds" -> printedOrNoMatch(
            KennelCtor.unapply(Type.of[Kennel[Dog]]).map(_.Underlying.plainPrint)
          ),
          "unapply at the upper bound" -> printedOrNoMatch(
            KennelCtor.unapply(Type.of[Kennel[Animal]]).map(_.Underlying.plainPrint)
          )
        )
      )
    )
  }

  def testPartialApplicationClaims: Expr[Data] = {
    import hearth.examples.kinds.Arity3

    val Arity3Ctor = Type.Ctor3.of[Arity3]
    val appliedFromBack =
      Arity3Ctor.setC(using EdgeDoubleType).setB(using EdgeStringType).setA(using EdgeIntType)
    val appliedFromFront =
      Arity3Ctor.setA(using EdgeIntType).setA(using EdgeStringType).setA(using EdgeDoubleType)
    val firstLeft = Arity3Ctor.setC(using EdgeDoubleType).setB(using EdgeStringType)

    val EitherWithString = Type.Ctor2.of[Either].setA(using EdgeStringType)
    val EitherWithInt = Type.Ctor2.of[Either].setB(using EdgeIntType)

    def isFixedEither[In](In: Type[In]): Data = Data(EitherWithString.unapply(In).isDefined)

    Expr(
      Data.map(
        "applying all parameters" -> Data.map(
          "from the back" -> Data(appliedFromBack.plainPrint),
          "from the front" -> Data(appliedFromFront.plainPrint)
        ),
        "constructor with the first parameter left" -> Data.map(
          "apply" -> Data(firstLeft.apply(using Type.of[Boolean]).plainPrint),
          "unapply matching fixed parameters" -> printedOrNoMatch(
            firstLeft.unapply(Type.of[Arity3[Int, String, Double]]).map(_.Underlying.plainPrint)
          ),
          "unapply not matching fixed parameters" -> printedOrNoMatch(
            firstLeft.unapply(Type.of[Arity3[Int, Int, Double]]).map(_.Underlying.plainPrint)
          )
        ),
        "Either with the first parameter fixed" -> Data.map(
          "Either[String, Int]" -> isFixedEither(Type.of[Either[String, Int]]),
          "Either[String, Double]" -> isFixedEither(Type.of[Either[String, Double]]),
          "Either[Int, String]" -> isFixedEither(Type.of[Either[Int, String]]),
          "Option[String]" -> isFixedEither(Type.of[Option[String]]),
          "Right[String, Int]" -> printedOrNoMatch(
            EitherWithString.unapply(Type.of[Right[String, Int]]).map(_.Underlying.plainPrint)
          )
        ),
        "Either with the last parameter fixed" -> Data.map(
          "Either[String, Int]" -> printedOrNoMatch(
            EitherWithInt.unapply(Type.of[Either[String, Int]]).map(_.Underlying.plainPrint)
          ),
          "Either[String, String]" -> printedOrNoMatch(
            EitherWithInt.unapply(Type.of[Either[String, String]]).map(_.Underlying.plainPrint)
          )
        )
      )
    )
  }

  def testQuoteEdgeCases: Expr[Data] = {
    def matchOnEither(either: Expr[Either[String, Int]]): Expr[String] = {
      def rightCase(right: Expr[Int]): Expr[String] = Expr.quote("right: " + Expr.splice(right).toString)
      def leftCase(left: Expr[String]): Expr[String] = Expr.quote("left: " + Expr.splice(left))
      Expr.quote {
        Expr.splice(either) match {
          case Right(int)   => Expr.splice(rightCase(Expr.quote(int)))
          case Left(string) => Expr.splice(leftCase(Expr.quote(string)))
        }
      }
    }

    def interpolation(name: Expr[String], n: Expr[Int]): Expr[String] = Expr.quote {
      s"name=${Expr.splice(name)}, n=${Expr.splice(n) + 1}"
    }

    def definitions: Expr[Int] = Expr.quote {
      val a = Expr.splice(Expr.quote(1))
      var b = Expr.splice(Expr.quote(2))
      lazy val c = Expr.splice(Expr.quote(3))
      def d = Expr.splice(Expr.quote(4))
      b += 10
      a + b + c + d
    }

    def lambdas: Expr[String] = {
      def plusOne(a: Expr[Int]): Expr[Int] = Expr.quote(Expr.splice(a) + 1)
      def concat(s: Expr[String], i: Expr[Int]): Expr[String] = Expr.quote(Expr.splice(s) + Expr.splice(i).toString)
      Expr.quote {
        val f = (a: Int) => Expr.splice(plusOne(Expr.quote(a)))
        val g = (a: Int, b: String) => Expr.splice(concat(Expr.quote(b), Expr.quote(a)))
        f(1).toString + g(2, "x")
      }
    }

    def controlFlow(limit: Expr[Int]): Expr[String] = {
      def failing: Expr[String] = Expr.quote(Integer.parseInt("not a number").toString)
      Expr.quote {
        var i = 0
        var acc = 0
        while (i < Expr.splice(limit)) {
          acc += i
          i += 1
        }
        val recovered =
          try Expr.splice(failing)
          catch { case _: NumberFormatException => "recovered" }
        if (acc > 5) s"big:$acc:$recovered" else s"small:$acc:$recovered"
      }
    }

    def literals: Expr[String] = Expr.quote {
      val long = 9223372036854775807L
      val minInt = -2147483648
      val char = 'c'
      val float = 1.5f
      val double = 2.25
      val str = "quote\" dollar$ backslash\\"
      val nul: String = null
      val `back ticked` = "bt"
      s"$long|$minInt|$char|$float|$double|$str|${nul == null}|${`back ticked`}|$$"
    }

    def defaultArgs(x: Expr[Int]): Expr[Int] = Expr.quote {
      def subtract(a: Int, b: Int = 10): Int = a - b
      subtract(100, Expr.splice(x)) + subtract(5)
    }

    def implicitsInQuote: Expr[String] = Expr.quote {
      implicit val reversed: Ordering[Int] = Ordering.Int.reverse
      List(3, 1, 2).sorted.mkString(",")
    }

    def collections(xs: Expr[List[Int]]): Expr[String] = Expr.quote {
      val collected = Expr.splice(xs).filter(_ > 1).map(_ * 2)
      val pairs = for {
        a <- List(1, 2)
        b <- Expr.splice(xs)
        if b != a
      } yield a * 10 + b
      val evensAndOdds = Expr.splice(xs).partition(_ % 2 == 0)
      collected.mkString(",") + "|" + pairs.mkString(",") + "|" + evensAndOdds._1.sum.toString + "/" +
        evensAndOdds._2.sum.toString
    }

    def byNameAndFunctionApplication(x: Expr[Int], f: Expr[Int => Int]): Expr[Int] = Expr.quote {
      Option.empty[Int].getOrElse(Expr.splice(x)) + Expr.splice(f)(3)
    }

    def varargs: Expr[Int] = {
      implicit val intType: Type[Int] = EdgeIntType
      Expr.quote(List(Expr.splice(VarArgs(Expr(1), Expr(2), Expr(3)))*).sum)
    }

    def unitSplice: Expr[String] = {
      def unit: Expr[Unit] = Expr.quote(())
      Expr.quote {
        Expr.splice(unit)
        "after unit"
      }
    }

    Expr.quote {
      Data.map(
        "match with splices in cases" -> Data.map(
          "right" -> Data(Expr.splice(matchOnEither(Expr.quote(Option(5).toRight("five"))))),
          "left" -> Data(Expr.splice(matchOnEither(Expr.quote(Option.empty[Int].toRight("five")))))
        ),
        "splices in string interpolation" -> Data(Expr.splice(interpolation(Expr("hearth"), Expr.quote(41)))),
        "splices as val, var, lazy val and def bodies" -> Data(Expr.splice(definitions)),
        "splices as lambda bodies" -> Data(Expr.splice(lambdas)),
        "while, try-catch and if-else" -> Data(Expr.splice(controlFlow(Expr.quote(5)))),
        "literals and back-ticked names" -> Data(Expr.splice(literals)),
        "default arguments" -> Data(Expr.splice(defaultArgs(Expr.quote(1)))),
        "implicits defined inside the quote" -> Data(Expr.splice(implicitsInQuote)),
        "higher-order functions and for-comprehensions" -> Data(
          Expr.splice(collections(Expr.quote(List(1, 2, 3))))
        ),
        "by-name argument and function application" -> Data(
          Expr.splice(byNameAndFunctionApplication(Expr.quote(4), Expr.quote((i: Int) => i * 7)))
        ),
        "varargs splice" -> Data(Expr.splice(varargs)),
        "Unit splice as a statement" -> Data(Expr.splice(unitSplice))
      )
    }
  }

  def testQuoteGenerics[A: Type](value: Expr[A]): Expr[Data] = {
    def buildList[B: Type](elements: List[Expr[B]]): Expr[List[B]] = elements match {
      case Nil          => Expr.quote(List.empty[B])
      case head :: tail => Expr.quote(Expr.splice(head) :: Expr.splice(buildList(tail)))
    }

    def cast[B: Type](any: Expr[Any]): Expr[B] = Expr.quote(Expr.splice(any).asInstanceOf[B])

    def wrapTwice[B: Type](b: Expr[B]): Expr[List[Option[B]]] =
      Expr.quote(List(Option(Expr.splice(b)), Some(Expr.splice(b))))

    def ascribed[B: Type](b: Expr[B]): Expr[Option[B]] = Expr.quote[Option[B]](Some(Expr.splice(b)))

    def summonedInQuote[B: Type](b: Expr[B]): Expr[String] = Expr.quote {
      implicitly[Ordering[String]].compare(Expr.splice(b).toString, Expr.splice(b).toString).toString
    }

    Expr.quote {
      Data.map(
        "recursive list building" -> Data(
          Expr.splice(buildList(List(value, value, value))).mkString(",")
        ),
        "asInstanceOf to the type parameter" -> Data(
          Expr.splice(cast[A](Expr.quote(Expr.splice(value): Any))).toString
        ),
        "type parameter in nested applied types" -> Data(Expr.splice(wrapTwice(value)).toString),
        "explicitly typed quote" -> Data(Expr.splice(ascribed(value)).toString),
        "implicit summoned inside the quote" -> Data(Expr.splice(summonedInQuote(value)))
      )
    }
  }
}
