package hearth
package crossquotes

import hearth.data.Data

trait CrossCtorInjectionFixturesImpl extends CrossCtorInjectionFixturesImplGen { this: MacroCommons =>

  def testTypeOfWithImportedCtor1[A: Type]: Expr[Data] = {
    // Mimics CtorLikeOf: the cross-quotes plugin only picks up an imported Type.Ctor1 when its name is one it
    // recognizes, and "Result" is one of them.
    val container = makeCtorResultContainer
    import container.Result

    Expr(
      Data.map(
        "resultOfInt" -> Data(Type.of[Result[Int]].plainPrint),
        "resultOfString" -> Data(Type.of[Result[String]].plainPrint),
        "resultOfA" -> Data(Type.of[Result[A]].plainPrint)
      )
    )
  }

  def testTypeOfWithCtor1Higher: Expr[Data] = {
    val container = makeCtorResultContainer
    import container.Result

    Expr(
      Data.map(
        "functorOfResult" -> Data(Type.of[hearth.fp.Functor[Result]].plainPrint),
        "containerOfResult" -> Data(Type.of[Container[Result]].plainPrint)
      )
    )
  }

  def testIdentityCtor1Untyped: Expr[Data] =
    Expr(
      Data.map(
        "identityCtor1Untyped" -> Data(UntypedType.plainPrint(Type.identityCtor1Untyped))
      )
    )

  /** A Type.CtorN declared as `implicit val`/`implicit def` in the enclosing block (not imported) is found by separate
    * `ValDef`/`DefDef` branches of `CrossQuotesPlugin.blockGivenCandidates` on Scala 3.
    */
  def testTypeOfWithDirectCtorValDef[A: Type]: Expr[Data] = {
    // Created outside this block: otherwise the plugin injects a given that references the val being initialized,
    // which loops forever.
    val optCtor = makeOptionCtor
    val eitherCtor = makeEitherCtor
    implicit val OptionCtor: Type.Ctor1[Option] = optCtor
    implicit def EitherCtor: Type.Ctor2[Either] = eitherCtor
    hearth.fp.ignore(OptionCtor, EitherCtor) // suppress unused warnings on Scala 2
    Expr(
      Data.map(
        "optionOfA" -> Data(Type.of[Option[A]].plainPrint),
        "eitherOfStringAndA" -> Data(Type.of[Either[String, A]].plainPrint)
      )
    )
  }

  protected def makeOptionCtor: Type.Ctor1[Option] = Type.Ctor1.of[Option]

  protected def makeEitherCtor: Type.Ctor2[Either] = Type.Ctor2.of[Either]

  /** A Type.Ctor1 coming from the enclosing method's implicit parameters is found by a different plugin path than an
    * imported or block-local one: `CrossQuotesPlugin.boundGivenCandidates` on Scala 3, `enclosingMethodImplicitTypes`
    * on Scala 2.
    */
  def testTypeOfWithCtor1ContextBound[F[_]](implicit FC: Type.Ctor1[F]): Expr[Data] = {
    hearth.fp.ignore(FC)
    Expr(
      Data.map(
        "fOfInt" -> Data(FC.apply[Int](using Type.of[Int]).plainPrint),
        "fOfString" -> Data(FC.apply[String](using Type.of[String]).plainPrint)
      )
    )
  }

  def testExprQuoteWithCtor1Param[F[_]](implicit FC: Type.Ctor1[F]): Expr[Data] = {
    hearth.fp.ignore(FC)
    Expr.quote {
      val container: Container[F] = new Container[F] {
        override def value: F[String] = null.asInstanceOf[F[String]]
      }
      Data(container.getClass.getSimpleName)
    }
  }

  def testTypeOfWithCtorHKTApplied[F[_]](implicit FC: Type.Ctor1[F]): Expr[Data] = {
    hearth.fp.ignore(FC)
    val fOfInt = Type.of[F[Int]]
    Expr(Data(fOfInt.plainPrint))
  }

  def testTypeOfWithCtorHKTAndTypeParam[F[_], A](implicit FC: Type.Ctor1[F], A: Type[A]): Expr[Data] = {
    hearth.fp.ignore(FC)
    val fOfA = Type.of[F[A]]
    Expr(Data(fOfA.plainPrint))
  }

  def testTypeOfLocalParamInSplice: Expr[Data] =
    Expr.quote {
      def helper[A]: String = Expr.splice {
        hearth.fp.ignore(Type.of[A])
        Expr("ok")
      }
      Data(helper[Int])
    }

  def testSpliceWithMultipleLocalParams: Expr[Data] =
    Expr.quote {
      def combine[A, B]: String = Expr.splice {
        hearth.fp.ignore(Type.of[A], Type.of[B])
        Expr("ok")
      }
      Data(combine[Int, String])
    }

  def testNestedQuoteNoTypeParams: Expr[Data] =
    Expr.quote {
      val greeting: String = Expr.splice {
        Expr.quote("hello")
      }
      Data(greeting)
    }

  /** The inner quote needs a WeakTypeTag for `A` - a type param that only exists in the generated code. */
  def testNestedQuoteWithTypeParam: Expr[Data] =
    Expr.quote {
      def identity[A](a: A): A = Expr.splice {
        Expr.quote(a)
      }
      Data(identity("hello"))
    }

  /** The inner quote needs a WeakTypeTag for `Option[A]` - a concrete type constructor applied to a type param that
    * only exists in the generated code.
    */
  def testNestedQuoteWithAppliedType: Expr[Data] =
    Expr.quote {
      def wrapInOption[A](a: A): Option[A] = Expr.splice {
        Expr.quote(Option(a))
      }
      Data(wrapInOption("hello").toString)
    }

  /** The quote-splice-quote-splice-quote shape type class derivation needs: the innermost quote splices in values
    * captured from the outer quote's scope.
    */
  def testNestedQuoteSpliceComposition: Expr[Data] =
    Expr.quote {
      def applyFn[A, B](a: A, f: A => B): B = Expr.splice {
        val aExpr: Expr[A] = Expr.quote(a)
        val fExpr: Expr[A => B] = Expr.quote(f)
        Expr.quote {
          Expr.splice(fExpr)(Expr.splice(aExpr))
        }
      }
      Data(applyFn("hello", (s: String) => s.length).toString)
    }

  /** Only checks that `Type.of[A]`, `Type.of[B]` and `F` are available in the splice, so `map` is a dummy -
    * testFunctorDerivation builds a real one.
    */
  def testFunctorSkeleton[F[_]](implicit FC: Type.Ctor1[F]): Expr[Data] = {
    hearth.fp.ignore(FC)
    Expr.quote {
      val functor: hearth.fp.Functor[F] = new hearth.fp.Functor[F] {
        def map[A, B](fa: F[A])(f: A => B): F[B] = {
          val _evidence: String = Expr.splice {
            hearth.fp.ignore(Type.of[A], Type.of[B])
            Expr("ok")
          }
          hearth.fp.ignore(_evidence)
          null.asInstanceOf[F[B]]
        }
      }
      hearth.fp.ignore(functor)
      Data("ok")
    }
  }

  def testFunctorDerivation: Expr[Data] =
    Expr.quote {
      val functor: hearth.fp.Functor[List] = new hearth.fp.Functor[List] {
        def map[A, B](fa: List[A])(f: A => B): List[B] = Expr.splice {
          val faExpr: Expr[List[A]] = Expr.quote(fa)
          val fExpr: Expr[A => B] = Expr.quote(f)
          Expr.quote {
            Expr.splice(faExpr).map(Expr.splice(fExpr))
          }
        }
      }
      Data(functor.map(List(1, 2, 3))(_.toString).mkString(", "))
    }

  /** Only the type evidence is built in a helper method: the nested Expr.quote has to stay inline in the splice body,
    * because free types for local type params don't cross into separately created quasiquotes.
    */
  def testFunctorDerivationWithCtor[F[_]](implicit FC: Type.Ctor1[F]): Expr[Data] = {
    hearth.fp.ignore(FC)

    def describeMapping[A, B](implicit A: Type[A], B: Type[B]): Expr[String] = {
      val fOfA = FC.apply[A](using A)
      val fOfB = FC.apply[B](using B)
      Expr(s"${fOfA.plainPrint} => ${fOfB.plainPrint}")
    }

    Expr.quote {
      val functor: hearth.fp.Functor[F] = new hearth.fp.Functor[F] {
        def map[A, B](fa: F[A])(f: A => B): F[B] = Expr.splice {
          val tpeA = Type.of[A]
          val tpeB = Type.of[B]
          val desc: Expr[String] = describeMapping[A, B](tpeA, tpeB)
          Expr.quote {
            hearth.fp.ignore(Expr.splice(desc))
            Expr.splice(Expr.quote(fa)).asInstanceOf[List[A]].map(Expr.splice(Expr.quote(f))).asInstanceOf[F[B]]
          }
        }
      }
      Data(functor.map(List(1, 2, 3).asInstanceOf[F[Int]])(_.toString).asInstanceOf[List[String]].mkString(", "))
    }
  }

  /** `Method.methodsOf[A]` builds trees whose `TypeTree`s may reference the free type symbol `A`; unless
    * `stripFreeTypes` removes them, expansion fails with "Macro expansion contains free type variable A".
    */
  def testSpliceWithMethodsOfLocalParam: Expr[Data] =
    Expr.quote {
      def helper[A]: String = Expr.splice {
        val tpeA = Type.of[A]
        hearth.fp.ignore(Method.methodsOf[A](using tpeA))
        Expr("ok")
      }
      Data(helper[Int])
    }

  def testSpliceWithCaseClassParseLocalParam: Expr[Data] =
    Expr.quote {
      def helper[A]: String = Expr.splice {
        val tpeA = Type.of[A]
        hearth.fp.ignore(Type.prettyPrint[A](using tpeA))
        hearth.fp.ignore(CaseClass.parse[A](using tpeA))
        Expr("ok")
      }
      Data(helper[Int])
    }

  def testCtorK1Of: Expr[Data] = {
    import hearth.examples.kinds.HigherKinded1
    val hk1Ctor: Type.CtorK1[HigherKinded1] = Type.CtorK1.of[HigherKinded1]
    val optionCtor = Type.Ctor1.of[Option]
    val applied = hk1Ctor.apply[Option](using optionCtor)
    val unapplyResult = hk1Ctor.unapply(applied)
    Expr(
      Data.map(
        "applied" -> Data(applied.plainPrint),
        "unapplyIsDefined" -> Data(unapplyResult.isDefined.toString),
        "asUntyped" -> Data(UntypedType.plainPrint(hk1Ctor.asUntyped))
      )
    )
  }

  def testCtorK1FromUntyped: Expr[Data] = {
    import hearth.examples.kinds.HigherKinded1
    val original = Type.CtorK1.of[HigherKinded1]
    val untyped = original.asUntyped
    val roundtripped = Type.CtorK1.fromUntyped[HigherKinded1](untyped)
    val optionCtor = Type.Ctor1.of[Option]
    val applied = roundtripped.apply[Option](using optionCtor)
    Expr(
      Data.map(
        "applied" -> Data(applied.plainPrint),
        "unapplyMatch" -> Data(roundtripped.unapply(applied).isDefined.toString),
        "unapplyNoMatch" -> Data(roundtripped.unapply(Type.of[String]).isDefined.toString)
      )
    )
  }

  def testTypeOfWithCtorK1ContextBound[F[_[_]]](implicit FK: Type.CtorK1[F]): Expr[Data] = {
    hearth.fp.ignore(FK)
    val optionCtor = Type.Ctor1.of[Option]
    val fOfOption = FK.apply[Option](using optionCtor)
    Expr(
      Data.map(
        "fOfOption" -> Data(fOfOption.plainPrint)
      )
    )
  }

  def testExprQuoteWithCtorK1Param[F[_[_]]](implicit FK: Type.CtorK1[F]): Expr[Data] = {
    hearth.fp.ignore(FK)
    Expr.quote {
      val c: ContainerK1[F] = new ContainerK1[F] {
        override def value: String = "ctorK1 works"
      }
      Data(c.value)
    }
  }

}
