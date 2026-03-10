package hearth
package crossquotes

import hearth.data.Data

trait CrossCtorInjectionFixturesImpl extends CrossCtorInjectionFixturesImplGen { this: MacroCommons =>

  /** Test: Type.of resolves types when an imported Type.Ctor1 has a recognized name (like "Result").
    *
    * Demonstrates that importing a Type.Ctor1 value named "Result" makes `Type.of[Result[A]]` work correctly. This
    * pattern is used by CtorLikeOf and similar traits.
    */
  def testTypeOfWithImportedCtor1[A: Type]: Expr[Data] = {
    // Create a container mimicking CtorLikeOf's pattern:
    // - type alias Result[X] defines the type constructor
    // - val Result: Type.Ctor1[Result] provides the type constructor value
    // The name "Result" is in the ImportedCrossTypeImplicit set
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

  /** Test: Type.of[Trait[F]] works when F comes from Type.Ctor1. */
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

  /** Test: Type.identityCtor1Untyped returns valid UntypedType. */
  def testIdentityCtor1Untyped: Expr[Data] =
    Expr(
      Data.map(
        "identityCtor1Untyped" -> Data(UntypedType.plainPrint(Type.identityCtor1Untyped))
      )
    )

  /** Test: Type.of resolves types when implicit val/def Type.CtorN is declared directly in the block body.
    *
    * On Scala 3, this exercises the `.orElse(TypeCtorAppliedTypeTree.unapply(...))` branches in
    * CrossQuotesPlugin.blockGivenCandidates — specifically the `ValDef` handler (implicit val with Type.CtorN) and the
    * `DefDef` handler (implicit def with Type.CtorN). These branches handle `implicit val/def` returning
    * `Type.CtorN[HKT]` directly in the enclosing block (not via imports).
    */
  def testTypeOfWithDirectCtorValDef[A: Type]: Expr[Data] = {
    // Create ctors outside the block that uses Type.of, to avoid the infinite loop
    // from the plugin injecting a given that references the val being initialized.
    val optCtor = makeOptionCtor
    val eitherCtor = makeEitherCtor
    // Declare as implicit val/def in the block body — this triggers the plugin's
    // .orElse(TypeCtorAppliedTypeTree.unapply) branches for ValDef and DefDef.
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

  /** Factory for Option ctor — outside the test block to avoid circular initialization. */
  protected def makeOptionCtor: Type.Ctor1[Option] = Type.Ctor1.of[Option]

  /** Factory for Either ctor — outside the test block to avoid circular initialization. */
  protected def makeEitherCtor: Type.Ctor2[Either] = Type.Ctor2.of[Either]

  /** Test: Type.of resolves types when Type.Ctor1 is provided as a context bound / implicit parameter on the enclosing
    * method.
    *
    * On Scala 3, this exercises the `CtxBoundsCtorTypeTree` extractor and the `TypeCtorAppliedTypeTree` fallback in
    * `CrossQuotesPlugin.boundGivenCandidates`. On Scala 2, it exercises `extractCtorHKT` in
    * `enclosingMethodImplicitTypes`.
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

  /** Test: Expr.quote works with F from Type.Ctor1 context bound in type positions. */
  def testExprQuoteWithCtor1Param[F[_]](implicit FC: Type.Ctor1[F]): Expr[Data] = {
    hearth.fp.ignore(FC)
    Expr.quote {
      val container: Container[F] = new Container[F] {
        override def value: F[String] = null.asInstanceOf[F[String]]
      }
      Data(container.getClass.getSimpleName)
    }
  }

  /** Test: Type.of[F[Int]] works when F comes from Type.Ctor1 context bound. */
  def testTypeOfWithCtorHKTApplied[F[_]](implicit FC: Type.Ctor1[F]): Expr[Data] = {
    hearth.fp.ignore(FC)
    val fOfInt = Type.of[F[Int]]
    Expr(Data(fOfInt.plainPrint))
  }

  /** Test: Type.of[F[A]] works when F comes from Type.Ctor1 and A from Type. */
  def testTypeOfWithCtorHKTAndTypeParam[F[_], A](implicit FC: Type.Ctor1[F], A: Type[A]): Expr[Data] = {
    hearth.fp.ignore(FC)
    val fOfA = Type.of[F[A]]
    Expr(Data(fOfA.plainPrint))
  }

  /** Test: Type.of[A] inside Expr.splice inside Expr.quote, where A is a local type param. */
  def testTypeOfLocalParamInSplice: Expr[Data] =
    Expr.quote {
      def helper[A]: String = Expr.splice {
        hearth.fp.ignore(Type.of[A]) // verify Type.of[A] is obtainable for local type param
        Expr("ok")
      }
      Data(helper[Int])
    }

  /** Test: Multiple local type params (A, B) inside Expr.splice inside Expr.quote.
    *
    * Extends the single-param test to verify the workaround handles multiple type params simultaneously.
    */
  def testSpliceWithMultipleLocalParams: Expr[Data] =
    Expr.quote {
      def combine[A, B]: String = Expr.splice {
        hearth.fp.ignore(Type.of[A], Type.of[B])
        Expr("ok")
      }
      Data(combine[Int, String])
    }

  /** Test: Nested Expr.quote inside Expr.splice without type params.
    *
    * Verifies that inner Expr.quote expansions work correctly when nested inside Expr.splice inside an outer
    * Expr.quote. This is the simplest nested quote case — no local type params involved.
    */
  def testNestedQuoteNoTypeParams: Expr[Data] =
    Expr.quote {
      val greeting: String = Expr.splice {
        Expr.quote("hello")
      }
      Data(greeting)
    }

  /** Test: Nested Expr.quote inside Expr.splice with a local type param.
    *
    * Verifies that inner Expr.quote can handle types that reference local type params from DefDef nodes in the outer
    * quote body. The inner quote needs to produce a WeakTypeTag for a type containing `A` — a type param that only
    * exists in the generated code.
    */
  def testNestedQuoteWithTypeParam: Expr[Data] =
    Expr.quote {
      def identity[A](a: A): A = Expr.splice {
        Expr.quote(a)
      }
      Data(identity("hello"))
    }

  /** Test: Nested Expr.quote with applied types using local type params.
    *
    * Tests that inner Expr.quote(Option(a)) works where a: A and A is a local type param. The inner quote needs
    * WeakTypeTag[Option[A]] which combines a concrete type constructor with a local type param.
    */
  def testNestedQuoteWithAppliedType: Expr[Data] =
    Expr.quote {
      def wrapInOption[A](a: A): Option[A] = Expr.splice {
        Expr.quote(Option(a))
      }
      Data(wrapInOption("hello").toString)
    }

  /** Test: Nested Expr.quote + Expr.splice composition (quote-splice-quote-splice-quote).
    *
    * Tests the pattern needed for type class derivation: the outer splice body contains multiple inner quotes, and the
    * innermost quote splices in values from the outer quote's scope.
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

  /** Test: Type class skeleton combining HKT (Type.Ctor1) with local type params (A, B).
    *
    * Verifies that Type.of[A], Type.of[B], and Type.Ctor1[F] are all accessible inside Expr.splice within Expr.quote
    * where A, B are local type params from methods inside the quote body. Uses a dummy map implementation — see
    * testFunctorDerivation for a real working Functor derivation.
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
          null.asInstanceOf[F[B]] // dummy impl — real impl would require nested Expr.quote support
        }
      }
      hearth.fp.ignore(functor)
      Data("ok")
    }
  }

  /** Test: Real Functor derivation using nested Expr.quote inside Expr.splice.
    *
    * Demonstrates a fully working Functor[List] derivation using the quote-splice-quote pattern. The map implementation
    * uses nested Expr.quote to capture `fa` and `f`, then splices them into a new quote that calls `.map`.
    *
    * This proves that Hearth's cross-quotes support the full pattern needed for type class derivation with HKT types
    * and local type params.
    */
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

  /** Test: Type.Ctor1[F] composes with an extracted body method that takes Type context bounds for local type params.
    *
    * Verifies that HKT type constructors (F from Type.Ctor1) compose with local type params (A, B from Type context
    * bounds on a helper method) and nested Expr.quote. The extracted `mapBody` uses Type.of and Type.Ctor1 to build
    * type evidence, while the nested Expr.quote that constructs the actual code is inline in the splice body (required
    * because free types from the outer workaround don't cross quasiquote boundaries into separately-created
    * quasiquotes).
    */
  def testFunctorDerivationWithCtor[F[_]](implicit FC: Type.Ctor1[F]): Expr[Data] = {
    hearth.fp.ignore(FC)

    /** Extracted body method: receives local type params as Type context bounds and returns a description combining HKT
      * (F from Type.Ctor1) with the local type params.
      */
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
          // Verify describeMapping composes Type.Ctor1[F] with local A, B
          val desc: Expr[String] = describeMapping[A, B](tpeA, tpeB)
          // Use nested inline Expr.quote for the actual map implementation
          Expr.quote {
            hearth.fp.ignore(Expr.splice(desc))
            Expr.splice(Expr.quote(fa)).asInstanceOf[List[A]].map(Expr.splice(Expr.quote(f))).asInstanceOf[F[B]]
          }
        }
      }
      Data(functor.map(List(1, 2, 3).asInstanceOf[F[Int]])(_.toString).asInstanceOf[List[String]].mkString(", "))
    }
  }

}
