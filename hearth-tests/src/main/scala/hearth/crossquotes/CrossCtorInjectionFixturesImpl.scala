package hearth
package crossquotes

import hearth.data.Data
import hearth.examples.kinds.*

trait CrossCtorInjectionFixturesImpl { this: MacroCommons =>

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

  /** Test: Type.CtorN.asUntyped returns correct representation for all arities. */
  def testCtorAsUntyped: Expr[Data] = {
    val OptionCtor: Type.Ctor1[Option] = Type.Ctor1.of[Option]
    val EitherCtor: Type.Ctor2[Either] = Type.Ctor2.of[Either]
    val Arity3Ctor: Type.Ctor3[Arity3] = Type.Ctor3.of[Arity3]
    val Arity4Ctor: Type.Ctor4[Arity4] = Type.Ctor4.of[Arity4]
    val Arity5Ctor: Type.Ctor5[Arity5] = Type.Ctor5.of[Arity5]
    val Arity6Ctor: Type.Ctor6[Arity6] = Type.Ctor6.of[Arity6]
    val Arity7Ctor: Type.Ctor7[Arity7] = Type.Ctor7.of[Arity7]
    val Arity8Ctor: Type.Ctor8[Arity8] = Type.Ctor8.of[Arity8]
    val Arity9Ctor: Type.Ctor9[Arity9] = Type.Ctor9.of[Arity9]
    val Arity10Ctor: Type.Ctor10[Arity10] = Type.Ctor10.of[Arity10]
    val Arity11Ctor: Type.Ctor11[Arity11] = Type.Ctor11.of[Arity11]
    val Arity12Ctor: Type.Ctor12[Arity12] = Type.Ctor12.of[Arity12]
    val Arity13Ctor: Type.Ctor13[Arity13] = Type.Ctor13.of[Arity13]
    val Arity14Ctor: Type.Ctor14[Arity14] = Type.Ctor14.of[Arity14]
    val Arity15Ctor: Type.Ctor15[Arity15] = Type.Ctor15.of[Arity15]
    val Arity16Ctor: Type.Ctor16[Arity16] = Type.Ctor16.of[Arity16]
    val Arity17Ctor: Type.Ctor17[Arity17] = Type.Ctor17.of[Arity17]
    val Arity18Ctor: Type.Ctor18[Arity18] = Type.Ctor18.of[Arity18]
    val Arity19Ctor: Type.Ctor19[Arity19] = Type.Ctor19.of[Arity19]
    val Arity20Ctor: Type.Ctor20[Arity20] = Type.Ctor20.of[Arity20]
    val Arity21Ctor: Type.Ctor21[Arity21] = Type.Ctor21.of[Arity21]
    val Arity22Ctor: Type.Ctor22[Arity22] = Type.Ctor22.of[Arity22]

    Expr(
      Data.map(
        "optionCtorUntyped" -> Data(UntypedType.plainPrint(OptionCtor.asUntyped)),
        "eitherCtorUntyped" -> Data(UntypedType.plainPrint(EitherCtor.asUntyped)),
        "arity3CtorUntyped" -> Data(UntypedType.plainPrint(Arity3Ctor.asUntyped)),
        "arity4CtorUntyped" -> Data(UntypedType.plainPrint(Arity4Ctor.asUntyped)),
        "arity5CtorUntyped" -> Data(UntypedType.plainPrint(Arity5Ctor.asUntyped)),
        "arity6CtorUntyped" -> Data(UntypedType.plainPrint(Arity6Ctor.asUntyped)),
        "arity7CtorUntyped" -> Data(UntypedType.plainPrint(Arity7Ctor.asUntyped)),
        "arity8CtorUntyped" -> Data(UntypedType.plainPrint(Arity8Ctor.asUntyped)),
        "arity9CtorUntyped" -> Data(UntypedType.plainPrint(Arity9Ctor.asUntyped)),
        "arity10CtorUntyped" -> Data(UntypedType.plainPrint(Arity10Ctor.asUntyped)),
        "arity11CtorUntyped" -> Data(UntypedType.plainPrint(Arity11Ctor.asUntyped)),
        "arity12CtorUntyped" -> Data(UntypedType.plainPrint(Arity12Ctor.asUntyped)),
        "arity13CtorUntyped" -> Data(UntypedType.plainPrint(Arity13Ctor.asUntyped)),
        "arity14CtorUntyped" -> Data(UntypedType.plainPrint(Arity14Ctor.asUntyped)),
        "arity15CtorUntyped" -> Data(UntypedType.plainPrint(Arity15Ctor.asUntyped)),
        "arity16CtorUntyped" -> Data(UntypedType.plainPrint(Arity16Ctor.asUntyped)),
        "arity17CtorUntyped" -> Data(UntypedType.plainPrint(Arity17Ctor.asUntyped)),
        "arity18CtorUntyped" -> Data(UntypedType.plainPrint(Arity18Ctor.asUntyped)),
        "arity19CtorUntyped" -> Data(UntypedType.plainPrint(Arity19Ctor.asUntyped)),
        "arity20CtorUntyped" -> Data(UntypedType.plainPrint(Arity20Ctor.asUntyped)),
        "arity21CtorUntyped" -> Data(UntypedType.plainPrint(Arity21Ctor.asUntyped)),
        "arity22CtorUntyped" -> Data(UntypedType.plainPrint(Arity22Ctor.asUntyped))
      )
    )
  }

  /** Test: Type.CtorN.setX[...].asUntyped works for all arities. */
  def testCtorSetAsUntyped: Expr[Data] = {
    val EitherCtor: Type.Ctor2[Either] = Type.Ctor2.of[Either]
    val Arity3Ctor: Type.Ctor3[Arity3] = Type.Ctor3.of[Arity3]
    val Arity4Ctor: Type.Ctor4[Arity4] = Type.Ctor4.of[Arity4]
    val Arity5Ctor: Type.Ctor5[Arity5] = Type.Ctor5.of[Arity5]
    val Arity6Ctor: Type.Ctor6[Arity6] = Type.Ctor6.of[Arity6]
    val Arity7Ctor: Type.Ctor7[Arity7] = Type.Ctor7.of[Arity7]
    val Arity8Ctor: Type.Ctor8[Arity8] = Type.Ctor8.of[Arity8]
    val Arity9Ctor: Type.Ctor9[Arity9] = Type.Ctor9.of[Arity9]
    val Arity10Ctor: Type.Ctor10[Arity10] = Type.Ctor10.of[Arity10]
    val Arity11Ctor: Type.Ctor11[Arity11] = Type.Ctor11.of[Arity11]
    val Arity12Ctor: Type.Ctor12[Arity12] = Type.Ctor12.of[Arity12]
    val Arity13Ctor: Type.Ctor13[Arity13] = Type.Ctor13.of[Arity13]
    val Arity14Ctor: Type.Ctor14[Arity14] = Type.Ctor14.of[Arity14]
    val Arity15Ctor: Type.Ctor15[Arity15] = Type.Ctor15.of[Arity15]
    val Arity16Ctor: Type.Ctor16[Arity16] = Type.Ctor16.of[Arity16]
    val Arity17Ctor: Type.Ctor17[Arity17] = Type.Ctor17.of[Arity17]
    val Arity18Ctor: Type.Ctor18[Arity18] = Type.Ctor18.of[Arity18]
    val Arity19Ctor: Type.Ctor19[Arity19] = Type.Ctor19.of[Arity19]
    val Arity20Ctor: Type.Ctor20[Arity20] = Type.Ctor20.of[Arity20]
    val Arity21Ctor: Type.Ctor21[Arity21] = Type.Ctor21.of[Arity21]
    val Arity22Ctor: Type.Ctor22[Arity22] = Type.Ctor22.of[Arity22]

    Expr(
      Data.map(
        "eitherSetAUntyped" -> Data(UntypedType.plainPrint(EitherCtor.setA[String](using Type.of[String]).asUntyped)),
        "eitherSetBUntyped" -> Data(UntypedType.plainPrint(EitherCtor.setB[Int](using Type.of[Int]).asUntyped)),
        "arity3SetAUntyped" -> Data(UntypedType.plainPrint(Arity3Ctor.setA[String](using Type.of[String]).asUntyped)),
        "arity3SetCUntyped" -> Data(UntypedType.plainPrint(Arity3Ctor.setC[Int](using Type.of[Int]).asUntyped)),
        "arity4SetAUntyped" -> Data(UntypedType.plainPrint(Arity4Ctor.setA[String](using Type.of[String]).asUntyped)),
        "arity4SetDUntyped" -> Data(UntypedType.plainPrint(Arity4Ctor.setD[Int](using Type.of[Int]).asUntyped)),
        "arity5SetAUntyped" -> Data(UntypedType.plainPrint(Arity5Ctor.setA[String](using Type.of[String]).asUntyped)),
        "arity5SetEUntyped" -> Data(UntypedType.plainPrint(Arity5Ctor.setE[Int](using Type.of[Int]).asUntyped)),
        "arity6SetAUntyped" -> Data(UntypedType.plainPrint(Arity6Ctor.setA[String](using Type.of[String]).asUntyped)),
        "arity6SetFUntyped" -> Data(UntypedType.plainPrint(Arity6Ctor.setF[Int](using Type.of[Int]).asUntyped)),
        "arity7SetAUntyped" -> Data(UntypedType.plainPrint(Arity7Ctor.setA[String](using Type.of[String]).asUntyped)),
        "arity7SetGUntyped" -> Data(UntypedType.plainPrint(Arity7Ctor.setG[Int](using Type.of[Int]).asUntyped)),
        "arity8SetAUntyped" -> Data(UntypedType.plainPrint(Arity8Ctor.setA[String](using Type.of[String]).asUntyped)),
        "arity8SetHUntyped" -> Data(UntypedType.plainPrint(Arity8Ctor.setH[Int](using Type.of[Int]).asUntyped)),
        "arity9SetAUntyped" -> Data(UntypedType.plainPrint(Arity9Ctor.setA[String](using Type.of[String]).asUntyped)),
        "arity9SetIUntyped" -> Data(UntypedType.plainPrint(Arity9Ctor.setI[Int](using Type.of[Int]).asUntyped)),
        "arity10SetAUntyped" -> Data(UntypedType.plainPrint(Arity10Ctor.setA[String](using Type.of[String]).asUntyped)),
        "arity10SetJUntyped" -> Data(UntypedType.plainPrint(Arity10Ctor.setJ[Int](using Type.of[Int]).asUntyped)),
        "arity11SetAUntyped" -> Data(UntypedType.plainPrint(Arity11Ctor.setA[String](using Type.of[String]).asUntyped)),
        "arity11SetKUntyped" -> Data(UntypedType.plainPrint(Arity11Ctor.setK[Int](using Type.of[Int]).asUntyped)),
        "arity12SetAUntyped" -> Data(UntypedType.plainPrint(Arity12Ctor.setA[String](using Type.of[String]).asUntyped)),
        "arity12SetLUntyped" -> Data(UntypedType.plainPrint(Arity12Ctor.setL[Int](using Type.of[Int]).asUntyped)),
        "arity13SetAUntyped" -> Data(UntypedType.plainPrint(Arity13Ctor.setA[String](using Type.of[String]).asUntyped)),
        "arity13SetMUntyped" -> Data(UntypedType.plainPrint(Arity13Ctor.setM[Int](using Type.of[Int]).asUntyped)),
        "arity14SetAUntyped" -> Data(UntypedType.plainPrint(Arity14Ctor.setA[String](using Type.of[String]).asUntyped)),
        "arity14SetNUntyped" -> Data(UntypedType.plainPrint(Arity14Ctor.setN[Int](using Type.of[Int]).asUntyped)),
        "arity15SetAUntyped" -> Data(UntypedType.plainPrint(Arity15Ctor.setA[String](using Type.of[String]).asUntyped)),
        "arity15SetOUntyped" -> Data(UntypedType.plainPrint(Arity15Ctor.setO[Int](using Type.of[Int]).asUntyped)),
        "arity16SetAUntyped" -> Data(UntypedType.plainPrint(Arity16Ctor.setA[String](using Type.of[String]).asUntyped)),
        "arity16SetPUntyped" -> Data(UntypedType.plainPrint(Arity16Ctor.setP[Int](using Type.of[Int]).asUntyped)),
        "arity17SetAUntyped" -> Data(UntypedType.plainPrint(Arity17Ctor.setA[String](using Type.of[String]).asUntyped)),
        "arity17SetQUntyped" -> Data(UntypedType.plainPrint(Arity17Ctor.setQ[Int](using Type.of[Int]).asUntyped)),
        "arity18SetAUntyped" -> Data(UntypedType.plainPrint(Arity18Ctor.setA[String](using Type.of[String]).asUntyped)),
        "arity18SetRUntyped" -> Data(UntypedType.plainPrint(Arity18Ctor.setR[Int](using Type.of[Int]).asUntyped)),
        "arity19SetAUntyped" -> Data(UntypedType.plainPrint(Arity19Ctor.setA[String](using Type.of[String]).asUntyped)),
        "arity19SetSUntyped" -> Data(UntypedType.plainPrint(Arity19Ctor.setS[Int](using Type.of[Int]).asUntyped)),
        "arity20SetAUntyped" -> Data(UntypedType.plainPrint(Arity20Ctor.setA[String](using Type.of[String]).asUntyped)),
        "arity20SetTUntyped" -> Data(UntypedType.plainPrint(Arity20Ctor.setT[Int](using Type.of[Int]).asUntyped)),
        "arity21SetAUntyped" -> Data(UntypedType.plainPrint(Arity21Ctor.setA[String](using Type.of[String]).asUntyped)),
        "arity21SetUUntyped" -> Data(UntypedType.plainPrint(Arity21Ctor.setU[Int](using Type.of[Int]).asUntyped)),
        "arity22SetAUntyped" -> Data(UntypedType.plainPrint(Arity22Ctor.setA[String](using Type.of[String]).asUntyped)),
        "arity22SetVUntyped" -> Data(UntypedType.plainPrint(Arity22Ctor.setV[Int](using Type.of[Int]).asUntyped))
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

  /** Test: Type.of[ContainerN[F]] works when F comes from Type.Ctor2. */
  def testTypeOfWithCtor2Higher: Expr[Data] = {
    val container = makeCtor2ResultContainer
    import container.Result

    Expr(
      Data.map(
        "container2OfResult" -> Data(Type.of[Container2[Result]].plainPrint)
      )
    )
  }

  /** Test: Type.of[ContainerN[F]] works when F comes from Type.Ctor3. */
  def testTypeOfWithCtor3Higher: Expr[Data] = {
    val container = makeCtor3ResultContainer
    import container.Result

    Expr(
      Data.map(
        "container3OfResult" -> Data(Type.of[Container3[Result]].plainPrint)
      )
    )
  }

  /** Test: Type.of[ContainerN[F]] works when F comes from Type.Ctor4. */
  def testTypeOfWithCtor4Higher: Expr[Data] = {
    val container = makeCtor4ResultContainer
    import container.Result

    Expr(
      Data.map(
        "container4OfResult" -> Data(Type.of[Container4[Result]].plainPrint)
      )
    )
  }

  /** Test: Type.of[ContainerN[F]] works when F comes from Type.Ctor5. */
  def testTypeOfWithCtor5Higher: Expr[Data] = {
    val container = makeCtor5ResultContainer
    import container.Result

    Expr(
      Data.map(
        "container5OfResult" -> Data(Type.of[Container5[Result]].plainPrint)
      )
    )
  }

  /** Test: Type.of[ContainerN[F]] works when F comes from Type.Ctor6. */
  def testTypeOfWithCtor6Higher: Expr[Data] = {
    val container = makeCtor6ResultContainer
    import container.Result

    Expr(
      Data.map(
        "container6OfResult" -> Data(Type.of[Container6[Result]].plainPrint)
      )
    )
  }

  /** Test: Type.of[ContainerN[F]] works when F comes from Type.Ctor7. */
  def testTypeOfWithCtor7Higher: Expr[Data] = {
    val container = makeCtor7ResultContainer
    import container.Result

    Expr(
      Data.map(
        "container7OfResult" -> Data(Type.of[Container7[Result]].plainPrint)
      )
    )
  }

  /** Test: Type.of[ContainerN[F]] works when F comes from Type.Ctor8. */
  def testTypeOfWithCtor8Higher: Expr[Data] = {
    val container = makeCtor8ResultContainer
    import container.Result

    Expr(
      Data.map(
        "container8OfResult" -> Data(Type.of[Container8[Result]].plainPrint)
      )
    )
  }

  /** Test: Type.of[ContainerN[F]] works when F comes from Type.Ctor9. */
  def testTypeOfWithCtor9Higher: Expr[Data] = {
    val container = makeCtor9ResultContainer
    import container.Result

    Expr(
      Data.map(
        "container9OfResult" -> Data(Type.of[Container9[Result]].plainPrint)
      )
    )
  }

  /** Test: Type.of[ContainerN[F]] works when F comes from Type.Ctor10. */
  def testTypeOfWithCtor10Higher: Expr[Data] = {
    val container = makeCtor10ResultContainer
    import container.Result

    Expr(
      Data.map(
        "container10OfResult" -> Data(Type.of[Container10[Result]].plainPrint)
      )
    )
  }

  /** Test: Type.of[ContainerN[F]] works when F comes from Type.Ctor11. */
  def testTypeOfWithCtor11Higher: Expr[Data] = {
    val container = makeCtor11ResultContainer
    import container.Result

    Expr(
      Data.map(
        "container11OfResult" -> Data(Type.of[Container11[Result]].plainPrint)
      )
    )
  }

  /** Test: Type.of[ContainerN[F]] works when F comes from Type.Ctor12. */
  def testTypeOfWithCtor12Higher: Expr[Data] = {
    val container = makeCtor12ResultContainer
    import container.Result

    Expr(
      Data.map(
        "container12OfResult" -> Data(Type.of[Container12[Result]].plainPrint)
      )
    )
  }

  /** Test: Type.of[ContainerN[F]] works when F comes from Type.Ctor13. */
  def testTypeOfWithCtor13Higher: Expr[Data] = {
    val container = makeCtor13ResultContainer
    import container.Result

    Expr(
      Data.map(
        "container13OfResult" -> Data(Type.of[Container13[Result]].plainPrint)
      )
    )
  }

  /** Test: Type.of[ContainerN[F]] works when F comes from Type.Ctor14. */
  def testTypeOfWithCtor14Higher: Expr[Data] = {
    val container = makeCtor14ResultContainer
    import container.Result

    Expr(
      Data.map(
        "container14OfResult" -> Data(Type.of[Container14[Result]].plainPrint)
      )
    )
  }

  /** Test: Type.of[ContainerN[F]] works when F comes from Type.Ctor15. */
  def testTypeOfWithCtor15Higher: Expr[Data] = {
    val container = makeCtor15ResultContainer
    import container.Result

    Expr(
      Data.map(
        "container15OfResult" -> Data(Type.of[Container15[Result]].plainPrint)
      )
    )
  }

  /** Test: Type.of[ContainerN[F]] works when F comes from Type.Ctor16. */
  def testTypeOfWithCtor16Higher: Expr[Data] = {
    val container = makeCtor16ResultContainer
    import container.Result

    Expr(
      Data.map(
        "container16OfResult" -> Data(Type.of[Container16[Result]].plainPrint)
      )
    )
  }

  /** Test: Type.of[ContainerN[F]] works when F comes from Type.Ctor17. */
  def testTypeOfWithCtor17Higher: Expr[Data] = {
    val container = makeCtor17ResultContainer
    import container.Result

    Expr(
      Data.map(
        "container17OfResult" -> Data(Type.of[Container17[Result]].plainPrint)
      )
    )
  }

  /** Test: Type.of[ContainerN[F]] works when F comes from Type.Ctor18. */
  def testTypeOfWithCtor18Higher: Expr[Data] = {
    val container = makeCtor18ResultContainer
    import container.Result

    Expr(
      Data.map(
        "container18OfResult" -> Data(Type.of[Container18[Result]].plainPrint)
      )
    )
  }

  /** Test: Type.of[ContainerN[F]] works when F comes from Type.Ctor19. */
  def testTypeOfWithCtor19Higher: Expr[Data] = {
    val container = makeCtor19ResultContainer
    import container.Result

    Expr(
      Data.map(
        "container19OfResult" -> Data(Type.of[Container19[Result]].plainPrint)
      )
    )
  }

  /** Test: Type.of[ContainerN[F]] works when F comes from Type.Ctor20. */
  def testTypeOfWithCtor20Higher: Expr[Data] = {
    val container = makeCtor20ResultContainer
    import container.Result

    Expr(
      Data.map(
        "container20OfResult" -> Data(Type.of[Container20[Result]].plainPrint)
      )
    )
  }

  /** Test: Type.of[ContainerN[F]] works when F comes from Type.Ctor21. */
  def testTypeOfWithCtor21Higher: Expr[Data] = {
    val container = makeCtor21ResultContainer
    import container.Result

    Expr(
      Data.map(
        "container21OfResult" -> Data(Type.of[Container21[Result]].plainPrint)
      )
    )
  }

  /** Test: Type.of[ContainerN[F]] works when F comes from Type.Ctor22. */
  def testTypeOfWithCtor22Higher: Expr[Data] = {
    val container = makeCtor22ResultContainer
    import container.Result

    Expr(
      Data.map(
        "container22OfResult" -> Data(Type.of[Container22[Result]].plainPrint)
      )
    )
  }

  /** Test: Expr.quote with Container[Option] (Ctor1 body). */
  def testExprQuoteWithCtor1Body: Expr[Data] =
    // Option is a globally available type, so Expr.quote handles it directly without Ctor1 injection.
    // This test validates that Expr.quote works with Container[Option] (a higher-kinded type application).
    Expr.quote {
      val c: Container[Option] = new Container[Option] {
        override def value: Option[String] = Some("ctor injection works")
      }
      Data(c.value.toString)
    }

  /** Test: Expr.quote with Container2[Either] (Ctor2 body). */
  def testExprQuoteWithCtor2Body: Expr[Data] =
    Expr.quote {
      val c: Container2[Either] = new Container2[Either] {
        override def value: String = "ctor2 works"
      }
      Data(c.value)
    }

  /** Test: Expr.quote with Container3[Arity3] (Ctor3 body). */
  def testExprQuoteWithCtor3Body: Expr[Data] =
    Expr.quote {
      val c: Container3[Arity3] = new Container3[Arity3] {
        override def value: String = "ctor3 works"
      }
      Data(c.value)
    }

  /** Test: Expr.quote with Container4[Arity4] (Ctor4 body). */
  def testExprQuoteWithCtor4Body: Expr[Data] =
    Expr.quote {
      val c: Container4[Arity4] = new Container4[Arity4] {
        override def value: String = "ctor4 works"
      }
      Data(c.value)
    }

  /** Test: Expr.quote with Container5[Arity5] (Ctor5 body). */
  def testExprQuoteWithCtor5Body: Expr[Data] =
    Expr.quote {
      val c: Container5[Arity5] = new Container5[Arity5] {
        override def value: String = "ctor5 works"
      }
      Data(c.value)
    }

  /** Test: Expr.quote with Container6[Arity6] (Ctor6 body). */
  def testExprQuoteWithCtor6Body: Expr[Data] =
    Expr.quote {
      val c: Container6[Arity6] = new Container6[Arity6] {
        override def value: String = "ctor6 works"
      }
      Data(c.value)
    }

  /** Test: Expr.quote with Container7[Arity7] (Ctor7 body). */
  def testExprQuoteWithCtor7Body: Expr[Data] =
    Expr.quote {
      val c: Container7[Arity7] = new Container7[Arity7] {
        override def value: String = "ctor7 works"
      }
      Data(c.value)
    }

  /** Test: Expr.quote with Container8[Arity8] (Ctor8 body). */
  def testExprQuoteWithCtor8Body: Expr[Data] =
    Expr.quote {
      val c: Container8[Arity8] = new Container8[Arity8] {
        override def value: String = "ctor8 works"
      }
      Data(c.value)
    }

  /** Test: Expr.quote with Container9[Arity9] (Ctor9 body). */
  def testExprQuoteWithCtor9Body: Expr[Data] =
    Expr.quote {
      val c: Container9[Arity9] = new Container9[Arity9] {
        override def value: String = "ctor9 works"
      }
      Data(c.value)
    }

  /** Test: Expr.quote with Container10[Arity10] (Ctor10 body). */
  def testExprQuoteWithCtor10Body: Expr[Data] =
    Expr.quote {
      val c: Container10[Arity10] = new Container10[Arity10] {
        override def value: String = "ctor10 works"
      }
      Data(c.value)
    }

  /** Test: Expr.quote with Container11[Arity11] (Ctor11 body). */
  def testExprQuoteWithCtor11Body: Expr[Data] =
    Expr.quote {
      val c: Container11[Arity11] = new Container11[Arity11] {
        override def value: String = "ctor11 works"
      }
      Data(c.value)
    }

  /** Test: Expr.quote with Container12[Arity12] (Ctor12 body). */
  def testExprQuoteWithCtor12Body: Expr[Data] =
    Expr.quote {
      val c: Container12[Arity12] = new Container12[Arity12] {
        override def value: String = "ctor12 works"
      }
      Data(c.value)
    }

  /** Test: Expr.quote with Container13[Arity13] (Ctor13 body). */
  def testExprQuoteWithCtor13Body: Expr[Data] =
    Expr.quote {
      val c: Container13[Arity13] = new Container13[Arity13] {
        override def value: String = "ctor13 works"
      }
      Data(c.value)
    }

  /** Test: Expr.quote with Container14[Arity14] (Ctor14 body). */
  def testExprQuoteWithCtor14Body: Expr[Data] =
    Expr.quote {
      val c: Container14[Arity14] = new Container14[Arity14] {
        override def value: String = "ctor14 works"
      }
      Data(c.value)
    }

  /** Test: Expr.quote with Container15[Arity15] (Ctor15 body). */
  def testExprQuoteWithCtor15Body: Expr[Data] =
    Expr.quote {
      val c: Container15[Arity15] = new Container15[Arity15] {
        override def value: String = "ctor15 works"
      }
      Data(c.value)
    }

  /** Test: Expr.quote with Container16[Arity16] (Ctor16 body). */
  def testExprQuoteWithCtor16Body: Expr[Data] =
    Expr.quote {
      val c: Container16[Arity16] = new Container16[Arity16] {
        override def value: String = "ctor16 works"
      }
      Data(c.value)
    }

  /** Test: Expr.quote with Container17[Arity17] (Ctor17 body). */
  def testExprQuoteWithCtor17Body: Expr[Data] =
    Expr.quote {
      val c: Container17[Arity17] = new Container17[Arity17] {
        override def value: String = "ctor17 works"
      }
      Data(c.value)
    }

  /** Test: Expr.quote with Container18[Arity18] (Ctor18 body). */
  def testExprQuoteWithCtor18Body: Expr[Data] =
    Expr.quote {
      val c: Container18[Arity18] = new Container18[Arity18] {
        override def value: String = "ctor18 works"
      }
      Data(c.value)
    }

  /** Test: Expr.quote with Container19[Arity19] (Ctor19 body). */
  def testExprQuoteWithCtor19Body: Expr[Data] =
    Expr.quote {
      val c: Container19[Arity19] = new Container19[Arity19] {
        override def value: String = "ctor19 works"
      }
      Data(c.value)
    }

  /** Test: Expr.quote with Container20[Arity20] (Ctor20 body). */
  def testExprQuoteWithCtor20Body: Expr[Data] =
    Expr.quote {
      val c: Container20[Arity20] = new Container20[Arity20] {
        override def value: String = "ctor20 works"
      }
      Data(c.value)
    }

  /** Test: Expr.quote with Container21[Arity21] (Ctor21 body). */
  def testExprQuoteWithCtor21Body: Expr[Data] =
    Expr.quote {
      val c: Container21[Arity21] = new Container21[Arity21] {
        override def value: String = "ctor21 works"
      }
      Data(c.value)
    }

  /** Test: Expr.quote with Container22[Arity22] (Ctor22 body). */
  def testExprQuoteWithCtor22Body: Expr[Data] =
    Expr.quote {
      val c: Container22[Arity22] = new Container22[Arity22] {
        override def value: String = "ctor22 works"
      }
      Data(c.value)
    }

  /** Factory that builds the container outside of the class body, avoiding infinite loop from the plugin injecting a
    * given for `Result` (via `@ImportedCrossTypeImplicit`) during the initialization of `val Result` itself.
    */
  private def makeCtorResultContainer: CtorResultContainer = {
    val ctor = Type.Ctor2.of[Either].setA[String](using Type.of[String])
    new CtorResultContainer(ctor.asInstanceOf[Type.Ctor1[Option]])
  }

  private def makeCtor2ResultContainer: Ctor2ResultContainer = {
    val ctor = Type.Ctor3.of[Arity3].setA[String](using Type.of[String])
    new Ctor2ResultContainer(ctor.asInstanceOf[Type.Ctor2[Either]])
  }

  private def makeCtor3ResultContainer: Ctor3ResultContainer = {
    val ctor = Type.Ctor4.of[Arity4].setA[String](using Type.of[String])
    new Ctor3ResultContainer(ctor.asInstanceOf[Type.Ctor3[Arity3]])
  }

  private def makeCtor4ResultContainer: Ctor4ResultContainer = {
    val ctor = Type.Ctor5.of[Arity5].setA[String](using Type.of[String])
    new Ctor4ResultContainer(ctor.asInstanceOf[Type.Ctor4[Arity4]])
  }

  private def makeCtor5ResultContainer: Ctor5ResultContainer = {
    val ctor = Type.Ctor6.of[Arity6].setA[String](using Type.of[String])
    new Ctor5ResultContainer(ctor.asInstanceOf[Type.Ctor5[Arity5]])
  }

  private def makeCtor6ResultContainer: Ctor6ResultContainer = {
    val ctor = Type.Ctor7.of[Arity7].setA[String](using Type.of[String])
    new Ctor6ResultContainer(ctor.asInstanceOf[Type.Ctor6[Arity6]])
  }

  private def makeCtor7ResultContainer: Ctor7ResultContainer = {
    val ctor = Type.Ctor8.of[Arity8].setA[String](using Type.of[String])
    new Ctor7ResultContainer(ctor.asInstanceOf[Type.Ctor7[Arity7]])
  }

  private def makeCtor8ResultContainer: Ctor8ResultContainer = {
    val ctor = Type.Ctor9.of[Arity9].setA[String](using Type.of[String])
    new Ctor8ResultContainer(ctor.asInstanceOf[Type.Ctor8[Arity8]])
  }

  private def makeCtor9ResultContainer: Ctor9ResultContainer = {
    val ctor = Type.Ctor10.of[Arity10].setA[String](using Type.of[String])
    new Ctor9ResultContainer(ctor.asInstanceOf[Type.Ctor9[Arity9]])
  }

  private def makeCtor10ResultContainer: Ctor10ResultContainer = {
    val ctor = Type.Ctor11.of[Arity11].setA[String](using Type.of[String])
    new Ctor10ResultContainer(ctor.asInstanceOf[Type.Ctor10[Arity10]])
  }

  private def makeCtor11ResultContainer: Ctor11ResultContainer = {
    val ctor = Type.Ctor12.of[Arity12].setA[String](using Type.of[String])
    new Ctor11ResultContainer(ctor.asInstanceOf[Type.Ctor11[Arity11]])
  }

  private def makeCtor12ResultContainer: Ctor12ResultContainer = {
    val ctor = Type.Ctor13.of[Arity13].setA[String](using Type.of[String])
    new Ctor12ResultContainer(ctor.asInstanceOf[Type.Ctor12[Arity12]])
  }

  private def makeCtor13ResultContainer: Ctor13ResultContainer = {
    val ctor = Type.Ctor14.of[Arity14].setA[String](using Type.of[String])
    new Ctor13ResultContainer(ctor.asInstanceOf[Type.Ctor13[Arity13]])
  }

  private def makeCtor14ResultContainer: Ctor14ResultContainer = {
    val ctor = Type.Ctor15.of[Arity15].setA[String](using Type.of[String])
    new Ctor14ResultContainer(ctor.asInstanceOf[Type.Ctor14[Arity14]])
  }

  private def makeCtor15ResultContainer: Ctor15ResultContainer = {
    val ctor = Type.Ctor16.of[Arity16].setA[String](using Type.of[String])
    new Ctor15ResultContainer(ctor.asInstanceOf[Type.Ctor15[Arity15]])
  }

  private def makeCtor16ResultContainer: Ctor16ResultContainer = {
    val ctor = Type.Ctor17.of[Arity17].setA[String](using Type.of[String])
    new Ctor16ResultContainer(ctor.asInstanceOf[Type.Ctor16[Arity16]])
  }

  private def makeCtor17ResultContainer: Ctor17ResultContainer = {
    val ctor = Type.Ctor18.of[Arity18].setA[String](using Type.of[String])
    new Ctor17ResultContainer(ctor.asInstanceOf[Type.Ctor17[Arity17]])
  }

  private def makeCtor18ResultContainer: Ctor18ResultContainer = {
    val ctor = Type.Ctor19.of[Arity19].setA[String](using Type.of[String])
    new Ctor18ResultContainer(ctor.asInstanceOf[Type.Ctor18[Arity18]])
  }

  private def makeCtor19ResultContainer: Ctor19ResultContainer = {
    val ctor = Type.Ctor20.of[Arity20].setA[String](using Type.of[String])
    new Ctor19ResultContainer(ctor.asInstanceOf[Type.Ctor19[Arity19]])
  }

  private def makeCtor20ResultContainer: Ctor20ResultContainer = {
    val ctor = Type.Ctor21.of[Arity21].setA[String](using Type.of[String])
    new Ctor20ResultContainer(ctor.asInstanceOf[Type.Ctor20[Arity20]])
  }

  private def makeCtor21ResultContainer: Ctor21ResultContainer = {
    val ctor = Type.Ctor22.of[Arity22].setA[String](using Type.of[String])
    new Ctor21ResultContainer(ctor.asInstanceOf[Type.Ctor21[Arity21]])
  }

  private def makeCtor22ResultContainer: Ctor22ResultContainer = {
    val ctor = Type.Ctor22.of[Arity22]
    new Ctor22ResultContainer(ctor.asInstanceOf[Type.Ctor22[Arity22]])
  }

  /** Helper providing the CtorLikeOf-like pattern for tests.
    *
    * The `Result` Ctor1 is passed in via constructor to avoid circular initialization in Scala 3. The type parameter is
    * erased at runtime, so the `Option` in `Type.Ctor1[Option]` is just a placeholder.
    */
  private class CtorResultContainer(ctor: Type.Ctor1[Option]) {
    type Result[X] = Either[String, X]
    @hearth.typed.ImportedCrossTypeImplicit
    implicit val Result: Type.Ctor1[Result] = ctor.asInstanceOf[Type.Ctor1[Result]]
  }

  private class Ctor2ResultContainer(ctor: Type.Ctor2[Either]) {
    type Result[X, Y] = Arity3[String, X, Y]
    @hearth.typed.ImportedCrossTypeImplicit
    implicit val Result: Type.Ctor2[Result] = ctor.asInstanceOf[Type.Ctor2[Result]]
  }

  private class Ctor3ResultContainer(ctor: Type.Ctor3[Arity3]) {
    type Result[B, C, D] = Arity4[String, B, C, D]
    @hearth.typed.ImportedCrossTypeImplicit
    implicit val Result: Type.Ctor3[Result] = ctor.asInstanceOf[Type.Ctor3[Result]]
  }

  private class Ctor4ResultContainer(ctor: Type.Ctor4[Arity4]) {
    type Result[B, C, D, E] = Arity5[String, B, C, D, E]
    @hearth.typed.ImportedCrossTypeImplicit
    implicit val Result: Type.Ctor4[Result] = ctor.asInstanceOf[Type.Ctor4[Result]]
  }

  private class Ctor5ResultContainer(ctor: Type.Ctor5[Arity5]) {
    type Result[B, C, D, E, F] = Arity6[String, B, C, D, E, F]
    @hearth.typed.ImportedCrossTypeImplicit
    implicit val Result: Type.Ctor5[Result] = ctor.asInstanceOf[Type.Ctor5[Result]]
  }

  private class Ctor6ResultContainer(ctor: Type.Ctor6[Arity6]) {
    type Result[B, C, D, E, F, G] = Arity7[String, B, C, D, E, F, G]
    @hearth.typed.ImportedCrossTypeImplicit
    implicit val Result: Type.Ctor6[Result] = ctor.asInstanceOf[Type.Ctor6[Result]]
  }

  private class Ctor7ResultContainer(ctor: Type.Ctor7[Arity7]) {
    type Result[B, C, D, E, F, G, H] = Arity8[String, B, C, D, E, F, G, H]
    @hearth.typed.ImportedCrossTypeImplicit
    implicit val Result: Type.Ctor7[Result] = ctor.asInstanceOf[Type.Ctor7[Result]]
  }

  private class Ctor8ResultContainer(ctor: Type.Ctor8[Arity8]) {
    type Result[B, C, D, E, F, G, H, I] = Arity9[String, B, C, D, E, F, G, H, I]
    @hearth.typed.ImportedCrossTypeImplicit
    implicit val Result: Type.Ctor8[Result] = ctor.asInstanceOf[Type.Ctor8[Result]]
  }

  private class Ctor9ResultContainer(ctor: Type.Ctor9[Arity9]) {
    type Result[B, C, D, E, F, G, H, I, J] = Arity10[String, B, C, D, E, F, G, H, I, J]
    @hearth.typed.ImportedCrossTypeImplicit
    implicit val Result: Type.Ctor9[Result] = ctor.asInstanceOf[Type.Ctor9[Result]]
  }

  private class Ctor10ResultContainer(ctor: Type.Ctor10[Arity10]) {
    type Result[B, C, D, E, F, G, H, I, J, K] = Arity11[String, B, C, D, E, F, G, H, I, J, K]
    @hearth.typed.ImportedCrossTypeImplicit
    implicit val Result: Type.Ctor10[Result] = ctor.asInstanceOf[Type.Ctor10[Result]]
  }

  private class Ctor11ResultContainer(ctor: Type.Ctor11[Arity11]) {
    type Result[B, C, D, E, F, G, H, I, J, K, L] = Arity12[String, B, C, D, E, F, G, H, I, J, K, L]
    @hearth.typed.ImportedCrossTypeImplicit
    implicit val Result: Type.Ctor11[Result] = ctor.asInstanceOf[Type.Ctor11[Result]]
  }

  private class Ctor12ResultContainer(ctor: Type.Ctor12[Arity12]) {
    type Result[B, C, D, E, F, G, H, I, J, K, L, M] = Arity13[String, B, C, D, E, F, G, H, I, J, K, L, M]
    @hearth.typed.ImportedCrossTypeImplicit
    implicit val Result: Type.Ctor12[Result] = ctor.asInstanceOf[Type.Ctor12[Result]]
  }

  private class Ctor13ResultContainer(ctor: Type.Ctor13[Arity13]) {
    type Result[B, C, D, E, F, G, H, I, J, K, L, M, N] = Arity14[String, B, C, D, E, F, G, H, I, J, K, L, M, N]
    @hearth.typed.ImportedCrossTypeImplicit
    implicit val Result: Type.Ctor13[Result] = ctor.asInstanceOf[Type.Ctor13[Result]]
  }

  private class Ctor14ResultContainer(ctor: Type.Ctor14[Arity14]) {
    type Result[B, C, D, E, F, G, H, I, J, K, L, M, N, O] = Arity15[String, B, C, D, E, F, G, H, I, J, K, L, M, N, O]
    @hearth.typed.ImportedCrossTypeImplicit
    implicit val Result: Type.Ctor14[Result] = ctor.asInstanceOf[Type.Ctor14[Result]]
  }

  private class Ctor15ResultContainer(ctor: Type.Ctor15[Arity15]) {
    type Result[B, C, D, E, F, G, H, I, J, K, L, M, N, O, P] =
      Arity16[String, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P]
    @hearth.typed.ImportedCrossTypeImplicit
    implicit val Result: Type.Ctor15[Result] = ctor.asInstanceOf[Type.Ctor15[Result]]
  }

  private class Ctor16ResultContainer(ctor: Type.Ctor16[Arity16]) {
    type Result[B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q] =
      Arity17[String, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q]
    @hearth.typed.ImportedCrossTypeImplicit
    implicit val Result: Type.Ctor16[Result] = ctor.asInstanceOf[Type.Ctor16[Result]]
  }

  private class Ctor17ResultContainer(ctor: Type.Ctor17[Arity17]) {
    type Result[B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R] =
      Arity18[String, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R]
    @hearth.typed.ImportedCrossTypeImplicit
    implicit val Result: Type.Ctor17[Result] = ctor.asInstanceOf[Type.Ctor17[Result]]
  }

  private class Ctor18ResultContainer(ctor: Type.Ctor18[Arity18]) {
    type Result[B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S] =
      Arity19[String, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S]
    @hearth.typed.ImportedCrossTypeImplicit
    implicit val Result: Type.Ctor18[Result] = ctor.asInstanceOf[Type.Ctor18[Result]]
  }

  private class Ctor19ResultContainer(ctor: Type.Ctor19[Arity19]) {
    type Result[B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T] =
      Arity20[String, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T]
    @hearth.typed.ImportedCrossTypeImplicit
    implicit val Result: Type.Ctor19[Result] = ctor.asInstanceOf[Type.Ctor19[Result]]
  }

  private class Ctor20ResultContainer(ctor: Type.Ctor20[Arity20]) {
    type Result[B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U] =
      Arity21[String, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U]
    @hearth.typed.ImportedCrossTypeImplicit
    implicit val Result: Type.Ctor20[Result] = ctor.asInstanceOf[Type.Ctor20[Result]]
  }

  private class Ctor21ResultContainer(ctor: Type.Ctor21[Arity21]) {
    type Result[B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V] =
      Arity22[String, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V]
    @hearth.typed.ImportedCrossTypeImplicit
    implicit val Result: Type.Ctor21[Result] = ctor.asInstanceOf[Type.Ctor21[Result]]
  }

  private class Ctor22ResultContainer(ctor: Type.Ctor22[Arity22]) {
    type Result[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V] =
      Arity22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V]
    @hearth.typed.ImportedCrossTypeImplicit
    implicit val Result: Type.Ctor22[Result] = ctor.asInstanceOf[Type.Ctor22[Result]]
  }

  /** Test: Type.CtorN.fromUntyped roundtrip - of -> asUntyped -> fromUntyped -> verify. */
  def testCtorFromUntyped: Expr[Data] = {
    val optionOf = Type.Ctor1.of[Option]
    val optionUntyped = optionOf.asUntyped
    val optionFrom = Type.Ctor1.fromUntyped[Option](optionUntyped)
    val eitherOf = Type.Ctor2.of[Either]
    val eitherUntyped = eitherOf.asUntyped
    val eitherFrom = Type.Ctor2.fromUntyped[Either](eitherUntyped)
    val arity3Of = Type.Ctor3.of[Arity3]
    val arity3Untyped = arity3Of.asUntyped
    val arity3From = Type.Ctor3.fromUntyped[Arity3](arity3Untyped)
    val arity4Of = Type.Ctor4.of[Arity4]
    val arity4Untyped = arity4Of.asUntyped
    val arity4From = Type.Ctor4.fromUntyped[Arity4](arity4Untyped)
    val arity5Of = Type.Ctor5.of[Arity5]
    val arity5Untyped = arity5Of.asUntyped
    val arity5From = Type.Ctor5.fromUntyped[Arity5](arity5Untyped)
    val arity6Of = Type.Ctor6.of[Arity6]
    val arity6Untyped = arity6Of.asUntyped
    val arity6From = Type.Ctor6.fromUntyped[Arity6](arity6Untyped)
    val arity7Of = Type.Ctor7.of[Arity7]
    val arity7Untyped = arity7Of.asUntyped
    val arity7From = Type.Ctor7.fromUntyped[Arity7](arity7Untyped)
    val arity8Of = Type.Ctor8.of[Arity8]
    val arity8Untyped = arity8Of.asUntyped
    val arity8From = Type.Ctor8.fromUntyped[Arity8](arity8Untyped)
    val arity9Of = Type.Ctor9.of[Arity9]
    val arity9Untyped = arity9Of.asUntyped
    val arity9From = Type.Ctor9.fromUntyped[Arity9](arity9Untyped)
    val arity10Of = Type.Ctor10.of[Arity10]
    val arity10Untyped = arity10Of.asUntyped
    val arity10From = Type.Ctor10.fromUntyped[Arity10](arity10Untyped)
    val arity11Of = Type.Ctor11.of[Arity11]
    val arity11Untyped = arity11Of.asUntyped
    val arity11From = Type.Ctor11.fromUntyped[Arity11](arity11Untyped)
    val arity12Of = Type.Ctor12.of[Arity12]
    val arity12Untyped = arity12Of.asUntyped
    val arity12From = Type.Ctor12.fromUntyped[Arity12](arity12Untyped)
    val arity13Of = Type.Ctor13.of[Arity13]
    val arity13Untyped = arity13Of.asUntyped
    val arity13From = Type.Ctor13.fromUntyped[Arity13](arity13Untyped)
    val arity14Of = Type.Ctor14.of[Arity14]
    val arity14Untyped = arity14Of.asUntyped
    val arity14From = Type.Ctor14.fromUntyped[Arity14](arity14Untyped)
    val arity15Of = Type.Ctor15.of[Arity15]
    val arity15Untyped = arity15Of.asUntyped
    val arity15From = Type.Ctor15.fromUntyped[Arity15](arity15Untyped)
    val arity16Of = Type.Ctor16.of[Arity16]
    val arity16Untyped = arity16Of.asUntyped
    val arity16From = Type.Ctor16.fromUntyped[Arity16](arity16Untyped)
    val arity17Of = Type.Ctor17.of[Arity17]
    val arity17Untyped = arity17Of.asUntyped
    val arity17From = Type.Ctor17.fromUntyped[Arity17](arity17Untyped)
    val arity18Of = Type.Ctor18.of[Arity18]
    val arity18Untyped = arity18Of.asUntyped
    val arity18From = Type.Ctor18.fromUntyped[Arity18](arity18Untyped)
    val arity19Of = Type.Ctor19.of[Arity19]
    val arity19Untyped = arity19Of.asUntyped
    val arity19From = Type.Ctor19.fromUntyped[Arity19](arity19Untyped)
    val arity20Of = Type.Ctor20.of[Arity20]
    val arity20Untyped = arity20Of.asUntyped
    val arity20From = Type.Ctor20.fromUntyped[Arity20](arity20Untyped)
    val arity21Of = Type.Ctor21.of[Arity21]
    val arity21Untyped = arity21Of.asUntyped
    val arity21From = Type.Ctor21.fromUntyped[Arity21](arity21Untyped)
    val arity22Of = Type.Ctor22.of[Arity22]
    val arity22Untyped = arity22Of.asUntyped
    val arity22From = Type.Ctor22.fromUntyped[Arity22](arity22Untyped)

    Expr(
      Data.map(
        "optionApply" -> Data(optionFrom.apply[Int](using Type.of[Int]).plainPrint),
        "optionUnapplyMatch" -> Data(optionFrom.unapply(Type.of[Option[String]]).isDefined.toString),
        "optionUnapplyNoMatch" -> Data(optionFrom.unapply(Type.of[List[String]]).isDefined.toString),
        "optionAsUntyped" -> Data(UntypedType.plainPrint(optionFrom.asUntyped)),
        "eitherApply" -> Data(eitherFrom.apply[String, Int](using Type.of[String], Type.of[Int]).plainPrint),
        "eitherUnapplyMatch" -> Data(eitherFrom.unapply(Type.of[Either[String, Int]]).isDefined.toString),
        "eitherUnapplyNoMatch" -> Data(eitherFrom.unapply(Type.of[Option[String]]).isDefined.toString),
        "eitherAsUntyped" -> Data(UntypedType.plainPrint(eitherFrom.asUntyped)),
        "arity3Apply" -> Data(
          arity3From.apply[Int, Int, Int](using Type.of[Int], Type.of[Int], Type.of[Int]).plainPrint
        ),
        "arity3UnapplyMatch" -> Data(arity3From.unapply(Type.of[Arity3[Int, Int, Int]]).isDefined.toString),
        "arity3UnapplyNoMatch" -> Data(arity3From.unapply(Type.of[Option[String]]).isDefined.toString),
        "arity3AsUntyped" -> Data(UntypedType.plainPrint(arity3From.asUntyped)),
        "arity4Apply" -> Data(
          arity4From.apply[Int, Int, Int, Int](using Type.of[Int], Type.of[Int], Type.of[Int], Type.of[Int]).plainPrint
        ),
        "arity4UnapplyMatch" -> Data(arity4From.unapply(Type.of[Arity4[Int, Int, Int, Int]]).isDefined.toString),
        "arity4UnapplyNoMatch" -> Data(arity4From.unapply(Type.of[Option[String]]).isDefined.toString),
        "arity4AsUntyped" -> Data(UntypedType.plainPrint(arity4From.asUntyped)),
        "arity5Apply" -> Data(
          arity5From
            .apply[Int, Int, Int, Int, Int](using Type.of[Int], Type.of[Int], Type.of[Int], Type.of[Int], Type.of[Int])
            .plainPrint
        ),
        "arity5UnapplyMatch" -> Data(arity5From.unapply(Type.of[Arity5[Int, Int, Int, Int, Int]]).isDefined.toString),
        "arity5UnapplyNoMatch" -> Data(arity5From.unapply(Type.of[Option[String]]).isDefined.toString),
        "arity5AsUntyped" -> Data(UntypedType.plainPrint(arity5From.asUntyped)),
        "arity6Apply" -> Data(
          arity6From
            .apply[Int, Int, Int, Int, Int, Int](using
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int]
            )
            .plainPrint
        ),
        "arity6UnapplyMatch" -> Data(
          arity6From.unapply(Type.of[Arity6[Int, Int, Int, Int, Int, Int]]).isDefined.toString
        ),
        "arity6UnapplyNoMatch" -> Data(arity6From.unapply(Type.of[Option[String]]).isDefined.toString),
        "arity6AsUntyped" -> Data(UntypedType.plainPrint(arity6From.asUntyped)),
        "arity7Apply" -> Data(
          arity7From
            .apply[Int, Int, Int, Int, Int, Int, Int](using
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int]
            )
            .plainPrint
        ),
        "arity7UnapplyMatch" -> Data(
          arity7From.unapply(Type.of[Arity7[Int, Int, Int, Int, Int, Int, Int]]).isDefined.toString
        ),
        "arity7UnapplyNoMatch" -> Data(arity7From.unapply(Type.of[Option[String]]).isDefined.toString),
        "arity7AsUntyped" -> Data(UntypedType.plainPrint(arity7From.asUntyped)),
        "arity8Apply" -> Data(
          arity8From
            .apply[Int, Int, Int, Int, Int, Int, Int, Int](using
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int]
            )
            .plainPrint
        ),
        "arity8UnapplyMatch" -> Data(
          arity8From.unapply(Type.of[Arity8[Int, Int, Int, Int, Int, Int, Int, Int]]).isDefined.toString
        ),
        "arity8UnapplyNoMatch" -> Data(arity8From.unapply(Type.of[Option[String]]).isDefined.toString),
        "arity8AsUntyped" -> Data(UntypedType.plainPrint(arity8From.asUntyped)),
        "arity9Apply" -> Data(
          arity9From
            .apply[Int, Int, Int, Int, Int, Int, Int, Int, Int](using
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int]
            )
            .plainPrint
        ),
        "arity9UnapplyMatch" -> Data(
          arity9From.unapply(Type.of[Arity9[Int, Int, Int, Int, Int, Int, Int, Int, Int]]).isDefined.toString
        ),
        "arity9UnapplyNoMatch" -> Data(arity9From.unapply(Type.of[Option[String]]).isDefined.toString),
        "arity9AsUntyped" -> Data(UntypedType.plainPrint(arity9From.asUntyped)),
        "arity10Apply" -> Data(
          arity10From
            .apply[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int](using
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int]
            )
            .plainPrint
        ),
        "arity10UnapplyMatch" -> Data(
          arity10From.unapply(Type.of[Arity10[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]]).isDefined.toString
        ),
        "arity10UnapplyNoMatch" -> Data(arity10From.unapply(Type.of[Option[String]]).isDefined.toString),
        "arity10AsUntyped" -> Data(UntypedType.plainPrint(arity10From.asUntyped)),
        "arity11Apply" -> Data(
          arity11From
            .apply[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int](using
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int]
            )
            .plainPrint
        ),
        "arity11UnapplyMatch" -> Data(
          arity11From
            .unapply(Type.of[Arity11[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]])
            .isDefined
            .toString
        ),
        "arity11UnapplyNoMatch" -> Data(arity11From.unapply(Type.of[Option[String]]).isDefined.toString),
        "arity11AsUntyped" -> Data(UntypedType.plainPrint(arity11From.asUntyped)),
        "arity12Apply" -> Data(
          arity12From
            .apply[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int](
              using
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int]
            )
            .plainPrint
        ),
        "arity12UnapplyMatch" -> Data(
          arity12From
            .unapply(Type.of[Arity12[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]])
            .isDefined
            .toString
        ),
        "arity12UnapplyNoMatch" -> Data(arity12From.unapply(Type.of[Option[String]]).isDefined.toString),
        "arity12AsUntyped" -> Data(UntypedType.plainPrint(arity12From.asUntyped)),
        "arity13Apply" -> Data(
          arity13From
            .apply[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int](
              using
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int]
            )
            .plainPrint
        ),
        "arity13UnapplyMatch" -> Data(
          arity13From
            .unapply(Type.of[Arity13[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]])
            .isDefined
            .toString
        ),
        "arity13UnapplyNoMatch" -> Data(arity13From.unapply(Type.of[Option[String]]).isDefined.toString),
        "arity13AsUntyped" -> Data(UntypedType.plainPrint(arity13From.asUntyped)),
        "arity14Apply" -> Data(
          arity14From
            .apply[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int](
              using
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int]
            )
            .plainPrint
        ),
        "arity14UnapplyMatch" -> Data(
          arity14From
            .unapply(Type.of[Arity14[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]])
            .isDefined
            .toString
        ),
        "arity14UnapplyNoMatch" -> Data(arity14From.unapply(Type.of[Option[String]]).isDefined.toString),
        "arity14AsUntyped" -> Data(UntypedType.plainPrint(arity14From.asUntyped)),
        "arity15Apply" -> Data(
          arity15From
            .apply[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int](
              using
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int]
            )
            .plainPrint
        ),
        "arity15UnapplyMatch" -> Data(
          arity15From
            .unapply(Type.of[Arity15[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]])
            .isDefined
            .toString
        ),
        "arity15UnapplyNoMatch" -> Data(arity15From.unapply(Type.of[Option[String]]).isDefined.toString),
        "arity15AsUntyped" -> Data(UntypedType.plainPrint(arity15From.asUntyped)),
        "arity16Apply" -> Data(
          arity16From
            .apply[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int](
              using
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int]
            )
            .plainPrint
        ),
        "arity16UnapplyMatch" -> Data(
          arity16From
            .unapply(Type.of[Arity16[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]])
            .isDefined
            .toString
        ),
        "arity16UnapplyNoMatch" -> Data(arity16From.unapply(Type.of[Option[String]]).isDefined.toString),
        "arity16AsUntyped" -> Data(UntypedType.plainPrint(arity16From.asUntyped)),
        "arity17Apply" -> Data(
          arity17From
            .apply[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int](
              using
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int]
            )
            .plainPrint
        ),
        "arity17UnapplyMatch" -> Data(
          arity17From
            .unapply(
              Type.of[Arity17[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]]
            )
            .isDefined
            .toString
        ),
        "arity17UnapplyNoMatch" -> Data(arity17From.unapply(Type.of[Option[String]]).isDefined.toString),
        "arity17AsUntyped" -> Data(UntypedType.plainPrint(arity17From.asUntyped)),
        "arity18Apply" -> Data(
          arity18From
            .apply[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int](
              using
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int]
            )
            .plainPrint
        ),
        "arity18UnapplyMatch" -> Data(
          arity18From
            .unapply(
              Type.of[Arity18[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]]
            )
            .isDefined
            .toString
        ),
        "arity18UnapplyNoMatch" -> Data(arity18From.unapply(Type.of[Option[String]]).isDefined.toString),
        "arity18AsUntyped" -> Data(UntypedType.plainPrint(arity18From.asUntyped)),
        "arity19Apply" -> Data(
          arity19From
            .apply[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int](
              using
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int]
            )
            .plainPrint
        ),
        "arity19UnapplyMatch" -> Data(
          arity19From
            .unapply(
              Type.of[
                Arity19[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]
              ]
            )
            .isDefined
            .toString
        ),
        "arity19UnapplyNoMatch" -> Data(arity19From.unapply(Type.of[Option[String]]).isDefined.toString),
        "arity19AsUntyped" -> Data(UntypedType.plainPrint(arity19From.asUntyped)),
        "arity20Apply" -> Data(
          arity20From
            .apply[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int](
              using
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int]
            )
            .plainPrint
        ),
        "arity20UnapplyMatch" -> Data(
          arity20From
            .unapply(
              Type.of[Arity20[
                Int,
                Int,
                Int,
                Int,
                Int,
                Int,
                Int,
                Int,
                Int,
                Int,
                Int,
                Int,
                Int,
                Int,
                Int,
                Int,
                Int,
                Int,
                Int,
                Int
              ]]
            )
            .isDefined
            .toString
        ),
        "arity20UnapplyNoMatch" -> Data(arity20From.unapply(Type.of[Option[String]]).isDefined.toString),
        "arity20AsUntyped" -> Data(UntypedType.plainPrint(arity20From.asUntyped)),
        "arity21Apply" -> Data(
          arity21From
            .apply[
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int
            ](
              using
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int]
            )
            .plainPrint
        ),
        "arity21UnapplyMatch" -> Data(
          arity21From
            .unapply(
              Type.of[Arity21[
                Int,
                Int,
                Int,
                Int,
                Int,
                Int,
                Int,
                Int,
                Int,
                Int,
                Int,
                Int,
                Int,
                Int,
                Int,
                Int,
                Int,
                Int,
                Int,
                Int,
                Int
              ]]
            )
            .isDefined
            .toString
        ),
        "arity21UnapplyNoMatch" -> Data(arity21From.unapply(Type.of[Option[String]]).isDefined.toString),
        "arity21AsUntyped" -> Data(UntypedType.plainPrint(arity21From.asUntyped)),
        "arity22Apply" -> Data(
          arity22From
            .apply[
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int
            ](
              using
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int]
            )
            .plainPrint
        ),
        "arity22UnapplyMatch" -> Data(
          arity22From
            .unapply(
              Type.of[Arity22[
                Int,
                Int,
                Int,
                Int,
                Int,
                Int,
                Int,
                Int,
                Int,
                Int,
                Int,
                Int,
                Int,
                Int,
                Int,
                Int,
                Int,
                Int,
                Int,
                Int,
                Int,
                Int
              ]]
            )
            .isDefined
            .toString
        ),
        "arity22UnapplyNoMatch" -> Data(arity22From.unapply(Type.of[Option[String]]).isDefined.toString),
        "arity22AsUntyped" -> Data(UntypedType.plainPrint(arity22From.asUntyped))
      )
    )
  }

  /** Verify a Ctor1[Option] that was constructed via fromUntyped using a platform-extracted UntypedType. */
  def verifyExtractedCtor1Option(ctor: Type.Ctor1[Option]): Expr[Data] =
    Expr(
      Data.map(
        "apply" -> Data(ctor.apply[Int](using Type.of[Int]).plainPrint),
        "unapplyMatch" -> Data(ctor.unapply(Type.of[Option[String]]).isDefined.toString),
        "unapplyNoMatch" -> Data(ctor.unapply(Type.of[List[String]]).isDefined.toString)
      )
    )

  /** Verify a Ctor2[Either] that was constructed via fromUntyped using a platform-extracted UntypedType. */
  def verifyExtractedCtor2Either(ctor: Type.Ctor2[Either]): Expr[Data] =
    Expr(
      Data.map(
        "apply" -> Data(ctor.apply[String, Int](using Type.of[String], Type.of[Int]).plainPrint),
        "unapplyMatch" -> Data(ctor.unapply(Type.of[Either[String, Int]]).isDefined.toString),
        "unapplyNoMatch" -> Data(ctor.unapply(Type.of[Option[String]]).isDefined.toString)
      )
    )

  /** Verify a Ctor3[Arity3] that was constructed via fromUntyped using a platform-extracted UntypedType. */
  def verifyExtractedCtor3Arity3(ctor: Type.Ctor3[Arity3]): Expr[Data] =
    Expr(
      Data.map(
        "apply" -> Data(
          ctor.apply[Int, String, Boolean](using Type.of[Int], Type.of[String], Type.of[Boolean]).plainPrint
        ),
        "unapplyMatch" -> Data(ctor.unapply(Type.of[Arity3[Int, String, Boolean]]).isDefined.toString),
        "unapplyNoMatch" -> Data(ctor.unapply(Type.of[Option[String]]).isDefined.toString)
      )
    )

  /** Verify a Ctor4[Arity4] that was constructed via fromUntyped using a platform-extracted UntypedType. */
  def verifyExtractedCtor4Arity4(ctor: Type.Ctor4[Arity4]): Expr[Data] =
    Expr(
      Data.map(
        "apply" -> Data(
          ctor.apply[Int, Int, Int, Int](using Type.of[Int], Type.of[Int], Type.of[Int], Type.of[Int]).plainPrint
        ),
        "unapplyMatch" -> Data(ctor.unapply(Type.of[Arity4[Int, Int, Int, Int]]).isDefined.toString),
        "unapplyNoMatch" -> Data(ctor.unapply(Type.of[Option[String]]).isDefined.toString)
      )
    )

  /** Verify a Ctor5[Arity5] that was constructed via fromUntyped using a platform-extracted UntypedType. */
  def verifyExtractedCtor5Arity5(ctor: Type.Ctor5[Arity5]): Expr[Data] =
    Expr(
      Data.map(
        "apply" -> Data(
          ctor
            .apply[Int, Int, Int, Int, Int](using Type.of[Int], Type.of[Int], Type.of[Int], Type.of[Int], Type.of[Int])
            .plainPrint
        ),
        "unapplyMatch" -> Data(ctor.unapply(Type.of[Arity5[Int, Int, Int, Int, Int]]).isDefined.toString),
        "unapplyNoMatch" -> Data(ctor.unapply(Type.of[Option[String]]).isDefined.toString)
      )
    )

  /** Verify a Ctor6[Arity6] that was constructed via fromUntyped using a platform-extracted UntypedType. */
  def verifyExtractedCtor6Arity6(ctor: Type.Ctor6[Arity6]): Expr[Data] =
    Expr(
      Data.map(
        "apply" -> Data(
          ctor
            .apply[Int, Int, Int, Int, Int, Int](using
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int]
            )
            .plainPrint
        ),
        "unapplyMatch" -> Data(ctor.unapply(Type.of[Arity6[Int, Int, Int, Int, Int, Int]]).isDefined.toString),
        "unapplyNoMatch" -> Data(ctor.unapply(Type.of[Option[String]]).isDefined.toString)
      )
    )

  /** Verify a Ctor7[Arity7] that was constructed via fromUntyped using a platform-extracted UntypedType. */
  def verifyExtractedCtor7Arity7(ctor: Type.Ctor7[Arity7]): Expr[Data] =
    Expr(
      Data.map(
        "apply" -> Data(
          ctor
            .apply[Int, Int, Int, Int, Int, Int, Int](using
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int]
            )
            .plainPrint
        ),
        "unapplyMatch" -> Data(
          ctor.unapply(Type.of[Arity7[Int, Int, Int, Int, Int, Int, Int]]).isDefined.toString
        ),
        "unapplyNoMatch" -> Data(ctor.unapply(Type.of[Option[String]]).isDefined.toString)
      )
    )

  /** Verify a Ctor8[Arity8] that was constructed via fromUntyped using a platform-extracted UntypedType. */
  def verifyExtractedCtor8Arity8(ctor: Type.Ctor8[Arity8]): Expr[Data] =
    Expr(
      Data.map(
        "apply" -> Data(
          ctor
            .apply[Int, Int, Int, Int, Int, Int, Int, Int](using
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int]
            )
            .plainPrint
        ),
        "unapplyMatch" -> Data(
          ctor.unapply(Type.of[Arity8[Int, Int, Int, Int, Int, Int, Int, Int]]).isDefined.toString
        ),
        "unapplyNoMatch" -> Data(ctor.unapply(Type.of[Option[String]]).isDefined.toString)
      )
    )

  /** Verify a Ctor9[Arity9] that was constructed via fromUntyped using a platform-extracted UntypedType. */
  def verifyExtractedCtor9Arity9(ctor: Type.Ctor9[Arity9]): Expr[Data] =
    Expr(
      Data.map(
        "apply" -> Data(
          ctor
            .apply[Int, Int, Int, Int, Int, Int, Int, Int, Int](using
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int]
            )
            .plainPrint
        ),
        "unapplyMatch" -> Data(
          ctor
            .unapply(Type.of[Arity9[Int, Int, Int, Int, Int, Int, Int, Int, Int]])
            .isDefined
            .toString
        ),
        "unapplyNoMatch" -> Data(ctor.unapply(Type.of[Option[String]]).isDefined.toString)
      )
    )

  /** Verify a Ctor10[Arity10] that was constructed via fromUntyped using a platform-extracted UntypedType. */
  def verifyExtractedCtor10Arity10(ctor: Type.Ctor10[Arity10]): Expr[Data] =
    Expr(
      Data.map(
        "apply" -> Data(
          ctor
            .apply[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int](using
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int]
            )
            .plainPrint
        ),
        "unapplyMatch" -> Data(
          ctor
            .unapply(Type.of[Arity10[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]])
            .isDefined
            .toString
        ),
        "unapplyNoMatch" -> Data(ctor.unapply(Type.of[Option[String]]).isDefined.toString)
      )
    )

  /** Verify a Ctor11[Arity11] that was constructed via fromUntyped using a platform-extracted UntypedType. */
  def verifyExtractedCtor11Arity11(ctor: Type.Ctor11[Arity11]): Expr[Data] =
    Expr(
      Data.map(
        "apply" -> Data(
          ctor
            .apply[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int](using
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int]
            )
            .plainPrint
        ),
        "unapplyMatch" -> Data(
          ctor
            .unapply(Type.of[Arity11[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]])
            .isDefined
            .toString
        ),
        "unapplyNoMatch" -> Data(ctor.unapply(Type.of[Option[String]]).isDefined.toString)
      )
    )

  /** Verify a Ctor12[Arity12] that was constructed via fromUntyped using a platform-extracted UntypedType. */
  def verifyExtractedCtor12Arity12(ctor: Type.Ctor12[Arity12]): Expr[Data] =
    Expr(
      Data.map(
        "apply" -> Data(
          ctor
            .apply[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int](
              using
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int]
            )
            .plainPrint
        ),
        "unapplyMatch" -> Data(
          ctor
            .unapply(Type.of[Arity12[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]])
            .isDefined
            .toString
        ),
        "unapplyNoMatch" -> Data(ctor.unapply(Type.of[Option[String]]).isDefined.toString)
      )
    )

  /** Verify a Ctor13[Arity13] that was constructed via fromUntyped using a platform-extracted UntypedType. */
  def verifyExtractedCtor13Arity13(ctor: Type.Ctor13[Arity13]): Expr[Data] =
    Expr(
      Data.map(
        "apply" -> Data(
          ctor
            .apply[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int](
              using
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int]
            )
            .plainPrint
        ),
        "unapplyMatch" -> Data(
          ctor
            .unapply(Type.of[Arity13[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]])
            .isDefined
            .toString
        ),
        "unapplyNoMatch" -> Data(ctor.unapply(Type.of[Option[String]]).isDefined.toString)
      )
    )

  /** Verify a Ctor14[Arity14] that was constructed via fromUntyped using a platform-extracted UntypedType. */
  def verifyExtractedCtor14Arity14(ctor: Type.Ctor14[Arity14]): Expr[Data] =
    Expr(
      Data.map(
        "apply" -> Data(
          ctor
            .apply[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int](
              using
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int]
            )
            .plainPrint
        ),
        "unapplyMatch" -> Data(
          ctor
            .unapply(Type.of[Arity14[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]])
            .isDefined
            .toString
        ),
        "unapplyNoMatch" -> Data(ctor.unapply(Type.of[Option[String]]).isDefined.toString)
      )
    )

  /** Verify a Ctor15[Arity15] that was constructed via fromUntyped using a platform-extracted UntypedType. */
  def verifyExtractedCtor15Arity15(ctor: Type.Ctor15[Arity15]): Expr[Data] =
    Expr(
      Data.map(
        "apply" -> Data(
          ctor
            .apply[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int](
              using
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int]
            )
            .plainPrint
        ),
        "unapplyMatch" -> Data(
          ctor
            .unapply(
              Type.of[Arity15[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]]
            )
            .isDefined
            .toString
        ),
        "unapplyNoMatch" -> Data(ctor.unapply(Type.of[Option[String]]).isDefined.toString)
      )
    )

  /** Verify a Ctor16[Arity16] that was constructed via fromUntyped using a platform-extracted UntypedType. */
  def verifyExtractedCtor16Arity16(ctor: Type.Ctor16[Arity16]): Expr[Data] =
    Expr(
      Data.map(
        "apply" -> Data(
          ctor
            .apply[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int](
              using
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int]
            )
            .plainPrint
        ),
        "unapplyMatch" -> Data(
          ctor
            .unapply(
              Type.of[Arity16[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]]
            )
            .isDefined
            .toString
        ),
        "unapplyNoMatch" -> Data(ctor.unapply(Type.of[Option[String]]).isDefined.toString)
      )
    )

  /** Verify a Ctor17[Arity17] that was constructed via fromUntyped using a platform-extracted UntypedType. */
  def verifyExtractedCtor17Arity17(ctor: Type.Ctor17[Arity17]): Expr[Data] =
    Expr(
      Data.map(
        "apply" -> Data(
          ctor
            .apply[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int](
              using
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int]
            )
            .plainPrint
        ),
        "unapplyMatch" -> Data(
          ctor
            .unapply(
              Type.of[
                Arity17[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]
              ]
            )
            .isDefined
            .toString
        ),
        "unapplyNoMatch" -> Data(ctor.unapply(Type.of[Option[String]]).isDefined.toString)
      )
    )

  /** Verify a Ctor18[Arity18] that was constructed via fromUntyped using a platform-extracted UntypedType. */
  def verifyExtractedCtor18Arity18(ctor: Type.Ctor18[Arity18]): Expr[Data] =
    Expr(
      Data.map(
        "apply" -> Data(
          ctor
            .apply[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int](
              using
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int]
            )
            .plainPrint
        ),
        "unapplyMatch" -> Data(
          ctor
            .unapply(
              Type.of[
                Arity18[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]
              ]
            )
            .isDefined
            .toString
        ),
        "unapplyNoMatch" -> Data(ctor.unapply(Type.of[Option[String]]).isDefined.toString)
      )
    )

  /** Verify a Ctor19[Arity19] that was constructed via fromUntyped using a platform-extracted UntypedType. */
  def verifyExtractedCtor19Arity19(ctor: Type.Ctor19[Arity19]): Expr[Data] =
    Expr(
      Data.map(
        "apply" -> Data(
          ctor
            .apply[
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int
            ](
              using
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int]
            )
            .plainPrint
        ),
        "unapplyMatch" -> Data(
          ctor
            .unapply(
              Type.of[
                Arity19[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]
              ]
            )
            .isDefined
            .toString
        ),
        "unapplyNoMatch" -> Data(ctor.unapply(Type.of[Option[String]]).isDefined.toString)
      )
    )

  /** Verify a Ctor20[Arity20] that was constructed via fromUntyped using a platform-extracted UntypedType. */
  def verifyExtractedCtor20Arity20(ctor: Type.Ctor20[Arity20]): Expr[Data] =
    Expr(
      Data.map(
        "apply" -> Data(
          ctor
            .apply[
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int
            ](
              using
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int]
            )
            .plainPrint
        ),
        "unapplyMatch" -> Data(
          ctor
            .unapply(
              Type.of[
                Arity20[
                  Int,
                  Int,
                  Int,
                  Int,
                  Int,
                  Int,
                  Int,
                  Int,
                  Int,
                  Int,
                  Int,
                  Int,
                  Int,
                  Int,
                  Int,
                  Int,
                  Int,
                  Int,
                  Int,
                  Int
                ]
              ]
            )
            .isDefined
            .toString
        ),
        "unapplyNoMatch" -> Data(ctor.unapply(Type.of[Option[String]]).isDefined.toString)
      )
    )

  /** Verify a Ctor21[Arity21] that was constructed via fromUntyped using a platform-extracted UntypedType. */
  def verifyExtractedCtor21Arity21(ctor: Type.Ctor21[Arity21]): Expr[Data] =
    Expr(
      Data.map(
        "apply" -> Data(
          ctor
            .apply[
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int
            ](
              using
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int]
            )
            .plainPrint
        ),
        "unapplyMatch" -> Data(
          ctor
            .unapply(
              Type.of[
                Arity21[
                  Int,
                  Int,
                  Int,
                  Int,
                  Int,
                  Int,
                  Int,
                  Int,
                  Int,
                  Int,
                  Int,
                  Int,
                  Int,
                  Int,
                  Int,
                  Int,
                  Int,
                  Int,
                  Int,
                  Int,
                  Int
                ]
              ]
            )
            .isDefined
            .toString
        ),
        "unapplyNoMatch" -> Data(ctor.unapply(Type.of[Option[String]]).isDefined.toString)
      )
    )

  /** Verify a Ctor22[Arity22] that was constructed via fromUntyped using a platform-extracted UntypedType. */
  def verifyExtractedCtor22Arity22(ctor: Type.Ctor22[Arity22]): Expr[Data] =
    Expr(
      Data.map(
        "apply" -> Data(
          ctor
            .apply[
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int
            ](
              using
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int],
              Type.of[Int]
            )
            .plainPrint
        ),
        "unapplyMatch" -> Data(
          ctor
            .unapply(
              Type.of[Arity22[
                Int,
                Int,
                Int,
                Int,
                Int,
                Int,
                Int,
                Int,
                Int,
                Int,
                Int,
                Int,
                Int,
                Int,
                Int,
                Int,
                Int,
                Int,
                Int,
                Int,
                Int,
                Int
              ]]
            )
            .isDefined
            .toString
        ),
        "unapplyNoMatch" -> Data(ctor.unapply(Type.of[Option[String]]).isDefined.toString)
      )
    )
}
