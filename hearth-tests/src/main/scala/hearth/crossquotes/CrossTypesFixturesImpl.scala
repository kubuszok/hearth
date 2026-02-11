package hearth
package crossquotes

import hearth.data.Data

/** Fixtures for testing [[CrossTypesSpec]]. */
trait CrossTypesFixturesImpl { this: MacroTypedCommons =>

  def testTypeOf[A: Type]: Expr[Data] = {
    import hearth.examples.classes.{ExampleTrait, ExampleTraitWithTypeParam}
    Expr(
      Data.map(
        // Scala 2 types in quasiquotes required fcqn or their resolution would fail, or resolve to
        // some local definition ofershading the intended one. We should make sure that if the type is
        // unambiguosuly resolved during the macro compilation, it would be resolved to the same type during its expansion.
        "resolvingType" -> Data.map(
          "ofImportedType" -> Data(Type.of[ExampleTrait].plainPrint),
          "ofImportedTypeWithTypeParam" -> Data(Type.of[ExampleTraitWithTypeParam[A]].plainPrint),
          "ofRelativePathType" -> Data(Type.of[examples.classes.ExampleClass].plainPrint),
          "ofRelativePathWithTypeParamType" -> Data(Type.of[examples.classes.ExampleClassWithTypeParam[A]].plainPrint),
          "ofFqcnType" -> Data(Type.of[hearth.examples.classes.ExampleClass].plainPrint),
          "ofFqcnWithTypeParamType" -> Data(Type.of[hearth.examples.classes.ExampleClassWithTypeParam[A]].plainPrint)
        ),
        // We should make sure that:
        //  - type bound: [A: Type]
        //  - import: import a.Underlying as Name
        //  - declared val A: Type[A] = someType (declared locally or inherited)
        // all would be considered when building a type that uses A.
        "resolvingImplicitType" -> Data.map(
          "viaTypeBound" -> Data {
            def body[B: Type] = Type.of[Option[B]].plainPrint
            body[A]
          },
          "viaImplicitParam" -> Data {
            def body[B](implicit B: Type[B]) = Type.of[Option[B]].plainPrint
            body[A]
          },
          "viaImport" -> Data {
            def body(b: ??) = {
              // compiles on Scala 2 but with the wrong result (implicit is ignored, and compiler creates Type[b.Underlying])
              import b.Underlying as B
              Type.of[Option[B]].plainPrint
            }
            body(Type[A].as_??)
          },
          "viaDeclaredVal" -> Data {
            def body[B](tpe: Type[B]) = {
              implicit val B: Type[B] = tpe
              Type.of[Option[B]].plainPrint
            }
            body(Type[A])
          },
          "viaDeclaredDef" -> Data {
            case class Ctx[B](tpe: Type[B])
            implicit def fromCtx[B](implicit ctx: Ctx[B]): Type[B] = ctx.tpe
            def body[B: Ctx] = Type.of[Option[B]].plainPrint
            body(using Ctx(Type[A]))
          } // ,
          /* Potentially impossible to implement on Scala 3, since we cannot use Types to find which implicits would be needed.
          "viaInheritance" -> Data {
            val a = Type[A]
            trait HelperA {
              type B
              implicit val B: Type[B]
            }
            trait HelperB extends HelperA {
              def result =
                Type.of[Option[B]].plainPrint // fails to compile on Scala 3 (we aren't picking up the implicit Type[B])
            }
            class HelperC extends HelperB {
              type B = A
              implicit val B: Type[B] = a
            }
            (new HelperC).result
          }
           */
        )
      )
    )
  }

  private val Int = Type.of[Int]
  private val String = Type.of[String]

  private def try1[Tested, Ctor[_]](
      Tested: Type[Tested],
      Ctor: Type.Ctor1[Ctor]
  ) = Tested match {
    case Ctor(a) =>
      val Z = String
      Some(
        Data.map(
          "unapplied" -> Data.list(
            Data(a.Underlying.plainPrint)
          ),
          "reapplied" -> Data(
            Ctor(using Z).plainPrint
          )
        )
      )
    case _ => None
  }

  def testTypeCtor1[In: Type]: Expr[Data] = {
    val classResult = try1(Type[In], Type.Ctor1.of[examples.kinds.Arity1]) match {
      case Some(data) => Seq("as class" -> data)
      case None       => Seq.empty
    }
    val renamedResult = try1(Type[In], Type.Ctor1.of[examples.kinds.Alias.Renamed1]) match {
      case Some(data) => Seq("as renamed" -> data)
      case None       => Seq.empty
    }
    val extraResult = try1(Type[In], Type.Ctor1.of[examples.kinds.Alias.Extra1]) match {
      case Some(data) => Seq("as extra" -> data)
      case None       => Seq.empty
    }
    val fixedFrontResult = try1(Type[In], Type.Ctor1.of[examples.kinds.Alias.FixedFront1]) match {
      case Some(data) => Seq("as fixed front" -> data)
      case None       => Seq.empty
    }
    val fixedBackResult = try1(Type[In], Type.Ctor1.of[examples.kinds.Alias.FixedBack1]) match {
      case Some(data) => Seq("as fixed back" -> data)
      case None       => Seq.empty
    }
    // format: off
    val result =
      (classResult.view ++ renamedResult.view ++ extraResult.view ++ fixedFrontResult.view ++ fixedBackResult.view).toSeq
    // format: on
    if (result.isEmpty) Expr(Data("Not one of the expected types")) else Expr(Data.map(result*))
  }

  private def try2[Tested, Ctor[_, _]](
      Tested: Type[Tested],
      Ctor: Type.Ctor2[Ctor]
  ) = Tested match {
    case Ctor(a, b) =>
      val Z = String
      Some(
        Data.map(
          "unapplied" -> Data.list(
            Data(a.Underlying.plainPrint),
            Data(b.Underlying.plainPrint)
          ),
          "reapplied" -> Data(
            Ctor(using Z, Z).plainPrint
          )
        )
      )
    case _ => None
  }

  def testTypeCtor2[In: Type]: Expr[Data] = {
    val ctor = Type.Ctor2.of[examples.kinds.Arity2]
    val classResult = try2(Type[In], ctor) match {
      case Some(data) => Seq("as class" -> data)
      case None       => Seq.empty
    }
    val renamedResult = try2(Type[In], Type.Ctor2.of[examples.kinds.Alias.Renamed2]) match {
      case Some(data) => Seq("as renamed" -> data)
      case None       => Seq.empty
    }
    val extraResult = try2(Type[In], Type.Ctor2.of[examples.kinds.Alias.Extra2]) match {
      case Some(data) => Seq("as extra" -> data)
      case None       => Seq.empty
    }
    val fixedFrontResult = try2(Type[In], Type.Ctor2.of[examples.kinds.Alias.FixedFront2]) match {
      case Some(data) => Seq("as fixed front" -> data)
      case None       => Seq.empty
    }
    val fixedBackResult = try2(Type[In], Type.Ctor2.of[examples.kinds.Alias.FixedBack2]) match {
      case Some(data) => Seq("as fixed back" -> data)
      case None       => Seq.empty
    }
    val setA = try1(Type[In], ctor.setA(using Int)) match {
      case Some(data) => Seq("as setA" -> data)
      case None       => Seq.empty
    }
    val setB = try1(Type[In], ctor.setB(using Int)) match {
      case Some(data) => Seq("as setB" -> data)
      case None       => Seq.empty
    }
    // format: off
    val result =
      (classResult.view ++ renamedResult.view ++ extraResult.view ++ fixedFrontResult.view ++ fixedBackResult.view ++ setA.view ++ setB.view).toSeq
    // format: on
    if (result.isEmpty) Expr(Data("Not one of the expected types")) else Expr(Data.map(result*))
  }

  private def try3[Tested, Ctor[_, _, _]](
      Tested: Type[Tested],
      Ctor: Type.Ctor3[Ctor]
  ) = Tested match {
    case Ctor(a, b, c) =>
      val Z = String
      Some(
        Data.map(
          "unapplied" -> Data.list(
            Data(a.Underlying.plainPrint),
            Data(b.Underlying.plainPrint),
            Data(c.Underlying.plainPrint)
          ),
          "reapplied" -> Data(
            Ctor(using Z, Z, Z).plainPrint
          )
        )
      )
    case _ => None
  }

  def testTypeCtor3[In: Type]: Expr[Data] = {
    val ctor = Type.Ctor3.of[examples.kinds.Arity3]
    val classResult = try3(Type[In], ctor) match {
      case Some(data) => Seq("as class" -> data)
      case None       => Seq.empty
    }
    val renamedResult = try3(Type[In], Type.Ctor3.of[examples.kinds.Alias.Renamed3]) match {
      case Some(data) => Seq("as renamed" -> data)
      case None       => Seq.empty
    }
    val extraResult = try3(Type[In], Type.Ctor3.of[examples.kinds.Alias.Extra3]) match {
      case Some(data) => Seq("as extra" -> data)
      case None       => Seq.empty
    }
    val fixedFrontResult = try3(Type[In], Type.Ctor3.of[examples.kinds.Alias.FixedFront3]) match {
      case Some(data) => Seq("as fixed front" -> data)
      case None       => Seq.empty
    }
    val fixedBackResult = try3(Type[In], Type.Ctor3.of[examples.kinds.Alias.FixedBack3]) match {
      case Some(data) => Seq("as fixed back" -> data)
      case None       => Seq.empty
    }
    val setA = try2(Type[In], ctor.setA(using Int)) match {
      case Some(data) => Seq("as setA" -> data)
      case None       => Seq.empty
    }
    val setB = try2(Type[In], ctor.setB(using Int)) match {
      case Some(data) => Seq("as setB" -> data)
      case None       => Seq.empty
    }
    val setC = try2(Type[In], ctor.setC(using Int)) match {
      case Some(data) => Seq("as setC" -> data)
      case None       => Seq.empty
    }
    // format: off
    val result =
      (classResult.view ++ renamedResult.view ++ extraResult.view ++ fixedFrontResult.view ++ fixedBackResult.view ++ setA.view ++ setB.view ++ setC.view).toSeq
    // format: on
    if (result.isEmpty) Expr(Data("Not one of the expected types")) else Expr(Data.map(result*))
  }

  private def try4[Tested, Ctor[_, _, _, _]](
      Tested: Type[Tested],
      Ctor: Type.Ctor4[Ctor]
  ) = Tested match {
    case Ctor(a, b, c, d) =>
      val Z = String
      Some(
        Data.map(
          "unapplied" -> Data.list(
            Data(a.Underlying.plainPrint),
            Data(b.Underlying.plainPrint),
            Data(c.Underlying.plainPrint),
            Data(d.Underlying.plainPrint)
          ),
          "reapplied" -> Data(
            Ctor(using Z, Z, Z, Z).plainPrint
          )
        )
      )
    case _ => None
  }

  def testTypeCtor4[In: Type]: Expr[Data] = {
    val ctor = Type.Ctor4.of[examples.kinds.Arity4]
    val classResult = try4(Type[In], ctor) match {
      case Some(data) => Seq("as class" -> data)
      case None       => Seq.empty
    }
    val renamedResult = try4(Type[In], Type.Ctor4.of[examples.kinds.Alias.Renamed4]) match {
      case Some(data) => Seq("as renamed" -> data)
      case None       => Seq.empty
    }
    val extraResult = try4(Type[In], Type.Ctor4.of[examples.kinds.Alias.Extra4]) match {
      case Some(data) => Seq("as extra" -> data)
      case None       => Seq.empty
    }
    val fixedFrontResult = try4(Type[In], Type.Ctor4.of[examples.kinds.Alias.FixedFront4]) match {
      case Some(data) => Seq("as fixed front" -> data)
      case None       => Seq.empty
    }
    val fixedBackResult = try4(Type[In], Type.Ctor4.of[examples.kinds.Alias.FixedBack4]) match {
      case Some(data) => Seq("as fixed back" -> data)
      case None       => Seq.empty
    }
    val setA = try3(Type[In], ctor.setA(using Int)) match {
      case Some(data) => Seq("as setA" -> data)
      case None       => Seq.empty
    }
    val setB = try3(Type[In], ctor.setB(using Int)) match {
      case Some(data) => Seq("as setB" -> data)
      case None       => Seq.empty
    }
    val setC = try3(Type[In], ctor.setC(using Int)) match {
      case Some(data) => Seq("as setC" -> data)
      case None       => Seq.empty
    }
    val setD = try3(Type[In], ctor.setD(using Int)) match {
      case Some(data) => Seq("as setD" -> data)
      case None       => Seq.empty
    }
    // format: off
    val result =
      (classResult.view ++ renamedResult.view ++ extraResult.view ++ fixedFrontResult.view ++ fixedBackResult.view ++ setA.view ++ setB.view ++ setC.view ++ setD.view).toSeq
    // format: on
    if (result.isEmpty) Expr(Data("Not one of the expected types")) else Expr(Data.map(result*))
  }

  private def try5[Tested, Ctor[_, _, _, _, _]](
      Tested: Type[Tested],
      Ctor: Type.Ctor5[Ctor]
  ) = Tested match {
    case Ctor(a, b, c, d, e) =>
      val Z = String
      Some(
        Data.map(
          "unapplied" -> Data.list(
            Data(a.Underlying.plainPrint),
            Data(b.Underlying.plainPrint),
            Data(c.Underlying.plainPrint),
            Data(d.Underlying.plainPrint),
            Data(e.Underlying.plainPrint)
          ),
          "reapplied" -> Data(
            Ctor(using Z, Z, Z, Z, Z).plainPrint
          )
        )
      )
    case _ => None
  }

  def testTypeCtor5[In: Type]: Expr[Data] = {
    val ctor = Type.Ctor5.of[examples.kinds.Arity5]
    val classResult = try5(Type[In], ctor) match {
      case Some(data) => Seq("as class" -> data)
      case None       => Seq.empty
    }
    val renamedResult = try5(Type[In], Type.Ctor5.of[examples.kinds.Alias.Renamed5]) match {
      case Some(data) => Seq("as renamed" -> data)
      case None       => Seq.empty
    }
    val extraResult = try5(Type[In], Type.Ctor5.of[examples.kinds.Alias.Extra5]) match {
      case Some(data) => Seq("as extra" -> data)
      case None       => Seq.empty
    }
    val fixedFrontResult = try5(Type[In], Type.Ctor5.of[examples.kinds.Alias.FixedFront5]) match {
      case Some(data) => Seq("as fixed front" -> data)
      case None       => Seq.empty
    }
    val fixedBackResult = try5(Type[In], Type.Ctor5.of[examples.kinds.Alias.FixedBack5]) match {
      case Some(data) => Seq("as fixed back" -> data)
      case None       => Seq.empty
    }
    val setA = try4(Type[In], ctor.setA(using Int)) match {
      case Some(data) => Seq("as setA" -> data)
      case None       => Seq.empty
    }
    val setB = try4(Type[In], ctor.setB(using Int)) match {
      case Some(data) => Seq("as setB" -> data)
      case None       => Seq.empty
    }
    val setC = try4(Type[In], ctor.setC(using Int)) match {
      case Some(data) => Seq("as setC" -> data)
      case None       => Seq.empty
    }
    val setD = try4(Type[In], ctor.setD(using Int)) match {
      case Some(data) => Seq("as setD" -> data)
      case None       => Seq.empty
    }
    val setE = try4(Type[In], ctor.setE(using Int)) match {
      case Some(data) => Seq("as setE" -> data)
      case None       => Seq.empty
    }
    // format: off
    val result =
      (classResult.view ++ renamedResult.view ++ extraResult.view ++ fixedFrontResult.view ++ fixedBackResult.view ++ setA.view ++ setB.view ++ setC.view ++ setD.view ++ setE.view).toSeq
    // format: on
    if (result.isEmpty) Expr(Data("Not one of the expected types")) else Expr(Data.map(result*))
  }

  private def try6[Tested, Ctor[_, _, _, _, _, _]](
      Tested: Type[Tested],
      Ctor: Type.Ctor6[Ctor]
  ) = Tested match {
    case Ctor(a, b, c, d, e, f) =>
      val Z = String
      Some(
        Data.map(
          "unapplied" -> Data.list(
            Data(a.Underlying.plainPrint),
            Data(b.Underlying.plainPrint),
            Data(c.Underlying.plainPrint),
            Data(d.Underlying.plainPrint),
            Data(e.Underlying.plainPrint),
            Data(f.Underlying.plainPrint)
          ),
          "reapplied" -> Data(
            Ctor(using Z, Z, Z, Z, Z, Z).plainPrint
          )
        )
      )
    case _ => None
  }

  def testTypeCtor6[In: Type]: Expr[Data] = {
    val ctor = Type.Ctor6.of[examples.kinds.Arity6]
    val classResult = try6(Type[In], ctor) match {
      case Some(data) => Seq("as class" -> data)
      case None       => Seq.empty
    }
    val renamedResult = try6(Type[In], Type.Ctor6.of[examples.kinds.Alias.Renamed6]) match {
      case Some(data) => Seq("as renamed" -> data)
      case None       => Seq.empty
    }
    val extraResult = try6(Type[In], Type.Ctor6.of[examples.kinds.Alias.Extra6]) match {
      case Some(data) => Seq("as extra" -> data)
      case None       => Seq.empty
    }
    val fixedFrontResult = try6(Type[In], Type.Ctor6.of[examples.kinds.Alias.FixedFront6]) match {
      case Some(data) => Seq("as fixed front" -> data)
      case None       => Seq.empty
    }
    val fixedBackResult = try6(Type[In], Type.Ctor6.of[examples.kinds.Alias.FixedBack6]) match {
      case Some(data) => Seq("as fixed back" -> data)
      case None       => Seq.empty
    }
    val setA = try5(Type[In], ctor.setA(using Int)) match {
      case Some(data) => Seq("as setA" -> data)
      case None       => Seq.empty
    }
    val setB = try5(Type[In], ctor.setB(using Int)) match {
      case Some(data) => Seq("as setB" -> data)
      case None       => Seq.empty
    }
    val setC = try5(Type[In], ctor.setC(using Int)) match {
      case Some(data) => Seq("as setC" -> data)
      case None       => Seq.empty
    }
    val setD = try5(Type[In], ctor.setD(using Int)) match {
      case Some(data) => Seq("as setD" -> data)
      case None       => Seq.empty
    }
    val setE = try5(Type[In], ctor.setE(using Int)) match {
      case Some(data) => Seq("as setE" -> data)
      case None       => Seq.empty
    }
    val setF = try5(Type[In], ctor.setF(using Int)) match {
      case Some(data) => Seq("as setF" -> data)
      case None       => Seq.empty
    }
    // format: off
    val result =
      (classResult.view ++ renamedResult.view ++ extraResult.view ++ fixedFrontResult.view ++ fixedBackResult.view ++ setA.view ++ setB.view ++ setC.view ++ setD.view ++ setE.view ++ setF.view).toSeq
    // format: on
    if (result.isEmpty) Expr(Data("Not one of the expected types")) else Expr(Data.map(result*))
  }

  private def try7[Tested, Ctor[_, _, _, _, _, _, _]](
      Tested: Type[Tested],
      Ctor: Type.Ctor7[Ctor]
  ) = Tested match {
    case Ctor(a, b, c, d, e, f, g) =>
      val Z = String
      Some(
        Data.map(
          "unapplied" -> Data.list(
            Data(a.Underlying.plainPrint),
            Data(b.Underlying.plainPrint),
            Data(c.Underlying.plainPrint),
            Data(d.Underlying.plainPrint),
            Data(e.Underlying.plainPrint),
            Data(f.Underlying.plainPrint),
            Data(g.Underlying.plainPrint)
          ),
          "reapplied" -> Data(
            Ctor(using Z, Z, Z, Z, Z, Z, Z).plainPrint
          )
        )
      )
    case _ => None
  }

  def testTypeCtor7[In: Type]: Expr[Data] = {
    val ctor = Type.Ctor7.of[examples.kinds.Arity7]
    val classResult = try7(Type[In], ctor) match {
      case Some(data) => Seq("as class" -> data)
      case None       => Seq.empty
    }
    val renamedResult = try7(Type[In], Type.Ctor7.of[examples.kinds.Alias.Renamed7]) match {
      case Some(data) => Seq("as renamed" -> data)
      case None       => Seq.empty
    }
    val extraResult = try7(Type[In], Type.Ctor7.of[examples.kinds.Alias.Extra7]) match {
      case Some(data) => Seq("as extra" -> data)
      case None       => Seq.empty
    }
    val fixedFrontResult = try7(Type[In], Type.Ctor7.of[examples.kinds.Alias.FixedFront7]) match {
      case Some(data) => Seq("as fixed front" -> data)
      case None       => Seq.empty
    }
    val fixedBackResult = try7(Type[In], Type.Ctor7.of[examples.kinds.Alias.FixedBack7]) match {
      case Some(data) => Seq("as fixed back" -> data)
      case None       => Seq.empty
    }
    val setA = try6(Type[In], ctor.setA(using Int)) match {
      case Some(data) => Seq("as setA" -> data)
      case None       => Seq.empty
    }
    val setB = try6(Type[In], ctor.setB(using Int)) match {
      case Some(data) => Seq("as setB" -> data)
      case None       => Seq.empty
    }
    val setC = try6(Type[In], ctor.setC(using Int)) match {
      case Some(data) => Seq("as setC" -> data)
      case None       => Seq.empty
    }
    val setD = try6(Type[In], ctor.setD(using Int)) match {
      case Some(data) => Seq("as setD" -> data)
      case None       => Seq.empty
    }
    val setE = try6(Type[In], ctor.setE(using Int)) match {
      case Some(data) => Seq("as setE" -> data)
      case None       => Seq.empty
    }
    val setF = try6(Type[In], ctor.setF(using Int)) match {
      case Some(data) => Seq("as setF" -> data)
      case None       => Seq.empty
    }
    val setG = try6(Type[In], ctor.setG(using Int)) match {
      case Some(data) => Seq("as setG" -> data)
      case None       => Seq.empty
    }
    // format: off
    val result =
      (classResult.view ++ renamedResult.view ++ extraResult.view ++ fixedFrontResult.view ++ fixedBackResult.view ++ setA.view ++ setB.view ++ setC.view ++ setD.view ++ setE.view ++ setF.view ++ setG.view).toSeq
    // format: on
    if (result.isEmpty) Expr(Data("Not one of the expected types")) else Expr(Data.map(result*))
  }

  private def try8[Tested, Ctor[_, _, _, _, _, _, _, _]](
      Tested: Type[Tested],
      Ctor: Type.Ctor8[Ctor]
  ) = Tested match {
    case Ctor(a, b, c, d, e, f, g, h) =>
      val Z = String
      Some(
        Data.map(
          "unapplied" -> Data.list(
            Data(a.Underlying.plainPrint),
            Data(b.Underlying.plainPrint),
            Data(c.Underlying.plainPrint),
            Data(d.Underlying.plainPrint),
            Data(e.Underlying.plainPrint),
            Data(f.Underlying.plainPrint),
            Data(g.Underlying.plainPrint),
            Data(h.Underlying.plainPrint)
          ),
          "reapplied" -> Data(
            Ctor(using Z, Z, Z, Z, Z, Z, Z, Z).plainPrint
          )
        )
      )
    case _ => None
  }

  def testTypeCtor8[In: Type]: Expr[Data] = {
    val ctor = Type.Ctor8.of[examples.kinds.Arity8]
    val classResult = try8(Type[In], ctor) match {
      case Some(data) => Seq("as class" -> data)
      case None       => Seq.empty
    }
    val renamedResult = try8(Type[In], Type.Ctor8.of[examples.kinds.Alias.Renamed8]) match {
      case Some(data) => Seq("as renamed" -> data)
      case None       => Seq.empty
    }
    val extraResult = try8(Type[In], Type.Ctor8.of[examples.kinds.Alias.Extra8]) match {
      case Some(data) => Seq("as extra" -> data)
      case None       => Seq.empty
    }
    val fixedFrontResult = try8(Type[In], Type.Ctor8.of[examples.kinds.Alias.FixedFront8]) match {
      case Some(data) => Seq("as fixed front" -> data)
      case None       => Seq.empty
    }
    val fixedBackResult = try8(Type[In], Type.Ctor8.of[examples.kinds.Alias.FixedBack8]) match {
      case Some(data) => Seq("as fixed back" -> data)
      case None       => Seq.empty
    }
    val setA = try7(Type[In], ctor.setA(using Int)) match {
      case Some(data) => Seq("as setA" -> data)
      case None       => Seq.empty
    }
    val setB = try7(Type[In], ctor.setB(using Int)) match {
      case Some(data) => Seq("as setB" -> data)
      case None       => Seq.empty
    }
    val setC = try7(Type[In], ctor.setC(using Int)) match {
      case Some(data) => Seq("as setC" -> data)
      case None       => Seq.empty
    }
    val setD = try7(Type[In], ctor.setD(using Int)) match {
      case Some(data) => Seq("as setD" -> data)
      case None       => Seq.empty
    }
    val setE = try7(Type[In], ctor.setE(using Int)) match {
      case Some(data) => Seq("as setE" -> data)
      case None       => Seq.empty
    }
    val setF = try7(Type[In], ctor.setF(using Int)) match {
      case Some(data) => Seq("as setF" -> data)
      case None       => Seq.empty
    }
    val setG = try7(Type[In], ctor.setG(using Int)) match {
      case Some(data) => Seq("as setG" -> data)
      case None       => Seq.empty
    }
    val setH = try7(Type[In], ctor.setH(using Int)) match {
      case Some(data) => Seq("as setH" -> data)
      case None       => Seq.empty
    }
    // format: off
    val result =
      (classResult.view ++ renamedResult.view ++ extraResult.view ++ fixedFrontResult.view ++ fixedBackResult.view ++ setA.view ++ setB.view ++ setC.view ++ setD.view ++ setE.view ++ setF.view ++ setG.view ++ setH.view).toSeq
    // format: on
    if (result.isEmpty) Expr(Data("Not one of the expected types")) else Expr(Data.map(result*))
  }

  private def try9[Tested, Ctor[_, _, _, _, _, _, _, _, _]](
      Tested: Type[Tested],
      Ctor: Type.Ctor9[Ctor]
  ) = Tested match {
    case Ctor(a, b, c, d, e, f, g, h, i) =>
      val Z = String
      Some(
        Data.map(
          "unapplied" -> Data.list(
            Data(a.Underlying.plainPrint),
            Data(b.Underlying.plainPrint),
            Data(c.Underlying.plainPrint),
            Data(d.Underlying.plainPrint),
            Data(e.Underlying.plainPrint),
            Data(f.Underlying.plainPrint),
            Data(g.Underlying.plainPrint),
            Data(h.Underlying.plainPrint),
            Data(i.Underlying.plainPrint)
          ),
          "reapplied" -> Data(
            Ctor(using Z, Z, Z, Z, Z, Z, Z, Z, Z).plainPrint
          )
        )
      )
    case _ => None
  }

  def testTypeCtor9[In: Type]: Expr[Data] = {
    val ctor = Type.Ctor9.of[examples.kinds.Arity9]
    val classResult = try9(Type[In], ctor) match {
      case Some(data) => Seq("as class" -> data)
      case None       => Seq.empty
    }
    val renamedResult = try9(Type[In], Type.Ctor9.of[examples.kinds.Alias.Renamed9]) match {
      case Some(data) => Seq("as renamed" -> data)
      case None       => Seq.empty
    }
    val extraResult = try9(Type[In], Type.Ctor9.of[examples.kinds.Alias.Extra9]) match {
      case Some(data) => Seq("as extra" -> data)
      case None       => Seq.empty
    }
    val fixedFrontResult = try9(Type[In], Type.Ctor9.of[examples.kinds.Alias.FixedFront9]) match {
      case Some(data) => Seq("as fixed front" -> data)
      case None       => Seq.empty
    }
    val fixedBackResult = try9(Type[In], Type.Ctor9.of[examples.kinds.Alias.FixedBack9]) match {
      case Some(data) => Seq("as fixed back" -> data)
      case None       => Seq.empty
    }
    val setA = try8(Type[In], ctor.setA(using Int)) match {
      case Some(data) => Seq("as setA" -> data)
      case None       => Seq.empty
    }
    val setB = try8(Type[In], ctor.setB(using Int)) match {
      case Some(data) => Seq("as setB" -> data)
      case None       => Seq.empty
    }
    val setC = try8(Type[In], ctor.setC(using Int)) match {
      case Some(data) => Seq("as setC" -> data)
      case None       => Seq.empty
    }
    val setD = try8(Type[In], ctor.setD(using Int)) match {
      case Some(data) => Seq("as setD" -> data)
      case None       => Seq.empty
    }
    val setE = try8(Type[In], ctor.setE(using Int)) match {
      case Some(data) => Seq("as setE" -> data)
      case None       => Seq.empty
    }
    val setF = try8(Type[In], ctor.setF(using Int)) match {
      case Some(data) => Seq("as setF" -> data)
      case None       => Seq.empty
    }
    val setG = try8(Type[In], ctor.setG(using Int)) match {
      case Some(data) => Seq("as setG" -> data)
      case None       => Seq.empty
    }
    val setH = try8(Type[In], ctor.setH(using Int)) match {
      case Some(data) => Seq("as setH" -> data)
      case None       => Seq.empty
    }
    val setI = try8(Type[In], ctor.setI(using Int)) match {
      case Some(data) => Seq("as setI" -> data)
      case None       => Seq.empty
    }
    // format: off
    val result =
      (classResult.view ++ renamedResult.view ++ extraResult.view ++ fixedFrontResult.view ++ fixedBackResult.view ++ setA.view ++ setB.view ++ setC.view ++ setD.view ++ setE.view ++ setF.view ++ setG.view ++ setH.view ++ setI.view).toSeq
    // format: on
    if (result.isEmpty) Expr(Data("Not one of the expected types")) else Expr(Data.map(result*))
  }

  private def try10[Tested, Ctor[_, _, _, _, _, _, _, _, _, _]](
      Tested: Type[Tested],
      Ctor: Type.Ctor10[Ctor]
  ) = Tested match {
    case Ctor(a, b, c, d, e, f, g, h, i, j) =>
      val Z = String
      Some(
        Data.map(
          "unapplied" -> Data.list(
            Data(a.Underlying.plainPrint),
            Data(b.Underlying.plainPrint),
            Data(c.Underlying.plainPrint),
            Data(d.Underlying.plainPrint),
            Data(e.Underlying.plainPrint),
            Data(f.Underlying.plainPrint),
            Data(g.Underlying.plainPrint),
            Data(h.Underlying.plainPrint),
            Data(i.Underlying.plainPrint),
            Data(j.Underlying.plainPrint)
          ),
          "reapplied" -> Data(
            Ctor(using Z, Z, Z, Z, Z, Z, Z, Z, Z, Z).plainPrint
          )
        )
      )
    case _ => None
  }

  def testTypeCtor10[In: Type]: Expr[Data] = {
    val ctor = Type.Ctor10.of[examples.kinds.Arity10]
    val classResult = try10(Type[In], ctor) match {
      case Some(data) => Seq("as class" -> data)
      case None       => Seq.empty
    }
    val renamedResult = try10(Type[In], Type.Ctor10.of[examples.kinds.Alias.Renamed10]) match {
      case Some(data) => Seq("as renamed" -> data)
      case None       => Seq.empty
    }
    val extraResult = try10(Type[In], Type.Ctor10.of[examples.kinds.Alias.Extra10]) match {
      case Some(data) => Seq("as extra" -> data)
      case None       => Seq.empty
    }
    val fixedFrontResult = try10(Type[In], Type.Ctor10.of[examples.kinds.Alias.FixedFront10]) match {
      case Some(data) => Seq("as fixed front" -> data)
      case None       => Seq.empty
    }
    val fixedBackResult = try10(Type[In], Type.Ctor10.of[examples.kinds.Alias.FixedBack10]) match {
      case Some(data) => Seq("as fixed back" -> data)
      case None       => Seq.empty
    }
    val setA = try9(Type[In], ctor.setA(using Int)) match {
      case Some(data) => Seq("as setA" -> data)
      case None       => Seq.empty
    }
    val setB = try9(Type[In], ctor.setB(using Int)) match {
      case Some(data) => Seq("as setB" -> data)
      case None       => Seq.empty
    }
    val setC = try9(Type[In], ctor.setC(using Int)) match {
      case Some(data) => Seq("as setC" -> data)
      case None       => Seq.empty
    }
    val setD = try9(Type[In], ctor.setD(using Int)) match {
      case Some(data) => Seq("as setD" -> data)
      case None       => Seq.empty
    }
    val setE = try9(Type[In], ctor.setE(using Int)) match {
      case Some(data) => Seq("as setE" -> data)
      case None       => Seq.empty
    }
    val setF = try9(Type[In], ctor.setF(using Int)) match {
      case Some(data) => Seq("as setF" -> data)
      case None       => Seq.empty
    }
    val setG = try9(Type[In], ctor.setG(using Int)) match {
      case Some(data) => Seq("as setG" -> data)
      case None       => Seq.empty
    }
    val setH = try9(Type[In], ctor.setH(using Int)) match {
      case Some(data) => Seq("as setH" -> data)
      case None       => Seq.empty
    }
    val setI = try9(Type[In], ctor.setI(using Int)) match {
      case Some(data) => Seq("as setI" -> data)
      case None       => Seq.empty
    }
    val setJ = try9(Type[In], ctor.setJ(using Int)) match {
      case Some(data) => Seq("as setJ" -> data)
      case None       => Seq.empty
    }
    // format: off
    val result =
      (classResult.view ++ renamedResult.view ++ extraResult.view ++ fixedFrontResult.view ++ fixedBackResult.view ++ setA.view ++ setB.view ++ setC.view ++ setD.view ++ setE.view ++ setF.view ++ setG.view ++ setH.view ++ setI.view ++ setJ.view).toSeq
    // format: on
    if (result.isEmpty) Expr(Data("Not one of the expected types")) else Expr(Data.map(result*))
  }

  private def try11[Tested, Ctor[_, _, _, _, _, _, _, _, _, _, _]](
      Tested: Type[Tested],
      Ctor: Type.Ctor11[Ctor]
  ) = Tested match {
    case Ctor(a, b, c, d, e, f, g, h, i, j, k) =>
      val Z = String
      Some(
        Data.map(
          "unapplied" -> Data.list(
            Data(a.Underlying.plainPrint),
            Data(b.Underlying.plainPrint),
            Data(c.Underlying.plainPrint),
            Data(d.Underlying.plainPrint),
            Data(e.Underlying.plainPrint),
            Data(f.Underlying.plainPrint),
            Data(g.Underlying.plainPrint),
            Data(h.Underlying.plainPrint),
            Data(i.Underlying.plainPrint),
            Data(j.Underlying.plainPrint),
            Data(k.Underlying.plainPrint)
          ),
          "reapplied" -> Data(
            Ctor(using Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z).plainPrint
          )
        )
      )
    case _ => None
  }

  def testTypeCtor11[In: Type]: Expr[Data] = {
    val ctor = Type.Ctor11.of[examples.kinds.Arity11]
    val classResult = try11(Type[In], ctor) match {
      case Some(data) => Seq("as class" -> data)
      case None       => Seq.empty
    }
    val renamedResult = try11(Type[In], Type.Ctor11.of[examples.kinds.Alias.Renamed11]) match {
      case Some(data) => Seq("as renamed" -> data)
      case None       => Seq.empty
    }
    val extraResult = try11(Type[In], Type.Ctor11.of[examples.kinds.Alias.Extra11]) match {
      case Some(data) => Seq("as extra" -> data)
      case None       => Seq.empty
    }
    val fixedFrontResult = try11(Type[In], Type.Ctor11.of[examples.kinds.Alias.FixedFront11]) match {
      case Some(data) => Seq("as fixed front" -> data)
      case None       => Seq.empty
    }
    val fixedBackResult = try11(Type[In], Type.Ctor11.of[examples.kinds.Alias.FixedBack11]) match {
      case Some(data) => Seq("as fixed back" -> data)
      case None       => Seq.empty
    }
    val setA = try10(Type[In], ctor.setA(using Int)) match {
      case Some(data) => Seq("as setA" -> data)
      case None       => Seq.empty
    }
    val setB = try10(Type[In], ctor.setB(using Int)) match {
      case Some(data) => Seq("as setB" -> data)
      case None       => Seq.empty
    }
    val setC = try10(Type[In], ctor.setC(using Int)) match {
      case Some(data) => Seq("as setC" -> data)
      case None       => Seq.empty
    }
    val setD = try10(Type[In], ctor.setD(using Int)) match {
      case Some(data) => Seq("as setD" -> data)
      case None       => Seq.empty
    }
    val setE = try10(Type[In], ctor.setE(using Int)) match {
      case Some(data) => Seq("as setE" -> data)
      case None       => Seq.empty
    }
    val setF = try10(Type[In], ctor.setF(using Int)) match {
      case Some(data) => Seq("as setF" -> data)
      case None       => Seq.empty
    }
    val setG = try10(Type[In], ctor.setG(using Int)) match {
      case Some(data) => Seq("as setG" -> data)
      case None       => Seq.empty
    }
    val setH = try10(Type[In], ctor.setH(using Int)) match {
      case Some(data) => Seq("as setH" -> data)
      case None       => Seq.empty
    }
    val setI = try10(Type[In], ctor.setI(using Int)) match {
      case Some(data) => Seq("as setI" -> data)
      case None       => Seq.empty
    }
    val setJ = try10(Type[In], ctor.setJ(using Int)) match {
      case Some(data) => Seq("as setJ" -> data)
      case None       => Seq.empty
    }
    val setK = try10(Type[In], ctor.setK(using Int)) match {
      case Some(data) => Seq("as setK" -> data)
      case None       => Seq.empty
    }
    // format: off
    val result =
      (classResult.view ++ renamedResult.view ++ extraResult.view ++ fixedFrontResult.view ++ fixedBackResult.view ++ setA.view ++ setB.view ++ setC.view ++ setD.view ++ setE.view ++ setF.view ++ setG.view ++ setH.view ++ setI.view ++ setJ.view ++ setK.view).toSeq
    // format: on
    if (result.isEmpty) Expr(Data("Not one of the expected types")) else Expr(Data.map(result*))
  }

  private def try12[Tested, Ctor[_, _, _, _, _, _, _, _, _, _, _, _]](
      Tested: Type[Tested],
      Ctor: Type.Ctor12[Ctor]
  ) = Tested match {
    case Ctor(a, b, c, d, e, f, g, h, i, j, k, l) =>
      val Z = String
      Some(
        Data.map(
          "unapplied" -> Data.list(
            Data(a.Underlying.plainPrint),
            Data(b.Underlying.plainPrint),
            Data(c.Underlying.plainPrint),
            Data(d.Underlying.plainPrint),
            Data(e.Underlying.plainPrint),
            Data(f.Underlying.plainPrint),
            Data(g.Underlying.plainPrint),
            Data(h.Underlying.plainPrint),
            Data(i.Underlying.plainPrint),
            Data(j.Underlying.plainPrint),
            Data(k.Underlying.plainPrint),
            Data(l.Underlying.plainPrint)
          ),
          "reapplied" -> Data(
            Ctor(using Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z).plainPrint
          )
        )
      )
    case _ => None
  }

  def testTypeCtor12[In: Type]: Expr[Data] = {
    val ctor = Type.Ctor12.of[examples.kinds.Arity12]
    val classResult = try12(Type[In], ctor) match {
      case Some(data) => Seq("as class" -> data)
      case None       => Seq.empty
    }
    val renamedResult = try12(Type[In], Type.Ctor12.of[examples.kinds.Alias.Renamed12]) match {
      case Some(data) => Seq("as renamed" -> data)
      case None       => Seq.empty
    }
    val extraResult = try12(Type[In], Type.Ctor12.of[examples.kinds.Alias.Extra12]) match {
      case Some(data) => Seq("as extra" -> data)
      case None       => Seq.empty
    }
    val fixedFrontResult = try12(Type[In], Type.Ctor12.of[examples.kinds.Alias.FixedFront12]) match {
      case Some(data) => Seq("as fixed front" -> data)
      case None       => Seq.empty
    }
    val fixedBackResult = try12(Type[In], Type.Ctor12.of[examples.kinds.Alias.FixedBack12]) match {
      case Some(data) => Seq("as fixed back" -> data)
      case None       => Seq.empty
    }
    val setA = try11(Type[In], ctor.setA(using Int)) match {
      case Some(data) => Seq("as setA" -> data)
      case None       => Seq.empty
    }
    val setB = try11(Type[In], ctor.setB(using Int)) match {
      case Some(data) => Seq("as setB" -> data)
      case None       => Seq.empty
    }
    val setC = try11(Type[In], ctor.setC(using Int)) match {
      case Some(data) => Seq("as setC" -> data)
      case None       => Seq.empty
    }
    val setD = try11(Type[In], ctor.setD(using Int)) match {
      case Some(data) => Seq("as setD" -> data)
      case None       => Seq.empty
    }
    val setE = try11(Type[In], ctor.setE(using Int)) match {
      case Some(data) => Seq("as setE" -> data)
      case None       => Seq.empty
    }
    val setF = try11(Type[In], ctor.setF(using Int)) match {
      case Some(data) => Seq("as setF" -> data)
      case None       => Seq.empty
    }
    val setG = try11(Type[In], ctor.setG(using Int)) match {
      case Some(data) => Seq("as setG" -> data)
      case None       => Seq.empty
    }
    val setH = try11(Type[In], ctor.setH(using Int)) match {
      case Some(data) => Seq("as setH" -> data)
      case None       => Seq.empty
    }
    val setI = try11(Type[In], ctor.setI(using Int)) match {
      case Some(data) => Seq("as setI" -> data)
      case None       => Seq.empty
    }
    val setJ = try11(Type[In], ctor.setJ(using Int)) match {
      case Some(data) => Seq("as setJ" -> data)
      case None       => Seq.empty
    }
    val setK = try11(Type[In], ctor.setK(using Int)) match {
      case Some(data) => Seq("as setK" -> data)
      case None       => Seq.empty
    }
    val setL = try11(Type[In], ctor.setL(using Int)) match {
      case Some(data) => Seq("as setL" -> data)
      case None       => Seq.empty
    }
    // format: off
    val result =
      (classResult.view ++ renamedResult.view ++ extraResult.view ++ fixedFrontResult.view ++ fixedBackResult.view ++ setA.view ++ setB.view ++ setC.view ++ setD.view ++ setE.view ++ setF.view ++ setG.view ++ setH.view ++ setI.view ++ setJ.view ++ setK.view ++ setL.view).toSeq
    // format: on
    if (result.isEmpty) Expr(Data("Not one of the expected types")) else Expr(Data.map(result*))
  }

  private def try13[Tested, Ctor[_, _, _, _, _, _, _, _, _, _, _, _, _]](
      Tested: Type[Tested],
      Ctor: Type.Ctor13[Ctor]
  ) = Tested match {
    case Ctor(a, b, c, d, e, f, g, h, i, j, k, l, m) =>
      val Z = String
      Some(
        Data.map(
          "unapplied" -> Data.list(
            Data(a.Underlying.plainPrint),
            Data(b.Underlying.plainPrint),
            Data(c.Underlying.plainPrint),
            Data(d.Underlying.plainPrint),
            Data(e.Underlying.plainPrint),
            Data(f.Underlying.plainPrint),
            Data(g.Underlying.plainPrint),
            Data(h.Underlying.plainPrint),
            Data(i.Underlying.plainPrint),
            Data(j.Underlying.plainPrint),
            Data(k.Underlying.plainPrint),
            Data(l.Underlying.plainPrint),
            Data(m.Underlying.plainPrint)
          ),
          "reapplied" -> Data(
            Ctor(using Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z).plainPrint
          )
        )
      )
    case _ => None
  }

  def testTypeCtor13[In: Type]: Expr[Data] = {
    val ctor = Type.Ctor13.of[examples.kinds.Arity13]
    val classResult = try13(Type[In], ctor) match {
      case Some(data) => Seq("as class" -> data)
      case None       => Seq.empty
    }
    val renamedResult = try13(Type[In], Type.Ctor13.of[examples.kinds.Alias.Renamed13]) match {
      case Some(data) => Seq("as renamed" -> data)
      case None       => Seq.empty
    }
    val extraResult = try13(Type[In], Type.Ctor13.of[examples.kinds.Alias.Extra13]) match {
      case Some(data) => Seq("as extra" -> data)
      case None       => Seq.empty
    }
    val fixedFrontResult = try13(Type[In], Type.Ctor13.of[examples.kinds.Alias.FixedFront13]) match {
      case Some(data) => Seq("as fixed front" -> data)
      case None       => Seq.empty
    }
    val fixedBackResult = try13(Type[In], Type.Ctor13.of[examples.kinds.Alias.FixedBack13]) match {
      case Some(data) => Seq("as fixed back" -> data)
      case None       => Seq.empty
    }
    val setA = try12(Type[In], ctor.setA(using Int)) match {
      case Some(data) => Seq("as setA" -> data)
      case None       => Seq.empty
    }
    val setB = try12(Type[In], ctor.setB(using Int)) match {
      case Some(data) => Seq("as setB" -> data)
      case None       => Seq.empty
    }
    val setC = try12(Type[In], ctor.setC(using Int)) match {
      case Some(data) => Seq("as setC" -> data)
      case None       => Seq.empty
    }
    val setD = try12(Type[In], ctor.setD(using Int)) match {
      case Some(data) => Seq("as setD" -> data)
      case None       => Seq.empty
    }
    val setE = try12(Type[In], ctor.setE(using Int)) match {
      case Some(data) => Seq("as setE" -> data)
      case None       => Seq.empty
    }
    val setF = try12(Type[In], ctor.setF(using Int)) match {
      case Some(data) => Seq("as setF" -> data)
      case None       => Seq.empty
    }
    val setG = try12(Type[In], ctor.setG(using Int)) match {
      case Some(data) => Seq("as setG" -> data)
      case None       => Seq.empty
    }
    val setH = try12(Type[In], ctor.setH(using Int)) match {
      case Some(data) => Seq("as setH" -> data)
      case None       => Seq.empty
    }
    val setI = try12(Type[In], ctor.setI(using Int)) match {
      case Some(data) => Seq("as setI" -> data)
      case None       => Seq.empty
    }
    val setJ = try12(Type[In], ctor.setJ(using Int)) match {
      case Some(data) => Seq("as setJ" -> data)
      case None       => Seq.empty
    }
    val setK = try12(Type[In], ctor.setK(using Int)) match {
      case Some(data) => Seq("as setK" -> data)
      case None       => Seq.empty
    }
    val setL = try12(Type[In], ctor.setL(using Int)) match {
      case Some(data) => Seq("as setL" -> data)
      case None       => Seq.empty
    }
    val setM = try12(Type[In], ctor.setM(using Int)) match {
      case Some(data) => Seq("as setM" -> data)
      case None       => Seq.empty
    }
    // format: off
    val result =
      (classResult.view ++ renamedResult.view ++ extraResult.view ++ fixedFrontResult.view ++ fixedBackResult.view ++ setA.view ++ setB.view ++ setC.view ++ setD.view ++ setE.view ++ setF.view ++ setG.view ++ setH.view ++ setI.view ++ setJ.view ++ setK.view ++ setL.view ++ setM.view).toSeq
    // format: on
    if (result.isEmpty) Expr(Data("Not one of the expected types")) else Expr(Data.map(result*))
  }

  private def try14[Tested, Ctor[_, _, _, _, _, _, _, _, _, _, _, _, _, _]](
      Tested: Type[Tested],
      Ctor: Type.Ctor14[Ctor]
  ) = Tested match {
    case Ctor(a, b, c, d, e, f, g, h, i, j, k, l, m, n) =>
      val Z = String
      Some(
        Data.map(
          "unapplied" -> Data.list(
            Data(a.Underlying.plainPrint),
            Data(b.Underlying.plainPrint),
            Data(c.Underlying.plainPrint),
            Data(d.Underlying.plainPrint),
            Data(e.Underlying.plainPrint),
            Data(f.Underlying.plainPrint),
            Data(g.Underlying.plainPrint),
            Data(h.Underlying.plainPrint),
            Data(i.Underlying.plainPrint),
            Data(j.Underlying.plainPrint),
            Data(k.Underlying.plainPrint),
            Data(l.Underlying.plainPrint),
            Data(m.Underlying.plainPrint),
            Data(n.Underlying.plainPrint)
          ),
          "reapplied" -> Data(
            Ctor(using Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z).plainPrint
          )
        )
      )
    case _ => None
  }

  def testTypeCtor14[In: Type]: Expr[Data] = {
    val ctor = Type.Ctor14.of[examples.kinds.Arity14]
    val classResult = try14(Type[In], ctor) match {
      case Some(data) => Seq("as class" -> data)
      case None       => Seq.empty
    }
    val renamedResult = try14(Type[In], Type.Ctor14.of[examples.kinds.Alias.Renamed14]) match {
      case Some(data) => Seq("as renamed" -> data)
      case None       => Seq.empty
    }
    val extraResult = try14(Type[In], Type.Ctor14.of[examples.kinds.Alias.Extra14]) match {
      case Some(data) => Seq("as extra" -> data)
      case None       => Seq.empty
    }
    val fixedFrontResult = try14(Type[In], Type.Ctor14.of[examples.kinds.Alias.FixedFront14]) match {
      case Some(data) => Seq("as fixed front" -> data)
      case None       => Seq.empty
    }
    val fixedBackResult = try14(Type[In], Type.Ctor14.of[examples.kinds.Alias.FixedBack14]) match {
      case Some(data) => Seq("as fixed back" -> data)
      case None       => Seq.empty
    }
    val setA = try13(Type[In], ctor.setA(using Int)) match {
      case Some(data) => Seq("as setA" -> data)
      case None       => Seq.empty
    }
    val setB = try13(Type[In], ctor.setB(using Int)) match {
      case Some(data) => Seq("as setB" -> data)
      case None       => Seq.empty
    }
    val setC = try13(Type[In], ctor.setC(using Int)) match {
      case Some(data) => Seq("as setC" -> data)
      case None       => Seq.empty
    }
    val setD = try13(Type[In], ctor.setD(using Int)) match {
      case Some(data) => Seq("as setD" -> data)
      case None       => Seq.empty
    }
    val setE = try13(Type[In], ctor.setE(using Int)) match {
      case Some(data) => Seq("as setE" -> data)
      case None       => Seq.empty
    }
    val setF = try13(Type[In], ctor.setF(using Int)) match {
      case Some(data) => Seq("as setF" -> data)
      case None       => Seq.empty
    }
    val setG = try13(Type[In], ctor.setG(using Int)) match {
      case Some(data) => Seq("as setG" -> data)
      case None       => Seq.empty
    }
    val setH = try13(Type[In], ctor.setH(using Int)) match {
      case Some(data) => Seq("as setH" -> data)
      case None       => Seq.empty
    }
    val setI = try13(Type[In], ctor.setI(using Int)) match {
      case Some(data) => Seq("as setI" -> data)
      case None       => Seq.empty
    }
    val setJ = try13(Type[In], ctor.setJ(using Int)) match {
      case Some(data) => Seq("as setJ" -> data)
      case None       => Seq.empty
    }
    val setK = try13(Type[In], ctor.setK(using Int)) match {
      case Some(data) => Seq("as setK" -> data)
      case None       => Seq.empty
    }
    val setL = try13(Type[In], ctor.setL(using Int)) match {
      case Some(data) => Seq("as setL" -> data)
      case None       => Seq.empty
    }
    val setM = try13(Type[In], ctor.setM(using Int)) match {
      case Some(data) => Seq("as setM" -> data)
      case None       => Seq.empty
    }
    val setN = try13(Type[In], ctor.setN(using Int)) match {
      case Some(data) => Seq("as setN" -> data)
      case None       => Seq.empty
    }
    // format: off
    val result =
      (classResult.view ++ renamedResult.view ++ extraResult.view ++ fixedFrontResult.view ++ fixedBackResult.view ++ setA.view ++ setB.view ++ setC.view ++ setD.view ++ setE.view ++ setF.view ++ setG.view ++ setH.view ++ setI.view ++ setJ.view ++ setK.view ++ setL.view ++ setM.view ++ setN.view).toSeq
    // format: on
    if (result.isEmpty) Expr(Data("Not one of the expected types")) else Expr(Data.map(result*))
  }

  private def try15[Tested, Ctor[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _]](
      Tested: Type[Tested],
      Ctor: Type.Ctor15[Ctor]
  ) = Tested match {
    case Ctor(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) =>
      val Z = String
      Some(
        Data.map(
          "unapplied" -> Data.list(
            Data(a.Underlying.plainPrint),
            Data(b.Underlying.plainPrint),
            Data(c.Underlying.plainPrint),
            Data(d.Underlying.plainPrint),
            Data(e.Underlying.plainPrint),
            Data(f.Underlying.plainPrint),
            Data(g.Underlying.plainPrint),
            Data(h.Underlying.plainPrint),
            Data(i.Underlying.plainPrint),
            Data(j.Underlying.plainPrint),
            Data(k.Underlying.plainPrint),
            Data(l.Underlying.plainPrint),
            Data(m.Underlying.plainPrint),
            Data(n.Underlying.plainPrint),
            Data(o.Underlying.plainPrint)
          ),
          "reapplied" -> Data(
            Ctor(using Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z).plainPrint
          )
        )
      )
    case _ => None
  }

  def testTypeCtor15[In: Type]: Expr[Data] = {
    val ctor = Type.Ctor15.of[examples.kinds.Arity15]
    val classResult = try15(Type[In], ctor) match {
      case Some(data) => Seq("as class" -> data)
      case None       => Seq.empty
    }
    val renamedResult = try15(Type[In], Type.Ctor15.of[examples.kinds.Alias.Renamed15]) match {
      case Some(data) => Seq("as renamed" -> data)
      case None       => Seq.empty
    }
    val extraResult = try15(Type[In], Type.Ctor15.of[examples.kinds.Alias.Extra15]) match {
      case Some(data) => Seq("as extra" -> data)
      case None       => Seq.empty
    }
    val fixedFrontResult = try15(Type[In], Type.Ctor15.of[examples.kinds.Alias.FixedFront15]) match {
      case Some(data) => Seq("as fixed front" -> data)
      case None       => Seq.empty
    }
    val fixedBackResult = try15(Type[In], Type.Ctor15.of[examples.kinds.Alias.FixedBack15]) match {
      case Some(data) => Seq("as fixed back" -> data)
      case None       => Seq.empty
    }
    val setA = try14(Type[In], ctor.setA(using Int)) match {
      case Some(data) => Seq("as setA" -> data)
      case None       => Seq.empty
    }
    val setB = try14(Type[In], ctor.setB(using Int)) match {
      case Some(data) => Seq("as setB" -> data)
      case None       => Seq.empty
    }
    val setC = try14(Type[In], ctor.setC(using Int)) match {
      case Some(data) => Seq("as setC" -> data)
      case None       => Seq.empty
    }
    val setD = try14(Type[In], ctor.setD(using Int)) match {
      case Some(data) => Seq("as setD" -> data)
      case None       => Seq.empty
    }
    val setE = try14(Type[In], ctor.setE(using Int)) match {
      case Some(data) => Seq("as setE" -> data)
      case None       => Seq.empty
    }
    val setF = try14(Type[In], ctor.setF(using Int)) match {
      case Some(data) => Seq("as setF" -> data)
      case None       => Seq.empty
    }
    val setG = try14(Type[In], ctor.setG(using Int)) match {
      case Some(data) => Seq("as setG" -> data)
      case None       => Seq.empty
    }
    val setH = try14(Type[In], ctor.setH(using Int)) match {
      case Some(data) => Seq("as setH" -> data)
      case None       => Seq.empty
    }
    val setI = try14(Type[In], ctor.setI(using Int)) match {
      case Some(data) => Seq("as setI" -> data)
      case None       => Seq.empty
    }
    val setJ = try14(Type[In], ctor.setJ(using Int)) match {
      case Some(data) => Seq("as setJ" -> data)
      case None       => Seq.empty
    }
    val setK = try14(Type[In], ctor.setK(using Int)) match {
      case Some(data) => Seq("as setK" -> data)
      case None       => Seq.empty
    }
    val setL = try14(Type[In], ctor.setL(using Int)) match {
      case Some(data) => Seq("as setL" -> data)
      case None       => Seq.empty
    }
    val setM = try14(Type[In], ctor.setM(using Int)) match {
      case Some(data) => Seq("as setM" -> data)
      case None       => Seq.empty
    }
    val setN = try14(Type[In], ctor.setN(using Int)) match {
      case Some(data) => Seq("as setN" -> data)
      case None       => Seq.empty
    }
    val setO = try14(Type[In], ctor.setO(using Int)) match {
      case Some(data) => Seq("as setO" -> data)
      case None       => Seq.empty
    }
    // format: off
    val result =
      (classResult.view ++ renamedResult.view ++ extraResult.view ++ fixedFrontResult.view ++ fixedBackResult.view ++ setA.view ++ setB.view ++ setC.view ++ setD.view ++ setE.view ++ setF.view ++ setG.view ++ setH.view ++ setI.view ++ setJ.view ++ setK.view ++ setL.view ++ setM.view ++ setN.view ++ setO.view).toSeq
    // format: on
    if (result.isEmpty) Expr(Data("Not one of the expected types")) else Expr(Data.map(result*))
  }

  private def try16[Tested, Ctor[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]](
      Tested: Type[Tested],
      Ctor: Type.Ctor16[Ctor]
  ) = Tested match {
    case Ctor(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) =>
      val Z = String
      Some(
        Data.map(
          "unapplied" -> Data.list(
            Data(a.Underlying.plainPrint),
            Data(b.Underlying.plainPrint),
            Data(c.Underlying.plainPrint),
            Data(d.Underlying.plainPrint),
            Data(e.Underlying.plainPrint),
            Data(f.Underlying.plainPrint),
            Data(g.Underlying.plainPrint),
            Data(h.Underlying.plainPrint),
            Data(i.Underlying.plainPrint),
            Data(j.Underlying.plainPrint),
            Data(k.Underlying.plainPrint),
            Data(l.Underlying.plainPrint),
            Data(m.Underlying.plainPrint),
            Data(n.Underlying.plainPrint),
            Data(o.Underlying.plainPrint),
            Data(p.Underlying.plainPrint)
          ),
          "reapplied" -> Data(
            Ctor(using Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z).plainPrint
          )
        )
      )
    case _ => None
  }

  def testTypeCtor16[In: Type]: Expr[Data] = {
    val ctor = Type.Ctor16.of[examples.kinds.Arity16]
    val classResult = try16(Type[In], ctor) match {
      case Some(data) => Seq("as class" -> data)
      case None       => Seq.empty
    }
    val renamedResult = try16(Type[In], Type.Ctor16.of[examples.kinds.Alias.Renamed16]) match {
      case Some(data) => Seq("as renamed" -> data)
      case None       => Seq.empty
    }
    val extraResult = try16(Type[In], Type.Ctor16.of[examples.kinds.Alias.Extra16]) match {
      case Some(data) => Seq("as extra" -> data)
      case None       => Seq.empty
    }
    val fixedFrontResult = try16(Type[In], Type.Ctor16.of[examples.kinds.Alias.FixedFront16]) match {
      case Some(data) => Seq("as fixed front" -> data)
      case None       => Seq.empty
    }
    val fixedBackResult = try16(Type[In], Type.Ctor16.of[examples.kinds.Alias.FixedBack16]) match {
      case Some(data) => Seq("as fixed back" -> data)
      case None       => Seq.empty
    }
    val setA = try15(Type[In], ctor.setA(using Int)) match {
      case Some(data) => Seq("as setA" -> data)
      case None       => Seq.empty
    }
    val setB = try15(Type[In], ctor.setB(using Int)) match {
      case Some(data) => Seq("as setB" -> data)
      case None       => Seq.empty
    }
    val setC = try15(Type[In], ctor.setC(using Int)) match {
      case Some(data) => Seq("as setC" -> data)
      case None       => Seq.empty
    }
    val setD = try15(Type[In], ctor.setD(using Int)) match {
      case Some(data) => Seq("as setD" -> data)
      case None       => Seq.empty
    }
    val setE = try15(Type[In], ctor.setE(using Int)) match {
      case Some(data) => Seq("as setE" -> data)
      case None       => Seq.empty
    }
    val setF = try15(Type[In], ctor.setF(using Int)) match {
      case Some(data) => Seq("as setF" -> data)
      case None       => Seq.empty
    }
    val setG = try15(Type[In], ctor.setG(using Int)) match {
      case Some(data) => Seq("as setG" -> data)
      case None       => Seq.empty
    }
    val setH = try15(Type[In], ctor.setH(using Int)) match {
      case Some(data) => Seq("as setH" -> data)
      case None       => Seq.empty
    }
    val setI = try15(Type[In], ctor.setI(using Int)) match {
      case Some(data) => Seq("as setI" -> data)
      case None       => Seq.empty
    }
    val setJ = try15(Type[In], ctor.setJ(using Int)) match {
      case Some(data) => Seq("as setJ" -> data)
      case None       => Seq.empty
    }
    val setK = try15(Type[In], ctor.setK(using Int)) match {
      case Some(data) => Seq("as setK" -> data)
      case None       => Seq.empty
    }
    val setL = try15(Type[In], ctor.setL(using Int)) match {
      case Some(data) => Seq("as setL" -> data)
      case None       => Seq.empty
    }
    val setM = try15(Type[In], ctor.setM(using Int)) match {
      case Some(data) => Seq("as setM" -> data)
      case None       => Seq.empty
    }
    val setN = try15(Type[In], ctor.setN(using Int)) match {
      case Some(data) => Seq("as setN" -> data)
      case None       => Seq.empty
    }
    val setO = try15(Type[In], ctor.setO(using Int)) match {
      case Some(data) => Seq("as setO" -> data)
      case None       => Seq.empty
    }
    val setP = try15(Type[In], ctor.setP(using Int)) match {
      case Some(data) => Seq("as setP" -> data)
      case None       => Seq.empty
    }
    // format: off
    val result =
      (classResult.view ++ renamedResult.view ++ extraResult.view ++ fixedFrontResult.view ++ fixedBackResult.view ++ setA.view ++ setB.view ++ setC.view ++ setD.view ++ setE.view ++ setF.view ++ setG.view ++ setH.view ++ setI.view ++ setJ.view ++ setK.view ++ setL.view ++ setM.view ++ setN.view ++ setO.view ++ setP.view).toSeq
    // format: on
    if (result.isEmpty) Expr(Data("Not one of the expected types")) else Expr(Data.map(result*))
  }

  private def try17[Tested, Ctor[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]](
      Tested: Type[Tested],
      Ctor: Type.Ctor17[Ctor]
  ) = Tested match {
    case Ctor(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) =>
      val Z = String
      Some(
        Data.map(
          "unapplied" -> Data.list(
            Data(a.Underlying.plainPrint),
            Data(b.Underlying.plainPrint),
            Data(c.Underlying.plainPrint),
            Data(d.Underlying.plainPrint),
            Data(e.Underlying.plainPrint),
            Data(f.Underlying.plainPrint),
            Data(g.Underlying.plainPrint),
            Data(h.Underlying.plainPrint),
            Data(i.Underlying.plainPrint),
            Data(j.Underlying.plainPrint),
            Data(k.Underlying.plainPrint),
            Data(l.Underlying.plainPrint),
            Data(m.Underlying.plainPrint),
            Data(n.Underlying.plainPrint),
            Data(o.Underlying.plainPrint),
            Data(p.Underlying.plainPrint),
            Data(q.Underlying.plainPrint)
          ),
          "reapplied" -> Data(
            Ctor(using Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z).plainPrint
          )
        )
      )
    case _ => None
  }

  def testTypeCtor17[In: Type]: Expr[Data] = {
    val ctor = Type.Ctor17.of[examples.kinds.Arity17]
    val classResult = try17(Type[In], ctor) match {
      case Some(data) => Seq("as class" -> data)
      case None       => Seq.empty
    }
    val renamedResult = try17(Type[In], Type.Ctor17.of[examples.kinds.Alias.Renamed17]) match {
      case Some(data) => Seq("as renamed" -> data)
      case None       => Seq.empty
    }
    val extraResult = try17(Type[In], Type.Ctor17.of[examples.kinds.Alias.Extra17]) match {
      case Some(data) => Seq("as extra" -> data)
      case None       => Seq.empty
    }
    val fixedFrontResult = try17(Type[In], Type.Ctor17.of[examples.kinds.Alias.FixedFront17]) match {
      case Some(data) => Seq("as fixed front" -> data)
      case None       => Seq.empty
    }
    val fixedBackResult = try17(Type[In], Type.Ctor17.of[examples.kinds.Alias.FixedBack17]) match {
      case Some(data) => Seq("as fixed back" -> data)
      case None       => Seq.empty
    }
    val setA = try16(Type[In], ctor.setA(using Int)) match {
      case Some(data) => Seq("as setA" -> data)
      case None       => Seq.empty
    }
    val setB = try16(Type[In], ctor.setB(using Int)) match {
      case Some(data) => Seq("as setB" -> data)
      case None       => Seq.empty
    }
    val setC = try16(Type[In], ctor.setC(using Int)) match {
      case Some(data) => Seq("as setC" -> data)
      case None       => Seq.empty
    }
    val setD = try16(Type[In], ctor.setD(using Int)) match {
      case Some(data) => Seq("as setD" -> data)
      case None       => Seq.empty
    }
    val setE = try16(Type[In], ctor.setE(using Int)) match {
      case Some(data) => Seq("as setE" -> data)
      case None       => Seq.empty
    }
    val setF = try16(Type[In], ctor.setF(using Int)) match {
      case Some(data) => Seq("as setF" -> data)
      case None       => Seq.empty
    }
    val setG = try16(Type[In], ctor.setG(using Int)) match {
      case Some(data) => Seq("as setG" -> data)
      case None       => Seq.empty
    }
    val setH = try16(Type[In], ctor.setH(using Int)) match {
      case Some(data) => Seq("as setH" -> data)
      case None       => Seq.empty
    }
    val setI = try16(Type[In], ctor.setI(using Int)) match {
      case Some(data) => Seq("as setI" -> data)
      case None       => Seq.empty
    }
    val setJ = try16(Type[In], ctor.setJ(using Int)) match {
      case Some(data) => Seq("as setJ" -> data)
      case None       => Seq.empty
    }
    val setK = try16(Type[In], ctor.setK(using Int)) match {
      case Some(data) => Seq("as setK" -> data)
      case None       => Seq.empty
    }
    val setL = try16(Type[In], ctor.setL(using Int)) match {
      case Some(data) => Seq("as setL" -> data)
      case None       => Seq.empty
    }
    val setM = try16(Type[In], ctor.setM(using Int)) match {
      case Some(data) => Seq("as setM" -> data)
      case None       => Seq.empty
    }
    val setN = try16(Type[In], ctor.setN(using Int)) match {
      case Some(data) => Seq("as setN" -> data)
      case None       => Seq.empty
    }
    val setO = try16(Type[In], ctor.setO(using Int)) match {
      case Some(data) => Seq("as setO" -> data)
      case None       => Seq.empty
    }
    val setP = try16(Type[In], ctor.setP(using Int)) match {
      case Some(data) => Seq("as setP" -> data)
      case None       => Seq.empty
    }
    val setQ = try16(Type[In], ctor.setQ(using Int)) match {
      case Some(data) => Seq("as setQ" -> data)
      case None       => Seq.empty
    }
    // format: off
    val result =
      (classResult.view ++ renamedResult.view ++ extraResult.view ++ fixedFrontResult.view ++ fixedBackResult.view ++ setA.view ++ setB.view ++ setC.view ++ setD.view ++ setE.view ++ setF.view ++ setG.view ++ setH.view ++ setI.view ++ setJ.view ++ setK.view ++ setL.view ++ setM.view ++ setN.view ++ setO.view ++ setP.view ++ setQ.view).toSeq
    // format: on
    if (result.isEmpty) Expr(Data("Not one of the expected types")) else Expr(Data.map(result*))
  }

  private def try18[Tested, Ctor[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]](
      Tested: Type[Tested],
      Ctor: Type.Ctor18[Ctor]
  ) = Tested match {
    case Ctor(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) =>
      val Z = String
      Some(
        Data.map(
          "unapplied" -> Data.list(
            Data(a.Underlying.plainPrint),
            Data(b.Underlying.plainPrint),
            Data(c.Underlying.plainPrint),
            Data(d.Underlying.plainPrint),
            Data(e.Underlying.plainPrint),
            Data(f.Underlying.plainPrint),
            Data(g.Underlying.plainPrint),
            Data(h.Underlying.plainPrint),
            Data(i.Underlying.plainPrint),
            Data(j.Underlying.plainPrint),
            Data(k.Underlying.plainPrint),
            Data(l.Underlying.plainPrint),
            Data(m.Underlying.plainPrint),
            Data(n.Underlying.plainPrint),
            Data(o.Underlying.plainPrint),
            Data(p.Underlying.plainPrint),
            Data(q.Underlying.plainPrint),
            Data(r.Underlying.plainPrint)
          ),
          "reapplied" -> Data(
            Ctor(using Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z).plainPrint
          )
        )
      )
    case _ => None
  }

  def testTypeCtor18[In: Type]: Expr[Data] = {
    val ctor = Type.Ctor18.of[examples.kinds.Arity18]
    val classResult = try18(Type[In], ctor) match {
      case Some(data) => Seq("as class" -> data)
      case None       => Seq.empty
    }
    val renamedResult = try18(Type[In], Type.Ctor18.of[examples.kinds.Alias.Renamed18]) match {
      case Some(data) => Seq("as renamed" -> data)
      case None       => Seq.empty
    }
    val extraResult = try18(Type[In], Type.Ctor18.of[examples.kinds.Alias.Extra18]) match {
      case Some(data) => Seq("as extra" -> data)
      case None       => Seq.empty
    }
    val fixedFrontResult = try18(Type[In], Type.Ctor18.of[examples.kinds.Alias.FixedFront18]) match {
      case Some(data) => Seq("as fixed front" -> data)
      case None       => Seq.empty
    }
    val fixedBackResult = try18(Type[In], Type.Ctor18.of[examples.kinds.Alias.FixedBack18]) match {
      case Some(data) => Seq("as fixed back" -> data)
      case None       => Seq.empty
    }
    val setA = try17(Type[In], ctor.setA(using Int)) match {
      case Some(data) => Seq("as setA" -> data)
      case None       => Seq.empty
    }
    val setB = try17(Type[In], ctor.setB(using Int)) match {
      case Some(data) => Seq("as setB" -> data)
      case None       => Seq.empty
    }
    val setC = try17(Type[In], ctor.setC(using Int)) match {
      case Some(data) => Seq("as setC" -> data)
      case None       => Seq.empty
    }
    val setD = try17(Type[In], ctor.setD(using Int)) match {
      case Some(data) => Seq("as setD" -> data)
      case None       => Seq.empty
    }
    val setE = try17(Type[In], ctor.setE(using Int)) match {
      case Some(data) => Seq("as setE" -> data)
      case None       => Seq.empty
    }
    val setF = try17(Type[In], ctor.setF(using Int)) match {
      case Some(data) => Seq("as setF" -> data)
      case None       => Seq.empty
    }
    val setG = try17(Type[In], ctor.setG(using Int)) match {
      case Some(data) => Seq("as setG" -> data)
      case None       => Seq.empty
    }
    val setH = try17(Type[In], ctor.setH(using Int)) match {
      case Some(data) => Seq("as setH" -> data)
      case None       => Seq.empty
    }
    val setI = try17(Type[In], ctor.setI(using Int)) match {
      case Some(data) => Seq("as setI" -> data)
      case None       => Seq.empty
    }
    val setJ = try17(Type[In], ctor.setJ(using Int)) match {
      case Some(data) => Seq("as setJ" -> data)
      case None       => Seq.empty
    }
    val setK = try17(Type[In], ctor.setK(using Int)) match {
      case Some(data) => Seq("as setK" -> data)
      case None       => Seq.empty
    }
    val setL = try17(Type[In], ctor.setL(using Int)) match {
      case Some(data) => Seq("as setL" -> data)
      case None       => Seq.empty
    }
    val setM = try17(Type[In], ctor.setM(using Int)) match {
      case Some(data) => Seq("as setM" -> data)
      case None       => Seq.empty
    }
    val setN = try17(Type[In], ctor.setN(using Int)) match {
      case Some(data) => Seq("as setN" -> data)
      case None       => Seq.empty
    }
    val setO = try17(Type[In], ctor.setO(using Int)) match {
      case Some(data) => Seq("as setO" -> data)
      case None       => Seq.empty
    }
    val setP = try17(Type[In], ctor.setP(using Int)) match {
      case Some(data) => Seq("as setP" -> data)
      case None       => Seq.empty
    }
    val setQ = try17(Type[In], ctor.setQ(using Int)) match {
      case Some(data) => Seq("as setQ" -> data)
      case None       => Seq.empty
    }
    val setR = try17(Type[In], ctor.setR(using Int)) match {
      case Some(data) => Seq("as setR" -> data)
      case None       => Seq.empty
    }
    // format: off
    val result =
      (classResult.view ++ renamedResult.view ++ extraResult.view ++ fixedFrontResult.view ++ fixedBackResult.view ++ setA.view ++ setB.view ++ setC.view ++ setD.view ++ setE.view ++ setF.view ++ setG.view ++ setH.view ++ setI.view ++ setJ.view ++ setK.view ++ setL.view ++ setM.view ++ setN.view ++ setO.view ++ setP.view ++ setQ.view ++ setR.view).toSeq
    // format: on
    if (result.isEmpty) Expr(Data("Not one of the expected types")) else Expr(Data.map(result*))
  }

  private def try19[Tested, Ctor[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]](
      Tested: Type[Tested],
      Ctor: Type.Ctor19[Ctor]
  ) = Tested match {
    case Ctor(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) =>
      val Z = String
      Some(
        Data.map(
          "unapplied" -> Data.list(
            Data(a.Underlying.plainPrint),
            Data(b.Underlying.plainPrint),
            Data(c.Underlying.plainPrint),
            Data(d.Underlying.plainPrint),
            Data(e.Underlying.plainPrint),
            Data(f.Underlying.plainPrint),
            Data(g.Underlying.plainPrint),
            Data(h.Underlying.plainPrint),
            Data(i.Underlying.plainPrint),
            Data(j.Underlying.plainPrint),
            Data(k.Underlying.plainPrint),
            Data(l.Underlying.plainPrint),
            Data(m.Underlying.plainPrint),
            Data(n.Underlying.plainPrint),
            Data(o.Underlying.plainPrint),
            Data(p.Underlying.plainPrint),
            Data(q.Underlying.plainPrint),
            Data(r.Underlying.plainPrint),
            Data(s.Underlying.plainPrint)
          ),
          "reapplied" -> Data(
            Ctor(using Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z).plainPrint
          )
        )
      )
    case _ => None
  }

  def testTypeCtor19[In: Type]: Expr[Data] = {
    val ctor = Type.Ctor19.of[examples.kinds.Arity19]
    val classResult = try19(Type[In], ctor) match {
      case Some(data) => Seq("as class" -> data)
      case None       => Seq.empty
    }
    val renamedResult = try19(Type[In], Type.Ctor19.of[examples.kinds.Alias.Renamed19]) match {
      case Some(data) => Seq("as renamed" -> data)
      case None       => Seq.empty
    }
    val extraResult = try19(Type[In], Type.Ctor19.of[examples.kinds.Alias.Extra19]) match {
      case Some(data) => Seq("as extra" -> data)
      case None       => Seq.empty
    }
    val fixedFrontResult = try19(Type[In], Type.Ctor19.of[examples.kinds.Alias.FixedFront19]) match {
      case Some(data) => Seq("as fixed front" -> data)
      case None       => Seq.empty
    }
    val fixedBackResult = try19(Type[In], Type.Ctor19.of[examples.kinds.Alias.FixedBack19]) match {
      case Some(data) => Seq("as fixed back" -> data)
      case None       => Seq.empty
    }
    val setA = try18(Type[In], ctor.setA(using Int)) match {
      case Some(data) => Seq("as setA" -> data)
      case None       => Seq.empty
    }
    val setB = try18(Type[In], ctor.setB(using Int)) match {
      case Some(data) => Seq("as setB" -> data)
      case None       => Seq.empty
    }
    val setC = try18(Type[In], ctor.setC(using Int)) match {
      case Some(data) => Seq("as setC" -> data)
      case None       => Seq.empty
    }
    val setD = try18(Type[In], ctor.setD(using Int)) match {
      case Some(data) => Seq("as setD" -> data)
      case None       => Seq.empty
    }
    val setE = try18(Type[In], ctor.setE(using Int)) match {
      case Some(data) => Seq("as setE" -> data)
      case None       => Seq.empty
    }
    val setF = try18(Type[In], ctor.setF(using Int)) match {
      case Some(data) => Seq("as setF" -> data)
      case None       => Seq.empty
    }
    val setG = try18(Type[In], ctor.setG(using Int)) match {
      case Some(data) => Seq("as setG" -> data)
      case None       => Seq.empty
    }
    val setH = try18(Type[In], ctor.setH(using Int)) match {
      case Some(data) => Seq("as setH" -> data)
      case None       => Seq.empty
    }
    val setI = try18(Type[In], ctor.setI(using Int)) match {
      case Some(data) => Seq("as setI" -> data)
      case None       => Seq.empty
    }
    val setJ = try18(Type[In], ctor.setJ(using Int)) match {
      case Some(data) => Seq("as setJ" -> data)
      case None       => Seq.empty
    }
    val setK = try18(Type[In], ctor.setK(using Int)) match {
      case Some(data) => Seq("as setK" -> data)
      case None       => Seq.empty
    }
    val setL = try18(Type[In], ctor.setL(using Int)) match {
      case Some(data) => Seq("as setL" -> data)
      case None       => Seq.empty
    }
    val setM = try18(Type[In], ctor.setM(using Int)) match {
      case Some(data) => Seq("as setM" -> data)
      case None       => Seq.empty
    }
    val setN = try18(Type[In], ctor.setN(using Int)) match {
      case Some(data) => Seq("as setN" -> data)
      case None       => Seq.empty
    }
    val setO = try18(Type[In], ctor.setO(using Int)) match {
      case Some(data) => Seq("as setO" -> data)
      case None       => Seq.empty
    }
    val setP = try18(Type[In], ctor.setP(using Int)) match {
      case Some(data) => Seq("as setP" -> data)
      case None       => Seq.empty
    }
    val setQ = try18(Type[In], ctor.setQ(using Int)) match {
      case Some(data) => Seq("as setQ" -> data)
      case None       => Seq.empty
    }
    val setR = try18(Type[In], ctor.setR(using Int)) match {
      case Some(data) => Seq("as setR" -> data)
      case None       => Seq.empty
    }
    val setS = try18(Type[In], ctor.setS(using Int)) match {
      case Some(data) => Seq("as setS" -> data)
      case None       => Seq.empty
    }
    // format: off
    val result =
      (classResult.view ++ renamedResult.view ++ extraResult.view ++ fixedFrontResult.view ++ fixedBackResult.view ++ setA.view ++ setB.view ++ setC.view ++ setD.view ++ setE.view ++ setF.view ++ setG.view ++ setH.view ++ setI.view ++ setJ.view ++ setK.view ++ setL.view ++ setM.view ++ setN.view ++ setO.view ++ setP.view ++ setQ.view ++ setR.view ++ setS.view).toSeq
    // format: on
    if (result.isEmpty) Expr(Data("Not one of the expected types")) else Expr(Data.map(result*))
  }

  private def try20[Tested, Ctor[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]](
      Tested: Type[Tested],
      Ctor: Type.Ctor20[Ctor]
  ) = Tested match {
    case Ctor(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) =>
      val Z = String
      Some(
        Data.map(
          "unapplied" -> Data.list(
            Data(a.Underlying.plainPrint),
            Data(b.Underlying.plainPrint),
            Data(c.Underlying.plainPrint),
            Data(d.Underlying.plainPrint),
            Data(e.Underlying.plainPrint),
            Data(f.Underlying.plainPrint),
            Data(g.Underlying.plainPrint),
            Data(h.Underlying.plainPrint),
            Data(i.Underlying.plainPrint),
            Data(j.Underlying.plainPrint),
            Data(k.Underlying.plainPrint),
            Data(l.Underlying.plainPrint),
            Data(m.Underlying.plainPrint),
            Data(n.Underlying.plainPrint),
            Data(o.Underlying.plainPrint),
            Data(p.Underlying.plainPrint),
            Data(q.Underlying.plainPrint),
            Data(r.Underlying.plainPrint),
            Data(s.Underlying.plainPrint),
            Data(t.Underlying.plainPrint)
          ),
          "reapplied" -> Data(
            Ctor(using Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z).plainPrint
          )
        )
      )
    case _ => None
  }

  def testTypeCtor20[In: Type]: Expr[Data] = {
    val ctor = Type.Ctor20.of[examples.kinds.Arity20]
    val classResult = try20(Type[In], ctor) match {
      case Some(data) => Seq("as class" -> data)
      case None       => Seq.empty
    }
    val renamedResult = try20(Type[In], Type.Ctor20.of[examples.kinds.Alias.Renamed20]) match {
      case Some(data) => Seq("as renamed" -> data)
      case None       => Seq.empty
    }
    val extraResult = try20(Type[In], Type.Ctor20.of[examples.kinds.Alias.Extra20]) match {
      case Some(data) => Seq("as extra" -> data)
      case None       => Seq.empty
    }
    val fixedFrontResult = try20(Type[In], Type.Ctor20.of[examples.kinds.Alias.FixedFront20]) match {
      case Some(data) => Seq("as fixed front" -> data)
      case None       => Seq.empty
    }
    val fixedBackResult = try20(Type[In], Type.Ctor20.of[examples.kinds.Alias.FixedBack20]) match {
      case Some(data) => Seq("as fixed back" -> data)
      case None       => Seq.empty
    }
    val setA = try19(Type[In], ctor.setA(using Int)) match {
      case Some(data) => Seq("as setA" -> data)
      case None       => Seq.empty
    }
    val setB = try19(Type[In], ctor.setB(using Int)) match {
      case Some(data) => Seq("as setB" -> data)
      case None       => Seq.empty
    }
    val setC = try19(Type[In], ctor.setC(using Int)) match {
      case Some(data) => Seq("as setC" -> data)
      case None       => Seq.empty
    }
    val setD = try19(Type[In], ctor.setD(using Int)) match {
      case Some(data) => Seq("as setD" -> data)
      case None       => Seq.empty
    }
    val setE = try19(Type[In], ctor.setE(using Int)) match {
      case Some(data) => Seq("as setE" -> data)
      case None       => Seq.empty
    }
    val setF = try19(Type[In], ctor.setF(using Int)) match {
      case Some(data) => Seq("as setF" -> data)
      case None       => Seq.empty
    }
    val setG = try19(Type[In], ctor.setG(using Int)) match {
      case Some(data) => Seq("as setG" -> data)
      case None       => Seq.empty
    }
    val setH = try19(Type[In], ctor.setH(using Int)) match {
      case Some(data) => Seq("as setH" -> data)
      case None       => Seq.empty
    }
    val setI = try19(Type[In], ctor.setI(using Int)) match {
      case Some(data) => Seq("as setI" -> data)
      case None       => Seq.empty
    }
    val setJ = try19(Type[In], ctor.setJ(using Int)) match {
      case Some(data) => Seq("as setJ" -> data)
      case None       => Seq.empty
    }
    val setK = try19(Type[In], ctor.setK(using Int)) match {
      case Some(data) => Seq("as setK" -> data)
      case None       => Seq.empty
    }
    val setL = try19(Type[In], ctor.setL(using Int)) match {
      case Some(data) => Seq("as setL" -> data)
      case None       => Seq.empty
    }
    val setM = try19(Type[In], ctor.setM(using Int)) match {
      case Some(data) => Seq("as setM" -> data)
      case None       => Seq.empty
    }
    val setN = try19(Type[In], ctor.setN(using Int)) match {
      case Some(data) => Seq("as setN" -> data)
      case None       => Seq.empty
    }
    val setO = try19(Type[In], ctor.setO(using Int)) match {
      case Some(data) => Seq("as setO" -> data)
      case None       => Seq.empty
    }
    val setP = try19(Type[In], ctor.setP(using Int)) match {
      case Some(data) => Seq("as setP" -> data)
      case None       => Seq.empty
    }
    val setQ = try19(Type[In], ctor.setQ(using Int)) match {
      case Some(data) => Seq("as setQ" -> data)
      case None       => Seq.empty
    }
    val setR = try19(Type[In], ctor.setR(using Int)) match {
      case Some(data) => Seq("as setR" -> data)
      case None       => Seq.empty
    }
    val setS = try19(Type[In], ctor.setS(using Int)) match {
      case Some(data) => Seq("as setS" -> data)
      case None       => Seq.empty
    }
    val setT = try19(Type[In], ctor.setT(using Int)) match {
      case Some(data) => Seq("as setT" -> data)
      case None       => Seq.empty
    }
    // format: off
    val result =
      (classResult.view ++ renamedResult.view ++ extraResult.view ++ fixedFrontResult.view ++ fixedBackResult.view ++ setA.view ++ setB.view ++ setC.view ++ setD.view ++ setE.view ++ setF.view ++ setG.view ++ setH.view ++ setI.view ++ setJ.view ++ setK.view ++ setL.view ++ setM.view ++ setN.view ++ setO.view ++ setP.view ++ setQ.view ++ setR.view ++ setS.view ++ setT.view).toSeq
    // format: on
    if (result.isEmpty) Expr(Data("Not one of the expected types")) else Expr(Data.map(result*))
  }

  private def try21[Tested, Ctor[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]](
      Tested: Type[Tested],
      Ctor: Type.Ctor21[Ctor]
  ) = Tested match {
    case Ctor(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u) =>
      val Z = String
      Some(
        Data.map(
          "unapplied" -> Data.list(
            Data(a.Underlying.plainPrint),
            Data(b.Underlying.plainPrint),
            Data(c.Underlying.plainPrint),
            Data(d.Underlying.plainPrint),
            Data(e.Underlying.plainPrint),
            Data(f.Underlying.plainPrint),
            Data(g.Underlying.plainPrint),
            Data(h.Underlying.plainPrint),
            Data(i.Underlying.plainPrint),
            Data(j.Underlying.plainPrint),
            Data(k.Underlying.plainPrint),
            Data(l.Underlying.plainPrint),
            Data(m.Underlying.plainPrint),
            Data(n.Underlying.plainPrint),
            Data(o.Underlying.plainPrint),
            Data(p.Underlying.plainPrint),
            Data(q.Underlying.plainPrint),
            Data(r.Underlying.plainPrint),
            Data(s.Underlying.plainPrint),
            Data(t.Underlying.plainPrint),
            Data(u.Underlying.plainPrint)
          ),
          "reapplied" -> Data(
            Ctor(using Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z).plainPrint
          )
        )
      )
    case _ => None
  }

  def testTypeCtor21[In: Type]: Expr[Data] = {
    val ctor = Type.Ctor21.of[examples.kinds.Arity21]
    val classResult = try21(Type[In], ctor) match {
      case Some(data) => Seq("as class" -> data)
      case None       => Seq.empty
    }
    val renamedResult = try21(Type[In], Type.Ctor21.of[examples.kinds.Alias.Renamed21]) match {
      case Some(data) => Seq("as renamed" -> data)
      case None       => Seq.empty
    }
    val extraResult = try21(Type[In], Type.Ctor21.of[examples.kinds.Alias.Extra21]) match {
      case Some(data) => Seq("as extra" -> data)
      case None       => Seq.empty
    }
    val fixedFrontResult = try21(Type[In], Type.Ctor21.of[examples.kinds.Alias.FixedFront21]) match {
      case Some(data) => Seq("as fixed front" -> data)
      case None       => Seq.empty
    }
    val fixedBackResult = try21(Type[In], Type.Ctor21.of[examples.kinds.Alias.FixedBack21]) match {
      case Some(data) => Seq("as fixed back" -> data)
      case None       => Seq.empty
    }
    val setA = try20(Type[In], ctor.setA(using Int)) match {
      case Some(data) => Seq("as setA" -> data)
      case None       => Seq.empty
    }
    val setB = try20(Type[In], ctor.setB(using Int)) match {
      case Some(data) => Seq("as setB" -> data)
      case None       => Seq.empty
    }
    val setC = try20(Type[In], ctor.setC(using Int)) match {
      case Some(data) => Seq("as setC" -> data)
      case None       => Seq.empty
    }
    val setD = try20(Type[In], ctor.setD(using Int)) match {
      case Some(data) => Seq("as setD" -> data)
      case None       => Seq.empty
    }
    val setE = try20(Type[In], ctor.setE(using Int)) match {
      case Some(data) => Seq("as setE" -> data)
      case None       => Seq.empty
    }
    val setF = try20(Type[In], ctor.setF(using Int)) match {
      case Some(data) => Seq("as setF" -> data)
      case None       => Seq.empty
    }
    val setG = try20(Type[In], ctor.setG(using Int)) match {
      case Some(data) => Seq("as setG" -> data)
      case None       => Seq.empty
    }
    val setH = try20(Type[In], ctor.setH(using Int)) match {
      case Some(data) => Seq("as setH" -> data)
      case None       => Seq.empty
    }
    val setI = try20(Type[In], ctor.setI(using Int)) match {
      case Some(data) => Seq("as setI" -> data)
      case None       => Seq.empty
    }
    val setJ = try20(Type[In], ctor.setJ(using Int)) match {
      case Some(data) => Seq("as setJ" -> data)
      case None       => Seq.empty
    }
    val setK = try20(Type[In], ctor.setK(using Int)) match {
      case Some(data) => Seq("as setK" -> data)
      case None       => Seq.empty
    }
    val setL = try20(Type[In], ctor.setL(using Int)) match {
      case Some(data) => Seq("as setL" -> data)
      case None       => Seq.empty
    }
    val setM = try20(Type[In], ctor.setM(using Int)) match {
      case Some(data) => Seq("as setM" -> data)
      case None       => Seq.empty
    }
    val setN = try20(Type[In], ctor.setN(using Int)) match {
      case Some(data) => Seq("as setN" -> data)
      case None       => Seq.empty
    }
    val setO = try20(Type[In], ctor.setO(using Int)) match {
      case Some(data) => Seq("as setO" -> data)
      case None       => Seq.empty
    }
    val setP = try20(Type[In], ctor.setP(using Int)) match {
      case Some(data) => Seq("as setP" -> data)
      case None       => Seq.empty
    }
    val setQ = try20(Type[In], ctor.setQ(using Int)) match {
      case Some(data) => Seq("as setQ" -> data)
      case None       => Seq.empty
    }
    val setR = try20(Type[In], ctor.setR(using Int)) match {
      case Some(data) => Seq("as setR" -> data)
      case None       => Seq.empty
    }
    val setS = try20(Type[In], ctor.setS(using Int)) match {
      case Some(data) => Seq("as setS" -> data)
      case None       => Seq.empty
    }
    val setT = try20(Type[In], ctor.setT(using Int)) match {
      case Some(data) => Seq("as setT" -> data)
      case None       => Seq.empty
    }
    val setU = try20(Type[In], ctor.setU(using Int)) match {
      case Some(data) => Seq("as setU" -> data)
      case None       => Seq.empty
    }
    // format: off
    val result =
      (classResult.view ++ renamedResult.view ++ extraResult.view ++ fixedFrontResult.view ++ fixedBackResult.view ++ setA.view ++ setB.view ++ setC.view ++ setD.view ++ setE.view ++ setF.view ++ setG.view ++ setH.view ++ setI.view ++ setJ.view ++ setK.view ++ setL.view ++ setM.view ++ setN.view ++ setO.view ++ setP.view ++ setQ.view ++ setR.view ++ setS.view ++ setT.view ++ setU.view).toSeq
    // format: on
    if (result.isEmpty) Expr(Data("Not one of the expected types")) else Expr(Data.map(result*))
  }

  private def try22[Tested, Ctor[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]](
      Tested: Type[Tested],
      Ctor: Type.Ctor22[Ctor]
  ) = Tested match {
    case Ctor(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) =>
      val Z = String
      Some(
        Data.map(
          "unapplied" -> Data.list(
            Data(a.Underlying.plainPrint),
            Data(b.Underlying.plainPrint),
            Data(c.Underlying.plainPrint),
            Data(d.Underlying.plainPrint),
            Data(e.Underlying.plainPrint),
            Data(f.Underlying.plainPrint),
            Data(g.Underlying.plainPrint),
            Data(h.Underlying.plainPrint),
            Data(i.Underlying.plainPrint),
            Data(j.Underlying.plainPrint),
            Data(k.Underlying.plainPrint),
            Data(l.Underlying.plainPrint),
            Data(m.Underlying.plainPrint),
            Data(n.Underlying.plainPrint),
            Data(o.Underlying.plainPrint),
            Data(p.Underlying.plainPrint),
            Data(q.Underlying.plainPrint),
            Data(r.Underlying.plainPrint),
            Data(s.Underlying.plainPrint),
            Data(t.Underlying.plainPrint),
            Data(u.Underlying.plainPrint),
            Data(v.Underlying.plainPrint)
          ),
          "reapplied" -> Data(
            Ctor(using Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z).plainPrint
          )
        )
      )
    case _ => None
  }

  def testTypeCtor22[In: Type]: Expr[Data] = {
    val ctor = Type.Ctor22.of[examples.kinds.Arity22]
    val classResult = try22(Type[In], ctor) match {
      case Some(data) => Seq("as class" -> data)
      case None       => Seq.empty
    }
    val renamedResult = try22(Type[In], Type.Ctor22.of[examples.kinds.Alias.Renamed22]) match {
      case Some(data) => Seq("as renamed" -> data)
      case None       => Seq.empty
    }
    val extraResult = try22(Type[In], Type.Ctor22.of[examples.kinds.Alias.Extra22]) match {
      case Some(data) => Seq("as extra" -> data)
      case None       => Seq.empty
    }
    val fixedFrontResult = try22(Type[In], Type.Ctor22.of[examples.kinds.Alias.FixedFront22]) match {
      case Some(data) => Seq("as fixed front" -> data)
      case None       => Seq.empty
    }
    val fixedBackResult = try22(Type[In], Type.Ctor22.of[examples.kinds.Alias.FixedBack22]) match {
      case Some(data) => Seq("as fixed back" -> data)
      case None       => Seq.empty
    }
    val setA = try21(Type[In], ctor.setA(using Int)) match {
      case Some(data) => Seq("as setA" -> data)
      case None       => Seq.empty
    }
    val setB = try21(Type[In], ctor.setB(using Int)) match {
      case Some(data) => Seq("as setB" -> data)
      case None       => Seq.empty
    }
    val setC = try21(Type[In], ctor.setC(using Int)) match {
      case Some(data) => Seq("as setC" -> data)
      case None       => Seq.empty
    }
    val setD = try21(Type[In], ctor.setD(using Int)) match {
      case Some(data) => Seq("as setD" -> data)
      case None       => Seq.empty
    }
    val setE = try21(Type[In], ctor.setE(using Int)) match {
      case Some(data) => Seq("as setE" -> data)
      case None       => Seq.empty
    }
    val setF = try21(Type[In], ctor.setF(using Int)) match {
      case Some(data) => Seq("as setF" -> data)
      case None       => Seq.empty
    }
    val setG = try21(Type[In], ctor.setG(using Int)) match {
      case Some(data) => Seq("as setG" -> data)
      case None       => Seq.empty
    }
    val setH = try21(Type[In], ctor.setH(using Int)) match {
      case Some(data) => Seq("as setH" -> data)
      case None       => Seq.empty
    }
    val setI = try21(Type[In], ctor.setI(using Int)) match {
      case Some(data) => Seq("as setI" -> data)
      case None       => Seq.empty
    }
    val setJ = try21(Type[In], ctor.setJ(using Int)) match {
      case Some(data) => Seq("as setJ" -> data)
      case None       => Seq.empty
    }
    val setK = try21(Type[In], ctor.setK(using Int)) match {
      case Some(data) => Seq("as setK" -> data)
      case None       => Seq.empty
    }
    val setL = try21(Type[In], ctor.setL(using Int)) match {
      case Some(data) => Seq("as setL" -> data)
      case None       => Seq.empty
    }
    val setM = try21(Type[In], ctor.setM(using Int)) match {
      case Some(data) => Seq("as setM" -> data)
      case None       => Seq.empty
    }
    val setN = try21(Type[In], ctor.setN(using Int)) match {
      case Some(data) => Seq("as setN" -> data)
      case None       => Seq.empty
    }
    val setO = try21(Type[In], ctor.setO(using Int)) match {
      case Some(data) => Seq("as setO" -> data)
      case None       => Seq.empty
    }
    val setP = try21(Type[In], ctor.setP(using Int)) match {
      case Some(data) => Seq("as setP" -> data)
      case None       => Seq.empty
    }
    val setQ = try21(Type[In], ctor.setQ(using Int)) match {
      case Some(data) => Seq("as setQ" -> data)
      case None       => Seq.empty
    }
    val setR = try21(Type[In], ctor.setR(using Int)) match {
      case Some(data) => Seq("as setR" -> data)
      case None       => Seq.empty
    }
    val setS = try21(Type[In], ctor.setS(using Int)) match {
      case Some(data) => Seq("as setS" -> data)
      case None       => Seq.empty
    }
    val setT = try21(Type[In], ctor.setT(using Int)) match {
      case Some(data) => Seq("as setT" -> data)
      case None       => Seq.empty
    }
    val setU = try21(Type[In], ctor.setU(using Int)) match {
      case Some(data) => Seq("as setU" -> data)
      case None       => Seq.empty
    }
    val setV = try21(Type[In], ctor.setV(using Int)) match {
      case Some(data) => Seq("as setV" -> data)
      case None       => Seq.empty
    }
    // format: off
    val result =
      (classResult.view ++ renamedResult.view ++ extraResult.view ++ fixedFrontResult.view ++ fixedBackResult.view ++ setA.view ++ setB.view ++ setC.view ++ setD.view ++ setE.view ++ setF.view ++ setG.view ++ setH.view ++ setI.view ++ setJ.view ++ setK.view ++ setL.view ++ setM.view ++ setN.view ++ setO.view ++ setP.view ++ setQ.view ++ setR.view ++ setS.view ++ setT.view ++ setU.view ++ setV.view).toSeq
    // format: on
    if (result.isEmpty) Expr(Data("Not one of the expected types")) else Expr(Data.map(result*))
  }
}
