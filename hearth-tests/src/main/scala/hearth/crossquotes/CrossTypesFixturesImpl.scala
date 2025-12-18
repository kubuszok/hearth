package hearth
package crossquotes

import hearth.data.Data

/** Fixtured for testing [[CrossTypesSpec]]. */
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
    val result =
      (classResult.view ++ renamedResult.view ++ extraResult.view ++ fixedFrontResult.view ++ fixedBackResult.view).toSeq
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
    val classResult = try2(Type[In], Type.Ctor2.of[examples.kinds.Arity2]) match {
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
    val result =
      (classResult.view ++ renamedResult.view ++ extraResult.view ++ fixedFrontResult.view ++ fixedBackResult.view).toSeq
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
    val classResult = try3(Type[In], Type.Ctor3.of[examples.kinds.Arity3]) match {
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
    val result =
      (classResult.view ++ renamedResult.view ++ extraResult.view ++ fixedFrontResult.view ++ fixedBackResult.view).toSeq
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
    val classResult = try4(Type[In], Type.Ctor4.of[examples.kinds.Arity4]) match {
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

    val result =
      (classResult.view ++ renamedResult.view ++ extraResult.view ++ fixedFrontResult.view ++ fixedBackResult.view).toSeq
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
    val classResult = try5(Type[In], Type.Ctor5.of[examples.kinds.Arity5]) match {
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
    val result =
      (classResult.view ++ renamedResult.view ++ extraResult.view ++ fixedFrontResult.view ++ fixedBackResult.view).toSeq
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
    val classResult = try6(Type[In], Type.Ctor6.of[examples.kinds.Arity6]) match {
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
    val result =
      (classResult.view ++ renamedResult.view ++ extraResult.view ++ fixedFrontResult.view ++ fixedBackResult.view).toSeq
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
    val classResult = try7(Type[In], Type.Ctor7.of[examples.kinds.Arity7]) match {
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
    val result =
      (classResult.view ++ renamedResult.view ++ extraResult.view ++ fixedFrontResult.view ++ fixedBackResult.view).toSeq
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
    val classResult = try8(Type[In], Type.Ctor8.of[examples.kinds.Arity8]) match {
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
    val result =
      (classResult.view ++ renamedResult.view ++ extraResult.view ++ fixedFrontResult.view ++ fixedBackResult.view).toSeq
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
    val classResult = try9(Type[In], Type.Ctor9.of[examples.kinds.Arity9]) match {
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
    val result =
      (classResult.view ++ renamedResult.view ++ extraResult.view ++ fixedFrontResult.view ++ fixedBackResult.view).toSeq
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
    val classResult = try10(Type[In], Type.Ctor10.of[examples.kinds.Arity10]) match {
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
    val result =
      (classResult.view ++ renamedResult.view ++ extraResult.view ++ fixedFrontResult.view ++ fixedBackResult.view).toSeq
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
    val classResult = try11(Type[In], Type.Ctor11.of[examples.kinds.Arity11]) match {
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
    val result =
      (classResult.view ++ renamedResult.view ++ extraResult.view ++ fixedFrontResult.view ++ fixedBackResult.view).toSeq
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
    val classResult = try12(Type[In], Type.Ctor12.of[examples.kinds.Arity12]) match {
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
    val result =
      (classResult.view ++ renamedResult.view ++ extraResult.view ++ fixedFrontResult.view ++ fixedBackResult.view).toSeq
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
    val classResult = try13(Type[In], Type.Ctor13.of[examples.kinds.Arity13]) match {
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
    val result =
      (classResult.view ++ renamedResult.view ++ extraResult.view ++ fixedFrontResult.view ++ fixedBackResult.view).toSeq
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
    val classResult = try14(Type[In], Type.Ctor14.of[examples.kinds.Arity14]) match {
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
    val result =
      (classResult.view ++ renamedResult.view ++ extraResult.view ++ fixedFrontResult.view ++ fixedBackResult.view).toSeq
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
    val classResult = try15(Type[In], Type.Ctor15.of[examples.kinds.Arity15]) match {
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
    val result =
      (classResult.view ++ renamedResult.view ++ extraResult.view ++ fixedFrontResult.view ++ fixedBackResult.view).toSeq
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
    val classResult = try16(Type[In], Type.Ctor16.of[examples.kinds.Arity16]) match {
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
    val result =
      (classResult.view ++ renamedResult.view ++ extraResult.view ++ fixedFrontResult.view ++ fixedBackResult.view).toSeq
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
    val classResult = try17(Type[In], Type.Ctor17.of[examples.kinds.Arity17]) match {
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
    val result =
      (classResult.view ++ renamedResult.view ++ extraResult.view ++ fixedFrontResult.view ++ fixedBackResult.view).toSeq
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
    val classResult = try18(Type[In], Type.Ctor18.of[examples.kinds.Arity18]) match {
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
    val result =
      (classResult.view ++ renamedResult.view ++ extraResult.view ++ fixedFrontResult.view ++ fixedBackResult.view).toSeq
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
    val classResult = try19(Type[In], Type.Ctor19.of[examples.kinds.Arity19]) match {
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
    val result =
      (classResult.view ++ renamedResult.view ++ extraResult.view ++ fixedFrontResult.view ++ fixedBackResult.view).toSeq
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
    val classResult = try20(Type[In], Type.Ctor20.of[examples.kinds.Arity20]) match {
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
    val result =
      (classResult.view ++ renamedResult.view ++ extraResult.view ++ fixedFrontResult.view ++ fixedBackResult.view).toSeq
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
    val classResult = try21(Type[In], Type.Ctor21.of[examples.kinds.Arity21]) match {
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
    val result =
      (classResult.view ++ renamedResult.view ++ extraResult.view ++ fixedFrontResult.view ++ fixedBackResult.view).toSeq
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
    val classResult = try22(Type[In], Type.Ctor22.of[examples.kinds.Arity22]) match {
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
    val result =
      (classResult.view ++ renamedResult.view ++ extraResult.view ++ fixedFrontResult.view ++ fixedBackResult.view).toSeq
    if (result.isEmpty) Expr(Data("Not one of the expected types")) else Expr(Data.map(result*))
  }
}
