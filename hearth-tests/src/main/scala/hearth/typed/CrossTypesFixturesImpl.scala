package hearth
package typed

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
            Type.of[Option[A]].plainPrint
          },
          "viaImport" -> Data {
            val b = Type[A].as_??
            import b.Underlying as B
            Type.of[Option[B]].plainPrint
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
            def body[B: Ctx] =
              Type.of[Option[B]].plainPrint
            body(using Ctx(Type[A]))
          },
          "viaInheritance" -> Data {
            val a = Type[A]
            trait HelperA {
              type B
              implicit val B: Type[B]
            }
            trait HelperB extends HelperA {
              def result = Type.of[Option[B]].plainPrint
            }
            class HelperC extends HelperB {
              type B = A
              implicit val B: Type[B] = a
            }
            (new HelperC).result
          }
        )
      )
    )
  }

  private val String = Type.of[String]

  def testTypeCtor1[In: Type]: Expr[Data] = {
    // TODO: type projector test as well

    val classTest = Type.Ctor1.of[examples.kinds.Arity1]
    val classResult = Type[In] match {
      case classTest(aParam) =>
        import aParam.Underlying as A
        Seq(
          "as class" -> Data.map(
            "unapplied" -> Data.list(Data(A.plainPrint)),
            "reapplied" -> Data(classTest(using String).plainPrint)
          )
        )
      case _ => Seq.empty
    }

    val renamedTest = Type.Ctor1.of[examples.kinds.Alias.Renamed1]
    val renamedResult = Type[In] match {
      case renamedTest(aParam) =>
        import aParam.Underlying as A
        Seq(
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(Data(A.plainPrint)),
            "reapplied" -> Data(renamedTest(using String).plainPrint)
          )
        )
      case _ => Seq.empty
    }

    val extraTest = Type.Ctor1.of[examples.kinds.Alias.Extra1]
    val extraResult = Type[In] match {
      case extraTest(aParam) =>
        import aParam.Underlying as A
        Seq(
          "as extra" -> Data.map(
            "unapplied" -> Data.list(Data(A.plainPrint)),
            "reapplied" -> Data(extraTest(using String).plainPrint)
          )
        )
      case _ => Seq.empty
    }

    val fixedFrontTest = Type.Ctor1.of[examples.kinds.Alias.FixedFront1]
    val fixedFrontResult = Type[In] match {
      case fixedFrontTest(aParam) =>
        import aParam.Underlying as A
        Seq(
          "as fixed front" -> Data.map(
            "unapplied" -> Data.list(Data(A.plainPrint)),
            "reapplied" -> Data(fixedFrontTest(using String).plainPrint)
          )
        )
      case _ => Seq.empty
    }

    val fixedBackTest = Type.Ctor1.of[examples.kinds.Alias.FixedBack1]
    val fixedBackResult = Type[In] match {
      case fixedBackTest(aParam) =>
        import aParam.Underlying as A
        Seq(
          "as fixed back" -> Data.map(
            "unapplied" -> Data.list(Data(A.plainPrint)),
            "reapplied" -> Data(fixedBackTest(using String).plainPrint)
          )
        )
      case _ => Seq.empty
    }

    val result =
      (classResult.view ++ renamedResult.view ++ extraResult.view ++ fixedFrontResult.view ++ fixedBackResult.view).toSeq
    if (result.isEmpty) Expr(Data("Not one of the expected types")) else Expr(Data.map(result*))
  }

  def testTypeCtor2[In: Type]: Expr[Data] = {
    // TODO: type projector test as well

    val classTest = Type.Ctor2.of[examples.kinds.Arity2]
    val classResult = Type[In] match {
      case classTest(aParam, bParam) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        Seq(
          "as class" -> Data.map(
            "unapplied" -> Data.list(Data(A.plainPrint), Data(B.plainPrint)),
            "reapplied" -> Data(classTest(using String, String).plainPrint)
          )
        )
      case _ => Seq.empty
    }

    val renamedTest = Type.Ctor2.of[examples.kinds.Alias.Renamed2]
    val renamedResult = Type[In] match {
      case renamedTest(aParam, bParam) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        Seq(
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(Data(A.plainPrint), Data(B.plainPrint)),
            "reapplied" -> Data(renamedTest(using String, String).plainPrint)
          )
        )
      case _ => Seq.empty
    }

    val extraTest = Type.Ctor2.of[examples.kinds.Alias.Extra2]
    val extraResult = Type[In] match {
      case extraTest(aParam, bParam) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        Seq(
          "as extra" -> Data.map(
            "unapplied" -> Data.list(Data(A.plainPrint), Data(B.plainPrint)),
            "reapplied" -> Data(extraTest(using String, String).plainPrint)
          )
        )
      case _ => Seq.empty
    }

    val fixedFrontTest = Type.Ctor2.of[examples.kinds.Alias.FixedFront2]
    val fixedFrontResult = Type[In] match {
      case fixedFrontTest(aParam, bParam) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        Seq(
          "as fixed front" -> Data.map(
            "unapplied" -> Data.list(Data(A.plainPrint), Data(B.plainPrint)),
            "reapplied" -> Data(fixedFrontTest(using String, String).plainPrint)
          )
        )
      case _ => Seq.empty
    }

    val fixedBackTest = Type.Ctor2.of[examples.kinds.Alias.FixedBack2]
    val fixedBackResult = Type[In] match {
      case fixedBackTest(aParam, bParam) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        Seq(
          "as fixed back" -> Data.map(
            "unapplied" -> Data.list(Data(A.plainPrint), Data(B.plainPrint)),
            "reapplied" -> Data(fixedBackTest(using String, String).plainPrint)
          )
        )
      case _ => Seq.empty
    }

    val result =
      (classResult.view ++ renamedResult.view ++ extraResult.view ++ fixedFrontResult.view ++ fixedBackResult.view).toSeq
    if (result.isEmpty) Expr(Data("Not one of the expected types")) else Expr(Data.map(result*))
  }

  def testTypeCtor3[In: Type]: Expr[Data] = {
    // TODO: type projector test as well

    val classTest = Type.Ctor3.of[examples.kinds.Arity3]
    val classResult = Type[In] match {
      case classTest(aParam, bParam, cParam) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        Seq(
          "as class" -> Data.map(
            "unapplied" -> Data.list(Data(A.plainPrint), Data(B.plainPrint), Data(C.plainPrint)),
            "reapplied" -> Data(classTest(using String, String, String).plainPrint)
          )
        )
      case _ => Seq.empty
    }

    val renamedTest = Type.Ctor3.of[examples.kinds.Alias.Renamed3]
    val renamedResult = Type[In] match {
      case renamedTest(aParam, bParam, cParam) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        Seq(
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(Data(A.plainPrint), Data(B.plainPrint), Data(C.plainPrint)),
            "reapplied" -> Data(renamedTest(using String, String, String).plainPrint)
          )
        )
      case _ => Seq.empty
    }

    val extraTest = Type.Ctor3.of[examples.kinds.Alias.Extra3]
    val extraResult = Type[In] match {
      case extraTest(aParam, bParam, cParam) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        Seq(
          "as extra" -> Data.map(
            "unapplied" -> Data.list(Data(A.plainPrint), Data(B.plainPrint), Data(C.plainPrint)),
            "reapplied" -> Data(extraTest(using String, String, String).plainPrint)
          )
        )
      case _ => Seq.empty
    }

    val fixedFrontTest = Type.Ctor3.of[examples.kinds.Alias.FixedFront3]
    val fixedFrontResult = Type[In] match {
      case fixedFrontTest(aParam, bParam, cParam) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        Seq(
          "as fixed front" -> Data.map(
            "unapplied" -> Data.list(Data(A.plainPrint), Data(B.plainPrint), Data(C.plainPrint)),
            "reapplied" -> Data(fixedFrontTest(using String, String, String).plainPrint)
          )
        )
      case _ => Seq.empty
    }

    val fixedBackTest = Type.Ctor3.of[examples.kinds.Alias.FixedBack3]
    val fixedBackResult = Type[In] match {
      case fixedBackTest(aParam, bParam, cParam) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        Seq(
          "as fixed back" -> Data.map(
            "unapplied" -> Data.list(Data(A.plainPrint), Data(B.plainPrint), Data(C.plainPrint)),
            "reapplied" -> Data(fixedBackTest(using String, String, String).plainPrint)
          )
        )
      case _ => Seq.empty
    }

    val result =
      (classResult.view ++ renamedResult.view ++ extraResult.view ++ fixedFrontResult.view ++ fixedBackResult.view).toSeq
    if (result.isEmpty) Expr(Data("Not one of the expected types")) else Expr(Data.map(result*))
  }

  def testTypeCtor4[In: Type]: Expr[Data] = {
    // TODO: type projector test as well

    val classTest = Type.Ctor4.of[examples.kinds.Arity4]
    val classResult = Type[In] match {
      case classTest(aParam, bParam, cParam, dParam) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        Seq(
          "as class" -> Data.map(
            "unapplied" -> Data.list(Data(A.plainPrint), Data(B.plainPrint), Data(C.plainPrint), Data(D.plainPrint)),
            "reapplied" -> Data(classTest(using String, String, String, String).plainPrint)
          )
        )
      case _ => Seq.empty
    }

    val renamedTest = Type.Ctor4.of[examples.kinds.Alias.Renamed4]
    val renamedResult = Type[In] match {
      case renamedTest(aParam, bParam, cParam, dParam) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        Seq(
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(Data(A.plainPrint), Data(B.plainPrint), Data(C.plainPrint), Data(D.plainPrint)),
            "reapplied" -> Data(renamedTest(using String, String, String, String).plainPrint)
          )
        )
      case _ => Seq.empty
    }

    val extraTest = Type.Ctor4.of[examples.kinds.Alias.Extra4]
    val extraResult = Type[In] match {
      case extraTest(aParam, bParam, cParam, dParam) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        Seq(
          "as extra" -> Data.map(
            "unapplied" -> Data.list(Data(A.plainPrint), Data(B.plainPrint), Data(C.plainPrint), Data(D.plainPrint)),
            "reapplied" -> Data(extraTest(using String, String, String, String).plainPrint)
          )
        )
      case _ => Seq.empty
    }

    val fixedFrontTest = Type.Ctor4.of[examples.kinds.Alias.FixedFront4]
    val fixedFrontResult = Type[In] match {
      case fixedFrontTest(aParam, bParam, cParam, dParam) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        Seq(
          "as fixed front" -> Data.map(
            "unapplied" -> Data.list(Data(A.plainPrint), Data(B.plainPrint), Data(C.plainPrint), Data(D.plainPrint)),
            "reapplied" -> Data(fixedFrontTest(using String, String, String, String).plainPrint)
          )
        )
      case _ => Seq.empty
    }

    val fixedBackTest = Type.Ctor4.of[examples.kinds.Alias.FixedBack4]
    val fixedBackResult = Type[In] match {
      case fixedBackTest(aParam, bParam, cParam, dParam) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        Seq(
          "as fixed back" -> Data.map(
            "unapplied" -> Data.list(Data(A.plainPrint), Data(B.plainPrint), Data(C.plainPrint), Data(D.plainPrint)),
            "reapplied" -> Data(fixedBackTest(using String, String, String, String).plainPrint)
          )
        )
      case _ => Seq.empty
    }

    val result =
      (classResult.view ++ renamedResult.view ++ extraResult.view ++ fixedFrontResult.view ++ fixedBackResult.view).toSeq
    if (result.isEmpty) Expr(Data("Not one of the expected types")) else Expr(Data.map(result*))
  }

  def testTypeCtor5[In: Type]: Expr[Data] = {
    // TODO: type projector test as well

    val classTest = Type.Ctor5.of[examples.kinds.Arity5]
    val classResult = Type[In] match {
      case classTest(aParam, bParam, cParam, dParam, eParam) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        Seq(
          "as class" -> Data.map(
            "unapplied" -> Data
              .list(Data(A.plainPrint), Data(B.plainPrint), Data(C.plainPrint), Data(D.plainPrint), Data(E.plainPrint)),
            "reapplied" -> Data(classTest(using String, String, String, String, String).plainPrint)
          )
        )
      case _ => Seq.empty
    }

    val renamedTest = Type.Ctor5.of[examples.kinds.Alias.Renamed5]
    val renamedResult = Type[In] match {
      case renamedTest(aParam, bParam, cParam, dParam, eParam) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        Seq(
          "as renamed" -> Data.map(
            "unapplied" -> Data
              .list(Data(A.plainPrint), Data(B.plainPrint), Data(C.plainPrint), Data(D.plainPrint), Data(E.plainPrint)),
            "reapplied" -> Data(renamedTest(using String, String, String, String, String).plainPrint)
          )
        )
      case _ => Seq.empty
    }

    val extraTest = Type.Ctor5.of[examples.kinds.Alias.Extra5]
    val extraResult = Type[In] match {
      case extraTest(aParam, bParam, cParam, dParam, eParam) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        Seq(
          "as extra" -> Data.map(
            "unapplied" -> Data
              .list(Data(A.plainPrint), Data(B.plainPrint), Data(C.plainPrint), Data(D.plainPrint), Data(E.plainPrint)),
            "reapplied" -> Data(extraTest(using String, String, String, String, String).plainPrint)
          )
        )
      case _ => Seq.empty
    }

    val fixedFrontTest = Type.Ctor5.of[examples.kinds.Alias.FixedFront5]
    val fixedFrontResult = Type[In] match {
      case fixedFrontTest(aParam, bParam, cParam, dParam, eParam) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        Seq(
          "as fixed front" -> Data.map(
            "unapplied" -> Data
              .list(Data(A.plainPrint), Data(B.plainPrint), Data(C.plainPrint), Data(D.plainPrint), Data(E.plainPrint)),
            "reapplied" -> Data(fixedFrontTest(using String, String, String, String, String).plainPrint)
          )
        )
      case _ => Seq.empty
    }

    val fixedBackTest = Type.Ctor5.of[examples.kinds.Alias.FixedBack5]
    val fixedBackResult = Type[In] match {
      case fixedBackTest(aParam, bParam, cParam, dParam, eParam) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        Seq(
          "as fixed back" -> Data.map(
            "unapplied" -> Data
              .list(Data(A.plainPrint), Data(B.plainPrint), Data(C.plainPrint), Data(D.plainPrint), Data(E.plainPrint)),
            "reapplied" -> Data(fixedBackTest(using String, String, String, String, String).plainPrint)
          )
        )
      case _ => Seq.empty
    }

    val result =
      (classResult.view ++ renamedResult.view ++ extraResult.view ++ fixedFrontResult.view ++ fixedBackResult.view).toSeq
    if (result.isEmpty) Expr(Data("Not one of the expected types")) else Expr(Data.map(result*))
  }

  def testTypeCtor6[In: Type]: Expr[Data] = {
    // TODO: type projector test as well

    val classTest = Type.Ctor6.of[examples.kinds.Arity6]
    val classResult = Type[In] match {
      case classTest(aParam, bParam, cParam, dParam, eParam, fParam) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        Seq(
          "as class" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint)
            ),
            "reapplied" -> Data(classTest(using String, String, String, String, String, String).plainPrint)
          )
        )
      case _ => Seq.empty
    }

    val renamedTest = Type.Ctor6.of[examples.kinds.Alias.Renamed6]
    val renamedResult = Type[In] match {
      case renamedTest(aParam, bParam, cParam, dParam, eParam, fParam) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        Seq(
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint)
            ),
            "reapplied" -> Data(renamedTest(using String, String, String, String, String, String).plainPrint)
          )
        )
      case _ => Seq.empty
    }

    val extraTest = Type.Ctor6.of[examples.kinds.Alias.Extra6]
    val extraResult = Type[In] match {
      case extraTest(aParam, bParam, cParam, dParam, eParam, fParam) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        Seq(
          "as extra" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint)
            ),
            "reapplied" -> Data(extraTest(using String, String, String, String, String, String).plainPrint)
          )
        )
      case _ => Seq.empty
    }

    val fixedFrontTest = Type.Ctor6.of[examples.kinds.Alias.FixedFront6]
    val fixedFrontResult = Type[In] match {
      case fixedFrontTest(aParam, bParam, cParam, dParam, eParam, fParam) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        Seq(
          "as fixed front" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint)
            ),
            "reapplied" -> Data(fixedFrontTest(using String, String, String, String, String, String).plainPrint)
          )
        )
      case _ => Seq.empty
    }

    val fixedBackTest = Type.Ctor6.of[examples.kinds.Alias.FixedBack6]
    val fixedBackResult = Type[In] match {
      case fixedBackTest(aParam, bParam, cParam, dParam, eParam, fParam) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        Seq(
          "as fixed back" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint)
            ),
            "reapplied" -> Data(fixedBackTest(using String, String, String, String, String, String).plainPrint)
          )
        )
      case _ => Seq.empty
    }

    val result =
      (classResult.view ++ renamedResult.view ++ extraResult.view ++ fixedFrontResult.view ++ fixedBackResult.view).toSeq
    if (result.isEmpty) Expr(Data("Not one of the expected types")) else Expr(Data.map(result*))
  }

  def testTypeCtor7[In: Type]: Expr[Data] = {
    // TODO: type projector test as well

    val classTest = Type.Ctor7.of[examples.kinds.Arity7]
    val classResult = Type[In] match {
      case classTest(aParam, bParam, cParam, dParam, eParam, fParam, gParam) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        Seq(
          "as class" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint)
            ),
            "reapplied" -> Data(classTest(using String, String, String, String, String, String, String).plainPrint)
          )
        )
      case _ => Seq.empty
    }

    val renamedTest = Type.Ctor7.of[examples.kinds.Alias.Renamed7]
    val renamedResult = Type[In] match {
      case renamedTest(aParam, bParam, cParam, dParam, eParam, fParam, gParam) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        Seq(
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint)
            ),
            "reapplied" -> Data(renamedTest(using String, String, String, String, String, String, String).plainPrint)
          )
        )
      case _ => Seq.empty
    }

    val extraTest = Type.Ctor7.of[examples.kinds.Alias.Extra7]
    val extraResult = Type[In] match {
      case extraTest(aParam, bParam, cParam, dParam, eParam, fParam, gParam) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        Seq(
          "as extra" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint)
            ),
            "reapplied" -> Data(extraTest(using String, String, String, String, String, String, String).plainPrint)
          )
        )
      case _ => Seq.empty
    }

    val fixedFrontTest = Type.Ctor7.of[examples.kinds.Alias.FixedFront7]
    val fixedFrontResult = Type[In] match {
      case fixedFrontTest(aParam, bParam, cParam, dParam, eParam, fParam, gParam) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        Seq(
          "as fixed front" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint)
            ),
            "reapplied" -> Data(fixedFrontTest(using String, String, String, String, String, String, String).plainPrint)
          )
        )
      case _ => Seq.empty
    }

    val fixedBackTest = Type.Ctor7.of[examples.kinds.Alias.FixedBack7]
    val fixedBackResult = Type[In] match {
      case fixedBackTest(aParam, bParam, cParam, dParam, eParam, fParam, gParam) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        Seq(
          "as fixed back" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint)
            ),
            "reapplied" -> Data(fixedBackTest(using String, String, String, String, String, String, String).plainPrint)
          )
        )
      case _ => Seq.empty
    }

    val result =
      (classResult.view ++ renamedResult.view ++ extraResult.view ++ fixedFrontResult.view ++ fixedBackResult.view).toSeq
    if (result.isEmpty) Expr(Data("Not one of the expected types")) else Expr(Data.map(result*))
  }

  def testTypeCtor8[In: Type]: Expr[Data] = {
    // TODO: type projector test as well

    val classTest = Type.Ctor8.of[examples.kinds.Arity8]
    val classResult = Type[In] match {
      case classTest(aParam, bParam, cParam, dParam, eParam, fParam, gParam, hParam) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        Seq(
          "as class" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint)
            ),
            "reapplied" -> Data(
              classTest(using String, String, String, String, String, String, String, String).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val renamedTest = Type.Ctor8.of[examples.kinds.Alias.Renamed8]
    val renamedResult = Type[In] match {
      case renamedTest(aParam, bParam, cParam, dParam, eParam, fParam, gParam, hParam) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        Seq(
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint)
            ),
            "reapplied" -> Data(
              renamedTest(using String, String, String, String, String, String, String, String).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val extraTest = Type.Ctor8.of[examples.kinds.Alias.Extra8]
    val extraResult = Type[In] match {
      case extraTest(aParam, bParam, cParam, dParam, eParam, fParam, gParam, hParam) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        Seq(
          "as extra" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint)
            ),
            "reapplied" -> Data(
              extraTest(using String, String, String, String, String, String, String, String).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val fixedFrontTest = Type.Ctor8.of[examples.kinds.Alias.FixedFront8]
    val fixedFrontResult = Type[In] match {
      case fixedFrontTest(aParam, bParam, cParam, dParam, eParam, fParam, gParam, hParam) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        Seq(
          "as fixed front" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint)
            ),
            "reapplied" -> Data(
              fixedFrontTest(using String, String, String, String, String, String, String, String).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val fixedBackTest = Type.Ctor8.of[examples.kinds.Alias.FixedBack8]
    val fixedBackResult = Type[In] match {
      case fixedBackTest(aParam, bParam, cParam, dParam, eParam, fParam, gParam, hParam) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        Seq(
          "as fixed back" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint)
            ),
            "reapplied" -> Data(
              fixedBackTest(using String, String, String, String, String, String, String, String).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val result =
      (classResult.view ++ renamedResult.view ++ extraResult.view ++ fixedFrontResult.view ++ fixedBackResult.view).toSeq
    if (result.isEmpty) Expr(Data("Not one of the expected types")) else Expr(Data.map(result*))
  }

  def testTypeCtor9[In: Type]: Expr[Data] = {
    // TODO: type projector test as well

    val classTest = Type.Ctor9.of[examples.kinds.Arity9]
    val classResult = Type[In] match {
      case classTest(aParam, bParam, cParam, dParam, eParam, fParam, gParam, hParam, iParam) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        Seq(
          "as class" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint)
            ),
            "reapplied" -> Data(
              classTest(using String, String, String, String, String, String, String, String, String).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val renamedTest = Type.Ctor9.of[examples.kinds.Alias.Renamed9]
    val renamedResult = Type[In] match {
      case renamedTest(aParam, bParam, cParam, dParam, eParam, fParam, gParam, hParam, iParam) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        Seq(
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint)
            ),
            "reapplied" -> Data(
              renamedTest(using String, String, String, String, String, String, String, String, String).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val extraTest = Type.Ctor9.of[examples.kinds.Alias.Extra9]
    val extraResult = Type[In] match {
      case extraTest(aParam, bParam, cParam, dParam, eParam, fParam, gParam, hParam, iParam) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        Seq(
          "as extra" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint)
            ),
            "reapplied" -> Data(
              extraTest(using String, String, String, String, String, String, String, String, String).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val fixedFrontTest = Type.Ctor9.of[examples.kinds.Alias.FixedFront9]
    val fixedFrontResult = Type[In] match {
      case fixedFrontTest(aParam, bParam, cParam, dParam, eParam, fParam, gParam, hParam, iParam) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        Seq(
          "as fixed front" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint)
            ),
            "reapplied" -> Data(
              fixedFrontTest(using String, String, String, String, String, String, String, String, String).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val fixedBackTest = Type.Ctor9.of[examples.kinds.Alias.FixedBack9]
    val fixedBackResult = Type[In] match {
      case fixedBackTest(aParam, bParam, cParam, dParam, eParam, fParam, gParam, hParam, iParam) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        Seq(
          "as fixed back" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint)
            ),
            "reapplied" -> Data(
              fixedBackTest(using String, String, String, String, String, String, String, String, String).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val result =
      (classResult.view ++ renamedResult.view ++ extraResult.view ++ fixedFrontResult.view ++ fixedBackResult.view).toSeq
    if (result.isEmpty) Expr(Data("Not one of the expected types")) else Expr(Data.map(result*))
  }

  def testTypeCtor10[In: Type]: Expr[Data] = {
    // TODO: type projector test as well

    val classTest = Type.Ctor10.of[examples.kinds.Arity10]
    val classResult = Type[In] match {
      case classTest(aParam, bParam, cParam, dParam, eParam, fParam, gParam, hParam, iParam, jParam) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        import jParam.Underlying as J
        Seq(
          "as class" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint),
              Data(J.plainPrint)
            ),
            "reapplied" -> Data(
              classTest(using String, String, String, String, String, String, String, String, String, String).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val renamedTest = Type.Ctor10.of[examples.kinds.Alias.Renamed10]
    val renamedResult = Type[In] match {
      case renamedTest(aParam, bParam, cParam, dParam, eParam, fParam, gParam, hParam, iParam, jParam) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        import jParam.Underlying as J
        Seq(
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint),
              Data(J.plainPrint)
            ),
            "reapplied" -> Data(
              renamedTest(using
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String
              ).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val extraTest = Type.Ctor10.of[examples.kinds.Alias.Extra10]
    val extraResult = Type[In] match {
      case extraTest(aParam, bParam, cParam, dParam, eParam, fParam, gParam, hParam, iParam, jParam) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        import jParam.Underlying as J
        Seq(
          "as extra" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint),
              Data(J.plainPrint)
            ),
            "reapplied" -> Data(
              extraTest(using String, String, String, String, String, String, String, String, String, String).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val fixedFrontTest = Type.Ctor10.of[examples.kinds.Alias.FixedFront10]
    val fixedFrontResult = Type[In] match {
      case fixedFrontTest(aParam, bParam, cParam, dParam, eParam, fParam, gParam, hParam, iParam, jParam) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        import jParam.Underlying as J
        Seq(
          "as fixed front" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint),
              Data(J.plainPrint)
            ),
            "reapplied" -> Data(
              fixedFrontTest(using
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String
              ).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val fixedBackTest = Type.Ctor10.of[examples.kinds.Alias.FixedBack10]
    val fixedBackResult = Type[In] match {
      case fixedBackTest(aParam, bParam, cParam, dParam, eParam, fParam, gParam, hParam, iParam, jParam) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        import jParam.Underlying as J
        Seq(
          "as fixed back" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint),
              Data(J.plainPrint)
            ),
            "reapplied" -> Data(
              fixedBackTest(using
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String
              ).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val result =
      (classResult.view ++ renamedResult.view ++ extraResult.view ++ fixedFrontResult.view ++ fixedBackResult.view).toSeq
    if (result.isEmpty) Expr(Data("Not one of the expected types")) else Expr(Data.map(result*))
  }

  def testTypeCtor11[In: Type]: Expr[Data] = {
    // TODO: type projector test as well

    val classTest = Type.Ctor11.of[examples.kinds.Arity11]
    val classResult = Type[In] match {
      case classTest(aParam, bParam, cParam, dParam, eParam, fParam, gParam, hParam, iParam, jParam, kParam) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        import jParam.Underlying as J
        import kParam.Underlying as K
        Seq(
          "as class" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint),
              Data(J.plainPrint),
              Data(K.plainPrint)
            ),
            "reapplied" -> Data(
              classTest(using
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String
              ).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val renamedTest = Type.Ctor11.of[examples.kinds.Alias.Renamed11]
    val renamedResult = Type[In] match {
      case renamedTest(aParam, bParam, cParam, dParam, eParam, fParam, gParam, hParam, iParam, jParam, kParam) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        import jParam.Underlying as J
        import kParam.Underlying as K
        Seq(
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint),
              Data(J.plainPrint),
              Data(K.plainPrint)
            ),
            "reapplied" -> Data(
              renamedTest(using
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String
              ).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val extraTest = Type.Ctor11.of[examples.kinds.Alias.Extra11]
    val extraResult = Type[In] match {
      case extraTest(aParam, bParam, cParam, dParam, eParam, fParam, gParam, hParam, iParam, jParam, kParam) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        import jParam.Underlying as J
        import kParam.Underlying as K
        Seq(
          "as extra" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint),
              Data(J.plainPrint),
              Data(K.plainPrint)
            ),
            "reapplied" -> Data(
              extraTest(using
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String
              ).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val fixedFrontTest = Type.Ctor11.of[examples.kinds.Alias.FixedFront11]
    val fixedFrontResult = Type[In] match {
      case fixedFrontTest(aParam, bParam, cParam, dParam, eParam, fParam, gParam, hParam, iParam, jParam, kParam) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        import jParam.Underlying as J
        import kParam.Underlying as K
        Seq(
          "as fixed front" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint),
              Data(J.plainPrint),
              Data(K.plainPrint)
            ),
            "reapplied" -> Data(
              fixedFrontTest(using
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String
              ).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val fixedBackTest = Type.Ctor11.of[examples.kinds.Alias.FixedBack11]
    val fixedBackResult = Type[In] match {
      case fixedBackTest(aParam, bParam, cParam, dParam, eParam, fParam, gParam, hParam, iParam, jParam, kParam) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        import jParam.Underlying as J
        import kParam.Underlying as K
        Seq(
          "as fixed back" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint),
              Data(J.plainPrint),
              Data(K.plainPrint)
            ),
            "reapplied" -> Data(
              fixedBackTest(using
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String
              ).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val result =
      (classResult.view ++ renamedResult.view ++ extraResult.view ++ fixedFrontResult.view ++ fixedBackResult.view).toSeq
    if (result.isEmpty) Expr(Data("Not one of the expected types")) else Expr(Data.map(result*))
  }

  def testTypeCtor12[In: Type]: Expr[Data] = {
    // TODO: type projector test as well

    val classTest = Type.Ctor12.of[examples.kinds.Arity12]
    val classResult = Type[In] match {
      case classTest(aParam, bParam, cParam, dParam, eParam, fParam, gParam, hParam, iParam, jParam, kParam, lParam) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        import jParam.Underlying as J
        import kParam.Underlying as K
        import lParam.Underlying as L
        Seq(
          "as class" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint),
              Data(J.plainPrint),
              Data(K.plainPrint),
              Data(L.plainPrint)
            ),
            "reapplied" -> Data(
              classTest(using
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String
              ).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val renamedTest = Type.Ctor12.of[examples.kinds.Alias.Renamed12]
    val renamedResult = Type[In] match {
      case renamedTest(
            aParam,
            bParam,
            cParam,
            dParam,
            eParam,
            fParam,
            gParam,
            hParam,
            iParam,
            jParam,
            kParam,
            lParam
          ) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        import jParam.Underlying as J
        import kParam.Underlying as K
        import lParam.Underlying as L
        Seq(
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint),
              Data(J.plainPrint),
              Data(K.plainPrint),
              Data(L.plainPrint)
            ),
            "reapplied" -> Data(
              renamedTest(using
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String
              ).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val extraTest = Type.Ctor12.of[examples.kinds.Alias.Extra12]
    val extraResult = Type[In] match {
      case extraTest(aParam, bParam, cParam, dParam, eParam, fParam, gParam, hParam, iParam, jParam, kParam, lParam) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        import jParam.Underlying as J
        import kParam.Underlying as K
        import lParam.Underlying as L
        Seq(
          "as extra" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint),
              Data(J.plainPrint),
              Data(K.plainPrint),
              Data(L.plainPrint)
            ),
            "reapplied" -> Data(
              extraTest(using
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String
              ).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val fixedFrontTest = Type.Ctor12.of[examples.kinds.Alias.FixedFront12]
    val fixedFrontResult = Type[In] match {
      case fixedFrontTest(
            aParam,
            bParam,
            cParam,
            dParam,
            eParam,
            fParam,
            gParam,
            hParam,
            iParam,
            jParam,
            kParam,
            lParam
          ) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        import jParam.Underlying as J
        import kParam.Underlying as K
        import lParam.Underlying as L
        Seq(
          "as fixed front" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint),
              Data(J.plainPrint),
              Data(K.plainPrint),
              Data(L.plainPrint)
            ),
            "reapplied" -> Data(
              fixedFrontTest(using
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String
              ).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val fixedBackTest = Type.Ctor12.of[examples.kinds.Alias.FixedBack12]
    val fixedBackResult = Type[In] match {
      case fixedBackTest(
            aParam,
            bParam,
            cParam,
            dParam,
            eParam,
            fParam,
            gParam,
            hParam,
            iParam,
            jParam,
            kParam,
            lParam
          ) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        import jParam.Underlying as J
        import kParam.Underlying as K
        import lParam.Underlying as L
        Seq(
          "as fixed back" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint),
              Data(J.plainPrint),
              Data(K.plainPrint),
              Data(L.plainPrint)
            ),
            "reapplied" -> Data(
              fixedBackTest(using
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String
              ).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val result =
      (classResult.view ++ renamedResult.view ++ extraResult.view ++ fixedFrontResult.view ++ fixedBackResult.view).toSeq
    if (result.isEmpty) Expr(Data("Not one of the expected types")) else Expr(Data.map(result*))
  }

  def testTypeCtor13[In: Type]: Expr[Data] = {
    // TODO: type projector test as well

    val classTest = Type.Ctor13.of[examples.kinds.Arity13]
    val classResult = Type[In] match {
      case classTest(
            aParam,
            bParam,
            cParam,
            dParam,
            eParam,
            fParam,
            gParam,
            hParam,
            iParam,
            jParam,
            kParam,
            lParam,
            mParam
          ) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        import jParam.Underlying as J
        import kParam.Underlying as K
        import lParam.Underlying as L
        import mParam.Underlying as M
        Seq(
          "as class" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint),
              Data(J.plainPrint),
              Data(K.plainPrint),
              Data(L.plainPrint),
              Data(M.plainPrint)
            ),
            "reapplied" -> Data(
              classTest(using
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String
              ).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val renamedTest = Type.Ctor13.of[examples.kinds.Alias.Renamed13]
    val renamedResult = Type[In] match {
      case renamedTest(
            aParam,
            bParam,
            cParam,
            dParam,
            eParam,
            fParam,
            gParam,
            hParam,
            iParam,
            jParam,
            kParam,
            lParam,
            mParam
          ) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        import jParam.Underlying as J
        import kParam.Underlying as K
        import lParam.Underlying as L
        import mParam.Underlying as M
        Seq(
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint),
              Data(J.plainPrint),
              Data(K.plainPrint),
              Data(L.plainPrint),
              Data(M.plainPrint)
            ),
            "reapplied" -> Data(
              renamedTest(using
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String
              ).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val extraTest = Type.Ctor13.of[examples.kinds.Alias.Extra13]
    val extraResult = Type[In] match {
      case extraTest(
            aParam,
            bParam,
            cParam,
            dParam,
            eParam,
            fParam,
            gParam,
            hParam,
            iParam,
            jParam,
            kParam,
            lParam,
            mParam
          ) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        import jParam.Underlying as J
        import kParam.Underlying as K
        import lParam.Underlying as L
        import mParam.Underlying as M
        Seq(
          "as extra" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint),
              Data(J.plainPrint),
              Data(K.plainPrint),
              Data(L.plainPrint),
              Data(M.plainPrint)
            ),
            "reapplied" -> Data(
              extraTest(using
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String
              ).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val fixedFrontTest = Type.Ctor13.of[examples.kinds.Alias.FixedFront13]
    val fixedFrontResult = Type[In] match {
      case fixedFrontTest(
            aParam,
            bParam,
            cParam,
            dParam,
            eParam,
            fParam,
            gParam,
            hParam,
            iParam,
            jParam,
            kParam,
            lParam,
            mParam
          ) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        import jParam.Underlying as J
        import kParam.Underlying as K
        import lParam.Underlying as L
        import mParam.Underlying as M
        Seq(
          "as fixed front" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint),
              Data(J.plainPrint),
              Data(K.plainPrint),
              Data(L.plainPrint),
              Data(M.plainPrint)
            ),
            "reapplied" -> Data(
              fixedFrontTest(using
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String
              ).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val fixedBackTest = Type.Ctor13.of[examples.kinds.Alias.FixedBack13]
    val fixedBackResult = Type[In] match {
      case fixedBackTest(
            aParam,
            bParam,
            cParam,
            dParam,
            eParam,
            fParam,
            gParam,
            hParam,
            iParam,
            jParam,
            kParam,
            lParam,
            mParam
          ) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        import jParam.Underlying as J
        import kParam.Underlying as K
        import lParam.Underlying as L
        import mParam.Underlying as M
        Seq(
          "as fixed back" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint),
              Data(J.plainPrint),
              Data(K.plainPrint),
              Data(L.plainPrint),
              Data(M.plainPrint)
            ),
            "reapplied" -> Data(
              fixedBackTest(using
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String
              ).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val result =
      (classResult.view ++ renamedResult.view ++ extraResult.view ++ fixedFrontResult.view ++ fixedBackResult.view).toSeq
    if (result.isEmpty) Expr(Data("Not one of the expected types")) else Expr(Data.map(result*))
  }

  def testTypeCtor14[In: Type]: Expr[Data] = {
    // TODO: type projector test as well

    val classTest = Type.Ctor14.of[examples.kinds.Arity14]
    val classResult = Type[In] match {
      case classTest(
            aParam,
            bParam,
            cParam,
            dParam,
            eParam,
            fParam,
            gParam,
            hParam,
            iParam,
            jParam,
            kParam,
            lParam,
            mParam,
            nParam
          ) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        import jParam.Underlying as J
        import kParam.Underlying as K
        import lParam.Underlying as L
        import mParam.Underlying as M
        import nParam.Underlying as N
        Seq(
          "as class" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint),
              Data(J.plainPrint),
              Data(K.plainPrint),
              Data(L.plainPrint),
              Data(M.plainPrint),
              Data(N.plainPrint)
            ),
            "reapplied" -> Data(
              classTest(using
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String
              ).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val renamedTest = Type.Ctor14.of[examples.kinds.Alias.Renamed14]
    val renamedResult = Type[In] match {
      case renamedTest(
            aParam,
            bParam,
            cParam,
            dParam,
            eParam,
            fParam,
            gParam,
            hParam,
            iParam,
            jParam,
            kParam,
            lParam,
            mParam,
            nParam
          ) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        import jParam.Underlying as J
        import kParam.Underlying as K
        import lParam.Underlying as L
        import mParam.Underlying as M
        import nParam.Underlying as N
        Seq(
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint),
              Data(J.plainPrint),
              Data(K.plainPrint),
              Data(L.plainPrint),
              Data(M.plainPrint),
              Data(N.plainPrint)
            ),
            "reapplied" -> Data(
              renamedTest(using
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String
              ).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val extraTest = Type.Ctor14.of[examples.kinds.Alias.Extra14]
    val extraResult = Type[In] match {
      case extraTest(
            aParam,
            bParam,
            cParam,
            dParam,
            eParam,
            fParam,
            gParam,
            hParam,
            iParam,
            jParam,
            kParam,
            lParam,
            mParam,
            nParam
          ) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        import jParam.Underlying as J
        import kParam.Underlying as K
        import lParam.Underlying as L
        import mParam.Underlying as M
        import nParam.Underlying as N
        Seq(
          "as extra" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint),
              Data(J.plainPrint),
              Data(K.plainPrint),
              Data(L.plainPrint),
              Data(M.plainPrint),
              Data(N.plainPrint)
            ),
            "reapplied" -> Data(
              extraTest(using
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String
              ).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val fixedFrontTest = Type.Ctor14.of[examples.kinds.Alias.FixedFront14]
    val fixedFrontResult = Type[In] match {
      case fixedFrontTest(
            aParam,
            bParam,
            cParam,
            dParam,
            eParam,
            fParam,
            gParam,
            hParam,
            iParam,
            jParam,
            kParam,
            lParam,
            mParam,
            nParam
          ) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        import jParam.Underlying as J
        import kParam.Underlying as K
        import lParam.Underlying as L
        import mParam.Underlying as M
        import nParam.Underlying as N
        Seq(
          "as fixed front" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint),
              Data(J.plainPrint),
              Data(K.plainPrint),
              Data(L.plainPrint),
              Data(M.plainPrint),
              Data(N.plainPrint)
            ),
            "reapplied" -> Data(
              fixedFrontTest(using
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String
              ).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val fixedBackTest = Type.Ctor14.of[examples.kinds.Alias.FixedBack14]
    val fixedBackResult = Type[In] match {
      case fixedBackTest(
            aParam,
            bParam,
            cParam,
            dParam,
            eParam,
            fParam,
            gParam,
            hParam,
            iParam,
            jParam,
            kParam,
            lParam,
            mParam,
            nParam
          ) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        import jParam.Underlying as J
        import kParam.Underlying as K
        import lParam.Underlying as L
        import mParam.Underlying as M
        import nParam.Underlying as N
        Seq(
          "as fixed back" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint),
              Data(J.plainPrint),
              Data(K.plainPrint),
              Data(L.plainPrint),
              Data(M.plainPrint),
              Data(N.plainPrint)
            ),
            "reapplied" -> Data(
              fixedBackTest(using
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String
              ).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val result =
      (classResult.view ++ renamedResult.view ++ extraResult.view ++ fixedFrontResult.view ++ fixedBackResult.view).toSeq
    if (result.isEmpty) Expr(Data("Not one of the expected types")) else Expr(Data.map(result*))
  }

  def testTypeCtor15[In: Type]: Expr[Data] = {
    // TODO: type projector test as well

    val classTest = Type.Ctor15.of[examples.kinds.Arity15]
    val classResult = Type[In] match {
      case classTest(
            aParam,
            bParam,
            cParam,
            dParam,
            eParam,
            fParam,
            gParam,
            hParam,
            iParam,
            jParam,
            kParam,
            lParam,
            mParam,
            nParam,
            oParam
          ) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        import jParam.Underlying as J
        import kParam.Underlying as K
        import lParam.Underlying as L
        import mParam.Underlying as M
        import nParam.Underlying as N
        import oParam.Underlying as O
        Seq(
          "as class" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint),
              Data(J.plainPrint),
              Data(K.plainPrint),
              Data(L.plainPrint),
              Data(M.plainPrint),
              Data(N.plainPrint),
              Data(O.plainPrint)
            ),
            "reapplied" -> Data(
              classTest(using
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String
              ).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val renamedTest = Type.Ctor15.of[examples.kinds.Alias.Renamed15]
    val renamedResult = Type[In] match {
      case renamedTest(
            aParam,
            bParam,
            cParam,
            dParam,
            eParam,
            fParam,
            gParam,
            hParam,
            iParam,
            jParam,
            kParam,
            lParam,
            mParam,
            nParam,
            oParam
          ) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        import jParam.Underlying as J
        import kParam.Underlying as K
        import lParam.Underlying as L
        import mParam.Underlying as M
        import nParam.Underlying as N
        import oParam.Underlying as O
        Seq(
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint),
              Data(J.plainPrint),
              Data(K.plainPrint),
              Data(L.plainPrint),
              Data(M.plainPrint),
              Data(N.plainPrint),
              Data(O.plainPrint)
            ),
            "reapplied" -> Data(
              renamedTest(using
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String
              ).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val extraTest = Type.Ctor15.of[examples.kinds.Alias.Extra15]
    val extraResult = Type[In] match {
      case extraTest(
            aParam,
            bParam,
            cParam,
            dParam,
            eParam,
            fParam,
            gParam,
            hParam,
            iParam,
            jParam,
            kParam,
            lParam,
            mParam,
            nParam,
            oParam
          ) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        import jParam.Underlying as J
        import kParam.Underlying as K
        import lParam.Underlying as L
        import mParam.Underlying as M
        import nParam.Underlying as N
        import oParam.Underlying as O
        Seq(
          "as extra" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint),
              Data(J.plainPrint),
              Data(K.plainPrint),
              Data(L.plainPrint),
              Data(M.plainPrint),
              Data(N.plainPrint),
              Data(O.plainPrint)
            ),
            "reapplied" -> Data(
              extraTest(using
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String
              ).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val fixedFrontTest = Type.Ctor15.of[examples.kinds.Alias.FixedFront15]
    val fixedFrontResult = Type[In] match {
      case fixedFrontTest(
            aParam,
            bParam,
            cParam,
            dParam,
            eParam,
            fParam,
            gParam,
            hParam,
            iParam,
            jParam,
            kParam,
            lParam,
            mParam,
            nParam,
            oParam
          ) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        import jParam.Underlying as J
        import kParam.Underlying as K
        import lParam.Underlying as L
        import mParam.Underlying as M
        import nParam.Underlying as N
        import oParam.Underlying as O
        Seq(
          "as fixed front" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint),
              Data(J.plainPrint),
              Data(K.plainPrint),
              Data(L.plainPrint),
              Data(M.plainPrint),
              Data(N.plainPrint),
              Data(O.plainPrint)
            ),
            "reapplied" -> Data(
              fixedFrontTest(using
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String
              ).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val fixedBackTest = Type.Ctor15.of[examples.kinds.Alias.FixedBack15]
    val fixedBackResult = Type[In] match {
      case fixedBackTest(
            aParam,
            bParam,
            cParam,
            dParam,
            eParam,
            fParam,
            gParam,
            hParam,
            iParam,
            jParam,
            kParam,
            lParam,
            mParam,
            nParam,
            oParam
          ) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        import jParam.Underlying as J
        import kParam.Underlying as K
        import lParam.Underlying as L
        import mParam.Underlying as M
        import nParam.Underlying as N
        import oParam.Underlying as O
        Seq(
          "as fixed back" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint),
              Data(J.plainPrint),
              Data(K.plainPrint),
              Data(L.plainPrint),
              Data(M.plainPrint),
              Data(N.plainPrint),
              Data(O.plainPrint)
            ),
            "reapplied" -> Data(
              fixedBackTest(using
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String
              ).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val result =
      (classResult.view ++ renamedResult.view ++ extraResult.view ++ fixedFrontResult.view ++ fixedBackResult.view).toSeq
    if (result.isEmpty) Expr(Data("Not one of the expected types")) else Expr(Data.map(result*))
  }

  def testTypeCtor16[In: Type]: Expr[Data] = {
    // TODO: type projector test as well

    val classTest = Type.Ctor16.of[examples.kinds.Arity16]
    val classResult = Type[In] match {
      case classTest(
            aParam,
            bParam,
            cParam,
            dParam,
            eParam,
            fParam,
            gParam,
            hParam,
            iParam,
            jParam,
            kParam,
            lParam,
            mParam,
            nParam,
            oParam,
            pParam
          ) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        import jParam.Underlying as J
        import kParam.Underlying as K
        import lParam.Underlying as L
        import mParam.Underlying as M
        import nParam.Underlying as N
        import oParam.Underlying as O
        import pParam.Underlying as P
        Seq(
          "as class" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint),
              Data(J.plainPrint),
              Data(K.plainPrint),
              Data(L.plainPrint),
              Data(M.plainPrint),
              Data(N.plainPrint),
              Data(O.plainPrint),
              Data(P.plainPrint)
            ),
            "reapplied" -> Data(
              classTest(using
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String
              ).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val renamedTest = Type.Ctor16.of[examples.kinds.Alias.Renamed16]
    val renamedResult = Type[In] match {
      case renamedTest(
            aParam,
            bParam,
            cParam,
            dParam,
            eParam,
            fParam,
            gParam,
            hParam,
            iParam,
            jParam,
            kParam,
            lParam,
            mParam,
            nParam,
            oParam,
            pParam
          ) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        import jParam.Underlying as J
        import kParam.Underlying as K
        import lParam.Underlying as L
        import mParam.Underlying as M
        import nParam.Underlying as N
        import oParam.Underlying as O
        import pParam.Underlying as P
        Seq(
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint),
              Data(J.plainPrint),
              Data(K.plainPrint),
              Data(L.plainPrint),
              Data(M.plainPrint),
              Data(N.plainPrint),
              Data(O.plainPrint),
              Data(P.plainPrint)
            ),
            "reapplied" -> Data(
              renamedTest(using
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String
              ).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val extraTest = Type.Ctor16.of[examples.kinds.Alias.Extra16]
    val extraResult = Type[In] match {
      case extraTest(
            aParam,
            bParam,
            cParam,
            dParam,
            eParam,
            fParam,
            gParam,
            hParam,
            iParam,
            jParam,
            kParam,
            lParam,
            mParam,
            nParam,
            oParam,
            pParam
          ) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        import jParam.Underlying as J
        import kParam.Underlying as K
        import lParam.Underlying as L
        import mParam.Underlying as M
        import nParam.Underlying as N
        import oParam.Underlying as O
        import pParam.Underlying as P
        Seq(
          "as extra" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint),
              Data(J.plainPrint),
              Data(K.plainPrint),
              Data(L.plainPrint),
              Data(M.plainPrint),
              Data(N.plainPrint),
              Data(O.plainPrint),
              Data(P.plainPrint)
            ),
            "reapplied" -> Data(
              extraTest(using
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String
              ).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val fixedFrontTest = Type.Ctor16.of[examples.kinds.Alias.FixedFront16]
    val fixedFrontResult = Type[In] match {
      case fixedFrontTest(
            aParam,
            bParam,
            cParam,
            dParam,
            eParam,
            fParam,
            gParam,
            hParam,
            iParam,
            jParam,
            kParam,
            lParam,
            mParam,
            nParam,
            oParam,
            pParam
          ) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        import jParam.Underlying as J
        import kParam.Underlying as K
        import lParam.Underlying as L
        import mParam.Underlying as M
        import nParam.Underlying as N
        import oParam.Underlying as O
        import pParam.Underlying as P
        Seq(
          "as fixed front" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint),
              Data(J.plainPrint),
              Data(K.plainPrint),
              Data(L.plainPrint),
              Data(M.plainPrint),
              Data(N.plainPrint),
              Data(O.plainPrint),
              Data(P.plainPrint)
            ),
            "reapplied" -> Data(
              fixedFrontTest(using
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String
              ).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val fixedBackTest = Type.Ctor16.of[examples.kinds.Alias.FixedBack16]
    val fixedBackResult = Type[In] match {
      case fixedBackTest(
            aParam,
            bParam,
            cParam,
            dParam,
            eParam,
            fParam,
            gParam,
            hParam,
            iParam,
            jParam,
            kParam,
            lParam,
            mParam,
            nParam,
            oParam,
            pParam
          ) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        import jParam.Underlying as J
        import kParam.Underlying as K
        import lParam.Underlying as L
        import mParam.Underlying as M
        import nParam.Underlying as N
        import oParam.Underlying as O
        import pParam.Underlying as P
        Seq(
          "as fixed back" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint),
              Data(J.plainPrint),
              Data(K.plainPrint),
              Data(L.plainPrint),
              Data(M.plainPrint),
              Data(N.plainPrint),
              Data(O.plainPrint),
              Data(P.plainPrint)
            ),
            "reapplied" -> Data(
              fixedBackTest(using
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String
              ).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val result =
      (classResult.view ++ renamedResult.view ++ extraResult.view ++ fixedFrontResult.view ++ fixedBackResult.view).toSeq
    if (result.isEmpty) Expr(Data("Not one of the expected types")) else Expr(Data.map(result*))
  }

  def testTypeCtor17[In: Type]: Expr[Data] = {
    // TODO: type projector test as well

    val classTest = Type.Ctor17.of[examples.kinds.Arity17]
    val classResult = Type[In] match {
      case classTest(
            aParam,
            bParam,
            cParam,
            dParam,
            eParam,
            fParam,
            gParam,
            hParam,
            iParam,
            jParam,
            kParam,
            lParam,
            mParam,
            nParam,
            oParam,
            pParam,
            qParam
          ) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        import jParam.Underlying as J
        import kParam.Underlying as K
        import lParam.Underlying as L
        import mParam.Underlying as M
        import nParam.Underlying as N
        import oParam.Underlying as O
        import pParam.Underlying as P
        import qParam.Underlying as Q
        Seq(
          "as class" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint),
              Data(J.plainPrint),
              Data(K.plainPrint),
              Data(L.plainPrint),
              Data(M.plainPrint),
              Data(N.plainPrint),
              Data(O.plainPrint),
              Data(P.plainPrint),
              Data(Q.plainPrint)
            ),
            "reapplied" -> Data(
              classTest(using
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String
              ).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val renamedTest = Type.Ctor17.of[examples.kinds.Alias.Renamed17]
    val renamedResult = Type[In] match {
      case renamedTest(
            aParam,
            bParam,
            cParam,
            dParam,
            eParam,
            fParam,
            gParam,
            hParam,
            iParam,
            jParam,
            kParam,
            lParam,
            mParam,
            nParam,
            oParam,
            pParam,
            qParam
          ) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        import jParam.Underlying as J
        import kParam.Underlying as K
        import lParam.Underlying as L
        import mParam.Underlying as M
        import nParam.Underlying as N
        import oParam.Underlying as O
        import pParam.Underlying as P
        import qParam.Underlying as Q
        Seq(
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint),
              Data(J.plainPrint),
              Data(K.plainPrint),
              Data(L.plainPrint),
              Data(M.plainPrint),
              Data(N.plainPrint),
              Data(O.plainPrint),
              Data(P.plainPrint),
              Data(Q.plainPrint)
            ),
            "reapplied" -> Data(
              renamedTest(using
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String
              ).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val extraTest = Type.Ctor17.of[examples.kinds.Alias.Extra17]
    val extraResult = Type[In] match {
      case extraTest(
            aParam,
            bParam,
            cParam,
            dParam,
            eParam,
            fParam,
            gParam,
            hParam,
            iParam,
            jParam,
            kParam,
            lParam,
            mParam,
            nParam,
            oParam,
            pParam,
            qParam
          ) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        import jParam.Underlying as J
        import kParam.Underlying as K
        import lParam.Underlying as L
        import mParam.Underlying as M
        import nParam.Underlying as N
        import oParam.Underlying as O
        import pParam.Underlying as P
        import qParam.Underlying as Q
        Seq(
          "as extra" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint),
              Data(J.plainPrint),
              Data(K.plainPrint),
              Data(L.plainPrint),
              Data(M.plainPrint),
              Data(N.plainPrint),
              Data(O.plainPrint),
              Data(P.plainPrint),
              Data(Q.plainPrint)
            ),
            "reapplied" -> Data(
              extraTest(using
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String
              ).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val fixedFrontTest = Type.Ctor17.of[examples.kinds.Alias.FixedFront17]
    val fixedFrontResult = Type[In] match {
      case fixedFrontTest(
            aParam,
            bParam,
            cParam,
            dParam,
            eParam,
            fParam,
            gParam,
            hParam,
            iParam,
            jParam,
            kParam,
            lParam,
            mParam,
            nParam,
            oParam,
            pParam,
            qParam
          ) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        import jParam.Underlying as J
        import kParam.Underlying as K
        import lParam.Underlying as L
        import mParam.Underlying as M
        import nParam.Underlying as N
        import oParam.Underlying as O
        import pParam.Underlying as P
        import qParam.Underlying as Q
        Seq(
          "as fixed front" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint),
              Data(J.plainPrint),
              Data(K.plainPrint),
              Data(L.plainPrint),
              Data(M.plainPrint),
              Data(N.plainPrint),
              Data(O.plainPrint),
              Data(P.plainPrint),
              Data(Q.plainPrint)
            ),
            "reapplied" -> Data(
              fixedFrontTest(using
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String
              ).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val fixedBackTest = Type.Ctor17.of[examples.kinds.Alias.FixedBack17]
    val fixedBackResult = Type[In] match {
      case fixedBackTest(
            aParam,
            bParam,
            cParam,
            dParam,
            eParam,
            fParam,
            gParam,
            hParam,
            iParam,
            jParam,
            kParam,
            lParam,
            mParam,
            nParam,
            oParam,
            pParam,
            qParam
          ) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        import jParam.Underlying as J
        import kParam.Underlying as K
        import lParam.Underlying as L
        import mParam.Underlying as M
        import nParam.Underlying as N
        import oParam.Underlying as O
        import pParam.Underlying as P
        import qParam.Underlying as Q
        Seq(
          "as fixed back" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint),
              Data(J.plainPrint),
              Data(K.plainPrint),
              Data(L.plainPrint),
              Data(M.plainPrint),
              Data(N.plainPrint),
              Data(O.plainPrint),
              Data(P.plainPrint),
              Data(Q.plainPrint)
            ),
            "reapplied" -> Data(
              fixedBackTest(using
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String
              ).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val result =
      (classResult.view ++ renamedResult.view ++ extraResult.view ++ fixedFrontResult.view ++ fixedBackResult.view).toSeq
    if (result.isEmpty) Expr(Data("Not one of the expected types")) else Expr(Data.map(result*))
  }

  def testTypeCtor18[In: Type]: Expr[Data] = {
    // TODO: type projector test as well

    val classTest = Type.Ctor18.of[examples.kinds.Arity18]
    val classResult = Type[In] match {
      case classTest(
            aParam,
            bParam,
            cParam,
            dParam,
            eParam,
            fParam,
            gParam,
            hParam,
            iParam,
            jParam,
            kParam,
            lParam,
            mParam,
            nParam,
            oParam,
            pParam,
            qParam,
            rParam
          ) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        import jParam.Underlying as J
        import kParam.Underlying as K
        import lParam.Underlying as L
        import mParam.Underlying as M
        import nParam.Underlying as N
        import oParam.Underlying as O
        import pParam.Underlying as P
        import qParam.Underlying as Q
        import rParam.Underlying as R
        Seq(
          "as class" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint),
              Data(J.plainPrint),
              Data(K.plainPrint),
              Data(L.plainPrint),
              Data(M.plainPrint),
              Data(N.plainPrint),
              Data(O.plainPrint),
              Data(P.plainPrint),
              Data(Q.plainPrint),
              Data(R.plainPrint)
            ),
            "reapplied" -> Data(
              classTest(using
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String
              ).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val renamedTest = Type.Ctor18.of[examples.kinds.Alias.Renamed18]
    val renamedResult = Type[In] match {
      case renamedTest(
            aParam,
            bParam,
            cParam,
            dParam,
            eParam,
            fParam,
            gParam,
            hParam,
            iParam,
            jParam,
            kParam,
            lParam,
            mParam,
            nParam,
            oParam,
            pParam,
            qParam,
            rParam
          ) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        import jParam.Underlying as J
        import kParam.Underlying as K
        import lParam.Underlying as L
        import mParam.Underlying as M
        import nParam.Underlying as N
        import oParam.Underlying as O
        import pParam.Underlying as P
        import qParam.Underlying as Q
        import rParam.Underlying as R
        Seq(
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint),
              Data(J.plainPrint),
              Data(K.plainPrint),
              Data(L.plainPrint),
              Data(M.plainPrint),
              Data(N.plainPrint),
              Data(O.plainPrint),
              Data(P.plainPrint),
              Data(Q.plainPrint),
              Data(R.plainPrint)
            ),
            "reapplied" -> Data(
              renamedTest(using
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String
              ).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val extraTest = Type.Ctor18.of[examples.kinds.Alias.Extra18]
    val extraResult = Type[In] match {
      case extraTest(
            aParam,
            bParam,
            cParam,
            dParam,
            eParam,
            fParam,
            gParam,
            hParam,
            iParam,
            jParam,
            kParam,
            lParam,
            mParam,
            nParam,
            oParam,
            pParam,
            qParam,
            rParam
          ) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        import jParam.Underlying as J
        import kParam.Underlying as K
        import lParam.Underlying as L
        import mParam.Underlying as M
        import nParam.Underlying as N
        import oParam.Underlying as O
        import pParam.Underlying as P
        import qParam.Underlying as Q
        import rParam.Underlying as R
        Seq(
          "as extra" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint),
              Data(J.plainPrint),
              Data(K.plainPrint),
              Data(L.plainPrint),
              Data(M.plainPrint),
              Data(N.plainPrint),
              Data(O.plainPrint),
              Data(P.plainPrint),
              Data(Q.plainPrint),
              Data(R.plainPrint)
            ),
            "reapplied" -> Data(
              extraTest(using
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String
              ).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val fixedFrontTest = Type.Ctor18.of[examples.kinds.Alias.FixedFront18]
    val fixedFrontResult = Type[In] match {
      case fixedFrontTest(
            aParam,
            bParam,
            cParam,
            dParam,
            eParam,
            fParam,
            gParam,
            hParam,
            iParam,
            jParam,
            kParam,
            lParam,
            mParam,
            nParam,
            oParam,
            pParam,
            qParam,
            rParam
          ) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        import jParam.Underlying as J
        import kParam.Underlying as K
        import lParam.Underlying as L
        import mParam.Underlying as M
        import nParam.Underlying as N
        import oParam.Underlying as O
        import pParam.Underlying as P
        import qParam.Underlying as Q
        import rParam.Underlying as R
        Seq(
          "as fixed front" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint),
              Data(J.plainPrint),
              Data(K.plainPrint),
              Data(L.plainPrint),
              Data(M.plainPrint),
              Data(N.plainPrint),
              Data(O.plainPrint),
              Data(P.plainPrint),
              Data(Q.plainPrint),
              Data(R.plainPrint)
            ),
            "reapplied" -> Data(
              fixedFrontTest(using
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String
              ).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val fixedBackTest = Type.Ctor18.of[examples.kinds.Alias.FixedBack18]
    val fixedBackResult = Type[In] match {
      case fixedBackTest(
            aParam,
            bParam,
            cParam,
            dParam,
            eParam,
            fParam,
            gParam,
            hParam,
            iParam,
            jParam,
            kParam,
            lParam,
            mParam,
            nParam,
            oParam,
            pParam,
            qParam,
            rParam
          ) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        import jParam.Underlying as J
        import kParam.Underlying as K
        import lParam.Underlying as L
        import mParam.Underlying as M
        import nParam.Underlying as N
        import oParam.Underlying as O
        import pParam.Underlying as P
        import qParam.Underlying as Q
        import rParam.Underlying as R
        Seq(
          "as fixed back" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint),
              Data(J.plainPrint),
              Data(K.plainPrint),
              Data(L.plainPrint),
              Data(M.plainPrint),
              Data(N.plainPrint),
              Data(O.plainPrint),
              Data(P.plainPrint),
              Data(Q.plainPrint),
              Data(R.plainPrint)
            ),
            "reapplied" -> Data(
              fixedBackTest(using
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String
              ).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val result =
      (classResult.view ++ renamedResult.view ++ extraResult.view ++ fixedFrontResult.view ++ fixedBackResult.view).toSeq
    if (result.isEmpty) Expr(Data("Not one of the expected types")) else Expr(Data.map(result*))
  }

  def testTypeCtor19[In: Type]: Expr[Data] = {
    // TODO: type projector test as well

    val classTest = Type.Ctor19.of[examples.kinds.Arity19]
    val classResult = Type[In] match {
      case classTest(
            aParam,
            bParam,
            cParam,
            dParam,
            eParam,
            fParam,
            gParam,
            hParam,
            iParam,
            jParam,
            kParam,
            lParam,
            mParam,
            nParam,
            oParam,
            pParam,
            qParam,
            rParam,
            sParam
          ) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        import jParam.Underlying as J
        import kParam.Underlying as K
        import lParam.Underlying as L
        import mParam.Underlying as M
        import nParam.Underlying as N
        import oParam.Underlying as O
        import pParam.Underlying as P
        import qParam.Underlying as Q
        import rParam.Underlying as R
        import sParam.Underlying as S
        Seq(
          "as class" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint),
              Data(J.plainPrint),
              Data(K.plainPrint),
              Data(L.plainPrint),
              Data(M.plainPrint),
              Data(N.plainPrint),
              Data(O.plainPrint),
              Data(P.plainPrint),
              Data(Q.plainPrint),
              Data(R.plainPrint),
              Data(S.plainPrint)
            ),
            "reapplied" -> Data(
              classTest(using
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String
              ).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val renamedTest = Type.Ctor19.of[examples.kinds.Alias.Renamed19]
    val renamedResult = Type[In] match {
      case renamedTest(
            aParam,
            bParam,
            cParam,
            dParam,
            eParam,
            fParam,
            gParam,
            hParam,
            iParam,
            jParam,
            kParam,
            lParam,
            mParam,
            nParam,
            oParam,
            pParam,
            qParam,
            rParam,
            sParam
          ) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        import jParam.Underlying as J
        import kParam.Underlying as K
        import lParam.Underlying as L
        import mParam.Underlying as M
        import nParam.Underlying as N
        import oParam.Underlying as O
        import pParam.Underlying as P
        import qParam.Underlying as Q
        import rParam.Underlying as R
        import sParam.Underlying as S
        Seq(
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint),
              Data(J.plainPrint),
              Data(K.plainPrint),
              Data(L.plainPrint),
              Data(M.plainPrint),
              Data(N.plainPrint),
              Data(O.plainPrint),
              Data(P.plainPrint),
              Data(Q.plainPrint),
              Data(R.plainPrint),
              Data(S.plainPrint)
            ),
            "reapplied" -> Data(
              renamedTest(using
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String
              ).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val extraTest = Type.Ctor19.of[examples.kinds.Alias.Extra19]
    val extraResult = Type[In] match {
      case extraTest(
            aParam,
            bParam,
            cParam,
            dParam,
            eParam,
            fParam,
            gParam,
            hParam,
            iParam,
            jParam,
            kParam,
            lParam,
            mParam,
            nParam,
            oParam,
            pParam,
            qParam,
            rParam,
            sParam
          ) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        import jParam.Underlying as J
        import kParam.Underlying as K
        import lParam.Underlying as L
        import mParam.Underlying as M
        import nParam.Underlying as N
        import oParam.Underlying as O
        import pParam.Underlying as P
        import qParam.Underlying as Q
        import rParam.Underlying as R
        import sParam.Underlying as S
        Seq(
          "as extra" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint),
              Data(J.plainPrint),
              Data(K.plainPrint),
              Data(L.plainPrint),
              Data(M.plainPrint),
              Data(N.plainPrint),
              Data(O.plainPrint),
              Data(P.plainPrint),
              Data(Q.plainPrint),
              Data(R.plainPrint),
              Data(S.plainPrint)
            ),
            "reapplied" -> Data(
              extraTest(using
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String
              ).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val fixedFrontTest = Type.Ctor19.of[examples.kinds.Alias.FixedFront19]
    val fixedFrontResult = Type[In] match {
      case fixedFrontTest(
            aParam,
            bParam,
            cParam,
            dParam,
            eParam,
            fParam,
            gParam,
            hParam,
            iParam,
            jParam,
            kParam,
            lParam,
            mParam,
            nParam,
            oParam,
            pParam,
            qParam,
            rParam,
            sParam
          ) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        import jParam.Underlying as J
        import kParam.Underlying as K
        import lParam.Underlying as L
        import mParam.Underlying as M
        import nParam.Underlying as N
        import oParam.Underlying as O
        import pParam.Underlying as P
        import qParam.Underlying as Q
        import rParam.Underlying as R
        import sParam.Underlying as S
        Seq(
          "as fixed front" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint),
              Data(J.plainPrint),
              Data(K.plainPrint),
              Data(L.plainPrint),
              Data(M.plainPrint),
              Data(N.plainPrint),
              Data(O.plainPrint),
              Data(P.plainPrint),
              Data(Q.plainPrint),
              Data(R.plainPrint),
              Data(S.plainPrint)
            ),
            "reapplied" -> Data(
              fixedFrontTest(using
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String
              ).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val fixedBackTest = Type.Ctor19.of[examples.kinds.Alias.FixedBack19]
    val fixedBackResult = Type[In] match {
      case fixedBackTest(
            aParam,
            bParam,
            cParam,
            dParam,
            eParam,
            fParam,
            gParam,
            hParam,
            iParam,
            jParam,
            kParam,
            lParam,
            mParam,
            nParam,
            oParam,
            pParam,
            qParam,
            rParam,
            sParam
          ) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        import jParam.Underlying as J
        import kParam.Underlying as K
        import lParam.Underlying as L
        import mParam.Underlying as M
        import nParam.Underlying as N
        import oParam.Underlying as O
        import pParam.Underlying as P
        import qParam.Underlying as Q
        import rParam.Underlying as R
        import sParam.Underlying as S
        Seq(
          "as fixed back" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint),
              Data(J.plainPrint),
              Data(K.plainPrint),
              Data(L.plainPrint),
              Data(M.plainPrint),
              Data(N.plainPrint),
              Data(O.plainPrint),
              Data(P.plainPrint),
              Data(Q.plainPrint),
              Data(R.plainPrint),
              Data(S.plainPrint)
            ),
            "reapplied" -> Data(
              fixedBackTest(using
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String
              ).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val result =
      (classResult.view ++ renamedResult.view ++ extraResult.view ++ fixedFrontResult.view ++ fixedBackResult.view).toSeq
    if (result.isEmpty) Expr(Data("Not one of the expected types")) else Expr(Data.map(result*))
  }

  def testTypeCtor20[In: Type]: Expr[Data] = {
    // TODO: type projector test as well

    val classTest = Type.Ctor20.of[examples.kinds.Arity20]
    val classResult = Type[In] match {
      case classTest(
            aParam,
            bParam,
            cParam,
            dParam,
            eParam,
            fParam,
            gParam,
            hParam,
            iParam,
            jParam,
            kParam,
            lParam,
            mParam,
            nParam,
            oParam,
            pParam,
            qParam,
            rParam,
            sParam,
            tParam
          ) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        import jParam.Underlying as J
        import kParam.Underlying as K
        import lParam.Underlying as L
        import mParam.Underlying as M
        import nParam.Underlying as N
        import oParam.Underlying as O
        import pParam.Underlying as P
        import qParam.Underlying as Q
        import rParam.Underlying as R
        import sParam.Underlying as S
        import tParam.Underlying as T
        Seq(
          "as class" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint),
              Data(J.plainPrint),
              Data(K.plainPrint),
              Data(L.plainPrint),
              Data(M.plainPrint),
              Data(N.plainPrint),
              Data(O.plainPrint),
              Data(P.plainPrint),
              Data(Q.plainPrint),
              Data(R.plainPrint),
              Data(S.plainPrint),
              Data(T.plainPrint)
            ),
            "reapplied" -> Data(
              classTest(using
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String
              ).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val renamedTest = Type.Ctor20.of[examples.kinds.Alias.Renamed20]
    val renamedResult = Type[In] match {
      case renamedTest(
            aParam,
            bParam,
            cParam,
            dParam,
            eParam,
            fParam,
            gParam,
            hParam,
            iParam,
            jParam,
            kParam,
            lParam,
            mParam,
            nParam,
            oParam,
            pParam,
            qParam,
            rParam,
            sParam,
            tParam
          ) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        import jParam.Underlying as J
        import kParam.Underlying as K
        import lParam.Underlying as L
        import mParam.Underlying as M
        import nParam.Underlying as N
        import oParam.Underlying as O
        import pParam.Underlying as P
        import qParam.Underlying as Q
        import rParam.Underlying as R
        import sParam.Underlying as S
        import tParam.Underlying as T
        Seq(
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint),
              Data(J.plainPrint),
              Data(K.plainPrint),
              Data(L.plainPrint),
              Data(M.plainPrint),
              Data(N.plainPrint),
              Data(O.plainPrint),
              Data(P.plainPrint),
              Data(Q.plainPrint),
              Data(R.plainPrint),
              Data(S.plainPrint),
              Data(T.plainPrint)
            ),
            "reapplied" -> Data(
              renamedTest(using
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String
              ).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val extraTest = Type.Ctor20.of[examples.kinds.Alias.Extra20]
    val extraResult = Type[In] match {
      case extraTest(
            aParam,
            bParam,
            cParam,
            dParam,
            eParam,
            fParam,
            gParam,
            hParam,
            iParam,
            jParam,
            kParam,
            lParam,
            mParam,
            nParam,
            oParam,
            pParam,
            qParam,
            rParam,
            sParam,
            tParam
          ) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        import jParam.Underlying as J
        import kParam.Underlying as K
        import lParam.Underlying as L
        import mParam.Underlying as M
        import nParam.Underlying as N
        import oParam.Underlying as O
        import pParam.Underlying as P
        import qParam.Underlying as Q
        import rParam.Underlying as R
        import sParam.Underlying as S
        import tParam.Underlying as T
        Seq(
          "as extra" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint),
              Data(J.plainPrint),
              Data(K.plainPrint),
              Data(L.plainPrint),
              Data(M.plainPrint),
              Data(N.plainPrint),
              Data(O.plainPrint),
              Data(P.plainPrint),
              Data(Q.plainPrint),
              Data(R.plainPrint),
              Data(S.plainPrint),
              Data(T.plainPrint)
            ),
            "reapplied" -> Data(
              extraTest(using
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String
              ).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val fixedFrontTest = Type.Ctor20.of[examples.kinds.Alias.FixedFront20]
    val fixedFrontResult = Type[In] match {
      case fixedFrontTest(
            aParam,
            bParam,
            cParam,
            dParam,
            eParam,
            fParam,
            gParam,
            hParam,
            iParam,
            jParam,
            kParam,
            lParam,
            mParam,
            nParam,
            oParam,
            pParam,
            qParam,
            rParam,
            sParam,
            tParam
          ) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        import jParam.Underlying as J
        import kParam.Underlying as K
        import lParam.Underlying as L
        import mParam.Underlying as M
        import nParam.Underlying as N
        import oParam.Underlying as O
        import pParam.Underlying as P
        import qParam.Underlying as Q
        import rParam.Underlying as R
        import sParam.Underlying as S
        import tParam.Underlying as T
        Seq(
          "as fixed front" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint),
              Data(J.plainPrint),
              Data(K.plainPrint),
              Data(L.plainPrint),
              Data(M.plainPrint),
              Data(N.plainPrint),
              Data(O.plainPrint),
              Data(P.plainPrint),
              Data(Q.plainPrint),
              Data(R.plainPrint),
              Data(S.plainPrint),
              Data(T.plainPrint)
            ),
            "reapplied" -> Data(
              fixedFrontTest(using
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String
              ).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val fixedBackTest = Type.Ctor20.of[examples.kinds.Alias.FixedBack20]
    val fixedBackResult = Type[In] match {
      case fixedBackTest(
            aParam,
            bParam,
            cParam,
            dParam,
            eParam,
            fParam,
            gParam,
            hParam,
            iParam,
            jParam,
            kParam,
            lParam,
            mParam,
            nParam,
            oParam,
            pParam,
            qParam,
            rParam,
            sParam,
            tParam
          ) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        import jParam.Underlying as J
        import kParam.Underlying as K
        import lParam.Underlying as L
        import mParam.Underlying as M
        import nParam.Underlying as N
        import oParam.Underlying as O
        import pParam.Underlying as P
        import qParam.Underlying as Q
        import rParam.Underlying as R
        import sParam.Underlying as S
        import tParam.Underlying as T
        Seq(
          "as fixed back" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint),
              Data(J.plainPrint),
              Data(K.plainPrint),
              Data(L.plainPrint),
              Data(M.plainPrint),
              Data(N.plainPrint),
              Data(O.plainPrint),
              Data(P.plainPrint),
              Data(Q.plainPrint),
              Data(R.plainPrint),
              Data(S.plainPrint),
              Data(T.plainPrint)
            ),
            "reapplied" -> Data(
              fixedBackTest(using
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String
              ).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val result =
      (classResult.view ++ renamedResult.view ++ extraResult.view ++ fixedFrontResult.view ++ fixedBackResult.view).toSeq
    if (result.isEmpty) Expr(Data("Not one of the expected types")) else Expr(Data.map(result*))
  }

  def testTypeCtor21[In: Type]: Expr[Data] = {
    // TODO: type projector test as well

    val classTest = Type.Ctor21.of[examples.kinds.Arity21]
    val classResult = Type[In] match {
      case classTest(
            aParam,
            bParam,
            cParam,
            dParam,
            eParam,
            fParam,
            gParam,
            hParam,
            iParam,
            jParam,
            kParam,
            lParam,
            mParam,
            nParam,
            oParam,
            pParam,
            qParam,
            rParam,
            sParam,
            tParam,
            uParam
          ) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        import jParam.Underlying as J
        import kParam.Underlying as K
        import lParam.Underlying as L
        import mParam.Underlying as M
        import nParam.Underlying as N
        import oParam.Underlying as O
        import pParam.Underlying as P
        import qParam.Underlying as Q
        import rParam.Underlying as R
        import sParam.Underlying as S
        import tParam.Underlying as T
        import uParam.Underlying as U
        Seq(
          "as class" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint),
              Data(J.plainPrint),
              Data(K.plainPrint),
              Data(L.plainPrint),
              Data(M.plainPrint),
              Data(N.plainPrint),
              Data(O.plainPrint),
              Data(P.plainPrint),
              Data(Q.plainPrint),
              Data(R.plainPrint),
              Data(S.plainPrint),
              Data(T.plainPrint),
              Data(U.plainPrint)
            ),
            "reapplied" -> Data(
              classTest(
                using
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String
              ).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val renamedTest = Type.Ctor21.of[examples.kinds.Alias.Renamed21]
    val renamedResult = Type[In] match {
      case renamedTest(
            aParam,
            bParam,
            cParam,
            dParam,
            eParam,
            fParam,
            gParam,
            hParam,
            iParam,
            jParam,
            kParam,
            lParam,
            mParam,
            nParam,
            oParam,
            pParam,
            qParam,
            rParam,
            sParam,
            tParam,
            uParam
          ) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        import jParam.Underlying as J
        import kParam.Underlying as K
        import lParam.Underlying as L
        import mParam.Underlying as M
        import nParam.Underlying as N
        import oParam.Underlying as O
        import pParam.Underlying as P
        import qParam.Underlying as Q
        import rParam.Underlying as R
        import sParam.Underlying as S
        import tParam.Underlying as T
        import uParam.Underlying as U
        Seq(
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint),
              Data(J.plainPrint),
              Data(K.plainPrint),
              Data(L.plainPrint),
              Data(M.plainPrint),
              Data(N.plainPrint),
              Data(O.plainPrint),
              Data(P.plainPrint),
              Data(Q.plainPrint),
              Data(R.plainPrint),
              Data(S.plainPrint),
              Data(T.plainPrint),
              Data(U.plainPrint)
            ),
            "reapplied" -> Data(
              renamedTest(
                using
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String
              ).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val extraTest = Type.Ctor21.of[examples.kinds.Alias.Extra21]
    val extraResult = Type[In] match {
      case extraTest(
            aParam,
            bParam,
            cParam,
            dParam,
            eParam,
            fParam,
            gParam,
            hParam,
            iParam,
            jParam,
            kParam,
            lParam,
            mParam,
            nParam,
            oParam,
            pParam,
            qParam,
            rParam,
            sParam,
            tParam,
            uParam
          ) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        import jParam.Underlying as J
        import kParam.Underlying as K
        import lParam.Underlying as L
        import mParam.Underlying as M
        import nParam.Underlying as N
        import oParam.Underlying as O
        import pParam.Underlying as P
        import qParam.Underlying as Q
        import rParam.Underlying as R
        import sParam.Underlying as S
        import tParam.Underlying as T
        import uParam.Underlying as U
        Seq(
          "as extra" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint),
              Data(J.plainPrint),
              Data(K.plainPrint),
              Data(L.plainPrint),
              Data(M.plainPrint),
              Data(N.plainPrint),
              Data(O.plainPrint),
              Data(P.plainPrint),
              Data(Q.plainPrint),
              Data(R.plainPrint),
              Data(S.plainPrint),
              Data(T.plainPrint),
              Data(U.plainPrint)
            ),
            "reapplied" -> Data(
              extraTest(
                using
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String
              ).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val fixedFrontTest = Type.Ctor21.of[examples.kinds.Alias.FixedFront21]
    val fixedFrontResult = Type[In] match {
      case fixedFrontTest(
            aParam,
            bParam,
            cParam,
            dParam,
            eParam,
            fParam,
            gParam,
            hParam,
            iParam,
            jParam,
            kParam,
            lParam,
            mParam,
            nParam,
            oParam,
            pParam,
            qParam,
            rParam,
            sParam,
            tParam,
            uParam
          ) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        import jParam.Underlying as J
        import kParam.Underlying as K
        import lParam.Underlying as L
        import mParam.Underlying as M
        import nParam.Underlying as N
        import oParam.Underlying as O
        import pParam.Underlying as P
        import qParam.Underlying as Q
        import rParam.Underlying as R
        import sParam.Underlying as S
        import tParam.Underlying as T
        import uParam.Underlying as U
        Seq(
          "as fixed front" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint),
              Data(J.plainPrint),
              Data(K.plainPrint),
              Data(L.plainPrint),
              Data(M.plainPrint),
              Data(N.plainPrint),
              Data(O.plainPrint),
              Data(P.plainPrint),
              Data(Q.plainPrint),
              Data(R.plainPrint),
              Data(S.plainPrint),
              Data(T.plainPrint),
              Data(U.plainPrint)
            ),
            "reapplied" -> Data(
              fixedFrontTest(
                using
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String
              ).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val fixedBackTest = Type.Ctor21.of[examples.kinds.Alias.FixedBack21]
    val fixedBackResult = Type[In] match {
      case fixedBackTest(
            aParam,
            bParam,
            cParam,
            dParam,
            eParam,
            fParam,
            gParam,
            hParam,
            iParam,
            jParam,
            kParam,
            lParam,
            mParam,
            nParam,
            oParam,
            pParam,
            qParam,
            rParam,
            sParam,
            tParam,
            uParam
          ) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        import jParam.Underlying as J
        import kParam.Underlying as K
        import lParam.Underlying as L
        import mParam.Underlying as M
        import nParam.Underlying as N
        import oParam.Underlying as O
        import pParam.Underlying as P
        import qParam.Underlying as Q
        import rParam.Underlying as R
        import sParam.Underlying as S
        import tParam.Underlying as T
        import uParam.Underlying as U
        Seq(
          "as fixed back" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint),
              Data(J.plainPrint),
              Data(K.plainPrint),
              Data(L.plainPrint),
              Data(M.plainPrint),
              Data(N.plainPrint),
              Data(O.plainPrint),
              Data(P.plainPrint),
              Data(Q.plainPrint),
              Data(R.plainPrint),
              Data(S.plainPrint),
              Data(T.plainPrint),
              Data(U.plainPrint)
            ),
            "reapplied" -> Data(
              fixedBackTest(
                using
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String
              ).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val result =
      (classResult.view ++ renamedResult.view ++ extraResult.view ++ fixedFrontResult.view ++ fixedBackResult.view).toSeq
    if (result.isEmpty) Expr(Data("Not one of the expected types")) else Expr(Data.map(result*))
  }

  def testTypeCtor22[In: Type]: Expr[Data] = {
    // TODO: type projector test as well

    val classTest = Type.Ctor22.of[examples.kinds.Arity22]
    val classResult = Type[In] match {
      case classTest(
            aParam,
            bParam,
            cParam,
            dParam,
            eParam,
            fParam,
            gParam,
            hParam,
            iParam,
            jParam,
            kParam,
            lParam,
            mParam,
            nParam,
            oParam,
            pParam,
            qParam,
            rParam,
            sParam,
            tParam,
            uParam,
            vParam
          ) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        import jParam.Underlying as J
        import kParam.Underlying as K
        import lParam.Underlying as L
        import mParam.Underlying as M
        import nParam.Underlying as N
        import oParam.Underlying as O
        import pParam.Underlying as P
        import qParam.Underlying as Q
        import rParam.Underlying as R
        import sParam.Underlying as S
        import tParam.Underlying as T
        import uParam.Underlying as U
        import vParam.Underlying as V
        Seq(
          "as class" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint),
              Data(J.plainPrint),
              Data(K.plainPrint),
              Data(L.plainPrint),
              Data(M.plainPrint),
              Data(N.plainPrint),
              Data(O.plainPrint),
              Data(P.plainPrint),
              Data(Q.plainPrint),
              Data(R.plainPrint),
              Data(S.plainPrint),
              Data(T.plainPrint),
              Data(U.plainPrint),
              Data(V.plainPrint)
            ),
            "reapplied" -> Data(
              classTest(
                using
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String
              ).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val renamedTest = Type.Ctor22.of[examples.kinds.Alias.Renamed22]
    val renamedResult = Type[In] match {
      case renamedTest(
            aParam,
            bParam,
            cParam,
            dParam,
            eParam,
            fParam,
            gParam,
            hParam,
            iParam,
            jParam,
            kParam,
            lParam,
            mParam,
            nParam,
            oParam,
            pParam,
            qParam,
            rParam,
            sParam,
            tParam,
            uParam,
            vParam
          ) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        import jParam.Underlying as J
        import kParam.Underlying as K
        import lParam.Underlying as L
        import mParam.Underlying as M
        import nParam.Underlying as N
        import oParam.Underlying as O
        import pParam.Underlying as P
        import qParam.Underlying as Q
        import rParam.Underlying as R
        import sParam.Underlying as S
        import tParam.Underlying as T
        import uParam.Underlying as U
        import vParam.Underlying as V
        Seq(
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint),
              Data(J.plainPrint),
              Data(K.plainPrint),
              Data(L.plainPrint),
              Data(M.plainPrint),
              Data(N.plainPrint),
              Data(O.plainPrint),
              Data(P.plainPrint),
              Data(Q.plainPrint),
              Data(R.plainPrint),
              Data(S.plainPrint),
              Data(T.plainPrint),
              Data(U.plainPrint),
              Data(V.plainPrint)
            ),
            "reapplied" -> Data(
              renamedTest(
                using
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String
              ).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val extraTest = Type.Ctor22.of[examples.kinds.Alias.Extra22]
    val extraResult = Type[In] match {
      case extraTest(
            aParam,
            bParam,
            cParam,
            dParam,
            eParam,
            fParam,
            gParam,
            hParam,
            iParam,
            jParam,
            kParam,
            lParam,
            mParam,
            nParam,
            oParam,
            pParam,
            qParam,
            rParam,
            sParam,
            tParam,
            uParam,
            vParam
          ) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        import jParam.Underlying as J
        import kParam.Underlying as K
        import lParam.Underlying as L
        import mParam.Underlying as M
        import nParam.Underlying as N
        import oParam.Underlying as O
        import pParam.Underlying as P
        import qParam.Underlying as Q
        import rParam.Underlying as R
        import sParam.Underlying as S
        import tParam.Underlying as T
        import uParam.Underlying as U
        import vParam.Underlying as V
        Seq(
          "as extra" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint),
              Data(J.plainPrint),
              Data(K.plainPrint),
              Data(L.plainPrint),
              Data(M.plainPrint),
              Data(N.plainPrint),
              Data(O.plainPrint),
              Data(P.plainPrint),
              Data(Q.plainPrint),
              Data(R.plainPrint),
              Data(S.plainPrint),
              Data(T.plainPrint),
              Data(U.plainPrint),
              Data(V.plainPrint)
            ),
            "reapplied" -> Data(
              extraTest(
                using
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String
              ).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val fixedFrontTest = Type.Ctor22.of[examples.kinds.Alias.FixedFront22]
    val fixedFrontResult = Type[In] match {
      case fixedFrontTest(
            aParam,
            bParam,
            cParam,
            dParam,
            eParam,
            fParam,
            gParam,
            hParam,
            iParam,
            jParam,
            kParam,
            lParam,
            mParam,
            nParam,
            oParam,
            pParam,
            qParam,
            rParam,
            sParam,
            tParam,
            uParam,
            vParam
          ) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        import jParam.Underlying as J
        import kParam.Underlying as K
        import lParam.Underlying as L
        import mParam.Underlying as M
        import nParam.Underlying as N
        import oParam.Underlying as O
        import pParam.Underlying as P
        import qParam.Underlying as Q
        import rParam.Underlying as R
        import sParam.Underlying as S
        import tParam.Underlying as T
        import uParam.Underlying as U
        import vParam.Underlying as V
        Seq(
          "as fixed front" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint),
              Data(J.plainPrint),
              Data(K.plainPrint),
              Data(L.plainPrint),
              Data(M.plainPrint),
              Data(N.plainPrint),
              Data(O.plainPrint),
              Data(P.plainPrint),
              Data(Q.plainPrint),
              Data(R.plainPrint),
              Data(S.plainPrint),
              Data(T.plainPrint),
              Data(U.plainPrint),
              Data(V.plainPrint)
            ),
            "reapplied" -> Data(
              fixedFrontTest(
                using
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String
              ).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val fixedBackTest = Type.Ctor22.of[examples.kinds.Alias.FixedBack22]
    val fixedBackResult = Type[In] match {
      case fixedBackTest(
            aParam,
            bParam,
            cParam,
            dParam,
            eParam,
            fParam,
            gParam,
            hParam,
            iParam,
            jParam,
            kParam,
            lParam,
            mParam,
            nParam,
            oParam,
            pParam,
            qParam,
            rParam,
            sParam,
            tParam,
            uParam,
            vParam
          ) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        import jParam.Underlying as J
        import kParam.Underlying as K
        import lParam.Underlying as L
        import mParam.Underlying as M
        import nParam.Underlying as N
        import oParam.Underlying as O
        import pParam.Underlying as P
        import qParam.Underlying as Q
        import rParam.Underlying as R
        import sParam.Underlying as S
        import tParam.Underlying as T
        import uParam.Underlying as U
        import vParam.Underlying as V
        Seq(
          "as fixed back" -> Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint),
              Data(H.plainPrint),
              Data(I.plainPrint),
              Data(J.plainPrint),
              Data(K.plainPrint),
              Data(L.plainPrint),
              Data(M.plainPrint),
              Data(N.plainPrint),
              Data(O.plainPrint),
              Data(P.plainPrint),
              Data(Q.plainPrint),
              Data(R.plainPrint),
              Data(S.plainPrint),
              Data(T.plainPrint),
              Data(U.plainPrint),
              Data(V.plainPrint)
            ),
            "reapplied" -> Data(
              fixedBackTest(
                using
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String,
                String
              ).plainPrint
            )
          )
        )
      case _ => Seq.empty
    }

    val result =
      (classResult.view ++ renamedResult.view ++ extraResult.view ++ fixedFrontResult.view ++ fixedBackResult.view).toSeq
    if (result.isEmpty) Expr(Data("Not one of the expected types")) else Expr(Data.map(result*))
  }
}
