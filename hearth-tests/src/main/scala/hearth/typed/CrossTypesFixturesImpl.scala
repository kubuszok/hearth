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

  private val optionTest = Type.Ctor1.of[Option]

  def testTypeCtor1[In: Type]: Expr[Data] =
    Type[In] match {
      case optionTest(aParam) =>
        import aParam.Underlying as A
        val optString = optionTest(using String)
        Expr(Data.map("unapplied" -> Data.list(Data(A.plainPrint)), "reapplied" -> Data(optString.plainPrint)))
      case _ => Expr(Data("Not an option"))
    }

  private val eitherTest = Type.Ctor2.of[Either]

  def testTypeCtor2[In: Type]: Expr[Data] =
    Type[In] match {
      case eitherTest(aParam, bParam) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        val optString = eitherTest(using String, String)
        Expr(
          Data.map(
            "unapplied" -> Data.list(Data(A.plainPrint), Data(B.plainPrint)),
            "reapplied" -> Data(optString.plainPrint)
          )
        )
      case _ => Expr(Data("Not an either"))
    }

  private val tuple3Test = Type.Ctor3.of[Tuple3]

  def testTypeCtor3[In: Type]: Expr[Data] =
    Type[In] match {
      case tuple3Test(aParam, bParam, cParam) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        val optString = tuple3Test(using String, String, String)
        Expr(
          Data.map(
            "unapplied" -> Data.list(Data(A.plainPrint), Data(B.plainPrint), Data(C.plainPrint)),
            "reapplied" -> Data(optString.plainPrint)
          )
        )
      case _ => Expr(Data("Not a tuple3"))
    }

  private val tuple4Test = Type.Ctor4.of[Tuple4]

  def testTypeCtor4[In: Type]: Expr[Data] =
    Type[In] match {
      case tuple4Test(aParam, bParam, cParam, dParam) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        val optString = tuple4Test(using String, String, String, String)
        Expr(
          Data.map(
            "unapplied" -> Data.list(Data(A.plainPrint), Data(B.plainPrint), Data(C.plainPrint), Data(D.plainPrint)),
            "reapplied" -> Data(optString.plainPrint)
          )
        )
      case _ => Expr(Data("Not a tuple4"))
    }

  private val tuple5Test = Type.Ctor5.of[Tuple5]

  def testTypeCtor5[In: Type]: Expr[Data] =
    Type[In] match {
      case tuple5Test(aParam, bParam, cParam, dParam, eParam) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        val optString = tuple5Test(using String, String, String, String, String)
        Expr(
          Data.map(
            "unapplied" -> Data
              .list(Data(A.plainPrint), Data(B.plainPrint), Data(C.plainPrint), Data(D.plainPrint), Data(E.plainPrint)),
            "reapplied" -> Data(optString.plainPrint)
          )
        )
      case _ => Expr(Data("Not a tuple5"))
    }

  private val tuple6Test = Type.Ctor6.of[Tuple6]

  def testTypeCtor6[In: Type]: Expr[Data] =
    Type[In] match {
      case tuple6Test(aParam, bParam, cParam, dParam, eParam, fParam) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        val optString = tuple6Test(using String, String, String, String, String, String)
        Expr(
          Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint)
            ),
            "reapplied" -> Data(optString.plainPrint)
          )
        )
      case _ => Expr(Data("Not a tuple6"))
    }

  private val tuple7Test = Type.Ctor7.of[Tuple7]

  def testTypeCtor7[In: Type]: Expr[Data] =
    Type[In] match {
      case tuple7Test(aParam, bParam, cParam, dParam, eParam, fParam, gParam) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        val optString = tuple7Test(using String, String, String, String, String, String, String)
        Expr(
          Data.map(
            "unapplied" -> Data.list(
              Data(A.plainPrint),
              Data(B.plainPrint),
              Data(C.plainPrint),
              Data(D.plainPrint),
              Data(E.plainPrint),
              Data(F.plainPrint),
              Data(G.plainPrint)
            ),
            "reapplied" -> Data(optString.plainPrint)
          )
        )
      case _ => Expr(Data("Not a tuple7"))
    }

  private val tuple8Test = Type.Ctor8.of[Tuple8]

  def testTypeCtor8[In: Type]: Expr[Data] =
    Type[In] match {
      case tuple8Test(aParam, bParam, cParam, dParam, eParam, fParam, gParam, hParam) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        val optString = tuple8Test(using String, String, String, String, String, String, String, String)
        Expr(
          Data.map(
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
            "reapplied" -> Data(optString.plainPrint)
          )
        )
      case _ => Expr(Data("Not a tuple8"))
    }

  private val tuple9Test = Type.Ctor9.of[Tuple9]

  def testTypeCtor9[In: Type]: Expr[Data] =
    Type[In] match {
      case tuple9Test(aParam, bParam, cParam, dParam, eParam, fParam, gParam, hParam, iParam) =>
        import aParam.Underlying as A
        import bParam.Underlying as B
        import cParam.Underlying as C
        import dParam.Underlying as D
        import eParam.Underlying as E
        import fParam.Underlying as F
        import gParam.Underlying as G
        import hParam.Underlying as H
        import iParam.Underlying as I
        val optString = tuple9Test(using String, String, String, String, String, String, String, String, String)
        Expr(
          Data.map(
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
            "reapplied" -> Data(optString.plainPrint)
          )
        )
      case _ => Expr(Data("Not a tuple9"))
    }

  private val tuple10Test = Type.Ctor10.of[Tuple10]

  def testTypeCtor10[In: Type]: Expr[Data] =
    Type[In] match {
      case tuple10Test(aParam, bParam, cParam, dParam, eParam, fParam, gParam, hParam, iParam, jParam) =>
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
        val optString =
          tuple10Test(using String, String, String, String, String, String, String, String, String, String)
        Expr(
          Data.map(
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
            "reapplied" -> Data(optString.plainPrint)
          )
        )
      case _ => Expr(Data("Not a tuple10"))
    }

  private val tuple11Test = Type.Ctor11.of[Tuple11]

  def testTypeCtor11[In: Type]: Expr[Data] =
    Type[In] match {
      case tuple11Test(aParam, bParam, cParam, dParam, eParam, fParam, gParam, hParam, iParam, jParam, kParam) =>
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
        val optString =
          tuple11Test(using String, String, String, String, String, String, String, String, String, String, String)
        Expr(
          Data.map(
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
            "reapplied" -> Data(optString.plainPrint)
          )
        )
      case _ => Expr(Data("Not a tuple11"))
    }

  private val tuple12Test = Type.Ctor12.of[Tuple12]

  def testTypeCtor12[In: Type]: Expr[Data] =
    Type[In] match {
      case tuple12Test(
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
        val optString = tuple12Test(using
          String,
          String,
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
        )
        Expr(
          Data.map(
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
            "reapplied" -> Data(optString.plainPrint)
          )
        )
      case _ => Expr(Data("Not a tuple12"))
    }

  private val tuple13Test = Type.Ctor13.of[Tuple13]

  def testTypeCtor13[In: Type]: Expr[Data] =
    Type[In] match {
      case tuple13Test(
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
        val optString = tuple13Test(using
          String,
          String,
          String,
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
        )
        Expr(
          Data.map(
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
            "reapplied" -> Data(optString.plainPrint)
          )
        )
      case _ => Expr(Data("Not a tuple13"))
    }

  private val tuple14Test = Type.Ctor14.of[Tuple14]

  def testTypeCtor14[In: Type]: Expr[Data] =
    Type[In] match {
      case tuple14Test(
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
        val optString = tuple14Test(using
          String,
          String,
          String,
          String,
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
        )
        Expr(
          Data.map(
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
            "reapplied" -> Data(optString.plainPrint)
          )
        )
      case _ => Expr(Data("Not a tuple14"))
    }

  private val tuple15Test = Type.Ctor15.of[Tuple15]

  def testTypeCtor15[In: Type]: Expr[Data] =
    Type[In] match {
      case tuple15Test(
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
        val optString = tuple15Test(using
          String,
          String,
          String,
          String,
          String,
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
        )
        Expr(
          Data.map(
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
            "reapplied" -> Data(optString.plainPrint)
          )
        )
      case _ => Expr(Data("Not a tuple15"))
    }

  private val tuple16Test = Type.Ctor16.of[Tuple16]

  def testTypeCtor16[In: Type]: Expr[Data] =
    Type[In] match {
      case tuple16Test(
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
        val optString = tuple16Test(using
          String,
          String,
          String,
          String,
          String,
          String,
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
        )
        Expr(
          Data.map(
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
            "reapplied" -> Data(optString.plainPrint)
          )
        )
      case _ => Expr(Data("Not a tuple16"))
    }

  private val tuple17Test = Type.Ctor17.of[Tuple17]

  def testTypeCtor17[In: Type]: Expr[Data] =
    Type[In] match {
      case tuple17Test(
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
        val optString = tuple17Test(using
          String,
          String,
          String,
          String,
          String,
          String,
          String,
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
        )
        Expr(
          Data.map(
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
            "reapplied" -> Data(optString.plainPrint)
          )
        )
      case _ => Expr(Data("Not a tuple17"))
    }

  private val tuple18Test = Type.Ctor18.of[Tuple18]

  def testTypeCtor18[In: Type]: Expr[Data] =
    Type[In] match {
      case tuple18Test(
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
        val optString = tuple18Test(using
          String,
          String,
          String,
          String,
          String,
          String,
          String,
          String,
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
        )
        Expr(
          Data.map(
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
            "reapplied" -> Data(optString.plainPrint)
          )
        )
      case _ => Expr(Data("Not a tuple18"))
    }

  private val tuple19Test = Type.Ctor19.of[Tuple19]

  def testTypeCtor19[In: Type]: Expr[Data] =
    Type[In] match {
      case tuple19Test(
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
        val optString = tuple19Test(using
          String,
          String,
          String,
          String,
          String,
          String,
          String,
          String,
          String,
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
        )
        Expr(
          Data.map(
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
            "reapplied" -> Data(optString.plainPrint)
          )
        )
      case _ => Expr(Data("Not a tuple19"))
    }

  private val tuple20Test = Type.Ctor20.of[Tuple20]

  def testTypeCtor20[In: Type]: Expr[Data] =
    Type[In] match {
      case tuple20Test(
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
        val optString = tuple20Test(using
          String,
          String,
          String,
          String,
          String,
          String,
          String,
          String,
          String,
          String,
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
        )
        Expr(
          Data.map(
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
            "reapplied" -> Data(optString.plainPrint)
          )
        )
      case _ => Expr(Data("Not a tuple20"))
    }

  private val tuple21Test = Type.Ctor21.of[Tuple21]

  def testTypeCtor21[In: Type]: Expr[Data] =
    Type[In] match {
      case tuple21Test(
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
        val optString = tuple21Test(
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
        )
        Expr(
          Data.map(
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
            "reapplied" -> Data(optString.plainPrint)
          )
        )
      case _ => Expr(Data("Not a tuple21"))
    }

  private val tuple22Test = Type.Ctor22.of[Tuple22]

  def testTypeCtor22[In: Type]: Expr[Data] =
    Type[In] match {
      case tuple22Test(
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
        val optString = tuple22Test(
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
        )
        Expr(
          Data.map(
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
            "reapplied" -> Data(optString.plainPrint)
          )
        )
      case _ => Expr(Data("Not a tuple22"))
    }
}
