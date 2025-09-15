package hearth
package typed

import hearth.data.Data
import hearth.fp.instances.*
import hearth.fp.syntax.*

/** Fixtured for testing [[ExprsSpec]]. */
trait ExprsFixturesImpl { this: MacroTypedCommons =>

  def testExprPrinters[A: Type](expr: Expr[A]): Expr[Data] = Expr(
    Data.map(
      "Expr.plainPrint" -> Data(Expr.plainPrint(expr)),
      "Expr.prettyPrint" -> Data(removeAnsiColors((Expr.prettyPrint(expr)))),
      "Expr.plainAST" -> Data(Expr.plainAST(expr)),
      "Expr.prettyAST" -> Data(removeAnsiColors((Expr.prettyAST(expr))))
    )
  )

  def testExprSummoning[A: Type]: Expr[Data] = Expr(
    Data.map(
      "Expr.summonImplicit" -> Data(Expr.summonImplicit[A].fold("<failed to summon>")(_.plainPrint))
    )
  )

  def testExprUpcasting[A: Type, B: Type](expr: Expr[A]): Expr[Data] = Expr(
    Data.map(
      "Expr.upcast" -> Data(scala.util.Try(Expr.upcast[A, B](expr)).toOption.fold("<failed to upcast>")(_.plainPrint))
    )
  )

  def testSuppressUnused[A: Type](expr: Expr[A]): Expr[Unit] =
    Expr.suppressUnused(expr)

  private val IntType = Type.of[Int]
  private val DataType = Type.of[Data]

  def testMatchCaseTypeMatch[A: Type](expr: Expr[A]): Expr[Data] = {
    implicit val dataType: Type[Data] = DataType

    Type
      .exhaustiveChildren[A]
      .fold(Expr(Data("<no exhaustive children>"))) { children =>
        expr.matchOn(children.toNonEmptyList.map { case (name, child) =>
          import child.Underlying as Child
          MatchCase.typeMatch[Child](FreshName.FromType).map { matchedExpr =>
            Expr(
              Data.map(
                "name" -> Data(name),
                "type" -> Data(Type.plainPrint[Child]),
                "matchCase" -> Data(matchedExpr.plainPrint)
              )
            )
          }
        })
      }
  }

  def testMatchCasePartition[A: Type, B: Type](expr: Expr[A]): Expr[B] = {
    val matched = MatchCase.typeMatch[B]("matched")
    val unmatchedOpt = MatchCase.typeMatch[A]("unmatched").partition { a =>
      if (Type[A] <:< Type.of[AnyRef] && Type[B] <:< Type.of[AnyRef]) Right {
        Expr.quote(Expr.splice(a).asInstanceOf[B])
      }
      else Left("Not supported")
    }
    @scala.annotation.nowarn // TODO: make quote by-name to avoid such errors
    def runtimeFail: Expr[B] = Expr.quote(???)
    unmatchedOpt.fold[Expr[B]](_ => runtimeFail, unmatched => expr.matchOn(matched, unmatched))
  }

  def testMatchCaseTraverse[A: Type, B: Type](expr: Expr[A]): Expr[B] = {
    val matched = MatchCase.typeMatch[B]("matched")
    val unmatchedOpt = MatchCase.typeMatch[A]("unmatched").traverse { a =>
      if (Type[A] <:< Type.of[AnyRef] && Type[B] <:< Type.of[AnyRef]) Some {
        Expr.quote(Expr.splice(a).asInstanceOf[B])
      }
      else None
    }
    @scala.annotation.nowarn // TODO: make quote by-name to avoid such errors
    def runtimeFail: Expr[B] = Expr.quote(???)
    unmatchedOpt.fold[Expr[B]](runtimeFail) { unmatched =>
      expr.matchOn(matched, unmatched)
    }
  }

  def testScopeCreateAndUse: Expr[Data] = {
    implicit val intType: Type[Int] = IntType
    val valValue = Scoped.createVal(Expr(1), "a").use { (a: Expr[Int]) =>
      Expr.quote(Expr.splice(a) + 1)
    }
    val varValue = Scoped.createVar(Expr(1), "b").use { case (b, set) =>
      Expr.quote {
        Expr.splice(set(Expr.quote(Expr.splice(b) + 1)))
        Expr.splice(b) * 10
      }
    }
    val lazyValue = Scoped.createLazy(Expr(1), "c").use { (c: Expr[Int]) =>
      Expr.quote((Expr.splice(c) + 2) * 100)
    }
    val defValue = Scoped.createDef(Expr(1), "d").use { (a: Expr[Int]) =>
      Expr.quote((Expr.splice(a) + 3) * 1000)
    }

    Expr.quote {
      Data(Expr.splice(valValue) + Expr.splice(varValue) + Expr.splice(lazyValue) + Expr.splice(defValue))
    }
  }

  // TODO: test partition
  // TODO: add traverse here
}
