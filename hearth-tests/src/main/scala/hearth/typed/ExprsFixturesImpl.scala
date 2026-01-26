package hearth
package typed

import hearth.data.Data
import hearth.fp.instances.*
import hearth.fp.syntax.*

/** Fixtured for testing [[ExprsSpec]]. */
trait ExprsFixturesImpl { this: MacroTypedCommons & hearth.untyped.UntypedMethods =>

  // Expr methods

  def testExprPrinters[A: Type](expr: Expr[A]): Expr[Data] = Expr(
    Data.map(
      "Expr.plainPrint" -> Data(expr.plainPrint),
      "Expr.prettyPrint" -> Data(removeAnsiColors((expr.prettyPrint))),
      "Expr.plainAST" -> Data(expr.plainAST),
      "Expr.prettyAST" -> Data(removeAnsiColors((expr.prettyAST)))
    )
  )

  def testExprSummoning[A: Type]: Expr[Data] = Expr(
    Data.map(
      "Expr.summonImplicit" -> Data(
        Expr
          .summonImplicit[A]
          .toEither
          .fold(error => "<failed to summon: " + removeAnsiColors(error) + ">", _.plainPrint)
      )
    )
  )

  def testExprSummoningIgnoring[A: Type, Companion: Type](ignoredNames: VarArgs[String]): Expr[Data] = {
    val shouldBeIgnored = ignoredNames.toList.collect { case Expr(name) =>
      name
    }.toSet
    val ignoredMethods = Type[Companion].methods.collect {
      case method if shouldBeIgnored(method.value.name) => method.value.asUntyped
    }.toSeq
    Expr(
      Data.map(
        "Expr.summonImplicitIgnoring" -> Data(
          Expr
            .summonImplicitIgnoring[A](ignoredMethods*)
            .toEither
            .fold(error => "<failed to summon: " + removeAnsiColors(error) + ">", _.plainPrint)
        )
      )
    )
  }

  def testExprUpcasting[A: Type, B: Type](expr: Expr[A]): Expr[Data] = Expr(
    Data.map(
      "Expr.upcast" -> Data(scala.util.Try(Expr.upcast[A, B](expr)).toOption.fold("<failed to upcast>")(_.plainPrint))
    )
  )

  def testSuppressUnused[A: Type](expr: Expr[A]): Expr[Unit] =
    expr.suppressUnused

  // VarArgs methods

  def testVarArgs[A: Type](exprs: VarArgs[A]): Expr[Data] = {
    implicit val dataType: Type[Data] = DataType
    val iterable = exprs.toIterable.map { expr =>
      Expr.quote {
        Data("value: " + Expr.splice(expr).toString)
      }
    }.toSeq

    Expr.quote {
      Data.map(
        "VarArgs.apply" -> Data.list(Expr.splice(VarArgs(iterable*))*),
        "VarArgs.from" -> Data.list(Expr.splice(VarArgs.from(iterable))*)
      )
    }
  }

  // MatchCase methods

  def testMatchCaseTypeMatch[A: Type](expr: Expr[A]): Expr[Data] = {
    implicit val dataType: Type[Data] = DataType

    Type[A].exhaustiveChildren
      .fold(Expr(Data("<no exhaustive children>"))) { children =>
        expr.matchOn(children.toNonEmptyList.map { case (name, child) =>
          import child.Underlying as Child
          MatchCase.typeMatch[Child](FreshName.FromType).map { matchedExpr =>
            Expr(
              Data.map(
                "name" -> Data(name),
                "type" -> Data(Type[Child].plainPrint),
                "matchCase" -> Data(matchedExpr.plainPrint)
              )
            )
          }
        })
      }
  }

  def testMatchCasePartition[A: Type, B: Type](expr: Expr[A]): Expr[B] = {
    val matched = MatchCase.typeMatch[B]("matched")
    val either = MatchCase.typeMatch[A]("unmatched").partition { a =>
      if (Type[A] <:< Type.of[AnyRef] && Type[B] <:< Type.of[AnyRef]) Right {
        Expr.quote(Expr.splice(a).asInstanceOf[B])
      }
      else Left(runtimeFail[B])
    }
    either.fold[Expr[B]](throwing => expr.matchOn(matched, throwing), unmatched => expr.matchOn(matched, unmatched))
  }

  def testMatchCaseTraverse[A: Type, B: Type](expr: Expr[A]): Expr[B] = {
    val matched = MatchCase.typeMatch[B]("matched")
    val option = MatchCase.typeMatch[A]("unmatched").traverse { a =>
      if (Type[A] <:< Type.of[AnyRef] && Type[B] <:< Type.of[AnyRef]) Some {
        Expr.quote(Expr.splice(a).asInstanceOf[B])
      }
      else None
    }
    option.fold[Expr[B]](runtimeFail[B]) { unmatched =>
      expr.matchOn(matched, unmatched)
    }
  }

  // ValDefs methods

  def testValDefsCreateAndUse: Expr[Data] = {
    implicit val intType: Type[Int] = IntType
    val valValue = ValDefs.createVal(Expr(1), "a").use { (a: Expr[Int]) =>
      Expr.quote(Expr.splice(a) + 1)
    }
    val varValue = ValDefs.createVar(Expr(1), "b").use { case (b, set) =>
      Expr.quote {
        Expr.splice(set(Expr.quote(Expr.splice(b) + 1)))
        Expr.splice(b) * 10
      }
    }
    val lazyValue = ValDefs.createLazy(Expr(1), "c").use { (c: Expr[Int]) =>
      Expr.quote((Expr.splice(c) + 2) * 100)
    }
    val defValue = ValDefs.createDef(Expr(1), "d").use { (a: Expr[Int]) =>
      Expr.quote((Expr.splice(a) + 3) * 1000)
    }

    Expr.quote {
      Data.map(
        "val" -> Data(Expr.splice(valValue)),
        "var" -> Data(Expr.splice(varValue)),
        "lazy" -> Data(Expr.splice(lazyValue)),
        "def" -> Data(Expr.splice(defValue))
      )
    }
  }

  def testValDefsPartitionAndClose[A: Type, B: Type](expr: Expr[A]): Expr[B] = {
    val either = ValDefs.createDef(expr, "a").partition { a =>
      if (Type[A] <:< Type.of[AnyRef] && Type[B] <:< Type.of[AnyRef]) Right {
        Expr.quote(Expr.splice(a).asInstanceOf[B])
      }
      else Left(runtimeFail[B])
    }
    either.fold[Expr[B]](_.close, _.close)
  }

  def testValDefsTraverseAndClose[A: Type, B: Type](expr: Expr[A]): Expr[B] = {
    val option = ValDefs.createDef(expr, "a").traverse { a =>
      if (Type[A] <:< Type.of[AnyRef] && Type[B] <:< Type.of[AnyRef]) Some {
        Expr.quote(Expr.splice(a).asInstanceOf[B])
      }
      else None
    }
    option.fold[Expr[B]](runtimeFail)(_.close)
  }

  // ValDefBuilder methods

  def testValDefBuilderCreateAndUse: Expr[Data] = {
    implicit val intType: Type[Int] = IntType
    // val has setter, cannot call itself, etc, so we're ignoring the Unit parameter
    val valValue = ValDefBuilder.ofVal[Int]("valExample").map(_ => Expr.quote(1)).build.close
    // var has setter, we could pass it to somewhere, but we're ignoring it here
    val varValue = ValDefBuilder.ofVar[Int]("varExample").map(_ => Expr.quote(2)).build.close
    // lazy val can refer to itself, we could pass it somewhere, but we're ignoring it here
    val lazyValue = ValDefBuilder.ofLazy[Int]("lazyExample").map(_ => Expr.quote(3)).build.close

    // def can refer to itself, we could pass it somewhere, but we're ignoring it here
    // format: off
    val def0Value = ValDefBuilder.ofDef0[Int]("def0Example").buildWith(_ => Expr.quote(0)).close
    val def1Value = ValDefBuilder
      .ofDef1[Int, Int]("def1Example", "a")
      .buildWith { case (_, a) =>
        Expr.quote(Expr.splice(a) * 10)
      }
      .use(_(Expr.quote(1)))
    val def2Value = ValDefBuilder
      .ofDef2[Int, Int, Int]("def2Example", "a", "b")
      .buildWith { case (_, (a, b)) =>
        Expr.quote((Expr.splice(a) + Expr.splice(b)) * 10)
      }
      .use(_(Expr.quote(1), Expr.quote(2)))
    val def3Value = ValDefBuilder
      .ofDef3[Int, Int, Int, Int]("def3Example", "a", "b", "c")
      .buildWith { case (_, (a, b, c)) =>
        Expr.quote((Expr.splice(a) + Expr.splice(b) + Expr.splice(c)) * 10)
      }
      .use(_(Expr.quote(1), Expr.quote(2), Expr.quote(3)))
    val def4Value = ValDefBuilder
      .ofDef4[Int, Int, Int, Int, Int]("def4Example", "a", "b", "c", "d")
      .buildWith { case (_, (a, b, c, d)) =>
        Expr.quote((Expr.splice(a) + Expr.splice(b) + Expr.splice(c) + Expr.splice(d)) * 10)
      }
      .use(_(Expr.quote(1), Expr.quote(2), Expr.quote(3), Expr.quote(4)))
    val def5Value = ValDefBuilder
      .ofDef5[Int, Int, Int, Int, Int, Int]("def5Example", "a", "b", "c", "d", "e")
      .buildWith { case (_, (a, b, c, d, e)) =>
        Expr.quote((Expr.splice(a) + Expr.splice(b) + Expr.splice(c) + Expr.splice(d) + Expr.splice(e)) * 10)
      }
      .use(_(Expr.quote(1), Expr.quote(2), Expr.quote(3), Expr.quote(4), Expr.quote(5)))
    val def6Value = ValDefBuilder
      .ofDef6[Int, Int, Int, Int, Int, Int, Int]("def6Example", "a", "b", "c", "d", "e", "f")
      .buildWith { case (_, (a, b, c, d, e, f)) =>
        Expr.quote((Expr.splice(a) + Expr.splice(b) + Expr.splice(c) + Expr.splice(d) + Expr.splice(e) + Expr.splice(f)) * 10)
      }
      .use(_(Expr.quote(1), Expr.quote(2), Expr.quote(3), Expr.quote(4), Expr.quote(5), Expr.quote(6)))
    val def7Value = ValDefBuilder
      .ofDef7[Int, Int, Int, Int, Int, Int, Int, Int]("def7Example", "a", "b", "c", "d", "e", "f", "g")
      .buildWith { case (_, (a, b, c, d, e, f, g)) =>
        Expr.quote((Expr.splice(a) + Expr.splice(b) + Expr.splice(c) + Expr.splice(d) + Expr.splice(e) + Expr.splice(f) + Expr.splice(g)) * 10)
      }
      .use(_(Expr.quote(1), Expr.quote(2), Expr.quote(3), Expr.quote(4), Expr.quote(5), Expr.quote(6), Expr.quote(7)))
    val def8Value = ValDefBuilder
      .ofDef8[Int, Int, Int, Int, Int, Int, Int, Int, Int]("def8Example", "a", "b", "c", "d", "e", "f", "g", "h")
      .buildWith { case (_, (a, b, c, d, e, f, g, h)) =>
        Expr.quote((Expr.splice(a) + Expr.splice(b) + Expr.splice(c) + Expr.splice(d) + Expr.splice(e) + Expr.splice(f) + Expr.splice(g) + Expr.splice(h)) * 10)
      }
      .use(_(Expr.quote(1), Expr.quote(2), Expr.quote(3), Expr.quote(4), Expr.quote(5), Expr.quote(6), Expr.quote(7), Expr.quote(8)))
    val def9Value = ValDefBuilder
      .ofDef9[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("def9Example", "a", "b", "c", "d", "e", "f", "g", "h", "i")
      .buildWith { case (_, (a, b, c, d, e, f, g, h, i)) =>
        Expr.quote((Expr.splice(a) + Expr.splice(b) + Expr.splice(c) + Expr.splice(d) + Expr.splice(e) + Expr.splice(f) + Expr.splice(g) + Expr.splice(h) + Expr.splice(i)) * 10)
      }
      .use(_(Expr.quote(1), Expr.quote(2), Expr.quote(3), Expr.quote(4), Expr.quote(5), Expr.quote(6), Expr.quote(7), Expr.quote(8), Expr.quote(9)))
    val def10Value = ValDefBuilder
      .ofDef10[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("def10Example", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j")
      .buildWith { case (_, (a, b, c, d, e, f, g, h, i, j)) =>
        Expr.quote((Expr.splice(a) + Expr.splice(b) + Expr.splice(c) + Expr.splice(d) + Expr.splice(e) + Expr.splice(f) + Expr.splice(g) + Expr.splice(h) + Expr.splice(i) + Expr.splice(j)) * 10)
      }
      .use(_(Expr.quote(1), Expr.quote(2), Expr.quote(3), Expr.quote(4), Expr.quote(5), Expr.quote(6), Expr.quote(7), Expr.quote(8), Expr.quote(9), Expr.quote(10)))
    val def11Value = ValDefBuilder
      .ofDef11[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("def11Example", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k")
      .buildWith { case (_, (a, b, c, d, e, f, g, h, i, j, k)) =>
        Expr.quote((Expr.splice(a) + Expr.splice(b) + Expr.splice(c) + Expr.splice(d) + Expr.splice(e) + Expr.splice(f) + Expr.splice(g) + Expr.splice(h) + Expr.splice(i) + Expr.splice(j) + Expr.splice(k)) * 10)
      }
      .use(_(Expr.quote(1), Expr.quote(2), Expr.quote(3), Expr.quote(4), Expr.quote(5), Expr.quote(6), Expr.quote(7), Expr.quote(8), Expr.quote(9), Expr.quote(10), Expr.quote(11)))
    val def12Value = ValDefBuilder
      .ofDef12[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("def12Example", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l")
      .buildWith { case (_, (a, b, c, d, e, f, g, h, i, j, k, l)) =>
        Expr.quote((Expr.splice(a) + Expr.splice(b) + Expr.splice(c) + Expr.splice(d) + Expr.splice(e) + Expr.splice(f) + Expr.splice(g) + Expr.splice(h) + Expr.splice(i) + Expr.splice(j) + Expr.splice(k) + Expr.splice(l)) * 10)
      }
      .use(_(Expr.quote(1), Expr.quote(2), Expr.quote(3), Expr.quote(4), Expr.quote(5), Expr.quote(6), Expr.quote(7), Expr.quote(8), Expr.quote(9), Expr.quote(10), Expr.quote(11), Expr.quote(12)))
    val def13Value = ValDefBuilder
      .ofDef13[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("def13Example", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m")
      .buildWith { case (_, (a, b, c, d, e, f, g, h, i, j, k, l, m)) =>
        Expr.quote((Expr.splice(a) + Expr.splice(b) + Expr.splice(c) + Expr.splice(d) + Expr.splice(e) + Expr.splice(f) + Expr.splice(g) + Expr.splice(h) + Expr.splice(i) + Expr.splice(j) + Expr.splice(k) + Expr.splice(l) + Expr.splice(m)) * 10)
      }
      .use(_(Expr.quote(1), Expr.quote(2), Expr.quote(3), Expr.quote(4), Expr.quote(5), Expr.quote(6), Expr.quote(7), Expr.quote(8), Expr.quote(9), Expr.quote(10), Expr.quote(11), Expr.quote(12), Expr.quote(13)))
    val def14Value = ValDefBuilder
      .ofDef14[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("def14Example", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n")
      .buildWith { case (_, (a, b, c, d, e, f, g, h, i, j, k, l, m, n)) =>
        Expr.quote((Expr.splice(a) + Expr.splice(b) + Expr.splice(c) + Expr.splice(d) + Expr.splice(e) + Expr.splice(f) + Expr.splice(g) + Expr.splice(h) + Expr.splice(i) + Expr.splice(j) + Expr.splice(k) + Expr.splice(l) + Expr.splice(m) + Expr.splice(n)) * 10)
      }
      .use(_(Expr.quote(1), Expr.quote(2), Expr.quote(3), Expr.quote(4), Expr.quote(5), Expr.quote(6), Expr.quote(7), Expr.quote(8), Expr.quote(9), Expr.quote(10), Expr.quote(11), Expr.quote(12), Expr.quote(13), Expr.quote(14)))
    val def15Value = ValDefBuilder
      .ofDef15[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("def15Example", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o")
      .buildWith { case (_, (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)) =>
        Expr.quote((Expr.splice(a) + Expr.splice(b) + Expr.splice(c) + Expr.splice(d) + Expr.splice(e) + Expr.splice(f) + Expr.splice(g) + Expr.splice(h) + Expr.splice(i) + Expr.splice(j) + Expr.splice(k) + Expr.splice(l) + Expr.splice(m) + Expr.splice(n) + Expr.splice(o)) * 10)
      }
      .use(_(Expr.quote(1), Expr.quote(2), Expr.quote(3), Expr.quote(4), Expr.quote(5), Expr.quote(6), Expr.quote(7), Expr.quote(8), Expr.quote(9), Expr.quote(10), Expr.quote(11), Expr.quote(12), Expr.quote(13), Expr.quote(14), Expr.quote(15)))
    val def16Value = ValDefBuilder
      .ofDef16[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("def16Example", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p")
      .buildWith { case (_, (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)) =>
        Expr.quote((Expr.splice(a) + Expr.splice(b) + Expr.splice(c) + Expr.splice(d) + Expr.splice(e) + Expr.splice(f) + Expr.splice(g) + Expr.splice(h) + Expr.splice(i) + Expr.splice(j) + Expr.splice(k) + Expr.splice(l) + Expr.splice(m) + Expr.splice(n) + Expr.splice(o) + Expr.splice(p)) * 10)
      }
      .use(_(Expr.quote(1), Expr.quote(2), Expr.quote(3), Expr.quote(4), Expr.quote(5), Expr.quote(6), Expr.quote(7), Expr.quote(8), Expr.quote(9), Expr.quote(10), Expr.quote(11), Expr.quote(12), Expr.quote(13), Expr.quote(14), Expr.quote(15), Expr.quote(16)))
    val def17Value = ValDefBuilder
      .ofDef17[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("def17Example", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q")
      .buildWith { case (_, (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q)) =>
        Expr.quote((Expr.splice(a) + Expr.splice(b) + Expr.splice(c) + Expr.splice(d) + Expr.splice(e) + Expr.splice(f) + Expr.splice(g) + Expr.splice(h) + Expr.splice(i) + Expr.splice(j) + Expr.splice(k) + Expr.splice(l) + Expr.splice(m) + Expr.splice(n) + Expr.splice(o) + Expr.splice(p) + Expr.splice(q)) * 10)
      }
      .use(_(Expr.quote(1), Expr.quote(2), Expr.quote(3), Expr.quote(4), Expr.quote(5), Expr.quote(6), Expr.quote(7), Expr.quote(8), Expr.quote(9), Expr.quote(10), Expr.quote(11), Expr.quote(12), Expr.quote(13), Expr.quote(14), Expr.quote(15), Expr.quote(16), Expr.quote(17)))
    val def18Value = ValDefBuilder
      .ofDef18[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("def18Example", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r")
      .buildWith { case (_, (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r)) =>
        Expr.quote((Expr.splice(a) + Expr.splice(b) + Expr.splice(c) + Expr.splice(d) + Expr.splice(e) + Expr.splice(f) + Expr.splice(g) + Expr.splice(h) + Expr.splice(i) + Expr.splice(j) + Expr.splice(k) + Expr.splice(l) + Expr.splice(m) + Expr.splice(n) + Expr.splice(o) + Expr.splice(p) + Expr.splice(q) + Expr.splice(r)) * 10)
      }
      .use(_(Expr.quote(1), Expr.quote(2), Expr.quote(3), Expr.quote(4), Expr.quote(5), Expr.quote(6), Expr.quote(7), Expr.quote(8), Expr.quote(9), Expr.quote(10), Expr.quote(11), Expr.quote(12), Expr.quote(13), Expr.quote(14), Expr.quote(15), Expr.quote(16), Expr.quote(17), Expr.quote(18)))
    val def19Value = ValDefBuilder
      .ofDef19[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("def19Example", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s")
      .buildWith { case (_, (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s)) =>
        Expr.quote((Expr.splice(a) + Expr.splice(b) + Expr.splice(c) + Expr.splice(d) + Expr.splice(e) + Expr.splice(f) + Expr.splice(g) + Expr.splice(h) + Expr.splice(i) + Expr.splice(j) + Expr.splice(k) + Expr.splice(l) + Expr.splice(m) + Expr.splice(n) + Expr.splice(o) + Expr.splice(p) + Expr.splice(q) + Expr.splice(r) + Expr.splice(s)) * 10)
      }
      .use(_(Expr.quote(1), Expr.quote(2), Expr.quote(3), Expr.quote(4), Expr.quote(5), Expr.quote(6), Expr.quote(7), Expr.quote(8), Expr.quote(9), Expr.quote(10), Expr.quote(11), Expr.quote(12), Expr.quote(13), Expr.quote(14), Expr.quote(15), Expr.quote(16), Expr.quote(17), Expr.quote(18), Expr.quote(19)))
    val def20Value = ValDefBuilder
      .ofDef20[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("def20Example", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t")
      .buildWith { case (_, (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t)) =>
        Expr.quote((Expr.splice(a) + Expr.splice(b) + Expr.splice(c) + Expr.splice(d) + Expr.splice(e) + Expr.splice(f) + Expr.splice(g) + Expr.splice(h) + Expr.splice(i) + Expr.splice(j) + Expr.splice(k) + Expr.splice(l) + Expr.splice(m) + Expr.splice(n) + Expr.splice(o) + Expr.splice(p) + Expr.splice(q) + Expr.splice(r) + Expr.splice(s) + Expr.splice(t)) * 10)
      }
      .use(_(Expr.quote(1), Expr.quote(2), Expr.quote(3), Expr.quote(4), Expr.quote(5), Expr.quote(6), Expr.quote(7), Expr.quote(8), Expr.quote(9), Expr.quote(10), Expr.quote(11), Expr.quote(12), Expr.quote(13), Expr.quote(14), Expr.quote(15), Expr.quote(16), Expr.quote(17), Expr.quote(18), Expr.quote(19), Expr.quote(20)))
    val def21Value = ValDefBuilder
      .ofDef21[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("def21Example", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u")
      .buildWith { case (_, (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u)) =>
        Expr.quote((Expr.splice(a) + Expr.splice(b) + Expr.splice(c) + Expr.splice(d) + Expr.splice(e) + Expr.splice(f) + Expr.splice(g) + Expr.splice(h) + Expr.splice(i) + Expr.splice(j) + Expr.splice(k) + Expr.splice(l) + Expr.splice(m) + Expr.splice(n) + Expr.splice(o) + Expr.splice(p) + Expr.splice(q) + Expr.splice(r) + Expr.splice(s) + Expr.splice(t) + Expr.splice(u)) * 10)
      }
      .use(_(Expr.quote(1), Expr.quote(2), Expr.quote(3), Expr.quote(4), Expr.quote(5), Expr.quote(6), Expr.quote(7), Expr.quote(8), Expr.quote(9), Expr.quote(10), Expr.quote(11), Expr.quote(12), Expr.quote(13), Expr.quote(14), Expr.quote(15), Expr.quote(16), Expr.quote(17), Expr.quote(18), Expr.quote(19), Expr.quote(20), Expr.quote(21)))
    val def22Value = ValDefBuilder
      .ofDef22[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("def22Example", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v")
      .buildWith { case (_, (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v)) =>
        Expr.quote((Expr.splice(a) + Expr.splice(b) + Expr.splice(c) + Expr.splice(d) + Expr.splice(e) + Expr.splice(f) + Expr.splice(g) + Expr.splice(h) + Expr.splice(i) + Expr.splice(j) + Expr.splice(k) + Expr.splice(l) + Expr.splice(m) + Expr.splice(n) + Expr.splice(o) + Expr.splice(p) + Expr.splice(q) + Expr.splice(r) + Expr.splice(s) + Expr.splice(t) + Expr.splice(u) + Expr.splice(v)) * 10)
      }
      .use(_(Expr.quote(1), Expr.quote(2), Expr.quote(3), Expr.quote(4), Expr.quote(5), Expr.quote(6), Expr.quote(7), Expr.quote(8), Expr.quote(9), Expr.quote(10), Expr.quote(11), Expr.quote(12), Expr.quote(13), Expr.quote(14), Expr.quote(15), Expr.quote(16), Expr.quote(17), Expr.quote(18), Expr.quote(19), Expr.quote(20), Expr.quote(21), Expr.quote(22)))
    // format: on

    Expr.quote {
      Data.map(
        "val" -> Data(Expr.splice(valValue)),
        "var" -> Data(Expr.splice(varValue)),
        "lazy" -> Data(Expr.splice(lazyValue)),
        "def0" -> Data(Expr.splice(def0Value)),
        "def1" -> Data(Expr.splice(def1Value)),
        "def2" -> Data(Expr.splice(def2Value)),
        "def3" -> Data(Expr.splice(def3Value)),
        "def4" -> Data(Expr.splice(def4Value)),
        "def5" -> Data(Expr.splice(def5Value)),
        "def6" -> Data(Expr.splice(def6Value)),
        "def7" -> Data(Expr.splice(def7Value)),
        "def8" -> Data(Expr.splice(def8Value)),
        "def9" -> Data(Expr.splice(def9Value)),
        "def10" -> Data(Expr.splice(def10Value)),
        "def11" -> Data(Expr.splice(def11Value)),
        "def12" -> Data(Expr.splice(def12Value)),
        "def13" -> Data(Expr.splice(def13Value)),
        "def14" -> Data(Expr.splice(def14Value)),
        "def15" -> Data(Expr.splice(def15Value)),
        "def16" -> Data(Expr.splice(def16Value)),
        "def17" -> Data(Expr.splice(def17Value)),
        "def18" -> Data(Expr.splice(def18Value)),
        "def19" -> Data(Expr.splice(def19Value)),
        "def20" -> Data(Expr.splice(def20Value)),
        "def21" -> Data(Expr.splice(def21Value)),
        "def22" -> Data(Expr.splice(def22Value))
      )
    }
  }

  def testValDefBuilderPartitionAndClose[A: Type, B: Type](expr: Expr[A]): Expr[B] = {
    val either = ValDefBuilder.ofDef0[B]("a").map(_ => expr).partition { a =>
      if (Type[A] <:< Type.of[AnyRef] && Type[B] <:< Type.of[AnyRef]) Right {
        Expr.quote(Expr.splice(a).asInstanceOf[B])
      }
      else Left(runtimeFail[B])
    }
    either.fold[ValDefs[Expr[B]]](_.build, _.build).close
  }

  def testValDefBuilderTraverseAndClose[A: Type, B: Type](expr: Expr[A]): Expr[B] = {
    val option = ValDefBuilder.ofDef0[B]("a").map(_ => expr).traverse { a =>
      if (Type[A] <:< Type.of[AnyRef] && Type[B] <:< Type.of[AnyRef]) Some {
        Expr.quote(Expr.splice(a).asInstanceOf[B])
      }
      else None
    }
    option.fold[Expr[B]](runtimeFail)(_.build.close)
  }

  // ValDefsCache methods

  def testValDefBuilderBuildCachedAndUse: Expr[Data] = {
    implicit val intType: Type[Int] = IntType
    var cache = ValDefsCache.empty

    // Test ofVal
    val valBuilder = ValDefBuilder.ofVal[Int]("valExample")
    cache = valBuilder.buildCachedWith(cache, "valKey")(_ => Expr.quote(1))
    val valValue = cache.get0Ary[Int]("valKey").fold(Expr.quote(0))(identity)

    // Test ofVar - getter and setter
    val varBuilder = ValDefBuilder.ofVar[Int]("varExample")
    cache = varBuilder.buildCachedWith(cache, "varKey")(_ => Expr.quote(2))
    val varGetter = cache.get0Ary[Int]("varKey").fold(Expr.quote(0))(identity)
    val varValue = varGetter // Just use getter value, setter existence is verified separately

    // Test ofLazy
    val lazyBuilder = ValDefBuilder.ofLazy[Int]("lazyExample")
    cache = lazyBuilder.buildCachedWith(cache, "lazyKey")(_ => Expr.quote(3))
    val lazyValue = cache.get0Ary[Int]("lazyKey").fold(Expr.quote(0))(identity)

    // Test ofDef0
    val def0Builder = ValDefBuilder.ofDef0[Int]("def0Example")
    cache = def0Builder.buildCachedWith(cache, "def0Key")(_ => Expr.quote(0))
    val def0Value = cache.get0Ary[Int]("def0Key").fold(Expr.quote(0))(identity)

    // Test ofDef1 through ofDef22
    // format: off
    val def1Builder = ValDefBuilder.ofDef1[Int, Int]("def1Example", "a")
    cache = def1Builder.buildCachedWith(cache, "def1Key") { case (_, a) => Expr.quote(Expr.splice(a) * 10) }
    val def1Value = cache.get1Ary[Int, Int]("def1Key").fold(Expr.quote(0))(_(Expr.quote(1)))

    val def2Builder = ValDefBuilder.ofDef2[Int, Int, Int]("def2Example", "a", "b")
    cache = def2Builder.buildCachedWith(cache, "def2Key") { case (_, (a, b)) => Expr.quote((Expr.splice(a) + Expr.splice(b)) * 10) }
    val def2Value = cache.get2Ary[Int, Int, Int]("def2Key").fold(Expr.quote(0))(f => f(Expr.quote(1), Expr.quote(2)))

    val def3Builder = ValDefBuilder.ofDef3[Int, Int, Int, Int]("def3Example", "a", "b", "c")
    cache = def3Builder.buildCachedWith(cache, "def3Key") { case (_, (a, b, c)) => Expr.quote((Expr.splice(a) + Expr.splice(b) + Expr.splice(c)) * 10) }
    val def3Value = cache.get3Ary[Int, Int, Int, Int]("def3Key").fold(Expr.quote(0))(f => f(Expr.quote(1), Expr.quote(2), Expr.quote(3)))

    val def4Builder = ValDefBuilder.ofDef4[Int, Int, Int, Int, Int]("def4Example", "a", "b", "c", "d")
    cache = def4Builder.buildCachedWith(cache, "def4Key") { case (_, (a, b, c, d)) => Expr.quote((Expr.splice(a) + Expr.splice(b) + Expr.splice(c) + Expr.splice(d)) * 10) }
    val def4Value = cache.get4Ary[Int, Int, Int, Int, Int]("def4Key").fold(Expr.quote(0))(f => f(Expr.quote(1), Expr.quote(2), Expr.quote(3), Expr.quote(4)))

    val def5Builder = ValDefBuilder.ofDef5[Int, Int, Int, Int, Int, Int]("def5Example", "a", "b", "c", "d", "e")
    cache = def5Builder.buildCachedWith(cache, "def5Key") { case (_, (a, b, c, d, e)) => Expr.quote((Expr.splice(a) + Expr.splice(b) + Expr.splice(c) + Expr.splice(d) + Expr.splice(e)) * 10) }
    val def5Value = cache.get5Ary[Int, Int, Int, Int, Int, Int]("def5Key").fold(Expr.quote(0))(f => f(Expr.quote(1), Expr.quote(2), Expr.quote(3), Expr.quote(4), Expr.quote(5)))

    val def6Builder = ValDefBuilder.ofDef6[Int, Int, Int, Int, Int, Int, Int]("def6Example", "a", "b", "c", "d", "e", "f")
    cache = def6Builder.buildCachedWith(cache, "def6Key") { case (_, (a, b, c, d, e, f)) => Expr.quote((Expr.splice(a) + Expr.splice(b) + Expr.splice(c) + Expr.splice(d) + Expr.splice(e) + Expr.splice(f)) * 10) }
    val def6Value = cache.get6Ary[Int, Int, Int, Int, Int, Int, Int]("def6Key").fold(Expr.quote(0))(f => f(Expr.quote(1), Expr.quote(2), Expr.quote(3), Expr.quote(4), Expr.quote(5), Expr.quote(6)))

    val def7Builder = ValDefBuilder.ofDef7[Int, Int, Int, Int, Int, Int, Int, Int]("def7Example", "a", "b", "c", "d", "e", "f", "g")
    cache = def7Builder.buildCachedWith(cache, "def7Key") { case (_, (a, b, c, d, e, f, g)) => Expr.quote((Expr.splice(a) + Expr.splice(b) + Expr.splice(c) + Expr.splice(d) + Expr.splice(e) + Expr.splice(f) + Expr.splice(g)) * 10) }
    val def7Value = cache.get7Ary[Int, Int, Int, Int, Int, Int, Int, Int]("def7Key").fold(Expr.quote(0))(f => f(Expr.quote(1), Expr.quote(2), Expr.quote(3), Expr.quote(4), Expr.quote(5), Expr.quote(6), Expr.quote(7)))

    val def8Builder = ValDefBuilder.ofDef8[Int, Int, Int, Int, Int, Int, Int, Int, Int]("def8Example", "a", "b", "c", "d", "e", "f", "g", "h")
    cache = def8Builder.buildCachedWith(cache, "def8Key") { case (_, (a, b, c, d, e, f, g, h)) => Expr.quote((Expr.splice(a) + Expr.splice(b) + Expr.splice(c) + Expr.splice(d) + Expr.splice(e) + Expr.splice(f) + Expr.splice(g) + Expr.splice(h)) * 10) }
    val def8Value = cache.get8Ary[Int, Int, Int, Int, Int, Int, Int, Int, Int]("def8Key").fold(Expr.quote(0))(f => f(Expr.quote(1), Expr.quote(2), Expr.quote(3), Expr.quote(4), Expr.quote(5), Expr.quote(6), Expr.quote(7), Expr.quote(8)))

    val def9Builder = ValDefBuilder.ofDef9[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("def9Example", "a", "b", "c", "d", "e", "f", "g", "h", "i")
    cache = def9Builder.buildCachedWith(cache, "def9Key") { case (_, (a, b, c, d, e, f, g, h, i)) => Expr.quote((Expr.splice(a) + Expr.splice(b) + Expr.splice(c) + Expr.splice(d) + Expr.splice(e) + Expr.splice(f) + Expr.splice(g) + Expr.splice(h) + Expr.splice(i)) * 10) }
    val def9Value = cache.get9Ary[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("def9Key").fold(Expr.quote(0))(f => f(Expr.quote(1), Expr.quote(2), Expr.quote(3), Expr.quote(4), Expr.quote(5), Expr.quote(6), Expr.quote(7), Expr.quote(8), Expr.quote(9)))

    val def10Builder = ValDefBuilder.ofDef10[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("def10Example", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j")
    cache = def10Builder.buildCachedWith(cache, "def10Key") { case (_, (a, b, c, d, e, f, g, h, i, j)) => Expr.quote((Expr.splice(a) + Expr.splice(b) + Expr.splice(c) + Expr.splice(d) + Expr.splice(e) + Expr.splice(f) + Expr.splice(g) + Expr.splice(h) + Expr.splice(i) + Expr.splice(j)) * 10) }
    val def10Value = cache.get10Ary[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("def10Key").fold(Expr.quote(0))(f => f(Expr.quote(1), Expr.quote(2), Expr.quote(3), Expr.quote(4), Expr.quote(5), Expr.quote(6), Expr.quote(7), Expr.quote(8), Expr.quote(9), Expr.quote(10)))

    val def11Builder = ValDefBuilder.ofDef11[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("def11Example", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k")
    cache = def11Builder.buildCachedWith(cache, "def11Key") { case (_, (a, b, c, d, e, f, g, h, i, j, k)) => Expr.quote((Expr.splice(a) + Expr.splice(b) + Expr.splice(c) + Expr.splice(d) + Expr.splice(e) + Expr.splice(f) + Expr.splice(g) + Expr.splice(h) + Expr.splice(i) + Expr.splice(j) + Expr.splice(k)) * 10) }
    val def11Value = cache.get11Ary[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("def11Key").fold(Expr.quote(0))(f => f(Expr.quote(1), Expr.quote(2), Expr.quote(3), Expr.quote(4), Expr.quote(5), Expr.quote(6), Expr.quote(7), Expr.quote(8), Expr.quote(9), Expr.quote(10), Expr.quote(11)))

    val def12Builder = ValDefBuilder.ofDef12[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("def12Example", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l")
    cache = def12Builder.buildCachedWith(cache, "def12Key") { case (_, (a, b, c, d, e, f, g, h, i, j, k, l)) => Expr.quote((Expr.splice(a) + Expr.splice(b) + Expr.splice(c) + Expr.splice(d) + Expr.splice(e) + Expr.splice(f) + Expr.splice(g) + Expr.splice(h) + Expr.splice(i) + Expr.splice(j) + Expr.splice(k) + Expr.splice(l)) * 10) }
    val def12Value = cache.get12Ary[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("def12Key").fold(Expr.quote(0))(f => f(Expr.quote(1), Expr.quote(2), Expr.quote(3), Expr.quote(4), Expr.quote(5), Expr.quote(6), Expr.quote(7), Expr.quote(8), Expr.quote(9), Expr.quote(10), Expr.quote(11), Expr.quote(12)))

    val def13Builder = ValDefBuilder.ofDef13[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("def13Example", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m")
    cache = def13Builder.buildCachedWith(cache, "def13Key") { case (_, (a, b, c, d, e, f, g, h, i, j, k, l, m)) => Expr.quote((Expr.splice(a) + Expr.splice(b) + Expr.splice(c) + Expr.splice(d) + Expr.splice(e) + Expr.splice(f) + Expr.splice(g) + Expr.splice(h) + Expr.splice(i) + Expr.splice(j) + Expr.splice(k) + Expr.splice(l) + Expr.splice(m)) * 10) }
    val def13Value = cache.get13Ary[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("def13Key").fold(Expr.quote(0))(f => f(Expr.quote(1), Expr.quote(2), Expr.quote(3), Expr.quote(4), Expr.quote(5), Expr.quote(6), Expr.quote(7), Expr.quote(8), Expr.quote(9), Expr.quote(10), Expr.quote(11), Expr.quote(12), Expr.quote(13)))

    val def14Builder = ValDefBuilder.ofDef14[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("def14Example", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n")
    cache = def14Builder.buildCachedWith(cache, "def14Key") { case (_, (a, b, c, d, e, f, g, h, i, j, k, l, m, n)) => Expr.quote((Expr.splice(a) + Expr.splice(b) + Expr.splice(c) + Expr.splice(d) + Expr.splice(e) + Expr.splice(f) + Expr.splice(g) + Expr.splice(h) + Expr.splice(i) + Expr.splice(j) + Expr.splice(k) + Expr.splice(l) + Expr.splice(m) + Expr.splice(n)) * 10) }
    val def14Value = cache.get14Ary[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("def14Key").fold(Expr.quote(0))(f => f(Expr.quote(1), Expr.quote(2), Expr.quote(3), Expr.quote(4), Expr.quote(5), Expr.quote(6), Expr.quote(7), Expr.quote(8), Expr.quote(9), Expr.quote(10), Expr.quote(11), Expr.quote(12), Expr.quote(13), Expr.quote(14)))

    val def15Builder = ValDefBuilder.ofDef15[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("def15Example", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o")
    cache = def15Builder.buildCachedWith(cache, "def15Key") { case (_, (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)) => Expr.quote((Expr.splice(a) + Expr.splice(b) + Expr.splice(c) + Expr.splice(d) + Expr.splice(e) + Expr.splice(f) + Expr.splice(g) + Expr.splice(h) + Expr.splice(i) + Expr.splice(j) + Expr.splice(k) + Expr.splice(l) + Expr.splice(m) + Expr.splice(n) + Expr.splice(o)) * 10) }
    val def15Value = cache.get15Ary[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("def15Key").fold(Expr.quote(0))(f => f(Expr.quote(1), Expr.quote(2), Expr.quote(3), Expr.quote(4), Expr.quote(5), Expr.quote(6), Expr.quote(7), Expr.quote(8), Expr.quote(9), Expr.quote(10), Expr.quote(11), Expr.quote(12), Expr.quote(13), Expr.quote(14), Expr.quote(15)))

    val def16Builder = ValDefBuilder.ofDef16[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("def16Example", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p")
    cache = def16Builder.buildCachedWith(cache, "def16Key") { case (_, (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)) => Expr.quote((Expr.splice(a) + Expr.splice(b) + Expr.splice(c) + Expr.splice(d) + Expr.splice(e) + Expr.splice(f) + Expr.splice(g) + Expr.splice(h) + Expr.splice(i) + Expr.splice(j) + Expr.splice(k) + Expr.splice(l) + Expr.splice(m) + Expr.splice(n) + Expr.splice(o) + Expr.splice(p)) * 10) }
    val def16Value = cache.get16Ary[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("def16Key").fold(Expr.quote(0))(f => f(Expr.quote(1), Expr.quote(2), Expr.quote(3), Expr.quote(4), Expr.quote(5), Expr.quote(6), Expr.quote(7), Expr.quote(8), Expr.quote(9), Expr.quote(10), Expr.quote(11), Expr.quote(12), Expr.quote(13), Expr.quote(14), Expr.quote(15), Expr.quote(16)))

    val def17Builder = ValDefBuilder.ofDef17[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("def17Example", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q")
    cache = def17Builder.buildCachedWith(cache, "def17Key") { case (_, (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q)) => Expr.quote((Expr.splice(a) + Expr.splice(b) + Expr.splice(c) + Expr.splice(d) + Expr.splice(e) + Expr.splice(f) + Expr.splice(g) + Expr.splice(h) + Expr.splice(i) + Expr.splice(j) + Expr.splice(k) + Expr.splice(l) + Expr.splice(m) + Expr.splice(n) + Expr.splice(o) + Expr.splice(p) + Expr.splice(q)) * 10) }
    val def17Value = cache.get17Ary[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("def17Key").fold(Expr.quote(0))(f => f(Expr.quote(1), Expr.quote(2), Expr.quote(3), Expr.quote(4), Expr.quote(5), Expr.quote(6), Expr.quote(7), Expr.quote(8), Expr.quote(9), Expr.quote(10), Expr.quote(11), Expr.quote(12), Expr.quote(13), Expr.quote(14), Expr.quote(15), Expr.quote(16), Expr.quote(17)))

    val def18Builder = ValDefBuilder.ofDef18[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("def18Example", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r")
    cache = def18Builder.buildCachedWith(cache, "def18Key") { case (_, (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r)) => Expr.quote((Expr.splice(a) + Expr.splice(b) + Expr.splice(c) + Expr.splice(d) + Expr.splice(e) + Expr.splice(f) + Expr.splice(g) + Expr.splice(h) + Expr.splice(i) + Expr.splice(j) + Expr.splice(k) + Expr.splice(l) + Expr.splice(m) + Expr.splice(n) + Expr.splice(o) + Expr.splice(p) + Expr.splice(q) + Expr.splice(r)) * 10) }
    val def18Value = cache.get18Ary[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("def18Key").fold(Expr.quote(0))(f => f(Expr.quote(1), Expr.quote(2), Expr.quote(3), Expr.quote(4), Expr.quote(5), Expr.quote(6), Expr.quote(7), Expr.quote(8), Expr.quote(9), Expr.quote(10), Expr.quote(11), Expr.quote(12), Expr.quote(13), Expr.quote(14), Expr.quote(15), Expr.quote(16), Expr.quote(17), Expr.quote(18)))

    val def19Builder = ValDefBuilder.ofDef19[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("def19Example", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s")
    cache = def19Builder.buildCachedWith(cache, "def19Key") { case (_, (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s)) => Expr.quote((Expr.splice(a) + Expr.splice(b) + Expr.splice(c) + Expr.splice(d) + Expr.splice(e) + Expr.splice(f) + Expr.splice(g) + Expr.splice(h) + Expr.splice(i) + Expr.splice(j) + Expr.splice(k) + Expr.splice(l) + Expr.splice(m) + Expr.splice(n) + Expr.splice(o) + Expr.splice(p) + Expr.splice(q) + Expr.splice(r) + Expr.splice(s)) * 10) }
    val def19Value = cache.get19Ary[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("def19Key").fold(Expr.quote(0))(f => f(Expr.quote(1), Expr.quote(2), Expr.quote(3), Expr.quote(4), Expr.quote(5), Expr.quote(6), Expr.quote(7), Expr.quote(8), Expr.quote(9), Expr.quote(10), Expr.quote(11), Expr.quote(12), Expr.quote(13), Expr.quote(14), Expr.quote(15), Expr.quote(16), Expr.quote(17), Expr.quote(18), Expr.quote(19)))

    val def20Builder = ValDefBuilder.ofDef20[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("def20Example", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t")
    cache = def20Builder.buildCachedWith(cache, "def20Key") { case (_, (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t)) => Expr.quote((Expr.splice(a) + Expr.splice(b) + Expr.splice(c) + Expr.splice(d) + Expr.splice(e) + Expr.splice(f) + Expr.splice(g) + Expr.splice(h) + Expr.splice(i) + Expr.splice(j) + Expr.splice(k) + Expr.splice(l) + Expr.splice(m) + Expr.splice(n) + Expr.splice(o) + Expr.splice(p) + Expr.splice(q) + Expr.splice(r) + Expr.splice(s) + Expr.splice(t)) * 10) }
    val def20Value = cache.get20Ary[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("def20Key").fold(Expr.quote(0))(f => f(Expr.quote(1), Expr.quote(2), Expr.quote(3), Expr.quote(4), Expr.quote(5), Expr.quote(6), Expr.quote(7), Expr.quote(8), Expr.quote(9), Expr.quote(10), Expr.quote(11), Expr.quote(12), Expr.quote(13), Expr.quote(14), Expr.quote(15), Expr.quote(16), Expr.quote(17), Expr.quote(18), Expr.quote(19), Expr.quote(20)))

    val def21Builder = ValDefBuilder.ofDef21[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("def21Example", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u")
    cache = def21Builder.buildCachedWith(cache, "def21Key") { case (_, (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u)) => Expr.quote((Expr.splice(a) + Expr.splice(b) + Expr.splice(c) + Expr.splice(d) + Expr.splice(e) + Expr.splice(f) + Expr.splice(g) + Expr.splice(h) + Expr.splice(i) + Expr.splice(j) + Expr.splice(k) + Expr.splice(l) + Expr.splice(m) + Expr.splice(n) + Expr.splice(o) + Expr.splice(p) + Expr.splice(q) + Expr.splice(r) + Expr.splice(s) + Expr.splice(t) + Expr.splice(u)) * 10) }
    val def21Value = cache.get21Ary[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("def21Key").fold(Expr.quote(0))(f => f(Expr.quote(1), Expr.quote(2), Expr.quote(3), Expr.quote(4), Expr.quote(5), Expr.quote(6), Expr.quote(7), Expr.quote(8), Expr.quote(9), Expr.quote(10), Expr.quote(11), Expr.quote(12), Expr.quote(13), Expr.quote(14), Expr.quote(15), Expr.quote(16), Expr.quote(17), Expr.quote(18), Expr.quote(19), Expr.quote(20), Expr.quote(21)))

    val def22Builder = ValDefBuilder.ofDef22[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("def22Example", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v")
    cache = def22Builder.buildCachedWith(cache, "def22Key") { case (_, (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v)) => Expr.quote((Expr.splice(a) + Expr.splice(b) + Expr.splice(c) + Expr.splice(d) + Expr.splice(e) + Expr.splice(f) + Expr.splice(g) + Expr.splice(h) + Expr.splice(i) + Expr.splice(j) + Expr.splice(k) + Expr.splice(l) + Expr.splice(m) + Expr.splice(n) + Expr.splice(o) + Expr.splice(p) + Expr.splice(q) + Expr.splice(r) + Expr.splice(s) + Expr.splice(t) + Expr.splice(u) + Expr.splice(v)) * 10) }
    val def22Value = cache.get22Ary[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("def22Key").fold(Expr.quote(0))(f => f(Expr.quote(1), Expr.quote(2), Expr.quote(3), Expr.quote(4), Expr.quote(5), Expr.quote(6), Expr.quote(7), Expr.quote(8), Expr.quote(9), Expr.quote(10), Expr.quote(11), Expr.quote(12), Expr.quote(13), Expr.quote(14), Expr.quote(15), Expr.quote(16), Expr.quote(17), Expr.quote(18), Expr.quote(19), Expr.quote(20), Expr.quote(21), Expr.quote(22)))
    // format: on

    cache.toValDefs.use { _ =>
      Expr.quote {
        Data.map(
          "val" -> Data(Expr.splice(valValue)),
          "var" -> Data(Expr.splice(varValue)),
          "lazy" -> Data(Expr.splice(lazyValue)),
          "def0" -> Data(Expr.splice(def0Value)),
          "def1" -> Data(Expr.splice(def1Value)),
          "def2" -> Data(Expr.splice(def2Value)),
          "def3" -> Data(Expr.splice(def3Value)),
          "def4" -> Data(Expr.splice(def4Value)),
          "def5" -> Data(Expr.splice(def5Value)),
          "def6" -> Data(Expr.splice(def6Value)),
          "def7" -> Data(Expr.splice(def7Value)),
          "def8" -> Data(Expr.splice(def8Value)),
          "def9" -> Data(Expr.splice(def9Value)),
          "def10" -> Data(Expr.splice(def10Value)),
          "def11" -> Data(Expr.splice(def11Value)),
          "def12" -> Data(Expr.splice(def12Value)),
          "def13" -> Data(Expr.splice(def13Value)),
          "def14" -> Data(Expr.splice(def14Value)),
          "def15" -> Data(Expr.splice(def15Value)),
          "def16" -> Data(Expr.splice(def16Value)),
          "def17" -> Data(Expr.splice(def17Value)),
          "def18" -> Data(Expr.splice(def18Value)),
          "def19" -> Data(Expr.splice(def19Value)),
          "def20" -> Data(Expr.splice(def20Value)),
          "def21" -> Data(Expr.splice(def21Value)),
          "def22" -> Data(Expr.splice(def22Value))
        )
      }
    }
  }

  def testValDefBuilderBuildCachedVarGetterSetter: Expr[Data] = {
    implicit val intType: Type[Int] = IntType
    implicit val unitType: Type[Unit] = UnitType
    var cache = ValDefsCache.empty

    // Create var builder and build cached
    val varBuilder = ValDefBuilder.ofVar[Int]("varExample")
    cache = varBuilder.buildCachedWith(cache, "varKey")(_ => Expr.quote(100))

    // Retrieve getter
    val getterOpt = cache.get0Ary[Int]("varKey")
    val getterValue = getterOpt.fold(Expr.quote(0))(identity)

    // Retrieve setter
    val setterOpt = cache.get1Ary[Int, Unit]("varKey")

    cache.toValDefs.use { _ =>
      Expr.quote {
        Data.map(
          "getterValue" -> Data(Expr.splice(getterValue)),
          "getterExists" -> Data(Expr.splice(Expr(getterOpt.isDefined))),
          "setterExists" -> Data(Expr.splice(Expr(setterOpt.isDefined)))
        )
      }
    }
  }

  // LambdaBuilder methods

  def testLambdaBuilderOfNAndBuild: Expr[Data] = {
    implicit val intType: Type[Int] = IntType
    // format: off
    val lambda1 = LambdaBuilder
      .of1[Int]("a")
      .map { case (a) =>
        Expr.quote(Expr.splice(a) + 1)
      }
      .build
    val lambda2 = LambdaBuilder
      .of2[Int, Int]("a", "b")
      .map { case ((a, b)) =>
        Expr.quote(Expr.splice(a) * Expr.splice(b) + 1)
      }
      .build
    val lambda3 = LambdaBuilder
      .of3[Int, Int, Int]("a", "b", "c")
      .map { case ((a, b, c)) =>
        Expr.quote(Expr.splice(a) * Expr.splice(b) * Expr.splice(c) + 1)
      }
      .build
    val lambda4 = LambdaBuilder
      .of4[Int, Int, Int, Int]("a", "b", "c", "d")
      .map { case ((a, b, c, d)) =>
        Expr.quote(Expr.splice(a) * Expr.splice(b) * Expr.splice(c) * Expr.splice(d) + 1)
      }
      .build
    val lambda5 = LambdaBuilder
      .of5[Int, Int, Int, Int, Int]("a", "b", "c", "d", "e")
      .map { case ((a, b, c, d, e)) =>
        Expr.quote(Expr.splice(a) * Expr.splice(b) * Expr.splice(c) * Expr.splice(d) * Expr.splice(e) + 1)
      }
      .build
    val lambda6 = LambdaBuilder
      .of6[Int, Int, Int, Int, Int, Int]("a", "b", "c", "d", "e", "f")
      .map { case ((a, b, c, d, e, f)) =>
        Expr.quote(Expr.splice(a) * Expr.splice(b) * Expr.splice(c) * Expr.splice(d) * Expr.splice(e) * Expr.splice(f) + 1)
      }
      .build
    val lambda7 = LambdaBuilder
      .of7[Int, Int, Int, Int, Int, Int, Int]("a", "b", "c", "d", "e", "f", "g")
      .map { case ((a, b, c, d, e, f, g)) =>
        Expr.quote(Expr.splice(a) * Expr.splice(b) * Expr.splice(c) * Expr.splice(d) * Expr.splice(e) * Expr.splice(f) * Expr.splice(g) + 1)
      }
      .build
    val lambda8 = LambdaBuilder
      .of8[Int, Int, Int, Int, Int, Int, Int, Int]("a", "b", "c", "d", "e", "f", "g", "h")
      .map { case ((a, b, c, d, e, f, g, h)) =>
        Expr.quote(Expr.splice(a) * Expr.splice(b) * Expr.splice(c) * Expr.splice(d) * Expr.splice(e) * Expr.splice(f) * Expr.splice(g) * Expr.splice(h) + 1)
      }
      .build
    val lambda9 = LambdaBuilder
      .of9[Int, Int, Int, Int, Int, Int, Int, Int, Int]("a", "b", "c", "d", "e", "f", "g", "h", "i")
      .map { case ((a, b, c, d, e, f, g, h, i)) =>
        Expr.quote(Expr.splice(a) * Expr.splice(b) * Expr.splice(c) * Expr.splice(d) * Expr.splice(e) * Expr.splice(f) * Expr.splice(g) * Expr.splice(h) * Expr.splice(i) + 1)
      }
      .build
    val lambda10 = LambdaBuilder
      .of10[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("a", "b", "c", "d", "e", "f", "g", "h", "i", "j")
      .map { case ((a, b, c, d, e, f, g, h, i, j)) =>
        Expr.quote(Expr.splice(a) * Expr.splice(b) * Expr.splice(c) * Expr.splice(d) * Expr.splice(e) * Expr.splice(f) * Expr.splice(g) * Expr.splice(h) * Expr.splice(i) * Expr.splice(j) + 1)
      }
      .build
    val lambda11 = LambdaBuilder
      .of11[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k")
      .map { case ((a, b, c, d, e, f, g, h, i, j, k)) =>
        Expr.quote(Expr.splice(a) * Expr.splice(b) * Expr.splice(c) * Expr.splice(d) * Expr.splice(e) * Expr.splice(f) * Expr.splice(g) * Expr.splice(h) * Expr.splice(i) * Expr.splice(j) * Expr.splice(k) + 1)
      }
      .build
    val lambda12 = LambdaBuilder
      .of12[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l")
      .map { case ((a, b, c, d, e, f, g, h, i, j, k, l)) =>
        Expr.quote(Expr.splice(a) * Expr.splice(b) * Expr.splice(c) * Expr.splice(d) * Expr.splice(e) * Expr.splice(f) * Expr.splice(g) * Expr.splice(h) * Expr.splice(i) * Expr.splice(j) * Expr.splice(k) * Expr.splice(l) + 1)
      }
      .build
    val lambda13 = LambdaBuilder
      .of13[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m")
      .map { case ((a, b, c, d, e, f, g, h, i, j, k, l, m)) =>
        Expr.quote(Expr.splice(a) * Expr.splice(b) * Expr.splice(c) * Expr.splice(d) * Expr.splice(e) * Expr.splice(f) * Expr.splice(g) * Expr.splice(h) * Expr.splice(i) * Expr.splice(j) * Expr.splice(k) * Expr.splice(l) * Expr.splice(m) + 1)
      }
      .build
    val lambda14 = LambdaBuilder
      .of14[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n")
      .map { case ((a, b, c, d, e, f, g, h, i, j, k, l, m, n)) =>
        Expr.quote(Expr.splice(a) * Expr.splice(b) * Expr.splice(c) * Expr.splice(d) * Expr.splice(e) * Expr.splice(f) * Expr.splice(g) * Expr.splice(h) * Expr.splice(i) * Expr.splice(j) * Expr.splice(k) * Expr.splice(l) * Expr.splice(m) * Expr.splice(n) + 1)
      }
      .build
    val lambda15 = LambdaBuilder
      .of15[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o")
      .map { case ((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)) =>
        Expr.quote(Expr.splice(a) * Expr.splice(b) * Expr.splice(c) * Expr.splice(d) * Expr.splice(e) * Expr.splice(f) * Expr.splice(g) * Expr.splice(h) * Expr.splice(i) * Expr.splice(j) * Expr.splice(k) * Expr.splice(l) * Expr.splice(m) * Expr.splice(n) * Expr.splice(o) + 1)
      }
      .build
    val lambda16 = LambdaBuilder
      .of16[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p")
      .map { case ((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)) =>
        Expr.quote(Expr.splice(a) * Expr.splice(b) * Expr.splice(c) * Expr.splice(d) * Expr.splice(e) * Expr.splice(f) * Expr.splice(g) * Expr.splice(h) * Expr.splice(i) * Expr.splice(j) * Expr.splice(k) * Expr.splice(l) * Expr.splice(m) * Expr.splice(n) * Expr.splice(o) * Expr.splice(p) + 1)
      }
      .build
    val lambda17 = LambdaBuilder
      .of17[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q")
      .map { case ((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q)) =>
        Expr.quote(Expr.splice(a) * Expr.splice(b) * Expr.splice(c) * Expr.splice(d) * Expr.splice(e) * Expr.splice(f) * Expr.splice(g) * Expr.splice(h) * Expr.splice(i) * Expr.splice(j) * Expr.splice(k) * Expr.splice(l) * Expr.splice(m) * Expr.splice(n) * Expr.splice(o) * Expr.splice(p) * Expr.splice(q) + 1)
      }
      .build
    val lambda18 = LambdaBuilder
      .of18[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r")
      .map { case ((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r)) =>
        Expr.quote(Expr.splice(a) * Expr.splice(b) * Expr.splice(c) * Expr.splice(d) * Expr.splice(e) * Expr.splice(f) * Expr.splice(g) * Expr.splice(h) * Expr.splice(i) * Expr.splice(j) * Expr.splice(k) * Expr.splice(l) * Expr.splice(m) * Expr.splice(n) * Expr.splice(o) * Expr.splice(p) * Expr.splice(q) * Expr.splice(r) + 1)
      }
      .build
    val lambda19 = LambdaBuilder
      .of19[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s")
      .map { case ((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s)) =>
        Expr.quote(Expr.splice(a) * Expr.splice(b) * Expr.splice(c) * Expr.splice(d) * Expr.splice(e) * Expr.splice(f) * Expr.splice(g) * Expr.splice(h) * Expr.splice(i) * Expr.splice(j) * Expr.splice(k) * Expr.splice(l) * Expr.splice(m) * Expr.splice(n) * Expr.splice(o) * Expr.splice(p) * Expr.splice(q) * Expr.splice(r) * Expr.splice(s) + 1)
      }
      .build
    val lambda20 = LambdaBuilder
      .of20[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t")
      .map { case ((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t)) =>
        Expr.quote(Expr.splice(a) * Expr.splice(b) * Expr.splice(c) * Expr.splice(d) * Expr.splice(e) * Expr.splice(f) * Expr.splice(g) * Expr.splice(h) * Expr.splice(i) * Expr.splice(j) * Expr.splice(k) * Expr.splice(l) * Expr.splice(m) * Expr.splice(n) * Expr.splice(o) * Expr.splice(p) * Expr.splice(q) * Expr.splice(r) * Expr.splice(s) * Expr.splice(t) + 1)
      }
      .build
    val lambda21 = LambdaBuilder
      .of21[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u")
      .map { case ((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u)) =>
        Expr.quote(Expr.splice(a) * Expr.splice(b) * Expr.splice(c) * Expr.splice(d) * Expr.splice(e) * Expr.splice(f) * Expr.splice(g) * Expr.splice(h) * Expr.splice(i) * Expr.splice(j) * Expr.splice(k) * Expr.splice(l) * Expr.splice(m) * Expr.splice(n) * Expr.splice(o) * Expr.splice(p) * Expr.splice(q) * Expr.splice(r) * Expr.splice(s) * Expr.splice(t) * Expr.splice(u) + 1)
      }
      .build
    val lambda22 = LambdaBuilder
      .of22[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v")
      .map { case ((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v)) =>
        Expr.quote(Expr.splice(a) * Expr.splice(b) * Expr.splice(c) * Expr.splice(d) * Expr.splice(e) * Expr.splice(f) * Expr.splice(g) * Expr.splice(h) * Expr.splice(i) * Expr.splice(j) * Expr.splice(k) * Expr.splice(l) * Expr.splice(m) * Expr.splice(n) * Expr.splice(o) * Expr.splice(p) * Expr.splice(q) * Expr.splice(r) * Expr.splice(s) * Expr.splice(t) * Expr.splice(u) * Expr.splice(v) + 1)
      }
      .build
    // format: on
    Expr.quote {
      Data.map(
        "of1" -> Data(Expr.splice(lambda1)(2)),
        "of2" -> Data(Expr.splice(lambda2)(2, 3)),
        "of3" -> Data(Expr.splice(lambda3)(2, 3, 5)),
        "of4" -> Data(Expr.splice(lambda4)(2, 3, 5, 7)),
        "of5" -> Data(Expr.splice(lambda5)(2, 3, 5, 7, 11)),
        "of6" -> Data(Expr.splice(lambda6)(2, 3, 5, 7, 11, 13)),
        "of7" -> Data(Expr.splice(lambda7)(2, 3, 5, 7, 11, 13, 17)),
        "of8" -> Data(Expr.splice(lambda8)(2, 3, 5, 7, 11, 13, 17, 19)),
        "of9" -> Data(Expr.splice(lambda9)(2, 3, 5, 7, 11, 13, 17, 19, 23)),
        "of10" -> Data(Expr.splice(lambda10)(2, 3, 5, 7, 11, 13, 17, 19, 23, 29)),
        "of11" -> Data(Expr.splice(lambda11)(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31)),
        "of12" -> Data(Expr.splice(lambda12)(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37)),
        "of13" -> Data(Expr.splice(lambda13)(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41)),
        "of14" -> Data(Expr.splice(lambda14)(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43)),
        "of15" -> Data(Expr.splice(lambda15)(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47)),
        "of16" -> Data(Expr.splice(lambda16)(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53)),
        "of17" -> Data(Expr.splice(lambda17)(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59)),
        "of18" -> Data(Expr.splice(lambda18)(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61)),
        "of19" -> Data(Expr.splice(lambda19)(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67)),
        "of20" -> Data(
          Expr.splice(lambda20)(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71)
        ),
        "of21" -> Data(
          Expr.splice(lambda21)(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73)
        ),
        "of22" -> Data(
          Expr.splice(lambda22)(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79)
        )
      )
    }
  }

  def testLambdaBuilderBuildWith: Expr[Data] = {
    implicit val intType: Type[Int] = IntType
    val lambda = LambdaBuilder
      .of1[Int]("a")
      .buildWith { case (a) => Expr.quote(Expr.splice(a) + 1) }
    Expr.quote {
      Data(Expr.splice(lambda)(2))
    }
  }

  def testLambdaBuilderPartition[A: Type](expr: Expr[A]): Expr[Data] = {
    implicit val intType: Type[Int] = IntType
    val either = LambdaBuilder
      .of1[Int]("a")
      .partition { case (a) =>
        if (Type[A] <:< Type.of[Int]) Right {
          Expr.quote(Expr.splice(expr.upcast[Int]) + Expr.splice(a))
        }
        else Left(runtimeFail[Int])
      }
    val lambda = either.fold[Expr[Int => Int]](_.build, _.build)
    Expr.quote {
      Data(Expr.splice(lambda)(2))
    }
  }

  def testLambdaBuilderTraverse[A: Type](expr: Expr[A]): Expr[Data] = {
    implicit val intType: Type[Int] = IntType
    implicit val intFunctionType: Type[Int => Int] = IntFunctionType
    val option = LambdaBuilder
      .of1[Int]("a")
      .traverse { case (a) =>
        if (Type[A] <:< Type.of[Int]) Some {
          Expr.quote(Expr.splice(expr.upcast[Int]) + Expr.splice(a))
        }
        else None
      }
    val lambda = option.fold[Expr[Int => Int]](runtimeFail)(_.build)
    Expr.quote {
      Data(Expr.splice(lambda)(2))
    }
  }

  // ExprCodecs

  def testBidirectionalCodecs: Expr[Data] = try {
    implicit val intType: Type[Int] = IntType
    def roundtrip[A: ExprCodec](value: A): Data = {
      val encoded = Expr(value)
      val decoded = Expr.unapply(encoded)
      Data.map(
        "encoded" -> Data(encoded.plainPrint),
        "decoded" -> Data(decoded.fold("not decoded")(s => s"$s"))
      )
    }
    Expr(
      Data.map(
        "null" -> roundtrip(null),
        "unit" -> roundtrip(()),
        "boolean" -> roundtrip(true),
        "byte" -> roundtrip[Byte](1),
        "short" -> roundtrip[Short](1),
        "int" -> roundtrip(1),
        "long" -> roundtrip(1L),
        "float" -> roundtrip(1.0f),
        "double" -> roundtrip(1.0),
        "char" -> roundtrip('a'),
        "string" -> roundtrip("a"),
        "Class" -> roundtrip(classOf[Int]),
        "ClassTag" -> roundtrip(scala.reflect.classTag[Int])
      )
    )
  } catch {
    case e: Throwable =>
      e.printStackTrace()
      Environment.reportErrorAndAbort(e.getMessage)
  }

  def testOneWayCodecs: Expr[Data] = {
    implicit val intType: Type[Int] = IntType
    def oneWay[A: ExprCodec](value: A): Data = {
      val encoded = Expr(value)
      Data.map(
        "encoded" -> Data(encoded.plainPrint)
      )
    }
    Expr(
      Data.map(
        "Array[Int]" -> oneWay(Array[Int](1)),
        "Seq[Int]" -> oneWay(Seq[Int](1)),
        "List[Int]" -> oneWay(List[Int](1)),
        "Nil" -> oneWay(Nil),
        "Vector[Int]" -> oneWay(Vector[Int](1)),
        "Map[Int, Int]" -> oneWay(Map[Int, Int](1 -> 1)),
        "Set[Int]" -> oneWay(Set[Int](1)),
        "Option[Int]" -> oneWay(Option[Int](1)),
        "Some[Int]" -> oneWay(Some[Int](1)),
        "None" -> oneWay(None),
        "Either[Int, Int]" -> oneWay(Left[Int, Int](1)),
        "Left[Int, Int]" -> oneWay(Left[Int, Int](1)),
        "Right[Int, Int]" -> oneWay(Right[Int, Int](1))
      )
    )
  }

  // types using in fixtures

  private val IntType = Type.of[Int]
  private val UnitType = Type.of[Unit]
  private val DataType = Type.of[Data]
  private val IntFunctionType = Type.of[Int => Int]

  private def runtimeFail[A: Type]: Expr[A] = Expr.quote(???)
}
