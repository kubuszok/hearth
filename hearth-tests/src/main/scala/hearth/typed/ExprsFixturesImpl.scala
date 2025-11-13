package hearth
package typed

import hearth.data.Data
import hearth.fp.instances.*
import hearth.fp.syntax.*

/** Fixtured for testing [[ExprsSpec]]. */
trait ExprsFixturesImpl { this: MacroTypedCommons & untyped.UntypedMethods =>

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

  // Scoped methods

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

  def testScopePartitionAndClose[A: Type, B: Type](expr: Expr[A]): Expr[B] = {
    val either = Scoped.createDef(expr, "a").partition { a =>
      if (Type[A] <:< Type.of[AnyRef] && Type[B] <:< Type.of[AnyRef]) Right {
        Expr.quote(Expr.splice(a).asInstanceOf[B])
      }
      else Left(runtimeFail[B])
    }
    either.fold[Expr[B]](_.close, _.close)
  }

  def testScopeTraverseAndClose[A: Type, B: Type](expr: Expr[A]): Expr[B] = {
    val option = Scoped.createDef(expr, "a").traverse { a =>
      if (Type[A] <:< Type.of[AnyRef] && Type[B] <:< Type.of[AnyRef]) Some {
        Expr.quote(Expr.splice(a).asInstanceOf[B])
      }
      else None
    }
    option.fold[Expr[B]](runtimeFail)(_.close)
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
  private val DataType = Type.of[Data]
  private val IntFunctionType = Type.of[Int => Int]

  private def runtimeFail[A: Type]: Expr[A] = Expr.quote(???)
}
