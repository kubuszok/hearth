package hearth

import hearth.data.Data
import hearth.fp.effect.*

/** Fixtured for testing [[MioIntegrationsSpec]]. */
trait MioIntegrationsFixturesImpl { this: hearth.MacroTypedCommons =>

  @scala.annotation.nowarn
  def testValDefBuilderBuildCachedWithMIO: Expr[Data] = {
    implicit val intType: Type[Int] = IntType
    val cacheLocal = ValDefsCache.mlocal

    // format: off
    val def0Builder = ValDefBuilder.ofDef0[Int]("def0Example")
    val def1Builder = ValDefBuilder.ofDef1[Int, Int]("def1Example", "a")
    val def2Builder = ValDefBuilder.ofDef2[Int, Int, Int]("def2Example", "a", "b")
    val def3Builder = ValDefBuilder.ofDef3[Int, Int, Int, Int]("def3Example", "a", "b", "c")
    val def4Builder = ValDefBuilder.ofDef4[Int, Int, Int, Int, Int]("def4Example", "a", "b", "c", "d")
    val def5Builder = ValDefBuilder.ofDef5[Int, Int, Int, Int, Int, Int]("def5Example", "a", "b", "c", "d", "e")
    val def6Builder = ValDefBuilder.ofDef6[Int, Int, Int, Int, Int, Int, Int]("def6Example", "a", "b", "c", "d", "e", "f")
    val def7Builder = ValDefBuilder.ofDef7[Int, Int, Int, Int, Int, Int, Int, Int]("def7Example", "a", "b", "c", "d", "e", "f", "g")
    val def8Builder = ValDefBuilder.ofDef8[Int, Int, Int, Int, Int, Int, Int, Int, Int]("def8Example", "a", "b", "c", "d", "e", "f", "g", "h")
    val def9Builder = ValDefBuilder.ofDef9[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("def9Example", "a", "b", "c", "d", "e", "f", "g", "h", "i")
    val def10Builder = ValDefBuilder.ofDef10[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("def10Example", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j")
    val def11Builder = ValDefBuilder.ofDef11[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("def11Example", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k")
    val def12Builder = ValDefBuilder.ofDef12[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("def12Example", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l")
    val def13Builder = ValDefBuilder.ofDef13[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("def13Example", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m")
    val def14Builder = ValDefBuilder.ofDef14[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("def14Example", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n")
    val def15Builder = ValDefBuilder.ofDef15[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("def15Example", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o")
    val def16Builder = ValDefBuilder.ofDef16[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("def16Example", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p")
    val def17Builder = ValDefBuilder.ofDef17[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("def17Example", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q")
    val def18Builder = ValDefBuilder.ofDef18[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("def18Example", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r")
    val def19Builder = ValDefBuilder.ofDef19[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("def19Example", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s")
    val def20Builder = ValDefBuilder.ofDef20[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("def20Example", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t")
    val def21Builder = ValDefBuilder.ofDef21[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("def21Example", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u")
    val def22Builder = ValDefBuilder.ofDef22[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("def22Example", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v")
    // format: on

    val result = for {
      // Test ofDef0 with forwardDeclare
      _ <- cacheLocal.forwardDeclare("def0Key", def0Builder)
      _ <- cacheLocal.buildCachedWith("def0Key", def0Builder)(_ => Expr.quote(0))
      def0Value <- cacheLocal.get0Ary[Int]("def0Key").map(_.fold(runtimeFail[Int])(identity))

      // Test ofDef1 with forwardDeclare
      _ <- cacheLocal.forwardDeclare("def1Key", def1Builder)
      _ <- cacheLocal.buildCachedWith("def1Key", def1Builder) { case (_, a) =>
        Expr.quote(Expr.splice(a) * 10)
      }
      def1Value <- cacheLocal
        .get1Ary[Int, Int]("def1Key")
        .map(_.fold(runtimeFail[Int])(f => f(Expr.quote(1))))

      // Test ofDef2 with forwardDeclare
      _ <- cacheLocal.forwardDeclare("def2Key", def2Builder)
      _ <- cacheLocal.buildCachedWith("def2Key", def2Builder) { case (_, (a, b)) =>
        Expr.quote((Expr.splice(a) + Expr.splice(b)) * 10)
      }
      def2Value <- cacheLocal
        .get2Ary[Int, Int, Int]("def2Key")
        .map(_.fold(runtimeFail[Int])(f => f(Expr.quote(1), Expr.quote(2))))

      // Test ofDef3 with forwardDeclare
      _ <- cacheLocal.forwardDeclare("def3Key", def3Builder)
      _ <- cacheLocal.buildCachedWith("def3Key", def3Builder) { case (_, (a, b, c)) =>
        Expr.quote((Expr.splice(a) + Expr.splice(b) + Expr.splice(c)) * 10)
      }
      def3Value <- cacheLocal
        .get3Ary[Int, Int, Int, Int]("def3Key")
        .map(_.fold(runtimeFail[Int])(f => f(Expr.quote(1), Expr.quote(2), Expr.quote(3))))

      // Test ofDef4 with forwardDeclare
      _ <- cacheLocal.forwardDeclare("def4Key", def4Builder)
      _ <- cacheLocal.buildCachedWith("def4Key", def4Builder) { case (_, (a, b, c, d)) =>
        Expr.quote((Expr.splice(a) + Expr.splice(b) + Expr.splice(c) + Expr.splice(d)) * 10)
      }
      def4Value <- cacheLocal
        .get4Ary[Int, Int, Int, Int, Int]("def4Key")
        .map(_.fold(runtimeFail[Int])(f => f(Expr.quote(1), Expr.quote(2), Expr.quote(3), Expr.quote(4))))

      // Test ofDef5 with forwardDeclare
      _ <- cacheLocal.forwardDeclare("def5Key", def5Builder)
      _ <- cacheLocal.buildCachedWith("def5Key", def5Builder) { case (_, (a, b, c, d, e)) =>
        Expr.quote((Expr.splice(a) + Expr.splice(b) + Expr.splice(c) + Expr.splice(d) + Expr.splice(e)) * 10)
      }
      def5Value <- cacheLocal
        .get5Ary[Int, Int, Int, Int, Int, Int]("def5Key")
        .map(
          _.fold(runtimeFail[Int])(f => f(Expr.quote(1), Expr.quote(2), Expr.quote(3), Expr.quote(4), Expr.quote(5)))
        )

      // Test ofDef6 with forwardDeclare
      _ <- cacheLocal.forwardDeclare("def6Key", def6Builder)
      _ <- cacheLocal.buildCachedWith("def6Key", def6Builder) { case (_, (a, b, c, d, e, f)) =>
        Expr.quote(
          (Expr.splice(a) + Expr.splice(b) + Expr.splice(c) + Expr.splice(d) + Expr.splice(e) + Expr.splice(f)) * 10
        )
      }
      def6Value <- cacheLocal
        .get6Ary[Int, Int, Int, Int, Int, Int, Int]("def6Key")
        .map(
          _.fold(runtimeFail[Int])(f =>
            f(Expr.quote(1), Expr.quote(2), Expr.quote(3), Expr.quote(4), Expr.quote(5), Expr.quote(6))
          )
        )

      // Test ofDef7 with forwardDeclare
      _ <- cacheLocal.forwardDeclare("def7Key", def7Builder)
      _ <- cacheLocal.buildCachedWith("def7Key", def7Builder) { case (_, (a, b, c, d, e, f, g)) =>
        Expr.quote(
          (Expr.splice(a) + Expr.splice(b) + Expr.splice(c) + Expr.splice(d) + Expr.splice(e) + Expr
            .splice(f) + Expr.splice(g)) * 10
        )
      }
      def7Value <- cacheLocal
        .get7Ary[Int, Int, Int, Int, Int, Int, Int, Int]("def7Key")
        .map(
          _.fold(runtimeFail[Int])(f =>
            f(Expr.quote(1), Expr.quote(2), Expr.quote(3), Expr.quote(4), Expr.quote(5), Expr.quote(6), Expr.quote(7))
          )
        )

      // Test ofDef8 with forwardDeclare
      _ <- cacheLocal.forwardDeclare("def8Key", def8Builder)
      _ <- cacheLocal.buildCachedWith("def8Key", def8Builder) { case (_, (a, b, c, d, e, f, g, h)) =>
        Expr.quote(
          (Expr.splice(a) + Expr.splice(b) + Expr.splice(c) + Expr.splice(d) + Expr.splice(e) + Expr
            .splice(f) + Expr.splice(g) + Expr.splice(h)) * 10
        )
      }
      def8Value <- cacheLocal
        .get8Ary[Int, Int, Int, Int, Int, Int, Int, Int, Int]("def8Key")
        .map(
          _.fold(runtimeFail[Int])(f =>
            f(
              Expr.quote(1),
              Expr.quote(2),
              Expr.quote(3),
              Expr.quote(4),
              Expr.quote(5),
              Expr.quote(6),
              Expr.quote(7),
              Expr.quote(8)
            )
          )
        )

      // Test ofDef9 with forwardDeclare
      _ <- cacheLocal.forwardDeclare("def9Key", def9Builder)
      _ <- cacheLocal.buildCachedWith("def9Key", def9Builder) { case (_, (a, b, c, d, e, f, g, h, i)) =>
        Expr.quote(
          (Expr.splice(a) + Expr.splice(b) + Expr.splice(c) + Expr.splice(d) + Expr.splice(e) + Expr
            .splice(f) + Expr.splice(g) + Expr.splice(h) + Expr.splice(i)) * 10
        )
      }
      def9Value <- cacheLocal
        .get9Ary[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("def9Key")
        .map(
          _.fold(runtimeFail[Int])(f =>
            f(
              Expr.quote(1),
              Expr.quote(2),
              Expr.quote(3),
              Expr.quote(4),
              Expr.quote(5),
              Expr.quote(6),
              Expr.quote(7),
              Expr.quote(8),
              Expr.quote(9)
            )
          )
        )

      // Test ofDef10 with forwardDeclare
      _ <- cacheLocal.forwardDeclare("def10Key", def10Builder)
      _ <- cacheLocal.buildCachedWith("def10Key", def10Builder) { case (_, (a, b, c, d, e, f, g, h, i, j)) =>
        Expr.quote(
          (Expr.splice(a) + Expr.splice(b) + Expr.splice(c) + Expr.splice(d) + Expr.splice(e) + Expr
            .splice(f) + Expr.splice(g) + Expr.splice(h) + Expr.splice(i) + Expr.splice(j)) * 10
        )
      }
      def10Value <- cacheLocal
        .get10Ary[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("def10Key")
        .map(
          _.fold(runtimeFail[Int])(f =>
            f(
              Expr.quote(1),
              Expr.quote(2),
              Expr.quote(3),
              Expr.quote(4),
              Expr.quote(5),
              Expr.quote(6),
              Expr.quote(7),
              Expr.quote(8),
              Expr.quote(9),
              Expr.quote(10)
            )
          )
        )

      // Test ofDef11 with forwardDeclare
      _ <- cacheLocal.forwardDeclare("def11Key", def11Builder)
      _ <- cacheLocal.buildCachedWith("def11Key", def11Builder) { case (_, (a, b, c, d, e, f, g, h, i, j, k)) =>
        Expr.quote(
          (Expr.splice(a) + Expr.splice(b) + Expr.splice(c) + Expr.splice(d) + Expr.splice(e) + Expr
            .splice(f) + Expr.splice(g) + Expr.splice(h) + Expr.splice(i) + Expr.splice(j) + Expr.splice(k)) * 10
        )
      }
      def11Value <- cacheLocal
        .get11Ary[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("def11Key")
        .map(
          _.fold(runtimeFail[Int])(f =>
            f(
              Expr.quote(1),
              Expr.quote(2),
              Expr.quote(3),
              Expr.quote(4),
              Expr.quote(5),
              Expr.quote(6),
              Expr.quote(7),
              Expr.quote(8),
              Expr.quote(9),
              Expr.quote(10),
              Expr.quote(11)
            )
          )
        )

      // Test ofDef12 with forwardDeclare
      _ <- cacheLocal.forwardDeclare("def12Key", def12Builder)
      _ <- cacheLocal.buildCachedWith("def12Key", def12Builder) { case (_, (a, b, c, d, e, f, g, h, i, j, k, l)) =>
        Expr.quote(
          (Expr.splice(a) + Expr.splice(b) + Expr.splice(c) + Expr.splice(d) + Expr.splice(e) + Expr
            .splice(f) + Expr.splice(g) + Expr.splice(h) + Expr.splice(i) + Expr.splice(j) + Expr.splice(k) + Expr
            .splice(l)) * 10
        )
      }
      def12Value <- cacheLocal
        .get12Ary[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("def12Key")
        .map(
          _.fold(runtimeFail[Int])(f =>
            f(
              Expr.quote(1),
              Expr.quote(2),
              Expr.quote(3),
              Expr.quote(4),
              Expr.quote(5),
              Expr.quote(6),
              Expr.quote(7),
              Expr.quote(8),
              Expr.quote(9),
              Expr.quote(10),
              Expr.quote(11),
              Expr.quote(12)
            )
          )
        )

      // Test ofDef13 with forwardDeclare
      _ <- cacheLocal.forwardDeclare("def13Key", def13Builder)
      _ <- cacheLocal.buildCachedWith("def13Key", def13Builder) { case (_, (a, b, c, d, e, f, g, h, i, j, k, l, m)) =>
        Expr.quote(
          (Expr.splice(a) + Expr.splice(b) + Expr.splice(c) + Expr.splice(d) + Expr.splice(e) + Expr
            .splice(f) + Expr.splice(g) + Expr.splice(h) + Expr.splice(i) + Expr.splice(j) + Expr.splice(k) + Expr
            .splice(l) + Expr.splice(m)) * 10
        )
      }
      def13Value <- cacheLocal
        .get13Ary[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("def13Key")
        .map(
          _.fold(runtimeFail[Int])(f =>
            f(
              Expr.quote(1),
              Expr.quote(2),
              Expr.quote(3),
              Expr.quote(4),
              Expr.quote(5),
              Expr.quote(6),
              Expr.quote(7),
              Expr.quote(8),
              Expr.quote(9),
              Expr.quote(10),
              Expr.quote(11),
              Expr.quote(12),
              Expr.quote(13)
            )
          )
        )

      // Test ofDef14 with forwardDeclare
      _ <- cacheLocal.forwardDeclare("def14Key", def14Builder)
      _ <- cacheLocal.buildCachedWith("def14Key", def14Builder) {
        case (_, (a, b, c, d, e, f, g, h, i, j, k, l, m, n)) =>
          Expr.quote(
            (Expr.splice(a) + Expr.splice(b) + Expr.splice(c) + Expr.splice(d) + Expr.splice(e) + Expr
              .splice(f) + Expr.splice(g) + Expr.splice(h) + Expr.splice(i) + Expr.splice(j) + Expr.splice(k) + Expr
              .splice(l) + Expr.splice(m) + Expr.splice(n)) * 10
          )
      }
      def14Value <- cacheLocal
        .get14Ary[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("def14Key")
        .map(
          _.fold(runtimeFail[Int])(f =>
            f(
              Expr.quote(1),
              Expr.quote(2),
              Expr.quote(3),
              Expr.quote(4),
              Expr.quote(5),
              Expr.quote(6),
              Expr.quote(7),
              Expr.quote(8),
              Expr.quote(9),
              Expr.quote(10),
              Expr.quote(11),
              Expr.quote(12),
              Expr.quote(13),
              Expr.quote(14)
            )
          )
        )

      // Test ofDef15 with forwardDeclare
      _ <- cacheLocal.forwardDeclare("def15Key", def15Builder)
      _ <- cacheLocal.buildCachedWith("def15Key", def15Builder) {
        case (_, (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)) =>
          Expr.quote(
            (Expr.splice(a) + Expr.splice(b) + Expr.splice(c) + Expr.splice(d) + Expr.splice(e) + Expr
              .splice(f) + Expr.splice(g) + Expr.splice(h) + Expr.splice(i) + Expr.splice(j) + Expr.splice(k) + Expr
              .splice(l) + Expr.splice(m) + Expr.splice(n) + Expr.splice(o)) * 10
          )
      }
      def15Value <- cacheLocal
        .get15Ary[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("def15Key")
        .map(
          _.fold(runtimeFail[Int])(f =>
            f(
              Expr.quote(1),
              Expr.quote(2),
              Expr.quote(3),
              Expr.quote(4),
              Expr.quote(5),
              Expr.quote(6),
              Expr.quote(7),
              Expr.quote(8),
              Expr.quote(9),
              Expr.quote(10),
              Expr.quote(11),
              Expr.quote(12),
              Expr.quote(13),
              Expr.quote(14),
              Expr.quote(15)
            )
          )
        )

      // Test ofDef16 with forwardDeclare
      _ <- cacheLocal.forwardDeclare("def16Key", def16Builder)
      _ <- cacheLocal.buildCachedWith("def16Key", def16Builder) {
        case (_, (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)) =>
          Expr.quote(
            (Expr.splice(a) + Expr.splice(b) + Expr.splice(c) + Expr.splice(d) + Expr.splice(e) + Expr
              .splice(f) + Expr.splice(g) + Expr.splice(h) + Expr.splice(i) + Expr.splice(j) + Expr.splice(k) + Expr
              .splice(l) + Expr.splice(m) + Expr.splice(n) + Expr.splice(o) + Expr.splice(p)) * 10
          )
      }
      def16Value <- cacheLocal
        .get16Ary[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("def16Key")
        .map(
          _.fold(runtimeFail[Int])(f =>
            f(
              Expr.quote(1),
              Expr.quote(2),
              Expr.quote(3),
              Expr.quote(4),
              Expr.quote(5),
              Expr.quote(6),
              Expr.quote(7),
              Expr.quote(8),
              Expr.quote(9),
              Expr.quote(10),
              Expr.quote(11),
              Expr.quote(12),
              Expr.quote(13),
              Expr.quote(14),
              Expr.quote(15),
              Expr.quote(16)
            )
          )
        )

      // Test ofDef17 with forwardDeclare
      _ <- cacheLocal.forwardDeclare("def17Key", def17Builder)
      _ <- cacheLocal.buildCachedWith("def17Key", def17Builder) {
        case (_, (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q)) =>
          Expr.quote(
            (Expr.splice(a) + Expr.splice(b) + Expr.splice(c) + Expr.splice(d) + Expr.splice(e) + Expr
              .splice(f) + Expr.splice(g) + Expr.splice(h) + Expr.splice(i) + Expr.splice(j) + Expr.splice(k) + Expr
              .splice(l) + Expr.splice(m) + Expr.splice(n) + Expr.splice(o) + Expr.splice(p) + Expr.splice(q)) * 10
          )
      }
      def17Value <- cacheLocal
        .get17Ary[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]("def17Key")
        .map(
          _.fold(runtimeFail[Int])(f =>
            f(
              Expr.quote(1),
              Expr.quote(2),
              Expr.quote(3),
              Expr.quote(4),
              Expr.quote(5),
              Expr.quote(6),
              Expr.quote(7),
              Expr.quote(8),
              Expr.quote(9),
              Expr.quote(10),
              Expr.quote(11),
              Expr.quote(12),
              Expr.quote(13),
              Expr.quote(14),
              Expr.quote(15),
              Expr.quote(16),
              Expr.quote(17)
            )
          )
        )

      // Test ofDef18 with forwardDeclare
      _ <- cacheLocal.forwardDeclare("def18Key", def18Builder)
      _ <- cacheLocal.buildCachedWith("def18Key", def18Builder) {
        case (_, (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r)) =>
          Expr.quote(
            (Expr.splice(a) + Expr.splice(b) + Expr.splice(c) + Expr.splice(d) + Expr.splice(e) + Expr
              .splice(f) + Expr.splice(g) + Expr.splice(h) + Expr.splice(i) + Expr.splice(j) + Expr.splice(k) + Expr
              .splice(l) + Expr.splice(m) + Expr.splice(n) + Expr.splice(o) + Expr.splice(p) + Expr.splice(q) + Expr
              .splice(r)) * 10
          )
      }
      def18Value <- cacheLocal
        .get18Ary[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int](
          "def18Key"
        )
        .map(
          _.fold(runtimeFail[Int])(f =>
            f(
              Expr.quote(1),
              Expr.quote(2),
              Expr.quote(3),
              Expr.quote(4),
              Expr.quote(5),
              Expr.quote(6),
              Expr.quote(7),
              Expr.quote(8),
              Expr.quote(9),
              Expr.quote(10),
              Expr.quote(11),
              Expr.quote(12),
              Expr.quote(13),
              Expr.quote(14),
              Expr.quote(15),
              Expr.quote(16),
              Expr.quote(17),
              Expr.quote(18)
            )
          )
        )

      // Test ofDef19 with forwardDeclare
      _ <- cacheLocal.forwardDeclare("def19Key", def19Builder)
      _ <- cacheLocal.buildCachedWith("def19Key", def19Builder) {
        case (_, (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s)) =>
          Expr.quote(
            (Expr.splice(a) + Expr.splice(b) + Expr.splice(c) + Expr.splice(d) + Expr.splice(e) + Expr
              .splice(f) + Expr.splice(g) + Expr.splice(h) + Expr.splice(i) + Expr.splice(j) + Expr.splice(k) + Expr
              .splice(l) + Expr.splice(m) + Expr.splice(n) + Expr.splice(o) + Expr.splice(p) + Expr.splice(q) + Expr
              .splice(r) + Expr.splice(s)) * 10
          )
      }
      def19Value <- cacheLocal
        .get19Ary[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int](
          "def19Key"
        )
        .map(
          _.fold(runtimeFail[Int])(f =>
            f(
              Expr.quote(1),
              Expr.quote(2),
              Expr.quote(3),
              Expr.quote(4),
              Expr.quote(5),
              Expr.quote(6),
              Expr.quote(7),
              Expr.quote(8),
              Expr.quote(9),
              Expr.quote(10),
              Expr.quote(11),
              Expr.quote(12),
              Expr.quote(13),
              Expr.quote(14),
              Expr.quote(15),
              Expr.quote(16),
              Expr.quote(17),
              Expr.quote(18),
              Expr.quote(19)
            )
          )
        )

      // Test ofDef20 with forwardDeclare
      _ <- cacheLocal.forwardDeclare("def20Key", def20Builder)
      _ <- cacheLocal.buildCachedWith("def20Key", def20Builder) {
        case (_, (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t)) =>
          Expr.quote(
            (Expr.splice(a) + Expr.splice(b) + Expr.splice(c) + Expr.splice(d) + Expr.splice(e) + Expr
              .splice(f) + Expr.splice(g) + Expr.splice(h) + Expr.splice(i) + Expr.splice(j) + Expr.splice(k) + Expr
              .splice(l) + Expr.splice(m) + Expr.splice(n) + Expr.splice(o) + Expr.splice(p) + Expr.splice(q) + Expr
              .splice(r) + Expr.splice(s) + Expr.splice(t)) * 10
          )
      }
      def20Value <- cacheLocal
        .get20Ary[
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
        ]("def20Key")
        .map(
          _.fold(runtimeFail[Int])(f =>
            f(
              Expr.quote(1),
              Expr.quote(2),
              Expr.quote(3),
              Expr.quote(4),
              Expr.quote(5),
              Expr.quote(6),
              Expr.quote(7),
              Expr.quote(8),
              Expr.quote(9),
              Expr.quote(10),
              Expr.quote(11),
              Expr.quote(12),
              Expr.quote(13),
              Expr.quote(14),
              Expr.quote(15),
              Expr.quote(16),
              Expr.quote(17),
              Expr.quote(18),
              Expr.quote(19),
              Expr.quote(20)
            )
          )
        )

      // Test ofDef21 with forwardDeclare
      _ <- cacheLocal.forwardDeclare("def21Key", def21Builder)
      _ <- cacheLocal.buildCachedWith("def21Key", def21Builder) {
        case (_, (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u)) =>
          Expr.quote(
            (Expr.splice(a) + Expr.splice(b) + Expr.splice(c) + Expr.splice(d) + Expr.splice(e) + Expr
              .splice(f) + Expr.splice(g) + Expr.splice(h) + Expr.splice(i) + Expr.splice(j) + Expr.splice(k) + Expr
              .splice(l) + Expr.splice(m) + Expr.splice(n) + Expr.splice(o) + Expr.splice(p) + Expr.splice(q) + Expr
              .splice(r) + Expr.splice(s) + Expr.splice(t) + Expr.splice(u)) * 10
          )
      }
      def21Value <- cacheLocal
        .get21Ary[
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
        ]("def21Key")
        .map(
          _.fold(runtimeFail[Int])(f =>
            f(
              Expr.quote(1),
              Expr.quote(2),
              Expr.quote(3),
              Expr.quote(4),
              Expr.quote(5),
              Expr.quote(6),
              Expr.quote(7),
              Expr.quote(8),
              Expr.quote(9),
              Expr.quote(10),
              Expr.quote(11),
              Expr.quote(12),
              Expr.quote(13),
              Expr.quote(14),
              Expr.quote(15),
              Expr.quote(16),
              Expr.quote(17),
              Expr.quote(18),
              Expr.quote(19),
              Expr.quote(20),
              Expr.quote(21)
            )
          )
        )

      // Test ofDef22 with forwardDeclare
      _ <- cacheLocal.forwardDeclare("def22Key", def22Builder)
      _ <- cacheLocal.buildCachedWith("def22Key", def22Builder) {
        case (_, (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v)) =>
          Expr.quote(
            (Expr.splice(a) + Expr.splice(b) + Expr.splice(c) + Expr.splice(d) + Expr.splice(e) + Expr
              .splice(f) + Expr.splice(g) + Expr.splice(h) + Expr.splice(i) + Expr.splice(j) + Expr.splice(k) + Expr
              .splice(l) + Expr.splice(m) + Expr.splice(n) + Expr.splice(o) + Expr.splice(p) + Expr.splice(q) + Expr
              .splice(r) + Expr.splice(s) + Expr.splice(t) + Expr.splice(u) + Expr.splice(v)) * 10
          )
      }
      def22Value <- cacheLocal
        .get22Ary[
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
          Int,
          Int
        ]("def22Key")
        .map(
          _.fold(runtimeFail[Int])(f =>
            f(
              Expr.quote(1),
              Expr.quote(2),
              Expr.quote(3),
              Expr.quote(4),
              Expr.quote(5),
              Expr.quote(6),
              Expr.quote(7),
              Expr.quote(8),
              Expr.quote(9),
              Expr.quote(10),
              Expr.quote(11),
              Expr.quote(12),
              Expr.quote(13),
              Expr.quote(14),
              Expr.quote(15),
              Expr.quote(16),
              Expr.quote(17),
              Expr.quote(18),
              Expr.quote(19),
              Expr.quote(20),
              Expr.quote(21),
              Expr.quote(22)
            )
          )
        )

      cache <- cacheLocal.get
    } yield cache.toValDefs.use { _ =>
      Expr.quote {
        Data.map(
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

    result.runToExprOrFail("testValDefBuilderBuildCachedWithMIO")((_, _) => "")
  }

  def testExtensionLoadingResultToMIO: Expr[Data] = {
    def processResult[A](result: MIO[ExtensionLoadingResult.Loaded[A]]): MIO[Data] =
      result.redeem(loaded =>
        Data.map(
          "loaded" -> Data.list(loaded.toSeq.map(e => Data(e.getClass.getName))*)
        )
      )(errors =>
        Data.map(
          "errors" -> Data.list(errors.toList.map(e => Data(e.getMessage))*)
        )
      )

    val program = for {
      successfulDontAllowFailures <- processResult(Environment.loadMacroExtensions[SuccessfulMacroExtension].toMIO())
      successfulAllowFailures <- processResult(
        Environment.loadMacroExtensions[SuccessfulMacroExtension].toMIO(allowFailures = true)
      )
      runningFailedDontAllowFailures <- processResult(
        Environment.loadMacroExtensions[PartiallyFailedMacroExtension].toMIO()
      )
      runningFailedAllowFailures <- processResult(
        Environment.loadMacroExtensions[PartiallyFailedMacroExtension].toMIO(allowFailures = true)
      )
      loadingFailedDontAllowFailures <- processResult(
        Environment.loadMacroExtensions[TotallyFailedMacroExtension].toMIO()
      )
      loadingFailedAllowFailures <- processResult(
        Environment.loadMacroExtensions[TotallyFailedMacroExtension].toMIO(allowFailures = true)
      )
    } yield Data.map(
      "successfulDontAllowFailures" -> successfulDontAllowFailures,
      "successfulAllowFailures" -> successfulAllowFailures,
      "runningFailedDontAllowFailures" -> runningFailedDontAllowFailures,
      "runningFailedAllowFailures" -> runningFailedAllowFailures,
      "loadingFailedDontAllowFailures" -> loadingFailedDontAllowFailures,
      "loadingFailedAllowFailures" -> loadingFailedAllowFailures
    )

    val (state, result) = program.unsafe.runSync
    val data = result.getOrElse(Environment.reportErrorAndAbort("Failed to run program"))
    val logs = state.logs.render.fromInfo("Logs")
    Expr(
      Data.map(
        "data" -> data,
        "logs" -> Data(logs)
      )
    )
  }

  // types using in fixtures

  private val IntType = Type.of[Int]

  private def runtimeFail[A: Type]: Expr[A] = Expr.quote(???)
}
