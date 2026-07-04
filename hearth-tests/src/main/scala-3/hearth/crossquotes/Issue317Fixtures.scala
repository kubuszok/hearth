package hearth
package crossquotes

import scala.quoted.*

/** Typeclass used by the hearth#317/#318 fixtures below. */
trait Show317 { def show(a: Int): String }

/** Fixtures for hearth#317/#318 (Scala 3 only).
  *
  * Exercises the two fixes:
  *   - owner-following symbol creation: `ValDefs.createVal` runs inside the instance quote's splice (the `def show`
  *     body), so its val must be owned by that nested splice's owner, not the macro-entry owner (see
  *     `currentSpliceOwner` in `ExprsScala3`);
  *   - `withMacroEntryCtx`: a class-level cache first-touched inside the first instance's splice is pinned to the
  *     macro-entry context so it can be reused when a SECOND instance is derived in the same expansion.
  *
  * The `-Xcheck-macros` aborts these guard against ("Block contains definition with different owners" / "created in a
  * splice ... used outside") only reproduce across SEPARATE compilation units (macro packaged to a jar, app compiled
  * against it) — see the repros linked from the PR. Here we verify the generated code runs correctly. (Kept non-generic
  * to avoid the unrelated `Type[A]` self-given loop of a generic `A: Type` bound.)
  */
final class Issue317Fixtures(q: Quotes) extends hearth.MacroCommonsScala3(using q) {

  private lazy val cachedSuffix: Expr[String] = Expr.quote("!")

  private def bodyWithCache(a: Expr[Int]): Expr[String] = Expr.quote {
    Expr.splice(a).toString + Expr.splice(withMacroEntryCtx(cachedSuffix))
  }

  private def bodyWithVal(a: Expr[Int]): Expr[String] =
    ValDefs.createVal[Int](a, "ref").use { ref =>
      Expr.quote {
        val prefix = "v:"
        prefix + Expr.splice(ref).toString
      }
    }

  def deriveWithVal: Expr[Show317] = Expr.quote {
    new Show317 {
      def show(a: Int): String = Expr.splice(bodyWithVal(Expr.quote(a)))
    }
  }

  def deriveTwoWithCache: Expr[(Show317, Show317)] = {
    def one: Expr[Show317] = Expr.quote {
      new Show317 {
        def show(a: Int): String = Expr.splice(bodyWithCache(Expr.quote(a)))
      }
    }
    Expr.quote((Expr.splice(one), Expr.splice(one)))
  }
}

object Issue317Fixtures {

  inline def deriveWithVal: Show317 = ${ deriveWithValImpl }
  private def deriveWithValImpl(using q: Quotes): Expr[Show317] =
    new Issue317Fixtures(q).deriveWithVal

  inline def deriveTwoWithCache: (Show317, Show317) = ${ deriveTwoWithCacheImpl }
  private def deriveTwoWithCacheImpl(using q: Quotes): Expr[(Show317, Show317)] =
    new Issue317Fixtures(q).deriveTwoWithCache
}
