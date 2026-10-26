package hearth
package crossquotes

import scala.quoted.*

trait IntShow { def show(a: Int): String }

/** Two things must hold when deriving instances inside a splice:
  *   - `ValDefs.createVal` runs inside the instance quote's splice (the `def show` body), so its val must be owned by
  *     that nested splice's owner, not the macro-entry owner (see `currentSpliceOwner` in `ExprsScala3`);
  *   - a class-level cache first touched inside the first instance's splice has to be pinned to the macro-entry context
  *     (`withMacroEntryCtx`), or it cannot be reused when a SECOND instance is derived in the same expansion.
  *
  * The `-Xcheck-macros` aborts these guard against ("Block contains definition with different owners" / "created in a
  * splice ... used outside") only reproduce across SEPARATE compilation units (macro packaged to a jar, app compiled
  * against it), so here we only verify that the generated code runs correctly. Kept non-generic to avoid the unrelated
  * `Type[A]` self-given loop of a generic `A: Type` bound.
  */
final class InstanceInSpliceFixtures(q: Quotes) extends hearth.MacroCommonsScala3(using q) {

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

  def deriveWithVal: Expr[IntShow] = Expr.quote {
    new IntShow {
      def show(a: Int): String = Expr.splice(bodyWithVal(Expr.quote(a)))
    }
  }

  def deriveTwoWithCache: Expr[(IntShow, IntShow)] = {
    def one: Expr[IntShow] = Expr.quote {
      new IntShow {
        def show(a: Int): String = Expr.splice(bodyWithCache(Expr.quote(a)))
      }
    }
    Expr.quote((Expr.splice(one), Expr.splice(one)))
  }
}

object InstanceInSpliceFixtures {

  inline def deriveWithVal: IntShow = ${ deriveWithValImpl }
  private def deriveWithValImpl(using q: Quotes): Expr[IntShow] =
    new InstanceInSpliceFixtures(q).deriveWithVal

  inline def deriveTwoWithCache: (IntShow, IntShow) = ${ deriveTwoWithCacheImpl }
  private def deriveTwoWithCacheImpl(using q: Quotes): Expr[(IntShow, IntShow)] =
    new InstanceInSpliceFixtures(q).deriveTwoWithCache
}
