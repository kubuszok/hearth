package hearth
package typed

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

/** Fixtures for [[CyclicReferenceScala2Spec]] — a Scala-2-only reproduction of the mechanism behind Chimney #899 (`case
  * class A() { val foo = this.transformInto[B] }` crashing with `CyclicReference: illegal cyclic reference involving
  * value foo`).
  *
  * The essential ingredient is that the reflection of `A` must happen INSIDE an implicit search that runs while an
  * enclosing `val` of `A` is still being inferred. [[CyclicTC]] plays the role of Chimney's `Transformer`: its implicit
  * materializer ([[deriveCyclicTC]]) reflects on the source with the very same calls Chimney's product extraction uses
  * (`Type[A].unsortedMethods` + forcing each return type). When driven through
  * [[CyclicConvertSyntax.Ops.cyclicConvertInto]] (the `transformInto` analogue), the first reflective access throws a
  * `scala.reflect.internal.Symbols$CyclicReference`.
  *
  * The standalone harness (hearth-repros/repro-899) additionally shows the exception is CATCHABLE: catching it and
  * letting the implicit search retry makes compilation succeed. This fixture intentionally does NOT catch, so the spec
  * can pin the observable failure with `compileErrors`.
  */
trait CyclicReferenceFixturesImpl { this: MacroCommons =>

  def deriveCyclicTC[A: Type, B: Type]: Expr[CyclicTC[A, B]] = {
    // Reflect on `A` exactly like Chimney's product extraction: list instance methods and force each return
    // type. Under the implicit search below this re-enters the still-running completer of the enclosing `val`.
    val _ = Type[A].unsortedMethods.collect { case oi: Method.OnInstance => (oi: Method) }.map(_.knownReturning)
    cyclicStub[A, B]
  }

  /** A do-nothing `CyclicTC[A, B]` value — the derivation's correctness is irrelevant, only its reflection is. */
  protected def cyclicStub[A: Type, B: Type]: Expr[CyclicTC[A, B]]
}

/** Type class standing in for Chimney's `Transformer[A, B]`. */
trait CyclicTC[A, B]

object CyclicTC {
  // Materializer that REFLECTS on `A` (the #899 trigger).
  implicit def derive[A, B]: CyclicTC[A, B] = macro CyclicReferenceFixtures.deriveCyclicTCImpl[A, B]
}

/** Same shape as [[CyclicTC]] but its materializer does NOT reflect on `A` — the control that isolates the reflection
  * as the cause of the cycle.
  */
trait PlainTC[A, B]

object PlainTC {
  implicit def derive[A, B]: PlainTC[A, B] = macro CyclicReferenceFixtures.derivePlainTCImpl[A, B]
}

/** `transformInto`-shaped syntax: extension methods whose result type is `B`, driven by an implicit. */
object CyclicConvertSyntax {
  implicit class Ops[A](private val a: A) extends AnyVal {
    def cyclicConvertInto[B](implicit tc: CyclicTC[A, B]): B = null.asInstanceOf[B] // reflecting implicit
    def plainConvertInto[B](implicit tc: PlainTC[A, B]): B = null.asInstanceOf[B] // non-reflecting control
  }
}

final private class CyclicReferenceFixtures(val c: blackbox.Context)
    extends MacroCommonsScala2
    with CyclicReferenceFixturesImpl {
  import c.universe.*

  protected def cyclicStub[A: Type, B: Type]: Expr[CyclicTC[A, B]] =
    c.Expr[CyclicTC[A, B]](q"null.asInstanceOf[_root_.hearth.typed.CyclicTC[${c.weakTypeOf[A]}, ${c.weakTypeOf[B]}]]")

  def deriveCyclicTCImpl[A: c.WeakTypeTag, B: c.WeakTypeTag]: c.Expr[CyclicTC[A, B]] = deriveCyclicTC[A, B]

  // Control: return a stub WITHOUT reflecting on `A`, so an inferred `val` driven by it compiles cleanly.
  def derivePlainTCImpl[A: c.WeakTypeTag, B: c.WeakTypeTag]: c.Expr[PlainTC[A, B]] =
    c.Expr[PlainTC[A, B]](q"null.asInstanceOf[_root_.hearth.typed.PlainTC[${c.weakTypeOf[A]}, ${c.weakTypeOf[B]}]]")
}
