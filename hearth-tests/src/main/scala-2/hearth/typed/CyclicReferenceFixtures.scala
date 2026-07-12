package hearth
package typed

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

/** Fixtures for [[CyclicReferenceScala2Spec]] — a Scala-2-only reproduction of the mechanism behind Chimney #899 (`case
  * class A() { val foo = this.transformInto[B] }`), and of Hearth's fix for it.
  *
  * The essential ingredient is that the reflection of `A` happens INSIDE an implicit search that runs while an
  * enclosing `val` of `A` is still being inferred. [[CyclicTC]] plays the role of Chimney's `Transformer`: its implicit
  * materializer ([[deriveCyclicTC]]) reflects on the source with the very same calls Chimney's product extraction uses
  * (`Type[A].unsortedMethods` + forcing each member's return type). Forcing the still-inferring member's signature
  * re-enters its own completer, which scala-reflect signals with a `CyclicReference`.
  *
  * BEFORE the fix that raw `CyclicReference` leaked out of the listing call and the compile failed with
  * `recursive value foo needs type`. AFTER the fix Hearth treats the un-completable member honestly: a getter provably
  * has no value parameters (so it stays listable with `Nil` parameters) and its still-unknown return type surfaces as
  * `knownReturning == None` instead of throwing — the caller then decides what to do with a member of unknown type.
  * Here the materializer simply ignores it and returns a stub, so the whole thing compiles.
  */
trait CyclicReferenceFixturesImpl { this: MacroCommons =>

  def deriveCyclicTC[A: Type, B: Type]: Expr[CyclicTC[A, B]] = {
    // Reflect on `A` exactly like Chimney's product extraction: list instance methods and force each return type.
    // Under the implicit search below this re-enters the still-running completer of the enclosing `val` — Hearth now
    // yields `knownReturning == None` for that member instead of throwing a raw CyclicReference.
    val _ = Type[A].unsortedMethods.collect { case oi: Method.OnInstance => (oi: Method) }.map(_.knownReturning)
    cyclicStub[A, B]
  }

  /** Reflects on a FULLY-TYPED `A` and reports, per instance method, whether its `knownReturning` is defined —
    * `"name:Some"` / `"name:None"`, comma-separated. Used to prove the #899 fix does not over-eagerly report `None`:
    * ordinary getters on a completed type must still resolve their return type (`Some`).
    */
  def knownReturningReport[A: Type]: Expr[String] = {
    val report = Type[A].unsortedMethods
      .collect { case oi: Method.OnInstance => oi }
      .map(m => s"${m.name}:${if (m.knownReturning.isDefined) "Some" else "None"}")
      .sorted
      .mkString(",")
    Expr(report)
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

object CyclicReferenceFixtures {
  def knownReturningReport[A]: String = macro CyclicReferenceFixtures.knownReturningReportImpl[A]
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

  def knownReturningReportImpl[A: c.WeakTypeTag]: c.Expr[String] = knownReturningReport[A]
}
