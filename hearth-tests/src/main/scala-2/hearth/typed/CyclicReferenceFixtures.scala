package hearth
package typed

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

/** Scala 2 only: `case class A() { val foo = this.cyclicConvertInto[B] }` reflects on `A` INSIDE an implicit search
  * that runs while `foo`'s type is still being inferred. Forcing `foo`'s signature re-enters its own completer, which
  * scala-reflect signals with a `CyclicReference`; letting it escape fails the compilation with
  * `recursive value foo needs type`. A getter provably has no value parameters, so it can still be listed (with `Nil`
  * parameters) and only its unknown return type surfaces as `knownReturning == None` - the caller decides what to do
  * with it. Here the materializer ignores it and returns a stub, so the whole thing compiles.
  */
trait CyclicReferenceFixturesImpl { this: MacroCommons =>

  def deriveCyclicTC[A: Type, B: Type]: Expr[CyclicTC[A, B]] = {
    // The same calls a product-type derivation would make. Under the implicit search this re-enters the still-running
    // completer of the enclosing `val`.
    val _ = Type[A].unsortedMethods.collect { case oi: Method.OnInstance => (oi: Method) }.map(_.knownReturning)
    cyclicStub[A, B]
  }

  /** On a FULLY-TYPED `A` ordinary getters must still resolve their return type: `None` must be reserved for members
    * whose type is still being inferred.
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

trait CyclicTC[A, B]

object CyclicTC {
  // Reflecting on `A` is what triggers the cycle.
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
