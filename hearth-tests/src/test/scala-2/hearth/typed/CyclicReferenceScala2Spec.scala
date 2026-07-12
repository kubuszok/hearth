package hearth
package typed

/** Scala-2-only reproduction of the mechanism behind Chimney #899 (`case class A() { val foo = this.transformInto[B] }`
  * failing to compile).
  *
  * `Type[A].unsortedMethods` (and forcing member return types) is safe on its own, but when it runs inside an implicit
  * search that fires while an enclosing `val` of `A` is still being inferred, completing `A`'s member list re-enters
  * that `val`'s running completer. In a normal file compile that surfaces as
  * `scala.reflect.internal.Symbols$CyclicReference: illegal cyclic reference involving value foo` (see the real Chimney
  * control and the standalone harness hearth-repros/repro-899); under the toolbox used by `compileErrors` the typer's
  * earlier guard fires first and reports `recursive value foo needs type`. Both are the same root cause.
  *
  * [[CyclicTC]] plays Chimney's `Transformer[A, B]`: its materializer reflects on the source. [[PlainTC]] is an
  * identical type class whose materializer does NOT reflect — the control proving the reflection is what turns the
  * `val` recursive. See [[CyclicReferenceFixtures]].
  */
final class CyclicReferenceScala2Spec extends MacroSuite {

  group("regression: reflecting an enclosing mid-inference member (Chimney #899)") {

    test("reflecting `A` from an implicit search makes an inferred `val` of `A` fail as recursive/cyclic") {
      compileErrors(
        """
        import hearth.typed.CyclicConvertSyntax._
        case class A() { val foo = this.cyclicConvertInto[B] }
        case class B()
        """
      ).check("recursive value foo needs type")
    }

    test("control: the SAME shape with a non-reflecting materializer compiles (reflection is the cause)") {
      import hearth.typed.CyclicConvertSyntax.*
      case class B()
      case class A() { val foo = this.plainConvertInto[B] } // no explicit type, yet compiles - no reflection
      A().foo ==> null
    }

    test("workaround: giving the enclosing member an explicit type avoids the cycle (documented for #899)") {
      import hearth.typed.CyclicConvertSyntax.*
      case class B()
      case class A() { val foo: B = this.cyclicConvertInto[B] } // explicit type -> no re-entry, compiles
      A().foo ==> null
    }

    // NOTE (verified empirically): Hearth CANNOT rescue the inferred-`val` case. The compiler's namer detects the
    // recursive `val` and reports `recursive value foo needs type` BEFORE Hearth ever forces the member's return type,
    // so no Hearth-level `try/catch` around the reflection (`unsortedMethods`/`knownReturning`) is ever reached - a
    // catch there does not fire. The user-facing remedy is the explicit type annotation above.
  }
}
