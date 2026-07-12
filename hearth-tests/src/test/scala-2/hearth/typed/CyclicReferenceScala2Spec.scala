package hearth
package typed

/** Scala-2-only reproduction of the mechanism behind Chimney #899 (`case class A() { val foo = this.transformInto[B]
  * }`) and of Hearth's fix for it.
  *
  * `Type[A].unsortedMethods` (and forcing member return types) is safe on its own, but when it runs inside an implicit
  * search that fires while an enclosing `val` of `A` is still being inferred, completing `A`'s member signatures
  * re-enters that `val`'s running completer and scala-reflect signals a `CyclicReference`. BEFORE the fix that raw
  * internal exception leaked out of the listing call and the compile failed with `recursive value foo needs type`.
  * AFTER the fix Hearth represents the un-completable member honestly — a getter stays listable with no parameters and
  * its unknown return type surfaces as `knownReturning == None` — so the reflecting materializer completes and the
  * whole thing compiles, no exception, no signature change.
  *
  * [[CyclicTC]] plays Chimney's `Transformer[A, B]`: its materializer reflects on the source. [[PlainTC]] is an
  * identical type class whose materializer does NOT reflect — the control proving the reflection is what turns the
  * `val` recursive. See [[CyclicReferenceFixtures]].
  */
final class CyclicReferenceScala2Spec extends MacroSuite {

  group("regression: reflecting an enclosing mid-inference member (Chimney #899)") {

    test("a reflecting materializer no longer breaks an inferred `val` of the reflected type") {
      import hearth.typed.CyclicConvertSyntax.*
      case class B()
      // No explicit type on `foo`, yet the reflecting `cyclicConvertInto` materializer compiles: Hearth yields
      // `knownReturning == None` for the mid-inference member instead of leaking a CyclicReference. Before the fix
      // this failed with `recursive value foo needs type`.
      case class A() { val foo = this.cyclicConvertInto[B] }
      A().foo ==> null
    }

    test("control: the SAME shape with a non-reflecting materializer compiles (reflection was the trigger)") {
      import hearth.typed.CyclicConvertSyntax.*
      case class B()
      case class A() { val foo = this.plainConvertInto[B] } // no explicit type, yet compiles - no reflection
      A().foo ==> null
    }

    test("an explicit type on the enclosing member also compiles (the pre-fix workaround still works)") {
      import hearth.typed.CyclicConvertSyntax.*
      case class B()
      case class A() { val foo: B = this.cyclicConvertInto[B] } // explicit type -> no re-entry, compiles
      A().foo ==> null
    }

    test("on a FULLY-TYPED type, getters still resolve their return type (the fix only yields None on a real cycle)") {
      // `knownReturning == None` must be specific to the mid-inference cycle: on a completed type every getter's
      // return type is still resolvable (`Some`). If the fix over-reported None this would contain `:None`.
      val report = CyclicReferenceFixtures.knownReturningReport[FullyTypedForReturning]
      // Fields/getters and other concrete members resolve their return type.
      report.contains("name:Some") ==> true
      report.contains("age:Some") ==> true
      report.contains("copy:Some") ==> true
      report.contains("toString:Some") ==> true
      // The only `None`s are the generic Object members whose type parameter is unresolved (pre-existing behavior,
      // `Methods.scala` — `hasUnresolvedTypeParams`), NOT anything the #899 fix touched.
      report.split(",").filter(_.endsWith(":None")).map(_.takeWhile(_ != ':')).sorted.toList ==>
        List("asInstanceOf", "isInstanceOf", "synchronized")
    }
  }
}

final case class FullyTypedForReturning(name: String, age: Int)
